import com.github.Mojashi.automaton.{ParikhAutomaton, VectorOutputTransducer}
import com.github.Mojashi.automaton.impl.BricsNFAAdapter.BricsNFAAdapter
import com.github.Mojashi.automaton.impl.NFTransducerImpl
import com.github.Mojashi.automaton.impl.ToVectorOutputTransducerConv.ToVectorOutputTransducerConv
import com.github.Mojashi.automaton.impl.ToParikhVectorTransducer.NFA_Parikh
import com.github.Mojashi.solver.algorithm.Implicits.{DoubleDoubleNumericCast, IntDoubleNumericCast}
import com.github.Mojashi.solver.mp.MIPSinglePointSolver
import com.github.Mojashi.solver.smt.SMTConventionalExactSolver
import com.github.Mojashi.utils.{NFAToDot, addVector, showDotInBrowser}
import dk.brics.automaton.RegExp
import org.scalatest.funsuite.AnyFunSuiteLike
import pcp.{PCP, Tile}
import utils.ConcatTransducer.ConcatTransducer
import utils.MapOutput.MapOutput
import utils.ParallelTransducer.ParallelTransducer
import utils.ShrinkMonoidTransducer.ShrinkMonoidTransducer
import utils.VectorMonoid.IntVectorMonoid
import watcher.{ConstLengthTransducer, SubStrCountTransducer}

class OverEstimatePCPTest extends AnyFunSuiteLike {
  com.google.ortools.Loader.loadNativeLibraries()

  test("check solvable") {
    val pcp = PCP(List(
      Tile("100", "1"),
      Tile("0", "100"),
      Tile("1", "00"),
    ))

    val words = Seq(
      "0", "1",
      "00", "11",
      "10", "01",
      "101", "010",
      "1001"
    )

    var watcher: VectorOutputTransducer[Char, String, Int] =
      ConstLengthTransducer.ConstLengthTransducer(
        1, pcp.alphabets
      ).mapOutput(o => Map((o, 1)))
        .concat(
          words
            .map(w => SubStrCountTransducer.SubStrCountTransducer(w, pcp.alphabets).mapOutput(o => Map((o, 1))))
            .reduce((t, t2) =>
              t.parallel(t2).mapOutput(o =>
                addVector(o._1.getOrElse(Map()), o._2.getOrElse(Map()))
              ).shrink
            )
        )

    println(s"transitions.size: ${watcher.transitions.size}, states.size: ${watcher.states.size}")
    println(s"size: ${watcher.states.count(s => watcher.sourceFrom(s).size == 1)}")

    watcher = watcher.shrink

    println(s"transitions.size: ${watcher.transitions.size}, states.size: ${watcher.states.size}")
    println(s"size: ${watcher.states.count(s => watcher.sourceFrom(s).size == 1)}")
    showDotInBrowser(watcher.toDot)

    val solver = new OverEstimatePCP(
      pcp,
      watcher,
      (pa: ParikhAutomaton[Int, String, Int]) =>
        new MIPSinglePointSolver[Int, String,Int](pa)
    )

    val ret = solver.solve
    val (o1, o2) = pcp.transduce(ret.get)
    println(ret)
    assert(words.forall(w =>
      countSubstring(o1.drop(1), w) == countSubstring(o2.drop(1), w)
    ))
    println(o1)
    println(o2)
  }


  def countSubstring(s: String, substr: String): Int = {
    s"$substr".r.findAllIn(s).length
  }
}
