import dk.brics.automaton.RegExp
import com.github.Mojashi.automaton.impl.BricsNFAAdapter.BricsNFAAdapter
import com.github.Mojashi.solver.ParikhAutomatonSolver
import com.github.Mojashi.automaton.ParikhAutomaton
import com.github.Mojashi.automaton.impl.ToParikhVectorTransducer.NFA_Parikh
import com.github.Mojashi.formula.{And, Constant, EQ, GTEQ, Sub, Times, Var}
import com.github.Mojashi.solver.algorithm.Implicits.IntDoubleNumericCast
import com.github.Mojashi.solver.mp.{MIPSinglePointSolver, getInputFromNEU}
import com.github.Mojashi.solver.smt.SMTConventionalExactSolver

object Main {
  def main(args: Array[String]): Unit = {
    com.google.ortools.Loader.loadNativeLibraries()

    val solverMakers = Seq(
      (pa: ParikhAutomaton[Char, Char, Int]) => new MIPSinglePointSolver(pa),
      (pa: ParikhAutomaton[Char, Char, Int]) => new SMTConventionalExactSolver(pa)
    )

    val r = new RegExp("xy+z+k*l")
    val a = r.toAutomaton().toNFA.toParikhVectorTransducer

    val pa = ParikhAutomaton(
      constraint = And(Seq(
        GTEQ(Var[Char, Int]('z'), Constant[Char, Int](11)),
        GTEQ(Var[Char, Int]('z'), Var[Char, Int]('k')),
        EQ(Var[Char, Int]('z'), Times(Constant[Char, Int](2), Var[Char, Int]('y'))),
      )),
      voa = a
    )

    //showDotInBrowser(a.toDot)
    def testIt(solver: ParikhAutomatonSolver[Char, Char, Int]) = {
      val inputOpt = for {
        neu <- solver.solve()
        in <- Some(getInputFromNEU(pa.voa, neu))
      } yield in
      assert(inputOpt.isDefined)

      val input = inputOpt.get
      println(s"ans: ${input.mkString}")

      assert(input.count(ch => ch == 'z') >= 11)
      assert(input.count(ch => ch == 'z') == 2 * input.count(ch => ch == 'y'))
    }

    solverMakers.foreach(maker => testIt(maker(pa)))
  }
}
