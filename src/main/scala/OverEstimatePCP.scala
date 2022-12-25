import com.github.Mojashi.automaton.{NFTransducer, ParikhAutomaton, TransducerTransition, TransducerTransitionImpl, VectorOutputTransducer}
import com.github.Mojashi.solver.ParikhAutomatonSolver
import com.github.Mojashi.automaton.impl.VectorOutputTransducerImpl
import com.github.Mojashi.formula.{And, Constant, EQ, GTEQ, Var}
import com.github.Mojashi.solver.mp.getInputFromNEU
import com.github.Mojashi.utils.{NFAToDot, showDotInBrowser, subVector}
import pcp.PCP
import utils.CombineTransducer.CombineTransducer
import utils.ParallelTransducer.ParallelTransducer

class OverEstimatePCP[Label]
(
  val pcp: PCP,
  val watcher: VectorOutputTransducer[Char, Label, Int],
  solverMaker:
    ParikhAutomaton[Int, Label, Int] =>
      ParikhAutomatonSolver[Int, Label, Int]
) {
  private val (outTU_W, outTD_W) = (
    pcp.transducers._1.combine(watcher),
    pcp.transducers._2.combine(watcher),
  )
  private val mult = outTU_W.parallel(outTD_W)
  private val diffPA = new VectorOutputTransducerImpl(
    start = mult.start,
    fin = mult.fin,
    transitions = mult.transitions.map(t =>
      TransducerTransitionImpl(
        from = t.from,
        to = t.to,
        in = t.in,
        out = t.out.flatMap(l => Some(subVector(
          l._1.getOrElse(Map()),
          l._2.getOrElse(Map()),
        ))),
        id = t.id
      )
    )
  )

  private val pa = ParikhAutomaton(
    voa = diffPA,
    constraint = And(
      watcher.transitions.flatMap(t => t.out.getOrElse(Map()).keys).distinct.map(label => EQ(
        Var[Label, Int](label),
        Constant[Label, Int](0)
      ))
    )
  )
  private val solver = solverMaker(pa)

  def solve = {
    val inputOpt = for {
      neu <- solver.solve()
      in <- Some(getInputFromNEU(pa.voa, neu))
    } yield (in, neu)

//    showDotInBrowser(diffPA.toDot(inputOpt.get._2))
    inputOpt.flatMap(o=>Some(o._1))
  }
}
