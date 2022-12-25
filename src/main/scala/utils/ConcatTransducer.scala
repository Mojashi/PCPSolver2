package utils

import com.github.Mojashi.automaton.impl.NFTransducerImpl
import com.github.Mojashi.automaton.{NFTransducer, TransducerTransition, TransducerTransitionImpl}
import com.github.Mojashi.graph.UniqueEdgeId
import utils.MapState.MapStateTransducer
import utils.TrimNFA.TrimTransducer

object ConcatTransducer {
  implicit class ConcatTransducer[In, Out]
  (
    t: NFTransducer[In, Out, TransducerTransition[In,Out]]
  ){
    def concat(right: NFTransducer[In, Out, TransducerTransition[In, Out]]):
      NFTransducer[In, Out, TransducerTransition[In,Out]] = {
      val prefixedRight = right.mapState(s => s"RIGHT{$s}")
      val prefixedLeft = t.mapState(s => s"LEFT{$s}")

      new TrimTransducer[In, Out, TransducerTransition[In, Out]](
        new NFTransducerImpl[In, Out, TransducerTransition[In,Out]](
          start = prefixedLeft.start,
          fin = prefixedRight.fin,
          transitions = prefixedLeft.transitions ++ prefixedRight.transitions :+ TransducerTransitionImpl(
            from = prefixedLeft.fin,
            to = prefixedRight.start,
            in = None,
            out = None,
            id = UniqueEdgeId.get()
          )
        )
      ).trim
    }
  }
}
