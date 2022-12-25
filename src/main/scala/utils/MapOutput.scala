package utils

import com.github.Mojashi.automaton.impl.NFTransducerImpl
import com.github.Mojashi.automaton.{NFTransducer, TransducerTransition, TransducerTransitionImpl}

object MapOutput {
  implicit class MapOutput[In, Out]
  (
    t: NFTransducer[In, Out, TransducerTransition[In, Out]]
  ) {
    def mapOutput[NewOut]
    (
      fn: Out => NewOut
    ): NFTransducer[In, NewOut, TransducerTransition[In, NewOut]] = {
      new NFTransducerImpl[In, NewOut, TransducerTransition[In, NewOut]](
        start = t.start,
        fin = t.fin,
        transitions = t.transitions.map(t =>
          TransducerTransitionImpl(
            from = t.from,
            to = t.to,
            in = t.in,
            out = t.out.flatMap(o => Some(fn(o))),
            id = t.id
          )
        )
      )
    }
  }
}
