package utils

import com.github.Mojashi.automaton.impl.NFTransducerImpl
import com.github.Mojashi.automaton.{NFTransducer, TransducerTransition, TransducerTransitionImpl}

object InverseTransducer {
  implicit class InverseTransducer[In, Out, T <: TransducerTransition[In, Out]]
  (
    t: NFTransducer[In, Out, T]
  ) {
    def inverse: NFTransducer[Out, In, TransducerTransition[Out,In]] = {
      new NFTransducerImpl (
        start = t.start,
        fin = t.fin,
        transitions = t.transitions.map(e =>
          TransducerTransitionImpl(
            from = e.from,
            to = e.to,
            in = e.out,
            out = e.in,
            id = e.id
          )
        )
      )
    }
  }

}
