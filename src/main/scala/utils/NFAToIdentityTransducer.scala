package utils

import com.github.Mojashi.automaton.impl.NFTransducerImpl
import com.github.Mojashi.automaton.{NFA, NFTransducer, TransducerTransition, TransducerTransitionImpl, Transition}

object NFAToIdentityTransducer {
  implicit class NFAToIdentityTransducer[In, T <: Transition[In]]
  (
    nfa: NFA[In, T]
  ){
    def toIdentityTransducer: NFTransducer[In, In, TransducerTransition[In,In]] = {
      new NFTransducerImpl (
        start = nfa.start,
        fin = nfa.fin,
        transitions = nfa.transitions.map(t =>
          TransducerTransitionImpl(
            from = t.from,
            to = t.to,
            in = t.in,
            out = t.in,
            id = t.id,
          )
        )
      )
    }
  }
}
