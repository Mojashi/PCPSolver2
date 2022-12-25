package utils

import com.github.Mojashi.automaton.impl.NFAImpl
import com.github.Mojashi.automaton.{NFA, NFTransducer, TransducerTransition, TransducerTransitionImpl, Transition, TransitionImpl}

object ExtractNFA {
  implicit class ExtractNFA[In, Out, T <: TransducerTransition[In, Out]]
  (
    t: NFTransducer[In, Out, T]
  ) {
    def extractOutToNFA: NFA[Out, Transition[Out]] = {
      new NFAImpl (
        start = t.start,
        fin = t.fin,
        transitions = t.transitions.map(e => TransitionImpl(
          from = e.from,
          to = e.to,
          in = e.out,
          id = e.id,
        ))
      )
    }

    def extractInToNFA: NFA[In, Transition[In]] = {
      new NFAImpl(
        start = t.start,
        fin = t.fin,
        transitions = t.transitions.map(e => TransitionImpl(
          from = e.from,
          to = e.to,
          in = e.in,
          id = e.id,
        ))
      )
    }
  }

}