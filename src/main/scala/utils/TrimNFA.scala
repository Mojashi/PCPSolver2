package utils

import com.github.Mojashi.automaton.impl.{NFAImpl, NFTransducerImpl}
import com.github.Mojashi.automaton.{NFA, NFTransducer, TransducerTransition, Transition}
import utils.GraphReachables.GraphReachables

object TrimNFA {
  implicit class TrimNFA[In, E <: Transition[In]]
  (
    nfa: NFA[In, E]
  ) {
    def trim: NFA[In, E] = {
      val validStates = GraphReachables(nfa).reachables(nfa.start).intersect(GraphReachables(GraphReachables(nfa).reversed).reachables(nfa.fin))

      new NFAImpl(
        start = nfa.start,
        fin = nfa.fin,
        transitions = nfa.transitions.filter(t =>
          validStates.contains(t.from) && validStates.contains(t.to)
        )
      )
    }
  }

  implicit class TrimTransducer[In, Out, E <: TransducerTransition[In, Out]]
  (
    nfa: NFTransducer[In, Out, E]
  ) {
    def trim: NFTransducer[In, Out, E] = {
      val validStates = GraphReachables(nfa).reachables(nfa.start).intersect(GraphReachables(GraphReachables(nfa).reversed).reachables(nfa.fin))

      new NFTransducerImpl(
        start = nfa.start,
        fin = nfa.fin,
        transitions = nfa.transitions.filter(t =>
          validStates.contains(t.from) && validStates.contains(t.to)
        )
      )
    }
  }
}

