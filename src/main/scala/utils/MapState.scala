package utils

import com.github.Mojashi.automaton.impl.{NFAImpl, NFTransducerImpl}
import com.github.Mojashi.automaton.{NFA, NFTransducer, TransducerTransition, TransducerTransitionImpl, Transition, TransitionImpl}
import com.github.Mojashi.graph.StateID

object MapState {
  implicit class MapStateNFA[In, T <: Transition[In]]
  (
    t: NFA[In, T]
  ){
    def mapState
    (
      fn: StateID=>StateID
    ): NFA[In, Transition[In]] = {
      new NFAImpl[In, Transition[In]](
        start = fn(t.start),
        fin = fn(t.fin),
        transitions = t.transitions.map(t =>
          TransitionImpl(
            from = fn(t.from),
            to = fn(t.to),
            in = t.in,
            id = t.id
          )
        )
      )
    }
  }

  implicit class MapStateTransducer[In, Out, T <: TransducerTransition[In, Out]]
  (
    t: NFTransducer[In, Out, T]
  ) {
    def mapState
    (
      fn: StateID => StateID
    ): NFTransducer[In,Out, TransducerTransition[In, Out]] = {
      new NFTransducerImpl[In,Out, TransducerTransition[In, Out]](
        start = fn(t.start),
        fin = fn(t.fin),
        transitions = t.transitions.map(t =>
          TransducerTransitionImpl(
            from = fn(t.from),
            to = fn(t.to),
            in = t.in,
            out = t.out,
            id = t.id
          )
        )
      )
    }
  }
}
