package utils

import com.github.Mojashi.automaton.{NFA, NFTransducer, TransducerTransition, Transition, TransitionImpl}
import utils.CombineTransducer.CombineTransducer
import utils.ExtractNFA.ExtractNFA
import utils.NFAToIdentityTransducer.NFAToIdentityTransducer

object BoundNFAToTransducer {
  implicit class BoundNFAToTransducer[In, Out]
  (
    t: NFTransducer[In, Out, TransducerTransition[In, Out]]
  ){
    def boundInput(nfa: NFA[In, Transition[In]]) = {
      ExtractNFA(CombineTransducer(NFAToIdentityTransducer(nfa).toIdentityTransducer).combine(t)).extractOutToNFA
    }

    def boundOutput(nfa: NFA[Out, Transition[Out]]):
      NFA[In, Transition[In]] = {
      ExtractNFA(CombineTransducer(t).combine(NFAToIdentityTransducer(nfa).toIdentityTransducer)).extractInToNFA
    }
  }
}
