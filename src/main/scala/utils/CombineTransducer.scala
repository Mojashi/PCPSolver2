package utils

import com.github.Mojashi.automaton.impl.NFTransducerImpl
import com.github.Mojashi.automaton.{NFTransducer, TransducerTransition, TransducerTransitionImpl}
import com.github.Mojashi.graph.UniqueEdgeId
import utils.TrimNFA.TrimTransducer

import scala.collection.mutable.ListBuffer

object CombineTransducer {

  implicit class CombineTransducer[In, Out, Out2]
  (
    val t: NFTransducer[In, Out, TransducerTransition[In, Out]]
  ) {
    def combine(right: NFTransducer[Out, Out2, TransducerTransition[Out, Out2]]):
      NFTransducer[In, Out2, TransducerTransition[In, Out2]] = {
      val newTransitions = ListBuffer[TransducerTransition[In, Out2]]()

      newTransitions ++= t.transitions.flatMap(t1 =>
        right.transitions
          .filter(t2 => t2.in == t1.out && t2.in.isDefined)
          .map(t2 =>
            TransducerTransitionImpl(
              from = (t1.from, t2.from).toString,
              to = (t1.to, t2.to).toString,
              in = t1.in,
              out = t2.out,
              id = UniqueEdgeId.get()
            )
          )
      )

      newTransitions ++= t.transitions.filter(t1 => t1.out.isEmpty).flatMap(t1 =>
        right.states.map(s2 =>
          TransducerTransitionImpl(
            from = (t1.from, s2).toString,
            to = (t1.to, s2).toString,
            in = t1.in,
            out = None,
            id = UniqueEdgeId.get()
          )
        )
      )

      newTransitions ++= right.transitions.filter(t2 => t2.in.isEmpty).flatMap(t2 =>
        t.states.map(s1 =>
          TransducerTransitionImpl(
            from = (s1, t2.from).toString,
            to = (s1, t2.to).toString,
            in = None,
            out = t2.out,
            id = UniqueEdgeId.get()
          )
        )
      )

      TrimTransducer[In, Out2, TransducerTransition[In, Out2]](new NFTransducerImpl(
        start = (t.start, right.start).toString,
        fin = (t.fin, right.fin).toString,
        transitions = newTransitions.toSeq
      )).trim
    }
  }
}
