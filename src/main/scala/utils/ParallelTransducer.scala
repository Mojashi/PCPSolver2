package utils

import com.github.Mojashi.automaton.impl.NFTransducerImpl
import com.github.Mojashi.automaton.{NFTransducer, TransducerTransition, TransducerTransitionImpl}
import com.github.Mojashi.graph.UniqueEdgeId
import utils.ShrinkMonoidTransducer.ShrinkMonoidTransducer
import utils.TrimNFA.{TrimNFA, TrimTransducer}

import scala.collection.mutable.ListBuffer

object ParallelTransducer {
  implicit class ParallelTransducer[In, Out1, Out2]
  (
    left: NFTransducer[In, Out1, TransducerTransition[In, Out1]]
  ) {
    def parallel(right: NFTransducer[In, Out2, TransducerTransition[In, Out2]]):
      NFTransducer[In, (Option[Out1], Option[Out2]), TransducerTransition[In, (Option[Out1], Option[Out2])]] = {
      val nt = ListBuffer[TransducerTransition[In, (Option[Out1], Option[Out2])]]()

      nt ++= left.transitions.filter(t1 => t1.in.isDefined).flatMap(t1 =>
        right.transitions.filter(t2 => t2.in == t1.in).map(t2 => {
          TransducerTransitionImpl(
            from = (t1.from, t2.from).toString,
            to = (t1.to, t2.to).toString,
            in = t1.in,
            out = Some((t1.out, t2.out)),
            id = UniqueEdgeId.get()
          )
        })
      )

      nt ++= left.transitions.filter(t1 => t1.in.isEmpty).flatMap(t1 =>
        right.states.map(s => {
          TransducerTransitionImpl(
            from = (t1.from, s).toString,
            to = (t1.to, s).toString,
            in = None,
            out = Some((t1.out, None)),
            id = UniqueEdgeId.get()
          )
        })
      )

      nt ++= right.transitions.filter(t2 => t2.in.isEmpty).flatMap(t2 =>
        left.states.map(s => {
          TransducerTransitionImpl(
            from = (s, t2.from).toString,
            to = (s, t2.to).toString,
            in = None,
            out = Some((None, t2.out)),
            id = UniqueEdgeId.get()
          )
        })
      )

      new TrimTransducer[In, (Option[Out1], Option[Out2]), TransducerTransition[In, (Option[Out1], Option[Out2])]](new NFTransducerImpl(
        start = (left.start, right.start).toString,
        fin = (left.fin, right.fin).toString,
        transitions = nt.toSeq.map(t =>
          t.out match {
            case Some((None, None)) =>
              TransducerTransitionImpl(
                from = t.from,
                to = t.to,
                in = t.in,
                out = None,
                id = t.id,
              )
            case _ => t
          }
        )
      )).trim
    }
  }
}
