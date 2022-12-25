package watcher

import com.github.Mojashi.automaton.{NFTransducer, TransducerTransition, TransducerTransitionImpl}
import com.github.Mojashi.automaton.impl.NFTransducerImpl
import com.github.Mojashi.graph.UniqueEdgeId

object ConstLengthTransducer {
  def ConstLengthTransducer(length: Int, alphabets: Set[Char]):
  NFTransducer[Char, String, TransducerTransition[Char,String]] = {
    new NFTransducerImpl(
      start = "0",
      fin = s"$length",
      transitions = for {
        idx <- 0 until length
        ch <- alphabets
      } yield TransducerTransitionImpl(
        from = idx.toString,
        to = (idx + 1).toString,
        in = Some(ch),
        out = Some(s"ch_${idx}_$ch"),
        id = UniqueEdgeId.get()
      )
    )
  }
}
