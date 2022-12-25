package watcher

import com.github.Mojashi.automaton.impl.NFTransducerImpl
import com.github.Mojashi.automaton.{NFTransducer, TransducerTransition, TransducerTransitionImpl}
import com.github.Mojashi.graph.UniqueEdgeId

object SubStrCountTransducer {
  def SubStrCountTransducer(word: String, alphabets: Set[Char]): NFTransducer[Char, String, TransducerTransition[Char,String]] = {
    val baseTransitions = word.indices.flatMap(idx =>
      alphabets.toSeq.map(other => {
        val newWord = word.substring(0, idx) + other
        val jumpTo =
          (0 to newWord.length).findLast(l => word.substring(0, l) == newWord.substring(newWord.length - l, newWord.length)).get % word.length

        TransducerTransitionImpl[Char, String](
          from = s"$idx",
          to = s"$jumpTo",
          in = Some(other),
          out = if (jumpTo <= idx) Some(s"${word}_${idx}_${other}") else None,
          id = UniqueEdgeId.get
        )
      })
    )

    val fin = "fin"
    val finTransitions = baseTransitions.map(t => t.from).distinct.map(s =>
      TransducerTransitionImpl[Char, String](
        from = s,
        to = fin,
        in = None,
        out = None,
        id = UniqueEdgeId.get
      )
    )

    new NFTransducerImpl(
      start = s"0",
      fin = fin,
      transitions = baseTransitions ++ finTransitions
    )
  }
}