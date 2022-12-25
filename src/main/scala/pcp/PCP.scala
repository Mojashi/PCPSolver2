package pcp

import com.github.Mojashi.automaton.{NFTransducer, TransducerTransition, TransducerTransitionImpl}
import com.github.Mojashi.automaton.impl.NFTransducerImpl
import com.github.Mojashi.graph.UniqueEdgeId

case class Tile(u: String, d: String)

case class PCP
(
  tiles: Seq[Tile]
) {
  def reversed(): PCP = PCP(tiles.map(tile => Tile(u = tile.u.reverse, d = tile.d.reverse)))

  val alphabets = tiles.flatMap(tile => tile.d ++ tile.u).toSet
  val transducers =
    (toTransducer(tiles.map(t => t.u).toList, "t1"), toTransducer(tiles.map(t => t.d).toList, "t2"))

  def transduce(word: Seq[Int]): (String, String) = (
    word.foldLeft("")((s, idx) => s + tiles(idx).u),
    word.foldLeft("")((s, idx) => s + tiles(idx).d),
  )


  private def toTransducer(ss: List[String], prefix: String=""):
    NFTransducer[Int, Char, TransducerTransition[Int, Char]] =
    new NFTransducerImpl(
      s"${prefix}_q", s"${prefix}_q", ss.zipWithIndex.flatMap{ case (s, wordIdx) =>
        s.zipWithIndex.map { case (ch, chIdx) =>
          TransducerTransitionImpl(
            from = if(chIdx==0) s"${prefix}_q" else s"${prefix}_q_${wordIdx}_${chIdx-1}",
            to = if(chIdx==s.length-1) s"${prefix}_q" else s"${prefix}_q_${wordIdx}_${chIdx}",
            in = if(chIdx==0) Some(wordIdx) else None,
            out = Some(ch),
            id = UniqueEdgeId.get()
          )
        }
      }
    )
}