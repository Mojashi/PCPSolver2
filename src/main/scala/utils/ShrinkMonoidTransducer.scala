package utils

import com.github.Mojashi.automaton.impl.NFTransducerImpl
import com.github.Mojashi.automaton.{NFTransducer, TransducerTransition, TransducerTransitionImpl}
import com.github.Mojashi.graph.{StateID, UniqueEdgeId}
import utils.TrimNFA.TrimTransducer

import scala.collection.mutable


object ShrinkMonoidTransducer {
  implicit class ShrinkMonoidTransducer[In, Out]
  (
    t: NFTransducer[In, Out, TransducerTransition[In, Out]]
  ){
    val alphabets = t.transitions.flatMap(t=>t.in).distinct

    case class Path
    (
      input: Seq[In],
      fin: StateID,
      output: Out,
    )
    case class FinPath
    (
      input: Seq[In],
      output: Out,
    )

    def shrink(implicit m: Monoid[Out]): NFTransducer[In, Out, TransducerTransition[In, Out]] = {
      var bef = t.transitions.size + 1
      var ret = t
      while(bef != ret.transitions.size) {
        //println(s"bef : ${bef}")
        bef = ret.transitions.size
        ret = ret.shrinkNext.shrinkSingleOut
      }
      ret
    }

    def shrinkNext(implicit m: Monoid[Out]): NFTransducer[In, Out, TransducerTransition[In, Out]] = {
      val newStateMap = t.states
        .groupBy(s =>
          t.sourceFrom(s).map(ts => (ts.to, ts.in, ts.out.getOrElse(m.unit), s==t.start, s==t.fin)).toSet
        )
        .flatMap{ case (p, ss) => ss.map(s => (s, ss.head)) }


      new NFTransducerImpl(
        start = newStateMap(t.start),
        fin = newStateMap(t.fin),
        transitions = t.transitions.map(t =>
          TransducerTransitionImpl(
            from = newStateMap(t.from),
            to = newStateMap(t.to),
            in = t.in,
            out = t.out,
            id = t.id
          )
        ).distinctBy(t =>
          (t.from, t.to, t.in.getOrElse(m.unit), t.out.getOrElse(m.unit))
        )
      )
    }

    def shrinkSingleOut(implicit m: Monoid[Out]): NFTransducer[In, Out, TransducerTransition[In, Out]] = {
      val targets = t.states.filter(s =>
        s != t.fin && s != t.start &&
          t.sourceFrom(s).size == 1 &&
          t.sourceFrom(s).head.from != t.sourceFrom(s).head.to
      )
      val sourceFroms = mutable.Map.from(mutable.Set.from(t.transitions).groupBy(t=>t.from))
      val targetTos = mutable.Map.from(mutable.Set.from(t.transitions).groupBy(t=>t.to))

      for(target <- targets) {
        if(targetTos.contains(target)) {
          val lefts = targetTos(target)
          val rights = sourceFroms(target)
          assert(rights.size == 1)

          sourceFroms.remove(target)
          targetTos.remove(target)
          lefts.foreach(l => sourceFroms(l.from).remove(l))
          rights.foreach(r => targetTos(r.to).remove(r))

          lefts.foreach(l =>
            rights.foreach(r => {
              val ts = TransducerTransitionImpl(
                from = l.from,
                to = r.to,
                in = l.in,
                out = Some(m.plus(l.out.getOrElse(m.unit), r.out.getOrElse(m.unit))),
                id = UniqueEdgeId.get(),
              )
              targetTos(r.to).add(ts)
              sourceFroms(l.from).add(ts)
            })
          )
        }
      }

      new NFTransducerImpl[In, Out, TransducerTransition[In, Out]] (
        start = t.start,
        fin = t.fin,
        transitions = sourceFroms.flatMap{ case (_,ts)=>ts.toSeq }.toSeq
      ).trim
    }

    private def enumerateReachable(from: StateID, depth: Int)(implicit m: Monoid[Out]): Set[Path] = {
      val reached = mutable.Set.empty[Path]

      def recPath(cur: StateID, cPath: Path): Set[Path] = {
        if(cPath.input.length == depth) {
          Set(cPath)
        } else {
          (for {
            e <- t.sourceFrom(cur)
            p <- recPath(e.to, Path(
              input = cPath.input ++ e.in.toSeq,
              fin = e.to,
              output = m.plus(cPath.output, e.out.getOrElse(m.unit))
            ))
          } yield p).toSet
        }
      }

      recPath(from, Path(
        input = Seq(),
        fin = from,
        output = m.unit
      ))
    }

    private def enumerateFinPath(from: StateID, depth: Int)(implicit m: Monoid[Out]): Set[FinPath] = {

      def recFinPath(cur: StateID, cureDepth: Int): Set[FinPath] = {
        Set()
      }
      ???
    }
  }
}
