package utils

import com.github.Mojashi.automaton.TransitionImpl
import com.github.Mojashi.graph.{Edge, EdgeImpl, Graph, GraphImpl, StateID}

import scala.collection.mutable

object GraphReachables {
  implicit class GraphReachables
  (
    g: Graph[Edge]
  ){
    def reachables(from: StateID): Set[StateID] = {
      val reached = mutable.Set[StateID]()
      def dfs(cur: StateID): Unit = {
        if(!reached.contains(cur)) {
          reached.add(cur)
          for (next <- g.sourceFrom(cur)) {
            dfs(next.to)
          }
        }
      }

      dfs(from)
      reached.toSet
    }

    def reversed = {
      new GraphImpl(
        transitions = g.transitions.map(t => EdgeImpl(
          from = t.to,
          to = t.from,
          id = t.id,
        ))
      )
    }
  }
}
