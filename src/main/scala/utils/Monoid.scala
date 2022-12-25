package utils

import com.github.Mojashi.utils.addVector

trait Monoid[V] {
  def unit: V
  def plus(l: V, r: V): V
}

object VectorMonoid {
  class VectorMonoid[K, V: Numeric] extends Monoid[Map[K, V]] {
    override def unit: Map[K, V] = Map()

    override def plus(l: Map[K, V], r: Map[K, V]): Map[K, V] =
      addVector(l, r)
  }


  implicit val IntVectorMonoid: Monoid[Map[String, Int]] = new VectorMonoid()
}
