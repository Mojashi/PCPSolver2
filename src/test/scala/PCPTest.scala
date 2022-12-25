import com.github.Mojashi.utils.{NFAToDot, showDotInBrowser}
import pcp.{PCP, Tile}

class PCPTest extends org.scalatest.funsuite.AnyFunSuiteLike {
  test("transduce") {
    val pcp = PCP(List(Tile("1111", "11"), Tile("111", "1110"), Tile("0", "111")))
    val (u,d) = pcp.transduce(Seq(0,1,1,2))
    assert(u == "11111111110")
    assert(d == "1111101110111")
  }

  test("transducer") {
    val pcp = PCP(List(Tile("1111", "11"), Tile("111", "1110"), Tile("0", "111")))
    val input = Seq(0,1,1,2)
    val (ut, dt) = pcp.transducers
    val (o1, o2) = (ut.run(input), dt.run(input))
    val (a1, a2) = pcp.transduce(input)
    assert(o1.size == 1)
    assert(o2.size == 1)

    showDotInBrowser(ut.toDot)

    assert(a1 == o1.head.mkString)
    assert(a2 == o2.head.mkString)
  }


}
