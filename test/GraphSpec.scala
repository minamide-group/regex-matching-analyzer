package matching.transition

import org.scalatest._

class GraphSpec extends FlatSpec with Matchers {
  "reverse" should "construct reverse graph" in {
    val g = new Graph(
      Seq(1,2,3,4),
      Seq(
        (1,2),
        (1,3),
        (2,3),
        (3,4)
      )
    )

    val gRev = g.reverse()
    gRev.nodes should contain theSameElementsAs Seq(1,2,3,4)
    gRev.edges should contain theSameElementsAs Seq((2,1),(3,1),(3,2),(4,3))
  }

  "reachableFrom" should "calculate reachable nodes from given node" in {
    val g = new Graph(
      Seq(1,2,3,4,5,6,7,8,9),
      Seq(
        (1,2),
        (2,3),
        (3,4),
        (4,2),
        (3,5),
        (5,6),
        (5,7),
        (6,8),
        (7,8),
        (9,9)
      )
    )

    g.reachableFrom(1) should be (Set(1,2,3,4,5,6,7,8))
    g.reachableFrom(2) should be (Set(2,3,4,5,6,7,8))
    g.reachableFrom(3) should be (Set(2,3,4,5,6,7,8))
    g.reachableFrom(4) should be (Set(2,3,4,5,6,7,8))
    g.reachableFrom(5) should be (Set(5,6,7,8))
    g.reachableFrom(6) should be (Set(6,8))
    g.reachableFrom(7) should be (Set(7,8))
    g.reachableFrom(8) should be (Set(8))
    g.reachableFrom(9) should be (Set(9))
  }

  "calcStrongComponents" should "calculate strong components" in {
    val g = new Graph(
      Seq(1,2,3,4,5,6,7,8),
      Seq(
        (1,2),
        (2,1),
        (2,3),
        (3,4),
        (4,5),
        (5,6),
        (6,4),
        (6,7),
        (7,6),
        (8,8)
      )
    )

    g.calcStrongComponents() should be (
      Set(Set(1,2), Set(3), Set(4,5,6,7), Set(8))
    )
  }
}
