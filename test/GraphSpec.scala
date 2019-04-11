package matching.transition

import org.scalatest._

class GraphSpec extends FlatSpec with Matchers {
  "adj" should "be adjacency list" in {
    val g = new Graph(
      Set(1,2,3,4),
      Seq(
        (1,2),
        (1,3),
        (1,3),
        (2,2),
        (3,1),
        (3,4)
      )
    )

    g.adj(1) should contain theSameElementsAs (Seq(2,3,3))
    g.adj(2) should contain theSameElementsAs (Seq(2))
    g.adj(3) should contain theSameElementsAs (Seq(1,4))
    g.adj(4) should contain theSameElementsAs (Seq())
  }

  "reverse" should "construct reverse graph" in {
    val g = new Graph(
      Set(1,2,3,4),
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

  "reachableMapDAG" should "calculate reachable nodes from each node in DAG" in {
    val g = new Graph(
      Seq(
        (1,2),
        (2,3),
        (2,4),
        (3,5),
        (4,5),
        (6,7)
      )
    )

    val m = g.reachableMapDAG()
    m(1) should be (Set(1,2,3,4,5))
    m(2) should be (Set(2,3,4,5))
    m(3) should be (Set(3,5))
    m(4) should be (Set(4,5))
    m(5) should be (Set(5))
    m(6) should be (Set(6,7))
    m(7) should be (Set(7))
  }

  "calcStrongComponents" should "calculate strong components" in {
    val g = new Graph(
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

  "labeledAdj" should "be adjacency list with label" in {
    val g = new LabeledGraph(
      Set(1,2,3,4),
      Seq(
        (1,'a',2),
        (1,'a',3),
        (1,'b',4),
        (2,'a',1),
        (2,'a',1),
        (2,'b',1),
        (3,'a',3)
      )
    )

    g.labeledAdj(1)('a') should contain theSameElementsAs (Seq(2,3))
    g.labeledAdj(1)('b') should contain theSameElementsAs (Seq(4))
    g.labeledAdj(2)('a') should contain theSameElementsAs (Seq(1,1))
    g.labeledAdj(2)('b') should contain theSameElementsAs (Seq(1))
    g.labeledAdj(3)('a') should contain theSameElementsAs (Seq(3))
    g.labeledAdj(3)('b') should contain theSameElementsAs (Seq())
    g.labeledAdj(4)('a') should contain theSameElementsAs (Seq())
    g.labeledAdj(4)('b') should contain theSameElementsAs (Seq())
  }
}
