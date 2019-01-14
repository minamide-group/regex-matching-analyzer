import org.scalatest._

import transition._
import Morph._

class TransitionSpec extends FlatSpec with Matchers {
  "morphs2Graph" should "construct graph" in {
    val morphs = Seq(
      Map(
        'a' -> "ab",
        'b' -> "bcc",
        'c' -> "a"
      ).mapValues(_.toSeq),
      Map(
        'a' -> "cba",
        'b' -> "b"
      ).mapValues(_.toSeq)
    )

    val g = morphs2Graph(morphs)
    g.nodes should contain only ('a','b','c')
    g.labeledEdges should contain only (
      ('a', 0, 'a'),
      ('a', 0, 'b'),
      ('b', 0, 'b'),
      ('b', 0, 'c'),
      ('c', 0, 'a'),
      ('a', 1, 'a'),
      ('a', 1, 'b'),
      ('a', 1, 'c'),
      ('b', 1, 'b')
    )
  }

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
    gRev.nodes contains only (1,2,3,4)
    gRev.edges contains only ((2,1),(3,1),(3,2),(4,3))
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

    g.reachableFrom(1) should contain only (1,2,3,4,5,6,7,8)
    g.reachableFrom(2) should contain only (2,3,4,5,6,7,8)
    g.reachableFrom(3) should contain only (2,3,4,5,6,7,8)
    g.reachableFrom(4) should contain only (2,3,4,5,6,7,8)
    g.reachableFrom(5) should contain only (5,6,7,8)
    g.reachableFrom(6) should contain only (6,8)
    g.reachableFrom(7) should contain only (7,8)
    g.reachableFrom(8) should contain only (8)
    g.reachableFrom(9) should contain only (9)
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

    g.calcStrongComponents() should contain only (
      Set(1,2), Set(3), Set(4,5,6,7), Set(8)
    )
  }


  "calcAmbiguity" should "decide finitely ambiguous graph" in {
    val g1 = new LabeledGraph(
      Seq(1,2,3),
      Seq(
        (1,'a',1),
        (1,'b',2),
        (2,'a',2)
      )
    )

    val g2 = new LabeledGraph(
      Seq(1,2,3,4),
      Seq(
        (1,'a',1),
        (1,'a',2),
        (2,'b',1),
        (2,'b',3),
        (3,'c',4),
        (4,'b',3)
      )
    )

    g1.calcAmbiguity() should be (Some(0))
    g2.calcAmbiguity() should be (Some(0))
  }

  it should "decide polynomially ambiguous graph" in {
    val g11 = new LabeledGraph(
      Seq(1,2),
      Seq(
        (1,'a',1),
        (1,'a',2),
        (2,'a',2)
      )
    )

    val g12 = new LabeledGraph(
      Seq(1,2,3,4),
      Seq(
        (1,'a',1),
        (1,'a',2),
        (2,'b',1),
        (2,'b',3),
        (3,'a',4),
        (4,'b',3)
      )
    )

    val g21 = new LabeledGraph(
      Seq(1,2,3,4),
      Seq(
        (1,'a',1),
        (1,'a',2),
        (2,'a',2),
        (2,'b',3),
        (3,'c',3),
        (3,'c',4),
        (4,'c',4),
      )
    )

    val g22 = new LabeledGraph(
      Seq(1,2,3,4,5,6),
      Seq(
        (1,'x',2),
        (2,'y',1),
        (2,'a',2),
        (2,'a',3),
        (3,'a',3),
        (2,'y',4),
        (4,'x',5),
        (5,'y',4),
        (4,'z',4),
        (4,'z',6),
        (6,'z',6)
      )
    )

    val g3 = new LabeledGraph(
      Seq(1,2,3,4),
      Seq(
        (1,'a',1),
        (1,'a',2),
        (2,'a',2),
        (2,'b',2),
        (2,'b',3),
        (3,'b',3),
        (3,'c',3),
        (3,'c',4),
        (4,'c',4)
      )
    )

    g11.calcAmbiguity() should be (Some(1))
    g12.calcAmbiguity() should be (Some(1))
    g21.calcAmbiguity() should be (Some(2))
    g22.calcAmbiguity() should be (Some(2))
    g3.calcAmbiguity() should be (Some(3))
  }

  it should "decide exponentially ambiguous graph" in {
    val g1 = new LabeledGraph(
      Seq(1,2,3),
      Seq(
        (1,'a',2),
        (1,'a',3),
        (2,'a',1),
        (3,'a',1)
      )
    )

    val g2 = new LabeledGraph(
      Seq(1,2,3),
      Seq(
        (1,'a',1),
        (1,'b',2),
        (1,'a',3),
        (2,'c',1),
        (3,'b',3),
        (3,'c',1)
      )
    )

    val g3 = new LabeledGraph(
      Seq(1,2,3,4,5,6),
      Seq(
        (1,'a',2),
        (2,'b',3),
        (3,'c',2),
        (2,'b',4),
        (4,'c',5),
        (5,'b',3),
        (2,'d',6),
        (6,'e',6)
      )
    )

    g1.calcAmbiguity() should be (None)
    g2.calcAmbiguity() should be (None)
    g3.calcAmbiguity() should be (None)
  }
}
