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
      ).mapValues(_.toList),
      Map(
        'a' -> "cba",
        'b' -> "b"
      ).mapValues(_.toList)
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

  "calcAmbiguity" should "decide exponentially ambiguous graph" in {
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
