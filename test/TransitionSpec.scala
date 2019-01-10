import org.scalatest._

import transition._
import Morph._

class TransitionSpec extends FlatSpec with Matchers {
  "morphs2Graph" should "generate graph correctly" in {
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
      ('a', 1, 'a'),
      ('a', 1, 'b'),
      ('b', 1, 'b'),
      ('b', 1, 'c'),
      ('c', 1, 'a'),
      ('a', 2, 'a'),
      ('a', 2, 'b'),
      ('a', 2, 'c'),
      ('b', 2, 'b')
    )
  }

  "calcStrongComponents" should "calculate strong components correctly" in {
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

    g.calcStrongComponents().map(_.toSet) should contain only (
      Set(1,2), Set(3), Set(4,5,6,7), Set(8)
    )
  }

  "toG4" should "calculate G4 correctly" in {
    val g = new LabeledGraph(
      Seq(1,2),
      Seq(
        (1,'a',1),
        (1,'a',2),
        (2,'b',2)
      )
    )

    val g4 = g.toG4
    g4.nodes should contain only (
      (1,1,1),(1,1,2),(1,2,1),(1,2,2),
      (2,1,1),(2,1,2),(2,2,1),(2,2,2)
    )
    g4.edges should contain only (
      ((1,1,1),(1,1,1)),
      ((1,1,1),(1,1,2)),
      ((1,1,1),(1,2,1)),
      ((1,1,1),(1,2,2)),
      ((1,1,1),(2,1,1)),
      ((1,1,1),(2,1,2)),
      ((1,1,1),(2,2,1)),
      ((1,1,1),(2,2,2)),
      ((2,2,2),(2,2,2)),
      ((1,2,2),(1,1,2)),
      ((2,1,1),(2,2,1))
    )
  }
}
