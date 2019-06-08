package matching.transition

import org.scalatest._

class DT0LSpec extends FlatSpec with Matchers {
  // "calcAmbiguity" should "decide finitely ambiguous graph" in {
  //   val g1 = new NFA(
  //     Set(1,2),
  //     Set('a','b'),
  //     Seq(
  //       (1,'a',1),
  //       (1,'b',2),
  //       (2,'a',2)
  //     ),
  //     Set(1),
  //     Set(2)
  //   )
  //
  //   val g2 = new NFA(
  //     Set(1,2,3,4),
  //     Set('a','b','c'),
  //     Seq(
  //       (1,'a',2),
  //       (2,'b',3),
  //       (3,'c',4)
  //     ),
  //     Set(1),
  //     Set(4)
  //   )
  //
  //   val g3 = new NFA(
  //     Set(1,2,3,4,5),
  //     Set('a','b','c'),
  //     Seq(
  //       (1,'a',1),
  //       (1,'a',2),
  //       (2,'b',1),
  //       (2,'b',3),
  //       (2,'b',3),
  //       (3,'c',4),
  //       (4,'b',3),
  //       (5,'a',5)
  //     ),
  //     Set(1,5),
  //     Set(4)
  //   )
  //
  //   g1.calcAmbiguity()._1 should be (Some(0))
  //   g2.calcAmbiguity()._1 should be (Some(0))
  //   g3.calcAmbiguity()._1 should be (Some(0))
  // }
  //
  // it should "decide polynomially ambiguous graph" in {
  //   val g11 = new NFA(
  //     Set(1,2),
  //     Set('a'),
  //     Seq(
  //       (1,'a',1),
  //       (1,'a',2),
  //       (2,'a',2)
  //     ),
  //     Set(1),
  //     Set(2)
  //   )
  //
  //   val g12 = new NFA(
  //     Set(1,2,3,4),
  //     Set('a','b'),
  //     Seq(
  //       (1,'a',1),
  //       (1,'a',2),
  //       (2,'b',1),
  //       (2,'b',3),
  //       (3,'a',4),
  //       (4,'b',3)
  //     ),
  //     Set(1),
  //     Set(4)
  //   )
  //
  //   val g21 = new NFA(
  //     Set(1,2,3,4),
  //     Set('a','b','c'),
  //     Seq(
  //       (1,'a',1),
  //       (1,'a',2),
  //       (2,'a',2),
  //       (2,'b',3),
  //       (3,'c',3),
  //       (3,'c',4),
  //       (4,'c',4)
  //     ),
  //     Set(1),
  //     Set(4)
  //   )
  //
  //   val g22 = new NFA(
  //     Set(1,2,3,4,5,6),
  //     Set('a','x','y','z'),
  //     Seq(
  //       (1,'x',2),
  //       (2,'y',1),
  //       (2,'a',2),
  //       (2,'a',3),
  //       (3,'a',3),
  //       (2,'y',4),
  //       (4,'x',5),
  //       (5,'y',4),
  //       (4,'z',4),
  //       (4,'z',6),
  //       (6,'z',6)
  //     ),
  //     Set(1),
  //     Set(6)
  //   )
  //
  //   val g3 = new NFA(
  //     Set(1,2,3,4),
  //     Set('a','b','c'),
  //     Seq(
  //       (1,'a',1),
  //       (1,'a',2),
  //       (2,'a',2),
  //       (2,'b',2),
  //       (2,'b',3),
  //       (3,'b',3),
  //       (3,'c',3),
  //       (3,'c',4),
  //       (4,'c',4)
  //     ),
  //     Set(1),
  //     Set(4)
  //   )
  //
  //   g11.calcAmbiguity()._1 should be (Some(1))
  //   g12.calcAmbiguity()._1 should be (Some(1))
  //   g21.calcAmbiguity()._1 should be (Some(2))
  //   g22.calcAmbiguity()._1 should be (Some(2))
  //   g3.calcAmbiguity()._1 should be (Some(3))
  // }
  //
  // it should "decide exponentially ambiguous graph" in {
  //   val g1 = new NFA(
  //     Set(1,2,3),
  //     Set('a'),
  //     Seq(
  //       (1,'a',2),
  //       (1,'a',3),
  //       (2,'a',1),
  //       (3,'a',1)
  //     ),
  //     Set(1),
  //     Set(1)
  //   )
  //
  //   val g2 = new NFA(
  //     Set(1,2,3),
  //     Set('a','b','c'),
  //     Seq(
  //       (1,'a',1),
  //       (1,'b',2),
  //       (1,'a',3),
  //       (2,'c',1),
  //       (3,'b',3),
  //       (3,'c',1)
  //     ),
  //     Set(1),
  //     Set(1)
  //   )
  //
  //   val g3 = new NFA(
  //     Set(1,2,3,4,5,6),
  //     Set('a','b','c','d','e'),
  //     Seq(
  //       (1,'a',2),
  //       (2,'b',3),
  //       (3,'c',2),
  //       (2,'b',4),
  //       (4,'c',5),
  //       (5,'b',3),
  //       (2,'d',6),
  //       (6,'e',6)
  //     ),
  //     Set(1),
  //     Set(6)
  //   )
  //
  //   val g4 = new NFA(
  //     Set(1,2,3),
  //     Set('a','b'),
  //     Seq(
  //       (1,'a',2),
  //       (2,'b',3),
  //       (2,'b',3),
  //       (3,'a',2)
  //     ),
  //     Set(1),
  //     Set(3)
  //   )
  //
  //   val g5 = new NFA(
  //     Set(1,2,3),
  //     Set('a','b','c'),
  //     Seq(
  //       (1,'a',2),
  //       (2,'b',3),
  //       (3,'c',3),
  //       (3,'c',3)
  //     ),
  //     Set(1),
  //     Set(3)
  //   )
  //
  //   g1.calcAmbiguity()._1 should be (None)
  //   g2.calcAmbiguity()._1 should be (None)
  //   g3.calcAmbiguity()._1 should be (None)
  //   g4.calcAmbiguity()._1 should be (None)
  //   g5.calcAmbiguity()._1 should be (None)
  // }
}
