package matching.regexp

import org.scalatest._
import RegExp._
import matching.monad._

class RegExpSpec extends FlatSpec with Matchers {
  def parseWithStartEnd(s: String): RegExp[Char] = {
    RegExpParser(s"^${s}${"$"}")
  }

  "optConcatExp" should "concat expressions with optimization on ε" in {
    optConcatExp(ElemExp('a'),ElemExp('b')) should be (ConcatExp(ElemExp('a'),ElemExp('b')))
    optConcatExp(EpsExp(),ElemExp('b')) should be (ElemExp('b'))
    optConcatExp(ElemExp('a'),EpsExp()) should be (ElemExp('a'))
    optConcatExp(EpsExp(),EpsExp()) should be (EpsExp())
  }

  it should "concat expressions with optimization on repeat expression" in {
    optConcatExp(ElemExp('a'),RepeatExp(ElemExp('a'),Some(3),Some(5),true)) should be (
      RepeatExp(ElemExp('a'),Some(4),Some(6),true)
    )
    optConcatExp(ElemExp('a'),RepeatExp(ElemExp('a'),None,Some(5),false)) should be (
      RepeatExp(ElemExp('a'),Some(1),Some(6),false)
    )
    optConcatExp(ElemExp('a'),RepeatExp(ElemExp('a'),Some(3),None,true)) should be (
      RepeatExp(ElemExp('a'),Some(4),None,true)
    )
    optConcatExp(ElemExp('a'),RepeatExp(ElemExp('b'),Some(3),Some(5),true)) should be (
      ConcatExp(ElemExp('a'),RepeatExp(ElemExp('b'),Some(3),Some(5),true))
    )
  }

  "constructTransducer" should "construct transducer which simulates exhaustive search" in {
    val r0 = parseWithStartEnd("a*a*b|ba")
    val r1 = parseWithStartEnd("a*a*b")
    val r2 = parseWithStartEnd("a*b")
    val r3 = parseWithStartEnd("a")
    val r4 = parseWithStartEnd("ε")
    val transducer = constructTransducer(r0)
    transducer.states should be (Set(r0,r1,r2,r3,r4))
    transducer.sigma should be (Set(Some('a'),Some('b'),None))
    transducer.initialState should be (r0)
    transducer.delta should have size (20)
    // transducer.delta should contain allOf (
    //   (r0,Some(Some('a'))) -> Or(Leaf(r1),Leaf(r2)),
    //   (r0,Some(Some('b'))) -> Or(Leaf(r4),Leaf(r3)),
    //   (r0,Some(None)) -> Fail,
    //   (r0,None) -> Fail
    // )
    // indexedMorphs.morphs should contain only (
    //   Some('a') -> Map(
    //     r0 -> Seq(r1,r2),
    //     r1 -> Seq(r1,r2),
    //     r2 -> Seq(r2),
    //     r3 -> Seq(r4),
    //     r4 -> Seq()
    //   ),
    //   Some('b') -> Map(
    //     r0 -> Seq(r4,r3),
    //     r1 -> Seq(r4),
    //     r2 -> Seq(r4),
    //     r3 -> Seq(),
    //     r4 -> Seq()
    //   ),
    //   None -> Map(
    //     r0 -> Seq(),
    //     r1 -> Seq(),
    //     r2 -> Seq(),
    //     r3 -> Seq(),
    //     r4 -> Seq()
    //   )
    // )
    // indexedMorphs.initials should be (Set(r0))
    // indexedMorphs.finals should be (Set(r4))
  }
}
