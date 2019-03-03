package matching.regexp

import org.scalatest._
import RegExp._

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

  "constructMorphs" should "construct morphs which simulates exhaustive search" in {
    val r0 = parseWithStartEnd("a*a*b|ba")
    val r1 = parseWithStartEnd("a*a*b")
    val r2 = parseWithStartEnd("a*b")
    val r3 = parseWithStartEnd("a")
    val r4 = parseWithStartEnd("ε")
    val indexedMorphs = constructMorphs[List](r0)
    indexedMorphs.morphs should contain only (
      Some('a') -> Map(
        r0 -> Seq(r1,r2),
        r1 -> Seq(r1,r2),
        r2 -> Seq(r2),
        r3 -> Seq(r4),
        r4 -> Seq()
      ),
      Some('b') -> Map(
        r0 -> Seq(r4,r3),
        r1 -> Seq(r4),
        r2 -> Seq(r4),
        r3 -> Seq(),
        r4 -> Seq()
      ),
      None -> Map(
        r0 -> Seq(),
        r1 -> Seq(),
        r2 -> Seq(),
        r3 -> Seq(),
        r4 -> Seq()
      )
    )
    indexedMorphs.initials should be (Set(r0))
    indexedMorphs.finals should be (Set(r4))
  }
}
