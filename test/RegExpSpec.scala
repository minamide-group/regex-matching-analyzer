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

  "RepeatExp.apply()" should "transform expression properly" in {
    val r = ElemExp('a')
    RepeatExp(r,Some(2),Some(5),true) should be (new RepeatExp(r,Some(2),Some(5),true))
    RepeatExp(r,None,Some(5),true) should be (new RepeatExp(r,None,Some(5),true))
    RepeatExp(r,Some(0),Some(5),true) should be (new RepeatExp(r,None,Some(5),true))
    RepeatExp(r,Some(0),Some(0),true) should be (EpsExp())
    RepeatExp(r,Some(0),None,true) should be (StarExp(r,true))

    a [InvalidRegExpException] should be thrownBy {RepeatExp(r,Some(5),Some(2),true)}
    a [InvalidRegExpException] should be thrownBy {RepeatExp(r,None,None,true)}
    a [InvalidRegExpException] should be thrownBy {RepeatExp(r,Some(-1),None,true)}
    a [InvalidRegExpException] should be thrownBy {RepeatExp(r,None,Some(-1),true)}
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
  }
}
