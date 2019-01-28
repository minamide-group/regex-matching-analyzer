package matching.regexp

import org.scalatest._
import RegExp._

class RegExpSpec extends FlatSpec with Matchers {
  "optConcatExp" should "concat expressions with optimization on ε" in {
    RegExp.optConcatExp(ElemExp('a'),ElemExp('b')) should be (ConcatExp(ElemExp('a'),ElemExp('b')))
    RegExp.optConcatExp(EpsExp(),ElemExp('b')) should be (ElemExp('b'))
    RegExp.optConcatExp(ElemExp('a'),EpsExp()) should be (ElemExp('a'))
    RegExp.optConcatExp(EpsExp(),EpsExp()) should be (EpsExp())
  }


  "derive" should "derive a" in {
    val r = RegExpParser("^a$")
    r.derive[List]('a') should be (List(Some(EpsExp())))
    r.derive[List]('b') should be (Nil)
  }

  it should "derive ∅" in {
    val r = RegExpParser("^∅$")
    r.derive[List]('a') should be (Nil)
  }

  it should "derive ε" in {
    val r = RegExpParser("^ε$")
    r.derive[List]('a') should be (List(None))
  }

  it should "derive ab" in {
    val r = RegExpParser("^ab$")
    r.derive[List]('a') should be (List(Some(ElemExp('b'))))
    r.derive[List]('b') should be (Nil)
    r.derive[List]('c') should be (Nil)
  }

  it should "derive a|b" in {
    val r = RegExpParser("^a|b$")
    r.derive[List]('a') should be (List(Some(EpsExp())))
    r.derive[List]('b') should be (List(Some(EpsExp())))
    r.derive[List]('c') should be (Nil)
  }

  it should "derive a*" in {
    val r = RegExpParser("^a*$")
    r.derive[List]('a') should be (List(Some(StarExp(ElemExp('a'), true)), None))
    r.derive[List]('b') should be (List(None))
  }

  it should "derive a+" in {
    val r = RegExpParser("^a+$")
    r.derive[List]('a') should be (List(Some(StarExp(ElemExp('a'), true))))
    r.derive[List]('b') should be (Nil)
  }

  it should "derive a?" in {
    val r = RegExpParser("^a?$")
    r.derive[List]('a') should be (List(Some(EpsExp()), None))
    r.derive[List]('b') should be (List(None))
  }

  it should "derive ." in {
    val r = RegExpParser("^.$")
    r.derive[List]('a') should be (List(Some(EpsExp())))
  }

  it should "derive character class" in {
    val r1 = RegExpParser("^[a-z]$")
    r1.derive[List]('a') should be (List(Some(EpsExp())))
    r1.derive[List]('h') should be (List(Some(EpsExp())))
    r1.derive[List]('z') should be (List(Some(EpsExp())))
    r1.derive[List]('A') should be (Nil)
    r1.derive[List]('H') should be (Nil)
    r1.derive[List]('Z') should be (Nil)

    val r2 = RegExpParser("^[^a-z]$")
    r2.derive[List]('a') should be (Nil)
    r2.derive[List]('h') should be (Nil)
    r2.derive[List]('z') should be (Nil)
    r2.derive[List]('A') should be (List(Some(EpsExp())))
    r2.derive[List]('H') should be (List(Some(EpsExp())))
    r2.derive[List]('Z') should be (List(Some(EpsExp())))
  }

  it should "derive repeat expression" in {
    val r1 = RegExpParser("^(ab){3,5}$")
    r1.derive[List]('a') should be (List(Some(RegExpParser("^b(ab){2,4}$"))))
    r1.derive[List]('b') should be (Nil)

    val r2 = RegExpParser("^(ab){3}$")
    r2.derive[List]('a') should be (List(Some(RegExpParser("^b(ab){2,2}$"))))
    r2.derive[List]('b') should be (Nil)

    val r3 = RegExpParser("^(ab){3,}$")
    r3.derive[List]('a') should be (List(Some(RegExpParser("^b(ab){2,}$"))))
    r3.derive[List]('b') should be (Nil)

    val r4 = RegExpParser("^(ab){,5}$")
    r4.derive[List]('a') should be (List(Some(RegExpParser("^b(ab){,4}$")), None))
    r4.derive[List]('b') should be (List(None))

    val r5 = RegExpParser("^(ab){1,5}$")
    r5.derive[List]('a') should be (List(Some(RegExpParser("^b(ab){,4}$"))))
    r5.derive[List]('b') should be (Nil)


    val r6 = RegExpParser("^(ab){1,}$")
    r6.derive[List]('a') should be (List(Some(RegExpParser("^b(ab)*$"))))
    r6.derive[List]('b') should be (Nil)

    val r7 = RegExpParser("^(ab){,1}$")
    r7.derive[List]('a') should be (List(Some(RegExpParser("^b$")), None))
    r7.derive[List]('b') should be (List(None))

    val r8 = RegExpParser("^(ab){1}$")
    r8.derive[List]('a') should be (List(Some(RegExpParser("^b$"))))
    r8.derive[List]('b') should be (Nil)
  }

  it should "derive lazy operations" in {
    val r1 = RegExpParser("^a*?$")
    r1.derive[List]('a') should be (List(None, Some(StarExp(ElemExp('a'), false))))
    r1.derive[List]('b') should be (List(None))

    val r2 = RegExpParser("^a+?$")
    r2.derive[List]('a') should be (List(Some(StarExp(ElemExp('a'), false))))
    r2.derive[List]('b') should be (Nil)

    val r3 = RegExpParser("^a??$")
    r3.derive[List]('a') should be (List(None, Some(EpsExp())))
    r3.derive[List]('b') should be (List(None))

    val r4 = RegExpParser("^(ab){3,5}?$")
    r4.derive[List]('a') should be (List(Some(RegExpParser("^b(ab){2,4}?$"))))
    r4.derive[List]('b') should be (Nil)

    val r5 = RegExpParser("^(ab){,5}?$")
    r5.derive[List]('a') should be (List(None, Some(RegExpParser("^b(ab){,4}?$"))))
    r5.derive[List]('b') should be (List(None))

    val r6 = RegExpParser("^(ab){,1}?$")
    r6.derive[List]('a') should be (List(None, Some(RegExpParser("^b$"))))
    r6.derive[List]('b') should be (List(None))
  }

  it should "derive complex expression" in {
    val r = RegExpParser("^a*(bc|d)$")
    r.derive[List]('a') should be (List(Some(RegExpParser("^a*(bc|d)$"))))
    r.derive[List]('b') should be (List(Some(RegExpParser("^c$"))))
    r.derive[List]('c') should be (Nil)
    r.derive[List]('d') should be (List(Some(RegExpParser("^ε$"))))
    r.derive[List]('e') should be (Nil)
  }

  "constructMorphs" should "construct morphs which simulates exhaustive search" in {
    val r0 = RegExpParser("^a*a*b|ba$")
    val r1 = RegExpParser("^a*a*b$")
    val r2 = RegExpParser("^a*b$")
    val r3 = RegExpParser("^a$")
    val r4 = RegExpParser("^ε$")
    val indexedMorphs = constructMorphs[List,Char](r0, Set('a','b'))
    indexedMorphs.morphs should contain only (
      'a' -> Map(
        r0 -> Seq(r1,r2),
        r1 -> Seq(r1,r2),
        r2 -> Seq(r2),
        r3 -> Seq(r4),
        r4 -> Seq()
      ),
      'b' -> Map(
        r0 -> Seq(r4,r3),
        r1 -> Seq(r4),
        r2 -> Seq(r4),
        r3 -> Seq(),
        r4 -> Seq()
      )
    )
    indexedMorphs.initials should be (Set(r0))
    indexedMorphs.finals should be (Set(r4))
  }
}
