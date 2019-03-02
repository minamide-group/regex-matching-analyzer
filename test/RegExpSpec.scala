package matching.regexp

import org.scalatest._
import RegExp._

class RegExpSpec extends FlatSpec with Matchers {
  def parseWithStartEnd(s: String): RegExp[Char] = {
    RegExpParser(s"^${s}${"$"}")
  }

  "optConcatExp" should "concat expressions with optimization on ε" in {
    RegExp.optConcatExp(ElemExp('a'),ElemExp('b')) should be (ConcatExp(ElemExp('a'),ElemExp('b')))
    RegExp.optConcatExp(EpsExp(),ElemExp('b')) should be (ElemExp('b'))
    RegExp.optConcatExp(ElemExp('a'),EpsExp()) should be (ElemExp('a'))
    RegExp.optConcatExp(EpsExp(),EpsExp()) should be (EpsExp())
  }

  it should "concat expressions with optimization on repeat expression" in {
    RegExp.optConcatExp(ElemExp('a'),RepeatExp(ElemExp('a'),Some(3),Some(5),true)) should be (
      RepeatExp(ElemExp('a'),Some(4),Some(6),true)
    )
    RegExp.optConcatExp(ElemExp('a'),RepeatExp(ElemExp('a'),None,Some(5),false)) should be (
      RepeatExp(ElemExp('a'),Some(1),Some(6),false)
    )
    RegExp.optConcatExp(ElemExp('a'),RepeatExp(ElemExp('a'),Some(3),None,true)) should be (
      RepeatExp(ElemExp('a'),Some(4),None,true)
    )
    RegExp.optConcatExp(ElemExp('a'),RepeatExp(ElemExp('b'),Some(3),Some(5),true)) should be (
      ConcatExp(ElemExp('a'),RepeatExp(ElemExp('b'),Some(3),Some(5),true))
    )
  }

  "derive" should "derive a" in {
    val r = parseWithStartEnd("a")
    r.derive[List]('a') should be (List(Some(EpsExp())))
    r.derive[List]('b') should be (Nil)
  }

  it should "derive ∅" in {
    val r = parseWithStartEnd("∅")
    r.derive[List]('a') should be (Nil)
  }

  it should "derive ε" in {
    val r = parseWithStartEnd("ε")
    r.derive[List]('a') should be (List(None))
  }

  it should "derive ab" in {
    val r = parseWithStartEnd("ab")
    r.derive[List]('a') should be (List(Some(ElemExp('b'))))
    r.derive[List]('b') should be (Nil)
    r.derive[List]('c') should be (Nil)
  }

  it should "derive a|b" in {
    val r = parseWithStartEnd("a|b")
    r.derive[List]('a') should be (List(Some(EpsExp())))
    r.derive[List]('b') should be (List(Some(EpsExp())))
    r.derive[List]('c') should be (Nil)
  }

  it should "derive a*" in {
    val r = parseWithStartEnd("a*")
    r.derive[List]('a') should be (List(Some(StarExp(ElemExp('a'), true)), None))
    r.derive[List]('b') should be (List(None))
  }

  it should "derive a+" in {
    val r = parseWithStartEnd("a+")
    r.derive[List]('a') should be (List(Some(StarExp(ElemExp('a'), true))))
    r.derive[List]('b') should be (Nil)
  }

  it should "derive a?" in {
    val r = parseWithStartEnd("a?")
    r.derive[List]('a') should be (List(Some(EpsExp()), None))
    r.derive[List]('b') should be (List(None))
  }

  it should "derive ." in {
    val r = parseWithStartEnd(".")
    r.derive[List]('a') should be (List(Some(EpsExp())))
  }

  it should "derive character class" in {
    val r1 = parseWithStartEnd("""[ah-k\d]""")
    r1.derive[List]('a') should be (List(Some(EpsExp())))
    r1.derive[List]('i') should be (List(Some(EpsExp())))
    r1.derive[List]('0') should be (List(Some(EpsExp())))
    r1.derive[List]('c') should be (Nil)
    r1.derive[List]('A') should be (Nil)
    r1.derive[List]('H') should be (Nil)
    r1.derive[List]('Z') should be (Nil)

    val r2 = parseWithStartEnd("""[^ah-k\d]""")
    r2.derive[List]('a') should be (Nil)
    r2.derive[List]('i') should be (Nil)
    r2.derive[List]('0') should be (Nil)
    r2.derive[List]('c') should be (List(Some(EpsExp())))
    r2.derive[List]('A') should be (List(Some(EpsExp())))
    r2.derive[List]('H') should be (List(Some(EpsExp())))
    r2.derive[List]('Z') should be (List(Some(EpsExp())))

    val r3 = parseWithStartEnd("""[\W\D]""")
    r3.derive[List]('a') should be (List(Some(EpsExp())))
    r3.derive[List]('A') should be (List(Some(EpsExp())))
    r3.derive[List]('!') should be (List(Some(EpsExp())))
    r3.derive[List]('0') should be (Nil)

    val r4 = parseWithStartEnd("""[^0\D]""")
    r4.derive[List]('1') should be (List(Some(EpsExp())))
    r4.derive[List]('0') should be (Nil)
    r4.derive[List]('a') should be (Nil)
    r4.derive[List]('A') should be (Nil)
    r4.derive[List]('!') should be (Nil)
  }

  it should "derive meta character" in {
    val rd = parseWithStartEnd("""\d""")
    rd.derive[List]('0') should be (List(Some(EpsExp())))
    rd.derive[List]('9') should be (List(Some(EpsExp())))
    rd.derive[List]('a') should be (Nil)
    rd.derive[List]('A') should be (Nil)

    val rD = parseWithStartEnd("""\D""")
    rD.derive[List]('a') should be (List(Some(EpsExp())))
    rD.derive[List]('A') should be (List(Some(EpsExp())))
    rD.derive[List]('0') should be (Nil)
    rD.derive[List]('9') should be (Nil)

    val rh = parseWithStartEnd("""\h""")
    rh.derive[List]('\u0009') should be (List(Some(EpsExp())))
    rh.derive[List]('a') should be (Nil)

    val rH = parseWithStartEnd("""\H""")
    rH.derive[List]('a') should be (List(Some(EpsExp())))
    rH.derive[List]('\u0009') should be (Nil)

    val rR = parseWithStartEnd("""\R""")
    rR.derive[List]('\r') should be (List(Some(EpsExp())))
    rR.derive[List]('\n') should be (List(Some(EpsExp())))
    rR.derive[List]('a') should be (Nil)

    val rs = parseWithStartEnd("""\s""")
    rs.derive[List](' ') should be (List(Some(EpsExp())))
    rs.derive[List]('\t') should be (List(Some(EpsExp())))
    rs.derive[List]('\n') should be (List(Some(EpsExp())))
    rs.derive[List]('\r') should be (List(Some(EpsExp())))
    rs.derive[List]('\f') should be (List(Some(EpsExp())))
    rs.derive[List]('a') should be (Nil)

    val rS = parseWithStartEnd("""\S""")
    rS.derive[List]('a') should be (List(Some(EpsExp())))
    rS.derive[List](' ') should be (Nil)
    rS.derive[List]('\t') should be (Nil)
    rS.derive[List]('\n') should be (Nil)
    rS.derive[List]('\r') should be (Nil)
    rS.derive[List]('\f') should be (Nil)

    val rv = parseWithStartEnd("""\v""")
    rv.derive[List]('\u000B') should be (List(Some(EpsExp())))
    rv.derive[List]('a') should be (Nil)

    val rV = parseWithStartEnd("""\V""")
    rV.derive[List]('a') should be (List(Some(EpsExp())))
    rV.derive[List]('\u000B') should be (Nil)

    val rw = parseWithStartEnd("""\w""")
    rw.derive[List]('a') should be (List(Some(EpsExp())))
    rw.derive[List]('z') should be (List(Some(EpsExp())))
    rw.derive[List]('A') should be (List(Some(EpsExp())))
    rw.derive[List]('Z') should be (List(Some(EpsExp())))
    rw.derive[List]('0') should be (List(Some(EpsExp())))
    rw.derive[List]('9') should be (List(Some(EpsExp())))
    rw.derive[List]('_') should be (List(Some(EpsExp())))
    rw.derive[List]('!') should be (Nil)

    val rW = parseWithStartEnd("""\W""")
    rW.derive[List]('!') should be (List(Some(EpsExp())))
    rW.derive[List]('0') should be (Nil)
    rW.derive[List]('9') should be (Nil)
    rW.derive[List]('a') should be (Nil)
    rW.derive[List]('z') should be (Nil)
    rW.derive[List]('A') should be (Nil)
    rW.derive[List]('Z') should be (Nil)
    rW.derive[List]('_') should be (Nil)
  }

  it should "derive repeat expression" in {
    val r1 = parseWithStartEnd("(ab){3,5}")
    r1.derive[List]('a') should be (List(Some(parseWithStartEnd("b(ab){2,4}"))))
    r1.derive[List]('b') should be (Nil)

    val r2 = parseWithStartEnd("(ab){3}")
    r2.derive[List]('a') should be (List(Some(parseWithStartEnd("b(ab){2,2}"))))
    r2.derive[List]('b') should be (Nil)

    val r3 = parseWithStartEnd("(ab){3,}")
    r3.derive[List]('a') should be (List(Some(parseWithStartEnd("b(ab){2,}"))))
    r3.derive[List]('b') should be (Nil)

    val r4 = parseWithStartEnd("(ab){,5}")
    r4.derive[List]('a') should be (List(Some(parseWithStartEnd("b(ab){,4}")), None))
    r4.derive[List]('b') should be (List(None))

    val r5 = parseWithStartEnd("(ab){1,5}")
    r5.derive[List]('a') should be (List(Some(parseWithStartEnd("b(ab){,4}"))))
    r5.derive[List]('b') should be (Nil)


    val r6 = parseWithStartEnd("(ab){1,}")
    r6.derive[List]('a') should be (List(Some(parseWithStartEnd("b(ab)*"))))
    r6.derive[List]('b') should be (Nil)

    val r7 = parseWithStartEnd("(ab){,1}")
    r7.derive[List]('a') should be (List(Some(parseWithStartEnd("b")), None))
    r7.derive[List]('b') should be (List(None))

    val r8 = parseWithStartEnd("(ab){1}")
    r8.derive[List]('a') should be (List(Some(parseWithStartEnd("b"))))
    r8.derive[List]('b') should be (Nil)
  }

  it should "derive lazy operations" in {
    val r1 = parseWithStartEnd("a*?")
    r1.derive[List]('a') should be (List(None, Some(StarExp(ElemExp('a'), false))))
    r1.derive[List]('b') should be (List(None))

    val r2 = parseWithStartEnd("a+?")
    r2.derive[List]('a') should be (List(Some(StarExp(ElemExp('a'), false))))
    r2.derive[List]('b') should be (Nil)

    val r3 = parseWithStartEnd("a??")
    r3.derive[List]('a') should be (List(None, Some(EpsExp())))
    r3.derive[List]('b') should be (List(None))

    val r4 = parseWithStartEnd("(ab){3,5}?")
    r4.derive[List]('a') should be (List(Some(parseWithStartEnd("b(ab){2,4}?"))))
    r4.derive[List]('b') should be (Nil)

    val r5 = parseWithStartEnd("(ab){,5}?")
    r5.derive[List]('a') should be (List(None, Some(parseWithStartEnd("b(ab){,4}?"))))
    r5.derive[List]('b') should be (List(None))

    val r6 = parseWithStartEnd("(ab){,1}?")
    r6.derive[List]('a') should be (List(None, Some(parseWithStartEnd("b"))))
    r6.derive[List]('b') should be (List(None))
  }

  it should "derive complex expression" in {
    val r = parseWithStartEnd("a*(bc|d)")
    r.derive[List]('a') should be (List(Some(parseWithStartEnd("a*(bc|d)"))))
    r.derive[List]('b') should be (List(Some(parseWithStartEnd("c"))))
    r.derive[List]('c') should be (Nil)
    r.derive[List]('d') should be (List(Some(parseWithStartEnd("ε"))))
    r.derive[List]('e') should be (Nil)
  }

  "constructMorphs" should "construct morphs which simulates exhaustive search" in {
    val r0 = parseWithStartEnd("a*a*b|ba")
    val r1 = parseWithStartEnd("a*a*b")
    val r2 = parseWithStartEnd("a*b")
    val r3 = parseWithStartEnd("a")
    val r4 = parseWithStartEnd("ε")
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
