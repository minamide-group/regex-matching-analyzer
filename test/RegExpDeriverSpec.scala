package matching.regexp

import org.scalatest._

class RegExpDeriverSpec extends FlatSpec with Matchers {
  implicit val deriver = new RegExpDeriver[List]()
  def parseWithStartEnd(s: String): RegExp[Char] = {
    RegExpParser(s"^${s}${"$"}")
  }

  "derive" should "derive a" in {
    val r = parseWithStartEnd("a")
    r.derive('a') should be (List(Some(EpsExp())))
    r.derive('b') should be (Nil)
  }

  it should "derive ∅" in {
    val r = parseWithStartEnd("∅")
    r.derive('a') should be (Nil)
  }

  it should "derive ε" in {
    val r = parseWithStartEnd("ε")
    r.derive('a') should be (List(None))
  }

  it should "derive ab" in {
    val r = parseWithStartEnd("ab")
    r.derive('a') should be (List(Some(ElemExp('b'))))
    r.derive('b') should be (Nil)
    r.derive('c') should be (Nil)
  }

  it should "derive a|b" in {
    val r = parseWithStartEnd("a|b")
    r.derive('a') should be (List(Some(EpsExp())))
    r.derive('b') should be (List(Some(EpsExp())))
    r.derive('c') should be (Nil)
  }

  it should "derive a*" in {
    val r = parseWithStartEnd("a*")
    r.derive('a') should be (List(Some(StarExp(ElemExp('a'), true)), None))
    r.derive('b') should be (List(None))
  }

  it should "derive a+" in {
    val r = parseWithStartEnd("a+")
    r.derive('a') should be (List(Some(StarExp(ElemExp('a'), true))))
    r.derive('b') should be (Nil)
  }

  it should "derive a?" in {
    val r = parseWithStartEnd("a?")
    r.derive('a') should be (List(Some(EpsExp()), None))
    r.derive('b') should be (List(None))
  }

  it should "derive ." in {
    val r = parseWithStartEnd(".")
    r.derive('a') should be (List(Some(EpsExp())))
    r.derive('\n') should be (Nil)
  }

  it should "derive character class" in {
    val r1 = parseWithStartEnd("""[ah-k\d]""")
    r1.derive('a') should be (List(Some(EpsExp())))
    r1.derive('i') should be (List(Some(EpsExp())))
    r1.derive('0') should be (List(Some(EpsExp())))
    r1.derive('c') should be (Nil)
    r1.derive('A') should be (Nil)
    r1.derive('H') should be (Nil)
    r1.derive('Z') should be (Nil)

    val r2 = parseWithStartEnd("""[^ah-k\d]""")
    r2.derive('a') should be (Nil)
    r2.derive('i') should be (Nil)
    r2.derive('0') should be (Nil)
    r2.derive('c') should be (List(Some(EpsExp())))
    r2.derive('A') should be (List(Some(EpsExp())))
    r2.derive('H') should be (List(Some(EpsExp())))
    r2.derive('Z') should be (List(Some(EpsExp())))

    val r3 = parseWithStartEnd("""[\W\D]""")
    r3.derive('a') should be (List(Some(EpsExp())))
    r3.derive('A') should be (List(Some(EpsExp())))
    r3.derive('!') should be (List(Some(EpsExp())))
    r3.derive('0') should be (Nil)

    val r4 = parseWithStartEnd("""[^0\D]""")
    r4.derive('1') should be (List(Some(EpsExp())))
    r4.derive('0') should be (Nil)
    r4.derive('a') should be (Nil)
    r4.derive('A') should be (Nil)
    r4.derive('!') should be (Nil)
  }

  it should "derive meta character" in {
    val rd = parseWithStartEnd("""\d""")
    rd.derive('0') should be (List(Some(EpsExp())))
    rd.derive('9') should be (List(Some(EpsExp())))
    rd.derive('a') should be (Nil)
    rd.derive('A') should be (Nil)

    val rD = parseWithStartEnd("""\D""")
    rD.derive('a') should be (List(Some(EpsExp())))
    rD.derive('A') should be (List(Some(EpsExp())))
    rD.derive('0') should be (Nil)
    rD.derive('9') should be (Nil)

    val rh = parseWithStartEnd("""\h""")
    rh.derive('\u0009') should be (List(Some(EpsExp())))
    rh.derive('a') should be (Nil)

    val rH = parseWithStartEnd("""\H""")
    rH.derive('a') should be (List(Some(EpsExp())))
    rH.derive('\u0009') should be (Nil)

    val rR = parseWithStartEnd("""\R""")
    rR.derive('\r') should be (List(Some(EpsExp())))
    rR.derive('\n') should be (List(Some(EpsExp())))
    rR.derive('a') should be (Nil)

    val rs = parseWithStartEnd("""\s""")
    rs.derive(' ') should be (List(Some(EpsExp())))
    rs.derive('\t') should be (List(Some(EpsExp())))
    rs.derive('\n') should be (List(Some(EpsExp())))
    rs.derive('\r') should be (List(Some(EpsExp())))
    rs.derive('\f') should be (List(Some(EpsExp())))
    rs.derive('a') should be (Nil)

    val rS = parseWithStartEnd("""\S""")
    rS.derive('a') should be (List(Some(EpsExp())))
    rS.derive(' ') should be (Nil)
    rS.derive('\t') should be (Nil)
    rS.derive('\n') should be (Nil)
    rS.derive('\r') should be (Nil)
    rS.derive('\f') should be (Nil)

    val rv = parseWithStartEnd("""\v""")
    rv.derive('\u000B') should be (List(Some(EpsExp())))
    rv.derive('a') should be (Nil)

    val rV = parseWithStartEnd("""\V""")
    rV.derive('a') should be (List(Some(EpsExp())))
    rV.derive('\u000B') should be (Nil)

    val rw = parseWithStartEnd("""\w""")
    rw.derive('a') should be (List(Some(EpsExp())))
    rw.derive('z') should be (List(Some(EpsExp())))
    rw.derive('A') should be (List(Some(EpsExp())))
    rw.derive('Z') should be (List(Some(EpsExp())))
    rw.derive('0') should be (List(Some(EpsExp())))
    rw.derive('9') should be (List(Some(EpsExp())))
    rw.derive('_') should be (List(Some(EpsExp())))
    rw.derive('!') should be (Nil)

    val rW = parseWithStartEnd("""\W""")
    rW.derive('!') should be (List(Some(EpsExp())))
    rW.derive('0') should be (Nil)
    rW.derive('9') should be (Nil)
    rW.derive('a') should be (Nil)
    rW.derive('z') should be (Nil)
    rW.derive('A') should be (Nil)
    rW.derive('Z') should be (Nil)
    rW.derive('_') should be (Nil)
  }

  it should "derive repeat expression" in {
    val r1 = parseWithStartEnd("(ab){3,5}")
    r1.derive('a') should be (List(Some(parseWithStartEnd("b(ab){2,4}"))))
    r1.derive('b') should be (Nil)

    val r2 = parseWithStartEnd("(ab){3}")
    r2.derive('a') should be (List(Some(parseWithStartEnd("b(ab){2,2}"))))
    r2.derive('b') should be (Nil)

    val r3 = parseWithStartEnd("(ab){3,}")
    r3.derive('a') should be (List(Some(parseWithStartEnd("b(ab){2,}"))))
    r3.derive('b') should be (Nil)

    val r4 = parseWithStartEnd("(ab){,5}")
    r4.derive('a') should be (List(Some(parseWithStartEnd("b(ab){,4}")), None))
    r4.derive('b') should be (List(None))

    val r5 = parseWithStartEnd("(ab){1,5}")
    r5.derive('a') should be (List(Some(parseWithStartEnd("b(ab){,4}"))))
    r5.derive('b') should be (Nil)


    val r6 = parseWithStartEnd("(ab){1,}")
    r6.derive('a') should be (List(Some(parseWithStartEnd("b(ab)*"))))
    r6.derive('b') should be (Nil)

    val r7 = parseWithStartEnd("(ab){,1}")
    r7.derive('a') should be (List(Some(parseWithStartEnd("b")), None))
    r7.derive('b') should be (List(None))

    val r8 = parseWithStartEnd("(ab){1}")
    r8.derive('a') should be (List(Some(parseWithStartEnd("b"))))
    r8.derive('b') should be (Nil)
  }

  it should "derive lazy operations" in {
    val r1 = parseWithStartEnd("a*?")
    r1.derive('a') should be (List(None, Some(StarExp(ElemExp('a'), false))))
    r1.derive('b') should be (List(None))

    val r2 = parseWithStartEnd("a+?")
    r2.derive('a') should be (List(Some(StarExp(ElemExp('a'), false))))
    r2.derive('b') should be (Nil)

    val r3 = parseWithStartEnd("a??")
    r3.derive('a') should be (List(None, Some(EpsExp())))
    r3.derive('b') should be (List(None))

    val r4 = parseWithStartEnd("(ab){3,5}?")
    r4.derive('a') should be (List(Some(parseWithStartEnd("b(ab){2,4}?"))))
    r4.derive('b') should be (Nil)

    val r5 = parseWithStartEnd("(ab){,5}?")
    r5.derive('a') should be (List(None, Some(parseWithStartEnd("b(ab){,4}?"))))
    r5.derive('b') should be (List(None))

    val r6 = parseWithStartEnd("(ab){,1}?")
    r6.derive('a') should be (List(None, Some(parseWithStartEnd("b"))))
    r6.derive('b') should be (List(None))
  }

  it should "derive complex expression" in {
    val r1 = parseWithStartEnd("a*(bc|d)")
    r1.derive('a') should be (List(Some(parseWithStartEnd("a*(bc|d)"))))
    r1.derive('b') should be (List(Some(parseWithStartEnd("c"))))
    r1.derive('c') should be (Nil)
    r1.derive('d') should be (List(Some(parseWithStartEnd("ε"))))
    r1.derive('e') should be (Nil)

    val r2 = parseWithStartEnd("(a*)*")
    r2.derive('a') should be (List(Some(parseWithStartEnd("a*(a*)*")), None, None))
    r2.derive('b') should be (List(None, None))
  }

  "derive with character not appear in given expression" should "derive a" in {
    val r = parseWithStartEnd("a")
    r.derive(None) should be (Nil)
  }

  it should "derive ." in {
    val r = parseWithStartEnd(".")
    r.derive(None) should be (List(Some(EpsExp())))
  }

  it should "derive character class" in {
    val r1 = parseWithStartEnd("""[a-z]""")
    r1.derive(None) should be (Nil)

    val r2 = parseWithStartEnd("""[^a-z]""")
    r2.derive(None) should be (List(Some(EpsExp())))

    val r3 = parseWithStartEnd("""[\w]""")
    r3.derive(None) should be (Nil)

    val r4 = parseWithStartEnd("""[\W]""")
    r4.derive(None) should be (List(Some(EpsExp())))

    val r6 = parseWithStartEnd("""[0-9\w]""")
    r6.derive(None) should be (Nil)

    val r7 = parseWithStartEnd("""[0-9\W]""")
    r7.derive(None) should be (List(Some(EpsExp())))

    val r8 = parseWithStartEnd("""[^0-9\W]""")
    r8.derive(None) should be (Nil)
  }

  it should "derive meta character" in {
    val r1 = parseWithStartEnd("""\w""")
    r1.derive(None) should be (Nil)

    val r2 = parseWithStartEnd("""\W""")
    r2.derive(None) should be (List(Some(EpsExp())))
  }
}
