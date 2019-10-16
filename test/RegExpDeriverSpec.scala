package matching.regexp

import org.scalatest._
import RegExp._

class RegExpDeriverSpec extends FlatSpec with Matchers {
  implicit var deriver = new RegExpDeriver[List]()

  "derive" should "derive a" in {
    val r = RegExpParser("a")
    r.derive('a', Nil) should be (List(Some(EpsExp())))
    r.derive('b', Nil) should be (Nil)
  }

  it should "derive ∅" in {
    val r = RegExpParser("∅")
    r.derive('a', Nil) should be (Nil)
  }

  it should "derive ε" in {
    val r = RegExpParser("ε")
    r.derive('a', Nil) should be (List(None))
  }

  it should "derive ab" in {
    val r = RegExpParser("ab")
    r.derive('a', Nil) should be (List(Some(ElemExp('b'))))
    r.derive('b', Nil) should be (Nil)
    r.derive('c', Nil) should be (Nil)
  }

  it should "derive a|b" in {
    val r = RegExpParser("a|b")
    r.derive('a', Nil) should be (List(Some(EpsExp())))
    r.derive('b', Nil) should be (List(Some(EpsExp())))
    r.derive('c', Nil) should be (Nil)
  }

  it should "derive a*" in {
    val r = RegExpParser("a*")
    r.derive('a', Nil) should be (List(Some(StarExp(ElemExp('a'), true)), None))
    r.derive('b', Nil) should be (List(None))
  }

  it should "derive a+" in {
    val r = RegExpParser("a+")
    r.derive('a', Nil) should be (List(Some(StarExp(ElemExp('a'), true))))
    r.derive('b', Nil) should be (Nil)
  }

  it should "derive a?" in {
    val r = RegExpParser("a?")
    r.derive('a', Nil) should be (List(Some(EpsExp()), None))
    r.derive('b', Nil) should be (List(None))
  }

  it should "derive ." in {
    val r = RegExpParser(".")
    r.derive('a', Nil) should be (List(Some(EpsExp())))
    r.derive('\n', Nil) should be (Nil)
  }

  it should "derive character class" in {
    val r1 = RegExpParser("""[ah-k\d]""")
    r1.derive('a', Nil) should be (List(Some(EpsExp())))
    r1.derive('i', Nil) should be (List(Some(EpsExp())))
    r1.derive('0', Nil) should be (List(Some(EpsExp())))
    r1.derive('c', Nil) should be (Nil)
    r1.derive('A', Nil) should be (Nil)
    r1.derive('H', Nil) should be (Nil)
    r1.derive('Z', Nil) should be (Nil)

    val r2 = RegExpParser("""[^ah-k\d]""")
    r2.derive('a', Nil) should be (Nil)
    r2.derive('i', Nil) should be (Nil)
    r2.derive('0', Nil) should be (Nil)
    r2.derive('c', Nil) should be (List(Some(EpsExp())))
    r2.derive('A', Nil) should be (List(Some(EpsExp())))
    r2.derive('H', Nil) should be (List(Some(EpsExp())))
    r2.derive('Z', Nil) should be (List(Some(EpsExp())))

    val r3 = RegExpParser("""[\W\D]""")
    r3.derive('a', Nil) should be (List(Some(EpsExp())))
    r3.derive('A', Nil) should be (List(Some(EpsExp())))
    r3.derive('!', Nil) should be (List(Some(EpsExp())))
    r3.derive('0', Nil) should be (Nil)

    val r4 = RegExpParser("""[^0\D]""")
    r4.derive('1', Nil) should be (List(Some(EpsExp())))
    r4.derive('0', Nil) should be (Nil)
    r4.derive('a', Nil) should be (Nil)
    r4.derive('A', Nil) should be (Nil)
    r4.derive('!', Nil) should be (Nil)
  }

  it should "derive meta character" in {
    val rd = RegExpParser("""\d""")
    rd.derive('0', Nil) should be (List(Some(EpsExp())))
    rd.derive('9', Nil) should be (List(Some(EpsExp())))
    rd.derive('a', Nil) should be (Nil)
    rd.derive('A', Nil) should be (Nil)

    val rD = RegExpParser("""\D""")
    rD.derive('a', Nil) should be (List(Some(EpsExp())))
    rD.derive('A', Nil) should be (List(Some(EpsExp())))
    rD.derive('0', Nil) should be (Nil)
    rD.derive('9', Nil) should be (Nil)

    val rh = RegExpParser("""\h""")
    rh.derive('\u0009', Nil) should be (List(Some(EpsExp())))
    rh.derive('a', Nil) should be (Nil)

    val rH = RegExpParser("""\H""")
    rH.derive('a', Nil) should be (List(Some(EpsExp())))
    rH.derive('\u0009', Nil) should be (Nil)

    val rR = RegExpParser("""\R""")
    rR.derive('\r', Nil) should be (List(Some(EpsExp())))
    rR.derive('\n', Nil) should be (List(Some(EpsExp())))
    rR.derive('a', Nil) should be (Nil)

    val rs = RegExpParser("""\s""")
    rs.derive(' ', Nil) should be (List(Some(EpsExp())))
    rs.derive('\t', Nil) should be (List(Some(EpsExp())))
    rs.derive('\n', Nil) should be (List(Some(EpsExp())))
    rs.derive('\r', Nil) should be (List(Some(EpsExp())))
    rs.derive('\f', Nil) should be (List(Some(EpsExp())))
    rs.derive('a', Nil) should be (Nil)

    val rS = RegExpParser("""\S""")
    rS.derive('a', Nil) should be (List(Some(EpsExp())))
    rS.derive(' ', Nil) should be (Nil)
    rS.derive('\t', Nil) should be (Nil)
    rS.derive('\n', Nil) should be (Nil)
    rS.derive('\r', Nil) should be (Nil)
    rS.derive('\f', Nil) should be (Nil)

    val rv = RegExpParser("""\v""")
    rv.derive('\u000B', Nil) should be (List(Some(EpsExp())))
    rv.derive('a', Nil) should be (Nil)

    val rV = RegExpParser("""\V""")
    rV.derive('a', Nil) should be (List(Some(EpsExp())))
    rV.derive('\u000B', Nil) should be (Nil)

    val rw = RegExpParser("""\w""")
    rw.derive('a', Nil) should be (List(Some(EpsExp())))
    rw.derive('z', Nil) should be (List(Some(EpsExp())))
    rw.derive('A', Nil) should be (List(Some(EpsExp())))
    rw.derive('Z', Nil) should be (List(Some(EpsExp())))
    rw.derive('0', Nil) should be (List(Some(EpsExp())))
    rw.derive('9', Nil) should be (List(Some(EpsExp())))
    rw.derive('_', Nil) should be (List(Some(EpsExp())))
    rw.derive('!', Nil) should be (Nil)

    val rW = RegExpParser("""\W""")
    rW.derive('!', Nil) should be (List(Some(EpsExp())))
    rW.derive('0', Nil) should be (Nil)
    rW.derive('9', Nil) should be (Nil)
    rW.derive('a', Nil) should be (Nil)
    rW.derive('z', Nil) should be (Nil)
    rW.derive('A', Nil) should be (Nil)
    rW.derive('Z', Nil) should be (Nil)
    rW.derive('_', Nil) should be (Nil)
  }

  it should "derive repeat expression" in {
    val r1 = RegExpParser("(ab){3,5}")
    r1.derive('a', Nil) should be (List(Some(RegExpParser("b(ab){2,4}"))))
    r1.derive('b', Nil) should be (Nil)

    val r2 = RegExpParser("(ab){3}")
    r2.derive('a', Nil) should be (List(Some(RegExpParser("b(ab){2,2}"))))
    r2.derive('b', Nil) should be (Nil)

    val r3 = RegExpParser("(ab){3,}")
    r3.derive('a', Nil) should be (List(Some(RegExpParser("b(ab){2,}"))))
    r3.derive('b', Nil) should be (Nil)

    val r4 = RegExpParser("(ab){,5}")
    r4.derive('a', Nil) should be (List(Some(RegExpParser("b(ab){,4}")), None))
    r4.derive('b', Nil) should be (List(None))

    val r5 = RegExpParser("(ab){1,5}")
    r5.derive('a', Nil) should be (List(Some(RegExpParser("b(ab){,4}"))))
    r5.derive('b', Nil) should be (Nil)


    val r6 = RegExpParser("(ab){1,}")
    r6.derive('a', Nil) should be (List(Some(RegExpParser("b(ab)*"))))
    r6.derive('b', Nil) should be (Nil)

    val r7 = RegExpParser("(ab){,1}")
    r7.derive('a', Nil) should be (List(Some(RegExpParser("b")), None))
    r7.derive('b', Nil) should be (List(None))

    val r8 = RegExpParser("(ab){1}")
    r8.derive('a', Nil) should be (List(Some(RegExpParser("b"))))
    r8.derive('b', Nil) should be (Nil)
  }

  it should "derive ^" in {
    val r = RegExpParser("^")
    r.derive('a', Nil) should be (List(None))
    r.derive('a', List(None)) should be (Nil)
  }

  it should "derive $" in {
    val r = RegExpParser("$")
    r.derive('a', List(None)) should be (Nil)
  }

  it should "derive lazy operations" in {
    val r1 = RegExpParser("a*?")
    r1.derive('a', Nil) should be (List(None, Some(StarExp(ElemExp('a'), false))))
    r1.derive('b', Nil) should be (List(None))

    val r2 = RegExpParser("a+?")
    r2.derive('a', Nil) should be (List(Some(StarExp(ElemExp('a'), false))))
    r2.derive('b', Nil) should be (Nil)

    val r3 = RegExpParser("a??")
    r3.derive('a', Nil) should be (List(None, Some(EpsExp())))
    r3.derive('b', Nil) should be (List(None))

    val r4 = RegExpParser("(ab){3,5}?")
    r4.derive('a', Nil) should be (List(Some(RegExpParser("b(ab){2,4}?"))))
    r4.derive('b', Nil) should be (Nil)

    val r5 = RegExpParser("(ab){,5}?")
    r5.derive('a', Nil) should be (List(None, Some(RegExpParser("b(ab){,4}?"))))
    r5.derive('b', Nil) should be (List(None))

    val r6 = RegExpParser("(ab){,1}?")
    r6.derive('a', Nil) should be (List(None, Some(RegExpParser("b"))))
    r6.derive('b', Nil) should be (List(None))
  }

  it should "derive complex expression" in {
    val r1 = RegExpParser("a*(bc|d)")
    r1.derive('a', Nil) should be (List(Some(RegExpParser("a*(bc|d)"))))
    r1.derive('b', Nil) should be (List(Some(RegExpParser("c"))))
    r1.derive('c', Nil) should be (Nil)
    r1.derive('d', Nil) should be (List(Some(RegExpParser("ε"))))
    r1.derive('e', Nil) should be (Nil)

    val r2 = RegExpParser("(a*)*")
    r2.derive('a', Nil) should be (List(Some(RegExpParser("a*(a*)*")), None, None))
    r2.derive('b', Nil) should be (List(None, None))
  }


  "derive with character not appear in given expression" should "derive a" in {
    val r = RegExpParser("a")
    r.derive(None, Nil) should be (Nil)
  }

  it should "derive ." in {
    val r = RegExpParser(".")
    r.derive(None, Nil) should be (List(Some(EpsExp())))
  }

  it should "derive character class" in {
    val r1 = RegExpParser("[a-z]")
    r1.derive(None, Nil) should be (Nil)

    val r2 = RegExpParser("[^a-z]")
    r2.derive(None, Nil) should be (List(Some(EpsExp())))

    val r3 = RegExpParser("""[\w]""")
    r3.derive(None, Nil) should be (Nil)

    val r4 = RegExpParser("""[\W]""")
    r4.derive(None, Nil) should be (List(Some(EpsExp())))

    val r6 = RegExpParser("""[0-9\w]""")
    r6.derive(None, Nil) should be (Nil)

    val r7 = RegExpParser("""[0-9\W]""")
    r7.derive(None, Nil) should be (List(Some(EpsExp())))

    val r8 = RegExpParser("""[^0-9\W]""")
    r8.derive(None, Nil) should be (Nil)
  }

  it should "derive meta character" in {
    val r1 = RegExpParser("""\w""")
    r1.derive(None, Nil) should be (Nil)

    val r2 = RegExpParser("""\W""")
    r2.derive(None, Nil) should be (List(Some(EpsExp())))
  }


  "derive with ignore case option" should "derive character" in {
    deriver = new RegExpDeriver[List](new PCREOption("i"))

    val r1 = RegExpParser("a")
    r1.derive('a', Nil) should be (List(Some(EpsExp())))
    r1.derive('A', Nil) should be (List(Some(EpsExp())))
    r1.derive('b', Nil) should be (Nil)
    r1.derive('B', Nil) should be (Nil)

    val r2 = RegExpParser("A")
    r2.derive('a', Nil) should be (List(Some(EpsExp())))
    r2.derive('A', Nil) should be (List(Some(EpsExp())))
    r2.derive('b', Nil) should be (Nil)
    r2.derive('B', Nil) should be (Nil)
  }

  it should "derive character class" in {
    deriver = new RegExpDeriver[List](new PCREOption("i"))

    val r1 = RegExpParser("[ac-e]")
    r1.derive('a', Nil) should be (List(Some(EpsExp())))
    r1.derive('A', Nil) should be (List(Some(EpsExp())))
    r1.derive('d', Nil) should be (List(Some(EpsExp())))
    r1.derive('D', Nil) should be (List(Some(EpsExp())))
    r1.derive('b', Nil) should be (Nil)
    r1.derive('B', Nil) should be (Nil)

    val r2 = RegExpParser("""[^abB]""")
    r2.derive('c', Nil) should be (List(Some(EpsExp())))
    r2.derive('C', Nil) should be (List(Some(EpsExp())))
    r2.derive('b', Nil) should be (Nil)
    r2.derive('B', Nil) should be (Nil)
    r2.derive('a', Nil) should be (Nil)
    r2.derive('A', Nil) should be (Nil)
  }

  "derive with dot all option" should "derive dot" in {
    deriver = new RegExpDeriver[List](new PCREOption("s"))

    val r = RegExpParser(".")
    r.derive('a', Nil) should be (List(Some(EpsExp())))
    r.derive('\n', Nil) should be (List(Some(EpsExp())))
  }

  "derive with ungreedy option" should "derive repeat expression" in {
    deriver = new RegExpDeriver[List](new PCREOption("U"))

    val r1 = RegExpParser("a*")
    r1.derive('a', Nil) should be (List(None, Some(StarExp(ElemExp('a'), true))))

    val r2 = RegExpParser("a*?")
    r2.derive('a', Nil) should be (List(Some(StarExp(ElemExp('a'), false)), None))
  }

  "deriveEOL" should "derive a" in {
    val r = RegExpParser("a")
    r.deriveEOL(Nil) should be (empty)
  }

  it should "derive ∅" in {
    val r = RegExpParser("∅")
    r.deriveEOL(Nil) should be (empty)
  }

  it should "derive ε" in {
    val r = RegExpParser("ε")
    r.deriveEOL(Nil) should not be (empty)
  }

  it should "derive ab" in {
    val r = RegExpParser("ab")
    r.deriveEOL(Nil) should be (empty)
  }

  it should "derive a|b" in {
    val r = RegExpParser("a|b")
    r.deriveEOL(Nil) should be (empty)
  }

  it should "derive a*" in {
    val r = RegExpParser("a*")
    r.deriveEOL(Nil) should not be (empty)
  }

  it should "derive +" in {
    val r1 = RegExpParser("a+")
    r1.deriveEOL(Nil) should be (empty)

    val r2 = RegExpParser("(a*)+")
    r2.deriveEOL(Nil) should not be (empty)
  }

  it should "derive a?" in {
    val r = RegExpParser("a?")
    r.deriveEOL(Nil) should not be (empty)
  }

  it should "derive ." in {
    val r = RegExpParser(".")
    r.deriveEOL(Nil) should be (empty)
  }

  it should "derive character class" in {
    val r1 = RegExpParser("""[a-z]""")
    r1.deriveEOL(Nil) should be (empty)

    val r2 = RegExpParser("""[^A-Z]""")
    r2.deriveEOL(Nil) should be (empty)
  }

  it should "derive meta character" in {
    val r = RegExpParser("""\d""")
    r.deriveEOL(Nil) should be (empty)
  }

  it should "derive repeat expression" in {
    val r1 = RegExpParser("(ab){3,5}")
    r1.deriveEOL(Nil) should be (empty)

    val r2 = RegExpParser("(ab){3,}")
    r2.deriveEOL(Nil) should be (empty)

    val r3 = RegExpParser("(ab){,5}")
    r3.deriveEOL(Nil) should not be (empty)

    val r4 = RegExpParser("(a*){3,5}")
    r4.deriveEOL(Nil) should not be (empty)
  }

  it should "derive ^" in {
    val r = RegExpParser("^")
    r.deriveEOL(Nil) should not be (empty)
    r.deriveEOL(List(None)) should be (empty)
  }

  it should "derive $" in {
    val r = RegExpParser("$")
    r.deriveEOL(Nil) should not be (empty)
  }

  it should "derive complex expression" in {
    val r1 = RegExpParser("(a|b*)c+d*")
    r1.deriveEOL(Nil) should be (empty)

    val r2 = RegExpParser("(a|b*)c?")
    r2.deriveEOL(Nil) should not be (empty)
  }
}
