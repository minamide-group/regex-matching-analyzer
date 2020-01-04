package matching.regexp

import org.scalatest._
import RegExp._
import matching.monad._
import ATree.ATreeMonad._
import StateT._

class RegExpDeriverSpec extends FlatSpec with Matchers {
  implicit var deriver = new RegExpDeriver[StateTBooleanATree]()

  def applyLeaves[A](m: StateTBooleanATree[A,A])
    = leaves(m.apply(true))

  "derive" should "derive a" in {
    val r = RegExpParser("a")
    applyLeaves(r.derive('a')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r.derive('b')) should be (Nil)
  }

  it should "derive ∅" in {
    val r = RegExpParser("∅")
    applyLeaves(r.derive('a')) should be (Nil)
  }

  it should "derive ε" in {
    val r = RegExpParser("ε")
    applyLeaves(r.derive('a')) should be (List((None, true)))
  }

  it should "derive ab" in {
    val r = RegExpParser("ab")
    applyLeaves(r.derive('a')) should be (List((Some(ElemExp('b')), false)))
    applyLeaves(r.derive('b')) should be (Nil)
    applyLeaves(r.derive('c')) should be (Nil)
  }

  it should "derive a|b" in {
    val r = RegExpParser("a|b")
    applyLeaves(r.derive('a')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r.derive('b')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r.derive('c')) should be (Nil)
  }

  it should "derive a*" in {
    val r = RegExpParser("a*")
    applyLeaves(r.derive('a')) should be (List((Some(StarExp(ElemExp('a'), true)), false), (None, true)))
    applyLeaves(r.derive('b')) should be (List((None, true)))
  }

  it should "derive a+" in {
    val r = RegExpParser("a+")
    applyLeaves(r.derive('a')) should be (List((Some(StarExp(ElemExp('a'), true)), false)))
    applyLeaves(r.derive('b')) should be (Nil)
  }

  it should "derive a?" in {
    val r = RegExpParser("a?")
    applyLeaves(r.derive('a')) should be (List((Some(EpsExp()), false), (None, true)))
    applyLeaves(r.derive('b')) should be (List((None, true)))
  }

  it should "derive ." in {
    val r = RegExpParser(".")
    applyLeaves(r.derive('a')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r.derive('\n')) should be (Nil)
  }

  it should "derive character class" in {
    val r1 = RegExpParser("""[ah-k\d]""")
    applyLeaves(r1.derive('a')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r1.derive('i')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r1.derive('0')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r1.derive('c')) should be (Nil)
    applyLeaves(r1.derive('A')) should be (Nil)
    applyLeaves(r1.derive('H')) should be (Nil)
    applyLeaves(r1.derive('Z')) should be (Nil)

    val r2 = RegExpParser("""[^ah-k\d]""")
    applyLeaves(r2.derive('a')) should be (Nil)
    applyLeaves(r2.derive('i')) should be (Nil)
    applyLeaves(r2.derive('0')) should be (Nil)
    applyLeaves(r2.derive('c')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r2.derive('A')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r2.derive('H')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r2.derive('Z')) should be (List((Some(EpsExp()), false)))

    val r3 = RegExpParser("""[\W\D]""")
    applyLeaves(r3.derive('a')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r3.derive('A')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r3.derive('!')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r3.derive('0')) should be (Nil)

    val r4 = RegExpParser("""[^0\D]""")
    applyLeaves(r4.derive('1')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r4.derive('0')) should be (Nil)
    applyLeaves(r4.derive('a')) should be (Nil)
    applyLeaves(r4.derive('A')) should be (Nil)
    applyLeaves(r4.derive('!')) should be (Nil)
  }

  it should "derive meta character" in {
    val rd = RegExpParser("""\d""")
    applyLeaves(rd.derive('0')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rd.derive('9')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rd.derive('a')) should be (Nil)
    applyLeaves(rd.derive('A')) should be (Nil)

    val rD = RegExpParser("""\D""")
    applyLeaves(rD.derive('a')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rD.derive('A')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rD.derive('0')) should be (Nil)
    applyLeaves(rD.derive('9')) should be (Nil)

    val rh = RegExpParser("""\h""")
    applyLeaves(rh.derive('\u0009')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rh.derive('a')) should be (Nil)

    val rH = RegExpParser("""\H""")
    applyLeaves(rH.derive('a')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rH.derive('\u0009')) should be (Nil)

    val rR = RegExpParser("""\R""")
    applyLeaves(rR.derive('\r')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rR.derive('\n')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rR.derive('a')) should be (Nil)

    val rs = RegExpParser("""\s""")
    applyLeaves(rs.derive(' ')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rs.derive('\t')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rs.derive('\n')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rs.derive('\r')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rs.derive('\f')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rs.derive('a')) should be (Nil)

    val rS = RegExpParser("""\S""")
    applyLeaves(rS.derive('a')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rS.derive(' ')) should be (Nil)
    applyLeaves(rS.derive('\t')) should be (Nil)
    applyLeaves(rS.derive('\n')) should be (Nil)
    applyLeaves(rS.derive('\r')) should be (Nil)
    applyLeaves(rS.derive('\f')) should be (Nil)

    val rv = RegExpParser("""\v""")
    applyLeaves(rv.derive('\u000B')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rv.derive('a')) should be (Nil)

    val rV = RegExpParser("""\V""")
    applyLeaves(rV.derive('a')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rV.derive('\u000B')) should be (Nil)

    val rw = RegExpParser("""\w""")
    applyLeaves(rw.derive('a')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rw.derive('z')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rw.derive('A')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rw.derive('Z')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rw.derive('0')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rw.derive('9')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rw.derive('_')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rw.derive('!')) should be (Nil)

    val rW = RegExpParser("""\W""")
    applyLeaves(rW.derive('!')) should be (List((Some(EpsExp()), false)))
    applyLeaves(rW.derive('0')) should be (Nil)
    applyLeaves(rW.derive('9')) should be (Nil)
    applyLeaves(rW.derive('a')) should be (Nil)
    applyLeaves(rW.derive('z')) should be (Nil)
    applyLeaves(rW.derive('A')) should be (Nil)
    applyLeaves(rW.derive('Z')) should be (Nil)
    applyLeaves(rW.derive('_')) should be (Nil)
  }

  it should "derive repeat expression" in {
    val r1 = RegExpParser("(?:ab){3,5}")
    applyLeaves(r1.derive('a')) should be (List((Some(RegExpParser("b(?:ab){2,4}")), false)))
    applyLeaves(r1.derive('b')) should be (Nil)

    val r2 = RegExpParser("(?:ab){3}")
    applyLeaves(r2.derive('a')) should be (List((Some(RegExpParser("b(?:ab){2,2}")), false)))
    applyLeaves(r2.derive('b')) should be (Nil)

    val r3 = RegExpParser("(?:ab){3,}")
    applyLeaves(r3.derive('a')) should be (List((Some(RegExpParser("b(?:ab){2,}")), false)))
    applyLeaves(r3.derive('b')) should be (Nil)

    val r4 = RegExpParser("(?:ab){,5}")
    applyLeaves(r4.derive('a')) should be (List((Some(RegExpParser("b(?:ab){,4}")), false), (None, true)))
    applyLeaves(r4.derive('b')) should be (List((None, true)))

    val r5 = RegExpParser("(?:ab){1,5}")
    applyLeaves(r5.derive('a')) should be (List((Some(RegExpParser("b(?:ab){,4}")), false)))
    applyLeaves(r5.derive('b')) should be (Nil)


    val r6 = RegExpParser("(?:ab){1,}")
    applyLeaves(r6.derive('a')) should be (List((Some(RegExpParser("b(?:ab)*")), false)))
    applyLeaves(r6.derive('b')) should be (Nil)

    val r7 = RegExpParser("(?:ab){,1}")
    applyLeaves(r7.derive('a')) should be (List((Some(RegExpParser("b")), false), (None, true)))
    applyLeaves(r7.derive('b')) should be (List((None, true)))

    val r8 = RegExpParser("(?:ab){1}")
    applyLeaves(r8.derive('a')) should be (List((Some(RegExpParser("b")), false)))
    applyLeaves(r8.derive('b')) should be (Nil)
  }

  it should "derive ^" in {
    val r = RegExpParser("^")
    applyLeaves(r.derive('a')) should be (List((None, true)))
    leaves(r.derive('a').apply(false)) should be (Nil)
  }

  it should "derive $" in {
    val r = RegExpParser("$")
    applyLeaves(r.derive('a')) should be (Nil)
  }

  it should "derive lookahead" in {
    val r1 = RegExpParser("(?=ab)")
    r1.derive('a').apply(true) should be (AAssert[(Option[RegExp[Char]], Boolean),(Option[RegExp[Char]], Boolean)](
      ALeaf((Some(ElemExp('b')), false)), ALeaf((None, true))))

    val r2 = RegExpParser("(?!ab)")
    r2.derive('a').apply(true) should be (AAssertNot[(Option[RegExp[Char]], Boolean),(Option[RegExp[Char]], Boolean)](
      ALeaf((Some(ElemExp('b')), false)), ALeaf((None, true))))
  }

  it should "derive lazy operations" in {
    val r1 = RegExpParser("a*?")
    applyLeaves(r1.derive('a')) should be (List((None, true), (Some(StarExp(ElemExp('a'), false)), false)))
    applyLeaves(r1.derive('b')) should be (List((None, true)))

    val r2 = RegExpParser("a+?")
    applyLeaves(r2.derive('a')) should be (List((Some(StarExp(ElemExp('a'), false)), false)))
    applyLeaves(r2.derive('b')) should be (Nil)

    val r3 = RegExpParser("a??")
    applyLeaves(r3.derive('a')) should be (List((None, true), (Some(EpsExp()), false)))
    applyLeaves(r3.derive('b')) should be (List((None, true)))

    val r4 = RegExpParser("(?:ab){3,5}?")
    applyLeaves(r4.derive('a')) should be (List((Some(RegExpParser("b(?:ab){2,4}?")), false)))
    applyLeaves(r4.derive('b')) should be (Nil)

    val r5 = RegExpParser("(?:ab){,5}?")
    applyLeaves(r5.derive('a')) should be (List((None, true), (Some(RegExpParser("b(?:ab){,4}?")), false)))
    applyLeaves(r5.derive('b')) should be (List((None, true)))

    val r6 = RegExpParser("(?:ab){,1}?")
    applyLeaves(r6.derive('a')) should be (List((None, true), (Some(RegExpParser("b")), false)))
    applyLeaves(r6.derive('b')) should be (List((None, true)))
  }

  it should "derive complex expression" in {
    val r1 = RegExpParser("a*(?:bc|d)")
    applyLeaves(r1.derive('a')) should be (List((Some(RegExpParser("a*(?:bc|d)")), false)))
    applyLeaves(r1.derive('b')) should be (List((Some(RegExpParser("c")), false)))
    applyLeaves(r1.derive('c')) should be (Nil)
    applyLeaves(r1.derive('d')) should be (List((Some(RegExpParser("ε")), false)))
    applyLeaves(r1.derive('e')) should be (Nil)

    val r2 = RegExpParser("(?:a*)*")
    applyLeaves(r2.derive('a')) should be (List((Some(RegExpParser("a*(?:a*)*")), false), (None, true), (None, true)))
    applyLeaves(r2.derive('b')) should be (List((None, true), (None, true)))
  }


  "derive with character not appear in given expression" should "derive a" in {
    val r = RegExpParser("a")
    applyLeaves(r.derive(None)) should be (Nil)
  }

  it should "derive ." in {
    val r = RegExpParser(".")
    applyLeaves(r.derive(None)) should be (List((Some(EpsExp()), false)))
  }

  it should "derive character class" in {
    val r1 = RegExpParser("[a-z]")
    applyLeaves(r1.derive(None)) should be (Nil)

    val r2 = RegExpParser("[^a-z]")
    applyLeaves(r2.derive(None)) should be (List((Some(EpsExp()), false)))

    val r3 = RegExpParser("""[\w]""")
    applyLeaves(r3.derive(None)) should be (Nil)

    val r4 = RegExpParser("""[\W]""")
    applyLeaves(r4.derive(None)) should be (List((Some(EpsExp()), false)))

    val r6 = RegExpParser("""[0-9\w]""")
    applyLeaves(r6.derive(None)) should be (Nil)

    val r7 = RegExpParser("""[0-9\W]""")
    applyLeaves(r7.derive(None)) should be (List((Some(EpsExp()), false)))

    val r8 = RegExpParser("""[^0-9\W]""")
    applyLeaves(r8.derive(None)) should be (Nil)
  }

  it should "derive meta character" in {
    val r1 = RegExpParser("""\w""")
    applyLeaves(r1.derive(None)) should be (Nil)

    val r2 = RegExpParser("""\W""")
    applyLeaves(r2.derive(None)) should be (List((Some(EpsExp()), false)))
  }


  "derive with ignore case option" should "derive character" in {
    deriver = new RegExpDeriver[StateTBooleanATree](new PCREOptions("i"))

    val r1 = RegExpParser("a")
    applyLeaves(r1.derive('a')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r1.derive('A')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r1.derive('b')) should be (Nil)
    applyLeaves(r1.derive('B')) should be (Nil)

    val r2 = RegExpParser("A")
    applyLeaves(r2.derive('a')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r2.derive('A')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r2.derive('b')) should be (Nil)
    applyLeaves(r2.derive('B')) should be (Nil)
  }

  it should "derive character class" in {
    deriver = new RegExpDeriver[StateTBooleanATree](new PCREOptions("i"))

    val r1 = RegExpParser("[ac-e]")
    applyLeaves(r1.derive('a')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r1.derive('A')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r1.derive('d')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r1.derive('D')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r1.derive('b')) should be (Nil)
    applyLeaves(r1.derive('B')) should be (Nil)

    val r2 = RegExpParser("""[^abB]""")
    applyLeaves(r2.derive('c')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r2.derive('C')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r2.derive('b')) should be (Nil)
    applyLeaves(r2.derive('B')) should be (Nil)
    applyLeaves(r2.derive('a')) should be (Nil)
    applyLeaves(r2.derive('A')) should be (Nil)
  }

  "derive with dot all option" should "derive dot" in {
    deriver = new RegExpDeriver[StateTBooleanATree](new PCREOptions("s"))

    val r = RegExpParser(".")
    applyLeaves(r.derive('a')) should be (List((Some(EpsExp()), false)))
    applyLeaves(r.derive('\n')) should be (List((Some(EpsExp()), false)))
  }

  "derive with ungreedy option" should "derive repeat expression" in {
    deriver = new RegExpDeriver[StateTBooleanATree](new PCREOptions("U"))

    val r1 = RegExpParser("a*")
    applyLeaves(r1.derive('a')) should be (List((None, true), (Some(StarExp(ElemExp('a'), true)), false)))

    val r2 = RegExpParser("a*?")
    applyLeaves(r2.derive('a')) should be (List((Some(StarExp(ElemExp('a'), false)), false), (None, true)))
  }

  "deriveEOL" should "derive a" in {
    val r = RegExpParser("a")
    applyLeaves(r.deriveEOL()) should be (empty)
  }

  it should "derive ∅" in {
    val r = RegExpParser("∅")
    applyLeaves(r.deriveEOL()) should be (empty)
  }

  it should "derive ε" in {
    val r = RegExpParser("ε")
    applyLeaves(r.deriveEOL()) should not be (empty)
  }

  it should "derive ab" in {
    val r = RegExpParser("ab")
    applyLeaves(r.deriveEOL()) should be (empty)
  }

  it should "derive a|b" in {
    val r = RegExpParser("a|b")
    applyLeaves(r.deriveEOL()) should be (empty)
  }

  it should "derive a*" in {
    val r = RegExpParser("a*")
    applyLeaves(r.deriveEOL()) should not be (empty)
  }

  it should "derive +" in {
    val r1 = RegExpParser("a+")
    applyLeaves(r1.deriveEOL()) should be (empty)

    val r2 = RegExpParser("(?:a*)+")
    applyLeaves(r2.deriveEOL()) should not be (empty)
  }

  it should "derive a?" in {
    val r = RegExpParser("a?")
    applyLeaves(r.deriveEOL()) should not be (empty)
  }

  it should "derive ." in {
    val r = RegExpParser(".")
    applyLeaves(r.deriveEOL()) should be (empty)
  }

  it should "derive character class" in {
    val r1 = RegExpParser("""[a-z]""")
    applyLeaves(r1.deriveEOL()) should be (empty)

    val r2 = RegExpParser("""[^A-Z]""")
    applyLeaves(r2.deriveEOL()) should be (empty)
  }

  it should "derive meta character" in {
    val r = RegExpParser("""\d""")
    applyLeaves(r.deriveEOL()) should be (empty)
  }

  it should "derive repeat expression" in {
    val r1 = RegExpParser("(?:ab){3,5}")
    applyLeaves(r1.deriveEOL()) should be (empty)

    val r2 = RegExpParser("(?:ab){3,}")
    applyLeaves(r2.deriveEOL()) should be (empty)

    val r3 = RegExpParser("(?:ab){,5}")
    applyLeaves(r3.deriveEOL()) should not be (empty)

    val r4 = RegExpParser("(?:a*){3,5}")
    applyLeaves(r4.deriveEOL()) should not be (empty)
  }

  it should "derive ^" in {
    val r = RegExpParser("^")
    applyLeaves(r.deriveEOL()) should not be (empty)
    leaves(r.deriveEOL().apply(false)) should be (empty)
  }

  it should "derive $" in {
    val r = RegExpParser("$")
    applyLeaves(r.deriveEOL()) should not be (empty)
  }

  it should "derive lookahead" in {
    val r1 = RegExpParser("(?=ab)")
    r1.deriveEOL().apply(true) should be (
      AAssert[(Unit, Boolean), (Unit, Boolean)](AFail(), ALeaf(((), true))))

    val r2 = RegExpParser("(?!ab)")
    r2.deriveEOL().apply(true) should be (
      AAssertNot[(Unit, Boolean), (Unit, Boolean)](AFail(), ALeaf(((), true))))
  }

  it should "derive complex expression" in {
    val r1 = RegExpParser("(?:a|b*)c+d*")
    applyLeaves(r1.deriveEOL()) should be (empty)

    val r2 = RegExpParser("(?:a|b*)c?")
    applyLeaves(r2.deriveEOL()) should not be (empty)
  }
}
