package matching.regexp

import org.scalatest._
import RegExp._
import matching.monad._
import DTree.DTreeMonad._
import StateT._

class RegExpDeriverSpec extends FlatSpec with Matchers {
  implicit var deriver = new RegExpDeriver[StateTStringDTree]()

  val eps = Vector()
  val anyChar = Vector(None)
  def char(a: Char) = Vector(Some(a))
  def applyLeaves[A](m: StateTStringDTree[A,A])
    = leaves(m.apply(eps))

  "derive" should "derive a" in {
    val r = RegExpParser("a")
    applyLeaves(r.derive('a')) should be (List((Some(EpsExp()), char('a'))))
    applyLeaves(r.derive('b')) should be (Nil)
  }

  it should "derive ∅" in {
    val r = RegExpParser("∅")
    applyLeaves(r.derive('a')) should be (Nil)
  }

  it should "derive ε" in {
    val r = RegExpParser("ε")
    applyLeaves(r.derive('a')) should be (List((None, eps)))
  }

  it should "derive ab" in {
    val r = RegExpParser("ab")
    applyLeaves(r.derive('a')) should be (List((Some(ElemExp('b')), char('a'))))
    applyLeaves(r.derive('b')) should be (Nil)
    applyLeaves(r.derive('c')) should be (Nil)
  }

  it should "derive a|b" in {
    val r = RegExpParser("a|b")
    applyLeaves(r.derive('a')) should be (List((Some(EpsExp()), char('a'))))
    applyLeaves(r.derive('b')) should be (List((Some(EpsExp()), char('b'))))
    applyLeaves(r.derive('c')) should be (Nil)
  }

  it should "derive a*" in {
    val r = RegExpParser("a*")
    applyLeaves(r.derive('a')) should be (List((Some(StarExp(ElemExp('a'), true)), char('a')), (None, eps)))
    applyLeaves(r.derive('b')) should be (List((None, eps)))
  }

  it should "derive a+" in {
    val r = RegExpParser("a+")
    applyLeaves(r.derive('a')) should be (List((Some(StarExp(ElemExp('a'), true)), char('a'))))
    applyLeaves(r.derive('b')) should be (Nil)
  }

  it should "derive a?" in {
    val r = RegExpParser("a?")
    applyLeaves(r.derive('a')) should be (List((Some(EpsExp()), char('a')), (None, eps)))
    applyLeaves(r.derive('b')) should be (List((None, eps)))
  }

  it should "derive ." in {
    val r = RegExpParser(".")
    applyLeaves(r.derive('a')) should be (List((Some(EpsExp()), char('a'))))
    applyLeaves(r.derive('\n')) should be (Nil)
  }

  it should "derive character class" in {
    val r1 = RegExpParser("""[ah-k\d]""")
    applyLeaves(r1.derive('a')) should be (List((Some(EpsExp()), char('a'))))
    applyLeaves(r1.derive('i')) should be (List((Some(EpsExp()), char('i'))))
    applyLeaves(r1.derive('0')) should be (List((Some(EpsExp()), char('0'))))
    applyLeaves(r1.derive('c')) should be (Nil)
    applyLeaves(r1.derive('A')) should be (Nil)
    applyLeaves(r1.derive('H')) should be (Nil)
    applyLeaves(r1.derive('Z')) should be (Nil)

    val r2 = RegExpParser("""[^ah-k\d]""")
    applyLeaves(r2.derive('a')) should be (Nil)
    applyLeaves(r2.derive('i')) should be (Nil)
    applyLeaves(r2.derive('0')) should be (Nil)
    applyLeaves(r2.derive('c')) should be (List((Some(EpsExp()), char('c'))))
    applyLeaves(r2.derive('A')) should be (List((Some(EpsExp()), char('A'))))
    applyLeaves(r2.derive('H')) should be (List((Some(EpsExp()), char('H'))))
    applyLeaves(r2.derive('Z')) should be (List((Some(EpsExp()), char('Z'))))

    val r3 = RegExpParser("""[\W\D]""")
    applyLeaves(r3.derive('a')) should be (List((Some(EpsExp()), char('a'))))
    applyLeaves(r3.derive('A')) should be (List((Some(EpsExp()), char('A'))))
    applyLeaves(r3.derive('!')) should be (List((Some(EpsExp()), char('!'))))
    applyLeaves(r3.derive('0')) should be (Nil)

    val r4 = RegExpParser("""[^0\D]""")
    applyLeaves(r4.derive('1')) should be (List((Some(EpsExp()), char('1'))))
    applyLeaves(r4.derive('0')) should be (Nil)
    applyLeaves(r4.derive('a')) should be (Nil)
    applyLeaves(r4.derive('A')) should be (Nil)
    applyLeaves(r4.derive('!')) should be (Nil)
  }

  it should "derive meta character" in {
    val rd = RegExpParser("""\d""")
    applyLeaves(rd.derive('0')) should be (List((Some(EpsExp()), char('0'))))
    applyLeaves(rd.derive('9')) should be (List((Some(EpsExp()), char('9'))))
    applyLeaves(rd.derive('a')) should be (Nil)
    applyLeaves(rd.derive('A')) should be (Nil)

    val rD = RegExpParser("""\D""")
    applyLeaves(rD.derive('a')) should be (List((Some(EpsExp()), char('a'))))
    applyLeaves(rD.derive('A')) should be (List((Some(EpsExp()), char('A'))))
    applyLeaves(rD.derive('0')) should be (Nil)
    applyLeaves(rD.derive('9')) should be (Nil)

    val rh = RegExpParser("""\h""")
    applyLeaves(rh.derive('\u0009')) should be (List((Some(EpsExp()), char('\u0009'))))
    applyLeaves(rh.derive('a')) should be (Nil)

    val rH = RegExpParser("""\H""")
    applyLeaves(rH.derive('a')) should be (List((Some(EpsExp()), char('a'))))
    applyLeaves(rH.derive('\u0009')) should be (Nil)

    val rR = RegExpParser("""\R""")
    applyLeaves(rR.derive('\r')) should be (List((Some(EpsExp()), char('\r'))))
    applyLeaves(rR.derive('\n')) should be (List((Some(EpsExp()), char('\n'))))
    applyLeaves(rR.derive('a')) should be (Nil)

    val rs = RegExpParser("""\s""")
    applyLeaves(rs.derive(' ')) should be (List((Some(EpsExp()), char(' '))))
    applyLeaves(rs.derive('\t')) should be (List((Some(EpsExp()), char('\t'))))
    applyLeaves(rs.derive('\n')) should be (List((Some(EpsExp()), char('\n'))))
    applyLeaves(rs.derive('\r')) should be (List((Some(EpsExp()), char('\r'))))
    applyLeaves(rs.derive('\f')) should be (List((Some(EpsExp()), char('\f'))))
    applyLeaves(rs.derive('a')) should be (Nil)

    val rS = RegExpParser("""\S""")
    applyLeaves(rS.derive('a')) should be (List((Some(EpsExp()), char('a'))))
    applyLeaves(rS.derive(' ')) should be (Nil)
    applyLeaves(rS.derive('\t')) should be (Nil)
    applyLeaves(rS.derive('\n')) should be (Nil)
    applyLeaves(rS.derive('\r')) should be (Nil)
    applyLeaves(rS.derive('\f')) should be (Nil)

    val rv = RegExpParser("""\v""")
    applyLeaves(rv.derive('\u000B')) should be (List((Some(EpsExp()), char('\u000B'))))
    applyLeaves(rv.derive('a')) should be (Nil)

    val rV = RegExpParser("""\V""")
    applyLeaves(rV.derive('a')) should be (List((Some(EpsExp()), char('a'))))
    applyLeaves(rV.derive('\u000B')) should be (Nil)

    val rw = RegExpParser("""\w""")
    applyLeaves(rw.derive('a')) should be (List((Some(EpsExp()), char('a'))))
    applyLeaves(rw.derive('z')) should be (List((Some(EpsExp()), char('z'))))
    applyLeaves(rw.derive('A')) should be (List((Some(EpsExp()), char('A'))))
    applyLeaves(rw.derive('Z')) should be (List((Some(EpsExp()), char('Z'))))
    applyLeaves(rw.derive('0')) should be (List((Some(EpsExp()), char('0'))))
    applyLeaves(rw.derive('9')) should be (List((Some(EpsExp()), char('9'))))
    applyLeaves(rw.derive('_')) should be (List((Some(EpsExp()), char('_'))))
    applyLeaves(rw.derive('!')) should be (Nil)

    val rW = RegExpParser("""\W""")
    applyLeaves(rW.derive('!')) should be (List((Some(EpsExp()), char('!'))))
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
    applyLeaves(r1.derive('a')) should be (List((Some(RegExpParser("b(?:ab){2,4}")), char('a'))))
    applyLeaves(r1.derive('b')) should be (Nil)

    val r2 = RegExpParser("(?:ab){3}")
    applyLeaves(r2.derive('a')) should be (List((Some(RegExpParser("b(?:ab){2,2}")), char('a'))))
    applyLeaves(r2.derive('b')) should be (Nil)

    val r3 = RegExpParser("(?:ab){3,}")
    applyLeaves(r3.derive('a')) should be (List((Some(RegExpParser("b(?:ab){2,}")), char('a'))))
    applyLeaves(r3.derive('b')) should be (Nil)

    val r4 = RegExpParser("(?:ab){,5}")
    applyLeaves(r4.derive('a')) should be (List((Some(RegExpParser("b(?:ab){,4}")), char('a')), (None, eps)))
    applyLeaves(r4.derive('b')) should be (List((None, eps)))

    val r5 = RegExpParser("(?:ab){1,5}")
    applyLeaves(r5.derive('a')) should be (List((Some(RegExpParser("b(?:ab){,4}")), char('a'))))
    applyLeaves(r5.derive('b')) should be (Nil)


    val r6 = RegExpParser("(?:ab){1,}")
    applyLeaves(r6.derive('a')) should be (List((Some(RegExpParser("b(?:ab)*")), char('a'))))
    applyLeaves(r6.derive('b')) should be (Nil)

    val r7 = RegExpParser("(?:ab){,1}")
    applyLeaves(r7.derive('a')) should be (List((Some(RegExpParser("b")), char('a')), (None, eps)))
    applyLeaves(r7.derive('b')) should be (List((None, eps)))

    val r8 = RegExpParser("(?:ab){1}")
    applyLeaves(r8.derive('a')) should be (List((Some(RegExpParser("b")), char('a'))))
    applyLeaves(r8.derive('b')) should be (Nil)
  }

  it should "derive ^" in {
    val r = RegExpParser("^")
    applyLeaves(r.derive('a')) should be (List((None, eps)))
    leaves(r.derive('a').apply(Vector(None))) should be (Nil)
  }

  it should "derive $" in {
    val r = RegExpParser("$")
    applyLeaves(r.derive('a')) should be (Nil)
  }

  it should "derive lookahead" in {
    val r1 = RegExpParser("(?=ab)")
    r1.derive('a').apply(eps) should be (DAssert[(Option[RegExp[Char]], OptString),(Option[RegExp[Char]], OptString)](
      DLeaf((Some(ElemExp('b')), char('a'))), DLeaf((None, eps))))

    val r2 = RegExpParser("(?!ab)")
    r2.derive('a').apply(eps) should be (DAssertNot[(Option[RegExp[Char]], OptString),(Option[RegExp[Char]], OptString)](
      DLeaf((Some(ElemExp('b')), char('a'))), DLeaf((None, eps))))
  }

  it should "derive lazy operations" in {
    val r1 = RegExpParser("a*?")
    applyLeaves(r1.derive('a')) should be (List((None, eps), (Some(StarExp(ElemExp('a'), false)), char('a'))))
    applyLeaves(r1.derive('b')) should be (List((None, eps)))

    val r2 = RegExpParser("a+?")
    applyLeaves(r2.derive('a')) should be (List((Some(StarExp(ElemExp('a'), false)), char('a'))))
    applyLeaves(r2.derive('b')) should be (Nil)

    val r3 = RegExpParser("a??")
    applyLeaves(r3.derive('a')) should be (List((None, eps), (Some(EpsExp()), char('a'))))
    applyLeaves(r3.derive('b')) should be (List((None, eps)))

    val r4 = RegExpParser("(?:ab){3,5}?")
    applyLeaves(r4.derive('a')) should be (List((Some(RegExpParser("b(?:ab){2,4}?")), char('a'))))
    applyLeaves(r4.derive('b')) should be (Nil)

    val r5 = RegExpParser("(?:ab){,5}?")
    applyLeaves(r5.derive('a')) should be (List((None, eps), (Some(RegExpParser("b(?:ab){,4}?")), char('a'))))
    applyLeaves(r5.derive('b')) should be (List((None, eps)))

    val r6 = RegExpParser("(?:ab){,1}?")
    applyLeaves(r6.derive('a')) should be (List((None, eps), (Some(RegExpParser("b")), char('a'))))
    applyLeaves(r6.derive('b')) should be (List((None, eps)))
  }

  it should "derive complex expression" in {
    val r1 = RegExpParser("a*(?:bc|d)")
    applyLeaves(r1.derive('a')) should be (List((Some(RegExpParser("a*(?:bc|d)")), char('a'))))
    applyLeaves(r1.derive('b')) should be (List((Some(RegExpParser("c")), char('b'))))
    applyLeaves(r1.derive('c')) should be (Nil)
    applyLeaves(r1.derive('d')) should be (List((Some(RegExpParser("ε")), char('d'))))
    applyLeaves(r1.derive('e')) should be (Nil)

    val r2 = RegExpParser("(?:a*)*")
    applyLeaves(r2.derive('a')) should be (List((Some(RegExpParser("a*(?:a*)*")), char('a')), (None, eps), (None, eps)))
    applyLeaves(r2.derive('b')) should be (List((None, eps), (None, eps)))
  }


  "derive with character not appear in given expression" should "derive a" in {
    val r = RegExpParser("a")
    applyLeaves(r.derive(None)) should be (Nil)
  }

  it should "derive ." in {
    val r = RegExpParser(".")
    applyLeaves(r.derive(None)) should be (List((Some(EpsExp()), anyChar)))
  }

  it should "derive character class" in {
    val r1 = RegExpParser("[a-z]")
    applyLeaves(r1.derive(None)) should be (Nil)

    val r2 = RegExpParser("[^a-z]")
    applyLeaves(r2.derive(None)) should be (List((Some(EpsExp()), anyChar)))

    val r3 = RegExpParser("""[\w]""")
    applyLeaves(r3.derive(None)) should be (Nil)

    val r4 = RegExpParser("""[\W]""")
    applyLeaves(r4.derive(None)) should be (List((Some(EpsExp()), anyChar)))

    val r6 = RegExpParser("""[0-9\w]""")
    applyLeaves(r6.derive(None)) should be (Nil)

    val r7 = RegExpParser("""[0-9\W]""")
    applyLeaves(r7.derive(None)) should be (List((Some(EpsExp()), anyChar)))

    val r8 = RegExpParser("""[^0-9\W]""")
    applyLeaves(r8.derive(None)) should be (Nil)
  }

  it should "derive meta character" in {
    val r1 = RegExpParser("""\w""")
    applyLeaves(r1.derive(None)) should be (Nil)

    val r2 = RegExpParser("""\W""")
    applyLeaves(r2.derive(None)) should be (List((Some(EpsExp()), anyChar)))
  }


  "derive with ignore case option" should "derive character" in {
    deriver = new RegExpDeriver[StateTStringDTree](new PCREOptions("i"))

    val r1 = RegExpParser("a")
    applyLeaves(r1.derive('a')) should be (List((Some(EpsExp()), char('a'))))
    applyLeaves(r1.derive('A')) should be (List((Some(EpsExp()), char('A'))))
    applyLeaves(r1.derive('b')) should be (Nil)
    applyLeaves(r1.derive('B')) should be (Nil)

    val r2 = RegExpParser("A")
    applyLeaves(r2.derive('a')) should be (List((Some(EpsExp()), char('a'))))
    applyLeaves(r2.derive('A')) should be (List((Some(EpsExp()), char('A'))))
    applyLeaves(r2.derive('b')) should be (Nil)
    applyLeaves(r2.derive('B')) should be (Nil)
  }

  it should "derive character class" in {
    deriver = new RegExpDeriver[StateTStringDTree](new PCREOptions("i"))

    val r1 = RegExpParser("[ac-e]")
    applyLeaves(r1.derive('a')) should be (List((Some(EpsExp()), char('a'))))
    applyLeaves(r1.derive('A')) should be (List((Some(EpsExp()), char('A'))))
    applyLeaves(r1.derive('d')) should be (List((Some(EpsExp()), char('d'))))
    applyLeaves(r1.derive('D')) should be (List((Some(EpsExp()), char('D'))))
    applyLeaves(r1.derive('b')) should be (Nil)
    applyLeaves(r1.derive('B')) should be (Nil)

    val r2 = RegExpParser("""[^abB]""")
    applyLeaves(r2.derive('c')) should be (List((Some(EpsExp()), char('c'))))
    applyLeaves(r2.derive('C')) should be (List((Some(EpsExp()), char('C'))))
    applyLeaves(r2.derive('b')) should be (Nil)
    applyLeaves(r2.derive('B')) should be (Nil)
    applyLeaves(r2.derive('a')) should be (Nil)
    applyLeaves(r2.derive('A')) should be (Nil)
  }

  "derive with dot all option" should "derive dot" in {
    deriver = new RegExpDeriver[StateTStringDTree](new PCREOptions("s"))

    val r = RegExpParser(".")
    applyLeaves(r.derive('a')) should be (List((Some(EpsExp()), char('a'))))
    applyLeaves(r.derive('\n')) should be (List((Some(EpsExp()), char('\n'))))
  }

  "derive with ungreedy option" should "derive repeat expression" in {
    deriver = new RegExpDeriver[StateTStringDTree](new PCREOptions("U"))

    val r1 = RegExpParser("a*")
    applyLeaves(r1.derive('a')) should be (List((None, eps), (Some(StarExp(ElemExp('a'), true)), char('a'))))

    val r2 = RegExpParser("a*?")
    applyLeaves(r2.derive('a')) should be (List((Some(StarExp(ElemExp('a'), false)), char('a')), (None, eps)))
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
    leaves(r.deriveEOL().apply(anyChar)) should be (empty)
  }

  it should "derive $" in {
    val r = RegExpParser("$")
    applyLeaves(r.deriveEOL()) should not be (empty)
  }

  it should "derive lookahead" in {
    val r1 = RegExpParser("(?=ab)")
    r1.deriveEOL().apply(eps) should be (
      DAssert[(Unit, OptString), (Unit, OptString)](DFail(), DLeaf(((), eps))))

    val r2 = RegExpParser("(?!ab)")
    r2.deriveEOL().apply(eps) should be (
      DAssertNot[(Unit, OptString), (Unit, OptString)](DFail(), DLeaf(((), eps))))
  }

  it should "derive complex expression" in {
    val r1 = RegExpParser("(?:a|b*)c+d*")
    applyLeaves(r1.deriveEOL()) should be (empty)

    val r2 = RegExpParser("(?:a|b*)c?")
    applyLeaves(r2.deriveEOL()) should not be (empty)
  }
}
