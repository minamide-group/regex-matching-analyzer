package matching.regexp

import org.scalatest._
import RegExp._
import matching.monad._
import DTree.DTreeMonad._

class RegExpDeriverSpec extends FlatSpec with Matchers {
  implicit var deriver = new RegExpDeriver[DTree]()

  "derive" should "derive a" in {
    val r = RegExpParser("a")
    leaves(r.derive('a', Nil)) should be (List(Some(EpsExp())))
    leaves(r.derive('b', Nil)) should be (Nil)
  }

  it should "derive ∅" in {
    val r = RegExpParser("∅")
    leaves(r.derive('a', Nil)) should be (Nil)
  }

  it should "derive ε" in {
    val r = RegExpParser("ε")
    leaves(r.derive('a', Nil)) should be (List(None))
  }

  it should "derive ab" in {
    val r = RegExpParser("ab")
    leaves(r.derive('a', Nil)) should be (List(Some(ElemExp('b'))))
    leaves(r.derive('b', Nil)) should be (Nil)
    leaves(r.derive('c', Nil)) should be (Nil)
  }

  it should "derive a|b" in {
    val r = RegExpParser("a|b")
    leaves(r.derive('a', Nil)) should be (List(Some(EpsExp())))
    leaves(r.derive('b', Nil)) should be (List(Some(EpsExp())))
    leaves(r.derive('c', Nil)) should be (Nil)
  }

  it should "derive a*" in {
    val r = RegExpParser("a*")
    leaves(r.derive('a', Nil)) should be (List(Some(StarExp(ElemExp('a'), true)), None))
    leaves(r.derive('b', Nil)) should be (List(None))
  }

  it should "derive a+" in {
    val r = RegExpParser("a+")
    leaves(r.derive('a', Nil)) should be (List(Some(StarExp(ElemExp('a'), true))))
    leaves(r.derive('b', Nil)) should be (Nil)
  }

  it should "derive a?" in {
    val r = RegExpParser("a?")
    leaves(r.derive('a', Nil)) should be (List(Some(EpsExp()), None))
    leaves(r.derive('b', Nil)) should be (List(None))
  }

  it should "derive ." in {
    val r = RegExpParser(".")
    leaves(r.derive('a', Nil)) should be (List(Some(EpsExp())))
    leaves(r.derive('\n', Nil)) should be (Nil)
  }

  it should "derive character class" in {
    val r1 = RegExpParser("""[ah-k\d]""")
    leaves(r1.derive('a', Nil)) should be (List(Some(EpsExp())))
    leaves(r1.derive('i', Nil)) should be (List(Some(EpsExp())))
    leaves(r1.derive('0', Nil)) should be (List(Some(EpsExp())))
    leaves(r1.derive('c', Nil)) should be (Nil)
    leaves(r1.derive('A', Nil)) should be (Nil)
    leaves(r1.derive('H', Nil)) should be (Nil)
    leaves(r1.derive('Z', Nil)) should be (Nil)

    val r2 = RegExpParser("""[^ah-k\d]""")
    leaves(r2.derive('a', Nil)) should be (Nil)
    leaves(r2.derive('i', Nil)) should be (Nil)
    leaves(r2.derive('0', Nil)) should be (Nil)
    leaves(r2.derive('c', Nil)) should be (List(Some(EpsExp())))
    leaves(r2.derive('A', Nil)) should be (List(Some(EpsExp())))
    leaves(r2.derive('H', Nil)) should be (List(Some(EpsExp())))
    leaves(r2.derive('Z', Nil)) should be (List(Some(EpsExp())))

    val r3 = RegExpParser("""[\W\D]""")
    leaves(r3.derive('a', Nil)) should be (List(Some(EpsExp())))
    leaves(r3.derive('A', Nil)) should be (List(Some(EpsExp())))
    leaves(r3.derive('!', Nil)) should be (List(Some(EpsExp())))
    leaves(r3.derive('0', Nil)) should be (Nil)

    val r4 = RegExpParser("""[^0\D]""")
    leaves(r4.derive('1', Nil)) should be (List(Some(EpsExp())))
    leaves(r4.derive('0', Nil)) should be (Nil)
    leaves(r4.derive('a', Nil)) should be (Nil)
    leaves(r4.derive('A', Nil)) should be (Nil)
    leaves(r4.derive('!', Nil)) should be (Nil)
  }

  it should "derive meta character" in {
    val rd = RegExpParser("""\d""")
    leaves(rd.derive('0', Nil)) should be (List(Some(EpsExp())))
    leaves(rd.derive('9', Nil)) should be (List(Some(EpsExp())))
    leaves(rd.derive('a', Nil)) should be (Nil)
    leaves(rd.derive('A', Nil)) should be (Nil)

    val rD = RegExpParser("""\D""")
    leaves(rD.derive('a', Nil)) should be (List(Some(EpsExp())))
    leaves(rD.derive('A', Nil)) should be (List(Some(EpsExp())))
    leaves(rD.derive('0', Nil)) should be (Nil)
    leaves(rD.derive('9', Nil)) should be (Nil)

    val rh = RegExpParser("""\h""")
    leaves(rh.derive('\u0009', Nil)) should be (List(Some(EpsExp())))
    leaves(rh.derive('a', Nil)) should be (Nil)

    val rH = RegExpParser("""\H""")
    leaves(rH.derive('a', Nil)) should be (List(Some(EpsExp())))
    leaves(rH.derive('\u0009', Nil)) should be (Nil)

    val rR = RegExpParser("""\R""")
    leaves(rR.derive('\r', Nil)) should be (List(Some(EpsExp())))
    leaves(rR.derive('\n', Nil)) should be (List(Some(EpsExp())))
    leaves(rR.derive('a', Nil)) should be (Nil)

    val rs = RegExpParser("""\s""")
    leaves(rs.derive(' ', Nil)) should be (List(Some(EpsExp())))
    leaves(rs.derive('\t', Nil)) should be (List(Some(EpsExp())))
    leaves(rs.derive('\n', Nil)) should be (List(Some(EpsExp())))
    leaves(rs.derive('\r', Nil)) should be (List(Some(EpsExp())))
    leaves(rs.derive('\f', Nil)) should be (List(Some(EpsExp())))
    leaves(rs.derive('a', Nil)) should be (Nil)

    val rS = RegExpParser("""\S""")
    leaves(rS.derive('a', Nil)) should be (List(Some(EpsExp())))
    leaves(rS.derive(' ', Nil)) should be (Nil)
    leaves(rS.derive('\t', Nil)) should be (Nil)
    leaves(rS.derive('\n', Nil)) should be (Nil)
    leaves(rS.derive('\r', Nil)) should be (Nil)
    leaves(rS.derive('\f', Nil)) should be (Nil)

    val rv = RegExpParser("""\v""")
    leaves(rv.derive('\u000B', Nil)) should be (List(Some(EpsExp())))
    leaves(rv.derive('a', Nil)) should be (Nil)

    val rV = RegExpParser("""\V""")
    leaves(rV.derive('a', Nil)) should be (List(Some(EpsExp())))
    leaves(rV.derive('\u000B', Nil)) should be (Nil)

    val rw = RegExpParser("""\w""")
    leaves(rw.derive('a', Nil)) should be (List(Some(EpsExp())))
    leaves(rw.derive('z', Nil)) should be (List(Some(EpsExp())))
    leaves(rw.derive('A', Nil)) should be (List(Some(EpsExp())))
    leaves(rw.derive('Z', Nil)) should be (List(Some(EpsExp())))
    leaves(rw.derive('0', Nil)) should be (List(Some(EpsExp())))
    leaves(rw.derive('9', Nil)) should be (List(Some(EpsExp())))
    leaves(rw.derive('_', Nil)) should be (List(Some(EpsExp())))
    leaves(rw.derive('!', Nil)) should be (Nil)

    val rW = RegExpParser("""\W""")
    leaves(rW.derive('!', Nil)) should be (List(Some(EpsExp())))
    leaves(rW.derive('0', Nil)) should be (Nil)
    leaves(rW.derive('9', Nil)) should be (Nil)
    leaves(rW.derive('a', Nil)) should be (Nil)
    leaves(rW.derive('z', Nil)) should be (Nil)
    leaves(rW.derive('A', Nil)) should be (Nil)
    leaves(rW.derive('Z', Nil)) should be (Nil)
    leaves(rW.derive('_', Nil)) should be (Nil)
  }

  it should "derive repeat expression" in {
    val r1 = RegExpParser("(ab){3,5}")
    leaves(r1.derive('a', Nil)) should be (List(Some(RegExpParser("b(ab){2,4}"))))
    leaves(r1.derive('b', Nil)) should be (Nil)

    val r2 = RegExpParser("(ab){3}")
    leaves(r2.derive('a', Nil)) should be (List(Some(RegExpParser("b(ab){2,2}"))))
    leaves(r2.derive('b', Nil)) should be (Nil)

    val r3 = RegExpParser("(ab){3,}")
    leaves(r3.derive('a', Nil)) should be (List(Some(RegExpParser("b(ab){2,}"))))
    leaves(r3.derive('b', Nil)) should be (Nil)

    val r4 = RegExpParser("(ab){,5}")
    leaves(r4.derive('a', Nil)) should be (List(Some(RegExpParser("b(ab){,4}")), None))
    leaves(r4.derive('b', Nil)) should be (List(None))

    val r5 = RegExpParser("(ab){1,5}")
    leaves(r5.derive('a', Nil)) should be (List(Some(RegExpParser("b(ab){,4}"))))
    leaves(r5.derive('b', Nil)) should be (Nil)


    val r6 = RegExpParser("(ab){1,}")
    leaves(r6.derive('a', Nil)) should be (List(Some(RegExpParser("b(ab)*"))))
    leaves(r6.derive('b', Nil)) should be (Nil)

    val r7 = RegExpParser("(ab){,1}")
    leaves(r7.derive('a', Nil)) should be (List(Some(RegExpParser("b")), None))
    leaves(r7.derive('b', Nil)) should be (List(None))

    val r8 = RegExpParser("(ab){1}")
    leaves(r8.derive('a', Nil)) should be (List(Some(RegExpParser("b"))))
    leaves(r8.derive('b', Nil)) should be (Nil)
  }

  it should "derive ^" in {
    val r = RegExpParser("^")
    leaves(r.derive('a', Nil)) should be (List(None))
    leaves(r.derive('a', List(None))) should be (Nil)
  }

  it should "derive $" in {
    val r = RegExpParser("$")
    leaves(r.derive('a', Nil)) should be (Nil)
  }

  it should "derive lookahead" in {
    val r1 = RegExpParser("(?=ab)")
    r1.derive('a', Nil) should be (DAssert[Option[RegExp[Char]],Option[RegExp[Char]]](
      DLeaf(Some(ElemExp('b'))), DLeaf(None)))

    val r2 = RegExpParser("(?!ab)")
    r2.derive('a', Nil) should be (DAssertNot[Option[RegExp[Char]],Option[RegExp[Char]]](
      DLeaf(Some(ElemExp('b'))), DLeaf(None)))
  }

  it should "derive lazy operations" in {
    val r1 = RegExpParser("a*?")
    leaves(r1.derive('a', Nil)) should be (List(None, Some(StarExp(ElemExp('a'), false))))
    leaves(r1.derive('b', Nil)) should be (List(None))

    val r2 = RegExpParser("a+?")
    leaves(r2.derive('a', Nil)) should be (List(Some(StarExp(ElemExp('a'), false))))
    leaves(r2.derive('b', Nil)) should be (Nil)

    val r3 = RegExpParser("a??")
    leaves(r3.derive('a', Nil)) should be (List(None, Some(EpsExp())))
    leaves(r3.derive('b', Nil)) should be (List(None))

    val r4 = RegExpParser("(ab){3,5}?")
    leaves(r4.derive('a', Nil)) should be (List(Some(RegExpParser("b(ab){2,4}?"))))
    leaves(r4.derive('b', Nil)) should be (Nil)

    val r5 = RegExpParser("(ab){,5}?")
    leaves(r5.derive('a', Nil)) should be (List(None, Some(RegExpParser("b(ab){,4}?"))))
    leaves(r5.derive('b', Nil)) should be (List(None))

    val r6 = RegExpParser("(ab){,1}?")
    leaves(r6.derive('a', Nil)) should be (List(None, Some(RegExpParser("b"))))
    leaves(r6.derive('b', Nil)) should be (List(None))
  }

  it should "derive complex expression" in {
    val r1 = RegExpParser("a*(bc|d)")
    leaves(r1.derive('a', Nil)) should be (List(Some(RegExpParser("a*(bc|d)"))))
    leaves(r1.derive('b', Nil)) should be (List(Some(RegExpParser("c"))))
    leaves(r1.derive('c', Nil)) should be (Nil)
    leaves(r1.derive('d', Nil)) should be (List(Some(RegExpParser("ε"))))
    leaves(r1.derive('e', Nil)) should be (Nil)

    val r2 = RegExpParser("(a*)*")
    leaves(r2.derive('a', Nil)) should be (List(Some(RegExpParser("a*(a*)*")), None, None))
    leaves(r2.derive('b', Nil)) should be (List(None, None))
  }


  "derive with character not appear in given expression" should "derive a" in {
    val r = RegExpParser("a")
    leaves(r.derive(None, Nil)) should be (Nil)
  }

  it should "derive ." in {
    val r = RegExpParser(".")
    leaves(r.derive(None, Nil)) should be (List(Some(EpsExp())))
  }

  it should "derive character class" in {
    val r1 = RegExpParser("[a-z]")
    leaves(r1.derive(None, Nil)) should be (Nil)

    val r2 = RegExpParser("[^a-z]")
    leaves(r2.derive(None, Nil)) should be (List(Some(EpsExp())))

    val r3 = RegExpParser("""[\w]""")
    leaves(r3.derive(None, Nil)) should be (Nil)

    val r4 = RegExpParser("""[\W]""")
    leaves(r4.derive(None, Nil)) should be (List(Some(EpsExp())))

    val r6 = RegExpParser("""[0-9\w]""")
    leaves(r6.derive(None, Nil)) should be (Nil)

    val r7 = RegExpParser("""[0-9\W]""")
    leaves(r7.derive(None, Nil)) should be (List(Some(EpsExp())))

    val r8 = RegExpParser("""[^0-9\W]""")
    leaves(r8.derive(None, Nil)) should be (Nil)
  }

  it should "derive meta character" in {
    val r1 = RegExpParser("""\w""")
    leaves(r1.derive(None, Nil)) should be (Nil)

    val r2 = RegExpParser("""\W""")
    leaves(r2.derive(None, Nil)) should be (List(Some(EpsExp())))
  }


  "derive with ignore case option" should "derive character" in {
    deriver = new RegExpDeriver[DTree](new PCREOptions("i"))

    val r1 = RegExpParser("a")
    leaves(r1.derive('a', Nil)) should be (List(Some(EpsExp())))
    leaves(r1.derive('A', Nil)) should be (List(Some(EpsExp())))
    leaves(r1.derive('b', Nil)) should be (Nil)
    leaves(r1.derive('B', Nil)) should be (Nil)

    val r2 = RegExpParser("A")
    leaves(r2.derive('a', Nil)) should be (List(Some(EpsExp())))
    leaves(r2.derive('A', Nil)) should be (List(Some(EpsExp())))
    leaves(r2.derive('b', Nil)) should be (Nil)
    leaves(r2.derive('B', Nil)) should be (Nil)
  }

  it should "derive character class" in {
    deriver = new RegExpDeriver[DTree](new PCREOptions("i"))

    val r1 = RegExpParser("[ac-e]")
    leaves(r1.derive('a', Nil)) should be (List(Some(EpsExp())))
    leaves(r1.derive('A', Nil)) should be (List(Some(EpsExp())))
    leaves(r1.derive('d', Nil)) should be (List(Some(EpsExp())))
    leaves(r1.derive('D', Nil)) should be (List(Some(EpsExp())))
    leaves(r1.derive('b', Nil)) should be (Nil)
    leaves(r1.derive('B', Nil)) should be (Nil)

    val r2 = RegExpParser("""[^abB]""")
    leaves(r2.derive('c', Nil)) should be (List(Some(EpsExp())))
    leaves(r2.derive('C', Nil)) should be (List(Some(EpsExp())))
    leaves(r2.derive('b', Nil)) should be (Nil)
    leaves(r2.derive('B', Nil)) should be (Nil)
    leaves(r2.derive('a', Nil)) should be (Nil)
    leaves(r2.derive('A', Nil)) should be (Nil)
  }

  "derive with dot all option" should "derive dot" in {
    deriver = new RegExpDeriver[DTree](new PCREOptions("s"))

    val r = RegExpParser(".")
    leaves(r.derive('a', Nil)) should be (List(Some(EpsExp())))
    leaves(r.derive('\n', Nil)) should be (List(Some(EpsExp())))
  }

  "derive with ungreedy option" should "derive repeat expression" in {
    deriver = new RegExpDeriver[DTree](new PCREOptions("U"))

    val r1 = RegExpParser("a*")
    leaves(r1.derive('a', Nil)) should be (List(None, Some(StarExp(ElemExp('a'), true))))

    val r2 = RegExpParser("a*?")
    leaves(r2.derive('a', Nil)) should be (List(Some(StarExp(ElemExp('a'), false)), None))
  }

  "deriveEOL" should "derive a" in {
    val r = RegExpParser("a")
    leaves(r.deriveEOL(Nil)) should be (empty)
  }

  it should "derive ∅" in {
    val r = RegExpParser("∅")
    leaves(r.deriveEOL(Nil)) should be (empty)
  }

  it should "derive ε" in {
    val r = RegExpParser("ε")
    leaves(r.deriveEOL(Nil)) should not be (empty)
  }

  it should "derive ab" in {
    val r = RegExpParser("ab")
    leaves(r.deriveEOL(Nil)) should be (empty)
  }

  it should "derive a|b" in {
    val r = RegExpParser("a|b")
    leaves(r.deriveEOL(Nil)) should be (empty)
  }

  it should "derive a*" in {
    val r = RegExpParser("a*")
    leaves(r.deriveEOL(Nil)) should not be (empty)
  }

  it should "derive +" in {
    val r1 = RegExpParser("a+")
    leaves(r1.deriveEOL(Nil)) should be (empty)

    val r2 = RegExpParser("(a*)+")
    leaves(r2.deriveEOL(Nil)) should not be (empty)
  }

  it should "derive a?" in {
    val r = RegExpParser("a?")
    leaves(r.deriveEOL(Nil)) should not be (empty)
  }

  it should "derive ." in {
    val r = RegExpParser(".")
    leaves(r.deriveEOL(Nil)) should be (empty)
  }

  it should "derive character class" in {
    val r1 = RegExpParser("""[a-z]""")
    leaves(r1.deriveEOL(Nil)) should be (empty)

    val r2 = RegExpParser("""[^A-Z]""")
    leaves(r2.deriveEOL(Nil)) should be (empty)
  }

  it should "derive meta character" in {
    val r = RegExpParser("""\d""")
    leaves(r.deriveEOL(Nil)) should be (empty)
  }

  it should "derive repeat expression" in {
    val r1 = RegExpParser("(ab){3,5}")
    leaves(r1.deriveEOL(Nil)) should be (empty)

    val r2 = RegExpParser("(ab){3,}")
    leaves(r2.deriveEOL(Nil)) should be (empty)

    val r3 = RegExpParser("(ab){,5}")
    leaves(r3.deriveEOL(Nil)) should not be (empty)

    val r4 = RegExpParser("(a*){3,5}")
    leaves(r4.deriveEOL(Nil)) should not be (empty)
  }

  it should "derive ^" in {
    val r = RegExpParser("^")
    leaves(r.deriveEOL(Nil)) should not be (empty)
    leaves(r.deriveEOL(List(None))) should be (empty)
  }

  it should "derive $" in {
    val r = RegExpParser("$")
    leaves(r.deriveEOL(Nil)) should not be (empty)
  }

  it should "derive lookahead" in {
    val r1 = RegExpParser("(?=ab)")
    r1.deriveEOL(Nil) should be (DAssert[Unit, Unit](DFail(), DLeaf(())))

    val r2 = RegExpParser("(?!ab)")
    r2.deriveEOL(Nil) should be (DAssertNot[Unit, Unit](DFail(), DLeaf(())))
  }

  it should "derive complex expression" in {
    val r1 = RegExpParser("(a|b*)c+d*")
    leaves(r1.deriveEOL(Nil)) should be (empty)

    val r2 = RegExpParser("(a|b*)c?")
    leaves(r2.deriveEOL(Nil)) should not be (empty)
  }
}
