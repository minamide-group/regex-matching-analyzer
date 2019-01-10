import org.scalatest._

import regexp._

class RegExpSpec extends FlatSpec with Matchers {
  "parser" should "parse a" in {
    RegExpParser("a") should be (ElemExp('a'))
  }

  it should "parse ∅" in {
    RegExpParser("∅") should be (EmptyExp())
  }

  it should "parse ε" in {
    RegExpParser("ε") should be (EpsExp())
  }

  it should "parse abc" in {
    RegExpParser("abc") should be (ConcatExp(ConcatExp(ElemExp('a'), ElemExp('b')), ElemExp('c')))
  }

  it should "parse a|b|c" in {
    RegExpParser("a|b|c") should be (AltExp(AltExp(ElemExp('a'), ElemExp('b')), ElemExp('c')))
  }

  it should "parse a*" in {
    RegExpParser("a*") should be (StarExp(ElemExp('a')))
  }

  it should "parse a(b(c|d)*e)" in {
    RegExpParser("a(b(c|d)*e)") should be (
      ConcatExp(
        ElemExp('a'),
        ConcatExp(
          ConcatExp(
            ElemExp('b'),
            StarExp(
              AltExp(
                ElemExp('c'),
                ElemExp('d')
              )
            )
          ),
          ElemExp('e')
        )
      )
    )
  }

  it should "parse escape characters" in {
    RegExpParser("""\s""") should be (ElemExp(' '))
    RegExpParser("""\t""") should be (ElemExp('\t'))
    RegExpParser("""\n""") should be (ElemExp('\n'))
    RegExpParser("""\ε""") should be (ElemExp('ε'))
    RegExpParser("""\∅""") should be (ElemExp('∅'))
    RegExpParser("""\|""") should be (ElemExp('|'))
    RegExpParser("""\*""") should be (ElemExp('*'))
    RegExpParser("""\(""") should be (ElemExp('('))
    RegExpParser("""\)""") should be (ElemExp(')'))
    RegExpParser("""\\""") should be (ElemExp('\\'))
    RegExpParser("""a\s\*b""") should be
      (ConcatExp(ConcatExp(ConcatExp(ElemExp('a'), ElemExp(' ')), ElemExp('*')), ElemExp('b')))
  }

  it should "ignore spaces" in {
    RegExpParser("a \t \n b") should be (ConcatExp(ElemExp('a'), ElemExp('b')))
  }


  "derive" should "derive a correctly" in {
    val r = RegExpParser("a")
    r.derive[List]('a') should be (List(Some(EpsExp())))
    r.derive[List]('b') should be (Nil)
  }

  it should "derive ∅ correctly" in {
    val r = RegExpParser("∅")
    r.derive[List]('a') should be (List(Some(EmptyExp())))
  }

  it should "derive ε correctly" in {
    val r = RegExpParser("ε")
    r.derive[List]('a') should be (List(None))
  }

  it should "derive ab correctly" in {
    val r = RegExpParser("ab")
    r.derive[List]('a') should be (List(Some(ConcatExp(EpsExp(), ElemExp('b')))))
    r.derive[List]('b') should be (Nil)
    r.derive[List]('c') should be (Nil)
  }

  it should "derive a|b correctly" in {
    val r = RegExpParser("a|b")
    r.derive[List]('a') should be (List(Some(EpsExp())))
    r.derive[List]('b') should be (List(Some(EpsExp())))
    r.derive[List]('c') should be (Nil)
  }

  it should "derive a* correctly" in {
    val r = RegExpParser("a*")
    r.derive[List]('a') should be (List(Some(ConcatExp(EpsExp(), StarExp(ElemExp('a')))), None))
    r.derive[List]('b') should be (List(None))
  }

  it should "derive a*(bc|d) correctly" in {
    val r = RegExpParser("a*(bc|d)")
    r.derive[List]('a') should be (List(Some(RegExpParser("εa*(bc|d)"))))
    r.derive[List]('b') should be (List(Some(RegExpParser("εc"))))
    r.derive[List]('c') should be (Nil)
    r.derive[List]('d') should be (List(Some(RegExpParser("ε"))))
    r.derive[List]('e') should be (Nil)
  }
}
