package matching.regexp

import org.scalatest._

class RegExpParserSpec extends FlatSpec with Matchers {
  "RegExpParser" should "parse a" in {
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

  it should "parse (a)" in {
    RegExpParser("(a)") should be (ElemExp('a'))
  }

  it should "parse a*" in {
    RegExpParser("a*") should be (StarExp(ElemExp('a'), true))
  }

  it should "parse a+" in {
    RegExpParser("a+") should be (PlusExp(ElemExp('a'), true))
  }

  it should "parse a?" in {
    RegExpParser("a?") should be (OptionExp(ElemExp('a'), true))
  }

  it should "parse ." in {
    RegExpParser(".") should be (DotExp())
  }

  it should "parse character class" in {
    RegExpParser("[abc]") should be (CharClassExp(Seq(SingleCharExp('a'),SingleCharExp('b'),SingleCharExp('c')), true))
    RegExpParser("[a-zA-Z]") should be (CharClassExp(Seq(RangeExp('a','z'),RangeExp('A','Z')), true))
    RegExpParser("[ab-def-h]") should be (CharClassExp(Seq(
      SingleCharExp('a'),
      RangeExp('b','d'),
      SingleCharExp('e'),
      RangeExp('f','h')
    ), true))

    RegExpParser("[^abc]") should be (
      CharClassExp(Seq(SingleCharExp('a'),SingleCharExp('b'),SingleCharExp('c')), false)
    )
    RegExpParser("[^a-zA-Z]") should be (CharClassExp(Seq(RangeExp('a','z'),RangeExp('A','Z')), false))
  }

  it should "parse repeat expression" in {
    RegExpParser("a{3,5}") should be (RepeatExp(ElemExp('a'),Some(3),Some(5),true))
    RegExpParser("a{3}") should be (RepeatExp(ElemExp('a'),Some(3),Some(3),true))
    RegExpParser("a{3,}") should be (RepeatExp(ElemExp('a'),Some(3),None,true))
    RegExpParser("a{,5}") should be (RepeatExp(ElemExp('a'),None,Some(5),true))
  }

  it should "parse lazy operations" in {
    RegExpParser("a*?") should be (StarExp(ElemExp('a'), false))
    RegExpParser("a+?") should be (PlusExp(ElemExp('a'), false))
    RegExpParser("a??") should be (OptionExp(ElemExp('a'), false))
    RegExpParser("a{3,5}?") should be (RepeatExp(ElemExp('a'),Some(3),Some(5),false))
  }

  it should "parse complex expression" in {
    RegExpParser("ab|c") should be (
      AltExp(
        ConcatExp(
          ElemExp('a'),
          ElemExp('b')
        ),
        ElemExp('c')
      )
    )

    RegExpParser("ab*c") should be (
      ConcatExp(
        ConcatExp(
          ElemExp('a'),
          StarExp(
            ElemExp('b'),
            true)
        ),
        ElemExp('c')
      )
    )

    RegExpParser("(a(bc))*") should be (
      StarExp(
        ConcatExp(
          ElemExp('a'),
          ConcatExp(
            ElemExp('b'),
            ElemExp('c')
          )
        ),
      true)
    )

    RegExpParser("((a*)?){3,5}?") should be (RepeatExp(OptionExp(StarExp(ElemExp('a'), true), true), Some(3), Some(5), false))
  }

  it should "parse escape characters" in {
    RegExpParser("""\s""") should be (ElemExp(' '))
    RegExpParser("""\t""") should be (ElemExp('\t'))
    RegExpParser("""\n""") should be (ElemExp('\n'))
    RegExpParser("""\ε""") should be (ElemExp('ε'))
    RegExpParser("""\∅""") should be (ElemExp('∅'))
    RegExpParser("""\.""") should be (ElemExp('.'))
    RegExpParser("""\,""") should be (ElemExp(','))
    RegExpParser("""\|""") should be (ElemExp('|'))
    RegExpParser("""\*""") should be (ElemExp('*'))
    RegExpParser("""\+""") should be (ElemExp('+'))
    RegExpParser("""\?""") should be (ElemExp('?'))
    RegExpParser("""\(""") should be (ElemExp('('))
    RegExpParser("""\)""") should be (ElemExp(')'))
    RegExpParser("""\{""") should be (ElemExp('{'))
    RegExpParser("""\}""") should be (ElemExp('}'))
    RegExpParser("""\[""") should be (ElemExp('['))
    RegExpParser("""\]""") should be (ElemExp(']'))
    RegExpParser("""\\""") should be (ElemExp('\\'))
    RegExpParser("""a\s\*b""") should be (
      ConcatExp(ConcatExp(ConcatExp(ElemExp('a'), ElemExp(' ')), ElemExp('*')), ElemExp('b'))
    )
  }

  it should "parse escape characters in character class" in {
    RegExpParser("""[\s]""") should be (CharClassExp(Seq(SingleCharExp(' ')), true))
    RegExpParser("""[\t]""") should be (CharClassExp(Seq(SingleCharExp('\t')), true))
    RegExpParser("""[\n]""") should be (CharClassExp(Seq(SingleCharExp('\n')), true))
    RegExpParser("""[\[]""") should be (CharClassExp(Seq(SingleCharExp('[')), true))
    RegExpParser("""[\]]""") should be (CharClassExp(Seq(SingleCharExp(']')), true))
    RegExpParser("""[\-]""") should be (CharClassExp(Seq(SingleCharExp('-')), true))
    RegExpParser("""[\^]""") should be (CharClassExp(Seq(SingleCharExp('^')), true))
    RegExpParser("""[a\s^-\[]""") should be (CharClassExp(Seq(
      SingleCharExp('a'),
      SingleCharExp(' '),
      RangeExp('^','[')
    ), true))
  }

  it should "ignore spaces" in {
    RegExpParser("a \t \n b") should be (ConcatExp(ElemExp('a'), ElemExp('b')))
  }

  it should "throw exception when given illegal expressions" in {
    a [Exception] should be thrownBy {RegExpParser("ab|")}
    a [Exception] should be thrownBy {RegExpParser("|ab")}
    a [Exception] should be thrownBy {RegExpParser("*ab")}
    a [Exception] should be thrownBy {RegExpParser("a*+")}
    a [Exception] should be thrownBy {RegExpParser("a()b")}
    a [Exception] should be thrownBy {RegExpParser("(a(b)")}
    a [Exception] should be thrownBy {RegExpParser("a(b))")}
    a [Exception] should be thrownBy {RegExpParser("a[]b")}
    a [Exception] should be thrownBy {RegExpParser("a[bc")}
    a [Exception] should be thrownBy {RegExpParser("abc]")}
    a [Exception] should be thrownBy {RegExpParser("[ab-]")}
    a [Exception] should be thrownBy {RegExpParser("[-ab]")}
    a [Exception] should be thrownBy {RegExpParser("[a--b]")}
    a [Exception] should be thrownBy {RegExpParser("[a-b-c]")}
    a [Exception] should be thrownBy {RegExpParser("a{5,3}")}
    a [Exception] should be thrownBy {RegExpParser("a{0,3}")}
    a [Exception] should be thrownBy {RegExpParser("a{x,5}")}
    a [Exception] should be thrownBy {RegExpParser("a{3,y}")}
    a [Exception] should be thrownBy {RegExpParser("a{,3,5}")}
    a [Exception] should be thrownBy {RegExpParser("a{3,,5}")}
    a [Exception] should be thrownBy {RegExpParser("a{3,5,}")}
    a [Exception] should be thrownBy {RegExpParser("a{3,5")}
    a [Exception] should be thrownBy {RegExpParser("a3,5}")}
    a [Exception] should be thrownBy {RegExpParser("a{,}")}
  }
}
