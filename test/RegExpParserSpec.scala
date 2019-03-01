package matching.regexp

import org.scalatest._

class RegExpParserSpec extends FlatSpec with Matchers {
  def withStartEnd[A](r: RegExp[A]): RegExp[A] = {
    RegExp.optConcatExp(
      RegExp.optConcatExp(
        StarExp(DotExp(),false),
        r
      ),
      StarExp(DotExp(),true)
    )
  }

  "RegExpParser" should "parse a" in {
    RegExpParser("a") should be (withStartEnd(ElemExp('a')))
  }

  it should "parse ∅" in {
    RegExpParser("∅") should be (withStartEnd(EmptyExp()))
  }

  it should "parse ε" in {
    RegExpParser("ε") should be (withStartEnd(EpsExp()))
  }

  it should "parse abc" in {
    RegExpParser("abc") should be (withStartEnd(ConcatExp(ConcatExp(ElemExp('a'), ElemExp('b')), ElemExp('c'))))
  }

  it should "parse a|b|c" in {
    RegExpParser("a|b|c") should be (withStartEnd(AltExp(AltExp(ElemExp('a'), ElemExp('b')), ElemExp('c'))))
  }

  it should "parse (a)" in {
    RegExpParser("(a)") should be (withStartEnd(ElemExp('a')))
  }

  it should "parse a*" in {
    RegExpParser("a*") should be (withStartEnd(StarExp(ElemExp('a'), true)))
  }

  it should "parse a+" in {
    RegExpParser("a+") should be (withStartEnd(PlusExp(ElemExp('a'), true)))
  }

  it should "parse a?" in {
    RegExpParser("a?") should be (withStartEnd(OptionExp(ElemExp('a'), true)))
  }

  it should "parse ." in {
    RegExpParser(".") should be (withStartEnd(DotExp()))
  }

  it should "parse | with empty string" in {
    RegExpParser("|a|b") should be (withStartEnd(AltExp(AltExp(EpsExp(), ElemExp('a')), ElemExp('b'))))
    RegExpParser("a||b") should be (withStartEnd(AltExp(AltExp(ElemExp('a'), EpsExp()), ElemExp('b'))))
    RegExpParser("a|b|") should be (withStartEnd(AltExp(AltExp(ElemExp('a'), ElemExp('b')), EpsExp())))
  }

  it should "parse repeat expression" in {
    RegExpParser("a{3,5}") should be (withStartEnd(RepeatExp(ElemExp('a'),Some(3),Some(5),true)))
    RegExpParser("a{3}") should be (withStartEnd(RepeatExp(ElemExp('a'),Some(3),Some(3),true)))
    RegExpParser("a{3,}") should be (withStartEnd(RepeatExp(ElemExp('a'),Some(3),None,true)))
    RegExpParser("a{,5}") should be (withStartEnd(RepeatExp(ElemExp('a'),None,Some(5),true)))
    RegExpParser("a{0,2}") should be (withStartEnd(RepeatExp(ElemExp('a'),None,Some(2),true)))
    RegExpParser("a{0}") should be (withStartEnd(EpsExp()))
  }

  it should "parse character class" in {
    RegExpParser("[abc]") should be (withStartEnd(CharClassExp(Seq(
      SingleCharExp('a'),
      SingleCharExp('b'),
      SingleCharExp('c')
    ), true)))
    RegExpParser("[a-zA-Z]") should be (withStartEnd(CharClassExp(Seq(
      RangeExp('a','z'),
      RangeExp('A','Z')
    ), true)))
    RegExpParser("""[\w\d\b]""") should be (withStartEnd(CharClassExp(Seq(
      MetaCharExp('w'),
      MetaCharExp('d'),
      MetaCharExp('b')
    ), true)))
    RegExpParser("""[ab-def-h\n]""") should be (withStartEnd(CharClassExp(Seq(
      SingleCharExp('a'),
      RangeExp('b','d'),
      SingleCharExp('e'),
      RangeExp('f','h'),
      MetaCharExp('n')
    ), true)))
    RegExpParser("""[a^[-]""") should be (withStartEnd(CharClassExp(Seq(
      SingleCharExp('a'),
      SingleCharExp('^'),
      SingleCharExp('['),
      SingleCharExp('-')
    ), true)))

    RegExpParser("[^abc]") should be (
      withStartEnd(CharClassExp(Seq(
        SingleCharExp('a'),
        SingleCharExp('b'),
        SingleCharExp('c')
      ), false))
    )
    RegExpParser("[^a-zA-Z]") should be (withStartEnd(CharClassExp(Seq(
      RangeExp('a','z'),
      RangeExp('A','Z')
    ), false)))
  }

  it should "parse meta character" in {
    RegExpParser("""\a""") should be (withStartEnd(MetaCharExp('a')))
    RegExpParser("""\d""") should be (withStartEnd(MetaCharExp('d')))
    RegExpParser("""\D""") should be (withStartEnd(MetaCharExp('D')))
    RegExpParser("""\e""") should be (withStartEnd(MetaCharExp('e')))
    RegExpParser("""\f""") should be (withStartEnd(MetaCharExp('f')))
    RegExpParser("""\h""") should be (withStartEnd(MetaCharExp('h')))
    RegExpParser("""\H""") should be (withStartEnd(MetaCharExp('H')))
    RegExpParser("""\n""") should be (withStartEnd(MetaCharExp('n')))
    RegExpParser("""\r""") should be (withStartEnd(MetaCharExp('r')))
    RegExpParser("""\R""") should be (withStartEnd(MetaCharExp('R')))
    RegExpParser("""\s""") should be (withStartEnd(MetaCharExp('s')))
    RegExpParser("""\S""") should be (withStartEnd(MetaCharExp('S')))
    RegExpParser("""\t""") should be (withStartEnd(MetaCharExp('t')))
    RegExpParser("""\v""") should be (withStartEnd(MetaCharExp('v')))
    RegExpParser("""\V""") should be (withStartEnd(MetaCharExp('V')))
    RegExpParser("""\w""") should be (withStartEnd(MetaCharExp('w')))
    RegExpParser("""\W""") should be (withStartEnd(MetaCharExp('W')))
  }

  it should "parse lazy operations" in {
    RegExpParser("a*?") should be (withStartEnd(StarExp(ElemExp('a'), false)))
    RegExpParser("a+?") should be (withStartEnd(PlusExp(ElemExp('a'), false)))
    RegExpParser("a??") should be (withStartEnd(OptionExp(ElemExp('a'), false)))
    RegExpParser("a{3,5}?") should be (withStartEnd(RepeatExp(ElemExp('a'),Some(3),Some(5),false)))
  }

  it should "parse start/end anchor" in {
    RegExpParser("^a") should be (ConcatExp(ElemExp('a'), StarExp(DotExp(),true)))
    RegExpParser("a$") should be (ConcatExp(StarExp(DotExp(),false),ElemExp('a')))
    RegExpParser("^a$") should be (ElemExp('a'))
  }

  it should "parse complex expression" in {
    RegExpParser("ab|c") should be (
      withStartEnd(AltExp(
        ConcatExp(
          ElemExp('a'),
          ElemExp('b')
        ),
        ElemExp('c')
      ))
    )

    RegExpParser("ab*c") should be (
      withStartEnd(ConcatExp(
        ConcatExp(
          ElemExp('a'),
          StarExp(
            ElemExp('b'),
            true)
        ),
        ElemExp('c')
      ))
    )

    RegExpParser("(a(bc))*") should be (
      withStartEnd(StarExp(
        ConcatExp(
          ElemExp('a'),
          ConcatExp(
            ElemExp('b'),
            ElemExp('c')
          )
        ),
      true))
    )

    RegExpParser("((a*)?){3,5}?") should be (
      withStartEnd(RepeatExp(OptionExp(StarExp(ElemExp('a'), true), true), Some(3), Some(5), false))
    )
  }

  it should "parse escape characters" in {
    RegExpParser("""\.""") should be (withStartEnd(ElemExp('.')))
    RegExpParser("""\*""") should be (withStartEnd(ElemExp('*')))
    RegExpParser("""\!""") should be (withStartEnd(ElemExp('!')))
    RegExpParser("""\\""") should be (withStartEnd(ElemExp('\\')))
  }

  it should "parse escape characters in character class" in {
    RegExpParser("""[\^]""") should be (withStartEnd(CharClassExp(Seq(SingleCharExp('^')), true)))
    RegExpParser("""[\]]""") should be (withStartEnd(CharClassExp(Seq(SingleCharExp(']')), true)))
    RegExpParser("""[\-]""") should be (withStartEnd(CharClassExp(Seq(SingleCharExp('-')), true)))
    RegExpParser("""[\.]""") should be (withStartEnd(CharClassExp(Seq(SingleCharExp('.')), true)))
    RegExpParser("""[\!]""") should be (withStartEnd(CharClassExp(Seq(SingleCharExp('!')), true)))
    RegExpParser("""[\\]""") should be (withStartEnd(CharClassExp(Seq(SingleCharExp('\\')), true)))
  }

  it should "ignore spaces" in {
    RegExpParser("a \t \n b") should be (withStartEnd(ConcatExp(ElemExp('a'), ElemExp('b'))))
  }

  it should "throw exception when given illegal expressions" in {
    a [Exception] should be thrownBy {RegExpParser("a^b")}
    a [Exception] should be thrownBy {RegExpParser("a$b")}
    a [Exception] should be thrownBy {RegExpParser("*ab")}
    a [Exception] should be thrownBy {RegExpParser("a*+")}
    a [Exception] should be thrownBy {RegExpParser("(a(b)")}
    a [Exception] should be thrownBy {RegExpParser("a(b))")}
    a [Exception] should be thrownBy {RegExpParser("a[]b")}
    a [Exception] should be thrownBy {RegExpParser("a[bc")}
    a [Exception] should be thrownBy {RegExpParser("abc]")}
    a [Exception] should be thrownBy {RegExpParser("a{5,3}")}
    a [Exception] should be thrownBy {RegExpParser("a{x,5}")}
    a [Exception] should be thrownBy {RegExpParser("a{3,y}")}
    a [Exception] should be thrownBy {RegExpParser("a{,3,5}")}
    a [Exception] should be thrownBy {RegExpParser("a{3,,5}")}
    a [Exception] should be thrownBy {RegExpParser("a{3,5,}")}
    a [Exception] should be thrownBy {RegExpParser("a{3,5")}
    a [Exception] should be thrownBy {RegExpParser("a3,5}")}
    a [Exception] should be thrownBy {RegExpParser("a{,}")}
    a [Exception] should be thrownBy {RegExpParser("a{}")}
  }
}
