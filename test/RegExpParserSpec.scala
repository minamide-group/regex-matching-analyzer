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

  it should "parse a*" in {
    RegExpParser("a*") should be (withStartEnd(StarExp(ElemExp('a'), true)))

    a [Exception] should be thrownBy {RegExpParser("*ab")}
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

  it should "parse group" in {
    RegExpParser("(a)") should be (withStartEnd(ElemExp('a')))
    RegExpParser("(?:a)") should be (withStartEnd(ElemExp('a')))

    a [Exception] should be thrownBy {RegExpParser("(a(b)")}
    a [Exception] should be thrownBy {RegExpParser("a(b))")}
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

  it should "parse meta character" in {
    RegExpParser("""\d""") should be (withStartEnd(MetaCharExp('d')))
    RegExpParser("""\D""") should be (withStartEnd(MetaCharExp('D')))
    RegExpParser("""\h""") should be (withStartEnd(MetaCharExp('h')))
    RegExpParser("""\H""") should be (withStartEnd(MetaCharExp('H')))
    RegExpParser("""\R""") should be (withStartEnd(MetaCharExp('R')))
    RegExpParser("""\s""") should be (withStartEnd(MetaCharExp('s')))
    RegExpParser("""\S""") should be (withStartEnd(MetaCharExp('S')))
    RegExpParser("""\v""") should be (withStartEnd(MetaCharExp('v')))
    RegExpParser("""\V""") should be (withStartEnd(MetaCharExp('V')))
    RegExpParser("""\w""") should be (withStartEnd(MetaCharExp('w')))
    RegExpParser("""\W""") should be (withStartEnd(MetaCharExp('W')))
  }

  it should "parse octal representation" in {
    RegExpParser("\\001") should be (withStartEnd(ElemExp('\u0001')))
    RegExpParser("\\10") should be (withStartEnd(ElemExp('\u0008')))
    RegExpParser("\\123") should be (withStartEnd(ElemExp('\u0053')))
    RegExpParser("\\567") should be (withStartEnd(ElemExp('\u0177')))
    RegExpParser("\\001") should be (withStartEnd(ElemExp('\u0001')))
    RegExpParser("\\01") should be (withStartEnd(ElemExp('\u0001')))
    RegExpParser("\\0") should be (withStartEnd(ElemExp('\u0000')))
    RegExpParser("\\1111") should be (withStartEnd(ConcatExp(ElemExp('\u0049'), ElemExp('1'))))
    RegExpParser("\\18") should be (withStartEnd(ConcatExp(ElemExp('\u0001'), ElemExp('8'))))
    RegExpParser("\\81") should be (withStartEnd(ConcatExp(ConcatExp(ElemExp('\u0000'), ElemExp('8')), ElemExp('1'))))
  }

  it should "parse hexadecimal representation" in {
    RegExpParser("\\x01") should be (withStartEnd(ElemExp('\u0001')))
    RegExpParser("\\xab") should be (withStartEnd(ElemExp('\u00AB')))
    RegExpParser("\\xEF") should be (withStartEnd(ElemExp('\u00EF')))
    RegExpParser("\\x2") should be (withStartEnd(ElemExp('\u0002')))
    RegExpParser("\\x") should be (withStartEnd(ElemExp('\u0000')))
    RegExpParser("\\x111") should be (withStartEnd(ConcatExp(ElemExp('\u0011'), ElemExp('1'))))
  }

  it should "parse unicode representation" in {
    RegExpParser("\\u0001") should be (withStartEnd(ElemExp('\u0001')))
    RegExpParser("\\u1234") should be (withStartEnd(ElemExp('\u1234')))
    RegExpParser("\\uabcd") should be (withStartEnd(ElemExp('\uABCD')))
    RegExpParser("\\uCDEF") should be (withStartEnd(ElemExp('\uCDEF')))

    a [Exception] should be thrownBy {RegExpParser("\\u")}
    a [Exception] should be thrownBy {RegExpParser("\\u1")}
    a [Exception] should be thrownBy {RegExpParser("\\u12")}
    a [Exception] should be thrownBy {RegExpParser("\\u123")}
  }

  it should "parse back reference" in {
    RegExpParser("(a)\\1") should be (withStartEnd(ConcatExp(ElemExp('a'),BackReferenceExp(1))))
    RegExpParser("(a|\\1b)*") should be (withStartEnd(
      StarExp(
        AltExp(
          ElemExp('a'),
          ConcatExp(
            BackReferenceExp(1),
            ElemExp('b')
          )
        )
      , true)
    ))
    RegExpParser(s"${"(a)" * 20}\\20") should be (withStartEnd(
      ConcatExp(
        (1 until 20).foldLeft(ElemExp('a'): RegExp[Char])((r,_) => ConcatExp(r,ElemExp('a'))),
        BackReferenceExp(20)
      )
    ))
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

    a [Exception] should be thrownBy {RegExpParser("a^b")}
    a [Exception] should be thrownBy {RegExpParser("a$b")}
  }

  it should "parse escape characters" in {
    RegExpParser("""\.""") should be (withStartEnd(ElemExp('.')))
    RegExpParser("""\*""") should be (withStartEnd(ElemExp('*')))
    RegExpParser("""\!""") should be (withStartEnd(ElemExp('!')))
    RegExpParser("""\\""") should be (withStartEnd(ElemExp('\\')))
  }

  it should "parse special escape characters" in {
    RegExpParser("""\a""") should be (withStartEnd(ElemExp('\u0007')))
    RegExpParser("""\e""") should be (withStartEnd(ElemExp('\u001B')))
    RegExpParser("""\f""") should be (withStartEnd(ElemExp('\f')))
    RegExpParser("""\n""") should be (withStartEnd(ElemExp('\n')))
    RegExpParser("""\r""") should be (withStartEnd(ElemExp('\r')))
    RegExpParser("""\t""") should be (withStartEnd(ElemExp('\t')))
  }

  it should "parse backslash assertions" in {
    RegExpParser("""\A""") should be (withStartEnd(UnsupportedExp("""\A""")))
    RegExpParser("""\b""") should be (withStartEnd(UnsupportedExp("""\b""")))
    RegExpParser("""\B""") should be (withStartEnd(UnsupportedExp("""\B""")))
    RegExpParser("""\z""") should be (withStartEnd(UnsupportedExp("""\z""")))
    RegExpParser("""\Z""") should be (withStartEnd(UnsupportedExp("""\Z""")))
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

    RegExpParser("""[ab-def-h]""") should be (withStartEnd(CharClassExp(Seq(
      SingleCharExp('a'),
      RangeExp('b','d'),
      SingleCharExp('e'),
      RangeExp('f','h')
    ), true)))
    RegExpParser("""[a^[-]""") should be (withStartEnd(CharClassExp(Seq(
      SingleCharExp('a'),
      SingleCharExp('^'),
      SingleCharExp('['),
      SingleCharExp('-')
    ), true)))
    RegExpParser("""[a-\d]""") should be (withStartEnd(CharClassExp(Seq(
      SingleCharExp('a'),
      SingleCharExp('-'),
      MetaCharExp('d')
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

    a [Exception] should be thrownBy {RegExpParser("a[]b")}
    a [Exception] should be thrownBy {RegExpParser("a[bc")}
    a [Exception] should be thrownBy {RegExpParser("abc]")}
  }

  it should "parse escape characters in character class" in {
    RegExpParser("""[\^]""") should be (withStartEnd(CharClassExp(Seq(SingleCharExp('^')), true)))
    RegExpParser("""[\]]""") should be (withStartEnd(CharClassExp(Seq(SingleCharExp(']')), true)))
    RegExpParser("""[\-]""") should be (withStartEnd(CharClassExp(Seq(SingleCharExp('-')), true)))
    RegExpParser("""[\.]""") should be (withStartEnd(CharClassExp(Seq(SingleCharExp('.')), true)))
    RegExpParser("""[\!]""") should be (withStartEnd(CharClassExp(Seq(SingleCharExp('!')), true)))
    RegExpParser("""[\\]""") should be (withStartEnd(CharClassExp(Seq(SingleCharExp('\\')), true)))
  }

  it should "parse meta characters in character class" in {
    RegExpParser("""[\s]""") should be (withStartEnd(CharClassExp(Seq(
      MetaCharExp('s')
    ), true)))
    RegExpParser("""[\W]""") should be (withStartEnd(CharClassExp(Seq(
      MetaCharExp('W')
    ), true)))
  }

  it should "parse special meta characters in character class" in {
    RegExpParser("""[\b]""") should be (withStartEnd(CharClassExp(Seq(
      SingleCharExp('\b')
    ), true)))
    RegExpParser("""[\n]""") should be (withStartEnd(CharClassExp(Seq(
      SingleCharExp('\n')
    ), true)))
  }

  it should "parse octal representation in character class" in {
    RegExpParser("[\\001]") should be (withStartEnd(CharClassExp(Seq(
      SingleCharExp('\u0001')
    ), true)))
    RegExpParser("[\\1111]") should be (withStartEnd(CharClassExp(Seq(
      SingleCharExp('\u0049'),
      SingleCharExp('1')
    ), true)))
    RegExpParser("[\\81]") should be (withStartEnd(CharClassExp(Seq(
      SingleCharExp('\u0000'),
      SingleCharExp('8'),
      SingleCharExp('1')
    ), true)))
  }

  it should "parse hexadecimal representation in character class" in {
    RegExpParser("[\\x01]") should be (withStartEnd(CharClassExp(Seq(
      SingleCharExp('\u0001')
    ), true)))
    RegExpParser("[\\x111]") should be (withStartEnd(CharClassExp(Seq(
      SingleCharExp('\u0011'),
      SingleCharExp('1')
    ), true)))
  }

  it should "parse unicode representation in character class" in {
    RegExpParser("[\\u0001]") should be (withStartEnd(CharClassExp(Seq(
      SingleCharExp('\u0001')
    ), true)))
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

    RegExpParser("""[a-z\wA]""") should be (withStartEnd(CharClassExp(Seq(
      RangeExp('a','z'),
      MetaCharExp('w'),
      SingleCharExp('A')
    ), true)))

    RegExpParser("""[\x10-\u00EF]""") should be (withStartEnd(CharClassExp(Seq(
      RangeExp('\u0010','\u00EF')
    ), true)))

    a [Exception] should be thrownBy {RegExpParser("a*+")}
  }
}
