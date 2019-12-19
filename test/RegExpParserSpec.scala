package matching.regexp

import RegExpParser._
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

  it should "parse a*" in {
    RegExpParser("a*") should be (StarExp(ElemExp('a'), true))

    a [ParseException] should be thrownBy {RegExpParser("*ab")}
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

  it should "parse group" in {
    RegExpParser("(a)") should be (GroupExp(ElemExp('a'), 1, None))
    RegExpParser("(?:a)") should be (ElemExp('a'))
    RegExpParser("(?<hoge>a)") should be (GroupExp(ElemExp('a'), 1, Some("hoge")))
    RegExpParser("(?'hoge'a)") should be (GroupExp(ElemExp('a'), 1, Some("hoge")))
    RegExpParser("(?P<hoge>a)") should be (GroupExp(ElemExp('a'), 1, Some("hoge")))

    a [ParseException] should be thrownBy {RegExpParser("(a(b)")}
    a [ParseException] should be thrownBy {RegExpParser("a(b))")}
  }

  it should "parse | with empty string" in {
    RegExpParser("|a|b") should be (AltExp(AltExp(EpsExp(), ElemExp('a')), ElemExp('b')))
    RegExpParser("a||b") should be (AltExp(AltExp(ElemExp('a'), EpsExp()), ElemExp('b')))
    RegExpParser("a|b|") should be (AltExp(AltExp(ElemExp('a'), ElemExp('b')), EpsExp()))
  }

  it should "parse repeat expression" in {
    RegExpParser("a{3,5}") should be (RepeatExp(ElemExp('a'),Some(3),Some(5),true))
    RegExpParser("a{3}") should be (RepeatExp(ElemExp('a'),Some(3),Some(3),true))
    RegExpParser("a{3,}") should be (RepeatExp(ElemExp('a'),Some(3),None,true))
    RegExpParser("a{,5}") should be (RepeatExp(ElemExp('a'),None,Some(5),true))
    RegExpParser("a{0,2}") should be (RepeatExp(ElemExp('a'),None,Some(2),true))
    RegExpParser("a{0}") should be (EpsExp())

    a [ParseException] should be thrownBy {RegExpParser("a{5,3}")}
  }

  it should "parse meta character" in {
    RegExpParser("""\d""") should be (MetaCharExp('d'))
    RegExpParser("""\D""") should be (MetaCharExp('D'))
    RegExpParser("""\h""") should be (MetaCharExp('h'))
    RegExpParser("""\H""") should be (MetaCharExp('H'))
    RegExpParser("""\R""") should be (MetaCharExp('R'))
    RegExpParser("""\s""") should be (MetaCharExp('s'))
    RegExpParser("""\S""") should be (MetaCharExp('S'))
    RegExpParser("""\v""") should be (MetaCharExp('v'))
    RegExpParser("""\V""") should be (MetaCharExp('V'))
    RegExpParser("""\w""") should be (MetaCharExp('w'))
    RegExpParser("""\W""") should be (MetaCharExp('W'))

    a [ParseException] should be thrownBy {RegExpParser("""\c""")}
  }

  it should "parse octal representation" in {
    RegExpParser("\\001") should be (ElemExp('\u0001'))
    RegExpParser("\\10") should be (ElemExp('\u0008'))
    RegExpParser("\\123") should be (ElemExp('\u0053'))
    RegExpParser("\\567") should be (ElemExp('\u0177'))
    RegExpParser("\\001") should be (ElemExp('\u0001'))
    RegExpParser("\\01") should be (ElemExp('\u0001'))
    RegExpParser("\\0") should be (ElemExp('\u0000'))
    RegExpParser("\\1111") should be (ConcatExp(ElemExp('\u0049'), ElemExp('1')))
    RegExpParser("\\18") should be (ConcatExp(ElemExp('\u0001'), ElemExp('8')))
    RegExpParser("\\81") should be (ConcatExp(ConcatExp(ElemExp('\u0000'), ElemExp('8')), ElemExp('1')))
  }

  it should "parse hexadecimal representation" in {
    RegExpParser("\\x01") should be (ElemExp('\u0001'))
    RegExpParser("\\xab") should be (ElemExp('\u00AB'))
    RegExpParser("\\xEF") should be (ElemExp('\u00EF'))
    RegExpParser("\\x2") should be (ElemExp('\u0002'))
    RegExpParser("\\x") should be (ElemExp('\u0000'))
    RegExpParser("\\x111") should be (ConcatExp(ElemExp('\u0011'), ElemExp('1')))
  }

  it should "parse unicode representation" in {
    RegExpParser("\\u0001") should be (ElemExp('\u0001'))
    RegExpParser("\\u1234") should be (ElemExp('\u1234'))
    RegExpParser("\\uabcd") should be (ElemExp('\uABCD'))
    RegExpParser("\\uCDEF") should be (ElemExp('\uCDEF'))

    a [ParseException] should be thrownBy {RegExpParser("\\u")}
    a [ParseException] should be thrownBy {RegExpParser("\\u1")}
    a [ParseException] should be thrownBy {RegExpParser("\\u12")}
    a [ParseException] should be thrownBy {RegExpParser("\\u123")}
  }

  it should "parse back reference" in {
    RegExpParser("""(a)\1""") should be (
      ConcatExp(GroupExp(ElemExp('a'),1,None),BackReferenceExp(1, None))
    )
    RegExpParser("""(a|\1b)*""") should be (
      StarExp(
        GroupExp(
          AltExp(
            ElemExp('a'),
            ConcatExp(
              BackReferenceExp(1, None),
              ElemExp('b')
            )
          ),
          1,None
        ),
        true
      )
    )
    RegExpParser("""(a)(b)(?<hoge>c)\3""") should be (
      ConcatExp(
        ConcatExp(
          ConcatExp(
            GroupExp(ElemExp('a'),1,None),
            GroupExp(ElemExp('b'),2,None)
          ),
          GroupExp(ElemExp('c'),3,Some("hoge"))
        ),
        BackReferenceExp(3, None)
      )
    )

    val r = ConcatExp(GroupExp(ElemExp('a'),1,Some("hoge")),BackReferenceExp(1, Some("hoge")))
    RegExpParser("(?<hoge>a)(?P=hoge)") should be (r)
    RegExpParser("""(?<hoge>a)\k<hoge>""") should be (r)
    RegExpParser("""(?<hoge>a)\k'hoge'""") should be (r)
    RegExpParser("""(?<hoge>a)\k{hoge}""") should be (r)

    a [ParseException] should be thrownBy {RegExpParser("""(?<123>a)""")}
    a [ParseException] should be thrownBy {RegExpParser("""(a)\k<hoge>""")}
    a [ParseException] should be thrownBy {RegExpParser("""(a)\2""")}
  }

  it should "parse start/end anchor" in {
    RegExpParser("^") should be (StartAnchorExp())
    RegExpParser("$") should be (EndAnchorExp())
  }

  it should "parse lookahead/lookbehind" in {
    RegExpParser("(?=a)") should be (LookaheadExp(ElemExp('a'), true))
    RegExpParser("(?!a)") should be (LookaheadExp(ElemExp('a'), false))
    RegExpParser("(?<=a)") should be (LookbehindExp(ElemExp('a'), true))
    RegExpParser("(?<!a)") should be (LookbehindExp(ElemExp('a'), false))
  }

  it should "parse if expression" in {
    RegExpParser("(?(a)b)") should be (IfExp(ElemExp('a'), ElemExp('b'), EpsExp()))
    RegExpParser("(?(a)b|c)") should be (IfExp(ElemExp('a'), ElemExp('b'), ElemExp('c')))
  }

  it should "parse lazy operations" in {
    RegExpParser("a*?") should be (StarExp(ElemExp('a'), false))
    RegExpParser("a+?") should be (PlusExp(ElemExp('a'), false))
    RegExpParser("a??") should be (OptionExp(ElemExp('a'), false))
    RegExpParser("a{3,5}?") should be (RepeatExp(ElemExp('a'),Some(3),Some(5),false))
  }

  it should "parse escape characters" in {
    RegExpParser("""\.""") should be (ElemExp('.'))
    RegExpParser("""\*""") should be (ElemExp('*'))
    RegExpParser("""\!""") should be (ElemExp('!'))
    RegExpParser("""\\""") should be (ElemExp('\\'))
  }

  it should "parse special escape characters" in {
    RegExpParser("""\a""") should be (ElemExp('\u0007'))
    RegExpParser("""\e""") should be (ElemExp('\u001B'))
    RegExpParser("""\f""") should be (ElemExp('\f'))
    RegExpParser("""\n""") should be (ElemExp('\n'))
    RegExpParser("""\r""") should be (ElemExp('\r'))
    RegExpParser("""\t""") should be (ElemExp('\t'))
  }

  it should "parse word boundary" in {
    RegExpParser("""\b""") should be (BoundaryExp())
  }

  it should "parse character class" in {
    RegExpParser("[abc]") should be (CharClassExp(Seq(
      SingleCharExp('a'),
      SingleCharExp('b'),
      SingleCharExp('c')
    ), true))
    RegExpParser("[a-zA-Z]") should be (CharClassExp(Seq(
      RangeExp('a','z'),
      RangeExp('A','Z')
    ), true))

    RegExpParser("""[ab-def-h]""") should be (CharClassExp(Seq(
      SingleCharExp('a'),
      RangeExp('b','d'),
      SingleCharExp('e'),
      RangeExp('f','h')
    ), true))
    RegExpParser("""[a^[-]""") should be (CharClassExp(Seq(
      SingleCharExp('a'),
      SingleCharExp('^'),
      SingleCharExp('['),
      SingleCharExp('-')
    ), true))
    RegExpParser("""[a-\d]""") should be (CharClassExp(Seq(
      SingleCharExp('a'),
      SingleCharExp('-'),
      MetaCharExp('d')
    ), true))

    RegExpParser("[^abc]") should be (
      CharClassExp(Seq(
        SingleCharExp('a'),
        SingleCharExp('b'),
        SingleCharExp('c')
      ), false)
    )
    RegExpParser("[^a-zA-Z]") should be (CharClassExp(Seq(
      RangeExp('a','z'),
      RangeExp('A','Z')
    ), false))

    a [ParseException] should be thrownBy {RegExpParser("a[]b")}
    a [ParseException] should be thrownBy {RegExpParser("a[bc")}
    a [ParseException] should be thrownBy {RegExpParser("abc]")}
  }

  it should "parse escape characters in character class" in {
    RegExpParser("""[\^]""") should be (CharClassExp(Seq(SingleCharExp('^')), true))
    RegExpParser("""[\]]""") should be (CharClassExp(Seq(SingleCharExp(']')), true))
    RegExpParser("""[\-]""") should be (CharClassExp(Seq(SingleCharExp('-')), true))
    RegExpParser("""[\.]""") should be (CharClassExp(Seq(SingleCharExp('.')), true))
    RegExpParser("""[\!]""") should be (CharClassExp(Seq(SingleCharExp('!')), true))
    RegExpParser("""[\\]""") should be (CharClassExp(Seq(SingleCharExp('\\')), true))
  }

  it should "parse meta characters in character class" in {
    RegExpParser("""[\s]""") should be (CharClassExp(Seq(
      MetaCharExp('s')
    ), true))
    RegExpParser("""[\W]""") should be (CharClassExp(Seq(
      MetaCharExp('W')
    ), true))
  }

  it should "parse special meta characters in character class" in {
    RegExpParser("""[\b]""") should be (CharClassExp(Seq(
      SingleCharExp('\b')
    ), true))
    RegExpParser("""[\n]""") should be (CharClassExp(Seq(
      SingleCharExp('\n')
    ), true))
  }

  it should "parse octal representation in character class" in {
    RegExpParser("[\\001]") should be (CharClassExp(Seq(
      SingleCharExp('\u0001')
    ), true))
    RegExpParser("[\\1111]") should be (CharClassExp(Seq(
      SingleCharExp('\u0049'),
      SingleCharExp('1')
    ), true))
    RegExpParser("[\\81]") should be (CharClassExp(Seq(
      SingleCharExp('\u0000'),
      SingleCharExp('8'),
      SingleCharExp('1')
    ), true))
  }

  it should "parse hexadecimal representation in character class" in {
    RegExpParser("[\\x01]") should be (CharClassExp(Seq(
      SingleCharExp('\u0001')
    ), true))
    RegExpParser("[\\x111]") should be (CharClassExp(Seq(
      SingleCharExp('\u0011'),
      SingleCharExp('1')
    ), true))
  }

  it should "parse unicode representation in character class" in {
    RegExpParser("[\\u0001]") should be (CharClassExp(Seq(
      SingleCharExp('\u0001')
    ), true))
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

    RegExpParser("(?:a(?:bc))*") should be (
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

    RegExpParser("(?:(?:a*)?){3,5}?") should be (
      RepeatExp(OptionExp(StarExp(ElemExp('a'), true), true), Some(3), Some(5), false)
    )

    RegExpParser("((?<hoge>a)b)(c(?:d))") should be (
      ConcatExp(
        GroupExp(
          ConcatExp(
            GroupExp(ElemExp('a'), 2, Some("hoge")),
            ElemExp('b')
          ),
          1, None
        ),
        GroupExp(ConcatExp(ElemExp('c'),ElemExp('d')), 3, None)
      )
    )

    RegExpParser("""[a-z\wA]""") should be (CharClassExp(Seq(
      RangeExp('a','z'),
      MetaCharExp('w'),
      SingleCharExp('A')
    ), true))

    RegExpParser("""[\x10-\u00EF]""") should be (CharClassExp(Seq(
      RangeExp('\u0010','\u00EF')
    ), true))

    RegExpParser("""(?(a)bc|d|e)""") should be (IfExp(
      ElemExp('a'),
      ConcatExp(ElemExp('b'), ElemExp('c')),
      AltExp(ElemExp('d'), ElemExp('e'))
    ))

    a [ParseException] should be thrownBy {RegExpParser("a*+")}
  }

  "parsePCRE" should "parse PCRE style regexp" in {
    val (r1,o1) = RegExpParser.parsePCRE("/a/")
    r1 should be (ElemExp('a'))
    o1.ignoreCase should be (false)
    o1.dotAll should be (false)
    o1.ungreedy should be (false)

    val (_,o2) = RegExpParser.parsePCRE("/a/i")
    o2.ignoreCase should be (true)

    val (_,o3) = RegExpParser.parsePCRE("/a/s")
    o3.dotAll should be (true)

    val (_,o4) = RegExpParser.parsePCRE("/a/U")
    o4.ungreedy should be (true)

    val (_,o5) = RegExpParser.parsePCRE("/a/isU")
    o5.ignoreCase should be (true)
    o5.dotAll should be (true)
    o5.ungreedy should be (true)

    noException should be thrownBy {RegExpParser.parsePCRE("#a#")}
    noException should be thrownBy {RegExpParser.parsePCRE("(a)")}
    noException should be thrownBy {RegExpParser.parsePCRE("{a}")}
    noException should be thrownBy {RegExpParser.parsePCRE("[a]")}
    noException should be thrownBy {RegExpParser.parsePCRE("<a>")}

    a [ParseException] should be thrownBy {RegExpParser.parsePCRE("/abc#i")}
  }
}
