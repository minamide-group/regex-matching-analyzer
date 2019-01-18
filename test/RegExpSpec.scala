package matching.regexp

import org.scalatest._

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


  "derive" should "derive a" in {
    val r = RegExpParser("a")
    r.derive[List]('a') should be (List(Some(EpsExp())))
    r.derive[List]('b') should be (Nil)
  }

  it should "derive ∅" in {
    val r = RegExpParser("∅")
    r.derive[List]('a') should be (Nil)
  }

  it should "derive ε" in {
    val r = RegExpParser("ε")
    r.derive[List]('a') should be (List(None))
  }

  it should "derive ab" in {
    val r = RegExpParser("ab")
    r.derive[List]('a') should be (List(Some(ElemExp('b'))))
    r.derive[List]('b') should be (Nil)
    r.derive[List]('c') should be (Nil)
  }

  it should "derive a|b" in {
    val r = RegExpParser("a|b")
    r.derive[List]('a') should be (List(Some(EpsExp())))
    r.derive[List]('b') should be (List(Some(EpsExp())))
    r.derive[List]('c') should be (Nil)
  }

  it should "derive a*" in {
    val r = RegExpParser("a*")
    r.derive[List]('a') should be (List(Some(StarExp(ElemExp('a'), true)), None))
    r.derive[List]('b') should be (List(None))
  }

  it should "derive a+" in {
    val r = RegExpParser("a+")
    r.derive[List]('a') should be (List(Some(StarExp(ElemExp('a'), true))))
    r.derive[List]('b') should be (Nil)
  }

  it should "derive a?" in {
    val r = RegExpParser("a?")
    r.derive[List]('a') should be (List(Some(EpsExp()), None))
    r.derive[List]('b') should be (List(None))
  }

  it should "derive ." in {
    val r = RegExpParser(".")
    r.derive[List]('a') should be (List(Some(EpsExp())))
  }

  it should "derive character class" in {
    val r1 = RegExpParser("[a-z]")
    r1.derive[List]('a') should be (List(Some(EpsExp())))
    r1.derive[List]('h') should be (List(Some(EpsExp())))
    r1.derive[List]('z') should be (List(Some(EpsExp())))
    r1.derive[List]('A') should be (Nil)
    r1.derive[List]('H') should be (Nil)
    r1.derive[List]('Z') should be (Nil)

    val r2 = RegExpParser("[^a-z]")
    r2.derive[List]('a') should be (Nil)
    r2.derive[List]('h') should be (Nil)
    r2.derive[List]('z') should be (Nil)
    r2.derive[List]('A') should be (List(Some(EpsExp())))
    r2.derive[List]('H') should be (List(Some(EpsExp())))
    r2.derive[List]('Z') should be (List(Some(EpsExp())))
  }

  it should "derive repeat expression" in {
    val r1 = RegExpParser("(ab){3,5}")
    r1.derive[List]('a') should be (List(Some(RegExpParser("b(ab){2,4}"))))
    r1.derive[List]('b') should be (Nil)

    val r2 = RegExpParser("(ab){3}")
    r2.derive[List]('a') should be (List(Some(RegExpParser("b(ab){2,2}"))))
    r2.derive[List]('b') should be (Nil)

    val r3 = RegExpParser("(ab){3,}")
    r3.derive[List]('a') should be (List(Some(RegExpParser("b(ab){2,}"))))
    r3.derive[List]('b') should be (Nil)

    val r4 = RegExpParser("(ab){,5}")
    r4.derive[List]('a') should be (List(Some(RegExpParser("b(ab){,4}")), None))
    r4.derive[List]('b') should be (List(None))

    val r5 = RegExpParser("(ab){1,5}")
    r5.derive[List]('a') should be (List(Some(RegExpParser("b(ab){,4}"))))
    r5.derive[List]('b') should be (Nil)

    val r6 = RegExpParser("(ab){,1}")
    r6.derive[List]('a') should be (List(Some(RegExpParser("b(ab)*")), None))
    r6.derive[List]('b') should be (List(None))

    val r7 = RegExpParser("(ab){1}")
    r7.derive[List]('a') should be (List(Some(RegExpParser("b(ab)*"))))
    r7.derive[List]('b') should be (Nil)
  }

  it should "derive lazy operations" in {
    val r1 = RegExpParser("a*?")
    r1.derive[List]('a') should be (List(None, Some(StarExp(ElemExp('a'), false))))
    r1.derive[List]('b') should be (List(None))

    val r2 = RegExpParser("a+?")
    r2.derive[List]('a') should be (List(Some(StarExp(ElemExp('a'), false))))
    r2.derive[List]('b') should be (Nil)

    val r3 = RegExpParser("a??")
    r3.derive[List]('a') should be (List(None, Some(EpsExp())))
    r3.derive[List]('b') should be (List(None))

    val r4 = RegExpParser("(ab){3,5}?")
    r4.derive[List]('a') should be (List(Some(RegExpParser("b(ab){2,4}?"))))
    r4.derive[List]('b') should be (Nil)

    val r5 = RegExpParser("(ab){,5}?")
    r5.derive[List]('a') should be (List(None, Some(RegExpParser("b(ab){,4}?"))))
    r5.derive[List]('b') should be (List(None))

    val r6 = RegExpParser("(ab){,1}?")
    r6.derive[List]('a') should be (List(None, Some(RegExpParser("b(ab)*?"))))
    r6.derive[List]('b') should be (List(None))
  }

  it should "derive complex expression" in {
    val r = RegExpParser("a*(bc|d)")
    r.derive[List]('a') should be (List(Some(RegExpParser("a*(bc|d)"))))
    r.derive[List]('b') should be (List(Some(RegExpParser("c"))))
    r.derive[List]('c') should be (Nil)
    r.derive[List]('d') should be (List(Some(RegExpParser("ε"))))
    r.derive[List]('e') should be (Nil)
  }


  "constructNFA" should "construct NFA" in {
    val r0 = RegExpParser("(ab)*ab")
    val r1 = RegExpParser("b(ab)*ab")
    val r2 = RegExpParser("b")
    val r3 = RegExpParser("ε")
    val nfa = RegExp.constructNFA(r0)

    nfa.states should contain only (r0,r1,r2,r3)
    nfa.sigma should contain only ('a','b')
    nfa.delta should contain only (
      (r0,'a',r1),
      (r0,'a',r2),
      (r1,'b',r0),
      (r2,'b',r3)
    )
    nfa.initialStates should contain only (r0)
    nfa.finalStates should contain only (r3)
  }
}
