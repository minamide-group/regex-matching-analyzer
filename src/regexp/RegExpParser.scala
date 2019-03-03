package matching.regexp

import scala.util.parsing.combinator._
import RegExp.optConcatExp

class RegExpParser() extends RegexParsers {
  override val skipWhitespace = false
  var captureGroups: Int = _

  def parseAll(s: String): RegExp[Char] = {
    parseAll(groupCount, s) match {
      case Success(i,_) => captureGroups = i.min(99)
      case NoSuccess(msg,next) =>
        throw RegExpParser.ParseException(s"${msg} at ${next.pos.line}:${next.pos.column} while checking groups count")
    }

    parseAll(expAnchor, s) match {
      case Success(r,_) => r
      case NoSuccess(msg,next) =>
        throw RegExpParser.ParseException(s"${msg} at ${next.pos.line}:${next.pos.column}")
    }
  }

  val specialMetaChars = "aefnrt"
  val chars = """[^\s∅ε.|*+?^$(){}\[\]\\]""".r
  val metas = s"[dDhHRsSvVwW]".r
  val backslashAssertions = s"[AbBzZ]".r
  val spacialMetas = s"[${specialMetaChars}]".r
  val charsInCharClass = """[^\s\]\\]""".r
  val spacialMetasInCharClass = s"[${specialMetaChars}b]".r

  private def expAnchor: Parser[RegExp[Char]] = {
    def convertSpecialMeta(c: Char): Char = {
      c match {
        case 'a' => '\u0007'
        case 'b' => '\b'
        case 'e' => '\u001B'
        case 'f' => '\f'
        case 'n' => '\n'
        case 'r' => '\r'
        case 't' => '\t'
      }
    }

    def term: Parser[RegExp[Char]] = {
      def quantifiedFactor: Parser[RegExp[Char]] = {
        def factor: Parser[RegExp[Char]] = {
          def elem: Parser[ElemExp[Char]] = {
            def char: Parser[Char] = chars ^^ {_.head}
            def specialMeta: Parser[Char] = "\\" ~> spacialMetas ^^ {c => convertSpecialMeta(c.head)}

            (char | specialMeta | hex | unicode | esc) ^^ {ElemExp(_)}
          }
          def empty: Parser[EmptyExp[Char]] = "∅" ^^ {_ => EmptyExp()}
          def eps: Parser[EpsExp[Char]] = "ε" ^^ {_ => EpsExp()}
          def dot: Parser[DotExp[Char]] = "." ^^ {_ => DotExp()}
          def group: Parser[RegExp[Char]] = {
            def exp: Parser[RegExp[Char]] = {
              rep1sep(term,"|") ^^ {_.reduceLeft(AltExp(_,_))}
            }

            ("(" <~ opt("?:")) ~ exp <~ ")" ^^ {
              case _ ~ e => e
            }
          }
          def charClass: Parser[CharClassExp] = "[" ~> charClassFactor <~ "]" ^^ {CharClassExp(_,true)}
          def charClassNeg: Parser[CharClassExp] = "[^" ~> charClassFactor <~ "]" ^^ {CharClassExp(_,false)}
          def charClassFactor: Parser[Seq[CharClassElem]] = {
            def charClassElem: Parser[Seq[Char]] = {
              def charClassChar: Parser[Char] = charsInCharClass ^^ {_.head}
              def charClassSpecialMeta: Parser[Char] = "\\" ~> spacialMetasInCharClass ^^ {c => convertSpecialMeta(c.head)}
              def oct: Parser[Seq[Char]] = "\\" ~> """\d+""".r ^^ { d =>
                val (octalPart, elemsPart) = d.take(3).span(_ < '8')
                Integer.parseInt(s"0${octalPart}", 8).toChar + (elemsPart + d.drop(3))
              }

              oct | (charClassChar | charClassSpecialMeta | hex | unicode | esc) ^^ {List(_)}
            }
            def charClassMeta: Parser[List[MetaCharExp]] = meta ^^ {List(_)}
            def hyphen: Parser[List[Unit]] = "-" ^^ {_ => List(())}

            rep1(hyphen | charClassMeta | charClassElem) ^^ { ll =>
              def parseCharClassElems(ss: List[Any]): List[CharClassElem] = {
                ss match {
                  case (c1: Char) +: () +: (c2: Char) +: rest =>
                    RangeExp(c1,c2) +: parseCharClassElems(rest)
                  case (c: Char) +: rest => SingleCharExp(c) :: parseCharClassElems(rest)
                  case (m: MetaCharExp) +: rest => m :: parseCharClassElems(rest)
                  case () +: rest => SingleCharExp('-') :: parseCharClassElems(rest)
                  case Nil => Nil
                }
              }

              parseCharClassElems(ll.flatten)
            }
          }
          def backReferenceOrOct: Parser[RegExp[Char]] = "\\" ~> """\d+""".r ^^ { d =>
            if (d.head != '0' && (d.length == 1 || d.toInt <= captureGroups)) {
              BackReferenceExp(d.toInt)
            } else {
              val (octalPart, elemsPart) = d.take(3).span(_ < '8')
              (elemsPart + d.drop(3)).foldLeft(ElemExp(Integer.parseInt(s"0${octalPart}", 8).toChar): RegExp[Char])(
                (r,c) => ConcatExp(r, ElemExp(c))
              )
            }
          }
          def meta: Parser[MetaCharExp] = "\\" ~> metas ^^ {s => MetaCharExp(s.last)}
          def backslashAssertion: Parser[UnsupportedExp] = "\\" ~> backslashAssertions ^^ {s => UnsupportedExp(s"\\${s}")}
          def hex: Parser[Char] = "\\x" ~> "[0-9a-fA-F]{0,2}".r ^^ { code =>
            Integer.parseInt(s"0${code}", 16).toChar
          }
          def unicode: Parser[Char] = "\\u" ~> "[0-9a-fA-F]{0,4}".r ^^ { code =>
            if (code.length == 4) Integer.parseInt(s"0${code}", 16).toChar
            else throw RegExpParser.ParseException(s"illegal unicode representation: \\u${code}")
          }
          def esc: Parser[Char] = "\\" ~> ".".r ^^ {_.head}


          backReferenceOrOct | meta | backslashAssertion | elem | empty | eps | dot | group | charClassNeg | charClass
        }
        def quantifier: Parser[Either[(Option[Int],Option[Int]),String]] = {
          def symbol: Parser[Right[Nothing,String]] = "[*+?]".r ^^ {Right(_)}
          def repeat: Parser[Left[(Option[Int],Option[Int]),Nothing]] = {
            def minmax: Parser[(Option[Int],Option[Int])] = ("{" ~> opt(posNum) <~ ",") ~ opt(posNum) <~ "}" ^^ {
              case min ~ max => (min, max)
            }
            def exact: Parser[(Option[Int],Option[Int])] = "{" ~> posNum <~ "}" ^^ {
              case num => (Some(num), Some(num))
            }
            def posNum: Parser[Int] = """\d+""".r ^^ {_.toInt}

            (minmax | exact) ^^ {Left(_)}
          }

          symbol | repeat
        }

        factor ~ opt(quantifier ~ opt("?")) ^^ {
          case f ~ Some(q ~ opt) =>
            val greedy = opt.isEmpty
            q match {
              case Right(s) => s match {
                case "*" => StarExp(f,greedy)
                case "+" => PlusExp(f,greedy)
                case "?" => OptionExp(f,greedy)
              }
              case Left((min,max)) => RepeatExp(f,min,max,greedy)
            }
          case f ~ None => f
        }
      }

      rep(quantifiedFactor) ^^ {_.foldLeft(EpsExp(): RegExp[Char])(optConcatExp(_,_))}
    }

    opt("^") ~ rep1sep(term,"|") ~ opt("$") ^^ { case start ~ ts ~ end =>
      optConcatExp(
        optConcatExp(
          if (start.isDefined) EpsExp() else StarExp(DotExp(),false),
          ts.reduceLeft(AltExp(_,_))
        ),
        if (end.isDefined) EpsExp() else StarExp(DotExp(),true)
      )
    }
  }

  private def groupCount: Parser[Int] = {
    def group: Parser[Int] = "(" ~> groupCount <~ ")" ^^ {_ + 1}
    def nonCaptureGroup: Parser[Int] = "(?:" ~> groupCount <~ ")"
    def other: Parser[Int] = rep1("[^()]".r) ^^ {_ => 0}

    rep1(nonCaptureGroup | group | other) ^^ {_.sum}
  }
}

object RegExpParser {
  case class ParseException(message: String) extends Exception(message)

  def apply(s: String): RegExp[Char] = {
    new RegExpParser().parseAll(s)
  }
}
