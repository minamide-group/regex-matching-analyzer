package matching.regexp

import scala.util.parsing.combinator._
import RegExp._
import RegExpIR._

class RegExpParser() extends RegexParsers {
  override val skipWhitespace = false

  val specialMetaChars = "aefnrt"
  val chars = """[^∅ε.|*+?^$()\[\]\\]""".r
  val metas = s"[dDhHRsSvVwW]".r
  val spacialMetas = s"[${specialMetaChars}]".r
  val charsInCharClass = """[^\]\\]""".r
  val spacialMetasInCharClass = s"[${specialMetaChars}b]".r

  def parseAll(s: String): RegExp[Char] = {
    def exp: Parser[RegExp[Char]] = {
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
            def dot: Parser[DotExp] = "." ^^ {_ => DotExp()}
            def group: Parser[RegExp[Char]] = {
              def captureGroup: Parser[RegExp[Char]] = "(" ~> exp <~ ")" ^^ {GroupExpIR(_,None)}
              def unCaptureGroup: Parser[RegExp[Char]] = "(?:" ~> exp <~ ")"
              def namedCaptureGroup: Parser[RegExp[Char]] = {
                "(?" ~> (opt("P") ~> "<" ~> variable <~ ">" | "'" ~> variable <~ "'") ~ exp <~ ")" ^^ {
                  case name ~ r => GroupExpIR(r,Some(name))
                }
              }

              captureGroup | unCaptureGroup | namedCaptureGroup
            }
            def startAnchor: Parser[StartAnchorExp[Char]] = "^" ^^ {_ => StartAnchorExp()}
            def endAnchor: Parser[EndAnchorExp[Char]] = "$" ^^ {_ => EndAnchorExp()}
            def charClass: Parser[CharClassExp] = {
              def charClassFactors: Parser[Seq[CharClassElem]] = {
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

              "[" ~> opt("^") ~ charClassFactors <~ "]" ^^ {
                case neg ~ fs => CharClassExp(fs,neg.isEmpty)
              }
            }
            def lookaround: Parser[RegExp[Char]] = "(?" ~> opt("<") ~ ("=" | "!") ~ exp <~ ")" ^^ {
              case behind ~ p ~ r =>
              val positive = p == "="
              if (behind.isEmpty) LookaheadExp(r, positive) else LookbehindExp(r, positive)
            }
            def ifCond: Parser[IfExp[Char]] = ("(?(" ~> exp <~ ")") ~ term ~ opt("|" ~> exp) <~ ")" ^^ {
              case cond ~ rt ~ rf => IfExp(cond, rt, rf.getOrElse(EpsExp()))
            }

            def exp: Parser[RegExp[Char]] = rep1sep(term,"|") ^^ {_.reduceLeft(AltExp(_,_))}
            def meta: Parser[MetaCharExp] = "\\" ~> metas ^^ {s => MetaCharExp(s.last)}
            def namedBackRef: Parser[NamedBackReferenceExpIR[Char]] = (
              "(?P=" ~> variable <~ ")" |
              "\\k" ~> ("<" ~> variable <~ ">" |
              "'" ~> variable <~ "'" |
              "{" ~> variable <~ "}")
            ) ^^ {NamedBackReferenceExpIR(_)}
            def num: Parser[NumExpIR] = "\\" ~> """\d+""".r ^^ {NumExpIR(_)}
            def hex: Parser[Char] = "\\x" ~> "[0-9a-fA-F]{0,2}".r ^^ { code =>
              Integer.parseInt(s"0${code}", 16).toChar
            }
            def unicode: Parser[Char] = "\\u" ~> "[0-9a-fA-F]{0,4}".r ^^ { code =>
              if (code.length == 4) {
                Integer.parseInt(s"0${code}", 16).toChar
              } else throw RegExpParser.ParseException(s"invalid unicode representation: \\u${code}")
            }
            def esc: Parser[Char] = "\\" ~> "[^a-zA-Z]".r ^^ {_.head}
            def variable: Parser[String] = "[a-zA-Z][a-zA-Z0-9]*".r

            meta | num | namedBackRef | elem | empty | eps | dot | group | charClass |
            startAnchor | endAnchor | lookaround | ifCond
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

      rep1sep(term,"|") ^^ {_.reduceLeft(AltExp(_,_))}
    }

    try {
      parseAll(exp, s) match {
        case Success(r,_) => r.toRegExp
        case NoSuccess(msg,next) =>
          throw RegExpParser.ParseException(s"${msg} at ${next.pos.line}:${next.pos.column}")
      }
    } catch {
      case e: InvalidRegExpException =>
        throw RegExpParser.ParseException(s"${e.message}")
    }
  }
}

object RegExpParser {
  case class ParseException(message: String) extends Exception(message)

  def apply(s: String): RegExp[Char] = {
    new RegExpParser().parseAll(s)
  }

  def parsePCRE(s: String): (RegExp[Char], PCREOption) = {
    val endDelimiter = s.head match {
      case '(' => ')'
      case '{' => '}'
      case '[' => ']'
      case '<' => '>'
      case c => c
    }

    val endBodyIndex = s.lastIndexWhere(_ == endDelimiter)
    if (endBodyIndex == 0) {
      throw new ParseException("invalid PCRE style expression.")
    } else {
      val body = s.slice(1,endBodyIndex)
      val options = s.drop(endBodyIndex+1)
      val option = try {
        new PCREOption(options)
      } catch {
        case e: InvalidRegExpException =>
          throw RegExpParser.ParseException(s"${e.message}")
      }

      (new RegExpParser().parseAll(body), option)
    }
  }
}
