package savinien.aoc18.parser

enum ParseError:
  private case SimpleParseError(message: String, input: Input, next: ParseError) extends ParseError
  private case CombinedParseError(message: String, list: List[ParseError]) extends ParseError
  private case EndError extends ParseError

  def merge(pe2: ParseError): ParseError = CombinedParseError("All branches failed", List(this, pe2))

  def push(msg: String, input: Input) : ParseError = SimpleParseError(msg, input, this)
  
  def relabel(message: => String): ParseError = this match
    case SimpleParseError(_, input, next) => SimpleParseError(message, input, next)
    case CombinedParseError(_, list)      => CombinedParseError(message, list)
    case EndError                         => this

  override def toString: String = output(0)

  private def safe(str: String): String = str.replaceAll("\n\r", "⬎").replaceAll("\n", "⬎").replaceAll("\r", "⬎").nn

  private def doIndent(level: Int): String =
    if level <= 0 then
      "\n"
    else
      "\n" + ("  " * level)

  private def output(level: Int): String = this match
    case SimpleParseError(message, input, next) => doIndent(level) + s"$input => ${safe(message)}" + next.output(level + 1)
    case CombinedParseError(message, list)      =>  list.foldLeft(s"${doIndent(level)}${safe(message)}") ((s, p) => s + p.output(level+1))
    case EndError                               => ""

object ParseError:
  def apply(message: String, input: Input): ParseError = ParseError.SimpleParseError(message, input, ParseError.EndError)