package savinien.aoc18
package parsers

trait ParserState[ST, LT]:
  def hasNext: Boolean
  def next: Option[(ST, ParserState[ST, LT])]
  def next(n: Int): Option[(LT, ParserState[ST, LT])]
  def rest: (LT, ParserState[ST, LT])

class StringParserState private(str: String, offset: Int) extends ParserState[Char, String]:
  def this(str: String) = this(str, 0)

  override def hasNext: Boolean = str.length > offset

  override def next: Option[(Char, StringParserState)] = 
    if str.length > offset then Some((str.charAt(offset), StringParserState(str, offset + 1)))
    else None

  override def next(c: Int): Option[(String, StringParserState)] = 
    if c < 0 || str.length < offset + c then None
    else Some((str.substring(offset, offset + c).nn, StringParserState(str, offset + c)))

  override def rest: (String, StringParserState) = 
    (str.substring(offset).nn, StringParserState(str, str.length))

  override def toString: String =
    s"(${offset}) '${noLineBreak(preParsed(5))}\u22EE${noLineBreak(postParsed(5))}'"

  private def preParsed(count: Int): String = {
    if offset <= count then
      str.substring(0, offset).nn
    else
      "\u2026" + str.substring(offset - count, offset).nn
  }

  private def postParsed(n: Int): String =
    if offset + n < str.length then
      str.substring(offset, offset + n).nn + "\u2026"
    else
      str.substring(offset).nn

  private def noLineBreak(str: String): String =
    str.replaceAll("\n\r", "⬎").nn.replaceAll("\n", "⬎").nn.replaceAll("\r", "⬎").nn
