package savinien.aoc18.parser


final class Input(str: String, offset: Int):
  def addOffset(plus: Int): Input =
    Input(str, offset + plus)

  def slice(n: Int): String = 
    if offset + n <= str.length then
      str.substring(offset, offset + n).nn
    else
      str.substring(offset).nn

  def toParse: String = str.substring(offset).nn

  private def preParsed(n: Int): String =
    val count = offset.min(n)
    if count == 0 then
      ""
    else
      val start = offset - count
      if start > 0 then
        "\u2026" + str.substring(start, offset).nn
      else
        str.substring(start, offset).nn

  private def postParsed(n: Int): String =
    if offset + n < str.length then
      str.substring(offset, offset + n).nn + "\u2026"
    else
      str.substring(offset).nn

  private def safe(str: String): String = 
    str.replaceAll("\n\r", "⬎").replaceAll("\n", "⬎").replaceAll("\r", "⬎").nn

  override def toString: String = 
    s"(${offset}) '${safe(preParsed(5))}'+'${safe(postParsed(5))}'"