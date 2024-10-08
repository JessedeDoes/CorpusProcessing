package utils

import java.util.regex.Pattern

import scala.collection.mutable.ArrayBuffer

trait Tokenizer
{
  case class Token(leading:String, token:String, trailing:String) {
    override def toString: String = leading + token + trailing
    def isEmpty = List(leading,token,trailing).forall(_.isEmpty)
    def hasContent = token.nonEmpty
  }

  def tokenize(s:String): Array[Token]
}

object Tokenizer extends Tokenizer
{
  import scala.util.matching._
  val Split = new Regex("(?s)^(\\p{P}*)(.*?)(\\p{P}*)$")

  def tokenizeOne(s:String): Token =
  {
    val Split(l,c,r) = s.trim
    if (l=="'" && Set("s", "m", "t").contains(c))
      Token("", l+c, r)
    else if (l.matches("\\.+") && c.isEmpty && r.isEmpty)
      Token("", "", l)
    else
      Token(l,c,r)
  }

  def tokenizeErVanaf(s: String)  = {
    val t = tokenizeOne(s)
    val lead = Token(t.leading,"","")
    val token =  Token("",t.token,"")
    val trail = Token("", "", t.trailing)
    List(lead,token,trail).filter(!_.isEmpty)
  }

  def doNotTokenize(s:String): Token = Token("",s,"")

  override def tokenize(s:String): Array[Token] =
    s.split("\\s+").map(tokenizeOne)

  def main(args:Array[String]):Unit =
  {
    println(tokenize("The dog, i think, is 'hardly-' interesting??!").toList);
  }
}

object TokenizerWithOffsets
{
  import Tokenizer._
  lazy val notWhite = Pattern.compile("\\S+")

  case class TokenWithOffsets(token:Token, startPosition:Int, endPosition:Int)
  {
    lazy val text = if (this.token.token.length > 0) this.token.token else "_blank_"
    lazy val textWithPunct = this.token.leading + this.token.token + this.token.trailing
  }

  implicit val tokenize = true

  def tokenize(s:String)(implicit really:Boolean): Array[TokenWithOffsets] =
  {
    val matcher = notWhite.matcher(s)
    val r = new ArrayBuffer[TokenWithOffsets]
    while (matcher.find)
    {
      val t = if (really) tokenizeOne(matcher.group) else doNotTokenize(matcher.group)
      val z = TokenWithOffsets(t, matcher.start,  matcher.end)
      r += z
    }
    r.toArray
  }

  def main(args:Array[String]):Unit = println(TokenizerWithOffsets.tokenize("Waarom, *waarom*, hebt u mij verlaten *ālgēr??").toList)
}
