package stone.exception

import java.io.IOException

import stone.lex.token.Token

class ParseException (message: String) extends Exception(message){

  def this(e: IOException) {
    this(e toString)
    fillInStackTrace
  }

  def this(t: Token) = {
    this(s"syntax error around ${ParseException.location(t)}")
    fillInStackTrace
  }

}

object ParseException{
  def location(t: Token): String = {
    t match {
      case Token.EOF => "the last line"
      case _ => "\"" + t.getText + "\" at line " + t.getLineNumber
    }
  }

  def e(t: Token): ParseException = new ParseException(s"syntax error around ${location(t)}")
  def e(message: String, t: Token): ParseException = new ParseException(s"syntax error around ${location(t)}. $message")
}