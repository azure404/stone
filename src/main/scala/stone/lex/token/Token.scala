package stone.lex.token

import stone.exception.StoneException

abstract class Token(lineNumber: Int) {
  def getLineNumber: Int = lineNumber
  def isIdentifier: Boolean = false
  def isString: Boolean = false
  def isNumber: Boolean = false
  def getNumber: Long = throw new StoneException("not number token")
  def getText: String = ""
}

object Token {
  val EOF: Token = new Token(-1) {}
  val EOL: String = "\n"
}





