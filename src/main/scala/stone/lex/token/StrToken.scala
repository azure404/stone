package stone.lex.token

class StrToken(lineNumber: Int, literal: String)
  extends Token(lineNumber){

  override def getText: String = literal

  override def isString: Boolean = true
}
