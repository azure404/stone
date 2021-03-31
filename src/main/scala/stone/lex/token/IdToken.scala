package stone.lex.token

class IdToken(lineNumber: Int, text: String)
  extends Token(lineNumber){

  override def isIdentifier: Boolean = true

  override def getText: String = text
}
