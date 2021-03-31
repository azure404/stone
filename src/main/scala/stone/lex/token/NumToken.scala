package stone.lex.token

class NumToken(lineNumber: Int, value: Long)
  extends Token(lineNumber){

  override def getNumber: Long = value

  override def isNumber: Boolean = true

  override def getText: String = value toString
}
