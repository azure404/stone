package stone.parse.ast

import stone.lex.token.Token

class Name(token: Token) extends ASTLeaf(token) {

  val UNKNOWN: Int = -1
  var nest: Int = _
  var index: Int = UNKNOWN

  def name: String = token getText

}
