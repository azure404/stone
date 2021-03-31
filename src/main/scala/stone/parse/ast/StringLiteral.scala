package stone.parse.ast

import stone.lex.token.Token

class StringLiteral(token: Token) extends ASTLeaf(token) {

  def value: String = token getText

}
