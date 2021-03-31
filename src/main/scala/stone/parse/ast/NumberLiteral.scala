package stone.parse.ast

import stone.lex.token.Token

class NumberLiteral(token: Token) extends ASTLeaf(token) {

  def value: Long = token getNumber

}
