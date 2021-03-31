package stone.parse.ast

import stone.lex.token.Token

class ASTLeaf(t: Token) extends ASTree{
  val empty: List[ASTree] = List()

  override def child(i: Int): ASTree = throw new IndexOutOfBoundsException()

  override def numChildren: Int = 0

  override def children: Iterator[ASTree] = empty.iterator

  override def location: String = "at line " + t.getLineNumber

  override def toString: String = t getText

  def token: Token = t

}
