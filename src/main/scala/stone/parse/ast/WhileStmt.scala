package stone.parse.ast

import scala.collection.mutable.ArrayBuffer

class WhileStmt(nodes: ArrayBuffer[ASTree]) extends ASTList(nodes) {

  def condition: ASTree = child(0)
  def body: ASTree = child(1)

  override def toString: String = s"(while $condition $body)"
}
