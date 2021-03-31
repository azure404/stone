package stone.parse.ast

import scala.collection.mutable.ArrayBuffer

class NegativeExpr(nodes: ArrayBuffer[ASTree]) extends ASTList(nodes){

  def operand: ASTree = child(0)
  override def toString: String = s"-$operand"

}
