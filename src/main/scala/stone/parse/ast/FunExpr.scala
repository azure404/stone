package stone.parse.ast

import scala.collection.mutable.ArrayBuffer

class FunExpr(nodes: ArrayBuffer[ASTree]) extends ASTList(nodes) {
  var size: Int = -1

  def parameters: ParameterList = child(0).asInstanceOf[ParameterList]
  def body: BlockStmt = child(1).asInstanceOf[BlockStmt]

  override def toString: String = s"(fun $parameters $body)"
}
