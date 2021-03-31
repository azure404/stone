package stone.parse.ast

import scala.collection.mutable.ArrayBuffer

class DefStmt(nodes: ArrayBuffer[ASTree]) extends ASTList(nodes) {
  var index: Int = _
  var size: Int = _

  def name: String = child(0).asInstanceOf[ASTLeaf].token.getText
  def parameters: ParameterList = child(1).asInstanceOf[ParameterList]
  def body: BlockStmt = child(2).asInstanceOf[BlockStmt]

  override def toString: String = s"(def $name $parameters $body)"
}
