package stone.parse.ast

import scala.collection.mutable.ArrayBuffer

class ASTList(nodes: ArrayBuffer[ASTree]) extends ASTree{

  override def child(i: Int): ASTree = nodes(i)

  override def numChildren: Int = nodes length

  override def children: Iterator[ASTree] = nodes iterator

  override def location: String = {
    var s: String = null
    for(n <- nodes if s != null){
      s = n location
    }
    s
  }

  override def toString: String = {
    "(" + (nodes mkString " ") + ")"
  }
}
