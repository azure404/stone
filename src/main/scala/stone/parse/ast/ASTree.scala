package stone.parse.ast

abstract class ASTree {

  def child(i: Int): ASTree

  def numChildren: Int

  def children: Iterator[ASTree]

  def location: String

  def iterator: Iterator[ASTree] = children

}
