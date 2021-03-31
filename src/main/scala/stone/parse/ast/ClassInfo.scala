package stone.parse.ast

import stone.exception.StoneException
import stone.scope.Environment

class ClassInfo(definition: ClassStmt, env: Environment) {
  val super_class: ClassInfo = {
    val sc: Option[String] = definition.superClass
    if(sc nonEmpty)
      env.get(sc get).get match {
        case c: ClassInfo => c
        case _ => throw new StoneException(s"unknown super class: ${definition.superClass}", definition)
      }
    else
      null
  }

  def name: String = definition.name
  def superClass: ClassInfo = super_class
  def body: ClassBody = definition.body
  def environment: Environment = env

  override def toString: String = s"<class $name>"
}
