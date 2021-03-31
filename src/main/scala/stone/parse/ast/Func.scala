package stone.parse.ast

import stone.scope.{Environment, NestedEnvironment}

class Func(val parameters: ParameterList, val body: BlockStmt, val env: Environment){

  def makeEnv(): Environment = new NestedEnvironment(env)
  override def toString: String = s"<func: ${hashCode()}>"
}
