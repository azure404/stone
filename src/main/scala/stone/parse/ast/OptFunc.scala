package stone.parse.ast

import stone.scope.{ArrayEnvironment, Environment}

class OptFunc(override val parameters: ParameterList,
              override val body: BlockStmt,
              override val env: Environment,
              val memorySize: Int)
  extends Func(parameters, body, env) {

  override def makeEnv(): Environment = new ArrayEnvironment(memorySize, env)
}
