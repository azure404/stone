package stone.parse.ast

import stone.interpreter.OptStoneObject
import stone.scope.{ArrayEnvironment, Environment}

class OptMethod(override val parameters: ParameterList,
                override val body: BlockStmt,
                override val env: Environment,
                override val memorySize: Int,
                val self: OptStoneObject)
  extends OptFunc(parameters, body, env, memorySize){

  override def makeEnv(): Environment = {
    val e: ArrayEnvironment = new ArrayEnvironment(memorySize, env)
    e put(0, 0, self)
    e
  }
}
