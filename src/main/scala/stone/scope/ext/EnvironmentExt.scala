package stone.scope.ext

import stone.exception.StoneException
import stone.scope.{ArrayEnvironment, Environment, NestedEnvironment, ResizableArrayEnvironment, Symbols}

object EnvironmentExt {

  implicit class EnvExt(e: Environment){
    def putNew(name: String, value: Any): Unit = {
      e match {
        case n: NestedEnvironment => n putNew(name, value)
        case r: ResizableArrayEnvironment => r putNew(name, value)
        case _ => throw new StoneException("bad type of environment")
      }
    }
    def where(name: String): Environment = {
      e match {
        case n: NestedEnvironment => n where name
        case _ => throw new StoneException("bad type of environment")
      }
    }
    def setOuter(e: Environment): Unit = {
      e match {
        case n: NestedEnvironment => n.outer = e
        case a: ArrayEnvironment => a.outer = e
        case _ => throw new StoneException("bad type of environment")
      }
    }
    def get(nest: Int, index: Int): Option[Any] = {
      e match {
        case a: ArrayEnvironment => a.get(nest, index)
        case _ => throw new StoneException("bad type of environment")
      }
    }
    def put(nest: Int, index: Int, value: Any): Unit = {
      e match {
        case a: ArrayEnvironment => a.put(nest, index, value)
        case _ => throw new StoneException("bad type of environment")
      }
    }

    def symbols: Symbols = {
      e match {
        case a: ResizableArrayEnvironment => a.symbols()
        case _ => throw new StoneException("bad type of environment")
      }
    }
  }

}
