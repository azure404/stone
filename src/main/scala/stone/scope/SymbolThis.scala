package stone.scope

import stone.exception.StoneException
import stone.scope.Symbols.Location

class SymbolThis(outer: Symbols) extends Symbols(outer) {

  add(SymbolThis.NAME)

  override def putNew(key: String): Int = throw new StoneException("fatal")

  override def put(key: String): Symbols.Location = {
    val loc: Location = outer put key
    if(loc.nest >= 0)
      loc.nest += 1
    loc
  }
}
object SymbolThis{
  val NAME: String = "this"
}

