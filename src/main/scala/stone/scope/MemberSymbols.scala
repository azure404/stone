package stone.scope

import stone.scope.Symbols.Location

class MemberSymbols(mType: Int, outer: Symbols) extends Symbols(outer) {

  override def get(key: String, nest: Int): Symbols.Location = {
    val index: Option[Int] = table.get(key)
    if(index isEmpty){
      if(outer == null)
        null
      else
        outer.get(key, nest)
    } else {
      new Location(mType, index get)
    }
  }

  override def put(key: String): Location = {
    val loc: Location = get(key, 0)
    if(loc == null)
      new Location(mType, add(key))
    else
      loc
  }
}
object MemberSymbols{
  val METHOD: Int = -1
  val FIELD: Int = -2
}
