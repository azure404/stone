package stone.interpreter

import stone.parse.ast.FunExpr

/**
 * 闭包语法：
 * primay     : "fun" param_list block | primary
 */
class ClosureParser extends ArrayParser {

  primary.insertChoice(rule(classOf[FunExpr]).sep("fun").ast(paramList).ast(block))
}
