package stone.interpreter

import stone.parse.Parser
import stone.parse.ast.{ArrayLiteral, ArrayRef}

/**
 * elements : expr { "," expr }
 * primary  : ( "[" [ elements ] "]" | "(" expr ")") | NUMBER | IDENTIFIER | STRING ) { postfix }
 * postfix  : "(" [ args ] ")" | "[" expr "]"
 */
class ArrayParser extends FunctionParser {

  val elements: Parser = rule(classOf[ArrayLiteral]).ast(expr).repeat(rule().sep(",").ast(expr))

  reserved + "]"
  primary.insertChoice(rule().sep("[").maybe(elements).sep("]"))
  postfix.insertChoice(rule(classOf[ArrayRef]).sep("[").ast(expr).sep("]"))
}
