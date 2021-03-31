package stone.interpreter

import stone.lex.token.Token
import stone.parse.Parser
import stone.parse.ast.{ClassBody, ClassStmt, Dot}

/**
 * member       : func | simple
 * class_body   : "{" [ member ] {( ";" | EOL ) [ member ]} "}"
 * defclass     : "class" IDENTIFIER [ "extends" IDENTIFIER ] class_body
 * postfix      : "." IDENTIFIER | "(" [ args ] ")"
 * program      : [ defclass | def | statement ] ( ";" | EOL )
 */
class ClassParser extends ClosureParser {

  val member: Parser = rule().or(func, simple)
  val class_body: Parser = rule(classOf[ClassBody]).sep("{")
                                  .option(member)
                                  .repeat(rule().sep(";", Token.EOL).option(member))
                                  .sep("}")
  val defclass: Parser = rule(classOf[ClassStmt])
                              .sep("class")
                              .identifier(reserved)
                              .option(rule().sep("extends").identifier(reserved))
                              .ast(class_body)

  postfix.insertChoice(rule(classOf[Dot]).sep(".").identifier(reserved))
  program.insertChoice(defclass)
}
