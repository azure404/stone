package stone.lex

import java.io.{LineNumberReader, Reader}
import java.util.regex.{Matcher, Pattern}

import stone.exception.ParseException
import stone.lex.token.{IdToken, NumToken, StrToken, Token}

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

class Lexer(r: Reader) {

  // \s*匹配空格, //.*匹配注释
  val blankPat = "\\s*"
  val commentPat = "//.*"
  val numPat: String = "[0-9]+"
  //String(\\,\n,\"为转义字符): "(\"|\\|\n|[^"])*"
  val strPat: String = "\"(\\\\\"|\\\\\\\\|\\\\n|[^\"])*\""
  val idPat: String = "[A-Z_a-z][A-Z_a-z0-9]*|==|<=|>=|&&|\\|\\||\\p{Punct}"
  val lexPat: String = s"$blankPat(($commentPat)|($numPat)|($strPat)|$idPat)?"
  val pattern: Pattern = Pattern compile lexPat

  private val queue: ArrayBuffer[Token] = ArrayBuffer()
  private var hasMore: Boolean = true
  private val reader: LineNumberReader = new LineNumberReader(r)

  def read(): Token = {
    if(fillQueue(0))
      queue remove 0
    else
      Token.EOF
  }

  def peek(index: Int): Token ={
    if(fillQueue(index))
      queue(index)
    else
      Token.EOF
  }

  private def fillQueue(index: Int): Boolean = {
    while(index >= queue.size){
      if(hasMore)
        readAndParse()
      else
        return false
    }
    true
  }

  private def readAndParse(): Unit = {
    var line: String  = readLine getOrElse null
    if(line == null){
      hasMore = false
      return
    }
    val lineNo: Int = reader getLineNumber
    val matcher: Matcher = pattern matcher line
    matcher useTransparentBounds true useAnchoringBounds false

    var pos: Int = 0
    val endPos: Int = line.length()
    while(pos < endPos) {
      matcher region(pos, endPos)
      if(matcher lookingAt){
        addToken(lineNo, matcher)
        pos = matcher end
      } else {
        throw new ParseException(s"bad token at line $lineNo")
      }
    }
    queue addOne new IdToken(lineNo, Token.EOL)
  }

  private def addToken(lineNo: Int, matcher: Matcher) = {
    val m: String = matcher group 1
    if(m != null){ //if not a space
      if(matcher.group(2) == null){ // if not a comment
        var token: Token = null
        if(matcher.group(3) != null){
          token = new NumToken(lineNo, m toLong)
        } else if(matcher.group(4) != null){
          token = new StrToken(lineNo, m.toStringLiteral())
        } else {
          token = new IdToken(lineNo, m)
        }
        queue addOne token
      }
    }
  }

  private def readLine: Try[String] = {
    Try(reader readLine)
  }

  implicit class StringLiteralConverter(str: String){

    def toStringLiteral(): String = {
      var literal: String = ""
      val len = str.length - 1
      var i = 1
      while(i < len){
        var c: Char = str charAt i
        if(c == '\\' && i + 1 < len){
          val c2: Char = str charAt(i + 1)
          if(c2 == '"' || c2 == '\\') {
            i += 1
            c = str charAt i
          } else if(c2 == 'n'){
            i += 1
            c = '\n'
          }
        }
        literal += c
        i += 1
      }
      literal
    }

  }




}
