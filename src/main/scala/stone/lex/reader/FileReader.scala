package stone.lex.reader

import java.io.Reader

import scala.io.{Codec, Source}

class FileReader(path: String) extends Reader{

  implicit val enc: Codec = Codec.UTF8
  var buffer: String = _
  var pos: Int = _

  override def read(cbuf: Array[Char], off: Int, len: Int): Int = {
    if(buffer == null){
      readFile()
    }
    var size: Int = 0
    while(pos < buffer.length && size < len){
      cbuf(size) = buffer(pos)
      size += 1
      pos += 1
    }
    if(size == 0)
      -1
    else
      size
  }

  private def readFile(): Unit ={
    buffer = Source fromFile path mkString
  }

  override def close(): Unit = {}
}
