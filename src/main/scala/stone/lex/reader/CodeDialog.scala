package stone.lex.reader

import java.io.{BufferedReader, FileNotFoundException, FileReader, Reader}

import javax.swing.{JFileChooser, JOptionPane, JScrollPane, JTextArea}

class CodeDialog extends Reader{

  var buffer: String = _
  var pos: Int = _

  override def read(cbuf: Array[Char], off: Int, len: Int): Int = {
    if(buffer == null){
      val in: Option[String] = showDialog()
      if(in isEmpty) {
        return -1
      } else {
        val str: String = in get;
        println(str)
        buffer = str + "\n"
        pos = 0
      }
    }
    fill(cbuf, off, len)
  }

  private def fill(cbuf: Array[Char], off: Int, len: Int): Int ={
    var size: Int = 0
    val length: Int = buffer.length()
    while(pos < length && size < len) {
      cbuf(off + size) = buffer charAt pos
      size += 1
      pos +=1
    }
    if(pos == length){
      buffer = null
    }
    size
  }

  private def showDialog(): Option[String] = {
    val area: JTextArea = new JTextArea(20, 40)
    val pane: JScrollPane = new JScrollPane(area)
    val result: Int = JOptionPane.showOptionDialog(
      null, pane, "Input",
      JOptionPane.OK_CANCEL_OPTION,
      JOptionPane.PLAIN_MESSAGE,
      null, null, null)

    result match {
      case JOptionPane.OK_OPTION => Option(area getText)
      case _ => Option empty
    }
  }

  override def close(): Unit = {}

  def file(): Reader = {
    val chooser: JFileChooser = new JFileChooser()
    if(chooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION)
      new BufferedReader(new FileReader(chooser getSelectedFile))
    else
      throw new FileNotFoundException("no file specified")
  }
}
