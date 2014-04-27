package buenrostro.flowdoc

import scala.io.Source
import java.io.{File,FileOutputStream}
import scala.collection.mutable.ListBuffer

object FileUtils {
  val NonSpacePattern = "\\S".r
  val StartCommentPattern = "\\/\\/|\\/\\*".r
}

object TokenType extends Enumeration {
  type TokenType = Value
  val StartDoc, Doc, EndDoc, InlineDoc, StartBlock, Block, EndBlock, InlineBlock, Comment, Code, LineStart = Value
}

case class Token(value: String, ttype: TokenType.TokenType)

case class Line(var line: String, initialType: TokenType.TokenType) extends Iterator[Token] {
  import TokenType._

  final val InlineTokens = List(TokenType.Code, TokenType.Comment)

  val tokens = new ListBuffer[Token]()
  var currentType = initialType
  var currentIndex = -1

  var indentationIndex: Int = FileUtils.NonSpacePattern.findFirstMatchIn(line) match {
    case Some(m) => m.start
    case None => line.size
  }

  val indentation: String = line.substring(0, indentationIndex)
  line = line.substring(indentationIndex)

  while (line.size > 0) {
    tokens += nextToken()
  }

  override def next(): Token = {
    currentIndex += 1
    tokens(currentIndex)
  }

  override def hasNext(): Boolean = {
    (currentIndex + 1) < tokens.size
  }

  def render(): String = {
    tokens.foldLeft(Token(indentation, TokenType.LineStart))({
      (container, newToken) => {
        (container.ttype, newToken.ttype) match {
          case (LineStart, _) =>
            Token(container.value + newToken.value, newToken.ttype)
          case (StartBlock, _) | (StartDoc, _) =>
            Token(container.value + newToken.value, newToken.ttype)
          case (x, y) if (InlineTokens.contains(x) && InlineTokens.contains(y)) =>
            Token(container.value + newToken.value, newToken.ttype)
          case _ =>
            Token(container.value + "\n" + indentation + removeInitialSpaces(newToken.value), newToken.ttype)
        }
      }
    }).value
  }

  def lastType(): TokenType = {
    if (tokens.size > 0) {
      tokens.last.ttype
    } else {
      initialType
    }
  }

  def removeInitialSpaces(string: String): String = {
    string.replaceAll("""^\s+""", "")
  }

  def nextToken(): Token = {
    if (currentType == TokenType.Block || currentType == TokenType.Doc) {
      if ( line.startsWith("""*/""") ) {
        val segments = line.splitAt(2)
        val tempType = currentType
        currentType = TokenType.Code
        line = segments._2
        tempType match {
          case TokenType.Block => Token(segments._1, TokenType.EndBlock)
          case TokenType.Doc => Token(segments._1, TokenType.EndDoc)
        }
      } else {
        val endComment = line.indexOf("""*/""") match {
          case x if ( x < 0 ) => line.size
          case x => x
        }
        val segments = line.splitAt(endComment)
        line = segments._2
        Token(segments._1, currentType)
      }
    } else {
      line match {
        case x if (x.startsWith( """/**""")) =>
          line.indexOf( """*/""") match {
            case -1 =>
              val segments = line.splitAt(2)
              line = segments._2
              currentType = TokenType.Doc
              Token(segments._1, TokenType.StartDoc)
            case endComment: Int =>
              val segments = line.splitAt(endComment + 2)
              line = segments._2
              Token(segments._1, TokenType.InlineDoc)
          }
        case x if (x.startsWith( """/*""")) =>
          line.indexOf( """*/""") match {
            case -1 =>
              val segments = line.splitAt(2)
              line = segments._2
              currentType = TokenType.Block
              Token(segments._1, TokenType.StartBlock)
            case endComment: Int =>
              val segments = line.splitAt(endComment + 2)
              line = segments._2
              Token(segments._1, TokenType.InlineBlock)
          }
        case x if (x.startsWith( """//""")) =>
          currentType = TokenType.Comment
          val tempLine = line
          line = ""
          Token(tempLine, TokenType.Comment)
        case _ =>
          FileUtils.StartCommentPattern.findFirstMatchIn(line) match {
            case Some(x) =>
              val segments = line.splitAt(x.start)
              line = segments._2
              Token(segments._1, TokenType.Code)
            case None =>
              val segments = (line, "")
              line = ""
              Token(segments._1, TokenType.Code)
        }
      }
    }
  }

}

/** File reader that also writes a new file.
  *
  * @param file input file name
  */
class MirrorFileReader(file: String) {

  private val inputFile = Source.fromFile(file)
  private val lineBuffer = inputFile.getLines()

  private val output = file + ".tmp"
  private val outFile = new File(output)
  private val os = new FileOutputStream(outFile)

  protected var line: Option[String] = None

  /** Gets a line as a String, flushes the previous line to output file.
    *
    * @return Option[String]
    */
  def getLine: Option[String] = {
    if (line.isDefined){
      os.write((line.get + "\n").getBytes)
    }
    if (lineBuffer.hasNext){
      line = Some(lineBuffer.next())
    } else {
      line = None
    }

    line
  }

  /** Get the first line of the file. Returns none if a line has already been read.
    *
    * @return Option[String]
    */
  def getFirstLine: Option[String] = {
    if (line.isDefined){
      return None
    }
    if (lineBuffer.hasNext){
      Some(lineBuffer.next())
    } else {
      None
    }
  }

  /** Close input and output files.
    * It does NOT flush lines.
    */
  def close() {
    inputFile.close()
    os.close()
  }

}


class CodeReader(file: String) extends MirrorFileReader(file) {
  import TokenType._

  private var codeLine: Option[Line] = getFirstLine match {
    case Some (str) => Some (Line (str, TokenType.Code) )
    case None => None
  }

  override def getLine: Option[String] = {
    if ( codeLine.isDefined ) {
      line = Some(codeLine.get.render())
    }
    super.getLine
  }

  def getWithFilter(allowTypes: Set[TokenType]): Option[Token] = {
    var token = getToken
    while (token.isDefined && !allowTypes.contains(token.get.ttype)) {
      token = getToken
    }
    token
  }

  def getToken: Option[Token] = {
    codeLine match {
      case Some(l) if (l.hasNext()) => Some(l.next())
      case Some(l) =>
        nextNonEmptyLine()
        codeLine match {
          case Some(lInner) => Some(lInner.next())
          case None => None
        }
      case None => None
    }
  }

  private def nextNonEmptyLine() {
    while (true) {
      getLine match {
        case None =>
          codeLine = None
          return
        case Some(s) =>
          codeLine = Some(Line(s, codeLine.get.lastType))
          if (codeLine.get.hasNext()) {
            return
          }
      }
    }
  }

}