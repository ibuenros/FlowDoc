package buenrostro.flowdoc

import scala.collection.mutable.ListBuffer
import scala.util.Random

case class DocNode( content: ListBuffer[String], id: String )

object DocNode {
  val InlinePattern = """\/\*\*(.*)\*\/""".r
  val IdPattern = """@docId ([A-Za-z0-9]+)""".r

  val rand = new Random

  def fromInline(doc: Token): DocNode = {
    val id: String = IdPattern.findFirstMatchIn(doc.value) match {
      case Some(m) => m.group(1)
      case None => rand.alphanumeric.take(10).mkString
    }
    val content = new ListBuffer[String]()
    content += InlinePattern.findFirstMatchIn(doc.value).get.group(1)
    DocNode( content, id)
  }

  def fromMultiLine(doc: ListBuffer[Token]): DocNode = {
    val content = ListBuffer[String]()
    var id = ""
    doc.foreach { token =>
      IdPattern.findFirstMatchIn(token.value) match {
        case Some(m) => id = m.group(1)
        case None =>
      }
      content += token.value
    }

    if (id == "") {
      id = rand.alphanumeric.take(10).mkString
    }
    DocNode(content, id)
  }
}

class DocParser(file: String) {
  val InlinePattern = """\/\*\*(.*)\*\/""".r

  val nodes = new ListBuffer[DocNode]()

  val reader = new CodeReader(file)

  var nextDoc = reader.getWithFilter(Set(TokenType.StartDoc, TokenType.InlineDoc))

  while (nextDoc.isDefined) {

    nextDoc match {
      case Some(doc) if doc.ttype == TokenType.InlineDoc => nodes += DocNode.fromInline(doc)
      case Some(doc) if doc.ttype == TokenType.StartDoc =>
        val docBuffer = new ListBuffer[Token]()
        var continue = true
        while (continue) {
          reader.getToken match {
            case None => continue = false
            case Some(t) if t.ttype == TokenType.Doc => docBuffer += t
            case Some(t) if t.ttype == TokenType.EndDoc => continue = false
          }
        }
        nodes += DocNode.fromMultiLine(docBuffer)
      case None =>
    }

    nextDoc = reader.getWithFilter(Set(TokenType.StartDoc, TokenType.InlineDoc))

  }
}