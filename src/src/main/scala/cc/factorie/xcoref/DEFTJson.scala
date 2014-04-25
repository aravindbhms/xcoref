package cc.factorie.xcoref

import java.io._
import java.util.zip.{GZIPOutputStream, GZIPInputStream}
import scala.collection.mutable.ArrayBuffer
import cc.factorie.app.nlp.{BasicSection, Section, TokenString, Sentence}
import cc.factorie.app.nlp.parse.{ParseTree, ParseTreeLabelDomain}
import cc.factorie.app.nlp.lemma.TokenLemma
import cc.factorie.app.nlp.pos.PennPosTag
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.core.`type`.TypeReference
import java.lang.reflect.{ParameterizedType, Type}
import scalax.io.Codec
import cc.factorie.app.nlp.mention.MentionList

class Paragraph(val document: cc.factorie.app.nlp.Document, val stringStart: Int, val stringEnd: Int) extends Section

class SGMSource(val source: String)

class JacksonWrapper {
  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)

  def serialize(value: Any): String = {
    import java.io.StringWriter
    val writer = new StringWriter()
    mapper.writeValue(writer, value)
    writer.toString
  }

  def deserialize[T: Manifest](value: String): T =
    mapper.readValue(value, typeReference[T])

  private[this] def typeReference[T: Manifest] = new TypeReference[T] {
    override def getType = typeFromManifest(manifest[T])
  }

  private[this] def typeFromManifest(m: Manifest[_]): Type = {
    if (m.typeArguments.isEmpty) {
      m.erasure
    }
    else new ParameterizedType {
      def getRawType = m.erasure

      def getActualTypeArguments = m.typeArguments.map(typeFromManifest).toArray

      def getOwnerType = null
    }
  }
}

object JacksonWrapper extends JacksonWrapper

object DEFTJson {

  case class CrossDocEntity(id: String,
                            deids: Seq[String],
                            reid: Option[String],
                            rmids: Seq[String],
                            rsids: Seq[String],
                            et: String,
                            strings: Seq[String], freebaseID: Option[String])

  case class Document(name: String,
                      src: String,
                      text: String,
                      sects: Seq[Section],
                      ents: Seq[DocEntity])

  case class DocEntity(id: String,
                       ms: Seq[DocMention],
                       canon: String)

  case class DocMention(id: String,
                        secStart: Int,
                        start: Int,
                        len: Int,
                        mType: Option[String],
                        headTokIdx: Int,
                        rmId: Option[String],
                        rsId: Option[String],
                        tackType: Option[String])

  case class Section(start: Int,
                     end: Int,
                     senStarts: Seq[Int],
                     senLens: Seq[Int],
                     tokens: Seq[Token])

  case class Token(start: Int,
                   length: Int,
                   string: String,
                   lemma: String,
                   pos: String,
                   parseParent: Int,
                   parseLabel: String)

  def deserializeFile(inputFilename: String, encoding: String = "UTF-8"): Iterator[cc.factorie.app.nlp.Document] = {
    new Iterator[cc.factorie.app.nlp.Document] {
      val reader = new BufferedReader(new InputStreamReader(
        new GZIPInputStream(new FileInputStream(inputFilename)), encoding))
      var line = reader.readLine()

      def hasNext = line != null

      def next() = {
        val d = deserialize(line)
        line = reader.readLine()
        d
      }
    }
  }

  def deserialize(string: String): cc.factorie.app.nlp.Document = {
    deserializeDoc(JacksonWrapper.deserialize[Document](string))
  }

  private def deserializeDoc(doc: Document): cc.factorie.app.nlp.Document = {
    var result: cc.factorie.app.nlp.Document = null
    try {
      val d = new cc.factorie.app.nlp.Document(doc.text.replaceAll("\t", " ").replaceAll("\n", " ")).setName(doc.name)
      d.attr += new SGMSource(doc.src)
      for (s <- doc.sects) d += deserializeSection(s, d)
      deserializeEntities(doc.ents, d)
      result = d
    }
    catch {
      case e: Exception => {
        result = new cc.factorie.app.nlp.Document("").setName("[EMPTY]"); println("Exception caught while reading document. Returning empty document."); e.printStackTrace
      }
      case e: Error => {
        result = new cc.factorie.app.nlp.Document("").setName("[EMPTY]"); println("Error thrown while reading document. Returning empty document."); e.printStackTrace
      }
    }
    result
  }

  private def deserializeEntities(es: Seq[DocEntity], d: cc.factorie.app.nlp.Document) = {
    val des = new DocEntities
    val dms = new DocMentions
    for (e <- es) {
      val mentions: ArrayBuffer[cc.factorie.xcoref.DocMention] = new ArrayBuffer
      val canonical: cc.factorie.xcoref.DocMention = null
      for (m <- e.ms) {
        val dm = cc.factorie.xcoref.DocMention(d, m.secStart, m.start, m.len,
          null, m.id, m.headTokIdx)
        if (m.mType.isDefined) dm.mentionType = m.mType.get
        if (m.tackType.isDefined) dm.entityType = m.tackType.get
        if (m.rmId.isDefined) dm.refMentionId = m.rmId.get
        if (m.rsId.isDefined) dm.refSlotValueId = m.rsId.get
        mentions += dm
        dms += dm
        //if (e.canon == m.id) canonical = dm
      }
      des += new cc.factorie.xcoref.DocEntity(mentions, null, e.id, canonical)
    }
    d.attr += des
    d.attr += dms
    d.attr += new MentionList
    d.attr[MentionList] ++= dms.map(_.mention)
    d
  }

  private def deserializeSection(s: Section, doc: cc.factorie.app.nlp.Document) = {
    //    val section = new Paragraph(doc, s.start, s.end)
    val section = new BasicSection(doc, s.start, s.end)
    val sentenceStarts = s.senStarts
    val sentenceLens = s.senLens

    val tokenStarts = s.tokens.map(_.start)
    val tokenLengths = s.tokens.map(_.length)
    val tokenStrings = s.tokens.map(_.string)
    val tokenLemma = s.tokens.map(_.lemma)
    val tokenPOS = s.tokens.map(_.pos)
    //val tokenNER = this.tokenNER.value
    val tokenParseParent = s.tokens.map(_.parseParent)
    val tokenParseDepRel = s.tokens.map(_.parseLabel)

    // add parse (fill the domain to start)
    ParseTreeLabelDomain ++= tokenParseDepRel
    for (s <- section.sentences) {
      val newTree = new ParseTree(s)
      for (si <- 0 until s.length) {
        val di = s.start + si
        newTree.setParent(si, tokenParseParent(di))
        val li = ParseTreeLabelDomain.index(tokenParseDepRel(di))
        newTree.label(si).set(li)(null)
      }
      s.attr += newTree
    }

    for (i <- 0 until tokenStarts.length) {
      val token = new cc.factorie.app.nlp.Token(section, tokenStarts(i), tokenStarts(i) + tokenLengths(i))
      token.attr += new TokenString(token, tokenStrings(i))
      token.attr += new TokenLemma(token, tokenLemma(i))
      token.attr += new PennPosTag(token, tokenPOS(i))
    }

    for (i <- 0 until sentenceStarts.length) {
      // add sentence
      new Sentence(section, sentenceStarts(i), sentenceLens(i))
    }

    section
  }

  def serializeCDE(cde: cc.factorie.xcoref.CrossDocEntity): String = {
    JacksonWrapper.serialize(
      CrossDocEntity(cde.id, cde.docEntityIds, cde.refEntityId,
        cde.refMentionIds.toSeq, cde.refSlotValueIds.toSeq, cde.entityType.toString, cde.strings.toSeq, cde.freebaseId))
  }

  def deserializeCDE(string: String): cc.factorie.xcoref.CrossDocEntity = {
    val cde = JacksonWrapper.deserialize[CrossDocEntity](string)
    val result = new cc.factorie.xcoref.CrossDocEntity(cde.id)
    result.refEntityId = cde.reid
    result.docEntityIds ++= cde.deids
    result.refMentionIds ++= cde.rmids
    result.refSlotValueIds ++= cde.rsids
    result.entityType = TackbpEntityType.withName(cde.et)
    result.strings ++= cde.strings
    result.freebaseId_(cde.freebaseID)
    result
  }

  def serialize(cdcs: CrossDocEntities, outputFilename: String): Unit = {
    val writer = new PrintWriter(new OutputStreamWriter(
      new GZIPOutputStream(new FileOutputStream(outputFilename)), "UTF-8"))
    for (cde <- cdcs.crossDocEntities) {
      val str = serializeCDE(cde)
      writer.println(str)
    }
    writer.flush()
    writer.close()
  }

  def deserializeCDEs(inputFilename: String): Iterator[cc.factorie.xcoref.CrossDocEntity] = {
    val reader = scalax.io.Resource.fromInputStream(new GZIPInputStream(new FileInputStream(inputFilename)))
    reader.reader(Codec.UTF8).lines().map(l => deserializeCDE(l)).toIterator
  }
}

object DEFTJsonChecker {
  def main(args: Array[String]) {
    println(System.getProperty("file.encoding"))
    val docPath = "/home/aravind/UMASS/IndependentStudy/tackbp/source/json/el/el.docs.comb.gz"
    val docs = DEFTJson.deserializeFile(docPath)
    println(docs.length)
  }
}