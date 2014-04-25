package cc.factorie.xcoref

import io.Source
import scala.xml.{Elem, XML}
import java.io._
import scala.collection.mutable
import java.util.zip.GZIPInputStream
import scala.collection.mutable.{HashMap, HashSet, ArrayBuffer}
import scala.Some

/**
 * Loading tac kbp annotation to Ref KB
 */
object Loader {

  def fillEntityData(refKB: RefKB, dirName: String) = {
    loadRefKB(refKB, dirName, false)
  }

  def loadRefKB(name: String, dirName: String): RefKB = {
    val refKB = new RefKB(name)
    loadRefKB(refKB, dirName, true)
  }

  def loadRefKB(refKB: RefKB, dirName: String, createEntities: Boolean): RefKB = {
    val dir = new File(dirName)
    assert(dir.isDirectory, dirName + " is not a directory!")
    for (xmlFile <- dir.listFiles(new FileFilter {
      def accept(p1: File) = p1.getName.endsWith(".xml")
    })) {
      println("-- Reading file: " + xmlFile.getName)
      val article = XML.loadFile(xmlFile)
      val kbElem = article \\ "knowledge_base"
      assert(kbElem.size == 1, "number of kb elems: " + kbElem.size)
      for (entity <- kbElem \ "entity") {
        val id = (entity \ "@id").text
        val entityType = (entity \ "@type").text
        val name = (entity \ "@name").text
        val wikiTitle = (entity \ "@wiki_title").text
        val wikiText = (entity \ "wiki_text").text

        val eopt = refKB.entityOpt(id)
        if (eopt.isDefined || createEntities) {
          val refEntity = eopt.getOrElse({
            assert(createEntities)
            refKB.entity(id, new RefEntity(id, TackbpEntityType.withName(entityType)))
          })

          val slots = new ArrayBuffer[RefSlot]
          val factsClass = (entity \ "facts" \ "@class").text
          for (fact <- entity \\ "fact") {
            val name = (fact \ "@name").text
            val value = new XmlValue(fact.asInstanceOf[Elem]) //new StringValue(fact.child.head.toString)
            val slot = new RefSlot(None, StringSlotName(name), refEntity, value, DataSource.reference)
            slots += slot
            refKB += slot
          }

          val refEntityData = new RefEntityData(name, wikiTitle, wikiText, factsClass, slots)
          if (!refEntity.refData.isEmpty) println("WARNING: replacing data in " + refEntity.entityId)
          refEntity.refData = refEntityData
        }
      }
    }
    refKB
  }

  private def readMap[A, B](filename: String, parseA: String => A, parseB: String => B, filterA: (A) => Boolean = (a: A) => true, posA: Int = 0, posB: Int = 1): mutable.HashMap[A, B] = {
    val map = new mutable.HashMap[A, B]
    val reader = new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(filename))))
    var line = ""
    while ( {
      line = reader.readLine();
      line != null
    }) {
      val split = line.split("\t")
      if (split.length < 2) {
        //println("split length < 2 : {" + line + "}")
      } else {
        val a = parseA(split(posA))
        val b = parseB(split(posB))
        if (map.contains(a)) {
          println("{" + a + "} already in map with " + map(a) + ", ignoring " + b)
        } else if (filterA(a)) {
          assert(!map.contains(a))
          map(a) = b
        }
      }
    }
    reader.close()
    map
  }

  def readWikipediaData(kb: RefKB,
                        idToRefFile: String = "/iesl/canvas/sameer/tackbp/resources/wikipedia/tac-wiki-mapping.gz"): Unit = {
    // refEntityId -> [wpId, wpTitle]
    println("loading wpid <-> ref map")
    val refToIdMap: mutable.HashMap[String, Int] = readMap(idToRefFile, _.toString, _.toInt, posA = 3, posB = 0)
    val refToTitleMap: mutable.HashMap[String, String] = readMap(idToRefFile, _.toString, _.toString, posA = 3, posB = 2)

    // go through refEntity
    println("linking entities")
    for (e <- kb.entities; d <- e.refData) {
      // get wpId
      val wpid = refToIdMap.get(e.id)
      // get wikipediaTitle
      val title = refToTitleMap.get(e.id)
      d.wikipediaId = wpid //Some(wpid)
      d.canonicalTitle = title //Some(title)
      d.normalizedCanonicalWikiTitle = title.map(t => normalizeTitle(t))
      //if (wpid == None || title == None) println("Warning: could not find Wikipedia title for entity: " + e.id)
      //println(s"linked ${d.wikiTitle} to ${title}")
    }

    refToIdMap.clear
    refToTitleMap.clear
  }

  def normalizeTitle(t: String): String = {
    //t.replaceAll(" ","_").replaceAll("[^a-zA-Z_]+","")
    //t.replaceAll("[^a-zA-Z]+","")
    t
  }

  def loadELQueries(file: String, kb: RefKB, source: DataSource.Value): HashSet[String] = {
    val docs = new HashSet[String]
    val filename = new File(file).getName.dropRight(4)
    println(filename)
    val article = XML.loadFile(file)
    var nilId = 0
    for (query <- article \\ "kbpentlink" \\ "query") yield {
      val id = (query \ "@id").text
      val name = (query \ "name").text
      val docName = (query \ "docid").text
      val beg = (query \ "beg")
      val end = (query \ "end")
      assert(beg.isEmpty == end.isEmpty)
      val offsets = if (beg.isEmpty || end.isEmpty) None else Some(beg.text.toInt, end.text.toInt)
      val mention = new RefMention(id, name, docName, offsets, source)

      // Also, entity might be sometimes missing, sometimes not
      val nodeId = (query \ "entity")
      if (!nodeId.isEmpty) {
        val normNodeId = if (nodeId.text.startsWith("NIL")) {
          if (nodeId.text == "NIL") {
            nilId += 1
            nodeId.text + ":" + filename + ":%04d".format(nilId)
          } else nodeId.text + ":" + filename
        } else nodeId.text
        val entity = kb.entity(normNodeId, new RefEntity(normNodeId, TackbpEntityType.UKN))
        mention.entity = entity
      }
      kb += mention
      docs += docName
    }
    docs
  }

  def loadELAnnotations(annotationFile: String, kb: RefKB, source: DataSource.Value) = {
    val stream = Source.fromFile(annotationFile)
    val filename = new File(annotationFile).getName.dropRight(4)
    var nilId = 0

    for (line <- stream.getLines()) {
      val split = line.split("\t")
      assert(split.length == 3 || split.length == 5)
      val mid = split(0)
      val eid = split(1)
      val typeStr = split(2)
      val refMention = kb.mention(mid)
      assert(refMention != null)
      // update entity and entityType for refMention
      if (refMention.entity.isDefined) {
        // annotation is already loaded
        if (!eid.startsWith("NIL")) assert(refMention.entity.get.id == eid)
        // otherwise do nothing
        if (refMention.entity.get.entityType == TackbpEntityType.UKN) refMention.entity.get.entityType = TackbpEntityType.withName(typeStr)
        else if (refMention.entity.get.entityType != TackbpEntityType.withName(typeStr)) {
          println(s"Type of entity ${refMention.entity.get.id}:${refMention.entity.get.refData.map(_.wikiTitle).getOrElse("NIL")} is ${refMention.entity.get.entityType}, which is different from ${TackbpEntityType.withName(typeStr)} predicted by $mid in $filename.")
        }
      } else {
        // annotation not loaded, create new entity
        val normEId = if (eid.startsWith("NIL")) {
          if (eid == "NIL") {
            nilId += 1
            eid + ":" + filename + ":%04d".format(nilId)
          } else eid + ":" + filename
        } else eid
        val refEntity = kb.entity(normEId, new RefEntity(normEId, TackbpEntityType.withName(typeStr)))
        refMention.entity = refEntity
      }
      refMention.entityType = TackbpEntityType.withName(typeStr)
    }
    stream.close
  }

  def removeUnannotatedELMentions(kb: RefKB) = {
    // delete all annotation queries that were never annotated
    val toRemove = new ArrayBuffer[RefMention]
    for (m <- kb.elQueries) {
      if (m.entity.isEmpty) toRemove += m
    }
    println("Going to remove " + toRemove.size + " mentions.")
    toRemove.foreach(m => kb -= m)
  }

  def readDocNames(filename: String): Seq[String] = {
    readDocNames(filename, filename.endsWith(".gz"))
  }

  def readDocNames(filename: String, gzip: Boolean): Seq[String] = {
    val stream = if (gzip) new GZIPInputStream(new FileInputStream(filename)) else new FileInputStream(filename)
    val source = Source.fromInputStream(stream)
    val names = new mutable.LinkedHashSet[String]
    for (line <- source.getLines()) {
      names += line.trim
    }
    source.close()
    names.toSeq
  }

  def deserializeWikipediaEntities(file: String, populateNameBags: Boolean = true, populateMentionBags: Boolean = true, populateContextBags: Boolean = true, onlyK: Option[Int] = None): Iterator[TACKBEntity] = {
    println("Deserializing Wikipedia mentions from file " + file)
    var i = 0
    var jacksonWrapper = new PermGenFriendlyJacksonWrapper
    new Iterator[TACKBEntity] {
      val reader = new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(file))))
      var line = reader.readLine

      def hasNext = line != null && (!onlyK.isDefined || (onlyK.isDefined && i <= onlyK.get))

      def next() = {
        val e = deserializeEntityFromJSON(line, populateNameBags, populateMentionBags, populateContextBags, jacksonWrapper)
        line = reader.readLine
        i += 1
        //println("l:"+line)
        if (i % 1000 == 0) {
          jacksonWrapper = new PermGenFriendlyJacksonWrapper
          print(".")
        }
        if (i % 50000 == 0) println(i + " lines.")
        e
      }
    }
  }

  protected def deserializeEntityFromJSON(jsonString: String, populateNameBags: Boolean = true, populateMentionBags: Boolean = true, populateContextBags: Boolean = true, jacksonWrapper: JacksonWrapper = new JacksonWrapper): TACKBEntity = {
    //class JacksonWrapper
    val serEnt = jacksonWrapper.deserialize[SerWikipediaEntity](jsonString)
    val result: TACKBEntity = CrossDocEntityUtils.newMention
    //result._id = new ObjectId(serEnt.id.getBytes)
    result._id = serEnt.id
    result.canonicalName := serEnt.name
    if (populateNameBags) result.bagOfNames ++= serEnt.nameBag
    if (populateMentionBags) result.bagOfMentions ++= serEnt.mentionBag
    if (populateContextBags) result.contextBag ++= serEnt.contextBag
    result.source = "wp"
    result.bagOfSources += "wp"
    result
  }

  case class SerWikipediaEntity(id: String,
                                name: String,
                                nameBag: HashMap[String, Double],
                                mentionBag: HashMap[String, Double],
                                contextBag: HashMap[String, Double])

  def main(args: Array[String]): Unit = {

    val baseDir = "/home/aravind/UMASS/IndependentStudy/tackbp/"

    val kb = new RefKB("test")

    // wikipedia and freebase linking
    readWikipediaData(kb)

    println("Done")
  }
}

class PermGenFriendlyJacksonWrapper extends JacksonWrapper {
  mapper.getFactory().disable(com.fasterxml.jackson.core.JsonFactory.Feature.INTERN_FIELD_NAMES)
  mapper.getFactory().disable(com.fasterxml.jackson.core.JsonFactory.Feature.CANONICALIZE_FIELD_NAMES)
}
