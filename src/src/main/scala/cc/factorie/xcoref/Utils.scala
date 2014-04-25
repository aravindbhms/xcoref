package cc.factorie.xcoref

import cc.factorie.app.nlp.hcoref._
import java.io._
import scala.collection.mutable._
import java.util.zip.{GZIPOutputStream, GZIPInputStream}
import cc.factorie.app.topics.lda.LDA
import cc.factorie.app.nlp.{Token, Document}
import cc.factorie._
import cc.factorie.directed.DirectedModel
import cc.factorie.variable.CategoricalSeqDomain
import scala.Some
import scala.Iterable
import scala.Seq
import cc.factorie.app.nlp.lexicon.StopWords
import org.bson.types.ObjectId
import cc.factorie.app.nlp.segment.DeterministicTokenizer
import scala.Some
import cc.factorie.util.coref.GenericEntityMap

object DBUtils {
  def compressId(ido: Any): String = {
    ido match {
      case id: ObjectId => oid2bitstring(id)
      case _ => ido.toString
    }
  }

  def oid2bitstring(oid: ObjectId): String = oid.toByteArray.mkString("")
}

object EntityUtils {
  val shortDecimal = new java.text.DecimalFormat("0.0#")

  def makeSingletons[E <: HierEntity](entities: Seq[E]): Seq[E] = {
    for (e <- entities)
      e.setParentEntity(null)(null)
    entities.filter(_.isObserved).toSeq
  }

  //todo: move here from bib?

  //  def collapseOn[E <: HierEntity](entities: Seq[E], collapser: E => Option[String], newEntity: () => E, propagater: E => Unit): Seq[E] = {
  //    val result = new ArrayBuffer[E]
  //    result ++= makeSingletons(entities)
  //    val key2entities = new HashMap[String, ArrayBuffer[E]]
  //    for (e <- entities.filter(collapser(_) != None)) {
  //      val rep = collapser(e).get
  //      key2entities.getOrElse(rep, {
  //        val r = new ArrayBuffer[E]
  //        key2entities(rep) = r
  //        r
  //      }) += e
  //    }
  //    for ((label, trueCluster) <- key2entities) {
  //      if (trueCluster.size > 1) {
  //        val root = newEntity()
  //        result += root
  //        for (e <- trueCluster) {
  //          this.linkChildToParent(e, root)(null)
  //          //e.setParentEntity(root)(null)
  //          //for(bag <- e.attr.all[BagOfWordsVariable])root.attr(bag.getClass).add(bag.value)(null)
  //        }
  //        propagater(root)
  //        //root.fullName.setFullName(trueCluster.head.fullName)(null)
  //      }
  //    }
  //    result
  //  }

  def collapseOnTruth(entities: Seq[KBEntity]) = cc.factorie.app.bib.EntityUtils.collapseOn[KBEntity](entities, (e: KBEntity) => {
    if (e.bagOfTruths.size == 1) Some(e.bagOfTruths.value.iterator.next._1) else None
  }, () => new KBEntity, (e: KBEntity) => {})

  //TODO: Two varying implementations of linkChildToParent... Check why!!!!

  //  def linkChildToParent(child: Entity, parent: Entity)(implicit d: DiffList): Unit = {
  //    if (child.parentEntity != null) propagateRemoveBag(child, child.parentEntity)
  //    child.setParentEntity(parent)(d)
  //    if (parent != null) propagateBagUp(child)(d)
  //  }

  def linkChildToParent(child: Entity, parent: Entity): Unit = {
    child.setParentEntity(parent)(null)
    propagateBagUp(child)(null)
  }

  def normalizeString(str: String): String = {
    str.toLowerCase.replaceAll( """[^\p{L}0-9\s\.']""", " ").replaceAll( """\s\s+""", " ").replaceAll( """\.""", "_").trim
  }

  //utils for inference
  def createBagsForMergeUp(e1: Entity, e2: Entity, parent: Entity)(implicit d: DiffList): Unit = {
    for (bag <- e1.attr.all[BagOfWordsVariable]) parent.attr(bag.getClass).add(bag.value)(d)
    for (bag <- e2.attr.all[BagOfWordsVariable]) parent.attr(bag.getClass).add(bag.value)(d)
    parent.attr[MentionCountVariable].set(parent.attr[MentionCountVariable].value + e1.attr[MentionCountVariable].value)(d)
    parent.attr[MentionCountVariable].set(parent.attr[MentionCountVariable].value + e2.attr[MentionCountVariable].value)(d)
  }

  def propagateBagUp(entity: Entity)(implicit d: DiffList): Unit = {
    var e = entity.parentEntity
    while (e != null) {
      e.attr[MentionCountVariable].set(e.attr[MentionCountVariable].value + entity.attr[MentionCountVariable].value)(d)
      for (bag <- entity.attr.all[BagOfWordsVariable])
        e.attr(bag.getClass).add(bag.value)(d)
      e = e.parentEntity
    }
  }

  def propagateRemoveBag(parting: Entity, formerParent: Entity)(implicit d: DiffList): Unit = {
    var e = formerParent
    while (e != null) {
      e.attr[MentionCountVariable].set(e.attr[MentionCountVariable].value - parting.attr[MentionCountVariable].value)(d)
      for (bag <- parting.attr.all[BagOfWordsVariable])
        e.attr(bag.getClass).remove(bag.value)(d)
      e = e.parentEntity
    }
  }

  //utils for diagnostics
  def prettyPrint[E <: HierEntity](entities: Seq[E], printSingletons: Boolean = false, out: PrintStream = System.out): Unit = {
    var count = 0
    var numSingletons = 0
    var singletons = new ArrayBuffer[E]
    val sizeDist = new HashMap[Int, Int]
    for (e <- entities.filter((e: E) => {
      e.isRoot && e.isConnected
    })) {
      if (!e.isObserved) prettyPrintEntity(e, None, out) else singletons += e
      val size = e.numLeaves
      sizeDist(size) = sizeDist.getOrElse(size, 0) + 1
      count += 1
    }
    if (printSingletons) {
      out.println("\n\n------SINGLETONS-----")
      out.println("Printing singletons")
      for (e <- singletons) prettyPrintEntity(e, None, out)
    }
    out.println("\nEntity size distribution")
    val sorted = sizeDist.toList.sortBy(_._2).reverse
    out.println(sorted)
    out.println("\nPrinted " + count + " entities  (" + singletons.size + " singletons).")
  }

  def sizeHistogram[E <: HierEntity](entities: Seq[E]): Seq[(Int, Int)] = {
    val sizeDist = new HashMap[Int, Int]
    for (e <- entities.filter((e: E) => {
      e.isRoot && e.isConnected
    })) {
      val size = e.numLeaves
      sizeDist(size) = sizeDist.getOrElse(size, 0) + 1
    }
    sizeDist.toList.sortBy(_._2).reverse
  }

  def prettyPrintEntity[E <: HierEntity](e: E, markOpt: Option[E], out: PrintStream = System.out): Unit = {
    val authorString = entityStringPretty(e, markOpt,
      (e: Entity) => {
        var result: String = "num-leaves:" + e.numLeaves + " id:" + e.id.toString + ";name:" + e.string + " wurl: " + e.asInstanceOf[KBEntity].wikiUrl + " (aliases:" + bagToString(e.attr[BagOfNamesVar].value) + ")"
        if (e.asInstanceOf[KBEntity].groundTruth != None) result = "truth:" + e.asInstanceOf[KBEntity].groundTruth.get + ";" + result
        //if(e.childEntitiesSize>1)result = "purity="+shortDecimal.format(purity(e))+";"+result
        result
      },
      Some(defaultFeaturesToPrint(_))
    )
    out.println(authorString)
    //println(authorString)
  }

  def entityStringPretty(e: Entity, markOpt: Option[Entity], flatRepresent: Entity => String, featuresToPrint: Option[Entity => Seq[String]], perLevelIndent: String = "   ", result: StringBuffer = new StringBuffer, depth: Int = 0): String = {
    var levelIndent = {
      var r = ""
      for (i <- 0 until depth) r += perLevelIndent
      r
    }
    if (markOpt.isDefined && (e eq markOpt.get)) levelIndent += "*-->"
    result.append("\n" + levelIndent)
    if (e.isRoot) {
      result.append("EntityRoot[" + flatRepresent(e) + "]")
      if (featuresToPrint != None) result.append("\n" + levelIndent + "| Features\n" + levelIndent + "|   ")
    } else if (e.isObserved) {
      result.append("-Mention[" + flatRepresent(e) + "]")
      if (featuresToPrint != None) result.append("\n" + levelIndent + "|   ")
    } else {
      result.append("*SubEntity[" + flatRepresent(e) + "]")
      if (e.childEntitiesSize == 0) result.append("-SUBENTITY ERROR") //throw new Exception("ERROR SUB ENTITY IS EMPTY")
      if (featuresToPrint != None) result.append("\n" + levelIndent + "| Features\n" + levelIndent + "|   ")
    }
    for (featuresToPrintFunc <- featuresToPrint) result.append(featuresToPrintFunc(e).mkString("\n" + levelIndent + "|   "))
    if (e.childEntitiesSize > 0) result.append("\n" + levelIndent + " \\Children (" + e.childEntitiesSize + ")")
    for (childEntity <- e.childEntitiesIterator) entityStringPretty(childEntity, markOpt, flatRepresent, featuresToPrint, perLevelIndent, result, depth + 1)
    result.toString
  }

  def bagToString(bag: BagOfWords, k: Int = 8): String = {
    val map = new HashMap[String, Double]
    for ((k, v) <- bag.iterator) map += k -> v
    topk(map, k)
  }

  def topk(bag: HashMap[String, Double], k: Int = 18): String = {
    val result = new StringBuffer
    val sorted = bag.toList.sortBy(_._2).reverse.take(k)
    for (i <- 0 until sorted.length) {
      result.append(sorted(i)._1 + "->" + shortDecimal.format(sorted(i)._2))
      if (i < sorted.length - 1)
        result.append(", ")
    }
    result.toString
  }

  def defaultFeaturesToPrint(e: Entity): Seq[String] = {
    val result = new ArrayBuffer[String]
    for (bagVar <- e.attr.all[BagOfWordsVariable]) {
      val bag = bagVar.value
      if (bag.size > 0) {
        val name = bagVar.getClass.getName.toString.split("\\.").toSeq.takeRight(1)(0).replaceAll("[A-Za-z]+\\(", "").replaceAll("\\)", "")
        result += name + "(" + bag.size + "): [" + this.bagToString(bag, 8) + "]"
      }
    }
    result
  }
}

object LDAUtils {

  object WordSeqDomain extends CategoricalSeqDomain[String]

  val model = DirectedModel()

  def bags2tokens(bag: BagOfWordsVariable): Seq[String] = bag.value.iterator.map(_._1).toSeq

  def getTopicMapKey(e: KBEntity) = if (e.source == "wp") e.canonicalName.value else e.id.toString

  def createTopics(entities: Iterator[KBEntity], phiFile: String, thetaFile: String, numTopics: Int = 400, numIterations: Int = 0, diagnostic: Int = 10): Unit = {
    implicit val random = new scala.util.Random(0)
    val lda = new LDA(WordSeqDomain, numTopics, 0.1, 0.01, 1)(model, random)
    val stopWords = cc.factorie.app.nlp.lexicon.StopWords.contents //generateStopWordsFromIDF(idfFile,percentageThreshold)
    println("Found " + stopWords.size + " stop words")
    val namesUsed = new HashSet[String]
    var numDocs = 0
    var numAdded = 0
    println("Adding lda docs")
    for (doc <- entities) {
      val tokens = kbe2document(doc, stopWords)._2
      val name = doc.id.toString
      println("nt: " + tokens.size)
      //val tokens = (doc.bagOfNames.value.iterator.map(_._1) ++ doc.contextBag.value.iterator.map(_._1) ++ doc.bagOfMentions.value.iterator.map(_._1)).toSeq
      if (!namesUsed.contains(name)) {
        namesUsed += name
        if (tokens.length >= 3) {
          lda.addDocument(new cc.factorie.app.topics.lda.Document(WordSeqDomain, name, tokens.toIndexedSeq), random)
          numAdded += 1
        }
      } else println("Warning: name " + name + " already in use, skipping document.")
      numDocs += 1
      if (numDocs % 1000 == 0) print("."); if (numDocs % 50000 == 0) println(numDocs)
    }
    println("Num docs: " + numDocs)
    println("Num added: " + numAdded)
    println("Num LDA docs: " + lda.documents.size)
    lda.inferTopics(numIterations, Int.MaxValue, 10, false)
    //lda.inferTopicsMultithreaded(numThreads, numIterations, diagnosticInterval = diagnostic, diagnosticShowPhrases = false)
    saveAlphaAndPhi(lda, new File(phiFile))
    writeLDADocuments(lda, new File(thetaFile))
  }

  def kbe2document(kbe: KBEntity, stopWords: HashSet[String]): (String, Seq[String]) = {
    val name = getTopicMapKey(kbe)
    val tokens = bags2tokens(kbe.bagOfNames) ++ bags2tokens(kbe.bagOfMentions) ++ bags2tokens(kbe.contextBag).filterNot(s => stopWords.contains(s))
    (name, tokens)
  }

  protected def getThetasForDoc(doc: cc.factorie.app.topics.lda.Doc, size: Int): Array[Double] = {
    //phis.foreach(_.value.zero())
    val result = new Array[Double](size)
    val len = doc.ws.length
    var i = 0
    while (i < len) {
      val zi = doc.zs.intValue(i)
      //phis(zi).value.masses.+=(doc.ws.intValue(i), 1.0)
      result(zi) += 1.0
      //doc.theta.value.masses.+=(zi, 1.0)
      i += 1
    }
    var norm = 0.0
    for (e <- result) norm += e
    for (i <- 0 until result.size) result(i) = result(i) / norm
    result
  }

  def saveAlphaAndPhi(lda: LDA, file: File) {
    val pw = new PrintWriter(new GZIPOutputStream(new FileOutputStream(file))) //new PrintWriter(file)
    pw.println(lda.phis.size)
    pw.println("/alphas")
    pw.println(lda.alphas.value.mkString(" "))
    pw.println()
    for (phi <- lda.phis) {
      pw.println("/topic")
      phi.value.foreachActiveElement((index, count) => {
        val word: String = lda.wordSeqDomain.elementDomain.category(index)
        if (count != 0.0) {
          pw.println(word)
          pw.println(count)
        }
      })
    }
    pw.close()
  }

  def loadLDAModelFromAlphaAndPhi(file: File): LDA = {
    println("Loading topic model from phis and alphas.")
    implicit val random = new scala.util.Random(0)
    val reader = new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(file))))
    val numTopics = reader.readLine.toInt
    val lda = new LDA(WordSeqDomain, numTopics)(model, random)
    lda.phis.foreach(_.value.zero())
    reader.mark(512)
    val alphasName = reader.readLine()
    if (alphasName == "/alphas") {
      // If they are present, read the alpha parameters.
      val alphasString = reader.readLine()
      lda.alphas.value := alphasString.split(" ").map(_.toDouble) // set lda.alphas
      reader.readLine() // consume delimiting newline
      println("Read alphas " + lda.alphas.value.mkString(" "))
    }
    var line = reader.readLine()
    var topicCount = -1
    var lineCount = 0
    val start = System.currentTimeMillis
    while (line != null) {
      if (line == "/topic") {
        topicCount += 1
        line = reader.readLine
      }
      lineCount += 1
      if (lineCount % 1000 == 0) print(lineCount + " ")
      if (lineCount % 25000 == 0) println("  time: " + ((System.currentTimeMillis - start) / 1000L))
      val word = line
      val count = reader.readLine.toDouble
      //lda.phis(topicCount).tensor.masses.+=(lda.wordDomain.index(word),count)
      lda.phis(topicCount).value.masses.+=(lda.wordDomain.index(word), count)
      line = reader.readLine
    }
    println("Topics: \n" + lda.topicsSummary(10))
    lda
  }

  protected def writeLDADocuments(lda: LDA, file: File) {
    val writer = new PrintWriter(new GZIPOutputStream(new FileOutputStream(file)))
    var numWritten = 0
    for (document <- lda.documents) {
      writeDocument(lda, document, writer)
      numWritten += 1
    }
    println("Wrote " + numWritten + " LDA docs.")
    writer.close()
  }

  protected def writeDocument(lda: LDA, document: cc.factorie.app.topics.lda.Doc, pw: PrintWriter) {
    val topics = getThetasForDoc(document, lda.phis.size).toSeq.zip(0 until lda.phis.size).filter((t: (Double, Int)) => {
      t._1 > 0.05
    }) //.foreach((t:(Double,Int))=>{paper.bagOfTopics.add(t._2+"",t._1)(null)})
    //val topics = document.theta.value.toSeq.zip(0 until lda.phis.size).filter((t:(Double,Int))=>{t._1>0.05})//.foreach((t:(Double,Int))=>{paper.bagOfTopics.add(t._2+"",t._1)(null)})
    val string = document.name + "\t" + topics.map(_._2).mkString(" ") + "\t" + topics.map(_._1).mkString(" ")
    pw.println(string)
  }

  def loadTopics(file: String, optionalKeys: Option[HashSet[String]] = None): HashMap[String, LinkedHashMap[String, Double]] = {
    println("Loading topics.")
    if (optionalKeys.isDefined) {
      //for(k <- optionalKeys.get)println("k: "+k)
      println("Using a preknown set of keys to filter out map (size=" + optionalKeys.get.size + ").")
    }
    val result = new HashMap[String, LinkedHashMap[String, Double]]
    val reader = new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(new File(file)))))
    var line: String = ""
    var lineCount = 0
    while ( {
      line = reader.readLine();
      line != null
    }) {
      val tuple = line.split("[\t]")
      if ((optionalKeys.isDefined && optionalKeys.get.contains(tuple(0))) || !optionalKeys.isDefined) {
        assert((tuple.length == 3 || tuple.length == 1), "{%s} is %d in length" format(line, tuple.length))
        val bag = new LinkedHashMap[String, Double]
        result += tuple(0) -> bag
        if (tuple.length == 3) {
          val topics = tuple(1).split(" ")
          val weights = tuple(2).split(" ").map(_.toDouble)
          assert(topics.length == weights.length, "error: topics and weights length differs")
          var i = 0
          while (i < topics.size) {
            bag(topics(i)) = weights(i)
            i += 1
          }
        }
      }
      if (lineCount % 10000 == 0) print(".");
      if (lineCount % 500000 == 0) println(lineCount)
      lineCount += 1
      //numDocs += 1
    }
    println("Loaded: " + result.size + " topic thetas.")
    result
  }
}

object KBEntityUtils {
  def defaultIDFFile: String = defaultWLIDFFile

  var defaultWPIDFFile: String = "resources/idf.wikipedia.tsv"
  var defaultWLIDFFile: String = "resources/idf.wikilinks.tsv"
  val defaultTopicFile: String = "/iesl/canvas/proj/wikicoref/topics/lda.wpwl.docs.theta.gz"

  //lazy val defaultIDFStatistics = loadIDFFrequencyStatistics(new java.io.File(idfFileName))
  //lazy val (numDocs,idfc) = loadIDFFrequencyStatistics(new java.io.File("resources/idf.tsv"))
  def makeFilteredTopicMap(originalMapFile: String, filteredMapFile: String, entities: Iterable[KBEntity]): Unit = {
    val keys = new HashSet[String]
    keys ++= entities.map(e => LDAUtils.getTopicMapKey(e))
    val filteredMap = loadTopics(originalMapFile, Some(keys))
    val writer = new PrintWriter(new GZIPOutputStream(new FileOutputStream(filteredMapFile)))
    for ((k, v) <- filteredMap) {
      writer.println(k + "\t" + v.keys.mkString(" ") + "\t" + v.values.mkString(" "))
    }
    writer.close()
  }

  def loadTopics(file: String, optionalKeys: Option[HashSet[String]] = None): HashMap[String, LinkedHashMap[String, Double]] = LDAUtils.loadTopics(file, optionalKeys)

  def loadIDFFrequencyStatistics(file: java.io.File): (Double, HashMap[String, Double]) = loadIDFFrequencyStatistics(new FileInputStream(file))

  def loadIDFFrequencyStatistics(in: java.io.InputStream): (Double, HashMap[String, Double]) = {
    val map = new HashMap[String, Double]
    val reader = new BufferedReader(new InputStreamReader(in, "UTF-8"))
    //var numDocs = 0.0
    val numDocs = reader.readLine.toDouble
    var line: String = ""
    while ( {
      line = reader.readLine();
      line != null
    }) {
      val pair = line.split("[\t]")
      assert(pair.length == 2, "{%s} is %d in length" format(line, pair.length))
      map += pair(0) -> pair(1).toDouble
      //numDocs += 1
    }
    reader.close()
    (numDocs, map)
  }

  def idfFrequencyStatisticsMap(entities: Seq[KBEntity]): HashMap[String, Double] = {
    val result = new HashMap[String, Double]
    for (e <- entities) {
      val set = (e.bagOfNames.value.iterator ++ e.bagOfMentions.value.iterator ++ e.contextBag.value.iterator).map(_._1).toSet
      for (token <- set) result(token) = result.getOrElse(token, 0.0) + 1.0
    }
    result
  }

  def tfidfify(bag: BagOfWordsVariable, idfCounts: HashMap[String, Double], numDocs: Double): Unit = {
    if (bag.size > 0) {
      val maxWeight = bag.iterator.toSeq.sortBy(_._2).reverse.map(_._2).head
      val newWeights = new ArrayBuffer[(String, Double)]
      for ((word, weight) <- bag.iterator) newWeights += word -> (weight / maxWeight * scala.math.log(numDocs / idfCounts.getOrElse(word, 1.0)))
      bag.clear
      for ((word, weight) <- newWeights) bag.add(word, weight)(null)
    }
  }

  /*
    def propagateBagUp(entity:Entity)(implicit d:DiffList):Unit ={
    var e = entity.parentEntity
    while(e!=null){
      val evar = e.attr[EditSetVariable]
      if(evar!=null)for(edit <- entity.attr[EditSetVariable])evar.add(edit)(d)
      //entity.attr[EditSetVariable].value.foreach(evar.add(_)(d))
      e.attr[MentionCountVariable].set(e.attr[MentionCountVariable].value + entity.attr[MentionCountVariable].value)(d)
      for(bag <- entity.attr.all[BagOfWordsVariable])
        e.attr(bag.getClass).add(bag.value)(d)
      e=e.parentEntity
    }
  }
   */

  /*
  def featureRefinementPipeline2(entity:KBEntity, idfMaps:HashMap[Class,(Double,HashMap[String,Double]])) ={
    removeNamesFromMentionsAndContext(entity)
    for(bag <- entity.attr.all[BagOfWordsVariable]){
      val idfOpt = idfMaps.get(bag.getClass)
      for(idf<-idfOpt)compressBagsWithTFIDF(bag,idf._1,idf._2)
    }
    removeTypesFromMentionBags(entity)

  }
*/
  def featureRefinementPipeline(entities: Seq[KBEntity], idfOpts: Option[(Double, HashMap[String, Double])] = None) = {
    removeNamesFromMentionsAndContext(entities)
    removeTypesFromMentionBags(entities)
    for (e <- entities) tokenizeBags(e) //only tokenize the mentions
    for ((docCount, idfCounts) <- idfOpts)
      testReweightAndCompress(entities, idfCounts, docCount)
    //simplifyMentionBagTags(entities)
    for (e <- entities) populateCombinedBag(e)
  }

  def addNameToNames(entities: Seq[KBEntity]) = {
    for (e <- entities) {
      val names = EntityUtils.normalizeString(e.canonicalName.value).split(" ")
      for (name <- names) e.bagOfNames.add(name, 1.0)(null)
    }
  }

  def removeTypesFromMentionBags(entities: Seq[KBEntity]) = {
    for (e <- entities) {
      val bag = e.bagOfMentions
      if (bag.size > 0) {
        val result = new HashMap[String, Double]
        for ((word, weight) <- bag.value.iterator) {
          //val tag = if(word.indexOf(">")!= -1)word.substring(1,word.indexOf(">"))
          val content = word.replaceAll("<[^>]+>", "")
          result += content -> weight
        }
        bag.clear
        for ((word, weight) <- result) bag.add(word, weight)(null)
      }
    }
  }

  def simplifyMentionBagTags(entities: Seq[KBEntity]) = {
    for (e <- entities) {
      val bag = e.bagOfMentions
      if (bag.size > 0) {
        val result = new HashMap[String, Double]
        for ((word, weight) <- bag.value.iterator) {
          val tag = word.substring(1, word.indexOf(">"))
          val content = word.replaceAll("<[^>]+>", "")
          result += (tag + "-" + content) -> weight
        }
        bag.clear
        for ((word, weight) <- result) bag.add(word, weight)(null)
      }
    }
  }

  def removeNamesFromMentionsAndContext(entities: Seq[KBEntity]) = {
    for (e <- entities) {
      if (e.bagOfMentions.size > 0 || e.contextBag.size > 0) {
        val names = e.bagOfNames.value.iterator.toSeq.map(_._1).toSet ++ e.bagOfNames.value.iterator.toSeq.flatMap(_._1.split(" ")).toSet
        //println("NAMES: "+names)
        //filter name bag
        val newMentionBag = new ArrayBuffer[(String, Double)]
        for ((word, weight) <- e.bagOfMentions.value.iterator) if (!names.contains(word.replaceAll("<[^>]+>", ""))) newMentionBag += word -> weight
        e.bagOfMentions.clear
        for ((word, weight) <- newMentionBag) e.bagOfMentions.add(word, weight)(null)
        //filter context bag
        val newContextBag = new ArrayBuffer[(String, Double)]
        for ((word, weight) <- e.contextBag.value.iterator) if (!names.contains(word)) newContextBag += word -> weight
        e.contextBag.clear
        for ((word, weight) <- newContextBag) e.contextBag.add(word, weight)(null)
      }
    }
  }

  def testReweightAndCompress(entities: Seq[KBEntity], idfc: HashMap[String, Double], numDocs: Double) = {
    //val idfc = idfFrequencyStatisticsMap(entities)
    //val numDocs = entities.size.toDouble
    //val (numDocs,idfc) = loadIDFFrequencyStatistics(new java.io.File("/Users/mwick/data/wikicoref/resources/idf.tsv"))
    //val (numDocs,idfc) = loadIDFFrequencyStatistics(new java.io.File("resources/idf.tsv"))
    //println("IDFC.size = "+idfc.size)
    reweightAndCompressBags(entities, idfc, numDocs)
  }

  def reweightAndCompressBags(entities: Seq[KBEntity], idfCounts: HashMap[String, Double], numDocs: Double): Unit = {
    //println("REDUCING BAGS")
    for (entity <- entities) {
      /*
      Wiki coref used the following:
      /*print("Reducing bag of names: ");*/compressBagsWithTFIDF(entity.bagOfNames,idfCounts,numDocs,64*2)          //2//Integer.MAX_VALUE)//64)
      /*print("Reducing bag of mentions: ");*/compressBagsWithTFIDF(entity.bagOfMentions,idfCounts,numDocs,32*4)    //4//Integer.MAX_VALUE)//)32)
      /*print("Reducing bag of context: ");*/compressBagsWithTFIDF(entity.contextBag,idfCounts,numDocs,32*4)        //4//32*2)
       */
      //for(bag <- entity.attr[BagOfWordsVariable])EntityUtils.reduceBagTopK(bag)
      //filterNonMentions(entity.bagOfMentions)
      //AKBC paper used the following
      //compressBagsWithTFIDF(entity.bagOfNames,idfCounts,numDocs,64)          //2//Integer.MAX_VALUE)//64)
      //compressBagsWithTFIDF(entity.bagOfMentions,idfCounts,numDocs,32/2)    //4//Integer.MAX_VALUE)//)32)
      //compressBagsWithTFIDF(entity.contextBag,idfCounts,numDocs,32/4)        //4//32*2)
      compressBagsWithTFIDF(entity.bagOfNames, idfCounts, numDocs, 256) //2//Integer.MAX_VALUE)//64)
      compressBagsWithTFIDF(entity.bagOfMentions, idfCounts, numDocs, 256) //4//Integer.MAX_VALUE)//)32)
      compressBagsWithTFIDF(entity.contextBag, idfCounts, numDocs, 256) //4//32*2)

    }
  }

  def compressBagsWithTFIDF(bag: BagOfWordsVariable, idfCounts: HashMap[String, Double], numDocs: Double, topK: Int): Unit = {
    def tfidf(maxWeight: Double, bagEntry: (String, Double)): Double = {
      val bias = 0.5
      //val result = bagEntry._2/maxWeight * scala.math.log(numDocs/idfCounts.getOrElse(bagEntry._1,1.0))
      //val result = 1.0 * scala.math.log(numDocs/idfCounts.getOrElse(bagEntry._1,1.0))
      //val result = scala.math.pow(bagEntry._2,1.0)/scala.math.pow(maxWeight+1,1.0) * scala.math.log(numDocs/idfCounts.getOrElse(bagEntry._1,1.0)) //this was used in our akbc submission
      val result = (bias + (1.0 - bias) * bagEntry._2 / (maxWeight + 1.0)) * scala.math.log(numDocs / idfCounts.getOrElse(bagEntry._1, 1.0))
      /*
      println("DEBUGGING TFIDF")
      println("  word: "+bagEntry._1)
      println("  num docs: "+numDocs)
      println("  idf-count: "+idfCounts.getOrElse(bagEntry._1,1.0))
      println("  tf-count: "+bagEntry._2)
      println("  tf-num : "+ (bias + (1.0-bias)*bagEntry._2/(maxWeight+1.0)))
      println("  tf-norm: "+(maxWeight+1))
      */
      if (result < 0) {
        println("ERROR: TFIDF should be >= 0")
        println("  offending word: " + bagEntry._1)
        println("  num docs: " + numDocs)
        println("  idf-count: " + idfCounts.getOrElse(bagEntry._1, 1.0))
        println("  tf-count: " + bagEntry._2)
        println("  tf-norm: " + maxWeight + 1)
      }
      //val result = scala.math.log(bagEntry._2+1)/scala.math.log(maxWeight+1) * scala.math.log(numDocs/idfCounts.getOrElse(bagEntry._1,1.0))
      //if(bagEntry._1 == "the c's"){
      //  println("word: "+bagEntry._1+" weight: "+bagEntry._2+" maxWeight "+maxWeight)
      //  println("  tf: "+(bagEntry._2/maxWeight)+" idf: "+scala.math.log(numDocs/idfCounts.getOrElse(bagEntry._1,1.0))+" tfidf: "+result)
      //}
      result
    }
    if (bag.size > 0) {
      //val normalizer = bag.value.iterator.toSeq.sortBy(_._2).reverse.head._2
      var normalizer = 0.0
      //for(w<-bag.value.iterator.map(_._2))normalizer += w
      normalizer = bag.value.iterator.maxBy(_._2)._2
      compressBagTopK(bag, topK, tfidf(normalizer, _: (String, Double)))
    }
  }

  def filterNonMentions(bag: BagOfWordsVariable): Unit = {
    if (bag.size > 0) {
      val result = new ArrayBuffer[(String, Double)]
      for ((word, weight) <- bag.value.iterator) if (word.startsWith("<") && word.endsWith(">")) result += word -> weight
      bag.clear
      for ((word, weight) <- result) bag.add(word, weight)(null)
    }
  }

  def compressBagTopK(bag: BagOfWordsVariable, k: Int, scoreBagEntry: (Pair[String, Double]) => Double): Unit = {
    if (bag.size > 0) {
      val topk = bag.value.iterator.toSeq.sortBy(scoreBagEntry(_)).reverse.take(k)
      //if(topk.size>=2){
      //  println(" TOP2: 1st:"+topk(0)._1+"->"+scoreBagEntry(topk(1))+"  2nd:"+topk(1)._1+"->"+scoreBagEntry(topk(1)))
      //} else println()
      bag.clear
      for ((word, weight) <- topk) if (weight > 0.0001) bag.add(word, scoreBagEntry((word -> weight)))(null)
    }
  }

  def tokenizeBags(e: KBEntity): Unit = {
    //tokenizeBag(e.bagOfMentions)
    tokenizeBag(e.bagOfNames)
    //tokenizeBag(e.contextBag)
  }

  def tokenizeBag(bag: BagOfWordsVariable): Unit = {
    if (bag.size > 0) {
      val newTokens = new HashMap[String, Double]
      for ((k, v) <- bag.value.iterator) {
        val toks = k.split(" ")
        if (toks.length > 1) {
          for (t <- toks)
            newTokens(t) = newTokens.getOrElse(t, 0.0) + v / toks.length.toDouble
        }
      }
      for ((k, v) <- newTokens) bag.add(k, v)(null)
    }
  }

  def populateCombinedBag(e: KBEntity, k: Int = 4): Unit = {
    if (e.bagOfMentions.size > 0 || e.contextBag.size > 0) {
      val combined = new HashMap[String, Double]
      val combinedBag = e.combinedBag
      for ((k, v) <- e.bagOfMentions.value.iterator.toSeq.sortBy(_._2).reverse.take(k)) combined(k) = combined.getOrElse(k, 0.0) + v
      for ((k, v) <- e.contextBag.value.iterator.toSeq.sortBy(_._2).reverse.take(k)) combined(k) = combined.getOrElse(k, 0.0) + v
      //for((k,v) <- e.bagOfNames.value.iterator.toSeq.sortBy(_._2).reverse.take(k))combined(k) = combined.getOrElse(k,0.0) + v
      combinedBag.clear
      for ((k, v) <- combined) combinedBag.add(k, v)(null)
    }
  }
}

object KBEntityProcessingUtils {
  def getTrainableModel = {
    //todo: add a feature that looks at the number of mentions... obviously if there are more mentions and there is no overlap, then this is a strong signal that they are not coreferent.
    val thresholds = Seq.empty[Double]
    //val thresholds = Seq(0.1)
    //val thresholds = Seq(0.0,0.1,0.5)
    //val thresholds = Seq(0.0,0.01,0.1,0.2,0.3,0.4,0.5,0.9)

    val entityLinkingModel = new EntityLinkingModel
    entityLinkingModel += new ParameterizedBoW2Compatibility[NameTopicVariable](entityLinkingModel, featureConjunctions = false, bins = thresholds)
    entityLinkingModel += new ParameterizedChildParentCompatibility[MentionBagVar]("m-", entityLinkingModel, bins = thresholds)
    entityLinkingModel += new BagCompatibilityTemplate[BagOfEntityTypesVar]((bag: BagOfEntityTypesVar#Value) => new EntityCompatibilityVector("ety-", topk = Integer.MAX_VALUE, sizePenalty = true, bag), entityLinkingModel)
    //    entityLinkingModel += new BagCompatibilityTemplate[BagOfAttributesVar]((bag:BagOfAttributesVar#Value)=> new AttributeCompatibilityVector(bag),entityLinkingModel)
    entityLinkingModel += new WikipediaContraintFactor //{override val src=WikiCrossDocCoreferencer.ReferenceSource}
    entityLinkingModel += new PStructuralPriorsTemplate(entityLinkingModel)

    //entityLinkingModel += new WikiCorefPort.ParameterizedChildParentCompatibility[BagOfTruths]("cheating-",entityLinkingModel,bins=thresholds)
    //entityLinkingModel += new WikiCorefPort.DepthPenaltyTemplate(0.001,weightByLeaves=true)
    //entityLinkingModel += new SourceDocContraintFactor(2.0)
    //entityLinkingModel += new WikiCorefPort.BagCompatibilityTemplate[BagOfTopicsVar]("tbc-",topk=3,sizePenalty=false,entityLinkingModel)
    println("Getting model with " + entityLinkingModel.templates.size + " templates.")
    entityLinkingModel
  }

  def getModel: EntityLinkingModel = {
    //need joint features. For example, if both the name bag and mention bag have little overlap, there should be a bigger punishment
    val entityLinkingModel = new EntityLinkingModel
    entityLinkingModel += new ChildParentCosineDistance[CombinedBagVar](2.0 / 4.0, 0.0, false) //try 1.0 for first argument
    //entityLinkingModel += new ChildParentCosineDistance[BagOfNamesVar](3.0, -0.125, false){ //was 2.0,-0.25
    //entityLinkingModel += new ChildParentCosineDistance[BagOfNamesVar](2.0, -0.25, false){ //was 2.0,-0.25
    entityLinkingModel += new ChildParentCosineDistance[BagOfNamesVar](2.0, 0.0, false) {
      //was 2.0,-0.25
      override def score(er: EntityRef#Value, childBow: BagOfNamesVar#Value, parentBow: BagOfNamesVar#Value): Double = {
        var weightV = weight
        var shiftV = shift
        val cossim = childBow.cosineSimilarity(parentBow, childBow)

        //if(cossim<=0.125)
        val uncertaintyWeight = 1.0 //childBow.size.toDouble/(childBow.size.toDouble+0.25)
        var result = (cossim + shiftV) * weightV * uncertaintyWeight
        if (cossim <= 0.25) result = -1.0
        else if (cossim <= 0.125) result = -2.0
        else if (cossim <= 0.0625) result = -4.0
        if (_debug) println("  " + debug(result) + "  w=" + weight + " s=" + shift + " cos=" + cossim + " uncertaintyWeight: " + uncertaintyWeight)
        //else if(cossim<=0.125)result = -2.0
        result
      }
    }
    entityLinkingModel += new ChildParentCosineDistance[BagOfTopicsVar](2.0, -0.33) {
      override def score(er: EntityRef#Value, childBow: BagOfTopicsVar#Value, parentBow: BagOfTopicsVar#Value): Double = {
        var result = childBow.cosineSimilarity(parentBow, childBow)
        if (result == 0.0) result = -4.0
        else if (result < 0.125) result = -2.0 * (1.0 - result)
        else if (result <= 0.25) result = -1.0 * (1.0 - result)
        else if (result <= 0.5) result = -result
        else result *= weight
        result
      }
    }
    //entityLinkingModel += new ChildParentBow2Compatibility[ContextVariable](2.0,0.0,0.0625/4.0)
    ////entityLinkingModel += new ChildParentCosineDistance[MentionBagVar](2.0, -0.25, false)
    ////entityLinkingModel += new ChildParentCosineDistance[ContextBagVar](2.0, -0.25, false)
    entityLinkingModel += {
      val t = new StructuralPriorsTemplate(-0.5, 0.25); t.debugOff; t
    }
    entityLinkingModel += new CustomChildParentCompatibility[BagOfEntityTypesVar](1.0, -1.0)
    entityLinkingModel += new SourceDocContraintFactor(2.0)

    /*
    entityLinkingModel += new ChildParentCosineDistance[MentionBagVar](4.0, 0.0, false)
    entityLinkingModel += new ChildParentCosineDistance[MentionBagVar](0.125, 0.0, false)
    entityLinkingModel += new ChildParentCosineDistance[ContextBagVar](0.125, 0.0, false)
    entityLinkingModel += new StructuralPriorsTemplate(-0.5, 0.25)
    //entityLinkingModel += new CustomChildParentCompatibility[BagOfEntityTypesVar](1.0, -1.0)
    entityLinkingModel += new SourceDocContraintFactor(2.0)
    */

    /*
    entityLinkingModel += new ChildParentCosineDistance[BagOfTopicsVar](2.0,-0.25)
    entityLinkingModel += new ContextBasedChildParentCompatibility[MentionBagVar](2.0,0.0625/4.0,0.25)
    entityLinkingModel += new ContextBasedChildParentCompatibility[ContextBagVar](1.0,0.0625/4.0,0.25)
    //entityLinkingModel += new ChildParentCosineDistance[MentionBagVar](2.0, -0.25/2.0, false)
    //entityLinkingModel += new ChildParentCosineDistance[ContextBagVar](1.0, -0.25/2.0, false)
    entityLinkingModel += {val t = new StructuralPriorsTemplate(-0.5, 0.25); t.debugOff; t}
    entityLinkingModel += new CustomChildParentCompatibility[BagOfEntityTypesVar](1.0, -1.0)
    entityLinkingModel += new SourceDocContraintFactor(2.0)
    */
    //entityLinkingModel += {val t=new WikipediaContraintFactor;t}
    entityLinkingModel
  }
}

object CrossDocEntityUtils {
  val filter = "(sunday|monday|tuesday|wednesday|thursday|friday|saturday)"
  //def newMention: KBEntity = KBEntity.newMention()

  def getQueryCanopy(supersetOfEntities: Seq[DocEntity], stopWords: cc.factorie.app.nlp.lexicon.WordLexicon = cc.factorie.app.nlp.lexicon.StopWords): HashSet[String] = {
    val queries = supersetOfEntities.filter(_.refMentionIds.size > 0)
    println("Found " + queries.size + " entities with query mentions.")
    val partialKBEs = queries.map(q => CrossDocEntityUtils.newMention(q, populateNameBag = true, populateMentionBag = false, populateContextBag = false, idfOpts = None, topicsOpts = None, topicsIDFOption = None))
    val canopySet = new HashSet[String]
    canopySet ++= partialKBEs.flatMap(e => e.canopyNames.filterNot(s => stopWords.contains(s)))
    if (canopySet.contains("")) canopySet.remove("")
    if (canopySet.contains("DFLT")) canopySet.remove("DFLT")
    println("  Found " + canopySet.size + " canopy strings")
    canopySet
  }

  def restrictRefsToQueryCanopies(refEntities: Seq[RefEntity], canopySet: HashSet[String]): Seq[RefEntity] = {
    refEntities.grouped(1000).flatMap(_.par.filter(e => {
      val partialKBE = CrossDocEntityUtils.newMention(e, populateNameBag = true, populateMentionBag = false, populateContextBag = false, idfOpts = None, topicsOpts = None, topicsIDFOption = None)
      //val canopy = partialKBE.canopyNames
      val canopy = partialKBE.bagOfNames.iterator.toSeq.sortBy(_._2).reverse.take(2).map(_._1)
      var result = false;
      var i = 0;
      while (i < canopy.size) {
        if (canopySet.contains(canopy(i))) {
          result = true;
          i = canopy.size
        };
        i += 1
      };
      result
    }).seq).toSeq
  }

  def restrictToQueryCanopies(allEntities: Seq[DocEntity], canopySet: HashSet[String]): Seq[DocEntity] = {
    allEntities.grouped(1000).flatMap(_.par.filter(e => {
      val partialKBE = CrossDocEntityUtils.newMention(e, populateNameBag = true, populateMentionBag = false, populateContextBag = false, idfOpts = None, topicsOpts = None, topicsIDFOption = None, None)
      val canopy = partialKBE.canopyNames
      var result = false;
      var i = 0;
      while (i < canopy.size) {
        if (canopySet.contains(canopy(i))) {
          result = true;
          i = canopy.size
        };
        i += 1
      };
      result
    }).seq).toSeq
  }

  def restrictKBEToQueryCanopies(allEntities: Iterator[TACKBEntity], canopySet: HashSet[String]): Seq[TACKBEntity] = {
    var in = 0
    var out = 0
    allEntities.grouped(1000).flatMap(_.par.filter(e => {
      val canopy = e.bagOfNames.iterator.toSeq.sortBy(_._2).reverse.take(2).map(_._1)
      var result = false
      var i = 0
      while (i < canopy.size) {
        if (canopySet.contains(canopy(i))) {
          result = true
          i = canopy.size
        }
        i += 1
      }
      //if(result==true){
      //  println("canopy:"+canopy.mkString(","))
      //}
      if (result) in += 1 else out += 1
      if ((in + out) % 10000 == 0) println("num in: " + in + " num out: " + out)
      result
    }).seq).toSeq
  }

  def addTopicsToKBEntity(e: KBEntity, topics: HashMap[String, Array[(String, Double)]]) = {
    val tbagOpt = topics.get(TFIDFUtils.docName(e))
    if (tbagOpt == None) println("Warning, no topics found for entity with dockey: " + TFIDFUtils.docName(e))
    for (tbag <- tbagOpt) {
      println("tbag.size: " + tbag.size)
      for ((k, v) <- tbag) e.attr[BagOfTopicsVar].add(k, v)(null)
    }
    println("WARNING, clearing mentions and context")
  }

  def newMention: TACKBEntity = {
    val result = new TACKBEntity("")
    result.flagAsMention
    result.exists.set(true)(null)
    result
  }

  def setGroundTruth(de: DocEntity, cde: TACKBEntity, ref: RefKB): Unit = {
    for (m <- de.refMentionIds.toSeq.flatMap(id => ref.mentionOpt(id))) {
      //if(cde.groundTruth != None)println("WARNING: multiple labels in ground-truth.")
      cde.groundTruth = Some(m.entity.get.id)
      cde.bagOfTruths += m.entity.get.id
    }
  }

  def newMention(de: DocEntity, populateNameBag: Boolean, populateMentionBag: Boolean, populateContextBag: Boolean, idfOpts: Option[(Double, HashMap[String, Double])], topicsOpts: Option[HashMap[String, Array[(String, Double)]]], topicsIDFOption: Option[(Double, HashMap[String, Double])], refOpt: Option[RefKB] = None): TACKBEntity = {
    val etOpt = refOpt.map(ref => de.entityType(ref))
    val cde = CrossDocEntityUtils.newMention
    for (et <- etOpt) cde.attr += new DocEntityData(de, et)
    cde.source = WikiCrossDocCoreferencer.DocumentSource
    val strId = de.id.toString
    cde._id = strId //if(strId.startsWith("EL") || strId.startsWith("SF"))strId else new ObjectId
    cde.docId = de.doc.name
    cde.canonicalName.set(de.canonicalMention.text)(null) //overwriting this later
    cde.attr[BagOfDocsVar] += cde.docId
    for (ref <- refOpt) setGroundTruth(de, cde, ref)
    for (ref <- refOpt) if (de.isELTrain(ref)) cde.queryMentionType = Some(DataSource.training) else if (de.isELEval(ref)) Some(DataSource.evaluation)
    // kbe.entityType.set(de.mentions.head)
    // kbe.canonicalName.set(de.mentions.head.text)
    cde.bagOfSources += WikiCrossDocCoreferencer.DocumentSource
    for (dm <- de.mentions) {
      if (populateNameBag) {
        val name = TFIDFUtils.normalizeMultiToken(dm.text)
        cde.bagOfNames += name
        if (dm.headToken.string != dm.text) cde.bagOfNames += TFIDFUtils.normalizeToken(dm.headToken.string)
        //for(nameToken <- name.split(" ").filter(_ != name))cde.bagOfNames += nameToken
        if (idfOpts.isDefined) {
          val (numDocs, idfCounts) = idfOpts.get
          TFIDFUtils.compressBagWithTFIDF(cde.bagOfNames, idfCounts, numDocs, 256)
        }
      }
      for (etype <- dm.entityType.filter(_ != TackbpEntityType.UKN)) {
        cde.attr[BagOfEntityTypesVar] += etype.toString
      }
      if (cde.bagOfEntityTypes.size > 0) cde.entityType := cde.bagOfEntityTypes.value.iterator.maxBy(_._2)._1
      if (populateContextBag) {
        for (t <- dm.section.tokens.slice(dm.start - WikiCrossDocCoreferencer.ContextWindow, dm.start)) {
          //val parent = t.parse.parent(t)
          //if (parent!=null)println("Token " + t.string+" parent: "+parent.string+" pos: "+parent.parseLabel.value.toString)
          cde.contextBag += TFIDFUtils.normalizeToken(t.string)
        }
        for (t <- dm.section.tokens.slice(dm.end, dm.end + WikiCrossDocCoreferencer.ContextWindow))
          cde.contextBag += TFIDFUtils.normalizeToken(t.string)
      }
    }
    if (populateMentionBag) {
      for (odm <- de.doc.attr[DocMentions].filter(m => m.mentionType.isDefined && m.mentionType.get == "NAM"); if odm.docEntity != de) {
        //val t = odm.headToken
        //val parent = t.parse.parent(t)
        /*
      if (parent != null) {
        println("Token " + t.string + " parent: " + parent.string + " pos: " + parent.parseLabel.value.toString)
        if (parent.parseLabel.value.toString == "root") println("  GOOD CONTEXT")
      } else println("MENTION IS ROOT: " + odm.headToken.string)
      */
        val mentionHead = TFIDFUtils.normalizeToken(odm.headToken.string)
        val mentionText = TFIDFUtils.normalizeMultiToken(odm.text) //odm.cleanedText())
        if (mentionHead.indexOf("#") == -1 && !mentionHead.matches(filter)) cde.bagOfMentions += mentionHead //odm.text
        if (mentionText.indexOf("#") == -1 && mentionText != mentionHead) cde.bagOfMentions += mentionText
        for (token <- mentionText.split(" ").filter(_ != mentionText)) cde.bagOfMentions += token
        if (idfOpts.isDefined) {
          val (numDocs, idfCounts) = idfOpts.get
          TFIDFUtils.compressBagWithTFIDF(cde.bagOfMentions, idfCounts, numDocs, 32)
        }
      }
    }
    for (topicMap <- topicsOpts) {
      val name = TFIDFUtils.docName(cde)
      val perETopics = topicMap.get(name)
      if (perETopics.isDefined) {
        val topicBag = cde.bagOfTopics
        if (topicsIDFOption.isDefined) {
          val (numDocs, tidfs) = topicsIDFOption.get
          for ((k, v) <- perETopics.get) topicBag.add(k, v * math.log(numDocs / tidfs(k)))(null)
        } else {
          for ((k, v) <- perETopics.get) topicBag.add(k, v)(null)
        }
      } else println("Warning, no topics found for doc-entity: " + name)
    }
    cde
  }

  def newMention(refEntity: RefEntity, populateNameBag: Boolean, populateMentionBag: Boolean, populateContextBag: Boolean, idfOpts: Option[(Double, HashMap[String, Double])], topicsOpts: Option[HashMap[String, Array[(String, Double)]]], topicsIDFOption: Option[(Double, HashMap[String, Double])]): TACKBEntity = {
    //refkb mentions do not have heads, they are the full token spans
    val cde = CrossDocEntityUtils.newMention
    cde.attr[BagOfDocsVar] += "refent"
    cde._id = refEntity.id.toString
    cde.attr += refEntity
    cde.source = WikiCrossDocCoreferencer.ReferenceSource
    cde.mId = refEntity.entityId
    cde.bagOfSources += WikiCrossDocCoreferencer.ReferenceSource
    cde.groundTruth = Some(refEntity.id)
    cde.bagOfTruths += refEntity.id
    cde.entityType := refEntity.entityType.toString
    cde.bagOfEntityTypes += refEntity.entityType.toString
    refEntity.refData.map(d => {
      cde.docId = d.wikiTitle
      //println("n: %s, wt: %s, canon: %s" format(d.name, d.wikiTitle, d.canonicalTitle))
      //d.canonicalTitle
      val canonicalName = d.canonicalTitle.getOrElse(d.name)
      //println("canonical name:"+canonicalName)
      cde.canonicalName := canonicalName
      if (populateNameBag) {
        cde.bagOfNames += TFIDFUtils.normalizeMultiToken(d.name)
        if (canonicalName != d.name) cde.bagOfNames += TFIDFUtils.normalizeMultiToken(canonicalName)
        cde.bagOfNames += TFIDFUtils.normalizeMultiToken(d.name)
        //val nameTokens = ClearTokenizer.getTokenList(d.name).map(_.text).toSeq
        //if(nameTokens.size>1)for (tok <- nameTokens) cde.bagOfNames += TFIDFUtils.normalizeToken(tok)
        if (idfOpts.isDefined) {
          val (numDocs, idfCounts) = idfOpts.get
          TFIDFUtils.compressBagWithTFIDF(cde.bagOfNames, idfCounts, numDocs, 256)
        }
      }
      if (populateContextBag || populateMentionBag) {
        val context = TFIDFUtils.extractWikiTitleContext(d.name)
        if (context.length > 0) {
          if (populateMentionBag) cde.bagOfMentions += context
          if (populateContextBag) cde.contextBag += context
        }
      }
      if (populateMentionBag) {
        for (slot <- d.slots.filter(_.source == DataSource.reference).filterNot(_.name.strValue.startsWith("blank"))) {
          slot.value match {
            case x: XmlValue => {
              //blank_name
              for (link <- x.allLinks) {
                //val string = TFIDFUtils.normalizeMultiToken(slot.value.strValue)
                val string = TFIDFUtils.normalizeMultiToken(TFIDFUtils.normalizeMultiToken(link))
                val strings = string.split(" ")
                cde.bagOfMentions += string
                if (strings.length > 0) cde.bagOfMentions ++= strings
              }
            }
          }
        }
        if (idfOpts.isDefined) {
          val (numDocs, idfCounts) = idfOpts.get
          TFIDFUtils.compressBagWithTFIDF(cde.bagOfMentions, idfCounts, numDocs, 32)
        }
      }
      //      if (populateContextBag) for (tok <- ClearTokenizer.getTokenList(d.wikiText).map(_.text)) cde.contextBag += TFIDFUtils.normalizeToken(tok)
      if (populateContextBag) for (tok <- DeterministicTokenizer.process(new Document(d.wikiText)).tokens.map(_.string)) cde.contextBag += TFIDFUtils.normalizeToken(tok)
    })
    // kbe.entityType.set(de.mentions.head)
    // kbe.canonicalName.set(de.mentions.head.text)
    //if(cde.canonicalName.value.trim.length==0){
    //  println("Warning, canonical name is empty!")
    //  EntityUtils.prettyPrint(Seq(cde),true)
    //}
    for (topicMap <- topicsOpts) {
      val name = TFIDFUtils.docName(cde)
      val perETopics = topicMap.get(name)
      //if(perETopics==None)perETopics = topicMap.get(cde.id)
      if (perETopics.isDefined) {
        val topicBag = cde.bagOfTopics
        if (topicsIDFOption.isDefined) {
          val (numDocs, tidfs) = topicsIDFOption.get
          for ((k, v) <- perETopics.get) topicBag.add(k, v * math.log(numDocs / tidfs(k)))(null)
        } else {
          for ((k, v) <- perETopics.get) topicBag.add(k, v)(null)
        }
        //println("Found topics for ref-ent: "+name+" with id: "+cde.id)
      } else println("Warning, no topics found for ref-entity: " + name + " with id: " + cde.id)
    }
    cde
  }

  def fromKBEntity(kbe: KBEntity) = {
    val result = new CrossDocEntity(DBUtils.compressId(kbe.id))
    //result.attr += kbe
    result
  }
}

object TFIDFUtils {
  def twoToOne(s1: String, s2: String): String = if (s1.compare(s2) >= 0) s1 + "-" + s2 else s2 + "-" + s1

  def normalizeBag(bag: BagOfWordsVariable) = {
    val newBag = bag.value.iterator.toSeq.map(sw => normalizeMultiToken(sw._1) -> sw._2)
    bag.clear
    for ((k, v) <- newBag) bag.add(k, v)(null)
  }

  def normalizeToken(s: String): String = {
    //todo: something more sophisticated if string is from a proper noun or proper mention
    s.toLowerCase.replaceAll("[\\._]", "").replaceAll("[^a-z ]+", " ").replaceAll(" +", " ").trim //.replaceAll("[0-9]","#")
  }

  def normalizeMultiToken(s: String): String = {
    s.toLowerCase.replaceAll("[\\._]", "").replaceAll("[^a-z ]+", " ").replaceAll(" +", " ").trim
  }

  def extractWikiTitleContext(s: String): String = extractWikiTitleAndName(s)._1

  def extractWikiTitleAndName(s: String): (String, String) = {
    var title = ""
    var name = ""
    val openParan = s.indexOf("(")
    if (openParan > 0) {
      val closeParan = s.indexOf(")")
      if (closeParan > openParan) {
        title = s.substring(openParan + 1, closeParan)
        name = s.substring(0, openParan - 1)
      }
    }
    (title, name)
  }

  // copied from WikiCorefPort
  def compressBagWithTFIDF(bag: BagOfWordsVariable, idfCounts: HashMap[String, Double], numDocs: Double, topK: Int): Unit = {
    def tfidf(maxWeight: Double, bagEntry: (String, Double)): Double = {
      val bias = 0.01
      val result = (bias + (1.0 - bias) * bagEntry._2 / (maxWeight + 1.0)) * scala.math.log(numDocs / idfCounts.getOrElse(bagEntry._1, 1.0))
      result
    }
    if (bag.size > 0) {
      var normalizer = 0.0
      normalizer = bag.value.iterator.maxBy(_._2)._2
      KBEntityUtils.compressBagTopK(bag, topK, tfidf(normalizer, _: (String, Double)))
    }
  }

  //  def generateIDFMaps(out: String = "src/main/resources/idfMaps/", fileName: String = "idf.gz", numDocsToProcess: Int = Integer.MAX_VALUE) {
  //    //:(Double,HashMap[String,Double]) ={
  //    println("Generating IDF counts")
  //    println("  output file: " + out)
  //    println("  max docs: " + numDocsToProcess)
  //    var numDocs: Double = 0.0
  //    var idfCounts = new LinkedHashMap[String, Double]
  //    //var coocCounts = new LinkedHashMap[String,Double]
  //    val writer = new PrintWriter(new GZIPOutputStream(new FileOutputStream(out + fileName)))
  //    //val coWriter = new PrintWriter(new GZIPOutputStream(new FileOutputStream(out+"cooc.gz")))
  //    //val mentionWriter = new PrintWriter(new GZIPOutputStream(new FileOutputStream(out+"idf.mentions.gz")))
  //
  //    val mongoConfigFile = "config/avon7-mongo.properties"
  //    //val mongoConfigFile = "config/remote-mongo.properties"
  //    lazy val mongoConfig = MongoConfig.fromFile(mongoConfigFile)
  //    println("Starting pipeline.")
  //    lazy val pipeline = new edu.umass.cs.iesl.tackbp.Pipeline(mongoConfig)
  //    println("About to get iterator.")
  //    var docIterator = pipeline.getAllDocsFromMongo: Iterator[Document]
  //    println("Got iterator, ready to go.")
  //    val startTime = System.currentTimeMillis
  //    var lastTimeCheck = startTime
  //    while (numDocs < numDocsToProcess && docIterator.hasNext) {
  //      try {
  //        val idfCountsPD = new LinkedHashSet[String]
  //        numDocs += 1
  //        val doc = docIterator.next()
  //        for (section <- doc.sections) {
  //          for (token <- section.tokens.map((t: Token) => normalizeToken(t.string)).toSet[String]) {
  //            //idfCountsPD(token) = idfCounts.getOrElse(token,0.0) + 1.0
  //            idfCountsPD += token
  //          }
  //        }
  //        for (mention: DocMention <- doc.attr[DocMentions].filter(m => m.isNamed)) {
  //          //val string = mention.headToken
  //          val string = normalizeMultiToken(mention.text)
  //          //idfCountsPD(string) = idfCounts.getOrElse(string,0.0)+1.0
  //          idfCountsPD += string
  //        }
  //        for (string <- idfCountsPD) idfCounts(string) = idfCounts.getOrElse(string, 0.0) + 1.0
  //        /*
  //        val coocPD = new LinkedHashSet[String]
  //        for(s1 <- idfCountsPD){
  //          for(s2 <- idfCountsPD){
  //            val key = twoToOne(s1,s2)
  //            coocPD += key
  //          }
  //        }
  //        for (string <- coocPD)coocCounts(string) = coocCounts.getOrElse(string,0.0)+1.0
  //        */
  //      }
  //      catch {
  //        case e: Exception => {
  //          println("Error processing document:" + e.getMessage)
  //          try {
  //            docIterator = pipeline.getAllDocsFromMongo: Iterator[Document]
  //            println("Redoing iterator")
  //            var count = 0
  //            while (docIterator.hasNext && count < numDocs) {
  //              docIterator.next();
  //              count += 1;
  //              if (count % 100 == 0) print("*");
  //              if (count % 5000 == 0) println("count:" + count)
  //            }
  //            println("Done: " + numDocs)
  //          } catch {
  //            case e: Exception => println("ERROR on retry: " + e.getMessage)
  //          }
  //        }
  //      }
  //      val elapsed = (System.currentTimeMillis() - startTime) / 1000L
  //      val window = ((System.currentTimeMillis() - lastTimeCheck) / 1000L).toInt
  //      if (window >= 60) {
  //        val docsPerSec = if (elapsed != 0) numDocs.toDouble / elapsed.toDouble else -1.0
  //        println("docs per sec: " + docsPerSec)
  //        lastTimeCheck = System.currentTimeMillis
  //      }
  //      if (numDocs % 1000 == 0) print(".")
  //      if (numDocs % 50000 == 0) println(numDocs)
  //    }
  //    println("counts.size " + idfCounts.size)
  //    println("num docs " + numDocs)
  //    writer.println(numDocs)
  //    for ((k, v) <- idfCounts.toSeq.sortBy(_._2).reverse) writer.println(k + "\t" + v)
  //    writer.flush()
  //    writer.close()
  //    /*
  //    //write cooc
  //    println("Writing cooc counts: "+coocCounts.size)
  //    coWriter.println(numDocs)
  //    coWriter.println(numDocs)
  //    for((k,v) <- coocCounts.toSeq.sortBy(_._2).reverse)coWriter.println(k+"\t"+v)
  //    coWriter.flush
  //    coWriter.close
  //    */
  //    //(numDocs,idfCounts)
  //  }

  //  def main(args:Array[String]){
  //    //generateIDFMaps("src/main/resources/idfMaps/")
  //    //trainAndSaveLDAForEL()
  //    //trainAndSaveLDAForWikiCoref()
  //    //augmentDocThetaMap("resources/elinking.lda.phi","resources/elinking.lda.docs.theta.gz","el","/iesl/canvas/sameer/tackbp/resources/idfMaps/idf.gz")
  //
  //    /*
  //    val tacFile     = if(args.length==0)"el-and-el-2013" else args(0)//"el-exp-comb" // //"el-exp-50" //"/iesl/canvas/sameer/tackbp/source/json/query/query.docs.comb.gz"
  //    val tacFile     = "/home/aravind/UMASS/IndependentStudy/tackbp/source/json/el/"
  //    println("Using tac file: "+tacFile)
  //    //val wikiFile    = "resources/wp.canopy-el-and-el2013.json.gz" //"resources/wikicoref.wp-entities.json.gz"
  //    val wikiFile    = "wikicoref.wp-entities-no-idf.json.gz"
  //    val wlFile    = "wikicoref.wl-entities-no-idf.json.gz" //"resources/wikicoref.wp-entities.json.gz"
  //    val ldaPhiFile  = "resources/el-exp50-years.lda.phi.gz"    //"resources/elinking.lda.phi.gz"
  //    val ldaThetaFile= "resources/el-exp50.lda.theta.gz" //"resources/elinking.lda.docs.theta.gz"
  //    val idfFile="/iesl/canvas/sameer/tackbp/resources/idfMaps/idf.gz" //"resources/elinking.idf.gz"
  //    trainAndSaveLDAForEL(numTopics=400,numIterations=150,numThreads=8,diagnostic=10,tacFile,wikiFile,wlFile,ldaPhiFile,ldaThetaFile,idfFile,0.25,None)
  //    */
  //    filterAndSerializeByCanopies("resources/wp.canopy-el-and-el2013.json.gz","el-and-el-2013","resources/wikicoref.wp-entities.json.gz")
  //  }

  //  def filterAndSerializeByCanopies(fileToSave: String, queryFileNameLookup: String, wpMasterFile: String) {
  //    println("Loading wikipedia")
  //    val wikiEntities = WikiCorefComm.deserialize(wpMasterFile)
  //    filterAndSerializeIteratorByCanopies(fileToSave, queryFileNameLookup, wikiEntities)
  //
  //    /*
  //    println("Loading query entities")
  //    val querySuperset = DEFTJson.deserializeFile(FileNames.docs(queryFileNameLookup)).toSeq.flatMap(_.attr[DocEntities])
  //    println("Generating canopies")
  //    val canopies = CrossDocEntityUtils.getQueryCanopy(querySuperset)
  //    //val canopies = new HashSet[String];canopies ++= Seq("obama","university","us","music")
  //    println("Canopies: "+canopies.mkString("\n"))
  //    println("Num canopies: "+canopies.size)
  //    println("Loading wikipedia")
  //    val wikiEntities = WikiCorefComm.deserialize(wpMasterFile)
  //    //println("Num wiki entities: "+wikiEntities.size)
  //    println("Filtering")
  //    val entitiesToSerialize = CrossDocEntityUtils.restrictKBEToQueryCanopies(wikiEntities,canopies)
  //    println("Num in canopies: "+entitiesToSerialize.size)
  //    println("Serializing")
  //    WikiCorefComm.serialize(entitiesToSerialize.iterator, fileToSave)
  //    */
  //  }

  //  def filterAndSerializeIteratorByCanopies(fileToSave: String, queryFileNameLookup: String, iterator: Iterator[TACKBEntity]) {
  //    println("Loading query entities")
  //    val querySuperset = DEFTJson.deserializeFile(FileNames.docs(queryFileNameLookup)).toSeq.flatMap(_.attr[DocEntities])
  //    println("Generating canopies")
  //    val canopies = CrossDocEntityUtils.getQueryCanopy(querySuperset)
  //    //val canopies = new HashSet[String];canopies ++= Seq("obama","university","us","music")
  //    println("Canopies: " + canopies.mkString("\n"))
  //    println("Num canopies: " + canopies.size)
  //    //println("Loading wikipedia")
  //    //val wikiEntities = WikiCorefComm.deserialize(wpMasterFile)
  //    //println("Num wiki entities: "+wikiEntities.size)
  //    println("Filtering")
  //    val entitiesToSerialize = CrossDocEntityUtils.restrictKBEToQueryCanopies(iterator, canopies)
  //    println("Num in canopies: " + entitiesToSerialize.size)
  //    println("Serializing")
  //    WikiCorefComm.serialize(entitiesToSerialize.iterator, fileToSave)
  //  }

  def inferTopicsForKBEntities(entities: Iterator[KBEntity], lda: LDA, numIterations: Int, stopWords: HashSet[String], ldaPhiFile: String = "resources/kbe.lda.phi.gz", ldaThetaFile: String = "resources/kbe.lda.theta.gz"): LDA = {
    implicit val random = new scala.util.Random(0)
    for (kbe <- entities) {
      val (name, tokens) = kbe2document(kbe, stopWords)
      if (tokens.length >= 3) {
        lda.addDocument(new cc.factorie.app.topics.lda.Document(lda.wordSeqDomain, name, tokens.toIndexedSeq), random)
      }
    }
    lda.inferTopicsMultithreaded(4, numIterations, diagnosticInterval = 10, diagnosticShowPhrases = false)
    saveAlphaAndPhi(lda, new File(ldaPhiFile))
    writeLDADocuments(lda, new File(ldaThetaFile))
    println("Maximizing phis and thetas")
    lda.maximizePhisAndThetas
    lda
  }

  //  def createIDFMapForTACEntities(tacFile: String, wpFile: String, file: String = "resources/elinking.idf.gz"): LinkedHashMap[String, Double] = {
  //    val entities = getEntitiesForTACTopicsAndIDF(tacFile, wpFile)
  //    createIDFMapForEntities(entities, file)
  //  }
  //
  //  def createIDFMapForEntities(entities: Iterator[KBEntity], file: String, useCounts: Boolean = false): LinkedHashMap[String, Double] = {
  //    val idfCounts = new LinkedHashMap[String, Double]
  //    val emptyStopWords = new HashSet[String]
  //    println("Generating TFIDF file: " + file)
  //    var numDocs = 0
  //    for (e <- entities) {
  //      val (name, tokens) = kbe2document(e, emptyStopWords)
  //      val toksToCount = if (useCounts) tokens else tokens.distinct
  //      for (token <- toksToCount) idfCounts(token) = idfCounts.getOrElse(token, 0.0) + 1.0
  //      numDocs += 1
  //      if (numDocs % 1000 == 0) print(".")
  //      if (numDocs % 50000 == 0) println(numDocs)
  //    }
  //    if (useCounts) numDocs = math.max(numDocs, (idfCounts.maxBy(_._2)._2 + 1.0).toInt)
  //    val writer = new PrintWriter(new GZIPOutputStream(new FileOutputStream(file)))
  //    println("TFIDF counts.size: " + idfCounts.size)
  //    println("TFIDF num docs: " + numDocs)
  //    writer.println(numDocs)
  //    for ((k, v) <- idfCounts.toSeq.sortBy(_._2).reverse) writer.println(k + "\t" + v)
  //    writer.flush()
  //    writer.close()
  //    idfCounts
  //  }
  //
  //  def getEntitiesForTACTopicsAndIDF(tacFile: String = "iesl/canvas/sameer/tackbp/source/json/query/query.docs.comb.gz", wpFile: String = "resources/wikicoref.wp-entities.json.gz"): Iterator[KBEntity] = {
  //    //(docEntities.map(de => CrossDocEntityUtils.newMention(de,ref))).toSeq
  //    val wpDocs: Iterator[TACKBEntity] = WikiCorefComm.deserialize(wpFile)
  //    val tkDocs: Iterator[TACKBEntity] = DEFTJson.deserializeFile(tacFile).map(_.attr[DocEntities].par.map((de: DocEntity) => CrossDocEntityUtils.newMention(de, populateNameBag = true, populateMentionBag = true, populateContextBag = true, idfOpts = None, topicsOpts = None, topicsIDFOption = None)).seq).flatMap(d => d)
  //    wpDocs.asInstanceOf[Iterator[KBEntity]] ++ tkDocs.asInstanceOf[Iterator[KBEntity]]
  //  }

  //  def trainAndSaveLDAForWikiCoref(numTopics: Int = 400, numIterations: Int = 100, numThreads: Int = 4, diagnostic: Int = 10, wpFile: String = "resources/wikicoref.wp-entities.json.gz", wlFile: String = "resources/wikicoref.wl-entities.json.gz", ldaFile: String = "resources/lda.wpwl.phi.gz", ldaDocFile: String = "resources/lda.wpwl.docs.theta.gz", idfFile: String = "/iesl/canvas/sameer/tackbp/resources/idfMaps/idf.wikilinks.tsv", percentageThreshold: Double = 0.10): LDA = {
  //    implicit val random = new scala.util.Random(0)
  //    object WordSeqDomain extends CategoricalSeqDomain[String]
  //    val model = DirectedModel()
  //    val lda = new LDA(WordSeqDomain, numTopics, 0.1, 0.01, 1)(model, random)
  //    val stopWords = generateStopWordsFromIDF(idfFile, percentageThreshold)
  //    println("Found " + stopWords.size + " stop words")
  //    val wlDocs = WikiCorefComm.deserialize(wlFile)
  //    val namesUsed = new HashSet[String]
  //    var numDocs = 0
  //    println("Adding lda docs")
  //    for (doc <- wlDocs) {
  //      val (name, tokens) = kbe2document(doc, stopWords)
  //      //val tokens = kbe2document(doc,stopWords)._2
  //      //val name = doc.id
  //      //val tokens = (doc.bagOfNames.value.iterator.map(_._1) ++ doc.contextBag.value.iterator.map(_._1) ++ doc.bagOfMentions.value.iterator.map(_._1)).toSeq
  //      if (!namesUsed.contains(name)) {
  //        namesUsed += name
  //        if (tokens.length >= 3) lda.addDocument(new cc.factorie.app.topics.lda.Document(WordSeqDomain, name, tokens.toIndexedSeq), random)
  //      }
  //      numDocs += 1
  //      if (numDocs % 1000 == 0) print("."); if (numDocs % 50000 == 0) println(numDocs)
  //    }
  //    val wpDocs = WikiCorefComm.deserialize(wpFile)
  //    for (doc <- wpDocs) {
  //      val (name, tokens) = kbe2document(doc, stopWords)
  //      //val tokens = kbe2document(doc,stopWords)._2
  //      //val tokens = (doc.bagOfNames.value.iterator.map(_._1) ++ doc.contextBag.value.iterator.map(_._1) ++ doc.bagOfMentions.value.iterator.map(_._1)).toSeq
  //      //val name = doc.canonicalName.value
  //      if (!namesUsed.contains(name)) {
  //        namesUsed += name
  //        if (tokens.length >= 3) lda.addDocument(new cc.factorie.app.topics.lda.Document(WordSeqDomain, name, tokens.toIndexedSeq), random)
  //      }
  //      numDocs += 1
  //      if (numDocs % 1000 == 0) print("."); if (numDocs % 50000 == 0) println(numDocs)
  //    }
  //    println("Num docs: " + numDocs)
  //    lda.inferTopicsMultithreaded(numThreads, numIterations, diagnosticInterval = diagnostic, diagnosticShowPhrases = false)
  //    saveAlphaAndPhi(lda, new File(ldaFile))
  //    writeLDADocuments(lda, new File(ldaDocFile))
  //    lda
  //  }

  def augmentDocThetaMap(entities: Seq[KBEntity], ldaPhiFile: String, ldaThetaFile: String, createSeparateFile: Boolean = false): HashMap[String, Array[(String, Double)]] = {
    println("Loading LDA model.")
    val lda = LDAUtils.loadLDAModelFromAlphaAndPhi(new File(ldaPhiFile))
    val stopWords = StopWords.contents //generateStopWordsFromIDF(idfStopFile,0.25)
    var thetaMap = new HashMap[String, Array[(String, Double)]]
    if (!createSeparateFile) {
      println("Loading theta map.")
      thetaMap = TFIDFUtils.readLDADocuments(new File(ldaThetaFile))
      //val extraThetas= new HashMap[String,Array[(String,Double)]]
    } else println("Creating a file: " + ldaThetaFile)
    println("About to perform inference on new documents.")
    val entries = entities.par.map(
      (m: KBEntity) => {
        val (name, tokens) = kbe2document(m, stopWords)
        var result = name -> Array.empty[(String, Double)]
        if (tokens.length >= 3 && !thetaMap.contains(name)) {
          val doc = new cc.factorie.app.topics.lda.Document(lda.wordSeqDomain, name, tokens)
          lda.inferDocumentTheta(doc, 10)
          val topics = (0 until lda.phis.size).toSeq.map(i => i.toString).zip(getThetasForDoc(doc, lda.phis.size).toSeq).filter((t: (String, Double)) => {
            t._2 > 0.00
          }).toArray[(String, Double)] //.foreach((t:(Double,Int))=>{paper.bagOfTopics.add(t._2+"",t._1)(null)})
          result = name -> topics
        } else println("Warning, entity " + name + " has fewer than three tokens, can't infer topics.")
        result
      }
    ).seq
    for (entry <- entries) {
      thetaMap += entry
    }
    println("Done inferring topics for documents.")
    val writer = new PrintWriter(new GZIPOutputStream(new FileOutputStream(ldaThetaFile)))
    for ((k, v) <- thetaMap) {
      val topics = v.map(_._1)
      val weights = v.map(_._2)
      writer.println(k + "\t" + topics.mkString(" ") + "\t" + weights.mkString(" "))
      writer.flush()
    }
    writer.close
    thetaMap
  }

  def augmentDocThetaMap(ldaPhiFile: String, ldaThetaFile: String, fileNameLookup: String, idfStopFile: String): HashMap[String, Array[(String, Double)]] = {
    println("Loading LDA model: " + ldaPhiFile)
    val lda = LDAUtils.loadLDAModelFromAlphaAndPhi(new File(ldaPhiFile))
    println("Loading theta map: " + ldaThetaFile)
    val thetaMap: HashMap[String, Array[(String, Double)]] = TFIDFUtils.readLDADocuments(new File(ldaThetaFile))
    val extraThetas = new HashMap[String, Array[(String, Double)]]
    val stopWords = generateStopWordsFromIDF(idfStopFile, 0.25)
    println("About to perform inference on new documents.")
    var count = 0
    println("Constructing iterator for parallel topic inference.")
    val docLocation = "/home/aravind/UMASS/IndependentStudy/tackbp/source/json/el/"
    //    val entriesBatch: Iterator[ArraySeq[(String, Array[(String, Double)])]] = DEFTJson.deserializeFile(FileNames.docs(fileNameLookup)).map(
    val entriesBatch: Iterator[ArraySeq[(String, Array[(String, Double)])]] = DEFTJson.deserializeFile(docLocation).map(
      (d: cc.factorie.app.nlp.Document) => {
        d.attr[DocEntities].map(e => e).filter(e => e.useForCoreference).par.map(e => {
          val m = CrossDocEntityUtils.newMention(e, populateNameBag = true, populateMentionBag = true, populateContextBag = true, idfOpts = None, topicsOpts = None, topicsIDFOption = None, None)
          val (name, tokens) = kbe2document(m, stopWords)
          //val tokens = kbe2document(m,stopWords)._2//(doc.bagOfNames.value.iterator.map(_._1) ++ doc.contextBag.value.iterator.map(_._1) ++ doc.bagOfMentions.value.iterator.map(_._1)).toSeq
          //val name:String = m.id
          var result = name -> Array.empty[(String, Double)]
          if (tokens.length >= 3) {
            val doc = new cc.factorie.app.topics.lda.Document(lda.wordSeqDomain, name, tokens)
            lda.inferDocumentTheta(doc, 10)
            val topics = (0 until lda.phis.size).toSeq.map(i => i.toString).zip(getThetasForDoc(doc, lda.phis.size).toSeq).filter((t: (String, Double)) => {
              t._2 > 0.00
            }).toArray[(String, Double)] //.foreach((t:(Double,Int))=>{paper.bagOfTopics.add(t._2+"",t._1)(null)})
            result = name -> topics
          } else println("Warning, entity " + name + " has fewer than three tokens, can't infer topics.")
          count += 1
          if (count % 1000 == 0) print(".");
          if (count % 50000 == 0) println(count)
          result
        }).seq
      }
    )
    println("Traversing iterator.")
    for (batch <- entriesBatch) {
      for (entry <- batch) {
        thetaMap += entry
        extraThetas += entry
      }
    }
    println("Done.")
    println("Done inferring topics for documents.")
    var writer = new PrintWriter(new GZIPOutputStream(new FileOutputStream(ldaThetaFile + ".aug.gz")))
    for ((k, v) <- thetaMap) {
      val topics = v.map(_._1)
      val weights = v.map(_._2)
      writer.println(k + "\t" + topics.mkString(" ") + "\t" + weights.mkString(" "))
      writer.flush()
    }
    writer.close()
    writer = new PrintWriter(new GZIPOutputStream(new FileOutputStream(ldaThetaFile + ".aug-only.gz")))
    for ((k, v) <- extraThetas) {
      val topics = v.map(_._1)
      val weights = v.map(_._2)
      writer.println(k + "\t" + topics.mkString(" ") + "\t" + weights.mkString(" "))
      writer.flush()
    }
    writer.close()
    thetaMap
  }

  def trainAndSaveLDAForEL(numTopics: Int = 400, numIterations: Int = 100, numThreads: Int = 4, diagnostic: Int = 10, tacFile: String, wpFile: String, wlFile: String, ldaFile: String = "resources/elinking.lda.phi.gz", ldaDocFile: String = "resources/elinking.lda.docs.theta.gz", idfFile: String = "resources/elinking.idf.gz", percentageThreshold: Double = 0.25, canopyStringsOpt: Option[HashSet[String]] = None): LDA = {
    //    println("Using tac file name identifier: " + tacFile)
    //    println("  file name: " + FileNames.docs(tacFile))
    println("  file name: " + tacFile)
    var numFiltered = 0
    def isInCanopy(kbe: KBEntity): Boolean = {
      var result = false
      if (canopyStringsOpt != None) {
        val tokens = kbe.canopyNames
        val canopyStrings = canopyStringsOpt.get
        var i = 0
        while (i < tokens.length) {
          if (canopyStrings.contains(tokens(i))) {
            i = tokens.length
            result = true
          }
        }
      } else result = true
      if (result == false) numFiltered += 1
      result
    }
    implicit val random = new scala.util.Random(0)
    object WordSeqDomain extends CategoricalSeqDomain[String]
    val model = DirectedModel()
    val lda = new LDA(WordSeqDomain, numTopics, 0.1, 0.01, 1)(model, random)
    /*
    if(!(new File(idfFile)).exists){
      println("No IDF file found, creating one...")
      createIDFMapForTACEntities(tacFile,wpFile,idfFile)
    }
    val stopWords = generateStopWordsFromIDF(idfFile,percentageThreshold)
    */
    val stopWords = StopWords.contents
    //
    //Wikipedia
    //    if ((new File(wpFile)).exists) {
    //      val wikiDocs = WikiCorefComm.deserialize(wpFile)
    //      //val namesUsed = new HashSet[String]
    //      for (doc <- wikiDocs.filter(kbe => isInCanopy(kbe))) {
    //        val (name, tokens) = kbe2document(doc, stopWords)
    //        //val tokens = kbe2document(doc,stopWords)._2//(doc.bagOfNames.value.iterator.map(_._1) ++ doc.contextBag.value.iterator.map(_._1) ++ doc.bagOfMentions.value.iterator.map(_._1)).toSeq
    //        //val name = doc.canonicalName.value
    //        //if(!namesUsed.contains(name)){
    //        //namesUsed += name
    //        if (tokens.length >= 3) lda.addDocument(new cc.factorie.app.topics.lda.Document(WordSeqDomain, name, tokens.toIndexedSeq), random)
    //        //}
    //      }
    //    } else println("Skipping wikipedia, file: " + wpFile + " not found.")
    //
    //Wikilinks
    //    if ((new File(wlFile)).exists) {
    //      val wikiDocs = WikiCorefComm.deserialize(wlFile)
    //      //val namesUsed = new HashSet[String]
    //      for (doc <- wikiDocs.filter(kbe => isInCanopy(kbe))) {
    //        val (name, tokens) = kbe2document(doc, stopWords)
    //        //val tokens = kbe2document(doc,stopWords)._2//(doc.bagOfNames.value.iterator.map(_._1) ++ doc.contextBag.value.iterator.map(_._1) ++ doc.bagOfMentions.value.iterator.map(_._1)).toSeq
    //        //val name = doc.canonicalName.value
    //        //if(!namesUsed.contains(name)){
    //        //  namesUsed += name
    //        if (tokens.length >= 3) lda.addDocument(new cc.factorie.app.topics.lda.Document(WordSeqDomain, name, tokens.toIndexedSeq), random)
    //        //}
    //      }
    //    } else println("Skipping wikilinks, file: " + wlFile + " not found.")
    //
    //RefEntities
    var numRefs = 0
    val ref = RefKB.loadFromDefault
    println("Num refs loaded: " + ref.entities.size)
    //grouped(1000).flatMap(_.par.map(
    var progress = 0
    var refEnts = ref.entities.toSeq.grouped(1000).flatMap(
      b => {
        progress += 1;
        print(".");
        if (progress % 50 == 0) println(progress)
        b.par.map(e => CrossDocEntityUtils.newMention(e, populateNameBag = true, populateMentionBag = true, populateContextBag = true, idfOpts = None, topicsOpts = None, topicsIDFOption = None)).seq
      }
    ).toSeq
    for (doc <- refEnts.filter(kbe => isInCanopy(kbe))) {
      val (name, tokens) = kbe2document(doc, stopWords)
      //val tokens = kbe2document(doc,stopWords)._2//(doc.bagOfNames.value.iterator.map(_._1) ++ doc.contextBag.value.iterator.map(_._1) ++ doc.bagOfMentions.value.iterator.map(_._1)).toSeq
      //val name = doc.id
      if (tokens.length >= 3) {
        numRefs += 1
        lda.addDocument(new cc.factorie.app.topics.lda.Document(WordSeqDomain, name, tokens.toIndexedSeq), random)
      } else println("Warning, ref entity with id " + name + " has fewer than 3 words, skipping.")
    }
    println("Num ref: " + numRefs)
    refEnts = null
    //
    //TacKBP
    println("Deserializing tac")
    //if(tacFile.startsWith("/"))DEFTJson.deserializeFile(tacFile) else
    //    val tacBatches: Iterator[ArraySeq[TACKBEntity]] = DEFTJson.deserializeFile(FileNames.docs(tacFile)).map(
    val tacBatches: Iterator[ArraySeq[TACKBEntity]] = DEFTJson.deserializeFile(tacFile).map(
      (d: cc.factorie.app.nlp.Document) => {
        //println("Document: "+d.name)
        d.attr[DocEntities].map(e => e).filter(e => e.useForCoreference).par.map(e => CrossDocEntityUtils.newMention(e, populateNameBag = true, populateMentionBag = true, populateContextBag = true, idfOpts = None, topicsOpts = None, topicsIDFOption = None, Some(ref))).seq
      }
    )
    //val tacEnts = DEFTJson.deserializeFile(FileNames.docs(tacFile)).flatMap(_.attr[DocEntities]).par.map(e => CrossDocEntityUtils.newMention(e,Some(ref))).seq
    //val tacDocs = DEFTJson.deserializeFile(FileNames.docs(tacFile))//("/iesl/canvas/sameer/tackbp/source/json/query.docs/query.docs.json.gz").toSeq
    var count = 0
    //    val tacEnts = DEFTJson.deserializeFile(FileNames.docs(tacFile)).grouped(1000).flatMap((docs:Seq[cc.factorie.app.nlp.Document])=>{
    //      docs.par.map
    //    })
    for (tacEnts <- tacBatches) {
      for (d <- tacEnts.filter(kbe => isInCanopy(kbe))) {
        val (name, tokens) = kbe2document(d, stopWords)
        //val tokens = kbe2document(d,stopWords)._2//(doc.bagOfNames.value.iterator.map(_._1) ++ doc.contextBag.value.iterator.map(_._1) ++ doc.bagOfMentions.value.iterator.map(_._1)).toSeq
        //val name = d.id
        //val doc = document2document(d,stopWords)
        //      if(!namesUsed.contains(doc.canonicalName.value)){
        if (tokens.length >= 3) {
          lda.addDocument(new cc.factorie.app.topics.lda.Document(WordSeqDomain, name, tokens), random)
          //namesUsed += name
        }
        count += 1
        //println("DOC: "+d.docId)
        if (count % 1000 == 0) print("."); if (count % 50000 == 0) println(count)
      }
    }
    println("Filtered " + numFiltered + " entities that were outside of the target canopies.")
    println("About to train topic model.")
    println("Num docs: " + lda.documents.size)
    if (numThreads == 1) lda.inferTopics(numIterations, Int.MaxValue, 10, false)
    else {
      println("About to infer lda params with  " + numThreads + " threads.");
      lda.inferTopicsMultithreaded(numThreads, numIterations, diagnosticInterval = diagnostic, diagnosticShowPhrases = false)
    }
    saveAlphaAndPhi(lda, new File(ldaFile))
    writeLDADocuments(lda, new File(ldaDocFile))
    lda
  }

  protected def getThetasForDoc(doc: cc.factorie.app.topics.lda.Doc, size: Int): Array[Double] = {
    //phis.foreach(_.value.zero())
    val result = new Array[Double](size)
    val len = doc.ws.length
    var i = 0
    while (i < len) {
      val zi = doc.zs.intValue(i)
      //phis(zi).value.masses.+=(doc.ws.intValue(i), 1.0)
      result(zi) += 1.0
      //doc.theta.value.masses.+=(zi, 1.0)
      i += 1
    }
    var norm = 0.0;
    for (e <- result) norm += e
    for (i <- 0 until result.size) result(i) = result(i) / norm
    result
  }

  def generateStopWordsFromIDF(idfFile: String = "/iesl/canvas/sameer/tackbp/resources/idfMaps/idf.gz", percentageThreshold: Double = 0.25): HashSet[String] = {
    val result = new HashSet[String]
    val idf = if (idfFile.endsWith(".gz")) KBEntityUtils.loadIDFFrequencyStatistics(new GZIPInputStream(new FileInputStream(idfFile))) else KBEntityUtils.loadIDFFrequencyStatistics(new FileInputStream(idfFile))
    generateStopWordsFromIDFDS(idf, percentageThreshold)
  }

  def generateStopWordsFromIDFDS(idf: (Double, HashMap[String, Double]), percentageThreshold: Double = 0.25): HashSet[String] = {
    val result = new HashSet[String]
    val docCount = idf._1
    val idfMap = idf._2
    //val (docCount,idfMap) = if(idfFile.endsWith(".gz"))KBEntityUtils.loadIDFFrequencyStatistics(new GZIPInputStream(new FileInputStream(idfFile))) else KBEntityUtils.loadIDFFrequencyStatistics(new FileInputStream(idfFile))
    //println("FYI: assumes sorted list.")
    var prevTokenVal = Double.PositiveInfinity
    var running = true
    val iterator = idfMap.iterator
    while (running && iterator.hasNext) {
      val pair = iterator.next
      val k = pair._1
      val v = pair._2
      //assert(v<=prevTokenVal,"Error, idf map should be sorted.")
      prevTokenVal = v
      val pctOfDocsInWhichWordOccurs = v / docCount
      if (pctOfDocsInWhichWordOccurs >= percentageThreshold) {
        println("Adding stopword: " + k + "  (pct:" + pctOfDocsInWhichWordOccurs + ")")
        result += k
      }
    }
    result
  }

  def document2document(document: Document, stopWords: HashSet[String]): (String, Seq[String]) = {
    val name = document.name
    val tokens = document.tokens.map(_.string).filterNot(s => stopWords.contains(s)).toSeq.map(t => TFIDFUtils.normalizeToken(t))
    val mentionTokens = document.attr[DocMentions].map((m: DocMention) => TFIDFUtils.normalizeMultiToken(m.text))
    //val mentionTokens = document.attr[DocMentions].map((m:DocMention) => TFIDFUtils.normalizeMultiToken(m.cleanedText(false)))
    (name, tokens ++ mentionTokens)
  }

  def kbe2document(kbe: KBEntity, stopWords: HashSet[String]): (String, Seq[String]) = {
    val name = docName(kbe)
    val tokens = (bags2tokens(kbe.bagOfNames).map(s => TFIDFUtils.normalizeMultiToken(s)) ++ bags2tokens(kbe.bagOfMentions) ++ bags2tokens(kbe.contextBag)).filterNot(s => stopWords.contains(s))
    (name, tokens)
  }

  def bags2tokens(bag: BagOfWordsVariable): Seq[String] = bag.value.iterator.map(_._1).toSeq

  def docName(kbe: KBEntity): String = DBUtils.compressId(kbe.id)

  def saveAlphaAndPhi(lda: LDA, file: File) {
    val pw = new PrintWriter(new GZIPOutputStream(new FileOutputStream(file))) //new PrintWriter(file)
    pw.println(lda.phis.size)
    pw.println("/alphas")
    pw.println(lda.alphas.value.mkString(" "))
    pw.println()
    for (phi <- lda.phis) {
      pw.println("/topic")
      phi.value.foreachActiveElement((index, count) => {
        val word: String = lda.wordSeqDomain.elementDomain.category(index)
        if (count != 0.0) {
          pw.println(word)
          pw.println(count)
        }
      })
    }
    pw.close()
  }

  def writeLDADocuments(lda: LDA, file: File) {
    val writer = new PrintWriter(new GZIPOutputStream(new FileOutputStream(file)))
    for (document <- lda.documents) writeDocument(lda, document, writer)
    writer.close()
  }

  protected def writeDocument(lda: LDA, document: cc.factorie.app.topics.lda.Doc, pw: PrintWriter) {
    val topics = getThetasForDoc(document, lda.phis.size).toSeq.zip(0 until lda.phis.size).filter((t: (Double, Int)) => {
      t._1 > 0.0
    }) //.foreach((t:(Double,Int))=>{paper.bagOfTopics.add(t._2+"",t._1)(null)})
    //val topics = document.theta.value.toSeq.zip(0 until lda.phis.size).filter((t:(Double,Int))=>{t._1>0.05})//.foreach((t:(Double,Int))=>{paper.bagOfTopics.add(t._2+"",t._1)(null)})
    val string = document.name + "\t" + topics.map(_._2).mkString(" ") + "\t" + topics.map(_._1).mkString(" ")
    pw.println(string)
  }

  def getTopicIDFs(topicMap: HashMap[String, Array[(String, Double)]]): (Double, HashMap[String, Double]) = {
    val idfs = new HashMap[String, Double]
    val count = topicMap.size.toDouble
    for ((k, v) <- topicMap) {
      for ((topic, weight) <- v) {
        idfs(topic) = idfs.getOrElse(topic, 0.0) + weight
      }
    }
    (count, idfs)
  }

  def readLDADocuments(file: File): HashMap[String, Array[(String, Double)]] = {
    val result = new HashMap[String, Array[(String, Double)]]
    val reader = new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(file))))
    var line = reader.readLine
    var count = 0
    while (line != null) {
      val fields = line.split("\t")
      if (fields.length == 3) {
        val topics = fields(1).split(" ")
        val weights = fields(2).split(" ").map(_.toDouble)
        assert(topics.length == weights.length)
        result += fields(0) -> (topics zip weights)
      } else println("Warning, line " + line + " only has " + fields.length + " fields.")
      count += 1
      if (count % 10000 == 0) print(".");
      if (count % 500000 == 0) println(count)
      line = reader.readLine
    }
    reader.close()
    println("Num entities with topics in file: " + result.size)
    println("Head: " + result.head._1 + "-->" + result.head._2.mkString(" "))
    result
  }
}

object EvalUtils {
  def getGenericEntityMapFromEntities[E](entities: Seq[E], filter: E => Boolean, mentionIdString: E => String, entityIdString: E => String): GenericEntityMap[String] = {
    val codeMap = collection.mutable.HashMap[String, Long]()
    var idCounter = 0l
    def inc: Long = {
      idCounter += 1
      idCounter
    }
    val map = new GenericEntityMap[String]()
    for (entity <- entities) {
      if (filter(entity)) {
        val mId = mentionIdString(entity)
        val id = codeMap.getOrElseUpdate(entityIdString(entity), inc)
        map.addMention(mId, id)
      }
    }
    map
  }
}

class DebugDiffList extends DiffList {
  override def scoreAndUndo(predictionModel: Model, objectiveModel: Model): (Double, Double) = {
    val score1 = scoreAndUndo(predictionModel)
    var score2 = 0.0
    if (objectiveModel != null) score2 = super.scoreAndUndo(objectiveModel)
    (score1, score2)
  }

  override def scoreAndUndo(model: Model): Double = {
    if (this.length == 0) return 0.0 // short-cut the simple case
    /*
    var containsWL=false
    for(v <- this.map(_.variable)){
      if(v.isInstanceOf[EntityAttr]){
        val kbe = v.asInstanceOf[EntityAttr].entity.asInstanceOf[KBEntity]
        if(kbe.source=="wp")containsWL=true
      }
    }
    */
    for (family <- model.asInstanceOf[TemplateModel].families) {
      family match {
        case f: DebugableTemplate => f.debugOn
        case _ => {}
      }
    }
    //for(t <- model.asInstanceOf[TemplateModel].familiesOfClass[DebugableTemplate])t.debugOn
    println("=====DEBUGGING MODEL SCORE=====")
    println("----NEXT WORLD----")
    var s = model.currentScore(this)
    println("  next: " + s)
    //log(Log.DEBUG)("DiffList scoreAndUndo  pre-undo score=" + s)
    this.undo
    // We need to re-calculate the Factors list because the structure may have changed
    println("----CURRENT WORLD----")
    val s2 = model.currentScore(this)
    println("  current: " + s2)
    s -= s2
    println("TOTAL SCORE: " + s)
    //for(t <- model.asInstanceOf[TemplateModel].familiesOfClass[DebugableTemplate])t.debugOff
    for (family <- model.asInstanceOf[TemplateModel].families) {
      family match {
        case f: DebugableTemplate => f.debugOff
        case _ => {}
      }
    }
    return s
    //}else return super.scoreAndUndo(model)
    //s
  }
}