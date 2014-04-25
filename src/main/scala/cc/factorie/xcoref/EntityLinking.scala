package cc.factorie.xcoref

import scala.collection.mutable.{HashMap, ArrayBuffer, HashSet}
import cc.factorie._
import scala.Some
import cc.factorie.util.coref.{CorefEvaluator, GenericEntityMap}
import cc.factorie.variable.DiffList
import cc.factorie.DiffList
import java.io.{FileInputStream, File}
import java.util.zip.GZIPInputStream
import cc.factorie.util.BinarySerializer

object EntityLinking {
  //val queriesDir = "/home/aravind/UMASS/IndependentStudy/tackbp/queries/"
  val queriesDir = "/iesl/canvas/sameer/tackbp/queries/"

  def loadQueries(kb: RefKB, makeAllEval: Boolean = false): HashSet[String] = {
    val docs = new HashSet[String]()
    val trainingOrEval = if (makeAllEval) DataSource.evaluation else DataSource.training
    docs ++= Loader.loadELQueries(queriesDir + "EL/2009-eval.xml", kb, trainingOrEval)
    docs ++= Loader.loadELQueries(queriesDir + "EL/2010-train.xml", kb, trainingOrEval)
    docs ++= Loader.loadELQueries(queriesDir + "EL/2010-eval.xml", kb, trainingOrEval)
    docs ++= Loader.loadELQueries(queriesDir + "EL/2011-eval.xml", kb, trainingOrEval)
    docs ++= Loader.loadELQueries(queriesDir + "EL/2012-eval.xml", kb, DataSource.evaluation)
    //    docs ++= Loader.loadELQueries(queriesDir + "el/2013-eval.xml", kb, DataSource.evaluation)
    docs
  }

  def loadAnnotations(kb: RefKB, makeAllEval: Boolean = false) {
    val trainingOrEval = if (makeAllEval) DataSource.evaluation else DataSource.training
    Loader.loadELAnnotations(queriesDir + "EL/2009-eval.tab", kb, trainingOrEval)
    Loader.loadELAnnotations(queriesDir + "EL/2010-train.tab", kb, trainingOrEval)
    Loader.loadELAnnotations(queriesDir + "EL/2010-eval.tab", kb, trainingOrEval)
    Loader.loadELAnnotations(queriesDir + "EL/2011-eval.tab", kb, trainingOrEval)
    Loader.loadELAnnotations(queriesDir + "EL/2012-eval.tab", kb, DataSource.evaluation)
    Loader.removeUnannotatedELMentions(kb)
  }

  def runWikiLinksEval(labeledMentions: Seq[KBEntity]) {
    val evaluationEntities = labeledMentions.filter(_.groundTruth != None)
    val tacDocsOnly = evaluationEntities.filter(_.source == WikiCrossDocCoreferencer.DocumentSource)
    //val entityLinkingOnly = tacDocsOnly.filterNot(_.groundTruth.get.startsWith("NIL"))
    val (nilOnly, entityLinkingOnly) = tacDocsOnly.partition(_.groundTruth.get.startsWith("NIL"))
    evaluate(evaluationEntities, "EVALUATING ALL ENTITIES")
    //for(e <- tacDocsOnly) {
    //  println(e.groundTruth.get)
    //}
    evaluate(tacDocsOnly, "EVALUATING DOC ENTITIES ONLY")
    evaluate(entityLinkingOnly, "ENTITY LINKING OF DOCS")
    evaluate(nilOnly, "NIL CLUSTERING OF DOCS")
  }

  protected def evaluate(labeledMentions: Seq[KBEntity], msg: String) {
    println(msg)
    println("Number of evaluation entities: " + labeledMentions.size)
    val truth = EvalUtils.getGenericEntityMapFromEntities[KBEntity](labeledMentions, _.isObserved, _.id.toString, _.groundTruth.get)
    val pred = EvalUtils.getGenericEntityMapFromEntities[KBEntity](labeledMentions, _.isObserved, _.id.toString, _.entityRoot.id.toString)
    println(CorefEvaluator.evaluate(pred, truth))
  }

  def collapseOnTruth(entities: Seq[KBEntity]) = cc.factorie.app.bib.EntityUtils.collapseOn[KBEntity](entities, (e: KBEntity) => {
    if (e.bagOfTruths.size == 1) Some(e.bagOfTruths.value.iterator.next._1) else None
  }, () => new TACKBEntity(""), (e: KBEntity) => {})

  def collapseOnTruthAndOverlap(entities: Seq[KBEntity]) = {
    val result = cc.factorie.app.bib.EntityUtils.collapseOn[KBEntity](entities, (e: KBEntity) => {
      if (e.bagOfTruths.size == 1) Some(e.bagOfTruths.value.iterator.next._1) else None
    }, () => new TACKBEntity(""), (e: KBEntity) => {})
    var numDisconnected = 0
    var numNotDisconnected = 0
    var canopyOverlapCount = 0
    var topicOverlapCount = 0
    var mentionOverlapCount = 0
    var nameOverlapCount = 0
    var nameOrTopicCount = 0
    var nameAndTopicOrMentionCount = 0
    for (e <- result.filter(m => m.parentEntity != null && m.groundTruth != None && m.source == WikiCrossDocCoreferencer.DocumentSource)) {
      val p = e.parentEntity.asInstanceOf[TACKBEntity]
      val d = new DiffList
      //p.setParentEntity(null.asInstanceOf[TACKBEntity])(d)
      cc.factorie.app.bib.EntityUtils.linkChildToParent(e, null.asInstanceOf[cc.factorie.app.nlp.hcoref.Entity])(d)
      val canopyOverlap: Boolean = e.canopyNames.toSet.intersect(p.canopyNames.toSet).size > 0
      val topicOverlap: Boolean = e.bagOfMentions.value.cosineSimilarity(p.bagOfMentions.value) == 0.0
      val mentionOverlap: Boolean = e.bagOfTopics.value.cosineSimilarity(p.bagOfTopics.value) == 0.0
      val nameOverlap: Boolean = e.bagOfNames.value.cosineSimilarity(p.bagOfTopics.value) == 0.0

      if (canopyOverlap && topicOverlap && mentionOverlap) {
        numNotDisconnected += 1
        d.undo
      } else {
        numDisconnected += 1
      }
      if (canopyOverlap) canopyOverlapCount += 1
      if (topicOverlap) topicOverlapCount += 1
      if (mentionOverlap) mentionOverlapCount += 1
      if (nameOverlap) nameOverlapCount += 1
      if (canopyOverlap || nameOverlap) nameOrTopicCount += 1
      if (nameOverlap && (topicOverlap || mentionOverlap)) nameAndTopicOrMentionCount += 1
      //if(true)
      //  e.setParentEntity(null.asInstanceOf[TACKBEntity])(null)
      //  numDisconnected += 1
      //} else numNotDisconnected += 1
    }
    val total = (numDisconnected + numNotDisconnected)
    println("Num disconnected: " + numDisconnected + " pct: " + (numDisconnected.toDouble / total.toDouble) + " num intact: " + numNotDisconnected)
    println("  canopy overlap: " + canopyOverlapCount + " pct: " + (canopyOverlapCount.toDouble / total.toDouble))
    println("  topic overlap: " + topicOverlapCount + " pct: " + (topicOverlapCount.toDouble / total.toDouble))
    println("  mention overlap: " + mentionOverlapCount + " pct: " + (mentionOverlapCount.toDouble / total.toDouble))
    println("  name overlap: " + nameOverlapCount + " pct: " + (nameOverlapCount.toDouble / total.toDouble))
    println("  name|topic overlap: " + nameOrTopicCount + " pct: " + (nameOrTopicCount.toDouble / total.toDouble))
    println("  name&(topic|mention) overlap: " + nameAndTopicOrMentionCount + " pct: " + (nameAndTopicOrMentionCount.toDouble / total.toDouble))
    println("  totalCount: " + total)
    result
  }

  def trainAndTest(): Unit = {
    println("Template are...")
    KBEntityProcessingUtils.getTrainableModel
    //val topicsLocation = "/home/aravind/UMASS/IndependentStudy/tackbp/resources/topics/el-only-all-years200t.lda.docs.theta.gz"
    //val idfLocation = "/home/aravind/UMASS/IndependentStudy/tackbp/resources/idf/elinking.idf.gz"
    //val wikiLocation = "/home/aravind/UMASS/IndependentStudy/tackbp/resources/wikipedia/wp.canopy-el-and-el2013.json.gz"
    //val referenceLocation = "/home/aravind/UMASS/IndependentStudy/tackbp/reference/2009"
    //val docsFile = "/home/aravind/UMASS/IndependentStudy/tackbp/source/json/el/el.docs.comb.gz"
    //val topicsOption = Some(TFIDFUtils.readLDADocuments(new File(topicsLocation)))
    //val idfOption = Some(KBEntityUtils.loadIDFFrequencyStatistics(new GZIPInputStream(new FileInputStream(idfLocation))))
    
    val topicsLocation = "/iesl/canvas/mwick/workspace/tackbp/resources/el-only-all-years200t.lda.docs.theta.gz"
    val idfLocation = "/iesl/canvas/mwick/workspace/tackbp/resources/elinking.idf.gz"
    val wikiLocation = "/iesl/canvas/mwick/workspace/tackbp/resources/wp.canopy-el-and-el2013.json.gz"
    val referenceLocation = "/iesl/canvas/sameer/tackbp/reference/2009"
    val docsFile = "/iesl/canvas/sameer/tackbp/source/json/el/el.docs.comb.gz"
    val topicsOption = Some(TFIDFUtils.readLDADocuments(new File(topicsLocation)))
    val idfOption = Some(KBEntityUtils.loadIDFFrequencyStatistics(new GZIPInputStream(new FileInputStream(idfLocation))))

    val useNameBags = true
    val useMentionBags = true
    val useContextBags = false
    val wikipediaFileOption = Some(wikiLocation)
    val tacFileName = "el" //"el-2012-exp-50"
    val useAllYearsForEval = false
    val limitMentionsToCanopiesOfQueries = true
    //
    //training parameters
    val trainIters = 16
    val trainSteps = 200000
    val includeSupplementalMentionsForTraining = false
    val trainWithEntityLinking = false
    //
    //testing parameters
    val testSteps = 200000//1000000
    val includeSupplementalMentionsForTesting = false
    val testWithEntityLinking = false

    println("Training and testing.")
    println("Loading Ref KB")
    val refKB = new RefKB("el")
    loadQueries(refKB, useAllYearsForEval)
    loadAnnotations(refKB, useAllYearsForEval)
    println("Filling entity data")
    Loader.fillEntityData(refKB, referenceLocation)
    //    Loader.fillEntityData(refKB,"/Users/mwick/data/tackbp/reference/2009/")
    //    Loader.fillEntityData(refKB, RefKB.defaultDirName)
    println("ref# " + refKB.entities.size)

    println("Loading TACKBP documents")
    //val docs = DEFTJson.deserializeFile(FileNames.docs(tacFileName)).toIndexedSeq
    val docs = DEFTJson.deserializeFile(docsFile).toIndexedSeq

    //println("Num refs: "+docs.flatMap(_.attr[DocEntities]).filterNot(_.refMentionIds.isEmpty).size)
    //val (trainDocs,testDocs) = random.shuffle(docs).split(0.5)
    //val training = trainDocs.flatMap(_.attr[DocEntities]).filter(_.useForCoreference).filterNot(_.refMentionIds.isEmpty)
    //val testing = testDocs.flatMap(_.attr[DocEntities]).filter(_.useForCoreference)//.filterNot(_.refMentionIds.isEmpty)

    val allEntities = docs.flatMap(_.attr[DocEntities]).filter(_.useForCoreference).toSeq
    val (nonRefsOnly, refsOnly) = allEntities.partition(_.refMentionIds.isEmpty)
    val (trainingNonRefs, testingNonRefs) = nonRefsOnly.split(0.1)
    var training = refsOnly.filter(_.isELTrain(refKB))
    if (includeSupplementalMentionsForTraining) training = training ++ nonRefsOnly //trainingNonRefs
    var testing = refsOnly.filter(_.isELEval(refKB))
    
    /*
    val sampleTestSet = Set("EL_ENG_02227", 
                            "EL_ENG_02225", 
                            "EL_ENG_02221", 
                            "EL_ENG_02177", 
                            "EL_ENG_02097",
                            "EL_ENG_00019",
                            "EL_ENG_00124")
    testing = testing.filter(_.refMentionIds.intersect(sampleTestSet.toSeq).size > 0)
    */
    
    val doubleTruthedCDEs = testing.filter(de => de.refMentionIds.toSet.size > 1)
    var doubleCount = 0
    for (e <- doubleTruthedCDEs) {
      doubleCount += e.refMentionIds.size; println("Refs: " + e.refMentionIds.mkString(" "))
    }
    println("Number of entities with different refs: " + doubleTruthedCDEs.size + " number of mentions affected: " + doubleCount)
    if (includeSupplementalMentionsForTesting) testing = testing ++ nonRefsOnly //testingNonRefs
    println("Non refs size: " + nonRefsOnly.size + ". Refs size: " + refsOnly.size)
    println("Training non refs size: " + trainingNonRefs.size + ". Testing non refs size: " + testingNonRefs.size)
    println("Training size: " + training.size + " num with refs: " + training.filterNot(_.refMentionIds.isEmpty).size)
    println("Testing size: " + testing.size + " num with refs: " + testing.filterNot(_.refMentionIds.isEmpty).size)

    if (wikipediaFileOption.isDefined) println("Loading Wikipedia") else println("No Wikipedia")
    val wikiEntitiesOpt = wikipediaFileOption.map(f => Loader.deserializeWikipediaEntities(f, populateNameBags = true, populateMentionBags = false, populateContextBags = false).toSeq)
    if (wikiEntitiesOpt.isDefined) println("Number of Wikipedia entities: " + wikiEntitiesOpt.get.size)

    println("About to train")
    //val trainer = new WikiCrossDocCoreferencer(limitMentionsToCanopiesOfQueries, useNameBags, useMentionBags, useContextBags, idfOption, topicsOption, wikiEntitiesOpt) with TrainableHierarchicalCoref {
    val trainer = new WikiCrossDocCoreferencer(limitMentionsToCanopiesOfQueries, useNameBags, useMentionBags, useContextBags, idfOption, topicsOption) with TrainableHierarchicalCoref {
      numStepsPerRound = trainSteps
      numTrainingRounds = trainIters
      this.useEntityLinking = trainWithEntityLinking
    }
    trainer.process(training.toIterator, refKB)

    //val modelFile = "/iesl/canvas/absmasti/xcoref/trainedModel"
    //BinarySerializer.serialize(trainer.model, modelFile)

    //val modelFile = "/iesl/canvas/absmasti/xcoref/trainedModel"
    //val model2 = KBEntityProcessingUtils.getTrainableModel
    //BinarySerializer.deserialize(model2, modelFile)

    println("About to test")
    val predicter = new WikiCrossDocCoreferencer(limitMentionsToCanopiesOfQueries, useNameBags, useMentionBags, useContextBags, idfOption, topicsOption, wikiEntitiesOpt) with HierarchicalCoref {
      override def numSteps(numKBMentions: Int, numRefEntities: Int): Int = testSteps

      _model = trainer.model
      this.useEntityLinking = testWithEntityLinking

      override def predict(kbMentions: Seq[KBEntity], refEntities: Seq[KBEntity]): Seq[KBEntity] = {
        val result = super.predict(kbMentions, refEntities)
        val evalMentions = result.filter((m: KBEntity) => m.isObserved && m.groundTruth != None)
        println("EVALUATING Testing")
        EntityLinking.runWikiLinksEval(evalMentions)
        //Evaluator.evaluate(evalMentions)
        result
      }
    }
    val cdcs = predicter.process(testing.toIterator, refKB)
    cdcs.savePredictionsToFile("out/el.pred.tab", refKB)

    //    Evaluator.evaluate(entities)
    //println("EVALUATING")
    //Evaluator.evaluate(testing.filterNot(_.refMentionIds.isEmpty))
  }

  def main(args: Array[String]) {
    trainAndTest()
    System.exit(0)

    val referenceLocation = "/home/aravind/UMASS/IndependentStudy/tackbp/reference/2009"
    println("Training and testing.")
    println("Loading Ref KB")
    val refKB = new RefKB("el")
    loadQueries(refKB, false)
    loadAnnotations(refKB, false)
    println("Filling entity data")
    Loader.fillEntityData(refKB, referenceLocation)
  }
}

object Baselines {
  //TODO: point to the string normalizer used to create the bags just in case it changes

  def evaluateEntityLinkingBaseline(model: Model, allEntities: Seq[KBEntity]): Unit = {
    val targets: ArrayBuffer[KBEntity] = {
      val result = new ArrayBuffer[KBEntity]
      result ++= allEntities.filter((e: KBEntity) => {
        e.isEntity.booleanValue && e.attr[SourceBagVar].value("wp") > 0
      })
      /*
      for(e <- wikies)if(e.isMention.booleanValue){
        val root = newEntity
        EntityUtils.propagateBagUp(e,root)(null) //this is a hack for better speed, we'll remove these laters
      }
      */
      result
    }
    val entityStream: Seq[KBEntity] = allEntities.filter((e: KBEntity) => {
      e.isEntity.booleanValue && e.attr[SourceBagVar].value("wp") == 0
    })
    println()
    println(targets.size)
    println(entityStream.size)
    evaluateEntityLinkingBaseline(model, entityStream, targets)
  }

  def evaluateEntityLinkingBaseline(model: Model, entityStream: Seq[KBEntity], preknownEntities: Seq[KBEntity]): Unit = {
    //    val truth = Evaluator.WithinWikiLinks.getTrueMap(entityStream)
    val baseline1PredictionMap = getEntityLinkingPrediction(model, entityStream, preknownEntities, false)
    val baseline2PredictionMap = getEntityLinkingPrediction(model, entityStream, preknownEntities, true)
    println(baseline1PredictionMap)
    println(baseline2PredictionMap)
    //    println(CorefEvaluator.evaluate(baseline1PredictionMap, truth))
    //    println(CorefEvaluator.evaluate(baseline2PredictionMap, truth))
  }

  protected def getEntityLinkingPrediction(model: Model, entityStream: Iterable[KBEntity], preknownEntities: Seq[KBEntity], updateTargets: Boolean): GenericEntityMap[String] = {
    val result = new GenericEntityMap[String]
    val id2long = new HashMap[String, Long]
    var nextId = 0L
    val masterDiff = new DiffList
    for (m <- entityStream) {
      val matched = pickBestTarget(model, m, preknownEntities, updateTargets, masterDiff)
      if (matched == None) {
        result.addMention(m.docId + "_" + m.mId, id2long.getOrElseUpdate(m.id.toString, {
          nextId += 1L
          nextId
        }))
      } else {
        for (target <- matched) {
          result.addMention(m.docId + "_" + m.mId, id2long.getOrElseUpdate(target.id.toString, {
            nextId += 1L
            nextId
          }))
        }
      }
    }
    //    if (masterDiff.size>0)masterDiff.undo
    result
  }

  def pickBestTarget(model: Model, m: KBEntity, targetEntities: Seq[KBEntity], updateTargets: Boolean, masterDiff: DiffList): Option[KBEntity] = {
    var max = 0.0
    var best: Option[KBEntity] = None
    var bestD: Option[DiffList] = None
    for (e <- targetEntities) {
      val d = new DiffList
      cc.factorie.app.nlp.hcoref.HierEntityUtils.linkChildToParent(m, e)(d)
      //val root = KBEntity.newEntity("")
      //cc.factorie.app.nlp.hcoref.HierEntityUtils.linkChildToParent(m,root)(d)
      //cc.factorie.app.nlp.hcoref.HierEntityUtils.linkChildToParent(e,root)(d)
      val score = d.scoreAndUndo(model)
      if (score > max) {
        max = score
        best = Some(e)
        bestD = Some(d)
      }
    }
    if (bestD.isDefined) masterDiff ++= bestD.get
    if (updateTargets && bestD.isDefined) bestD.get.redo
    best
  }

  def main(args: Array[String]) {
    val referenceLocation = "/iesl/canvas/sameer/tackbp/reference/2009"
    val docsFile = "/iesl/canvas/sameer/tackbp/source/json/el/el.docs.comb.gz"
    val wpFile = "/iesl/canvas/mwick/workspace/tackbp/resources/wp.canopy-el-and-el2013.json.gz"
    //val referenceLocation = "/home/aravind/UMASS/IndependentStudy/tackbp/reference/2009"
    //val docsFile = "/home/aravind/UMASS/IndependentStudy/tackbp/source/json/el/el.docs.comb.gz"
    //val wpFile = "/home/aravind/UMASS/IndependentStudy/tackbp/resources/wikipedia/wp.canopy-el-and-el2013.json.gz"
    val entityLinkingModel = KBEntityProcessingUtils.getTrainableModel
    println("Training and testing.")
    println("Loading Ref KB")
    val refKB = new RefKB("el")
    EntityLinking.loadQueries(refKB, false)
    EntityLinking.loadAnnotations(refKB, false)
    println("Filling entity data")
    Loader.fillEntityData(refKB, referenceLocation)
    println("ref# " + refKB.entities.size)
    println("Loading TACKBP documents")
    val docs = DEFTJson.deserializeFile(docsFile).toIndexedSeq
    val docEntities = docs.flatMap(_.attr[DocEntities]).filter(_.useForCoreference).toSeq
    val wpEntities = Loader.deserializeWikipediaEntities(wpFile).toSeq
    var progress = 0
    val kbMentions = docEntities.grouped(1000).flatMap(
      g => {
        progress += 1
        print(".")
        if (progress % 50 == 0) println(progress)
        g.par.map(de => CrossDocEntityUtils.newMention(de, populateNameBag = true, populateMentionBag = true, populateContextBag = true, idfOpts = None, topicsOpts = None, topicsIDFOption = None, Some(refKB))).seq
      }
    ).toSeq
    val allEntities = kbMentions ++ wpEntities
    evaluateEntityLinkingBaseline(entityLinkingModel, allEntities)
  }
}
