package cc.factorie.xcoref

import cc.factorie.app.nlp.hcoref.HierEntity
import scala.collection.mutable.{ArrayBuffer, HashSet, HashMap}
import scala.collection.mutable
import java.io.PrintWriter
import cc.factorie.optimize.{GradientOptimizer, SampleRankTrainer, ParameterAveraging, AdaGrad}
import cc.factorie.model.Model

trait CoreferenceInferencer[T <: HierEntity] {
  def setEntities(es: Iterable[T])

  def getEntities: Seq[T]

  def getTopLevelEntities: Seq[T] = getEntities.filter(e => e.isRoot && e.isConnected)

  def getDeletedEntities: Seq[T]

  def infer: Unit
}

class DocEntityData(val id: String, val rms: Seq[String], val rsv: Seq[String], val et: TackbpEntityType.Value) {
  def this(de: DocEntity, et: TackbpEntityType.Value) =
    this(de.id, de.refMentionIds, de.mentions.flatMap(_.refSlotValueId), et)
}

trait CrossDocEntities {
  def crossDocEntity(id: String): CrossDocEntity

  def crossDocEntities: Seq[CrossDocEntity]

  def refMention(refMentionId: String): Option[CrossDocEntity]

  def refEntity(refEntityId: String): Option[CrossDocEntity]

  def refSlotValueID(refSlotValueId: String): Option[CrossDocEntity]

  def docEntityParent(deId: String): CrossDocEntity

  def forString(string: String): Seq[CrossDocEntity]

  def crossDocEntityForWikiId(id: Int): Option[CrossDocEntity] = {
    None //todo: implement this
  }

  def savePredictionsToFile(filename: String, ref: RefKB): Unit = {
    val writer = new PrintWriter(filename)
    val refsInHand = new HashSet[String]
    for (cde <- crossDocEntities) {
      for (m <- cde.refMentions(ref).filter(m => /*m.isEL &&*/ m.source == DataSource.evaluation)) {
        val entityId = cde.refEntityId.getOrElse("NIL" + cde.id)
        //if(cde.refEntityId!=None)refsInHand += entityId
        refsInHand += m.id
        writer.println("%s\t%s\t%f" format(m.id, entityId, 0.99999))
      }
    }
    println("Head of ids found: " + refsInHand.head)
    println("Coref'd " + refsInHand.size + ", but should've coref'd " + ref.elEvaluationQueries.size + " mentions.")
    val missedIds = ref.elEvaluationQueries.map(_.id).toSet -- refsInHand
    if (missedIds.size > 0) {
      println("  Missed: " + missedIds.size + ". predicting singletons for missed mentions")
      println("  head of missed: " + missedIds.head.toString)
      for (missed <- missedIds) {
        writer.println("%s\t%s\t%f" format(missed, "NIL" + missed, 0.99999))
      }
    }
    writer.flush()
    writer.close()
  }

  def saveToJson(filename: String): Unit = {
    DEFTJson.serialize(this, filename)
  }
}

/**
 * Implementation of the CrossDocEntities that uses HashMaps to represent the output
 * Should be used after deserialization from Mongo, right before Relation Extraction
 * If running in-memory, should be used right after serialization
 * @author sameer
 */
class CrossDocEntitiesImpl(cdcs: TraversableOnce[CrossDocEntity]) extends CrossDocEntities {
  val crossDocEntities: ArrayBuffer[CrossDocEntity] = new ArrayBuffer
  val refMentionMap = new HashMap[String, CrossDocEntity]
  val crossDocCorefMap = new HashMap[String, CrossDocEntity]
  val docEntityCorefMap = new HashMap[String, CrossDocEntity]
  val refEntityMap = new HashMap[String, CrossDocEntity]
  val freebaseIdMap = new HashMap[String, CrossDocEntity]
  val refSlotValueMap = new HashMap[String, CrossDocEntity]
  val stringsMap = new HashMap[String, Seq[CrossDocEntity]]
  val maps = Seq(freebaseIdMap, refMentionMap, crossDocCorefMap, docEntityCorefMap,
    refEntityMap, refSlotValueMap, stringsMap)

  initFromMemory(cdcs)

  def this(jsonFileaname: String) = this(DEFTJson.deserializeCDEs(jsonFileaname))

  def initFromMemory(cdcs: TraversableOnce[CrossDocEntity]) = {
    crossDocEntities.clear
    crossDocEntities ++= cdcs
    maps.foreach(_.clear)
    for (cdc <- crossDocEntities) {
      cdc.refMentionIds.foreach(rm => refMentionMap(rm) = cdc)
      assert(!crossDocCorefMap.contains(cdc.id), cdc.id + " already in the map")
      crossDocCorefMap(cdc.id) = cdc
      cdc.refEntityId.foreach(re => refEntityMap(re) = cdc)
      cdc.refSlotValueIds.foreach(rsv => refSlotValueMap(rsv) = cdc)
      cdc.docEntityIds.foreach(deid => docEntityCorefMap(deid) = cdc)
      for (str <- cdc.strings) {
        stringsMap(str) = stringsMap.getOrElse(str, Seq.empty) ++ Seq(cdc)
      }
      cdc.freebaseId.foreach(fb => {
        //if(freebaseIdMap.contains(fb)) println("cdcs already contained " + fb + " linked to " + freebaseIdMap(fb).id + ", replacing with " + cdc.id);
        freebaseIdMap(fb) = cdc
      })
      //stringsMap.foreach(s_cdes => println("%s\t->\t%d".format(s_cdes._1, s_cdes._2.size)))
    }
    println("XDES LOADED : " + crossDocEntities.size)
    println("      ids : " + crossDocCorefMap.size)
    println("    docEs : " + docEntityCorefMap.size)
    println("    fbIds : " + freebaseIdMap.size)
    println("  strings : " + stringsMap.size)
    println("    refMs : " + refMentionMap.size)
    println("    refEs : " + refEntityMap.size)
    println("    refSs : " + refSlotValueMap.size)
  }

  def forString(string: String) = {
    //if (!stringsMap.contains(string)) println("CDCS WARNING: string " + string + " not found.")
    stringsMap.getOrElse(string, Seq.empty)
  }

  def refMention(refMentionId: String) = {
    if (!refMentionMap.contains(refMentionId)) println("CDCS WARNING: ref mention " + refMentionId + " not found.")
    refMentionMap.get(refMentionId)
  }

  def freebaseId(freebaseId: String) = {
    //if (!freebaseIdMap.contains(freebaseId)) println("CDCS WARNING: freebaseId " + freebaseId + " not found.")
    freebaseIdMap.get(freebaseId)
  }

  def crossDocEntity(id: String) = {
    assert(crossDocCorefMap.contains(id), id + " not found in cross doc entities.")
    crossDocCorefMap(id)
  }

  def docEntityParent(deId: String) = {
    if (!docEntityCorefMap.contains(deId)) println("CDCS WARNING: doc entity " + deId + " not found.")
    docEntityCorefMap(deId)
  }

  def refEntity(refEntityId: String) = {
    //if (!refEntityMap.contains(refEntityId)) println("CDCS WARNING: ref entity " + refEntityId + " not found.")
    refEntityMap.get(refEntityId)
  }

  def refSlotValueID(refSlotValueId: String) = refSlotValueMap.get(refSlotValueId)
}

trait CrossDocCoreferencer {
  def performCoref(docEntities: Iterator[DocEntity], ref: RefKB): Iterator[CrossDocEntity]

  def process(docEntities: Iterator[DocEntity], ref: RefKB): CrossDocEntities = {
    val cdcs = performCoref(docEntities, ref)
    postProcess(cdcs)
  }

  def postProcess(cdcs: Iterator[CrossDocEntity]): CrossDocEntities = {
    new CrossDocEntitiesImpl(cdcs.toSeq)
  }

  def entityType(overall: Seq[TackbpEntityType.Value]): TackbpEntityType.Value = {
    val et = new HashMap[TackbpEntityType.Value, Int]
    for (t <- overall) {
      et(t) = et.getOrElse(t, 0) + 1
    }
    if (et.size == 0) TackbpEntityType.UKN
    else if (et.size == 1) et.head._1
    else et.filter(_._1 != TackbpEntityType.UKN).maxBy(_._2)._1
  }

  def entityType(rms: Iterable[String], overall: Seq[TackbpEntityType.Value], kb: RefKB): TackbpEntityType.Value = {
    val et = new HashMap[TackbpEntityType.Value, Int]
    if (rms.size > 0) {
      rms.flatMap(rm => kb.mentionOpt(rm)).foreach(m => {
        val t = m.entityType.getOrElse(TackbpEntityType.UKN)
        et(t) = et.getOrElse(t, 0) + 1
      })
    } else {
      for (t <- overall) {
        et(t) = et.getOrElse(t, 0) + 1
      }
    }
    if (et.size == 0) TackbpEntityType.UKN
    else if (et.size == 1) et.head._1
    else et.filter(_._1 != TackbpEntityType.UKN).maxBy(_._2)._1
  }
}

abstract class WikiCrossDocCoreferencer(val restrictToQueryCanopies: Boolean = true, val useNameBags: Boolean = true, val useMentionBags: Boolean = true, val useContextBags: Boolean = true, idfOptsIn: Option[(Double, HashMap[String, Double])] = None, topicsOption: Option[HashMap[String, Array[(String, Double)]]] = None, wikipediaOption: Option[Seq[TACKBEntity]] = None) extends CrossDocCoreferencer {
  val _topicsIDFOption: Option[(Double, HashMap[String, Double])] = None
  //topicsOption.map(to => TFIDFUtils.getTopicIDFs(to))
  var useEntityLinking = false

  def model: EntityLinkingModel

  protected val title2wiki = new HashMap[String, KBEntity]
  protected var numWikiMatches = 0
  protected var numWikiMisses = 0
  for (wikis <- wikipediaOption) for (e <- wikis) {
    println("Adding title: " + e.canonicalName.value)
    title2wiki += e.canonicalName.value -> e
  }

  def wpToKBEntity(wpTitleWithUScore: String): Option[KBEntity] = {
    val wpTitle = wpTitleWithUScore.replaceAll("_", " ")
    val result = title2wiki.get(wpTitle)
    if (result == None) {
      println("Missed: " + wpTitle)
      numWikiMisses += 1
    } else {
      println("Matched: " + wpTitle)
      numWikiMatches += 1
    }
    result
  }

  def persistWikipediaEntity(kbe: KBEntity): Unit = {}

  def predict(kbMentions: Seq[KBEntity], refEntities: Seq[KBEntity]): Seq[KBEntity]

  def performCoref(docEntities: Iterator[DocEntity], ref: RefKB) = {
    // convert refEntities to kb entities
    var docEntitiesSeq = docEntities.toSeq
    val refEntitiesSeq = ref.entities.toSeq
    if (restrictToQueryCanopies) {
      println("Restricting mentions to query canopies")
      val queryCanopies = CrossDocEntityUtils.getQueryCanopy(docEntitiesSeq)
      println("  initial doc-ent#: " + docEntitiesSeq.size)
      docEntitiesSeq = CrossDocEntityUtils.restrictToQueryCanopies(docEntitiesSeq, queryCanopies)
      println("  after restricting doc-ent#: " + docEntitiesSeq.size)
      println("  initial ref-ent#: " + refEntitiesSeq.size)
      //refEntitiesSeq = CrossDocEntityUtils.restrictRefsToQueryCanopies(refEntitiesSeq,queryCanopies)
      //println("  after restricting ref-ent#: "+refEntitiesSeq.size)
    }
    var progress = 0
    val refEntities = refEntitiesSeq.filterNot(_.isNil).toSeq.grouped(1000).flatMap(
      g => {
        progress += 1
        print(".")
        if (progress % 50 == 0) println(progress)
        g.par.map(e => CrossDocEntityUtils.newMention(e, populateNameBag = useNameBags, populateMentionBag = useMentionBags, populateContextBag = useContextBags, idfOpts = idfOptsIn, topicsOpts = topicsOption, topicsIDFOption = _topicsIDFOption)).seq
      }
    ).toSeq
    progress = 0
    val refNilEntities = refEntitiesSeq.filter(_.isNil).toSeq.grouped(1000).flatMap(
      g => {
        progress += 1
        print(".")
        if (progress % 50 == 0) println(progress)
        g.par.map(e => CrossDocEntityUtils.newMention(e, populateNameBag = useNameBags, populateMentionBag = useMentionBags, populateContextBag = useContextBags, idfOpts = idfOptsIn, topicsOpts = topicsOption, topicsIDFOption = _topicsIDFOption)).seq
      }
    ).toSeq
    /*val refEntities = ref.entities.filterNot(_.isNil).toSeq.par.map(e => CrossDocEntityUtils.newMention(e,populateNameBag=true,populateMentionBag=true,populateContextBag=true,idfOpts=idfOptsIn,topicsOpts=topicsOption,topicsIDFOption=_topicsIDFOption)).seq
    val refNilEntities = ref.entities.filter(_.isNil).toSeq.par.map(e => CrossDocEntityUtils.newMention(e,populateNameBag=true,populateMentionBag=true,populateContextBag=true,idfOpts=idfOptsIn,topicsOpts=topicsOption,topicsIDFOption=_topicsIDFOption)).seq.filter(_.canonicalName.value.length>0)
    val noTopics = (refEntities ++ refNilEntities).filter(_.bagOfTopics.size==0).toSeq
    println("There are "+noTopics.size +" ref entities with no topics. About to extend the map.")
    if(noTopics.size>0 && _topicsIDFOption != None){
      TFIDFUtils.augmentDocThetaMap(noTopics,"resources/elinking.lda.phi","resources/elinking.lda.docs.theta.gz.aug.gz","/iesl/canvas/sameer/tackbp/resources/idfMaps/idf.gz")
      println("DONE AUGMENTING")
    }*/
    println("Printing ref entities (" + refEntities.size + ")")
    EntityUtils.prettyPrint(refEntities, true)

    val refMap = new mutable.HashMap[Any, KBEntity]
    (refEntities ++ refNilEntities).foreach(kbe => refMap(kbe.attr[RefEntity].id) = kbe)
    println("xdoc ref: num mentions = " + refEntities.size)
    // read in wikipedia entity
    /*    if(useMentionBags || useContextBags){
          println("Adding features for ref entities: "+refEntities.size)
          for(e <- (refEntities++refNilEntities).toSeq.par)KBEntityProcessingUtils.addFeatures(Seq(e),topicsOption)
        }*/
    //KBEntityProcessingUtils.addFeatures(refEntities++refNilEntities,topicsOption)

    for (refKBE <- refEntities) {
      refKBE.attr[RefEntity].refData.foreach(d =>
        wpToKBEntity(d.wikiTitle).foreach(wikiKBE => {
          EntityUtils.linkChildToParent(wikiKBE, refKBE)
          wikiKBE.moveable = false
          persistWikipediaEntity(wikiKBE)
        })
      )
    }
    println("Num wiki matches: " + numWikiMatches + " num misses: " + numWikiMisses)
    //for(wikiEntities <- wikipediaOption){WikiCorefComm.getAndLinkWiki2Ref(refEntities,Some(wikiEntities))}
    //WikiCorefComm.getAndLinkWiki2Ref(refEntities)
    //val extraWikipediaEntities = WikiCorefComm.getWikiEnts(refNilEntities)
    val extraWikipediaEntities = Seq.empty[TACKBEntity]
    // convert doc entities to kb entities
    progress = 0
    val kbMentions = (docEntitiesSeq.toSeq.grouped(1000).flatMap(
      g => {
        progress += 1;
        print(".");
        if (progress % 50 == 0) println(progress)
        g.par.map(de => CrossDocEntityUtils.newMention(de, populateNameBag = useNameBags, populateMentionBag = useMentionBags, populateContextBag = useContextBags, idfOpts = idfOptsIn, topicsOpts = topicsOption, topicsIDFOption = _topicsIDFOption, Some(ref))).seq
      }
    ).toSeq)
    val docEntityMap = new mutable.HashMap[Any, DocEntityData]()
    for (kbm <- kbMentions) {
      docEntityMap(kbm.id) = kbm.attr[DocEntityData]
      val refEntityIds = kbm.attr[DocEntityData].rms.flatMap(rmid => {
        val refMention = ref.mentionOpt(rmid)
        if (refMention.isDefined) {
          if (false && refMention.get.source != DataSource.evaluation) {
            refMention.get.entity.map(refEnt => refEnt.id)
          } else None
        } else {
          println(s"ref mentions ${rmid} not found in ParameterizedBoW2Compatibility the refKB")
          None
        }
      }).distinct
      //assert(refEntityIds.size <= 1, s"More than one refEntity found for ${kbm.attr[DocEntityData].id}: ${refEntityIds.mkString(", ")}")
      if (refEntityIds.size > 1) println(s"WARNING: More than one refEntity found for ${kbm.attr[DocEntityData].id}: ${refEntityIds.mkString(", ")}")
      for (refEntityId <- refEntityIds.headOption) {
        println("Locking in a kb to a ref: " + refEntityId)
        val refEntityKBE = refMap(refEntityId)
        EntityUtils.linkChildToParent(kbm, refEntityKBE)
        kbm.moveable = false
      }
    }
    println("#KBMENTION truths: " + kbMentions.flatMap(_.groundTruth).toSet.size + " num w/ truths: " + kbMentions.filter(_.groundTruth != None).size)
    val mentions = (kbMentions.filter(m => m.isMention.booleanValue && m.isEntity.booleanValue) ++ refNilEntities)
    var errors = 0
    /*
    for(m <- mentions){
      if(m.groundTruth != None && !m.id.startsWith("EL")){
        println("ERROR: mention with label "+m.groundTruth.get+" has unofficial query id: "+m.id)
        errors += 1
      }
    }*/
    assert(errors == 0)
    println("#MENTION truths: " + mentions.flatMap(_.groundTruth).toSet.size + " num w/ truths: " + mentions.filter(_.groundTruth != None).size)
    println("xdoc: num mentions = " + mentions.size)
    println("xdoc: first mention = " + mentions.head)
    // call inferencer on kb entities
    val result = predict(mentions, extraWikipediaEntities ++ refEntities.map(_.entityRoot.asInstanceOf[TACKBEntity]))
    //debug(cdcs.crossDocEntities,refKB,docs.flatMap(_.attr[DocEntities]).filter(_.useForCoreference).filterNot(_.refMentionIds.isEmpty))
    // convert kbentities to xdoc entities
    println("xdoc: num resulting entities = " + result.size)
    // convert back to CrossDocEntities
    val xdocEntities = result.filter(_.isRoot).map(kbe => CrossDocEntityUtils.fromKBEntity(kbe))
    val xdocMap = new mutable.HashMap[String, CrossDocEntity]()
    xdocEntities.foreach(cde => xdocMap(cde.id) = cde)
    println("xdoc: num xdoc entities = " + xdocEntities.size)
    // fill xdoc entities with doc entities
    println("NUM OBSERVED: " + result.filter(_.isObserved).size)
    for (child <- result.filter(_.isObserved)) {
      //println("  child id: "+child.id)
      val cde = xdocMap(child.entityRoot.id.toString)
      docEntityMap.get(child.id).foreach(ded => {
        //de.crossDocEntity = cde
        cde.docEntityIds += ded.id
        cde.refMentionIds ++= ded.rms
        cde.refSlotValueIds ++= ded.rsv
        // TODO: sameer: entity type cannot be computed incrementally
        cde.entityType = TackbpEntityType.UKN
      })
      refMap.get(child.id).filterNot(_.id.toString.startsWith("NIL")).foreach(re => {
        assert(cde.refEntityId.isEmpty, "Assertion failed because there are multiple ref-entities in this cross-doc entity.")
        cde.refEntityId = Some(re.id.toString)
        //println("  re.id:"+re.id)
      })
      //if(!refMap.contains(child.id)){
      //  println("  no ref entity found for child: "+child.id)
      //}
    }
    println("Num xdocEntities: " + xdocEntities.size)
    //println("Num gt0 xdocEntities: "+xdocEntities.filter(_.docEntities.size > 0).size)
    //
    //debug
    println("DEBUGGING OFF")
    //println("\n===========DEBUGGING===========")
    //EntityLinking.debug(xdocEntities.filter(_.docEntityIds.size > 0),ref,kbMentions,refEntities,model)
    //println("===========DONE DEBUGGING===========")
    xdocEntities.filter(_.docEntityIds.size > 0).iterator
  }
}

trait TrainableHierarchicalCoref extends WikiCrossDocCoreferencer {
  val model: EntityLinkingModel = KBEntityProcessingUtils.getTrainableModel
  //val model:EntityLinkingModel = KBEntityProcessingUtils.getModelWithTopics
  var numTrainingRounds = 16
  var numStepsPerRound = 200000

  def predict(kbMentions: Seq[KBEntity], refEntities: Seq[KBEntity]): Seq[KBEntity] = {
    var trainEntities = kbMentions ++ refEntities
    //EntityUtils.prettyPrint(trainEntities,false)
    println("Running coref trainer on " + trainEntities.size)
    println("Initial accuracy")
    //EntityLinking.runWikiLinksEval(trainEntities)
    println("Visualizing ground truth")
    //val truth = EntityLinking.collapseOnTruth(trainEntities.filter(_.groundTruth != None)).filter(_.isEntity.booleanValue)
    //EntityUtils.prettyPrint(truth)
    for (mention <- kbMentions) {
      assert(mention.isEntity.booleanValue, "Assert failed, id: " + mention.id) //,"Assertion failed, not an entity"+EntityUtils.entityStringPretty(e)})
      //      assert(mention.isMention.booleanValue && mention.isEntity.booleanValue)
    }
    //trainEntities = trainEntities.filter(_.groundTruth != None)
    println("Filtered down to labeled mentions only. Num labeled: " + trainEntities.size)
    //val updater = new AdaGrad(rate=0.1, delta=10) with ParameterAveraging //works well with original features
    val updater = new AdaGrad(rate = 1.0, delta = 100) with ParameterAveraging
    //val updater = new AdaGrad(rate=10.0, delta=100) with ParameterAveraging
    for (i <- 0 until numTrainingRounds) {
      val objectiveModel = new TrainingModel
      trainEntities.foreach(_.setParentEntity(null)(null))
      trainEntities = trainEntities.filter(_.isMention.booleanValue)
      if (useEntityLinking) trainEntities = link(kbMentions, refEntities, objectiveModel, updater)
      //val initer = new EntityLinkingInitializer(model,trainEntities,1)
      val trainSampler = new TACKBPEntitySampler(model) {
        override def objective = objectiveModel
        override def samplingSteps = numStepsPerRound
      }
      trainSampler.printUpdateInterval = 10000
      trainSampler.printDotInterval = 1000
      trainSampler.setEntities(trainEntities)
      trainSampler.timeAndProcess(0)
      val learner = new SampleRankTrainer(trainSampler, updater)
      learner.processContext(null, numStepsPerRound) // 3000
      println("TRAIN ENTITIES: ")
      EntityUtils.prettyPrint(trainEntities.filter(e => e.groundTruth != None).map(_.entityRoot.asInstanceOf[TACKBEntity]).distinct, false)
      EntityLinkingModel.printWeights(model, AffinityVectorDomain)
      try {
        evaluate(trainEntities, "Training set accuracy for round " + i)
      } catch {
        case e: Error => println("Evaluation error: " + e.toString)
      }
    }
    updater.setWeightsToAverage(model.parameters)

    println("Done training. Returning singletons.")
    //val result = (entityLinker.getEntities.toSeq ++ todoChangeThisToAnIterator ++ refEntities.toSeq).map(_.entityRoot.asInstanceOf[TACKBEntity]).distinct.flatMap(_.descendantsOfClass[TACKBEntity])
    trainEntities.foreach(_.setParentEntity(null)(null))
    trainEntities.filter(_.isMention.booleanValue)
  }

  def evaluate(entities: Seq[KBEntity], description: String): Unit = {
    println("\n-----" + description + "-----")
    EntityLinking.runWikiLinksEval(entities)
    //Evaluator.evaluate(entities)
  }

  def link(kbMentions: Seq[KBEntity], refEntities: Seq[KBEntity], objective: Model, optimizer: GradientOptimizer) = {
    println("About to do link training.")
    val entityLinker = new LinkTrainingSampler(model, refEntities, objective) {
      override def newEntity = new TACKBEntity("")
    }
    entityLinker.setEntities(Iterable.empty)
    val trainer = new LinkingSamplerRankTrainer(entityLinker, optimizer)
    trainer.train(kbMentions.filter(_.asInstanceOf[TACKBEntity].queryMentionType != None).iterator)
    //entityLinker.linkAll(kbMentions.filter(_.isEntity.booleanValue).iterator)
    val result = (entityLinker.getEntities.toSeq ++ kbMentions ++ refEntities.toSeq).map(_.entityRoot.asInstanceOf[TACKBEntity]).distinct.flatMap(_.descendantsOfClass[TACKBEntity])
    result
  }
}

trait HierarchicalCoref extends WikiCrossDocCoreferencer {
  var _model = KBEntityProcessingUtils.getModel

  def model: EntityLinkingModel = _model

  def setModel(m: EntityLinkingModel) = _model = m

  def numSteps(numKBMentions: Int, numRefEntities: Int): Int = 20000

  def predict(kbMentions: Seq[KBEntity], refEntities: Seq[KBEntity]): Seq[KBEntity] = {
    //KBEntityProcessingUtils.addFeatures(kbMentions ++ refEntities)
    //run(kbMentions.toIterator,refEntities,KBEntityProcessingUtils.getModel,20000)
    if (1 + 1 == 2) {
      val bestPossible = EntityLinking.collapseOnTruthAndOverlap(kbMentions ++ refEntities)
      EntityUtils.prettyPrint(bestPossible)
      EntityLinking.runWikiLinksEval(bestPossible)
      return bestPossible
    }
    if (useEntityLinking) run(link(kbMentions, refEntities), model, numSteps(kbMentions.size, refEntities.size))
    else run(kbMentions ++ refEntities, model, numSteps(kbMentions.size, refEntities.size))
  }

  def run(entities: Seq[KBEntity], model: EntityLinkingModel, numSamples: Int = 200000): Seq[KBEntity] = {
    val sampler = new TACKBPEntitySampler(model) {
      override def samplingSteps = numSamples

      //override def newEntity = new TACKBEntity("")
    }
    sampler.setEntities(entities)
    sampler.printUpdateInterval = 10000
    sampler.printDotInterval = 1000
    //sampler.setEntities(entities.drop(1000).take(1000))
    sampler.infer
    val result = sampler.getEntities
    println("Printing test entities.")
    EntityUtils.prettyPrint(result.filter(e => e.groundTruth != None).map(_.entityRoot.asInstanceOf[TACKBEntity]).distinct, false)
    EntityLinking.runWikiLinksEval(result)
    result
  }

  def link(kbMentions: Seq[KBEntity], refEntities: Seq[KBEntity]) = {
    println("LINKING")
    val entityLinker = new IndexedEntityLinkingInitializer(model, refEntities, None) {
      override def newEntity = new TACKBEntity("")
    }
    entityLinker.setEntities(Iterable.empty)
    entityLinker.linkAll(kbMentions.filter(_.isEntity.booleanValue).filter(_.asInstanceOf[TACKBEntity].queryMentionType != None).iterator)
    val result = (entityLinker.getEntities.toSeq ++ kbMentions ++ refEntities.toSeq).map(_.entityRoot.asInstanceOf[TACKBEntity]).distinct.flatMap(_.descendantsOfClass[TACKBEntity])
    result
  }
}

trait HierarchicalEntityLinking extends WikiCrossDocCoreferencer {
  val model: EntityLinkingModel = KBEntityProcessingUtils.getModel

  def predict(kbMentions: Seq[KBEntity], refEntities: Seq[KBEntity]): Seq[KBEntity] = {
    //KBEntityProcessingUtils.addFeatures(kbMentions ++ refEntities)
    /*
        if(1+1==2){
          println("WARNING, ABOUT TO GENERATE TOPICS INSTEAD OF DOING COREF.")
          val percentageThreshold=0.25
          object WordSeqDomain extends CategoricalSeqDomain[String]
          val model = DirectedModel()
          val lda = new LDA(WordSeqDomain, 400, 0.1, 0.01, 1)(model,random)
          val stopWords = TFIDFUtils.generateStopWordsFromIDF("/iesl/canvas/sameer/tackbp/resources/idfMaps/idf.gz",percentageThreshold)
          TFIDFUtils.inferTopicsForKBEntities((kbMentions ++ refEntities.flatMap(_.descendantsOfClass[KBEntity])).filter(_.isObserved).distinct.iterator,lda,100,stopWords)
          println("DONE CREATING TOPICS (NO COREF, EXITING)")
          System.exit(1)
          // def inferTopicsForKBEntities(entities:Iterator[KBEntity],lda:LDA,ldaPhiFile:String="resources/kbe.lda.phi.gz",ldaThetaFile:String="resources/kbe.lda.theta.gz",stopWords:HashSet[String]):LDA ={
        }
       */

    run(kbMentions.toIterator, refEntities, model, 0)
  }

  def run(kbMentions: Iterator[KBEntity], refEntities: Seq[KBEntity], model: EntityLinkingModel, numSamples: Int = 200000): Seq[KBEntity] = {
    println("\n=====Ref entities=====")
    //EntityUtils.prettyPrint(refEntities, true)
    val entityLinker = new IndexedEntityLinkingInitializer(model, refEntities, None) {
      override def newEntity = new TACKBEntity("")

      /*
      def getFromDoc(e:KBEntity,docId:String):Seq[KBEntity] ={
        val result = e.descendantsOfClass[KBEntity].filter(_.docId==docId).toSeq
        //assert(result.size==1)
        //result.head
        result
      }
      override def settings(c:Null) : SettingIterator = new SettingIterator {
        settingsCount += 1
        //println("Proposal count: " + proposalCount+" settings count: "+settingsCount+" mention name: "+streamedMention.canonicalName.value+" id: "+streamedMention.id)
        val changes = new scala.collection.mutable.ArrayBuffer[(DiffList)=>Unit]
        for(entityCandidate <- entityCandidates.asInstanceOf[TACKBEntity]){
          if(entityCandidate.bagOfDocs.value(streamedMention.docId)>0.0){
            val oldRefs = getRefs(e)
            for(or <- oldRef)
          }
          if(!entityCandidate.isMention){
            //println("MergingLeft")
            assert(entityCandidate.isEntity.booleanValue)
            changes += {(d:DiffList) => mergeLeft(entityCandidate, streamedMention)(d)}
          }else {
            //println("MergingUp")
            changes += {(d:DiffList) => mergeUp(streamedMention,entityCandidate)(d)}
          }
        }
        changes += {(d:DiffList) => {}} //give the sampler an option to reject all other proposals
        var i = 0
        def hasNext = i < changes.length
        def next(d:DiffList) = {val d = newDiffList; changes(i).apply(d); i += 1; d }
        def reset = i = 0
      }
      */
    }
    val todoChangeThisToAnIterator = kbMentions.toSeq
    entityLinker.setEntities(Iterable.empty)
    entityLinker.linkAll(todoChangeThisToAnIterator.filter(_.isEntity.booleanValue).iterator)
    println("About to return results")
    val result = (entityLinker.getEntities.toSeq ++ todoChangeThisToAnIterator ++ refEntities.toSeq).map(_.entityRoot.asInstanceOf[TACKBEntity]).distinct.flatMap(_.descendantsOfClass[TACKBEntity])
    //for(r <- result)println("    result id: "+r.id)
    result
  }
}

object WikiCrossDocCoreferencer {
  val DocumentSource = "tac_doc"
  val ReferenceSource = "tac_ref"
  val ContextWindow = 15
}
