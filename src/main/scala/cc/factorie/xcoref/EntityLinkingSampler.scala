package cc.factorie.xcoref

import cc.factorie.infer.Proposal
import scala.collection.mutable.{HashMap, ArrayBuffer, LinkedHashSet}
import cc.factorie._
import cc.factorie.optimize.{SampleRankExample, SampleRankTrainer, GradientOptimizer}
import cc.factorie.app.nlp.hcoref.{Dirty, HierCorefSampler}
import cc.factorie.variable.SettingIterator

class StreamingInitializer(model: Model) extends IndexedEntityLinkingInitializer(model, Seq.empty[KBEntity], None) {
  var lastProposals: Seq[Proposal[Null]] = null
  override val topk = 2

  override protected def getKeysForEntity(e: KBEntity): LinkedHashSet[String] = {
    val tokens = new LinkedHashSet[String]
    val etype = e.entityType.value
    val names = e.bagOfNames.value.iterator.toSeq.filter(_._2 > 0.1).map(_._1 + ":" + etype).reverse.take(topk)
    val topics = e.bagOfTopics.value.iterator.toSeq.filter(_._2 > 0.1).map(_._1).reverse.take(topk)
    for (name <- names) {
      for (topic <- topics) {
        tokens += name + ":" + topic
      }
    }
    tokens
  }

  override def initializeAttributesOfNewRoot(e1: KBEntity, e2: KBEntity, parentEntity: KBEntity)(implicit d: DiffList) {
    super.initializeAttributesOfNewRoot(e1, e2, parentEntity)(d)
    parentEntity.entityType := e1.entityType.value
  }

  def link(m: KBEntity) = {
    prepareForLinking(m)
    process(0)
    finishProcessing
  }

  def prepareForLinking(m: KBEntity) = {
    streamedMention = m
    //var root:KBEntity = streamedMention.entityRoot.asInstanceOf[KBEntity]
    entityCandidates = new ArrayBuffer[KBEntity]
    entityCandidates ++= getCandidateSet(streamedMention, index).map(_.entityRoot.asInstanceOf[KBEntity]).toSet
    //println("prepping linker for mention: "+m.id)
  }

  def finishProcessing {
    if (streamedMention.parentEntity != null) {
      updateEntityRootInIndex(streamedMention) //this will actually create a new entry in the index for the parent, but that's OK because the toSet call earlier in this function will unsure it's not redundantly computed during sampling.
    } else {
      targets += streamedMention
      updateEntityRootInIndex(streamedMention)
    }
  }

  override def proposals(c: Null) = {
    val result = super.proposals(c)
    lastProposals = result
    //println("#link-props: "+result.size)
    result
  }
}

class LinkTrainingSampler(model: Model, refEntities: Seq[KBEntity], objectiveModel: Model) extends StreamingInitializer(model) {
  //extends IndexedEntityLinkingInitializer(model, refEntities, None){
  override def objective = objectiveModel

  override def initializeAttributesOfNewRoot(e1: KBEntity, e2: KBEntity, parentEntity: KBEntity)(implicit d: DiffList) {
    super.initializeAttributesOfNewRoot(e1, e2, parentEntity)(d)
    parentEntity.entityType := e1.entityType.value
  }

  override def finishProcessing {
    if (lastProposals.size > 0) {
      val picked = pickProposal(lastProposals)
      if (objectiveModel != null && picked.objectiveScore < 0) picked.diff.undo
    }
    super.finishProcessing
  }
}

class LinkingSamplerRankTrainer(linker: LinkTrainingSampler, updater: GradientOptimizer) extends SampleRankTrainer[Null](sampler=linker, optimizer=updater) {
  def train(mentions: Iterator[KBEntity]): Unit = {
    var count = 0
    println("About to link-train")
    for (m <- mentions.filter(_.isEntity.booleanValue)) {
      linker.prepareForLinking(m)
      process(new SampleRankExample(null, linker))
      linker.finishProcessing
      //linker.pickProposal(linker.proposalsCached)
      //count += 1;if(count % 1000==0)print(".");if(count % 50000==0)println(count)
    }
  }
}


trait SamplingStatistics {
  //todo: move to factorie.app
  val shortDecimal = new java.text.DecimalFormat("0.0#")
  var printDotInterval = 1000
  var printUpdateInterval = 20000
  var proposalCount = 0
  protected var totalTime: Long = 0L
  protected var intervalTime: Long = 0L
  var printInfo = true
  var numAccepted = 0
  var numAcceptedInTimeWindow = 0
  var numDiffVars = 0L
  var numDiffVarsInWindow = 0
  var allDiffVarsInWindow = 0
  var numNonTrivialDiffs = 0

  def resetSamplingStatistics: Unit = {
    totalTime = System.currentTimeMillis
    intervalTime = totalTime
    numAccepted = 0
    numAcceptedInTimeWindow = 0
    numDiffVars = 0L
    numDiffVarsInWindow = 0
    allDiffVarsInWindow = 0
    numNonTrivialDiffs = 0
    proposalCount = 0
  }
}

class KBEntitySampler(model: Model) extends HierCorefSampler[KBEntity](model) with SamplingStatistics with CoreferenceInferencer[KBEntity] {
  this.temperature = 0.001
  protected var canopies = new HashMap[String, ArrayBuffer[KBEntity]]

  def samplingSteps: Int = 100000

  def newEntity = KBEntity.newEntity()

  def infer: Unit = timeAndProcess(samplingSteps)

  override def timeAndProcess(n: Int): Unit = {
    if (printInfo) println("About to take " + n + " samples.")
    totalTime = System.currentTimeMillis
    intervalTime = totalTime
    resetSamplingStatistics
    super.process(n)
  }

  override def addEntity(e: KBEntity): Unit = {
    super.addEntity(e)
    //for(cname <- e.canopyAttributes.map(_.canopyName)){
    for (cname <- e.canopyNames) {
      canopies.getOrElse(cname, {
        val a = new ArrayBuffer[KBEntity];
        canopies(cname) = a;
        a
      }) += e
    }
  }

  override def setEntities(ents: Iterable[KBEntity]): Unit = {
    canopies = new HashMap[String, ArrayBuffer[KBEntity]]
    super.setEntities(ents)
    canopies = canopies.filter(_._2.size > 1)
  }

  override def proposeMergeIfValid(entity1: KBEntity, entity2: KBEntity, changes: ArrayBuffer[(DiffList) => Unit]): Unit = {
    if (entity1.numMentionsInSubtree.value >= entity2.numMentionsInSubtree.value)
      super.proposeMergeIfValid(entity1, entity2, changes)
    else
      super.proposeMergeIfValid(entity2, entity1, changes)
  }

  /*override def proposeMergeIfValid(entity1:KBEntity,entity2:KBEntity,changes:ArrayBuffer[(DiffList)=>Unit]):Unit ={
    if (entity1.entityRoot.id != entity2.entityRoot.id){ //sampled nodes refer to different entities
      if(!isMention(entity1)){
        changes += {(d:DiffList) => mergeLeft(entity1,entity2)(d)} //what if entity2 is a mention?
      }
      else if(!isMention(entity2)){
        changes += {(d:DiffList) => mergeLeft(entity2,entity1)(d)}
      }
    }
  }*/
  override def nextEntityPair: (KBEntity, KBEntity) = {
    var (e1, e2) = super.nextEntityPair
    while (e1 != null && !e1.moveable) e1 = e1.parentEntity.asInstanceOf[KBEntity]
    while (e2 != null && !e2.moveable) e2 = e2.parentEntity.asInstanceOf[KBEntity]
    if (e1 == null || e2 == null) nextEntityPair else (e1, e2)
  }

  override def settings(c: Null): SettingIterator = new SettingIterator {
    val changes = new scala.collection.mutable.ArrayBuffer[(DiffList) => Unit]
    val (entityS1, entityS2) = nextEntityPair
    val entity1 = entityS1.getAncestor(random.nextInt(entityS1.depth + 1)).asInstanceOf[KBEntity]
    val entity2 = entityS2.getAncestor(random.nextInt(entityS2.depth + 1)).asInstanceOf[KBEntity]
    if (!entity1.moveable || !entity2.moveable || !entityS1.moveable || !entityS2.moveable) println("WARNING: attempting to move a moveable entity.")
    //if(entity1.source=="wp" || entity2.source=="wp" || entityS1.source=="wp" || entityS2.source=="wp")println("WP MENTION")
    val r1 = entity1.entityRoot.asInstanceOf[KBEntity]
    val r2 = entity2.entityRoot.asInstanceOf[KBEntity]
    if (r1.id != r2.id) {
      //sampled nodes refer to different entities
      var e1 = entityS1
      var e2 = entity2 //entityS2.getAncestor(random.nextInt(entityS2.depth+1)).asInstanceOf[KBEntity]
      if (entity1.source == "wp") {
        //println("Sampling WP")
        if (!r2.isMention) changes += {
          (d: DiffList) => mergeLeft(r2, entity1)(d)
        }
        else changes += {
          (d: DiffList) => mergeUp(entity1, r2)(d)
        }
      }
      if (entity2.source == "wp") {
        //println("Sampling WP")
        if (!r1.isMention) changes += {
          (d: DiffList) => mergeLeft(r1, entity2)(d)
        }
        else changes += {
          (d: DiffList) => mergeUp(entity2, r1)(d)
        }
      }
      while (e1 != null) {
        proposeMergeIfValid(e1, e2, changes)
        e1 = e1.parentEntity.asInstanceOf[KBEntity]
      }
      //if(!e1.eq(r1))proposeMergeIfValid(r1,r2,changes)

      if (entity1.parentEntity == null && entity2.parentEntity == null) {
        changes += {
          (d: DiffList) => mergeUp(entity1, entity2)(d)
        }
      }
      if (changes.size == 0) {
        //val r1 = entity1.entityRoot.asInstanceOf[KBEntity]
        //val r2 = entity2.entityRoot.asInstanceOf[KBEntity]
        if (!isMention(r2)) {
          proposeMergeIfValid(r2, entity1, changes)
          if (!r1.eq(entity1)) proposeMergeIfValid(r2, r1, changes)
        } else {
          //val r1 = entity1.entityRoot.asInstanceOf[KBEntity]
          if (!isMention(r1)) {
            proposeMergeIfValid(r1, entity2, changes)
          } else changes += {
            (d: DiffList) => mergeUp(entity1.entityRoot.asInstanceOf[KBEntity], entity2.entityRoot.asInstanceOf[KBEntity])(d)
          }
        }
      }
    } else {
      //sampled nodes refer to same entity
      if (entity1.numMentionsInSubtree.value > entity2.numMentionsInSubtree.value)
        changes += {
          (d: DiffList) => splitRight(entity2, entity1)(d)
        }
      else
        changes += {
          (d: DiffList) => splitRight(entity1, entity2)(d)
        }

      //if(entity1.parentEntity != null && !entity1.isObserved)
      //  changes += {(d:DiffList) => {collapse(entity1)(d)}} //Note that if collapse is called, it deletes enitty1 and sample attribute will fail.
    }

    //if collapse is called on entiy1, then it is possible that entity1 will not have any children.
    if (!entity1.isObserved && entity1.dirty.value > 0 && entity1.exists.booleanValue) changes += {
      (d: DiffList) => sampleAttributes(entity1)(d)
    }
    if (!entity1.entityRoot.isObserved && entity1.entityRoot.id != entity1.id && entity1.entityRoot.attr[Dirty].value > 0 && entity1.entityRoot.asInstanceOf[KBEntity].exists.booleanValue) changes += {
      (d: DiffList) => sampleAttributes(entity1.entityRoot.asInstanceOf[KBEntity])(d)
    }
    changes += {
      (d: DiffList) => {}
    }
    //give the sampler an option to reject all other proposals
    //println("CHANGES:"+changes.size);System.out.flush
    //if(changes.size<=1){println("ERROR");System.out.flush;System.exit(1)}
    var i = 0

    def hasNext = i < changes.length

    def next(d: DiffList) = {
      val d = newDiffList;
      changes(i).apply(d);
      i += 1;
      d
    }

    //def next(d:DiffList) = {val d = new DebugDiffList; changes(i).apply(d); i += 1; d}
    //override def next = {val d2 = new DebugDiffList; changes(i).apply(d2); i += 1; d2 }
    def reset = i = 0
  }

  def newDebugDiffList = new DebugDiffList()

  def sampleCanopyName(context: KBEntity): String = {
    if (context.id.toString.startsWith("NIL")) {
      //println("Sampling NIL entity's canopy: " + context.id.toString)
      context.id.toString
    } else {
      context.canopyAttributes.sampleUniformly(random).canopyName
    }
  }

  override def nextEntity(context: KBEntity = null.asInstanceOf[KBEntity]): KBEntity = {
    var result: KBEntity = null.asInstanceOf[KBEntity]
    val src = "wp"
    if (context == null) result = sampleEntity(entities)
    else {
      var count = 0
      val cname = sampleCanopyName(context)
      //val cname = context.canopyAttributes.sampleUniformly(random).canopyName
      val canopy = canopies.getOrElse(cname, {
        val c = new ArrayBuffer[KBEntity];
        c += context;
        c
      })
      result = if (canopy.size > 0) sampleEntity(canopy) else sampleEntity(entities) //{val c = new ArrayBuffer[E];c+=context;c})
      if (context != null && context.entityRoot.attr[SourceBagVar].value(src) > 0) {
        while (result != null && result.entityRoot.attr[SourceBagVar].value(src) > 0 && count < 10) {
          val cname = sampleCanopyName(context) //context.canopyAttributes.sampleUniformly(random).canopyName
          val canopy = canopies.getOrElse(cname, {
              val c = new ArrayBuffer[KBEntity];
              c += context;
              c
            })
          result = if (canopy.size > 0) sampleEntity(canopy) else sampleEntity(entities)
          count += 1
        }
      }
    }
    if (result == null) result = context
    result
  }

  protected def initializeAttributesOfNewRoot(e1: KBEntity, e2: KBEntity, parent: KBEntity)(implicit d: DiffList): Unit = {
    parent.attr[CanonicalRepresentationVar].set(e1.attr[CanonicalRepresentationVar].value)
    EntityUtils.createBagsForMergeUp(e1, e2, parent)(d)
  }

  //mcmc moves. todo: factor out structure preservation and bag propagation into a trait and move to factorie.app
  override def mergeLeft(left: KBEntity, right: KBEntity)(implicit d: DiffList): Unit = {
    val oldParent = right.parentEntity
    right.setParentEntity(left)(d)
    EntityUtils.propagateBagUp(right)(d)
    EntityUtils.propagateRemoveBag(right, oldParent)(d)
    structurePreservationForEntityThatLostChild(oldParent)(d)
  }

  override def mergeUp(e1: KBEntity, e2: KBEntity)(implicit d: DiffList): KBEntity = {
    val oldParent1 = e1.parentEntity
    val oldParent2 = e2.parentEntity
    val result = newEntity
    e1.setParentEntity(result)(d)
    e2.setParentEntity(result)(d)
    initializeAttributesOfNewRoot(e1, e2, result)(d)
    EntityUtils.propagateRemoveBag(e1, oldParent1)(d)
    EntityUtils.propagateRemoveBag(e2, oldParent2)(d)
    structurePreservationForEntityThatLostChild(oldParent1)(d)
    structurePreservationForEntityThatLostChild(oldParent2)(d)
    result
  }

  override def splitRight(left: KBEntity, right: KBEntity)(implicit d: DiffList): Unit = {
    val oldParent = right.parentEntity
    right.setParentEntity(null)(d)
    EntityUtils.propagateRemoveBag(right, oldParent)(d)
    structurePreservationForEntityThatLostChild(oldParent)(d)
  }

  def sampleAttributes(entity: KBEntity)(implicit d: DiffList) = {
    val representative = entity.childEntities.value.sampleUniformly(random)
    entity.attr[CanonicalRepresentationVar].set(representative.attr[CanonicalRepresentationVar].value)
    if (entity.attr[Dirty].value > 0) entity.attr[Dirty].--()(d)
    if (entity.parentEntity != null) entity.parentEntity.attr[Dirty].++()(d)
  }

  //diagnostics. todo: factor this out and move to factorie.app
  override def proposalHook(proposal: Proposal[Null]) = {
    super.proposalHook(proposal)
    //if(proposal.diff.size>0)println("DIFF SIZE: "+proposal.diff.size)
    if (proposal.diff.size > 0) {
      numAccepted += 1
      numAcceptedInTimeWindow += 1
      numDiffVars += proposal.diff.size.toLong
      numDiffVarsInWindow += proposal.diff.size
      numNonTrivialDiffs += 1
    }
    //if(proposal.diff.size>0 && proposal.modelScore>0){}
    proposalCount += 1
    if (printInfo) {
      if (proposalCount % printDotInterval == 0)
        print(".")
      if (proposalCount % printUpdateInterval == 0)
        printSamplingInfo
    }
  }

  def printSamplingInfo: Unit = {
    val pctAccepted = numAccepted.toDouble / proposalCount.toDouble * 100
    val instantPctAccepted = numAcceptedInTimeWindow.toDouble / proposalCount.toDouble * 100
    val timeDiffMil = (System.currentTimeMillis - totalTime).toInt
    val timeWindowMS = (System.currentTimeMillis - intervalTime).toInt
    var timeDiffSec = timeDiffMil / 1000
    var sampsPerSec: Int = -1
    var acceptedPerSec: Int = -1
    var instantSampsPerSec: Int = -1
    var instantAcceptedPerSec: Int = -1
    if (timeDiffMil != 0) {
      sampsPerSec = (proposalCount.toDouble / timeDiffMil.toDouble * 1000.0).toInt
      acceptedPerSec = (numAccepted.toDouble / timeDiffMil.toDouble * 1000.0).toInt
      instantSampsPerSec = (printUpdateInterval.toDouble / timeWindowMS.toDouble * 1000.0).toInt
      instantAcceptedPerSec = (numAcceptedInTimeWindow.toDouble / timeWindowMS.toDouble * 1000.0).toInt
    }
    println(" No. samples: " + proposalCount + ", %accepted: " + shortDecimal.format(pctAccepted) + " " + shortDecimal.format(instantPctAccepted) + ", time: " + (timeWindowMS / 1000) + "sec., total: " + (System.currentTimeMillis - totalTime) / 1000L + " sec.  Samples per sec: " + sampsPerSec + " " + instantSampsPerSec + " Accepted per sec: " + acceptedPerSec + " " + instantAcceptedPerSec + ". Avg diff size: " + (numDiffVarsInWindow) / (numNonTrivialDiffs + 1))
    intervalTime = System.currentTimeMillis
    numDiffVarsInWindow = 0
    allDiffVarsInWindow = 0
    numNonTrivialDiffs = 0
    numAcceptedInTimeWindow = 0

    //Evaluator.eval(getEntities)
    System.out.flush
  }
}

class TACKBPEntitySampler(model: Model, filterByQueryCanopies: Boolean = true) extends KBEntitySampler(model) {
  override def newEntity = new TACKBEntity("")

  override def setEntities(entities: Iterable[KBEntity]) {
    super.setEntities(entities)
    if (filterByQueryCanopies) {
      println("Setting " + entities.size + " found " + canopies.size + " canopies.")
      canopies = canopies.filter(c => c._2.filter(_.source == WikiCrossDocCoreferencer.DocumentSource).size >= 1) //no point in doing coref on clusters of ref mentions
      println("Filtered out canopies with only refs. Current number of canopies is: " + canopies.size)
    }
  }

  override def initializeAttributesOfNewRoot(e1: KBEntity, e2: KBEntity, parentEntity: KBEntity)(implicit d: DiffList) {
    super.initializeAttributesOfNewRoot(e1, e2, parentEntity)(d)
    parentEntity.entityType := e1.entityType.value
  }

  override def sampleCanopyName(context: KBEntity): String = {
    val exploit = random.nextDouble() <= 0.75
    //if(exploit && context.bagOfNames.size>0){
    //  val names = context.bagOfNames.value.iterator.toSeq.filter(x => x==x)
    //  if(names.size>0)names.sampleProportionally(_._2)._1 else super.sampleCanopyName(context)
    //}
    if (exploit && context.bagOfNames.size > 0) context.bagOfNames.value.iterator.toSeq.sampleProportionally(_._2)._1 else super.sampleCanopyName(context)
  }
}
