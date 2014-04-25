package cc.factorie.xcoref

import cc.factorie._
import scala.Some
import java.io.{PrintWriter, OutputStream}
import scala.collection.mutable.{LinkedHashSet, HashMap, ArrayBuffer}
import cc.factorie.infer.Proposal
import cc.factorie.variable.SettingIterator

class IndexedEntityLinkingInitializer(model: Model, val initialTargets: Seq[KBEntity], val out: Option[OutputStream] = None) extends KBEntitySampler(model) {
  val targets = new ArrayBuffer[KBEntity]
  var numCandidatesConsidered = 0L
  targets ++= initialTargets
  var printDebugInfo = false
  val topk = 4
  this.temperature = 0.001
  protected var streamedMention: KBEntity = null
  protected var entityCandidates = new ArrayBuffer[KBEntity]
  var pickedProposal: Proposal[Null] = null
  var bestNonTrivProposal: Proposal[Null] = null

  val index = {
    println("Building index....");
    val st = System.currentTimeMillis;
    val result = buildIndex(targets);
    println("building index took " + getElapsedSec(st) + " sec.");
    result
  }

  protected def buildIndex(entities: Seq[KBEntity]): HashMap[String, LinkedHashSet[KBEntity]] = {
    val result = new HashMap[String, LinkedHashSet[KBEntity]]
    for (e <- entities) updateEntityRootInIndex(e, result)
    result
  }

  protected def updateEntityRootInIndex(e: KBEntity, index: HashMap[String, LinkedHashSet[KBEntity]] = index) = {
    val tokens = getKeysForEntity(e)
    for (token <- tokens) index.getOrElseUpdate(token, new LinkedHashSet[KBEntity]) += e.entityRoot.asInstanceOf[KBEntity]
  }

  protected def getKeysForEntity(e: KBEntity): LinkedHashSet[String] = {
    val tokens = new LinkedHashSet[String]
    tokens ++= e.bagOfNames.value.iterator.toSeq.filter(_._2 > 1.0).map(_._1).toSet
    tokens ++= e.bagOfMentions.value.iterator.toSeq.filter(_._2 > 1.0).sortBy(_._2).reverse.take(topk).map(_._1).toSet
    tokens ++= e.contextBag.value.iterator.toSeq.filter(_._2 > 1.0).sortBy(_._2).reverse.take(topk).map(_._1).toSet
    tokens ++= e.bagOfTopics.value.iterator.toSeq.filter(_._2 > 0.1).map(_._1).toSet
    //tokens ++= e.bagOfMentions.value.iterator.toSeq.filter(_._2>0.005).sortBy(_._2).reverse.take(topk).map(_._1).toSet
    //tokens ++= e.contextBag.value.iterator.toSeq.filter(_._2>0.01).sortBy(_._2).reverse.take(topk).map(_._1).toSet
  }

  protected def getCandidateSet(e: KBEntity, index: HashMap[String, LinkedHashSet[KBEntity]] = index): LinkedHashSet[KBEntity] = {
    var result = new LinkedHashSet[KBEntity]
    val tokens = getKeysForEntity(e)
    for (token <- tokens) {
      val esetOpt = index.get(token)
      for (eset <- esetOpt) for (candidate <- eset) if (!(e eq candidate)) result += candidate
    }
    result
  }

  protected def getElapsedSec(startTimeMS: Long): String = ((System.currentTimeMillis - startTimeMS) / 1000L).toString + " sec"

  def writeResult(pw: PrintWriter, m: KBEntity): Unit = {
    if (!(m.entityRoot eq m)) {
      pw.write(m.id + " " + m.entityRoot.id)
      pw.flush()
    }
  }

  def linkEntity(m: KBEntity): Boolean = {
    assert(m.parentEntity == null)
    var foundLink = false
    streamedMention = m
    //var root:KBEntity = streamedMention.entityRoot.asInstanceOf[KBEntity]
    entityCandidates = new ArrayBuffer[KBEntity]
    entityCandidates ++= getCandidateSet(streamedMention, index).map(_.entityRoot.asInstanceOf[KBEntity]).toSet
    if (entityCandidates.size > 0) {
      process(0)
    }
    //if(!(root eq streamedMention.entityRoot.asInstanceOf[KBEntity])){
    if (streamedMention.parentEntity != null) {
      foundLink = true
      updateEntityRootInIndex(streamedMention) //this will actually create a new entry in the index for the parent, but that's OK because the toSet call earlier in this function will unsure it's not redundantly computed during sampling.
    } else if (initialTargets.size == 0) {
      targets += streamedMention
      updateEntityRootInIndex(streamedMention)
    }
    foundLink
  }

  def linkAll(mentionRootStream: Iterator[KBEntity]): Unit = {
    val startTime = System.currentTimeMillis
    println("Linking entities")
    val pwOpt = out.map(new PrintWriter(_))
    var count = 0
    var numLinked = 0
    var numCandidates = 0L
    for (m <- mentionRootStream) {
      assert(m.parentEntity == null)
      if (printDebugInfo) println("Attempting to link mention " + m.canonicalName.value + " id: " + m.id)
      //println("IsEntity: "+m.isEntity.booleanValue+"  Exists: "+m.exists.booleanValue)
      //EntityUtils.prettyPrintEntity(m,None)
      count += 1
      if (count % 1000 == 0) print(".")
      if (count % 50000 == 0) println(count)

      //linkEntity(m.entityRoot.asInstanceOf[KBEntity])
      val foundALink = linkEntity(m)
      numCandidates += entityCandidates.size.toLong
      if (foundALink) {
        numLinked += 1
        if (printDebugInfo) {
          println("\nLinked a mention! Score " + pickedProposal.modelScore)
          println("  " + streamedMention.canonicalName.value + ":{" + EntityUtils.bagToString(streamedMention.bagOfNames.value) + "}----->{" + EntityUtils.bagToString(streamedMention.entityRoot.asInstanceOf[KBEntity].bagOfNames.value) + "}")
          EntityUtils.prettyPrintEntity(streamedMention.entityRoot.asInstanceOf[KBEntity], Some(streamedMention))
          if (pickedProposal != null && pickedProposal.diff.size > 0) {
            //println("\nBest nontrivial proposal score: "+bestNonTrivialProposal.modelScore)
            val debugdiff = new DebugDiffList
            debugdiff ++= pickedProposal.diff
            debugdiff.scoreAndUndo(model)
            debugdiff.redo
          }
          println("  num linked: " + numLinked)
          println("  total mentions: " + count)
          println("  num candidates: " + numCandidates + "  avg: " + (numCandidates / count.toLong).toDouble)
          println("  time: " + getElapsedSec(startTime))
        }
      } else {
        /*
        println("SHOULD HAVE PICKED THIS PROPOSAL?")
        val debugdiff = new DebugDiffList
        debugdiff ++= bestNonTrivProposal.diff
        bestNonTrivProposal.diff.redo
        debugdiff.scoreAndUndo(model)
        bestNonTrivProposal.diff.done=false
        */
      }
      for (pw <- pwOpt) writeResult(pw, m)
    }
    for (pw <- pwOpt) {
      pw.flush()
      pw.close()
    }
    println("  num linked: " + numLinked)
    println("  total mentions: " + count)
    println("  num candidates: " + numCandidates + "  avg: " + (numCandidates / count.toLong).toDouble)
    println("  time: " + getElapsedSec(startTime))
    println("Finished linking " + count + " mentions against " + targets.size + " entities. Linked " + numLinked + ".")
    println("Linking time: " + getElapsedSec(startTime) + ".")
  }

  override def pickProposal(proposals: Seq[Proposal[Null]]): Proposal[Null] = {
    val result = proposals.maxBy(_.modelScore) //super.pickProposal(proposals)
    val sorted = proposals.sortBy(_.modelScore).reverse
    //println("Created "+sorted.size+" proposals. Settings count = "+this.settingsCount)
    /*
    for(p <- sorted){
      println("   proposal score: "+p.modelScore+" size: "+p.diff.size)
      if(p eq result) println("  *proposal score: "+p.modelScore+" size: "+p.diff.size)
    }
    for(p <- sorted){
      if(!p.diff.done && p.diff.size>0){
        p.diff.redo
        val debugdiff = new DebugDiffList
        debugdiff ++= p.diff
        println("SCORING/DEBUGGING: "+p.modelScore)
        debugdiff.scoreAndUndo(model)
        p.diff.done=false
      }
    }
    */
    if (proposals.filter(_.diff.size > 0).size > 0) bestNonTrivProposal = proposals.filter(_.diff.size > 0).maxBy(_.modelScore)
    pickedProposal = result
    result
  }

  var settingsCount = 0

  override def settings(c: Null): SettingIterator = new SettingIterator {
    settingsCount += 1
    //println("Proposal count: " + proposalCount+" settings count: "+settingsCount+" mention name: "+streamedMention.canonicalName.value+" id: "+streamedMention.id)
    val changes = new scala.collection.mutable.ArrayBuffer[(DiffList) => Unit]
    for (entityCandidate <- entityCandidates.filter(e => e.isEntity.booleanValue)) {
      numCandidatesConsidered += 1L
      if (!entityCandidate.isMention) {
        //println("MergingLeft")
        assert(entityCandidate.isEntity.booleanValue)
        changes += {
          (d: DiffList) => mergeLeft(entityCandidate, streamedMention)(d)
        }
      } else {
        //the entity candidate is a mention
        //println("MergingUp")
        if (streamedMention.isMention.booleanValue) changes += {
          (d: DiffList) => mergeUp(streamedMention, entityCandidate)(d)
        }
        else changes += {
          (d: DiffList) => mergeLeft(streamedMention, entityCandidate)(d)
        }
      }
    }
    changes += {
      (d: DiffList) => {}
    }
    //give the sampler an option to reject all other proposals
    var i = 0

    def hasNext = i < changes.length

    def next(d: DiffList) = {
      val d = newDiffList;
      changes(i).apply(d);
      i += 1;
      d
    }

    def reset = i = 0
  }

  override def printSamplingInfo: Unit = {
    super.printSamplingInfo
    val timeDiffMil = (System.currentTimeMillis - totalTime).toInt
    val timeWindowMS = (System.currentTimeMillis - intervalTime).toInt
    var timeDiffSec = timeDiffMil / 1000
    var candidatesConsideredPerSec: Int = -1
    var instantCandidatesConsideredPerSec: Int = -1
    if (timeDiffMil != 0) {
      candidatesConsideredPerSec = (numCandidatesConsidered.toDouble / timeDiffMil.toDouble * 1000.0).toInt
    }
    println("  (Num candidates considered: " + numCandidatesConsidered + " per sec: " + candidatesConsideredPerSec + ")")
    //Evaluator.eval(getEntities)
    System.out.flush
  }
}