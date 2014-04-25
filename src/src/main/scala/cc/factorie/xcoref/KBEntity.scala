package cc.factorie.xcoref

import cc.factorie.app.nlp.hcoref._
import cc.factorie.variable.StringVariable
import cc.factorie._
import scala.collection.mutable.ArrayBuffer

// Entity attribute variables

class BoW2Variable(val entity: Entity, val bag1: BagOfWordsVariable, val bag2: BagOfWordsVariable) extends Var with EntityAttr {
  type Value = Pair[Pair[BagOfWords, BagOfWords], String]

  def value = (bag1.value -> bag2.value) -> entity.attr[EntityTypeVar].value.toString
}

class NameTopicVariable(entity: Entity, bag1: BagOfNamesVar, bag2: BagOfTopicsVar) extends BoW2Variable(entity, bag1, bag2)

class CombinedBagVar(val entity: Entity) extends BagOfWordsVariable(Nil, null) with EntityAttr

class ContextBagVar(val entity: Entity) extends BagOfWordsVariable(Nil, null) with EntityAttr

class BagOfAttributesVar(val entity: Entity) extends BagOfWordsVariable(Nil, null) with EntityAttr

class BagOfNamesVar(val entity: Entity) extends BagOfWordsVariable(Nil, null) with EntityAttr

//statistics for the canonical representation, todo: put a factor between this and canonical representation
class EntityTypeVar(val entity: Entity, etype: String = "???") extends StringVariable(etype) with EntityAttr

class CanonicalRepresentationVar(val entity: Entity, string: String = "") extends StringVariable(string) with EntityAttr

class SourceBagVar(val entity: Entity) extends BagOfWordsVariable(Nil, null) with EntityAttr

class MentionBagVar(val entity: Entity) extends BagOfWordsVariable(Nil, null) with EntityAttr

class MentionTitleBagVar(val entity: Entity) extends BagOfWordsVariable(Nil, null) with EntityAttr

class BagOfTopicsVar(val entity: Entity) extends BagOfWordsVariable(Nil, null) with EntityAttr

class ContextVariable(entity: Entity, bag1: MentionBagVar, bag2: ContextBagVar) extends BoW2Variable(entity, bag1, bag2)

class TopicMentionVariable(entity: Entity, bag1: BagOfTopicsVar, bag2: MentionBagVar) extends BoW2Variable(entity, bag1, bag2)

class BagOfEntityTypesVar(val entity: Entity) extends BagOfWordsVariable(Nil, null) with EntityAttr

class BagOfDocsVar(val entity: Entity) extends BagOfWordsVariable(Nil, null) with EntityAttr


//canopies
class DefaultCanopy(val entity: KBEntity) extends CanopyAttribute[KBEntity] {
  lazy val canopyName = "DFLT"
}

class NameCanopy(val entity: KBEntity) extends CanopyAttribute[KBEntity] {
  def canopyName = entity.canonicalName.value.toLowerCase
}

class NonDetNameCanopy(val entity: KBEntity) extends NonDetCanopyAttribute[KBEntity] {
  protected var _names: Seq[String] = null

  def canopyNames: Seq[String] = {
    if (_names == null) {
      _names = entity.bagOfNames.value.iterator.map(_._1).toSeq //++ entity.canonicalName.value.toLowerCase.split(" ")// ++ Seq("DFLT")
    }
    _names
  }

  def reset: Unit = _names = null
}

//entities
class KBEntity(name: String = "") extends HierEntity with HasCanopyAttributes[KBEntity] with Prioritizable {
  var _id: Any = java.util.UUID.randomUUID.toString
  //new ObjectId()//java.util.UUID.randomUUID.toString
  var auxiliaryId: Option[String] = None
  var dateOption: Option[Int] = None
  var source = ""
  var mId = ""
  var docId = ""
  var wikiUrl = ""
  var moveable: Boolean = true

  override def id = _id

  def string = canonicalName.value

  priority = random.nextDouble
  //canopyAttributes += new NameCanopy(this)
  canopyAttributes += new NonDetNameCanopy(this)
  //attributes
  val combinedBag = new CombinedBagVar(this)
  val contextBag = new ContextBagVar(this)
  val bagOfNames = new BagOfNamesVar(this)
  val entityType = new EntityTypeVar(this)
  val canonicalName = new CanonicalRepresentationVar(this, name)
  val bagOfSources = new SourceBagVar(this)
  val bagOfMentions = new MentionBagVar(this)
  val bagofMentionTitles = new MentionTitleBagVar(this)
  val bagOfTopics = new BagOfTopicsVar(this)
  val nameXtopicVar = new NameTopicVariable(this, bagOfNames, bagOfTopics)
  val attributesBag = new BagOfAttributesVar(this)
  //make sure to add all attributes to attr so inference can automatically propagate them
  attr += bagOfNames
  attr += bagOfTopics
  attr += nameXtopicVar
  attr += combinedBag
  attr += contextBag
  attr += entityType
  attr += canonicalName
  attr += bagOfSources
  attr += bagOfMentions
  attr += bagofMentionTitles
  attr += attributesBag

  override def canopyNames: Seq[String] = {
    val cnames = new ArrayBuffer[String]
    for (canopyAttribute <- canopyAttributes) {
      canopyAttribute match {
        case nondet: NonDetCanopyAttribute[KBEntity] => cnames ++= nondet.canopyNames
        case det: CanopyAttribute[KBEntity] => cnames += det.canopyName
        case _ => {}
      }
    }
    cnames //.distinct
  }
}

object KBEntity {
  //factorie methods for instantiating KBEntity variables
  def newEntity(name: String = ""): KBEntity = new KBEntity(name)

  def newMention(name: String = ""): KBEntity = {
    val result = new KBEntity(name)
    result.flagAsMention
    result
  }

  def newUnmoveableEntity(name: String = ""): KBEntity = {
    val result = newEntity(name)
    result.moveable = false
    result
  }

  def newUnmoveableMention(name: String = ""): KBEntity = {
    val result = newMention(name)
    result.moveable = false
    result
  }
}

class TACKBEntity(name: String) extends KBEntity(name) {
  var queryMentionType: Option[DataSource.Value] = None

  def isRef: Boolean = isObserved && bagOfSources.value.apply("refent") > 0.0

  val bagOfEntityTypes = new BagOfEntityTypesVar(this)
  val bagOfDocs = new BagOfDocsVar(this)
  attr += bagOfEntityTypes
  attr += bagOfDocs
  attr += new ContextVariable(this, bagOfMentions, contextBag)
  attr += new TopicMentionVariable(this, bagOfTopics, bagOfMentions)
  //canopyAttributes += new NonDetTopicCanopy(this)
  //attr += bagOfTopics
}