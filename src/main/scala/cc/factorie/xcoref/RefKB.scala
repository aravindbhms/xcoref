package cc.factorie.xcoref

import scala.collection.mutable.{HashMap, LinkedHashMap, ArrayBuffer}
import scala.xml.Elem

object DataSource extends Enumeration {
  val training, reference, evaluation = Value
}

object TackbpEntityType extends Enumeration {

  val PER, ORG, GPE, UKN, DATE, CARDINAL, EVENT, FAC, LANGUAGE, LAW, LOC, MONEY, NORP, ORDINAL, PERCENT, PRODUCT, QUANTITY, TIME, WORK_OF_ART = Value

  def tackbpType(et: Value) = {
    if (Seq(PER, ORG, GPE).contains(et)) et else UKN
  }

  def convertToBestFirstString(v: Value): String = {
    val s = v.toString

    val toReturn = {
      if (s == "PER")
        return "PERSON"
      else if (s == "UKN")
        return "O"
      else
        return s
    }
    //assert(cc.factorie.app.nlp.coref.OntonotesEntityTypeDomain.contains(s))
    toReturn
  }

  def bestFirstCorefType(ets: String): Value = {
    if (ets == "PERSON") PER
    else if (ets == "O") UKN
    else if (ets == "MISC") UKN
    else withName(ets)
  }
}

// things that are the objects of slots: can be mentions or entities
trait SlotObject {
  def id: String
}

// things that are values of slots: can be strings (in ref KB), annotated string (in SF annotations), or a mention (not supported yet)
trait SlotValue {
  def strValue: String

  def asv = this.asInstanceOf[AnnotatedSlotValue]

  def xmlVal = this.asInstanceOf[XmlValue]
}

// can be a name of a Slot.. most of the times RelationName, but sometimes more random, such as when loading from RefKB
trait SlotName {
  def strValue: String
}

case class StringSlotName(name: String) extends SlotName {
  def strValue = name
}

class RefMention(val mentionId: String, // query id or sf_id
                 val name: String,
                 val docName: String,
                 val offsets: Option[(Int, Int)], // given in EL evaluation queries
                 val source: DataSource.Value) extends SlotObject with SlotValue {

  private var _entity: Option[RefEntity] = None // given in training queries, and evaluation annotations

  private var _TackbpEntityType: Option[TackbpEntityType.Value] = None // given in annotations

  def entity: Option[RefEntity] = _entity

  def entityType: Option[TackbpEntityType.Value] = _TackbpEntityType

  def entity_=(e: RefEntity) = {
    assert(entity.isEmpty, "You can only set entity for a singleton RefMention: " + this + ", " + entity.get)
    _entity = Some(e)
    e += this
  }

  def entityType_=(et: TackbpEntityType.Value) = _TackbpEntityType = Some(et)

  def id = mentionId

  def strValue = name

  def isEval = source == DataSource.evaluation

  def isTrain = source == DataSource.training

  override def toString = s"RefM($mentionId: $name)"
}

// to store the SF slot values when all you know is the string (the case in reference data)
class StringValue(val str: String) extends SlotValue {
  def apply: String = str

  def strValue = str
}

class XmlValue(val xml: Elem) extends SlotValue {
  def apply: String = xml.text

  def strValue = apply

  def entityLinks: Seq[(String, String)] = (xml \ "link").filter(l => l.attribute("entity_id").isDefined).map(l => (l.attribute("entity_id").get.text, l.text)).toSeq

  def nonEntityLinks: Seq[String] = (xml \ "link").filterNot(l => l.attribute("entity_id").isDefined).map(_.text).flatMap(s => if (s.trim.length > 0) Some(s.trim) else None).toSeq

  def allLinks: Seq[String] = (xml \ "link").map(_.text)
}

// to store the SF slot values when all you know is that they're a list of strings (the case in reference data)
class ListStringValue(val strs: Seq[String]) extends SlotValue {
  def apply: String = strs.mkString(", ")

  def strValue = strs.mkString(", ")
}

class Justification(val charStart: Int, val charEnd: Int, val justification: String, val score: Option[Int])

// to store the SF slot values (found in both training and evaluation SF annotations)
class AnnotatedSlotValue(val docName: String,
                         val response: String,
                         val normResponse: String,
                         val equivClassId: Int,
                         val judgement: Int,
                         val offsets: (Int, Int),
                         val justification: Option[Justification]
                          ) extends SlotValue {
  def strValue = response // or normResponse?

  private var _mention: Option[DocMention] = None

  def mention: Option[DocMention] = _mention

  def mention_=(m: DocMention) = _mention = Some(m)
}

// to store the slots (found in both training and evaluation SF annotations)
// also store the facts from the reference KB
class RefSlot(val slotId: Option[String], // comb of filename and filler id, absent in reference KB
              val name: SlotName,
              val obj: SlotObject, // absent in reference KB
              val value: SlotValue,
              val source: DataSource.Value) {
  def id: String = if (slotId.isDefined) slotId.get else obj.id + "_" + name

  //def rname = name.asInstanceOf[RelationName]
}

class RefEntityData(val name: String,
                    val wikiTitle: String,
                    val wikiText: String,
                    val factsClass: String,
                    val slots: Seq[RefSlot]) {
  private var _wikipediaId: Option[Int] = None
  private var _wikipediaPageId: Option[Int] = None
  private var _canonicalWikiTitle: Option[String] = None
  private var _normalizedCanonicalWikiTitle: Option[String] = None

  def wikipediaPageId_=(wpid: Int) = this._wikipediaPageId = Some(wpid)

  def wikipediaPageId: Option[Int] = _wikipediaPageId

  def wikipediaId_=(wpid: Option[Int]) = this._wikipediaId = wpid

  def wikipediaId: Option[Int] = _wikipediaId

  def canonicalTitle_=(wikiTitle: Option[String]) = this._canonicalWikiTitle = wikiTitle

  def canonicalTitle: Option[String] = _canonicalWikiTitle

  def normalizedCanonicalWikiTitle_=(wikiTitle: Option[String]) = this._normalizedCanonicalWikiTitle = wikiTitle

  def normalizedCanonicalWikiTitle: Option[String] = _normalizedCanonicalWikiTitle
}

class RefEntity(val entityId: String, // nodeId or id
                var entityType: TackbpEntityType.Value) extends SlotObject {
  private var data: Option[RefEntityData] = None //available only in reference data

  private val _mentions: ArrayBuffer[RefMention] = new ArrayBuffer

  def refData_=(data: RefEntityData) = this.data = Some(data)

  def refData: Option[RefEntityData] = data

  def mentions = _mentions.iterator

  def +=(m: RefMention) = _mentions += m

  def isNil = entityId.startsWith("NIL")

  def id = entityId

  override def toString = {
    "RefEntity(" + entityId + ")" + (if (data.isDefined) ": " + data.get.name else "")
  }
}

class RefKB(val name: String = "") {
  private val _entities = new LinkedHashMap[String, RefEntity]() // eId -> Entity

  private val _mentions = new LinkedHashMap[String, RefMention]() // mId -> Mention

  private val _mentionsInDoc = new LinkedHashMap[String, ArrayBuffer[RefMention]]() // doc -> mentions

  private val _slotsInDoc = new LinkedHashMap[String, ArrayBuffer[RefSlot]]() // doc -> slots

  private val _slots = new HashMap[String, RefSlot]() // slotId -> slot

  private val _objectSlots = new HashMap[String, ArrayBuffer[RefSlot]]

  def entities = _entities.values

  def mentions = _mentions.values

  def elQueries = _mentions.values

  def elTrainingQueries = elQueries.filter(_.source == DataSource.training)

  def elEvaluationQueries = elQueries.filter(_.source == DataSource.evaluation)

  def slots = _slots.values

  def hasAnnotations(docName: String) = _mentionsInDoc.contains(docName) || _slotsInDoc.contains(docName)

  def +=(m: RefMention): Unit = {
    assert(!_mentions.contains(m.id), s"mention ${m.id} already present in the RefKB")
    mention(m.id, m)
  }

  def -=(m: RefMention): Unit = {
    println("WARNING: removing ref mention " + m.id)
    _mentions.remove(m.id)
    _mentionsInDoc(m.docName).remove(_mentionsInDoc(m.docName).indexOf(m))
  }

  def +=(s: RefSlot): Unit = {
    if (_slots.contains(s.id)) {
      assert(s.value.strValue == _slots(s.id).value.strValue, s"slot ${s.id} already present in the RefKB, $s ignored, keeping ${_slots(s.id)}: {" + s.value.strValue + "} and {" + _slots(s.id).value.strValue + "} should be same")
    } else slot(s.id, s)
  }

  def +=(e: RefEntity): Unit = {
    assert(!_entities.contains(e.id), s"entity ${e.id} already present in the RefKB")
    entity(e.id, e)
  }

  def entity(entityId: String): RefEntity = {
    assert(_entities.contains(entityId), "Could not find entity with id " + entityId)
    _entities(entityId)
  }

  def entityOpt(entityId: String): Option[RefEntity] = _entities.get(entityId)

  def entity(nodeId: String, newEntity: => RefEntity): RefEntity = {
    assert(newEntity != null)
    if (_entities.contains(nodeId)) {
      val e = _entities(nodeId)
      if (e.entityType == TackbpEntityType.UKN) {
        // type of old E is unknown
        val ne = newEntity
        if (ne.entityType != TackbpEntityType.UKN) {
          // but it is known of the new entity!
          assert(ne.mentions.size == 0)
          e.entityType = ne.entityType
          e
        } else {
          // both unknown, don't update
          e
        }
      } else {
        val ne = newEntity
        if (e.entityType != ne.entityType && ne.entityType != TackbpEntityType.UKN) {
          val pickE = if (ne.refData.isDefined) {
            assert(e.refData.isEmpty)
            ne
          } else if (e.refData.isDefined) {
            e
          } else {
            // pick the later one..
            ne
          }
          println(s"Annotations disagree for ${e.id}: ${e.entityType} and ${ne.entityType}, taking ${pickE.entityType}.")
          e.entityType = pickE.entityType
        }
        e
      }
    } else {
      val e = newEntity
      assert(nodeId == e.id)
      _entities(nodeId) = e
      e
    }
  }

  def mentionOpt(mentionId: String) = _mentions.get(mentionId)

  def mention(mentionId: String): RefMention = {
    assert(_mentions.contains(mentionId), "Could not find mention with id " + mentionId)
    _mentions(mentionId)
  }

  def mention(mentionId: String, newMention: => RefMention) = {
    if (_mentions.contains(mentionId)) {
      _mentions(mentionId)
    } else {
      val mention: RefMention = newMention
      assert(mentionId == mention.id, "Id of the mention should match mentionId: " + mentionId + " != " + mention.id)
      _mentions(mentionId) = mention
      _mentionsInDoc.getOrElseUpdate(mention.docName, new ArrayBuffer) += mention
      mention
    }
  }

  def mentionsForDoc(docName: String): Seq[RefMention] = _mentionsInDoc.getOrElse(docName, Seq.empty)

  def slotsForDoc(docName: String): Seq[RefSlot] = _slotsInDoc.getOrElse(docName, Seq.empty)

  def docNames = _mentionsInDoc.keySet

  def slotOpt(slotId: String): Option[RefSlot] = _slots.get(slotId)

  def slot(slotId: String): RefSlot = {
    assert(_slots.contains(slotId), "Could not find slot with id " + slotId)
    _slots(slotId)
  }

  def slot(slotId: String, newSlot: => RefSlot) = {
    if (_slots.contains(slotId)) {
      _slots(slotId)
    } else {
      val slot: RefSlot = newSlot
      assert(slotId == slot.id, "Id of the slot should match slotId: " + slotId + " != " + slot.id)
      _slots(slotId) = slot
      _objectSlots.getOrElseUpdate(slot.obj.id, new ArrayBuffer) += slot
      slot.value match {
        case asv: AnnotatedSlotValue => _slotsInDoc.getOrElseUpdate(asv.docName, new ArrayBuffer) += slot
        case _ =>
      }
      slot
    }
  }

  def objectSlots(objectId: String): Seq[RefSlot] = _objectSlots.getOrElse(objectId, Seq.empty[RefSlot])
}

object RefKB {
  val defaultDirName = "/home/aravind/UMASS/IndependentStudy/tackbp/reference/2009/"

  def loadFromDefault: RefKB = Loader.loadRefKB("reference", defaultDirName)

  def main(args: Array[String]) {
    val ref = loadFromDefault
    Loader.readWikipediaData(ref)
    val data = ref.entity("E0000001").refData.get
    println(data.wikipediaId)
    println(data.wikipediaPageId)
    println(data.canonicalTitle)
    println(data.normalizedCanonicalWikiTitle)
    println(data.slots)
  }
}
