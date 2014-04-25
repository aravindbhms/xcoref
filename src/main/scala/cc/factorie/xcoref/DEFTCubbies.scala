package cc.factorie.xcoref

import scala.collection.mutable.{LinkedHashSet, ArrayBuffer}
import scala.collection.mutable
import cc.factorie.app.nlp.{Token, Document}
import cc.factorie.app.nlp.mention.{MentionType, MentionEntityType, Mention}

object Gender extends Enumeration {
  val UNKNOWN, MALE, FEMALE = Value;
}

class CrossDocEntity(val id: String = java.util.UUID.randomUUID.toString) {
  val docEntityIds: ArrayBuffer[String] = new ArrayBuffer
  var refEntityId: Option[String] = None
  var entityType: TackbpEntityType.Value = TackbpEntityType.UKN
  val refMentionIds: LinkedHashSet[String] = new LinkedHashSet
  val refSlotValueIds: LinkedHashSet[String] = new LinkedHashSet
  val strings: LinkedHashSet[String] = new LinkedHashSet
  var freebaseID: Option[String] = None

  def hasAnnotations: Boolean = refEntityId.isDefined || refMentionIds.size > 0 || refSlotValueIds.size > 0

  def isTackType = entityType == TackbpEntityType.PER || entityType == TackbpEntityType.ORG || entityType == TackbpEntityType.GPE || entityType == TackbpEntityType.LOC

  def isELEval(ref: RefKB): Boolean = hasAnnotations && refMentionIds.exists(id => ref.mentionOpt(id).fold(false)(m => m.isEval))

  def refMentions(kb: RefKB): Seq[RefMention] = refMentionIds.toSeq.flatMap(id => kb.mentionOpt(id))

  def singleton: Boolean = docEntityIds.size <= 1

  def freebaseId_(id: Option[String]) = this.freebaseID = id

  def freebaseId: Option[String] = freebaseID
}

class DocMention(val mention: Mention,
                 var docEntity: DocEntity,
                 val id: String) {

  def doc: Document = mention.document

  // position of the mention inside the section (in tokens)
  def start: Int = mention.start

  // length of the mention in number of tokens
  def length: Int = mention.length

  def section = mention.section

  def sentence = mention.sentence

  def tokens = mention.tokens

  def headToken = mention.headToken

  def isWebsite = text startsWith "http"

  def stringStart = tokens.head.stringStart

  def stringEnd = tokens.head.stringEnd

  // the id of the refMention (if any) that this mention overlaps with (part of queries in SF and EL)
  private var _refMentionId: Option[String] = None
  // the id of the slot (if any) that this mention is the value of (part of annotations in SF and EL)
  private var _refSlotValueId: Option[String] = None

  def headTokenIdx = mention.headTokenIndex

  //todo: the next three lines are huge hacks. We should get rid of the TackbpEntityType enum within tackbp all together
  def entityType: Option[TackbpEntityType.Value] = Some(TackbpEntityType.bestFirstCorefType(this.mention.attr[MentionEntityType].categoryValue))

  def entityType_=(et: TackbpEntityType.Value): Unit = if (et != TackbpEntityType.UKN) this.mention.attr[MentionEntityType].setCategory(TackbpEntityType.convertToBestFirstString(et))(null)

  def entityType_=(ets: String): Unit = entityType_=(TackbpEntityType.withName(ets))

  def mentionType_=(dt: String): Unit = this.mention.attr[MentionType].setCategory(dt)(null)

  def mentionType: Option[String] = Some(this.mention.attr[MentionType].categoryValue)

  def refMentionId: Option[String] = _refMentionId

  def refMentionId_=(rm: String) = _refMentionId = Some(rm)

  def refSlotValueId: Option[String] = _refSlotValueId

  def refSlotValueId_=(rm: String) = _refSlotValueId = Some(rm)

  def end: Int = start + length

  def text: String = {
    val off = offsets
    doc.string.substring(off._1, off._2)
  }

  def offsets: (Int, Int) = tokens.head.stringStart -> tokens.last.stringEnd

  def isNamed: Boolean = (
    mentionType.fold(false)(_ == "NAM")
      && !headToken.posTag.categoryValue.startsWith("PRP")
      && tokens.exists(!_.posTag.categoryValue.startsWith("PRP"))
    )

  def cleanOffsets(neverSkipPreps: Boolean = false): (Int, Int) = {
    val ct = cleanTokens(neverSkipPreps)
    ct.head.stringStart -> ct.last.stringEnd
  }

  @deprecated(message = "was useful when mentions were bad.")
  def cleanTokens(neverSkipPreps: Boolean = false): Seq[Token] = {
    if (refMentionId.isDefined) return this.tokens
    val skipPreps = if (neverSkipPreps) false else this.entityType.fold(true)(_ != TackbpEntityType.PER)
    var tokens = List.empty[Token]
    var t = headToken.positionInSection
    while (t >= start && (section.tokens(t) == headToken
      || (section.tokens(t).posTag.categoryValue == "NNP")
      || (section.tokens(t).posTag.categoryValue == "NNPS")
      || (
      if (skipPreps) {
        (section.tokens(t).string.toLowerCase == "of"
          || section.tokens(t).string.toLowerCase == "on"
          || section.tokens(t).string.toLowerCase == "in"
          || section.tokens(t).string.toLowerCase == "for")
      } else false
      ))) {
      tokens = List(section.tokens(t)) ++ tokens
      t -= 1
    }
    if (tokens.size > 1 && (
      tokens.head.string.toLowerCase == "of"
        || tokens.head.string.toLowerCase == "on"
        || tokens.head.string.toLowerCase == "for"
      ))
      tokens = tokens.drop(1)
    if (headToken.hasNext) {
      t = headToken.next.positionInSection
      while (t < start + length && (section.tokens(t) == headToken
        || (section.tokens(t).posTag.categoryValue == "NNP")
        || (section.tokens(t).posTag.categoryValue == "NNPS")
        || (
        if (skipPreps) {
          (section.tokens(t).string.toLowerCase == "of"
            || section.tokens(t).string.toLowerCase == "on"
            || section.tokens(t).string.toLowerCase == "in"
            || section.tokens(t).string.toLowerCase == "for")
        } else false
        ))) {
        tokens = tokens ++ List(section.tokens(t))
        t += 1
      }
      if (tokens.size > 1 && (tokens.last.string.toLowerCase == "of"
        || tokens.last.string.toLowerCase == "on"
        || tokens.last.string.toLowerCase == "in"
        || tokens.last.string.toLowerCase == "for"
        ))
        tokens = tokens.dropRight(1)
    }
    if (tokens.size == 0) {
      val filteredTokens = this.tokens.filter(t => t == headToken
        || (t.posTag.categoryValue == "NNP"
        || t.posTag.categoryValue == "NNPS"))
      if (filteredTokens.size == 0)
        this.tokens //.map(_.string).mkString(" ") //text
      else
        filteredTokens //.map(_.string).mkString(" ")
    } else tokens //.map(_.string).mkString(" ")
  }

  @deprecated(message = "was useful when mentions were bad.")
  def cleanedText(neverSkipPreps: Boolean = false): String = cleanTokens(neverSkipPreps).map(_.string).mkString(" ")
}

object DocMention {
  def apply(doc: Document, sectionStart: Int, start: Int, length: Int, docEntity: DocEntity = null, mId: String = java.util.UUID.randomUUID.toString, headTokenIdx: Int = -1): DocMention = {
    val section = doc.sections.find(_.stringStart == sectionStart)
    assert(section.isDefined)
    val mention = new Mention(section.get, start, length, headTokenIdx)
    mention.attr += new MentionEntityType(mention, "O")
    mention.attr += new MentionType(mention, "NAM")
    DocMention(mention, docEntity, mId)
  }

  def apply(mention: Mention): DocMention = {
    DocMention(mention, null)
  }

  def apply(mention: Mention, docEntity: DocEntity): DocMention = {
    DocMention(mention, docEntity, java.util.UUID.randomUUID.toString)
  }

  def apply(mention: Mention, docEntity: DocEntity, mId: String): DocMention = {
    new DocMention(mention, docEntity, mId)
  }
}

class DocEntity(val mentions: Seq[DocMention],
                var crossDocEntity: CrossDocEntity,
                val id: String = java.util.UUID.randomUUID.toString,
                canonical: DocMention = null) {
  private var _canonicalMention: DocMention = canonical
  var gender: Gender.Value = Gender.UNKNOWN
  val refMentionIds = new ArrayBuffer[String]

  def isELEval(ref: RefKB): Boolean = hasAnnotations && refMentionIds.exists(id => ref.mentionOpt(id).fold(false)(m => m.isEval))

  def isELTrain(ref: RefKB): Boolean = hasAnnotations && refMentionIds.exists(id => ref.mentionOpt(id).fold(false)(m => m.isTrain))

  def hasAnnotations : Boolean = !refMentionIds.isEmpty

  mentions.foreach(_.docEntity = this)
  refMentionIds ++= mentions.flatMap(_.refMentionId).toSet
  if (_canonicalMention == null) _canonicalMention = FirstNamedMentionCanonicalizer(mentions, _.text)

  def canonicalMention: DocMention = _canonicalMention

  def doc = mentions.head.doc

//  def useForRelations = mentions.exists(m => {
//    m.mentionType.isDefined && m.mentionType.get == "NAM"
//  }) || refMentionIds.size > 0
//
//  def useForCoreference = useForRelations

  def useForCoreference = mentions.exists(m => {
  m.mentionType.isDefined && m.mentionType.get == "NAM"
  }) || refMentionIds.size > 0

  def entityType: TackbpEntityType.Value = {
    val et = new mutable.HashMap[TackbpEntityType.Value, Int]
    for (m <- mentions) {
      val t = m.entityType.getOrElse(TackbpEntityType.UKN)
      et(t) = et.getOrElse(t, 0) + 1
    }
    if (et.size == 0) TackbpEntityType.UKN
    else if (et.size == 1) et.head._1
    else et.filter(_._1 != TackbpEntityType.UKN).maxBy(_._2)._1
  }

  def entityType(kb: RefKB): TackbpEntityType.Value = {
    val et = new mutable.HashMap[TackbpEntityType.Value, Int]
    if (refMentionIds.size > 0) {
      refMentionIds.flatMap(rm => kb.mentionOpt(rm)).foreach(m => {
        val t = m.entityType.getOrElse(TackbpEntityType.UKN)
        et(t) = et.getOrElse(t, 0) + 1
      })
    } else {
      for (m <- mentions) {
        val t = m.entityType.getOrElse(TackbpEntityType.UKN)
        et(t) = et.getOrElse(t, 0) + 1
      }
    }
    if (et.size == 0) TackbpEntityType.UKN
    else if (et.size == 1) et.head._1
    else et.filter(_._1 != TackbpEntityType.UKN).maxBy(_._2)._1
  }
}

class DocEntities extends ArrayBuffer[DocEntity]

class DocMentions extends ArrayBuffer[DocMention]

class DEFTCubbies {

}


