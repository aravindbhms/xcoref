package cc.factorie.xcoref

import cc.factorie.model._
import cc.factorie._
import cc.factorie.app.nlp.hcoref._
import cc.factorie.variable.{FeatureVectorVariable, CategoricalVectorDomain}
import scala.collection.mutable.ArrayBuffer
import cc.factorie.Parameters
import cc.factorie.CombinedModel
import cc.factorie.TemplateModel
import cc.factorie.Factor

class EntityLinkingModel extends TemplateModel with Parameters

abstract class ParameterizedChildParentTemplateWithStatistics[A <: EntityAttr](implicit m: Manifest[A]) extends ChildParentTemplate[A] with DotFamily3[EntityRef, A, A]

object AffinityVectorDomain extends CategoricalVectorDomain[String] {
  dimensionDomain.maxSize = 100000
}

class ChildParentAffinityVector(prefix: String, child: BagOfWords, parent: BagOfWords, checks: Seq[Double]) extends FeatureVectorVariable[String] {
  def domain = AffinityVectorDomain

  val score = compatibilityL2Deduct(child, parent)
  //compatibilityL2(child,parent)
  val csize = child.size
  val csizeFeature = if (csize <= 2) csize.toString else "gt2"
  if (score <= 0.00001) {
    this +=(prefix + "isZero", 1.0)
    //this +=(prefix+csizeFeature+"isZero",1.0)
  }
  this +=(prefix + "raw-score", score)
  //
  //number of matches above the average weight of a word.
  val averageWeight = parent.l1Norm / parent.size.toDouble
  var numMatchesAboveAverage = 0
  for ((k, v) <- child.iterator) {
    if (parent(k) - v > averageWeight) numMatchesAboveAverage += 1
  }
  for ((k, v) <- child.iterator.filter(weightedWord => parent(weightedWord._1) - weightedWord._2 > averageWeight)) {
    //if(prefix=="t-" && k.length<=3 && k.matches("[0-9]+"))this += (prefix+k,1.0)
    numMatchesAboveAverage += 1
  }
  if (numMatchesAboveAverage == 0) this +=(prefix + "#(num>avg)=0", 1.0)
  if (numMatchesAboveAverage > 0) this +=(prefix + "#(num>avg)>0", (numMatchesAboveAverage / child.size.toDouble))
  //
  //top 2 words overlap in child
  /*    if(child.size>=2){
        val top2 = child.iterator.toSeq.sortBy(_._2).reverse.take(2)
        val numMatches=0
        for((k,v) <- top2){
          if(parent(k)>0.0)numMatches += 1
        }
        if(numMatches>=2)this += (prefix+"#top-matches=2",1.0)
      }*/
  var k = 0;
  while (k < checks.length) {
    if (score <= checks(k)) {
      this +=(prefix + "s<=" + checks(k), 1.0 - score)
      //this += (csize+":s<="+checks(k),1.0-score)
    } else {
      this +=(prefix + "s >" + checks(k), score)
      //this += (csize+":s >"+checks(k),score)
    }
    k += 1
  }
  val cats = this.activeCategories.toSeq
  var i = 0
  var j = 0
  while (i < cats.length) {
    if (this.value(i) <= 0.0001) this.value(i) = 0.0
    i += 1
  }

  def compatibilityL2Deduct(childBag: BagOfWords, parentBag: BagOfWords): Double = childBag.cosineSimilarity(parentBag, childBag)

  def compatibilityL2(childBag: BagOfWords, parentBag: BagOfWords): Double = {
    val childDOTparent = childBag * parentBag
    val numerator = childDOTparent - childBag.l2Norm //mathematically the same as deducted dot
    if (numerator != 0.0) {
      //(a-b)^2 = a^2+b^2 - 2ab
      val parentL2Norm = scala.math.sqrt(childBag.l2Norm * childBag.l2Norm + parentBag.l2Norm * parentBag.l2Norm - 2 * (childDOTparent))
      val denominator = childBag.l2Norm * parentL2Norm
      if (denominator == 0.0 || denominator != denominator) 0.0 else numerator / denominator
    } else 0.0
  }

  def compatibilityL2ChildOnly(childBag: BagOfWords, parentBag: BagOfWords): Double = {
    val numerator = childBag * parentBag - childBag.l2Norm //mathematically the same as deducted dot
    if (numerator != 0.0) {
      val denominator = scala.math.sqrt(childBag.l2Norm * (parentBag.l1Norm - childBag.l1Norm))
      if (denominator == 0.0 || denominator != denominator) 0.0 else numerator / denominator
    } else 0.0
  }

  def compatibilityL1(childBag: BagOfWords, parentBag: BagOfWords): Double = {
    //val smaller = if(this.size<that.size)this else that
    //val larger = if(that.size<this.size)this else that
    val numerator: Double = childBag.deductedDot(parentBag, childBag)
    if (numerator != 0.0) {
      //val parentNorm = Math.sqrt(childBag.l2Norm*childBag.l2Norm+parentBag.l2Norm*parentBag.l2Norm - 2*(childBag * parentBag))
      val parentNorm = parentBag.l1Norm - childBag.l1Norm
      val denominator: Double = childBag.l1Norm * parentNorm
      if (denominator == 0.0 || denominator != denominator) 0.0 else numerator / denominator
    } else 0.0
  }
}

object ChildParentAffinityVector {
  //val checks = Array[Double](0.0,0.1,0.5,1.0)
  val defaultChecks = Array[Double](0.0, 0.001, 0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
}

class TopicCPAVector(prefix: String, child: BagOfWords, parent: BagOfWords, checks: Seq[Double]) extends FeatureVectorVariable[String] {
  def domain = AffinityVectorDomain

  val richness = parent.l1Norm
  //this is a reasonable estimate of the number of leaves because the bags sum close to one
  val topicFeatures = new ChildParentAffinityVector(prefix, child, parent, checks)
  val score = topicFeatures.score
  var i = 0
  this.value += topicFeatures.value
  if (score < 0.0001) {
    if (richness > 2) this +=(prefix + "isZero/r2", 1.0)
    if (richness >= 4) this +=(prefix + "isZero/r4", 1.0)
    if (richness >= 8) this +=(prefix + "isZero/r8", 1.0)
    if (richness >= 16) this +=(prefix + "isZero/r16", 1.0)
  }
}

class ChildParent2AffinityVector(childBagPairAndEntityType: Pair[Pair[BagOfWords, BagOfWords], String], parentBagPairAndEntityType: Pair[Pair[BagOfWords, BagOfWords], String], featureConjunctions: Boolean, bins: Seq[Double]) extends FeatureVectorVariable[String] {
  def domain = AffinityVectorDomain

  val concatstr = "_X_"
  val childEntityType = childBagPairAndEntityType._2
  val parentEntityType = parentBagPairAndEntityType._2
  val childBagPair = childBagPairAndEntityType._1
  val parentBagPair = parentBagPairAndEntityType._1
  val vec1 = new ChildParentAffinityVector("n-", childBagPair._1, parentBagPair._1, bins)
  val vec2 = new TopicCPAVector("t-", childBagPair._2, parentBagPair._2, bins)
  //new ChildParentAffinityVector("t-",childBagPair._2,parentBagPair._2,bins)
  val activeCats1 = vec1.activeCategories.toIndexedSeq
  val activeCats2 = vec2.activeCategories.toIndexedSeq
  if (featureConjunctions) {
    for ((index1, value1) <- vec1.value.activeElements) {
      val name1 = domain.dimensionDomain.category(index1)
      for ((index2, value2) <- vec2.value.activeElements) {
        val value = scala.math.sqrt(math.abs(value1 * value2))
        val name2 = domain.dimensionDomain.category(index2)
        this +=(name1 + concatstr + name2, value)
      }
    }
  }
  this.value += vec1.value
  this.value += vec2.value

  if (childEntityType == parentEntityType) {
    entityTypeConjunctions(vec1)
    entityTypeConjunctions(vec2)
  } else this +=("CP-EType-Mismatch", 1.0)

  def entityTypeConjunctions(vec: FeatureVectorVariable[String]) {
    val entityType = childEntityType
    for ((index, value) <- vec.value.activeElements) {
      val name = domain.dimensionDomain.category(index)
      this +=(name + entityType, value)
    }
  }
}

class ParameterizedBoW2Compatibility[B <: BoW2Variable with EntityAttr](model: Parameters, featureConjunctions: Boolean = false, bins: Seq[Double] = ChildParentAffinityVector.defaultChecks)(implicit m: Manifest[B]) extends ParameterizedChildParentTemplateWithStatistics[B] with DebugableTemplate {
  val name = "PChildBoW2(" + m.runtimeClass + ")"
  println(name)

  override def unroll2(childBow: B) = Nil

  //note: this is a slight approximation for efficiency
  override def unroll3(childBow: B) = Nil

  //note this is a slight approximation for efficiency
  //val weights = model.Weights(new la.DenseTensor1(AffinityVectorDomain.dimensionSize))
  lazy val weights = model.Weights(new la.GrowableDenseTensor1(AffinityVectorDomain.dimensionDomain.maxSize))

  def statistics(er: EntityRef#Value, childBowAndEType: B#Value, parentBowAndEType: B#Value) = new ChildParent2AffinityVector(childBowAndEType.asInstanceOf[Pair[Pair[BagOfWords, BagOfWords], String]], parentBowAndEType.asInstanceOf[Pair[Pair[BagOfWords, BagOfWords], String]], featureConjunctions, bins).value

  override def score(er: EntityRef#Value, childBow: B#Value, parentBow: B#Value): Double = {
    val result = super.score(er, childBow, parentBow)
    if (_debug) println("  " + debug(result))
    //println("   dotscore: "+result)
    result
  }
}

class ParameterizedChildParentCompatibility[B <: BagOfWordsVariable with EntityAttr](prefix: String, model: Parameters, bins: Seq[Double] = ChildParentAffinityVector.defaultChecks)(implicit m: Manifest[B]) extends ParameterizedChildParentTemplateWithStatistics[B] with DebugableTemplate {
  val name = "PChildParentCompatibility(" + m.runtimeClass + ")"
  println(name)

  override def unroll2(childBow: B) = Nil

  //note: this is a slight approximation for efficiency
  override def unroll3(childBow: B) = Nil

  //note this is a slight approximation for efficiency
  //val weights = model.Weights(new la.DenseTensor1(AffinityVectorDomain.dimensionSize))
  lazy val weights = model.Weights(new la.GrowableDenseTensor1(AffinityVectorDomain.dimensionDomain.maxSize))

  def statistics(er: EntityRef#Value, childBow: B#Value, parentBow: B#Value) = new ChildParentAffinityVector(prefix, childBow, parentBow, bins).value

  override def score(er: EntityRef#Value, childBow: B#Value, parentBow: B#Value): Double = {
    val result = super.score(er, childBow, parentBow)
    if (_debug) println("  " + debug(result))
    //println("   dotscore: "+result)
    result
  }
}

class BagCompatibilityTemplate[B <: BagOfWordsVariable with EntityAttr](compatibilityVec: (B#Value) => FeatureVectorVariable[String], model: Parameters)(implicit m: Manifest[B]) extends Template3[EntityExists, IsEntity, B] with DotFamily3[EntityExists, IsEntity, B] with DebugableTemplate {
  //class BagCompatibilityTemplate[B<:BagOfWordsVariable with EntityAttr](val prefix:String,topk:Int,sizePenalty:Boolean,model:Parameters)(implicit m:Manifest[B]) extends Template3[EntityExists,IsEntity,B] with DotFamily3[EntityExists,IsEntity,B] with DebugableTemplate{
  val name = "BagCompatibilityTemplate(" + m.runtimeClass + ")"
  println(name)
  lazy val weights = model.Weights(new la.GrowableDenseTensor1(AffinityVectorDomain.dimensionDomain.maxSize))

  def statistics(exists: EntityExists#Value, isEntity: IsEntity#Value, bow: B#Value) = compatibilityVec(bow).value

  //new EntityCompatibilityVector(prefix,topk,sizePenalty,bow).value
  def unroll1(exists: EntityExists) = if (exists.booleanValue) Factor(exists, exists.entity.attr[IsEntity], exists.entity.attr[B]) else Nil

  def unroll2(isEntity: IsEntity) = Factor(isEntity.entity.attr[EntityExists], isEntity, isEntity.entity.attr[B])

  def unroll3(bag: B) = Factor(bag.entity.attr[EntityExists], bag.entity.attr[IsEntity], bag) //throw new Exception("An entitie's status as a mention should never change.")
}

class EntityCompatibilityVector(prefix: String, topk: Int, sizePenalty: Boolean, bag: BagOfWords) extends FeatureVectorVariable[String] {
  def domain = AffinityVectorDomain

  //val l0 = bag.size.toDouble
  val l1 = bag.l1Norm
  //val l2 = bag.l2Norm
  //val one2zero = l1/l0
  //val two2one = l2/l1
  if (sizePenalty) {
    if (bag.size > 1) this +=(prefix + "bag.size>1", bag.size / l1) else if (bag.size == 1) this +=("bag.size=1", 1.0)
    //var r = 0;for((k,v) <- bag.iterator)r += (v/bag.l1Norm-1.0/bag.size.toDouble);r/=2.0
  }
  val bagSeq = if (topk == 0 || topk == Integer.MAX_VALUE) bag.iterator.toArray else bag.iterator.toArray.sortBy(_._2 * -1.0).take(topk)
  var pairwiseSizePenalty = 0.0
  var i = 0;
  var j = 0;
  while (i < bagSeq.size) {
    j = i + 1
    val (k1, v1) = bagSeq(i)
    while (j < bagSeq.size) {
      val (k2, v2) = bagSeq(j)
      val v = scala.math.sqrt(scala.math.abs(v1 * v2))
      if (k2.compareTo(k1) >= 0) this +=(prefix + k1 + "-->" + k2, v) else this +=(prefix + k2 + "-->" + k1, v)
      j += 1
      pairwiseSizePenalty += v1 * v2
    }
    i += 1
  }
  if (sizePenalty) this +=(prefix + "log-pw-size", scala.math.log(pairwiseSizePenalty + 1))
  //this += (prefix+"l2/ll=x",two2one)
}

class AttributeCompatibilityVector(bag: BagOfWords) extends FeatureVectorVariable[String] {
  val prefix = "atr"

  def domain = AffinityVectorDomain

  val lastNames = bag.iterator.filter((wordNweight: (String, Double)) => AttributeExtraction.isLastNameAttr(wordNweight._1)).toIndexedSeq
  val titles = bag.iterator.filter((wordNweight: (String, Double)) => AttributeExtraction.isTitleAttr(wordNweight._1)).toIndexedSeq
  val titleSet = titles.map(_._1).toSet
  val firstNames = bag.iterator.filter((wordNweight: (String, Double)) => AttributeExtraction.isFirstNameAttr(wordNweight._1) && !titleSet.contains(wordNweight._1)).toIndexedSeq
  val lastNameMisMatch = pairwisePenalty(lastNames, logarithmic = true)
  //
  //penalty features
  val firstNameMisMatch = pairwisePenalty(firstNames, logarithmic = true)
  if (lastNames.size > 0) addFeature(lastNameMisMatch, "attr-ln-pwp")
  if (firstNames.size > 0) addFeature(firstNameMisMatch, "attr-fn-pwp")
  //
  //reward features
  val firstNameScore = pairwiseReward(firstNames, logarithmic = true)
  val lastNameScore = pairwiseReward(lastNames, logarithmic = true)
  val titleScore = pairwiseReward(titles, logarithmic = true)
  addFeatureConjunction(firstNameScore, lastNameScore, "fnXln-rwd")
  addFeatureConjunction(titleScore, lastNameScore, "fnXln-rwd")

  def addFeature(weight: Double, name: String) = if (weight > 0.0001) this +=(prefix + name, weight)

  def addFeatureConjunction(weight1: Double, weight2: Double, name: String) = {
    val weight = math.sqrt(weight1 * weight2);
    if (weight > 0.0001) this +=(prefix + name, weight)
  }

  def pairwisePenalty(vs: IndexedSeq[(String, Double)], logarithmic: Boolean): Double = {
    var i = 0;
    var j = 0;
    var result = 0.0
    while (i < vs.length) {
      j = i + 1
      while (j < vs.length) {
        result += vs(i)._2 * vs(j)._2
        j += 1
      }
      i += 1
    }
    if (logarithmic) scala.math.log(result + 1.0) else result
  }

  def pairwiseReward(vs: IndexedSeq[(String, Double)], logarithmic: Boolean): Double = {
    var i = 0;
    var result = 0.0;
    while (i < vs.length) {
      result += vs(i)._2 * vs(i)._2;
      i += 1
    };
    if (logarithmic) scala.math.log(result + 1.0) else result
  }
}

class WikipediaContraintFactor extends TupleTemplateWithStatistics3[EntityExists, IsEntity, SourceBagVar] with DebugableTemplate {
  val src = "wp"
  val name = "WikipediaContraintFactor"

  def unroll1(exists: EntityExists) = Factor(exists, exists.entity.attr[IsEntity], exists.entity.attr[SourceBagVar])

  def unroll2(isEntity: IsEntity) = Factor(isEntity.entity.attr[EntityExists], isEntity, isEntity.entity.attr[SourceBagVar])

  def unroll3(bag: SourceBagVar) = Factor(bag.entity.attr[EntityExists], bag.entity.attr[IsEntity], bag)

  //throw new Exception("An entitie's status as a mention should never change.")
  def score(exists: EntityExists#Value, isEntity: IsEntity#Value, bag: SourceBagVar#Value): Double = {
    if (exists.booleanValue && bag(src) >= 2 /*&& isEntity.booleanValue*/ ) {
      -99999.0
      //Double.MinValue
      //Double.NegativeInfinity
    }
    else 0.0
  }
}

class ReferenceConstraintFactor extends TupleTemplateWithStatistics3[EntityExists, IsEntity, SourceBagVar] with DebugableTemplate {
  val src = WikiCrossDocCoreferencer.ReferenceSource
  val name = "ReferenceConstraintFactor"

  def unroll1(exists: EntityExists) = Factor(exists, exists.entity.attr[IsEntity], exists.entity.attr[SourceBagVar])

  def unroll2(isEntity: IsEntity) = Factor(isEntity.entity.attr[EntityExists], isEntity, isEntity.entity.attr[SourceBagVar])

  def unroll3(bag: SourceBagVar) = Factor(bag.entity.attr[EntityExists], bag.entity.attr[IsEntity], bag)

  def score(exists: EntityExists#Value, isEntity: IsEntity#Value, bag: SourceBagVar#Value): Double = {
    if (exists.booleanValue && bag(src) >= 2 /*&& isEntity.booleanValue*/ ) {
      -99999.0
      //Double.MinValue
      //Double.NegativeInfinity
    }
    else 0.0
  }
}

class SourceDocContraintFactor(val weight: Double = 9999.0) extends TupleTemplateWithStatistics3[EntityExists, IsEntity, BagOfDocsVar] with DebugableTemplate {
  //val src = "wp"
  val name = "SourceDocContraintFactor"

  def unroll1(exists: EntityExists) = Factor(exists, exists.entity.attr[IsEntity], exists.entity.attr[BagOfDocsVar])

  def unroll2(isEntity: IsEntity) = Factor(isEntity.entity.attr[EntityExists], isEntity, isEntity.entity.attr[BagOfDocsVar])

  def unroll3(bag: BagOfDocsVar) = Factor(bag.entity.attr[EntityExists], bag.entity.attr[IsEntity], bag)

  //throw new Exception("An entitie's status as a mention should never change.")
  def score(exists: EntityExists#Value, isEntity: IsEntity#Value, bag: BagOfDocsVar#Value): Double = {
    //TODO: make this faster by incremental keeping track...
    var result = 0.0
    for ((k, v) <- bag.iterator) result += (v - 1.0)
    result
    //for ((k, v) <- bag.iterator) if (v > 1.0) return -weight
    //return 0.0
  }
}


class PStructuralPriorsTemplate(model: Parameters) extends Template3[EntityExists, IsEntity, IsMention] with DotFamily3[EntityExists, IsEntity, IsMention] with DebugableTemplate {
  val name = "PStructuralPriorsTemplate()"
  val prefix = "pstr-"
  lazy val weights = model.Weights(new la.GrowableDenseTensor1(AffinityVectorDomain.dimensionDomain.maxSize))
  println(name)

  def unroll1(exists: EntityExists) = if (exists.booleanValue) Factor(exists, exists.entity.attr[IsEntity], exists.entity.attr[IsMention]) else Nil

  def unroll2(isEntity: IsEntity) = Factor(isEntity.entity.attr[EntityExists], isEntity, isEntity.entity.attr[IsMention])

  def unroll3(isMention: IsMention) = throw new Exception("An entitie's status as a mention should never change.")

  def statistics(exists: EntityExists#Value, isEntity: IsEntity#Value, isMention: IsMention#Value) = new FeatureVectorVariable[String] {
    def domain = AffinityVectorDomain

    if (exists.booleanValue && isEntity.booleanValue) this += (prefix + "EntExist")
    if (exists.booleanValue && !isEntity.booleanValue && !isMention.booleanValue) this += (prefix + "SubExist")
  }.value
}

class CustomChildParentCompatibility[B <: BagOfWordsVariable with EntityAttr](val weight: Double = 4.0, val shift: Double = -0.25)(implicit m: Manifest[B]) extends ChildParentTemplateWithStatistics[B] with DebugableTemplate {
  val name = "ChildParentCompatibility(weight=" + weight + " shift=" + shift + ")"
  println("ChildParentCompatibility: weight=" + weight + " shift=" + shift)

  override def unroll2(childBow: B) = Nil

  override def unroll3(childBow: B) = Nil

  def score(er: EntityRef#Value, childBow: B#Value, parentBow: B#Value): Double = {
    val cossim = childBow.cosineSimilarity(parentBow, childBow)
    var result = (cossim + shift) * weight
    if (childBow.size == 0 || childBow.l2Norm == parentBow.l2Norm) result = 0.0 //note, this only makes sense for things like entity types...
    if (_debug) println("  " + debug(result) + "  w=" + weight + " s=" + shift + " cos=" + cossim)
    result
  }
}

class TrainingModel extends CombinedModel {
  this += new TupleTemplateWithStatistics2[BagOfTruths, IsEntity] {
    val precisionWeight = 0.75

    def unroll1(bot: BagOfTruths) = if (bot.entity.isRoot) Factor(bot, bot.entity.attr[IsEntity]) else Nil

    def unroll2(isEntity: IsEntity) = if (isEntity.entity.isRoot) Factor(isEntity.entity.attr[BagOfTruths], isEntity) else Nil

    override def score(bag: BagOfTruths#Value, isEntity: IsEntity#Value): Double = {
      var result = 0.0
      //val bag = s._1
      val bagSeq = bag.iterator.toSeq
      var i = 0;
      var j = 0;
      var tp = 0.0
      var fp = 0.0
      while (i < bagSeq.size) {
        val (labeli, weighti) = bagSeq(i)
        j = i
        while (j < bagSeq.size) {
          val (labelj, weightj) = bagSeq(j)
          if (labeli == labelj)
            tp += (weighti * (weighti - 1)) / 2.0
          else
            fp += weighti * weightj
          j += 1
        }
        i += 1
      }
      //val normalizer = tp+fp
      result = tp * (1.0 - precisionWeight) - fp * precisionWeight
      result
    }
  }
}

object EntityLinkingModel {
  def printWeights(model: EntityLinkingModel with Parameters, domainForAllTemplates: CategoricalVectorDomain[String]) {
    val positiveWeights = new ArrayBuffer[(String, Double)]
    val negativeWeights = new ArrayBuffer[(String, Double)]
    for (unknownTemplateType <- model.templates) {
      unknownTemplateType match {
        case t: DotFamily => {
          val weights = t.weights.value
          for ((i, v) <- weights.activeElements) {
            if (v > 0.0) positiveWeights += domainForAllTemplates.dimensionDomain.category(i) -> v
            else if (v < 0.0) negativeWeights += domainForAllTemplates.dimensionDomain.category(i) -> v
          }
        }
        case _ => {}
      }
    }
    val sortedPositiveWeights = positiveWeights.sortBy(_._2)
    val sortedNegativeWeights = negativeWeights.sortBy(_._2)
    println("----PRINTING WEIGHTS-----")
    println("Printing positive weights (" + sortedPositiveWeights.size + ")")
    for ((featureName, weight) <- sortedPositiveWeights) println(weight + "\t" + featureName)
    println("Printing negative weights (" + sortedNegativeWeights.size + ")")
    for ((featureName, weight) <- sortedNegativeWeights) println(weight + "\t" + featureName)
  }
}
