package cc.factorie.xcoref

import cc.factorie._
import collection.mutable.HashMap
import cc.factorie.random

/**
 * Canonicalizers select a single representative string from a set of strings
 * @author sameer
 */
trait Canonicalizer[A] {
  def apply(seq: Seq[A], op: A => String): A

  def apply(seq: Seq[A]): A = apply(seq, (a: A) => a.toString)
}

class RandomCanonicalizer[A] extends Canonicalizer[A] {
  implicit val rand = random

  def apply(seq: Seq[A], op: A => String) = seq.sampleUniformly(cc.factorie.random)
}

class LongestStringCanonicalizer[A] extends Canonicalizer[A] {
  def apply(seq: Seq[A], op: A => String) = seq.maxBy(a => op(a).length)
}

class PopularStringCanonicalizer[A] extends Canonicalizer[A] {
  def apply(seq: Seq[A], op: A => String) = {
    val counts = new HashMap[String, Int]
    var maxA: A = null.asInstanceOf[A]
    var maxCount = -1
    for (a <- seq) {
      val s = op(a)
      val nc = counts.getOrElse(s, 0) + 1
      if (maxCount <= nc) {
        maxA = a
        maxCount = nc
      }
      counts(s) = nc
    }
    maxA
  }
}

object Canonicalizer {
  def default[A]: Canonicalizer[A] = longest[A]

  def longest[A]: Canonicalizer[A] = new LongestStringCanonicalizer[A]

  def random[A]: Canonicalizer[A] = new RandomCanonicalizer[A]

  def popular[A]: Canonicalizer[A] = new PopularStringCanonicalizer[A]
}

object FirstNamedMentionCanonicalizer extends Canonicalizer[DocMention] {
  def apply(seq: Seq[DocMention], op: (DocMention) => String) = {
    /*if(seq.exists(_.refMentionId.isDefined))val triggerTokens = Some(
    Set("secretary","technician", "correspondent", "soldier", "engineer", "pilot",
      "columnist", "rep", "congressman", "congresswoman", "representative", "ambassador", "lieutenant", "captain",
      "gen.", "rep.", "gov.", "capt.", "col.", "sen.")
  )
      seq.filter(_.refMentionId.isDefined).maxBy(s => op(s).size)
    else*/
    if (seq.filter(_.isNamed).size > 0)
      seq.filter(_.isNamed).sortBy(_.tokens.head.stringStart).head
    else Canonicalizer.popular[DocMention](seq, op)
  }
}

object FirstNamedMentionCanonicalizerWithRefMentions extends Canonicalizer[DocMention] {
  def apply(seq: Seq[DocMention], op: (DocMention) => String) = {
    if(seq.exists(_.refMentionId.isDefined))
      seq.filter(_.refMentionId.isDefined).maxBy(s => op(s).size)
    else
    if (seq.filter(_.isNamed).size > 0)
      seq.filter(_.isNamed).sortBy(_.cleanTokens().head.stringStart).head
    else Canonicalizer.popular[DocMention](seq, op)
  }
}
