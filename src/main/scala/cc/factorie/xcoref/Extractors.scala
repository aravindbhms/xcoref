package cc.factorie.xcoref

import cc.factorie.app.nlp.hcoref.BagOfWordsVariable
import scala.collection.mutable.ArrayBuffer

object AttributeExtraction {
  //attribute prefixes
  val genderPrefix = "sex:"
  val firstNamePrefix = "fn:"
  val middleNamePrefix = "mn:"
  val lastNamePrefix = "ln:"
  val titlePrefix = "tl:"
  val orgTypePrefix = "ot:"
  val orgSubTypePrefix = "ost:"
  val suffixPrefix = "sfx:"

  def isFirstNameAttr(s: String) = s.startsWith(firstNamePrefix)

  def isMiddleNameAttr(s: String) = s.startsWith(middleNamePrefix)

  def isLastNameAttr(s: String) = s.startsWith(lastNamePrefix)

  def isTitleAttr(s: String) = s.startsWith(titlePrefix)

  def isSuffixAttr(s: String) = s.startsWith(suffixPrefix)

  def isGenderAttr(s: String) = s.startsWith(genderPrefix)

  def attrValue(name: String, value: String, bag: BagOfWordsVariable): Double = bag.value(name + value)

  def attributeName(nameWithPrefix: String, prefix: String) = nameWithPrefix.substring(prefix.length, nameWithPrefix.length)

  def firstNameValue(s: String, bag: BagOfWordsVariable) = attrValue(firstNamePrefix, s, bag)

  def middleNameValue(s: String, bag: BagOfWordsVariable) = attrValue(middleNamePrefix, s, bag)

  def lastNameValue(s: String, bag: BagOfWordsVariable) = attrValue(lastNamePrefix, s, bag)

  def titleValue(s: String, bag: BagOfWordsVariable) = attrValue(titlePrefix, s, bag)

  //def isOrgType(s:String,bag:BagOfWordsVariable) = attrValue(orgTypePrefix,s,bag)
  //def isOrgSubType(s:String,bag:BagOfWordsVariable) = attrValue(orgSubTypePrefix,s,bag)
  def titleValueName(s: String) = attributeName(s, titlePrefix)

  def genderValue(s: String, bag: BagOfWordsVariable) = attrValue(genderPrefix, s, bag)

  //names
  val fullNameRegexStr = "[A-Z][a-z]+"
  val initialsRegexStr = "[A-Z]\\.?"
  //titles
  val honorifics = Set("mr.", "mr", "mrs", "mrs.", "ms.", "miss", "dr.", "mister", "misses", "doctor", "sir", "m.d.", "d.m.d.")
  val titlesNews = Set("correspondent", "columnist", "journalist", "photographer", "blogger", "writer", "contributing writer")
  val titlesJobs = Set("technician", "engineer", "pilot", "businessman", "businesswoman", "entrepreneur", "professor", "software developer", "manager", "ceo", "cfo", "attorney", "dr.", "doctor", "judge", "father")
  val titlesMilitary = Set("soldier", "lieutenant", "sergeant", "officer", "captain", "commander", "general", "private", "corporal", "colonel", "col.", "col", "specialist", "pvt.", "pvt", "spc", "spc.", "gen.", "gen")
  val titlesPolitics = Set("prez", "secretary", "congressman", "congresswoman", "representative", "ambassador", "justice", "president", "senator", "state senator", "u.s. senator", "sen.", "sen", "minister", "prime minister", "governor", "gov", "gov.", "prime", "vice president", "sen.", "pres", "pres.", "rep.", "rep", "first lady", "first gentleman", "secretary of state", "secretary general", "secretary-general", "ayatollah")
  val otherTitles = Set("co-founder", "founder", "magnate", "visionary", "leader")
  val titles = titlesNews ++ titlesJobs ++ titlesMilitary ++ titlesPolitics ++ honorifics ++ otherTitles
  //orgs
  val orgFilterWords = Set("senate", "president", "government")
  val orgTypes = Set("inc", "inc.", "corp", "corp.", "corporation", "group", "llc", "llc.", "co.", "company", "co", "ltd", "ltd.", "gmbh", "ag", "llp", "ipc", "plc")
  val orgStops = orgTypes ++ Set("international", "foundation", "association", "party", "administration", "committee")
  val companyTypeExpander = Map[String, String](
    "inc" -> "Incorporation",
    "inc." -> "Incorporation",
    "incorporated" -> "Incorporation",
    "corp." -> "Corporation",
    "corp" -> "Corporation",
    "corporation" -> "Corporation",
    "co." -> "Company",
    "co" -> "Company",
    "company" -> "Company",
    "ltd" -> "Ltd.",
    "ltd." -> "Ltd.",
    "llc" -> "LLC",
    "llc." -> "LLC.",
    "gmbh" -> "GmbH",
    "ag" -> "AG"
  )
  //val gender
  val male = Set("mr.", "mr", "mister", "father", "son", "he", "him", "himself", "father", "grandfather", "nephew", "brother")
  val female = Set("mrs.", "mrs", "ms", "miss", "madam", "ms.", "father", "daughter", "niece", "she", "her", "herself", "mother", "grandmother", "sister")
  //misc
  val acPattern = "[A-Z][a-z']+\\.?"
  val romanNumeralPattern = "(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})"
  val suffix = Set("sr.", "jr.", "sr", "jr", "senior", "junior", "esq", "esq.", "esquire")


  def isTitle(string: String): Boolean = titles.contains(string.toLowerCase)

  def splitIntoTitleAndName(canonicalName: String): (Option[String], Option[String]) = {
    val nameTokens = canonicalName.split(" ")
    var title: Option[String] = None
    var name: Option[String] = None
    var titleFromContext: Option[String] = None
    //first try Wikipedia page extraction; e.g., Michael Jordan (Basketball player)  ==> title:Basketball player name: Michael Jordan
    val context = TFIDFUtils.extractWikiTitleContext(canonicalName)
    if (context.length > 0) {
      titleFromContext = Some(context)
      name = Some(canonicalName.substring(0, canonicalName.indexOf("(")))
    }
    //if that fails, use our humble lexicon to try to find titles at the beginning of the name
    var i = 0
    var slicePoint = 0
    while (title == None && i < nameTokens.length) {
      val unigram = nameTokens(i)
      if (isTitle(unigram)) {
        title = Some(unigram)
        slicePoint = i + 1
      }
      if (i < nameTokens.length - 1) {
        val bigram = unigram + " " + nameTokens(i + 1)
        if (isTitle(bigram)) {
          title = Some(bigram)
          slicePoint = i + 2
        }
      }
      if (title != None) {
        name = Some(nameTokens.slice(slicePoint, nameTokens.length).mkString(" "))
      }
      i += 1
    }
    if (titleFromContext != None) title = titleFromContext
    if (title == None) name = Some(canonicalName)
    (title, name)
  }

  /*
  def extractAcronym(e:TACKBEntity):Unit ={
    canonicalName:String = e.canonicalName.value
    val tokens = canonicalName.split(" ")
    if(tokens.length>=2){

    }
  }*/

  def computeGender(e: TACKBEntity): Unit = {
    var maleVote = 0.0
    var femaleVote = 0.0
    for ((k, v) <- e.bagOfNames.value.iterator) {
      if (AttributeExtraction.male.contains(k)) maleVote += 1.0 //v
      else if (AttributeExtraction.female.contains(k)) femaleVote += 1.0 //v
    }
    for ((k, v) <- e.attributesBag.value.iterator) {
      if (isTitleAttr(k)) {
        val title = titleValueName(k)
        if (male.contains(title)) maleVote += 1.0
        else if (female.contains(title)) femaleVote += 1.0
      }
    }
    //EntityUtils.prettyPrintEntity(e,None,System.out)
    //println("m: "+maleVote+" f: "+femaleVote+" name: "+e.canonicalName.value)
    if (maleVote > femaleVote && femaleVote == 0.0) {
      e.attributesBag += genderPrefix + "m"
    } else if (femaleVote > maleVote && maleVote == 0.0) {
      e.attributesBag += genderPrefix + "f"
    }
  }

  def extractName(e: TACKBEntity): Unit = {
    val canonicalName: String = e.canonicalName.value //.replaceAll("[^A-Za-z ]","").replaceAll(" +"," ")
    val nameSeq = e.bagOfNames.value.iterator.toSeq
    val (titleOption, nameOption) = splitIntoTitleAndName(canonicalName)
    for (title <- titleOption) e.attributesBag += titlePrefix + title.toLowerCase
    //println("Name: "+canonicalName)
    //for(title <- titleOption)println("  -title: "+title)
    for (name <- nameOption) {
      val (firstOption, middleOption, lastOption, suffixOption) = splitFullNameIntoFirstAndLast(name.split(" "))
      if (isValidNameOpt(firstOption) && isValidNameOpt(middleOption) && isValidNameOpt(lastOption)) {
        //for(first <- firstOption)println("  -f: "+first)
        //for(middle <- middleOption)println("  -m: "+middle)
        //for(last <- lastOption)println("  -l: "+last)
        for (first <- firstOption) e.attributesBag += firstNamePrefix + first.toLowerCase
        for (middle <- middleOption) e.attributesBag += middleNamePrefix + middle.toLowerCase
        for (last <- lastOption) {
          e.attributesBag += lastNamePrefix + last.toLowerCase
          e.bagOfNames += TFIDFUtils.normalizeToken(last)
        }
        if (name.indexOf(" ") == -1 && titleOption.isDefined) {
          e.attributesBag += lastNamePrefix + name.toLowerCase
          e.bagOfNames += TFIDFUtils.normalizeToken(name)
        }
        for (suffix <- suffixOption) {
          e.attributesBag += suffixPrefix + suffix
        }
      }
    }
  }

  def splitFullNameIntoFirstAndLast(nameTokens: Array[String]): (Option[String], Option[String], Option[String], Option[String]) = {
    var first: Option[String] = None
    var middle: Option[String] = None
    var last: Option[String] = None
    var suffix: Option[String] = None
    var lastNameIndex = nameTokens.length - 1
    if (isSuffix(nameTokens(nameTokens.length - 1))) {
      suffix = Some(nameTokens(nameTokens.length - 1))
      lastNameIndex = nameTokens.length - 2
    }
    if (nameTokens.length >= 2) {
      first = Some(nameTokens.head)
      if (nameTokens.length >= 3) middle = Some(nameTokens.slice(1, lastNameIndex).mkString(" "))
      last = Some(nameTokens(lastNameIndex))
    }
    Tuple4(first, middle, last, suffix)
  }

  val nameRegex = "[A-Z][a-z]*\\.?"

  def isValidNameOpt(nameOpt: Option[String]) = nameOpt == None || isValidName(nameOpt.get)

  def isValidName(s: String) = s.matches(nameRegex)

  def isSuffix(s: String): Boolean = isRomanNumeral(s) || suffix.contains(s.toLowerCase)

  def isRomanNumeral(s: String): Boolean = s.matches(romanNumeralPattern)

  def getAcronyms(name: String): Seq[String] = {
    val result = new ArrayBuffer[String]
    if (name.length > 0 && name.indexOf(",") == -1) {
      val tokens = name.split(" ")
      if (tokens.length >= 2 && tokens(0).matches(acPattern) && tokens(tokens.length - 1).matches(acPattern)) {
        val basicAcc = tokens.filter(s => s.length >= 1 && s.matches(acPattern) && s != "The").map(s => s.substring(0, 1)).mkString("").toLowerCase
        if (basicAcc.length >= 2 && basicAcc.length <= 7) {
          //println("Acc: "+basicAcc +" for "+name)
          result += basicAcc
        } //else println("Acc not found for "+name)
      }
    }
    result
  }

  def downWeightItem(bag: BagOfWordsVariable, s: String, scale: Double) {
    val oldValue = bag.value(s)
    if (oldValue > 0) {
      bag.remove(s)(null)
      bag.add(s, oldValue / scale)(null)
    }
  }

  def extractAttributesForOrg(e: TACKBEntity) {
    val name = e.canonicalName.value
    if (name.indexOf(",") == -1) {
      val tokens = name.split(" ")
      val tail = tokens(tokens.length - 1)
      if (orgTypes.contains(tail.toLowerCase)) {
        e.attributesBag += AttributeExtraction.orgTypePrefix + "Company"
        val subTypeOption = companyTypeExpander.get(tail.toLowerCase)
        for (subType <- subTypeOption) e.attributesBag += AttributeExtraction.orgSubTypePrefix + subType
        val shorterName = tokens.take(tokens.length - 1).filter(t => t.toLowerCase != "the").mkString(" ")
        e.bagOfNames += TFIDFUtils.normalizeMultiToken(shorterName)
        downWeightItem(e.bagOfNames, tail.toLowerCase, 1000.0)
        downWeightItem(e.bagOfNames, "company", 1000.0)
        if (e.bagOfNames.value(TFIDFUtils.normalizeToken(tail)) > 0) e.bagOfNames.value(TFIDFUtils.normalizeToken(tail))
        //println("Found company: "+name+" shorterName "+shorterName)
        //println("  -predicted name: "+shorterName)
        //for (subType <- subTypeOption)println("  -sub type: "+subType)
      }
    }
  }
}
