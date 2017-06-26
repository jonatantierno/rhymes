package com.jonatantierno.rhymes

trait Letter {
  def isVowel(letter: Char): Boolean = "aeiouAEIOUáéíóúüÁÉÍÓÚÜ".contains(letter)

  def areVowels(letters: String): Boolean = letters.forall(isVowel(_))

  def areStrongVowels(group: String): Boolean = group.toLowerCase.forall("aeoáéíóú".contains(_))

  def isDoubleConsonant(letters: String): Boolean = letters.equals("ch") || letters.equals("ll") || letters.equals("rr");
  def endsWithDoubleConsonant(letters: String): Boolean = isDoubleConsonant(letters.takeRight(2))

  def isLetter(letter: Char): Boolean = 
    if (isVowel(letter)) true
    else if (letter == 'ñ') true
    else if (letter == 'Ñ') true
    else if (letter >= 'A' && letter <= 'Z') true
    else if (letter >= 'a' && letter <= 'z') true
    else false

  def isConsonant(letter: Char): Boolean = isLetter(letter) && !isVowel(letter) 
  def areConsonants(letters:String): Boolean = letters.forall(isConsonant(_))
}

trait Syllable extends Letter{
  def splitInSyllables(word: String): List[String] = { 
    val groups:List[String] = divideInGroups(word)
    val syllables:List[String] = groups.foldLeft(List[String]())(classifyNextGroup)

    if(syllables == List()) List() 
    else if (areConsonants(syllables.last)) appendToLastGroup(syllables.dropRight(1),syllables.last)
    else syllables
  }
  private def last(syllables: List[String]): String = if (syllables.isEmpty) "" else syllables.last

  private def classifyNextGroup(syllables:List[String], nextGroup:String): List[String] =
      if (syllables == List()) List(nextGroup)
      else if (areConsonants(last(syllables))) 
        if (areVowels(nextGroup)) appendToLastGroup(syllables, nextGroup)
        else syllables :+ nextGroup
      else if(areVowels(nextGroup)) syllables :+ nextGroup
      else {
         val (left,right) = divideConsonantsBetweenVowels(nextGroup) 
         appendToLastGroup(syllables,left) :+ right
        }

  def divideInGroups(word:String):List[String] = 
    word.foldLeft(List[String]())(incorporateToGroups).flatMap(divideHiatus(_))

  private def incorporateToGroups(groups: List[String], nextLetter: Char):List[String] =
      if (groups.isEmpty) List(nextLetter.toString)
      else if (isVowel(groups.last.head) == isVowel(nextLetter)) appendToLastGroup(groups, nextLetter)
      else groups :+ nextLetter.toString

  private def divideHiatus(group: String): List[String] = 
    if (areStrongVowels(group)) stringAsList(group)
    else List(group)

  private def stringAsList(group: String): List[String] =
    group.map(_.toString).toList

  private def appendToLastGroup(groups: List[String], nextLetter: Char): List[String] = appendToLastGroup(groups, nextLetter.toString)

  private def appendToLastGroup(groups: List[String], nextLetters: String): List[String] = {
          val lastGroup = last(groups) + nextLetters
          groups.dropRight(1) :+ lastGroup 
  }

  def divideConsonantsBetweenVowels(consonants: String): (String, String) = {
    if (consonants.isEmpty) ("","") 
    else if (consonants.length == 1) ("",consonants) 
    else if (isDoubleConsonant(consonants)) ("",consonants) 
    else if (endsWithDoubleConsonant(consonants)) (consonants.take(consonants.length-2),consonants.takeRight(2)) 
    else if (consonants.length == 2) 
      if (endsInRL(consonants)) ("", consonants) 
      else (consonants.head.toString, consonants.tail)
    else if (consonants.length == 3) 
      if (endsInRL(consonants)) (consonants.head.toString, consonants.tail) 
      else (consonants.take(2), consonants.takeRight(1)) 
    else if (consonants.length == 4)  (consonants.take(2), consonants.takeRight(2)) 
    else ("","") 
  }

  private def endsInRL(consonants: String):Boolean = consonants.endsWith("r") || consonants.endsWith("l") 
}

trait Stress extends Letter{
  def stressSyllable(word: List[String]): Int = {  
    val tildeIndex = word.indexWhere(_.matches(".*[áéíóúÁÉÍÓÚ].*"))
    if (tildeIndex != -1) word.length - tildeIndex - 1
    else if (word.length <=1) 0 
    else if (word.last.endsWith("n"))  1
    else if (word.last.endsWith("s"))  1
    else if (isVowel(word.last.last))  1
    else  0
  }
  def name(word: List[String]): String= {  
    val index = stressSyllable(word) 
    if (index == 0) "Aguda"
    else if (index == 1) "Llana"
    else if (index == 2) "Esdrújula"
    else if (index == 3) "Sobresdrújula"
    else "Inválida"
  }
}

trait Sentence extends Syllable{
  def splitInSentences(text:String): List[String] = text.split("[.;,?!\"]").toList
  def lastWord(sentence: String): String = sentence.split(" ").takeRight(1).foldLeft("")(_ concat _)
  def numberOfSyllables(sentence: String): Int = splitSentenceInSyllables(sentence.split(" ").toList).size
  def splitSentenceInSyllables(sentence: List[String]): List[String] = { 
    def hiatusLeft(syllable: String): Boolean = isVowel(syllable.lastOption.getOrElse('-')) 
    def hiatusRight(syllable: String): Boolean = isVowel(syllable.headOption.getOrElse('-')) || syllable.startsWith("h") || syllable.equals("y")
    def possibleHiatusLeft(syllables: List[String]): Boolean = syllables match { 
      case Nil => false 
      case a => hiatusLeft(a.lastOption.getOrElse(""));
    }
    def possibleHiatusRight(rest: List[String]): Boolean = rest match {
      case Nil => false 
      case head :: _ => hiatusRight(head);
    } 
    def fuse(syllables: List[String], rest: List[String]): List[String] =
      syllables.dropRight(1) ::: syllables.takeRight(1).map(_ concat rest.head) ::: rest.drop(1)

    sentence.map(splitInSyllables(_)).foldLeft(Nil:List[String])( (syllables, rest) => 
        if (possibleHiatusLeft(syllables) && possibleHiatusRight(rest)) fuse(syllables, rest)
        else syllables ::: rest 
        )
  }
}

trait Rhymes extends Syllable with Stress with Sentence {
   def rhymesWith(sentence: String, word: String): Boolean = rhymingSuffix(word) equals rhymingSuffix(lastWord(sentence))
   def rhymesNoRepeat(sentence: String, word: String): Boolean = rhymesWith(sentence, word) && !lastWord(sentence).equals(word) 
  
   private def rhymingSuffix(word: String): String = {
      val syllables = splitInSyllables(word) 
      syllables.takeRight(stressSyllable(syllables) + 1).foldLeft("")(_ concat _).dropWhile(letter => isConsonant(letter))
   } 
}

