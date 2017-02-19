package com.jonatantierno.rhymes

trait SentencesTc[T[_]]{
  def SentenceTc()
  def lastWord(): T[WordTc[T]]
}
trait WordTc[T[_]] {
  def splitInSyllables(): T[List[Syllable]]
  def rhymesWith(other: WordTc[T]): T[List[Syllable]]
  def pretty(): T[String]
  def stressSylableFromEnd(): T[Int]
  def stressTypeName(): T[String]
}

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

  def classifyNextGroup(syllables:List[String], nextGroup:String): List[String] =
      if (syllables == List()) List(nextGroup)
      else if (areConsonants(syllables.last)) 
        if (areVowels(nextGroup)) appendToLastGroup(syllables, nextGroup)
        else syllables :+ nextGroup
      else if(areVowels(nextGroup)) syllables :+ nextGroup
      else {
         val (left,right) = divideConsonantsBetweenVowels(nextGroup) 
         appendToLastGroup(syllables,left) :+ right
        }

  def divideInGroups(word:String):List[String] = 
    word.foldLeft(List[String]())(incorporateToGroups).flatMap(divideHiatus(_))

  def incorporateToGroups(groups: List[String], nextLetter: Char):List[String] =
      if (groups.isEmpty) List(nextLetter.toString)
      else if (isVowel(groups.last.head) == isVowel(nextLetter)) appendToLastGroup(groups, nextLetter)
      else groups :+ nextLetter.toString

  def divideHiatus(group: String): List[String] = 
    if (areStrongVowels(group)) stringAsList(group)
    else List(group)

  def stringAsList(group: String): List[String] =
    group.map(_.toString).toList

  def appendToLastGroup(groups: List[String], nextLetter: Char): List[String] = appendToLastGroup(groups, nextLetter.toString)

  def appendToLastGroup(groups: List[String], nextLetters: String): List[String] = {
          val lastGroup = groups.last + nextLetters
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

  def endsInRL(consonants: String):Boolean = consonants.endsWith("r") || consonants.endsWith("l") 

  def splitWhen(s:String, predicate:Char => Boolean): (String, String) = 
    (s.takeWhile(!predicate.apply(_)),s.dropWhile(!predicate.apply(_))) 
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

trait Sentence {
  def splitInSentences(text:String): List[String] = text.split("[.;,?!\"]").toList
}

trait Rhymes {
   def rhymeConsonant(a: String, b: String): Boolean = true
}

