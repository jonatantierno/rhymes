package com.jonatantierno.rhymes

trait Rhymes {
   def rhymeConsonant(a: String, b: String): Boolean = true
}

sealed abstract class ExtractedSyllable {
  def get: String
}
case object EmptySyllable extends ExtractedSyllable {
  def get = ""
}
case class NoVowelYetSyllable(s: String) extends ExtractedSyllable {
  def get = s
}
case class VowelReachedSyllable(s: String) extends ExtractedSyllable {
  def get = s
}


trait Syllable extends Letter{
  def splitInSyllables(word: String): List[String] = {
    splitInSyllables(Nil, EmptySyllable, word)
  }

  def splitInSyllables(alreadyProcessed: List[String], syllable: ExtractedSyllable, rest: String): List[String] = Nil

  def splitWhen(s:String, predicate:Char => Boolean): (String, String) = 
    (s.takeWhile(!predicate.apply(_)),s.dropWhile(!predicate.apply(_))) 
}

trait Letter {
  def isVowel(letter: Char): Boolean = return "aeiouAEIOUáéíóúüÁÉÍÓÚÜ".contains(letter)

  def isLetter(letter: Char): Boolean = 
    if (isVowel(letter)) true
    else if (letter == 'ñ') true
    else if (letter >= 'A' && letter <= 'Z') true
    else if (letter >= 'a' && letter <= 'z') true
    else false

}
