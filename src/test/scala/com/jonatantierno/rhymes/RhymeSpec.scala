package com.jonatantierno.rhymes
import org.scalatest._

@Ignore
class RhymeSpec extends FlatSpec with Matchers with Rhymes{

  "'Aguda' words with tilde" should "rhyme with consonant rhyme" in {
       rhymeConsonant("Salón","jamón") should be (true)
  }

  "Non-Aguda words with tilde" should "not rhyme" in {
       rhymeConsonant("Salón","canon") should be (false)
  }
}

@Ignore
class SyllableSpec extends FlatSpec with Matchers with Syllable {
  "empty word" should "have no syllables" in {
    splitInSyllables("") should be (Nil)
  }
  "1 letter word" should "have 1 syllable" in {
    splitInSyllables("a") should be (List("a"))
  }
  "Two consonants between two vowels" should "group each with its closest vowel" in {
    splitInSyllables("arte") should be (List("ar","te"))
    splitInSyllables("contento") should be (List("con","ten","to"))
  }
  "Exception: Two consonants between two vowels, second being r or l" should "both group with second vowel" in {
    splitInSyllables("abrigo") should be (List("a", "bri", "go"))
    splitInSyllables("regla") should be (List("re", "gla"))
  } 
  "Three consonants between two vowels" should "first two go left, third goes right" in {
    splitInSyllables("inspección") should be (List("ins", "pec", "ción"))
    splitInSyllables("transmitir") should be (List("trans", "mi", "tir"))
  } 
  "Exception: Three consonants between two vowels, third being r or l" should "first goes left, rest go right" in {
    splitInSyllables("abrigo") should be (List("a", "bri", "go"))
    splitInSyllables("regla") should be (List("re", "gla"))
  } 
  "Four consonants between two vowels" should "group with closest vowel" in {
    splitInSyllables("monstruo") should be (List("mons", "truo"))
    splitInSyllables("obstrucción") should be (List("obs", "truc","ción"))
  }
  "Double consonants" should "count as just one" in {
    splitInSyllables("arroyo") should be (List("a", "rro", "yo"))
    splitInSyllables("allanamiento") should be (List("a", "lla","na", "mien", "to"))
  }
}

class SyllableSpecAux extends FlatSpec with Matchers with Syllable {
  "word" should "split" in {
    splitWhen("abcde", _ == 'd') should be (("abc","de"))
  }
}

class LetterSpec extends FlatSpec with Matchers with Letter {
  "vowels" should "be recognised" in {
    isVowel('a') should be (true)
    isVowel('é') should be (true)
    isVowel('ü') should be (true)
    isVowel('A') should be (true)
    isVowel('Í') should be (true)
    isVowel('z') should be (false)
    isVowel('ñ') should be (false)
  }

  "letters" should "be recognised" in {
    isLetter('A') should be (true)
    isLetter('M') should be (true)
    isLetter('Z') should be (true)
    isLetter('a') should be (true)
    isLetter('m') should be (true)
    isLetter('z') should be (true)
    isLetter('é') should be (true)
    isLetter('ñ') should be (true)
    isLetter('ü') should be (true)
    isLetter('.') should be (false)
  }

}
