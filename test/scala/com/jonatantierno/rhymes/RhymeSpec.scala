package com.jonatantierno.rhymes
import org.scalatest._

class RhymeSpec extends FlatSpec with Matchers with Rhymes{

  "Consonant rhymes" should "be detected" in {
       rhymesWith("Salón","jamón") should be (true)
       rhymesWith("Salón","paco") should be (false)
       rhymesWith("la trócola","cola") should be (false)
  }
  "A word" should "not rhyme with itself" in {
       rhymesNoRepeat("mundo","mundo") should be (false)
  }
  "A word" should "rhyme with its suffix" in {
       rhymesNoRepeat("el profundo","fundo") should be (true)
       rhymesNoRepeat("me lo fundo","profundo") should be (true)
  }
  "A bad word" should "not rhyme with anything" in {
       rhymesWith("","") should be (false)
       rhymesWith("jj","1614") should be (false)
  }
}

class SyllableSpec extends FlatSpec with Matchers with Syllable {
  "empty word" should "have no syllables" in {
    splitInSyllables("") should be (Nil)
  }
  "1 letter word" should "have 1 syllable" in {
    splitInSyllables("a") should be (List("a"))
  }
  "Final symbols" should "be ignored" in {
    splitInSyllables("a,") should be (List("a"))
    splitInSyllables("a.") should be (List("a"))
    splitInSyllables("a.,.") should be (List("a"))
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
    splitInSyllables("desprecio") should be (List("des", "pre", "cio"))
    splitInSyllables("amplitud") should be (List("am", "pli","tud"))
  } 
  "Four consonants between two vowels" should "group with closest vowel" in {
    splitInSyllables("monstruo") should be (List("mons", "truo"))
    splitInSyllables("obstrucción") should be (List("obs", "truc","ción"))
  }
  "Double consonants" should "count as just one" in {
    splitInSyllables("arroyo") should be (List("a", "rro", "yo"))
    splitInSyllables("allanamiento") should be (List("a", "lla","na", "mien", "to"))
    splitInSyllables("marcha") should be (List("mar", "cha"))
    splitInSyllables("valla") should be (List("va", "lla"))
    splitInSyllables("cachorro") should be (List("ca", "cho","rro"))
  }
  "Hiatus" should "be divided properly" in {
    splitInSyllables("maría") should be (List("ma", "rí","a"))
    splitInSyllables("baúl") should be (List("ba", "úl"))
    splitInSyllables("maíz") should be (List("ma", "íz"))
    splitInSyllables("salía") should be (List("sa", "lí","a"))
    splitInSyllables("oía") should be (List("o", "í", "a"))
    splitInSyllables("preescolar") should be (List("pre", "es", "co", "lar"))
    splitInSyllables("saeta") should be (List("sa", "e", "ta"))
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

class DivideLettersSpec extends FlatSpec with Matchers with Syllable {
  "word" should "split in groups" in {
    divideInGroups("mesa") should be (List("m","e","s","a"))
    divideInGroups("ifrusk") should be (List("i","fr","u","sk"))
    divideInGroups("pasión") should be (List("p","a","s","ió","n"))
  }
  "Empty Consonant group between vowels" should "divide correctly" in {
    divideConsonantsBetweenVowels("") should be ("", "")
  }
  "1 char Consonant group between vowels" should "divide correctly" in {
    divideConsonantsBetweenVowels("l") should be ("", "l")
  }
  "Two Consonant group between vowels" should "divide correctly" in {
    divideConsonantsBetweenVowels("rt") should be ("r", "t")
  }
  "Two Consonant group between vowels, second being r or l" should "both go in second part" in {
    divideConsonantsBetweenVowels("tr") should be ("", "tr")
    divideConsonantsBetweenVowels("cl") should be ("", "cl")
  }
  "Three consonants between two vowels" should "group correctly" in {
    divideConsonantsBetweenVowels("nsp") should be ("ns", "p")
  }
  "Three consonants between two vowels, third being r or l" should "group correctly" in {
    divideConsonantsBetweenVowels("spr") should be ("s", "pr")
    divideConsonantsBetweenVowels("mpl") should be ("m", "pl")
  }
  "Four consonants between two vowels" should "divide correctly" in {
    divideConsonantsBetweenVowels("nstr") should be ("ns", "tr")
  }
  "Double consonant" should "count as one" in {
    divideConsonantsBetweenVowels("ll") should be ("", "ll")
    divideConsonantsBetweenVowels("rr") should be ("", "rr")
    divideConsonantsBetweenVowels("nch") should be ("n", "ch")
    divideConsonantsBetweenVowels("rch") should be ("r", "ch")
    divideConsonantsBetweenVowels("ch") should be ("", "ch")
  }
}

class StressSpec extends FlatSpec with Matchers with Stress {
  "Aguda words" should "be stressed in the last syllable" in {
    stressSyllable(List("co","mer")) should be (0)
    stressSyllable(List("bi","llón")) should be (0)
    stressSyllable(List("pa","red")) should be (0)
    stressSyllable(List("ca","fé")) should be (0)
  } 
  "Llana words" should "be stressed in the previous to last syllable" in {
    stressSyllable(List("dul","ce")) should be (1)
    stressSyllable(List("dé","bil")) should be (1)
    stressSyllable(List("lá","piz")) should be (1)
    stressSyllable(List("me","sa")) should be (1)
  } 
  "Llana words ending in n or s" should "be stressed in the previous to last syllable" in {
    stressSyllable(List("re","tra","tos")) should be (1)
    stressSyllable(List("jo","ven")) should be (1)
  } 
  "Words with tilde" should "be stressed in that syllable" in {
    stressSyllable(List("bo","lí","gra","fo")) should be (2)
    stressSyllable(List("có","me","te","lo")) should be (3)
  }

  "Stressed words" should "be named correctly" in {
    name(List("có","me","te","lo")) should be ("Sobresdrújula")
    name(List("mé","to","do")) should be ("Esdrújula")
    name(List("pa","na")) should be ("Llana")
    name(List("la","tón")) should be ("Aguda")
  }
}

class SentenceSpec extends FlatSpec with Matchers with Sentence {
  "Function" should "divide text in sentences" in {
    splitInSentences("Frase uno, frase dos. Frase tres. Frase cuatro") should be (
      List("Frase uno"," frase dos"," Frase tres"," Frase cuatro")
    )
  }
  "Last word of an empty sentence" should "be blank" in {
    lastWord("/n") should be ("/n")
  }
  "Hiatus" should "be ignored when in consecutive words" in {
    splitSentenceInSyllables(List("siempre","hace", "frío")) should be (List("siem", "preha","ce", "frí", "o"))
    splitSentenceInSyllables(List("siembra","antes", "de","tiempo")) should be (List("siem", "braan","tes", "de","tiem","po"))
  } 
  "Letter y" should "form hiatus in sentences when possible" in {
    splitSentenceInSyllables(List("mira","y", "aprende")) should be (List("mi", "ray","a", "pren", "de"))
    splitSentenceInSyllables(List("mira","y", "sigue")) should be (List("mi", "ray","si", "gue"))
    splitSentenceInSyllables(List("la","yema")) should be (List("la", "ye","ma"))
  }
  "number of syllables in sentence" should "be correct" in {
    numberOfSyllables("mi carro me lo robaron") should be (8) 
    numberOfSyllables("siempre estás bien") should be (4) 
  }
}
