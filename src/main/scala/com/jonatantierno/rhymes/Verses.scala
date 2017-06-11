package com.jonatantierno.rhymes

trait Verse extends Rhymes{
    def getVerses(target: String, text: String): String = 
      flattenResult(getVersesAsList(target,text))

    def getVersesAsList(target: String, text: String): List[String] = 
        getRhymes(lastWord(target), text)
          .filter((rhyme: String) => numberOfSyllables(rhyme) == numberOfSyllables(target))

    def describeWord(word: String): String = {
      val syllables = splitInSyllables(word)
      val prettySyllables = syllables.tail.foldLeft(syllables.head)(_ + "-" + _)
      val stressName = name(syllables)
      s"$prettySyllables ($stressName)\n"
    }

    def getRhymesAsString(target: String, text: String): String = 
      flattenResult(getRhymes(target,text))

    def flattenResult(res: List[String]): String =
      if (res.length == 0) "No se ha encontrado\n"
      else res.foldLeft("")(_ + "\n" + _).concat("\n")

    def getRhymes(target: String, text: String): List[String] = 
      splitInSentences(text).filter(rhymesNoRepeat(_, target)).map(_.replace("\n"," ").replace("\r"," ").trim())
}
