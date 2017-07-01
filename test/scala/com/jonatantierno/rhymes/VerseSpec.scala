package com.jonatantierno.rhymes
import org.scalatest._

class VerseSpec extends FlatSpec with Verse with Matchers {
   "Rhymes" should "get sentences that rhyme" in {
        getRhymes("rimes", "No estimes.Pica.") should contain ("No estimes") 
   }

   "Verses" should "have same length" in  {
        getVersesAsList("caracola","como mola") should contain ("como mola") 
        getVersesAsList("caracola","que mola") shouldBe empty 
   }
   "Verses" should "Rhyme with list of sentences" in  {
        findRhymesInListAsList("caracola",List("como mola")) should contain ("como mola") 
        findRhymesInListAsListStrict("caracola",List("como mola")) should contain ("como mola") 
        findRhymesInListAsList("caracola",List("que mola")) should contain ("que mola")  
        findRhymesInListAsListStrict("caracola",List("que mola")) shouldBe empty
   }
}
