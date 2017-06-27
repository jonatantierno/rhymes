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
}
