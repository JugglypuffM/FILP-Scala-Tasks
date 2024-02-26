package exercises02

import scala.util.matching.Regex

object Counter {
  private val wordSeparatorsRegex = raw"(.*?)[\s.,!?:\n\t\r()]".r
  private val intSeparatorsRegex = raw"(.*?)[\s!?:\n\t\r]".r
  private val wordRegex = raw"[\w-']+".r
  private val intRegex = raw"[\d.,]+".r
  def count(text:String, separatorsRegex: Regex, filter: String => Boolean): Map[String, Int] =
    (for (m <- separatorsRegex.findAllMatchIn(text)) yield m.group(1)).toList
    .filter(filter)
    .groupBy(_.toLowerCase)
    .view
    .mapValues(_.length)
    .toMap
  /**
    * Посчитать количество вхождений слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countWords(text: String): Map[String, Int] =
    count(text+" ", wordSeparatorsRegex, _.nonEmpty)

  /**
    * Посчитать количество вхождений английских слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countEnglishWords(text: String): Map[String, Int] =
    count(text+" ", wordSeparatorsRegex, wordRegex.pattern.matcher(_).matches)


  /**
    * Посчитать количество вхождений чисел в тексте
    * число отделено символами [\s!?:\n\t\r]
    */
  def countNumbers(text: String): Map[String, Int] =
    count(text+" ", intSeparatorsRegex, intRegex.pattern.matcher(_).matches)

}
