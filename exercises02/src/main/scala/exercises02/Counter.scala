package exercises02

import scala.util.matching.Regex

object Counter {

  /**
    * Посчитать количество вхождений слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countWords(text: String): Map[String, Int] =
    text.split("[\\s.,!?:\\n\\t\\r()]").filter(_.nonEmpty).groupBy(_.toLowerCase).view.mapValues(_.length).toMap

  /**
    * Посчитать количество вхождений английских слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countEnglishWords(text: String): Map[String, Int] =
    countWords(text).filter(pair => raw"[\w-']+".r.pattern.matcher(pair._1).matches)

  /**
    * Посчитать количество вхождений чисел в тексте
    * число отделено символами [\s!?:\n\t\r]
    */
  def countNumbers(text: String): Map[String, Int] =
    text.split("[\\s!?:\\n\\t\\r]").filter(word => raw"[\d.,]+".r.pattern.matcher(word).matches).groupBy(_.toLowerCase).view.mapValues(_.length).toMap

}
