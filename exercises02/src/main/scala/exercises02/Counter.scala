package exercises02

object Counter {
  private val wordSeparators = "[\\s.,!?:\\n\\t\\r()]"
  private val intSeparators = "[\\s!?:\\n\\t\\r]"
  private val wordRegex = raw"[\w-']+".r
  private val intRegex = raw"[\d.,]+".r
  def count(text:String, separators: String, filter: String => Boolean): Map[String, Int] = text
    .split(separators)
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
    count(text, wordSeparators, _.nonEmpty)

  /**
    * Посчитать количество вхождений английских слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countEnglishWords(text: String): Map[String, Int] =
    count(text, wordSeparators, word => wordRegex.pattern.matcher(word).matches)


  /**
    * Посчитать количество вхождений чисел в тексте
    * число отделено символами [\s!?:\n\t\r]
    */
  def countNumbers(text: String): Map[String, Int] =
    count(text, intSeparators, word => intRegex.pattern.matcher(word).matches)

}
