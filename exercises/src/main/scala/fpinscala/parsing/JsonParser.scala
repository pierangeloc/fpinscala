package fpinscala.parsing

import fpinscala.parsing.JSON.JNumber


trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  /**
   * a jsonParser based on a representation of Parsers
   * @param P
   * @tparam Parser
   * @return
   */
  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    
  }
}