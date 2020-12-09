import scala.io.Source
import scala.collection.mutable.Map
import scala.util.Try

package movies {
object Movies {
    case class MovieDetails(title: String,
    director: String,
    year: Int,
    language: String,
    userReview: Int,
    budget: Int,
    country: String,
    duration: Int,
    votes: Int) {
    override def toString(): String = s"Title: $title |\tDirector: $director |\tYear: $year |\tLanguage: $language |\tUser Review: $userReview |\tBudget: $budget |\tCountry: $country |\tDuration: $duration |\tVotes: $votes\n"
  }

  def parseRows(data: Seq[Map[String, String]]): Seq[MovieDetails] = // Use apply/unapply on case class or implicit convertor instead
    data map (row => MovieDetails(
      row.getOrElse("title", ""),
      row.getOrElse("director", ""),
      toIntHandleNull(row.getOrElse("year", "0")),
      row.getOrElse("language", ""),
      toIntHandleNull(row.getOrElse("reviews_from_users", "0")),
      parseMoney(row.getOrElse("budget", "0")),
      row.getOrElse("country", ""),
      toIntHandleNull(row.getOrElse("duration", "0")),
      toIntHandleNull(row.getOrElse("votes", "0"))
    ))

  def parseMoney(s: String) = "[0-9]+".r.findFirstIn(s).getOrElse("0").toInt
  def toIntHandleNull(s: String) = if (s == "") 0 else s.toInt

  def tokenize(row: String) = row.split("\t").map(_.trim)

  def processFile(filepath: String, limit: Int) = Try { // Add type info and return map for implicit conversion
    val file = Source.fromFile(filepath)
    val lines = file.getLines()

    val headers = tokenize(lines.next())

    var data = Vector[Map[String, String]]()
    for (line <- lines.take(limit)) {
      var row = Map[String, String]()
      for ((header, value) <- headers.zip(tokenize(line))) {
        row += (header -> value)
      }
      data = data :+ row
    }
    data
  }

  def assignment(data: Seq[MovieDetails]) {
    println(first(data, "Louis Feuillade", 1900, 1915))
    println(second(data, "English", 20))
    println(third(data, 1913, "USA"))
    println(fourth(data, "USA", 100))
    println(fifth(data, 10000, 100000))
  }

  def first(data: Seq[MovieDetails], director: String, startYear: Int, endYear: Int): Seq[MovieDetails] =
    data.filter(movie => movie.director == director && movie.year > startYear && movie.year < endYear)

  def second(data: Seq[MovieDetails], language: String, userReviewThreshold: Int): Seq[MovieDetails] =
    data
      .filter(movie => movie.language == language && movie.userReview > userReviewThreshold)
      .sortWith((first, second) => first.userReview > second.userReview)

  def third(data: Seq[MovieDetails], year: Int, country: String) = {
    val filtered = data.filter(movie => movie.year == year && movie.country == country)
    if (!filtered.isEmpty) filtered.maxBy(_.budget)
  }

  def fourth(data: Seq[MovieDetails], country: String, minVotes: Int) = {
    data
      .filter(movie => movie.country == country && movie.votes >= minVotes)
      .sortWith((first, second) => first.duration > second.duration)
  }

  def fifth(data: Seq[MovieDetails], budgetMin: Int, budgetMax: Int) = {
    data
      .filter(movie => movie.budget > budgetMin && movie.budget < budgetMax)
      .groupBy(movie => movie.language)
      .map[(String, Int)](moviesByLang => (moviesByLang._1, moviesByLang._2.length))
      .toVector
      .sortWith((first, second) => first._2 > second._2)
  }
}
}
