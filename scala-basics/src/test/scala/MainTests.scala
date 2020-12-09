import org.scalatest.FunSuite
import movies.Movies._
class MainTests extends FunSuite {
  def testData = {
    Vector(
      MovieDetails("title1", "director1", 2003, "language1", 5, 500000, "country1", 180, 10),
      MovieDetails("title2", "director2", 2009, "language2", 10, 70000, "country3", 150, 18),
      MovieDetails("title3", "director1", 2005, "language1", 12, 80000, "country2", 120, 1),
      MovieDetails("title4", "director2", 2010, "language2", 10, 900000, "country2", 180, 10),
      MovieDetails("title5", "director3", 2015, "language1", 18, 60000, "country1", 150, 13)
    )
  }

  def generateResult(data: Seq[MovieDetails], idxs: Seq[Int]) =
    idxs.collect { case i => data(i) }

  test("sanity test") {
    assert(2 + 2 == 4)
  }
  
  test("movies directed by director1 between 2000 and 2010") {
    val actual = first(testData, "director1", 2000, 2010)
    val expected = generateResult(testData, Vector(0, 2))
    assert(actual == expected)
  }

  test("language1 titles having user reviews more than 10 sorted descending") {
    val actual = second(testData, "language1", 10)
    val expected = generateResult(testData, Vector(4, 2))
    assert(actual == expected)
  }

  test("highest budget title in 2010  in country2") {
    val actual = third(testData, 2010, "country2")
    val expected = testData(3)
    assert(actual == expected)
  }

  test("longest titles in country1 with at least 8 votes sorted descending") {
    val actual = fourth(testData, "country1", 8)
    val expected = generateResult(testData, Vector(0, 4))
    assert(actual == expected)
  }

  test("language wise report with count of titles in budget 10000 and 100000 in country1 sorted descending"){
    val actual = fifth(testData, 10000, 100000)
    val expected = Vector(("language1", 2), ("language2", 1))
    assert(actual == expected)
  }
}
