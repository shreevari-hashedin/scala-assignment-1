import java.io.FileNotFoundException
import scala.util.{Failure, Success}
import movies.Movies.{parseRows, processFile, assignment}
object Main extends App {

  if (args.size == 0) {
    println("Please provide path to data source file")
  } else {
    val fileName = args(0)
    val limit = 10000
    processFile(fileName, limit) match {
      case Failure(_: FileNotFoundException) => println("file not found")
      case Failure(_) => println("unknown error")
      case Success(r) => assignment(parseRows(r))
    }
  }
}
