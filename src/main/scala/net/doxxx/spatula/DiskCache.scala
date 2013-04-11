package net.doxxx.spatula

import spray.caching.Cache
import scalax.file.Path
import scala.io.Codec
import scala.concurrent._
import scala.util.Success

class DiskCache(cacheDir: Path) extends Cache[String]  {
  import ExecutionContext.Implicits.global

  implicit val code = Codec.UTF8

  def get(key: Any): Option[Future[String]] = {
    val file = cacheDir / key.toString
    if (file.exists) {
      Some(future {
//        println("Reading from %s".format(file.path))
        file.string
      })
    }
    else {
      None
    }
  }

  def fromFuture(key: Any)(f: => Future[String])(implicit executor: ExecutionContext): Future[String] = {
    val file = cacheDir / key.toString
    if (file.exists) {
      future {
//        println("Reading from %s".format(file.path))
        file.string
      }
    }
    else {
      f.andThen {
        case Success(s) => {
//          println("Writing %d chars to %s".format(s.length, file.path))
          file.write(s)
        }
      }
    }
  }

  def remove(key: Any): Option[Future[String]] = {
    val file = cacheDir / key.toString
    val r = get(key)
//    println("Deleting %s".format(file.path))
    file.deleteIfExists()
    r
  }

  def clear() {
    cacheDir.children().foreach(_.delete())
  }
}
