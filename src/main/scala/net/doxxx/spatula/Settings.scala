package net.doxxx.spatula

import com.typesafe.config.Config
import akka.contrib.throttle.Throttler._
import scala.concurrent.duration._

class Settings(config: Config) {

  val requestRate = config.getInt("spatula.wowdb-api.request-rate").msgsPer(1.second)

}
