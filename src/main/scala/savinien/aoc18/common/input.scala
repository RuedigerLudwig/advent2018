package savinien.aoc18.common

import zio.*

type AdventInput = Has[AdventInput.Service]

object AdventInput:
  trait Service:
    def getData: AdventTask[String]

  def live(day: Int): Layer[Nothing, AdventInput] =
    ZLayer.succeed[AdventInput.Service] {
      new Service:
        override def getData: AdventTask[String] =
          FileReader.getContent(f"input/day$day%02d.txt")
    }

def getData: ZIO[AdventInput, Throwable, String] =
  ZIO.accessM(_.get.getData)
