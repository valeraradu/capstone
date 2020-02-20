package observatory

import java.util.concurrent.ConcurrentHashMap

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    Location(toDegrees(atan(sinh(Pi * (1.0 - 2.0 * tile.y.toDouble / (1 << tile.zoom))))),
      tile.x.toDouble / (1 << tile.zoom) * 360.0 - 180.0)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */

  //val pixelHashes  = new ConcurrentHashMap[Location, Pixel]()

  def tile(temperatures: Iterable[(Location, Temperature)],
           colors: Iterable[(Temperature, Color)], tile: Tile): Image = {


    //println("pixel hash size " + pixelHashes.size)

    val pixels = for {
      y <- tile.y * 256 to tile.y * 256 + 255
      x <- tile.x * 256 to tile.x * 256 + 255
    } yield {

      //val pixelHash = (x*1.0/(tile.zoom+1), y*1.0/(tile.zoom+1))

      val location = tileLocation(Tile(x, y, tile.zoom + 8))

      //pixelHashes.getOrDefault(location, {
      val color = interpolateColor(colors, predictTemperature(temperatures, location))
      //val pixel =
      Pixel(color.red, color.green, color.blue, 127)
      //if (tile.zoom < 3) pixelHashes.put(location, pixel)
      //pixel
      //})

    }

    Image(256, 256, pixels.toArray)
  }

  def zoomTile(tile: Tile, depth: Int): Seq[Tile] = {
    if (depth <= 0) {
      Seq()
    } else {
      for {
        y <- tile.y * 2 to tile.y * 2 + 1
        x <- tile.x * 2 to tile.x * 2 + 1
      } yield {
        Tile(x, y, tile.zoom + 1) +: zoomTile(Tile(x, y, tile.zoom + 1), depth - 1)
      }
      }.flatten
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    *
    *                      This method generates all the tiles for a given dataset yearlyData, for zoom levels 0 to 3 (included).
    *                      The dataset contains pairs of (Year, Data) values, or, said otherwise, data associated with years. In your case,
    *                      this data will be the result of Extraction.locationYearlyAverageRecords. The second parameter of the generateTiles
    *                      method is a function that takes a year, the coordinates of the tile to generate, and the data associated with the
    *                      year, and computes the tile and writes it on your filesystem.
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Year, Data)],
                           generateImage: (Year, Tile, Data) => Unit
                         ): Unit = {
    yearlyData.foreach(data => {

      val t1 = System.currentTimeMillis / 1000

      val tray = new ConcurrentHashMap[Tile, Tile]()

      (Tile(0, 0, 0) +: zoomTile(Tile(0, 0, 0), 3)).map(tail => {
          tray.put(tail, tail)
      })


      val threads = (0 to 7).map(thread =>
        Future {
          println(s"thread ${thread} started ")
          var task = if (tray.keys().hasMoreElements) tray.remove(tray.keys().nextElement()) else null
          while (task != null) {
            generateImage(data._1, task, data._2)
            task = if (tray.keys().hasMoreElements) tray.remove(tray.keys().nextElement()) else null
          }
          println(s"thread ${thread} exit ")
        }
      )

      val all = Future.sequence(threads)

      Await.result(all, Duration.Inf)

      val elapsed_s = (System.currentTimeMillis / 1000 - t1)
      val residual_s = elapsed_s % 60
      val residual_m = (elapsed_s / 60) % 60
      val elapsed_h = (elapsed_s / 60 / 60)
      println("elapsed time for year  " + data._1 + " " + "%02d:%02d:%02d".format(elapsed_h, residual_m, residual_s))

    })

  }
}

/*yearlyData.foreach(data => {
(Tile(0,0,0) +: zoomTile(Tile(0,0,0), 3)).groupBy(t => t.zoom).toList.sortBy(_._1)
.foreach(zoom => {
println("starting computing zoom " + zoom._1)
println("tiles for zoom " + zoom._1 + " " + zoom._2)
val t1 = System.currentTimeMillis/1000

val slide = if( zoom._2.size <= 4) 1 else zoom._2.size/8

val threads =
zoom._2.sliding(slide, slide).map(group => Future {
println("thread started for tiles " + group.toList)
group.foreach(tile => generateImage(data._1, tile, data._2))
})


val all = Future.sequence(threads)

Await.result(all, Duration.Inf)

val elapsed_s = (System.currentTimeMillis/1000 - t1)
val residual_s = elapsed_s % 60
val residual_m = (elapsed_s/60) % 60
val elapsed_h = (elapsed_s/60/60)
println("elapsed time for zoom  " + zoom._1 + " "  + "%02d:%02d:%02d".format(elapsed_h, residual_m,
residual_s))
})
})*/

