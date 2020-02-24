package observatory

import java.io.File
import java.nio.file.{Files, Paths}

import com.sksamuel.scrimage.{Image, Pixel}

import scala.io.Source

object Main extends App {

  val path = getClass.getResource("/")
  val folder = new File(path.getPath)
  val tempFiles = folder.listFiles.filter(file => file.getName.matches("\\d\\d\\d\\d\\.csv"))
  //.filter(_.getName.matches("2015.csv"))

  val colorScheme = Interaction2.availableLayers.head.colorScale

  val srcfolder = "temperatures_encoded_z3"
  val destfolder = "temperatures"
  val deviationsFolder = "deviations"
  val normFolder = "norm"

  tempFiles.foreach(println)

  tempFiles.foreach(file => {

    val year = file.getName.substring(0, 4).toInt

    //val yearData = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(
    //  year,
    //  "/stations.csv",
    //  s"/${year}.csv"))

    //Interaction.generateTiles(List((year, yearData)), processYear)

    //decodeTile(year)

  })

  def processYear = (year: Year, tile: Tile, data: Iterable[(Location, Temperature)]) => {

    new File(folder.getParentFile.getParentFile + s"/$srcfolder/$year/${tile.zoom}").mkdirs()
    val file = new File(folder.getParentFile.getParentFile + s"/$srcfolder/$year/${tile.zoom}/${tile.x}-${tile.y}")

    if (!file.exists()) {

      println("starting computing tile " + tile)
      val t1 = System.currentTimeMillis / 1000

      //Interaction.tile(data, colorScheme, tile).output(file)

      val raw = Interaction.tileRaw(data, tile)

      Files.write(Paths.get(file.getAbsolutePath), raw.mkString("\n").getBytes)

      val elapsed_s = System.currentTimeMillis / 1000 - t1
      val s = elapsed_s % 60
      val m = (elapsed_s / 60) % 60
      val h = (elapsed_s / 60 / 60)
      println("elapsed time for tile  " + tile + " " + "%02d:%02d:%02d".format(h, m, s))
    }

  }

  def decodeTile(year: Year): Unit = {

    new File(folder.getParentFile.getParentFile + s"/$destfolder/$year/3").mkdirs()

    new File(folder.getParentFile.getParentFile + s"/$srcfolder/$year/3")
      .listFiles
      .map(tile => {
      val src = Source.fromFile(tile.getAbsolutePath).getLines.toList.map(temp => {
        val color = Visualization.interpolateColor(colorScheme, temp.toDouble)
        Pixel(color.red, color.green, color.blue, 127)
      })

      Image(256, 256, src.toArray).output(
        new File(folder.getParentFile.getParentFile + s"/$destfolder/$year/3/${tile.getName}.png")
      )
    })
  }

  def calculateNorm(min: Year, max: Year): Unit = {

    val normFolder = "norm"

    new File(folder.getParentFile.getParentFile + s"/$normFolder/3/").mkdirs()
    val listOfTiles =  new File(folder.getParentFile.getParentFile + s"/$srcfolder/$min/3").listFiles

    listOfTiles.foreach(tile => {
      println(tile)
      val listOfTemps = (min to max).map(year => {
          val fileName = folder.getParentFile.getParentFile + s"/$srcfolder/$year/3/${tile.getName}"
          Source.fromFile(fileName).getLines.toList.map(_.toDouble)
        })

      val meanTempList = listOfTemps.foldLeft(List.fill(256 * 256)(0.0))((list1, list2) =>
        (list2 zip list1).map { case (l1, l2) => l1 + l2 }).map(v => v / listOfTemps.size)

      val file = new File(folder.getParentFile.getParentFile + s"/$normFolder/3/${tile.getName}")

      Files.write(Paths.get(file.getAbsolutePath), meanTempList.mkString("\n").getBytes)

    })
  }

  //calculateNorm(1975, 1990)

  def calculateDeviation(min: Year, max: Year): Unit = {

    val listOfTiles =  new File(folder.getParentFile.getParentFile + s"/$normFolder/3").listFiles

    listOfTiles.foreach(tile => {
      println(tile)
      (min to max).map(year => {

        new File(folder.getParentFile.getParentFile + s"/$deviationsFolder/${year}/3/").mkdirs()

        val src = folder.getParentFile.getParentFile + s"/$srcfolder/$year/3/${tile.getName}"
        val srcTemps  = Source.fromFile(src).getLines.toList.map(_.toDouble)
        val normTemp = Source.fromFile(tile.getAbsoluteFile).getLines.toList.map(_.toDouble)

        val deviationTemps = (srcTemps zip normTemp).map(pair => {
          val color = Visualization.interpolateColor(colorScheme, pair._1 - pair._2)
          Pixel(color.red, color.green, color.blue, 127)
        })

        Image(256, 256, deviationTemps.toArray).output(
          new File(folder.getParentFile.getParentFile + s"/$deviationsFolder/${year}/3/${tile.getName}.png"))

      })
    })
  }

  calculateDeviation(1990, 2015)
}
