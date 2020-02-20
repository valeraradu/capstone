package observatory

import java.io.File

object Main extends App {

  val path = getClass.getResource("/")
  val folder = new File(path.getPath)
  val tempFiles =  folder.listFiles.filter(file => file.getName.matches("\\d\\d\\d\\d\\.csv"))
    //.filter(_.getName.matches("2015.csv"))

  tempFiles.foreach(println)

  tempFiles.foreach(file => {

    val year = file.getName.substring(0, 4).toInt

    val yearData = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(
      year,
      "/stations.csv",
      s"/${year}.csv"))

    Interaction.generateTiles(List((year, yearData)), processYear)
  })

  def processYear = (year: Year, tile: Tile, data: Iterable[(Location, Temperature)]) => {

    val color = Interaction2.availableLayers.head.colorScale

    new File(folder.getParentFile.getParentFile +  s"/temperatures/$year/${tile.zoom}").mkdirs()
    val file = new File(folder.getParentFile.getParentFile  + s"/temperatures/$year/${tile.zoom}/${tile.x}-${tile.y}.png")

    if(!file.exists()){

      println("starting computing tile " + tile)
      val t1 = System.currentTimeMillis/1000

      Interaction.tile(data, color, tile).output(file)

      val elapsed_s = System.currentTimeMillis/1000 - t1
      val s = elapsed_s % 60
      val m = (elapsed_s/60) % 60
      val h = (elapsed_s/60/60)
      println("elapsed time for tile  " + tile + " "  + "%02d:%02d:%02d".format(h, m, s))
    }

  }
}
