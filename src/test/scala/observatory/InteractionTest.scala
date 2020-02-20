package observatory

import org.junit.Assert._
import org.junit.{Rule, Test}

trait InteractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("interactive visualization", 3) _

  @Test def `test tile produces an image of 256×256 pixels`: Unit = {

    val image = Interaction.tile(List(), List(), Tile(0,0,0))

    assert(image.height == 256)
    assert(image.width == 256)
  }

  @Test def `test tile zoomTile produces Tiles up to 1 zoom`: Unit = {

    val tiles = Interaction.zoomTile(Tile(0,0,0), 1)
    assert(tiles.size == 5, s"tile zoom expected to generate 4 but it was ${tiles.size}")
  }

  @Test def `test tile zoomTile produces Tiles up to 2 zoom`: Unit = {

    val tiles = Interaction.zoomTile(Tile(0,0,0), 2)
    assert(tiles.size == 21, s"tile zoom expected to generate 20 but it was ${tiles.size}")
  }




}
