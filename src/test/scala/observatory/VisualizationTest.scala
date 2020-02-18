package observatory

import observatory.util.DistanceCalculatorImpl
import org.junit.Assert._
import org.junit.{Rule, Test}

import scala.util.Random

trait VisualizationTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("raw data display", 2) _

  // Implement tests for the methods of the `Visualization` object

  @Test def `test predictTemperature for the same two locations should predict average`: Unit = {
      assert(Visualization.predictTemperature(List((Location(45,45), 10.0), (Location(45,45), 0.0)), Location(0,0)) == 5.0)
  }


  @Test def `test predictTemperature for two locations where one is exact match should predict exact match`: Unit = {
    assert(Visualization.predictTemperature(List((Location(0,0), 10.0), (Location(45,45), 0.0)), Location(0,0)) == 10.0)
  }


  @Test def `test interpolateColor for two equidistant points should be average`: Unit = {
    val color = Visualization.interpolateColor(List((10, Color(100, 100, 100)), (20, Color(50, 50, 50))), 15.0)
    assert(
      color == Color(75, 75, 75)
    )
  }

  @Test def `test interpolateColor for two points where one is exact match should return match`: Unit = {
    val color = Visualization.interpolateColor(List((10, Color(100, 100, 100)), (20, Color(50, 50, 50))), 10.0)
    assert(
      color == Color(100, 100, 100)
    )
  }

  @Test def `exceeding the greatest value of a color scale should return the color associated with the greatest value (2pts)`: Unit = {
    val color = Visualization.interpolateColor(List((10, Color(100, 100, 100)), (20, Color(50, 50, 50))), 30.0)
    assert(color == Color(50, 50, 50), s"expected Color(50, 50, 50) but it was $color")
  }

  @Test def `predicted color: Color(63,0,191). Expected: Color(191,0,64) (scale = List((-1.0,Color(255,0,0)), (0.0,Color(0,0,255))), value = -0.75)`:
  Unit = {
    val color = Visualization.interpolateColor(List((-1.0,Color(255,0,0)), (0.0,Color(0,0,255))), -0.75)
    assert(color == Color(191,0,64), s"expected Color(191,0,64) but it was $color")
  }


  @Test def `test visualize produces an image of 360×180 pixels`: Unit = {
    val image = Visualization.visualize(List(), List())
    assert(image.height == 180)
    assert(image.width == 360)
  }

  @Test def `test visualize produces an image of same color if input is one location`: Unit = {
    val locations = List((Location(45,45), 10.0))
    val temperatures = List((10.0, Color(100, 100, 100)))
    val image = Visualization.visualize(locations, temperatures)

    assert(image.pixel(0, 0) == image.pixel(359, 179))
    assert(image.pixel(359, 0) == image.pixel(359, 179))
    assert(image.pixel(0, 179) == image.pixel(359, 179))

    assert(image.pixel(Random.nextInt(359), Random.nextInt(179)) == image.pixel(359, 179))
  }

  @Test def `test coordToGeo x:0 y:0 is Location(90, -180)`: Unit = {
    assert(DistanceCalculatorImpl.coordToGeo(0, 0) == Location(90, -180))
  }

  @Test def `test geoToCoord Location(90, -180) is x:90, y:180`: Unit = {
    assert(DistanceCalculatorImpl.geoToCoord(Location(90, -180)) == (0, 0))
  }

  @Test def `test geoToCoord Location((-89, 179)) is x:359, y:179`: Unit = {
    assert(DistanceCalculatorImpl.geoToCoord(Location(-89, 179)) == (359, 179))
  }

  @Test def `test coordToGeo to geoToCoord and back is equivalent`: Unit = {
    val x =  Random.nextInt(360)
    val y =  Random.nextInt(180)
    val geo = DistanceCalculatorImpl.coordToGeo(x, y)
    assert(DistanceCalculatorImpl.geoToCoord(geo) == (x, y))
  }



}
