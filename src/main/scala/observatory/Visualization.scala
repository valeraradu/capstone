package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.util.DistanceCalculatorImpl

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  /*
    def CalcIDWvalue(unknowncell, knowncells):
      weighted_values_sum = 0.0
      sum_of_weights = 0.0
      neg_half_sens = -sensitivity/2.0
      for knowncell in knowncells:
        weight = ((unknowncell.x-knowncell.x)**2 + (unknowncell.y-knowncell.y)**2)**neg_half_sens
        sum_of_weights += weight
        weighted_values_sum += weight * knowncell.value
      return weighted_values_sum / sum_of_weights
  */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {

      def isCloseEnough(loc: Location): Boolean  = {

        val lonDiff = math.abs(loc.lon - location.lon)

        math.abs(loc.lat - location.lat) < 22 && //roughly Latitude: 1 deg = 110.574 km
        (if (lonDiff > 180) 180 - lonDiff%180 else lonDiff)*
          (111.32*math.cos(math.toRadians(location.lat))) < 2500 //Longitude: 1 deg = 111.320*cos(latitude) km
      }

      val temps =
        temperatures
          .filter(f => isCloseEnough(f._1))
          .map(temp => (DistanceCalculatorImpl.calculateDistanceInKilometer(location, temp._1), temp._2))

      val p = 6
      val predicted = temps.find(temp => temp._1 <= 1).getOrElse({
        val (sum, weights) = temps.foldLeft((0.0, 0.0))((agg, temp) => {
          val weight = Math.pow(temp._1, -p)
          (agg._1 + weight, agg._2 + weight * temp._2)
        }
        )
        (0.0, weights/sum)
      })._2

    predicted

  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

    val(lower, upper) = points.foldLeft(
      ((Double.MinValue, Color(-1, -1, -1)),
      (Double.MaxValue, Color(-1, -1, -1)))
    )((minmaxcolors, point) => {
      minmaxcolors match {
        case ((mintemp: Temperature, mincolor: Color),(maxtemp: Temperature, maxcolor: Color)) =>
          (if( point._1 <= value && point._1 >  mintemp ) point else (mintemp, mincolor),
          if( point._1 >= value && point._1 < maxtemp ) point else (maxtemp, maxcolor))
      }

    })

    if(lower == upper) lower._2
    else if (lower._1 == Double.MinValue) upper._2
    else if (upper._1 == Double.MaxValue) lower._2

    else {
      val weightLower = (1.0 * upper._1 - value)/(upper._1 - lower._1)
      val weightUper = (1.0 * value - lower._1)/(upper._1 - lower._1)

      Color(Math.round((lower._2.red * weightLower + upper._2.red * weightUper).toInt),
            Math.round((lower._2.green * weightLower + upper._2.green * weightUper).toInt),
            Math.round(lower._2.blue * weightLower + upper._2.blue * weightUper).toInt)
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {

    val pixels = for {
      y <- 0 to 179
      x <- 0 to 359
    } yield {
      val color = interpolateColor(colors, predictTemperature(temperatures, DistanceCalculatorImpl.coordToGeo(x, y)))
        Pixel(color.red, color.green, color.blue, 127)
    }

    Image(360, 180, pixels.toArray)

  }

  def defXYtoLatLong(x: Int, y: Int): Location = {
    import scala.math._

    Location(toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y.toDouble /0)))),
    x.toDouble / 1* 360.0 - 180.0)
  }

}

