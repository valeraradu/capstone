package observatory.util

import observatory.Location

trait DistanceCalcular {
  def calculateDistanceInKilometer(location1: Location, location2: Location): Int
}

object DistanceCalculatorImpl extends DistanceCalcular {
  private val AVERAGE_RADIUS_OF_EARTH_KM = 6371
  override def calculateDistanceInKilometer(location1: Location, location2: Location): Int = {
    val latDistance = Math.toRadians(location1.lat - location2.lat)
    val lngDistance = Math.toRadians(location1.lon - location2.lon)
    val sinLat = Math.sin(latDistance / 2)
    val sinLng = Math.sin(lngDistance / 2)
    val a = sinLat * sinLat +
      (Math.cos(Math.toRadians(location1.lat)) * Math.cos(Math.toRadians(location2.lat)) * sinLng * sinLng)
    val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))
    (AVERAGE_RADIUS_OF_EARTH_KM * c).toInt
  }

  //https://stackoverflow.com/questions/14329691/convert-latitude-longitude-point-to-a-pixels-x-y-on-mercator-projection
  //https://github.com/mraad/WebMercator/blob/master/src/main/java/com/esri/WebMercator.java
  /*
      Pseudo code example, so this can be adapted to every programming language.

      latitude    = 41.145556; // (φ)
      longitude   = -73.995;   // (λ)

      mapWidth    = 200;
      mapHeight   = 100;

      // get x value
      x = (longitude+180)*(mapWidth/360)

      // convert from degrees to radians
      latRad = latitude*PI/180;

      // get y value
      mercN = ln(tan((PI/4)+(latRad/2)));
      y     = (mapHeight/2)-(mapWidth*mercN/(2*PI));
  */
  def geoToCoord(location: Location): (Int, Int) = {
    val mapWidth    = 360;
    val mapHeight   = 180;

    val x = (location.lon + 180)*(mapWidth/360)

    // convert from degrees to radians
    //val latRad = location.lat * Math.PI/180;

    // get y value
    //val mercN = Math.log(Math.tan((Math.PI/4)+(latRad/2)))
    //val y     = (mapHeight/2)-(mapHeight*mercN/(2*Math.PI));

    //https://github.com/mfeldheim/hermap/blob/master/src/Geo/Projection.php
    //'x' => ($lng+180)*($width/360),
    //'y' => ($height/2)-($width*log(tan((M_PI/4)+(($lat*M_PI/180)/2)))/(2*M_PI))
    //val y = (mapHeight/2)-(mapWidth*Math.log(Math.tan((Math.PI/4)+((location.lat*Math.PI/180)/2)))/(2*Math.PI))
    val y  = mapHeight/2 -((location.lat * mapHeight) / 180)
    (x.toInt, y.toInt)
  }

  def coordToGeo(x: Int, y: Int): Location = {
    val mapWidth    = 360;
    val mapHeight   = 180;
    val lon = x/(mapWidth/360)-180
    val lat = -((y - mapHeight/2)*180)/mapHeight
    Location(lat, lon)

    /*val n = math.pow(2.0, 0)
    val lon_deg = x / n * 360.0 - 180.0
    val lat_rad = math.atan(math.sinh(math.Pi * (1 - 2 * y / n)))
    val lat_deg = math.toDegrees(lat_rad)

    Location(lat_deg, lon_deg)*/
  }
}