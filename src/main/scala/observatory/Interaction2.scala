package observatory

import observatory.LayerName.{Deviations, Temperatures}

/**
  * 6th (and last) milestone: user interface polishing
  */
object Interaction2 extends Interaction2Interface {

  /**
    * @return The available layers of the application
    *
    *         Temperature (°C)	Red	Green	Blue
    *         7	                0	  0	    0
    *         4	                255	0	    0
    *         2	                255	255	  0
    *         0	                255	255	255
    *         -2	              0	  255	255
    *         -7	              0	  0	  255
    */
  def availableLayers: Seq[Layer] = {
    val colorScheme = Seq((7.0, Color(0,0,0)),
                          (4.0, Color(255,0,0)),
                          (2.0, Color(255,255,0)),
                          (0.0, Color(255,255,255)),
                          (-2.0, Color(0,255,255)),
                          (-7.0, Color(0,0,255))
    )
    Seq(Layer(Temperatures, colorScheme, Range(1975, 2016)),
      Layer(Deviations, colorScheme, Range(1975, 2016)))
  }

  /**
    * @param selectedLayer A signal carrying the layer selected by the user
    * @return A signal containing the year bounds corresponding to the selected layer
    */
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range] = {
    Signal(selectedLayer().bounds)
  }

  /**
    * @param selectedLayer The selected layer
    * @param sliderValue The value of the year slider
    * @return The value of the selected year, so that it never goes out of the layer bounds.
    *         If the value of `sliderValue` is out of the `selectedLayer` bounds,
    *         this method should return the closest value that is included
    *         in the `selectedLayer` bounds.
    */
  def yearSelection(selectedLayer: Signal[Layer], sliderValue: Signal[Year]): Signal[Year] = {
    Signal(if (sliderValue() >= selectedLayer().bounds.max)
              selectedLayer().bounds.max
           else if (sliderValue() <= selectedLayer().bounds.min)
              selectedLayer().bounds.min
           else
              sliderValue()
    )

  }

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The URL pattern to retrieve tiles
    */
  def layerUrlPattern(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String] = {
    Signal("target/" + selectedLayer().layerName.id + "/" + selectedYear() + "/{z}/{x}-{y}.png")
  }

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The caption to show
    *         {z}/{x}/{y}.png
    */
  def caption(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String] = {
    Signal(selectedLayer().layerName + " (" + selectedYear() + ")")
  }

}

// Interface used by the grading infrastructure. Do not change signatures
// or your submission will fail with a NoSuchMethodError.
trait Interaction2Interface {
  def availableLayers: Seq[Layer]
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range]
  def yearSelection(selectedLayer: Signal[Layer], sliderValue: Signal[Year]): Signal[Year]
  def layerUrlPattern(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String]
  def caption(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String]
}

sealed abstract class LayerName(val id: String)
object LayerName {
  case object Temperatures extends LayerName("temperatures")
  case object Deviations extends LayerName("deviations")
}

/**
  * @param layerName Name of the layer
  * @param colorScale Color scale used by the layer
  * @param bounds Minimum and maximum year supported by the layer
  */
case class Layer(layerName: LayerName, colorScale: Seq[(Temperature, Color)], bounds: Range)

