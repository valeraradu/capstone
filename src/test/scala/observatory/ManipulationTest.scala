package observatory

import org.junit.Test

trait ManipulationTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data manipulation", 4) _

  // Implement tests for methods of the `Manipulation` object

 /*   [Test Description] average must return a grid whose predicted temperatures are the average of the known temperatures (4pts)
    [Observed Error] Invalid predicted temperature at (90, -180): 23.599904068111883. Expected: 9.499760170279702.

  @Test def `average must return a grid whose predicted temperatures are the average of the known temperatures`: Unit = {
    val average = Manipulation.average()
    assert(biinterpolated == 1.52587890625E-7, s"biinterpolated expected 1.52587890625E-7 but was ${biinterpolated}")
  }*/

}
