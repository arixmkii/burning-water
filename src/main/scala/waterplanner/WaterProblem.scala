package waterplanner

import org.optaplanner.core.api.domain.entity.PlanningEntity
import org.optaplanner.core.api.domain.solution.{PlanningEntityCollectionProperty, PlanningScore, PlanningSolution}
import org.optaplanner.core.api.domain.solution.ProblemFactCollectionProperty
import org.optaplanner.core.api.domain.valuerange.{CountableValueRange, ValueRangeFactory, ValueRangeProvider}
import org.optaplanner.core.api.domain.variable.PlanningVariable
import org.optaplanner.core.api.score.buildin.hardsoft.HardSoftScore

import scala.jdk.CollectionConverters._

class WaterContainer(val name: String,
                     val capacity: Double,
                     var isGrey: Boolean) {
  def this() = this(null, 0, true)
}

class WaterBarrel(val id: Integer) extends WaterContainer(s"barrel $id", 55, false)
class GreywaterBarrel(val id: Integer) extends WaterContainer(s"grey barrel $id", 55, true)
class Boxes(c: Double) extends WaterContainer("boxwater", c, false)
class RVContainer(n: String, c: Int, g: Boolean) extends WaterContainer(n, c, g)
class RVWater() extends RVContainer("rv water", 55, false)
class RVGreyWater(nTanks: Int) extends RVContainer(s"rv grey", 28*nTanks, true)
class RVBlackWater() extends RVContainer("rv black", 21, true)

@PlanningEntity
class WaterUseDay(val day: Int, val waterUse: Double, val greyWater: Double,
                  _source: WaterContainer, _dest: WaterContainer) {

  @PlanningVariable(valueRangeProviderRefs = Array("nshowers"))
  var showers: Integer = 0
  @PlanningVariable(valueRangeProviderRefs = Array("containers"))
  var dest: WaterContainer = _dest
  @PlanningVariable(valueRangeProviderRefs = Array("containers"))
  var source: WaterContainer = _source

  def this() = this(0, 0, 0, null, null)
}

object WaterUseDay {
  def fromPeople(day: Int, n: Int) = new WaterUseDay(day, n*1.5, n*0.75, null, null)
}

@PlanningSolution
class WaterProblem(val showerTarget: Int,
                   private val _containers: List[WaterContainer],
                   private val _grains: List[WaterUseDay]) {
  def this() = this(0, null, null)

  @ValueRangeProvider(id = "containers") @ProblemFactCollectionProperty
  val containers: java.util.List[WaterContainer] = _containers.asJava
  @PlanningEntityCollectionProperty
  val usageGrains: java.util.List[WaterUseDay] = _grains.asJava

  @PlanningScore
  var score: HardSoftScore = _

  @ValueRangeProvider(id = "nshowers")
  val showerCapacity: CountableValueRange[Integer] = { ValueRangeFactory.createIntValueRange(0, 3)}

  // To keep the solution neat, I've put the messy print code into a separate function.
  override def toString: String = Utils.problemToString(this)
}