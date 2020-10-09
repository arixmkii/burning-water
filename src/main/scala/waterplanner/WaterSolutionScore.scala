package waterplanner

import org.optaplanner.core.api.score.buildin.hardsoft.HardSoftScore
import org.optaplanner.core.impl.score.director.easy.EasyScoreCalculator

import scala.collection.mutable
import scala.jdk.CollectionConverters._

// Old scorer
class WaterSolutionScore extends EasyScoreCalculator[WaterProblem] {

  // done
  def forceRV(waterProblem: WaterProblem): HardSoftScore = {
    /*
    The EA team will only have the RV, so every grain's source or destination
    must either be the boxes or the RV.
     */
    var hardScore = 0
    for (grain <- waterProblem.usageGrains.asScala) {
      if (grain.day < 3) {
        if (!(grain.source.isInstanceOf[RVContainer] || grain.source.isInstanceOf[Boxes])) hardScore += 1
        if (!grain.dest.isInstanceOf[RVContainer]) hardScore += 1
      }
    }
    HardSoftScore.of(-hardScore, 0)
  }

  // done
  def checkGrainExists(waterProblem: WaterProblem): HardSoftScore = {
    /*
    Check every water usage grain is assigned somewhere, and ensure we
    don't put fresh water into a grey water barrel.
     */
    var hardScore = 0
    for (grain <- waterProblem.usageGrains.asScala) {
      if (grain.source == null) {
        hardScore += 1
      } else {
        if (grain.source.isGrey) hardScore += 1
      }

      if (grain.dest == null) {
        hardScore += 1
      } else {
        if (!grain.dest.isGrey) hardScore += 1
      }
    }
    HardSoftScore.of(-hardScore, 0)
  }

  // done
  def verifyCapacity(waterProblem: WaterProblem): HardSoftScore = {
    /*
    check the containers have space! Super duper important!

    1. Make a mapping, container -> capacity
    2. Over each grain, subtract water and grey water use
    3. Find the RV fresh water and the RV grey water tank
    4. Over each grain, find showers, and appropriately handle their
       3GA of water use and grey water production.
    5. We add one soft constraint, to discourage filling the water
       barrels too high. They become hard to manage.
     */
    val containerCapacities = mutable.HashMap(waterProblem.containers.asScala.toSeq.map(c => c -> c.capacity): _*)
    for (grain <- waterProblem.usageGrains.asScala) {
      // Handle direct source/dest
      if (grain.source != null) containerCapacities(grain.source) = containerCapacities(grain.source) - grain.waterUse
      if (grain.dest != null) containerCapacities(grain.dest) = containerCapacities(grain.dest) - grain.greyWater
    }
    val rvFresh = waterProblem.containers.asScala.find(_.isInstanceOf[RVWater]).get
    val rvGrey = waterProblem.containers.asScala.find(_.isInstanceOf[RVGreyWater]).get
    for (grain <- waterProblem.usageGrains.asScala) {
      containerCapacities(rvFresh) = containerCapacities(rvFresh) - grain.showers * 3
      containerCapacities(rvGrey) = containerCapacities(rvGrey) - grain.showers * 3
    }

    val problems = containerCapacities.values.count(_ < 0)
    HardSoftScore.of(-problems, 0)
  }

  // done
  def neatness(waterProblem: WaterProblem): HardSoftScore = {
    /*
    Discourage changes. I want neat solutions.
    This is a soft score, because we'll take a messy solution if one is
    needed.
     */
    var softScore = 0
    for ((a, b) <- waterProblem.usageGrains.asScala.zip(waterProblem.usageGrains.asScala.tail)) {
      if (a.source != b.source) softScore += 1
      if (a.dest != b.dest) softScore += 1
    }
    HardSoftScore.of(0, -softScore)
  }

  // done
  def ensureNoverlap(waterProblem: WaterProblem): HardSoftScore = {
    /*
    All the barrels have IDs. No fresh water barrel may be used as grey water,
    and then as fresh water again.

    Basically, this represents taking an (ideally empty) barrel, and using it for
    grey water after emptying the fresh onto the playa in a responsible way. I'd
    recommend getting drunk and rolling the barrel up and down the street with the
    cap off.
     */
    val waterBarrelUses = waterProblem.usageGrains.asScala.flatMap{ grain =>
      // For any grain with non-null source, find the ones using water barrels
      // and then make an ID -> Day list
      Option(grain.source).collect{
        case b: WaterBarrel => b.id -> grain.day
      }
    }
    val greyWaterBarrelUses = waterProblem.usageGrains.asScala.flatMap{ grain =>
      Option(grain.dest).collect{
        case b: GreywaterBarrel => b.id -> grain.day
      }
    }

    // Escape early if there can be no overlap because no
    // barrel is used for water or grey water
    if (waterBarrelUses.isEmpty || greyWaterBarrelUses.isEmpty)
      return HardSoftScore.ZERO

    val barrelLastFresh = waterBarrelUses.groupBy(_._1).view.mapValues(v => v.maxBy(_._2)._2)
    val barrelFirstGrey = greyWaterBarrelUses.groupBy(_._1).view.mapValues(v => v.minBy(_._2)._2)

    val scores = for {
      (barrel, lastFresh) <- barrelLastFresh
    } yield {
      barrelFirstGrey.get(barrel) match {
        case None => 0
        case Some(v) if v <= lastFresh => 1
        case _ => 0
      }
    }

    HardSoftScore.of(-scores.sum, 0)
  }

  // done
  def discourageBarrelGreyWater(problem: WaterProblem): HardSoftScore = {
    val dests = problem.usageGrains.asScala.flatMap(v => Option(v.dest))
    val barrelDests = dests.filter(_.isInstanceOf[GreywaterBarrel]).distinct
    HardSoftScore.of(0, -barrelDests.length * 100)
  }

  // done
  def encourageShowers(problem: WaterProblem): HardSoftScore = {
    // Not negative! We _want_ showers.
    HardSoftScore.of(0, problem.usageGrains.asScala.map {
      // Less value at the start
      case g if g.day < 4 => g.showers * 3
      case g if g.day < 10 => g.showers * 8
      // Less value at the end, about to go home...
      case g => g.showers * 4
    }.sum)
  }

  def calculateScore(problem: WaterProblem): HardSoftScore = {
    HardSoftScore.ZERO
      .add(forceRV(problem))
      .add(checkGrainExists(problem))
      .add(verifyCapacity(problem))
      .add(neatness(problem))
      .add(ensureNoverlap(problem))
      .add(discourageBarrelGreyWater(problem))
      .add(encourageShowers(problem))
  }

  def scoreString(problem: WaterProblem): String = {
    "Greybarrels:" + discourageBarrelGreyWater(problem).toString + "\n" +
    "Graincheck: " + checkGrainExists(problem).toString + "\n" +
    "Capacity:   " + verifyCapacity(problem).toString + "\n" +
    "neatness:   " + neatness(problem).toString + "\n" +
    "Noverlap:   " + ensureNoverlap(problem).toString + "\n" +
    "Showers:    " + encourageShowers(problem).toString + "\n" +
    "forceRV:    " + forceRV(problem).toString + "\n" +
    "total:      " + calculateScore(problem).toString
  }
}
