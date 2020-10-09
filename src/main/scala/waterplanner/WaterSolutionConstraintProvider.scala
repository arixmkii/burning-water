package waterplanner

import org.optaplanner.core.api.score.buildin.hardsoft.HardSoftScore
import org.optaplanner.core.api.score.stream.ConstraintCollectors.{countDistinct, sumBigDecimal}
import org.optaplanner.core.api.score.stream.{Constraint, ConstraintFactory, ConstraintProvider}

// Reimplementation of original rules using Constraint Provider
class WaterSolutionConstraintProvider extends ConstraintProvider {
  override def defineConstraints(constraintFactory: ConstraintFactory): Array[Constraint] =
    Array(
      forceRV1(constraintFactory),
      forceRV2(constraintFactory),
      checkGrainExists1(constraintFactory),
      checkGrainExists2(constraintFactory),
      verifyCapacity(constraintFactory),
      neatness1(constraintFactory),
      neatness2(constraintFactory),
      ensureNoverlap(constraintFactory),
      discourageBarrelGreyWater(constraintFactory),
      encourageShowers(constraintFactory)
    )

  private def forceRV1(constraintFactory: ConstraintFactory): Constraint =
    constraintFactory.
      from(classOf[WaterUseDay])
      .filter(_.day < 3)
      .filter(g => !(g.source.isInstanceOf[RVContainer] || g.source.isInstanceOf[Boxes]))
      .penalize("RV1", HardSoftScore.ONE_HARD)

  private def forceRV2(constraintFactory: ConstraintFactory): Constraint =
    constraintFactory
      .from(classOf[WaterUseDay])
      .filter(_.day < 3)
      .filter(g => !g.dest.isInstanceOf[RVContainer])
      .penalize("RV2", HardSoftScore.ONE_HARD)

  private def discourageBarrelGreyWater(constraintFactory: ConstraintFactory): Constraint =
    constraintFactory
      .from(classOf[WaterUseDay])
      .filter(g => g.dest != null && g.dest.isInstanceOf[GreywaterBarrel])
      .groupBy((g: WaterUseDay) => g.dest, countDistinct((g: WaterUseDay) => g.dest))
      .penalize("barrelGrey", HardSoftScore.of(0, 100), (_, c) => c)

  private def encourageShowers(constraintFactory: ConstraintFactory): Constraint =
    constraintFactory
      .from(classOf[WaterUseDay])
      .reward("showers", HardSoftScore.ONE_SOFT, (gg: WaterUseDay) => gg match {
        case g if g.day < 4 => g.showers * 3
        case g if g.day < 10 => g.showers * 8
        case g => g.showers * 4
      })

  private def checkGrainExists1(constraintFactory: ConstraintFactory): Constraint =
    constraintFactory
      .from(classOf[WaterUseDay])
      .filter(g => g.source == null || g.source.isGrey)
      .penalize("checkGrain1", HardSoftScore.ONE_HARD)

  private def checkGrainExists2(constraintFactory: ConstraintFactory): Constraint =
    constraintFactory
      .from(classOf[WaterUseDay])
      .filter(g => g.dest == null || !g.dest.isGrey)
      .penalize("checkGrain2", HardSoftScore.ONE_HARD)

  private def verifyCapacity(constraintFactory: ConstraintFactory): Constraint =
    constraintFactory
      .from(classOf[WaterContainer])
      .join(classOf[WaterUseDay])
      .groupBy(
        (c: WaterContainer, _: WaterUseDay) => c,
        sumBigDecimal(
          (c: WaterContainer, g: WaterUseDay) =>
            java.math.BigDecimal.valueOf(
              Option(g.source).filter(_ == c).map(_ => g.waterUse).getOrElse(0.0d) +
                Option(g.dest).filter(_ == c).map(_ => g.greyWater).getOrElse(0.0d) +
                Option.when(c.isInstanceOf[RVWater] || c.isInstanceOf[RVGreyWater])(g.showers.toDouble * 3).getOrElse(0.0d))
        ))
      .penalize(
        "verifyCapacity",
        HardSoftScore.ONE_HARD,
        (c, s) => if (BigDecimal.valueOf(c.capacity).compareTo(s) < 0) 1 else 0)

  private def ensureNoverlap(constraintFactory: ConstraintFactory): Constraint =
    constraintFactory
      .from(classOf[WaterUseDay])
      .join(classOf[WaterUseDay])
      .filter((d1, d2) => d1.day < d2.day)
      .join(classOf[WaterContainer])
      .filter((d1, d2, c) => d1.dest == c && d2.source == c)
      .penalize("ensureNoverlap", HardSoftScore.ONE_HARD)

  private def neatness1(constraintFactory: ConstraintFactory): Constraint =
    constraintFactory
      .from(classOf[WaterUseDay])
      .join(classOf[WaterUseDay])
      .filter((d1, d2) => d1.day + 1 == d2.day)
      .filter((d1, d2) => d1.source != d2.source)
      .penalize("neatness1", HardSoftScore.ONE_SOFT)

  private def neatness2(constraintFactory: ConstraintFactory): Constraint =
    constraintFactory
      .from(classOf[WaterUseDay])
      .join(classOf[WaterUseDay])
      .filter((d1, d2) => d1.day + 1 == d2.day)
      .filter((d1, d2) => d1.dest != d2.dest)
      .penalize("neatness2", HardSoftScore.ONE_SOFT)
}
