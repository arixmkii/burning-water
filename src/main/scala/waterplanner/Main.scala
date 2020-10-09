package waterplanner

//import org.optaplanner.core.api.score.stream.ConstraintStreamImplType
import org.optaplanner.core.api.solver.{Solver, SolverFactory}
import org.optaplanner.core.config.score.director.ScoreDirectorFactoryConfig
import org.optaplanner.core.config.solver.SolverConfig
import org.optaplanner.core.config.solver.termination.TerminationConfig

/**
  * Created by richardweiss on 5/8/17.
  */
object Main extends App {
  def makeProblem(): WaterProblem = {
    val containers = List(
      new WaterBarrel(1),
      new WaterBarrel(2),
      new Boxes(12 * 2 + 5 * 3),
      new GreywaterBarrel(1),
      new GreywaterBarrel(2),
      new RVBlackWater(),
      new RVGreyWater(2),
      new RVWater()
    )
    val useGrains = List(
      WaterUseDay.fromPeople(1,  5),
      WaterUseDay.fromPeople(2,  5),
      WaterUseDay.fromPeople(3,  12),
      WaterUseDay.fromPeople(4,  12),
      WaterUseDay.fromPeople(5,  12),
      WaterUseDay.fromPeople(6,  12),
      WaterUseDay.fromPeople(7,  12),
      WaterUseDay.fromPeople(8,  12),
      WaterUseDay.fromPeople(9,  12),
      WaterUseDay.fromPeople(10, 12),
      WaterUseDay.fromPeople(11, 12)
    )
    new WaterProblem(5, containers, useGrains)
  }

  def solveNew(problem: WaterProblem): WaterProblem = {
    val terminationConfig = new TerminationConfig()
      .withSecondsSpentLimit(30)
    val scoreDirectorFactoryConfig = new ScoreDirectorFactoryConfig()
      .withConstraintProviderClass(classOf[WaterSolutionConstraintProvider])
//      .withConstraintStreamImplType(ConstraintStreamImplType.BAVET) // BAVET doesn't work with BigDecimals
    val solverConfig = new SolverConfig()
      .withTerminationConfig(terminationConfig)
      .withScoreDirectorFactory(scoreDirectorFactoryConfig)
      .withEntityClasses(classOf[WaterUseDay])
      .withSolutionClass(classOf[WaterProblem])
    val solverFactory: SolverFactory[WaterProblem] = SolverFactory.create(solverConfig)
    val solver: Solver[WaterProblem] = solverFactory.buildSolver
    val solvedWaterProblem = solver.solve(problem)
    solvedWaterProblem
  }

  def solveOriginal(problem: WaterProblem): WaterProblem = {
    val configStream = Class.forName("waterplanner.Main").getResourceAsStream("/waterplanner/solverconfig.xml")
    assert(configStream != null)
    val solverFactory: SolverFactory[WaterProblem] = SolverFactory.create(SolverConfig.createFromXmlInputStream(configStream))
    val solver: Solver[WaterProblem] = solverFactory.buildSolver
    val solvedWaterProblem = solver.solve(problem)
    solvedWaterProblem
  }

  val problem1 = makeProblem()
  val problem2 = makeProblem()

  val solvedWaterProblem1 = solveNew(problem1)
  val solvedWaterProblem2 = solveOriginal(problem2)

  print(problem1)
  println("----------")
  print(solvedWaterProblem1)
  println(">>>>>>>>>>")
  println(solvedWaterProblem1.score)
  println("==========")
  print(solvedWaterProblem2)
  println(">>>>>>>>>>")
  println(solvedWaterProblem2.score)

  assert(solvedWaterProblem1.score == solvedWaterProblem2.score)
}
