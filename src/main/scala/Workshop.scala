import scala.collection.parallel.CollectionConverters._

object Workshop {

  // =========================================================================
  // ЗАВДАННЯ 2: РЕФАКТОРИНГ В ЧИСТЕ ФП (EXPRESSION-ORIENTED)
  // =========================================================================

  sealed trait RiskLevel
  case object HighRisk extends RiskLevel
  case object MediumRisk extends RiskLevel
  case object LowRisk extends RiskLevel

  def categorize(amount: Double): RiskLevel =
    if amount > 80.0 then HighRisk
    else if amount > 50.0 then MediumRisk
    else LowRisk

  def getMultiplier(level: RiskLevel): Double =
    level match {
      case HighRisk => 1.5
      case MediumRisk => 1.2
      case LowRisk => 1.0
    }

  @main def runWorkshop(): Unit = {
    println("=== Практика 00: Вмикаємо мозок ===")

    val data: Vector[Double] = (1 to 100).toVector.map(_.toDouble)

    // =========================================================================
    // ЗАВДАННЯ 1: ПАСТКА НА RACE CONDITION
    // =========================================================================
    var totalRisk = 0.0

    data.par.foreach { transactionId =>
      val risk = math.sin(transactionId) * math.cos(transactionId) + math.tan(transactionId % 1.0)
      totalRisk += risk
    }

    println(s"Сумарний ризик (через var): $totalRisk")

    // =========================================================================
    // 4. ЧИСТИЙ КОНВЕЄР ОБЧИСЛЕНЬ
    // =========================================================================
    val finalRiskSum = data
      .filter(amount => amount > 50.0)
      .map(amount => amount * getMultiplier(categorize(amount)))
      .sum

    println(s"Фінальний сумарний ризик (чистий конвеєр): $finalRiskSum")
  }

  def main(args: Array[String]): Unit = runWorkshop()
}