package ru.dru

import zio.{Duration, Scope, ZIO, ZIOAppArgs, ZIOAppDefault, durationLong}

import java.time.LocalDateTime

case class SaladInfoTime(tomatoTime: Duration, cucumberTime: Duration)


object Breakfast extends ZIOAppDefault {

  /**
   * Функция должна эмулировать приготовление завтрака. Продолжительные операции необходимо эмулировать через ZIO.sleep.
   * Правила приготовления следующие:
   *  1. Нобходимо вскипятить воду (время кипячения waterBoilingTime)
   *  2. Параллельно с этим нужно жарить яичницу eggsFiringTime
   *  3. Параллельно с этим готовим салат:
   *    * сначала режим  огурцы
   *    * после этого режим помидоры
   *    * после этого добавляем в салат сметану
   *  4. После того, как закипит вода необходимо заварить чай, время заваривания чая teaBrewingTime
   *  5. После того, как всё готово, можно завтракать
   *
   * @param eggsFiringTime время жарки яичницы
   * @param waterBoilingTime время кипячения воды
   * @param saladInfoTime информация о времени для приготовления салата
   * @param teaBrewingTime время заваривания чая
   * @return Мапу с информацией о том, когда завершился очередной этап (eggs, water, saladWithSourCream, tea)
   */
  def makeBreakfast(eggsFiringTime: Duration,
                    waterBoilingTime: Duration,
                    saladInfoTime: SaladInfoTime,
                    teaBrewingTime: Duration): ZIO[Any, Throwable, Map[String, LocalDateTime]] = {
    for {
      start <- ZIO.succeed(LocalDateTime.now())
      eggsTimes <- ZIO.sleep(eggsFiringTime).as(durations(start, eggsFiringTime)).fork
      waterBoiling <- ZIO.sleep(waterBoilingTime).as(durations(start, waterBoilingTime)).fork
      salad <- ZIO.sleep(
          durationLong(saladInfoTime.cucumberTime.getSeconds + saladInfoTime.tomatoTime.getSeconds).seconds
        ).as(durations(start, saladInfoTime.tomatoTime, saladInfoTime.cucumberTime))
      waterTime <- waterBoiling.join
      teaTime <- ZIO.sleep(teaBrewingTime).as(durations(waterTime, teaBrewingTime))
      eggsTime <- eggsTimes.join
    } yield Map (
      "eggs" -> eggsTime,
      "water" -> waterTime,
      "saladWithSourCream" -> salad,
      "tea" -> teaTime
    )
  }

  private def durations(time: LocalDateTime, durations: Duration*): LocalDateTime = {
    durations.foldLeft(time)((date, duration) => date.plusSeconds(duration.getSeconds))
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed(println("Done"))

}
