import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime

object Gigasecond
{

  private def giga:Long = 1000000000

  def add(startDate: LocalDate): LocalDateTime = LocalDateTime.of(startDate, LocalTime.MIDNIGHT).plusSeconds(giga)

  def add(startDateTime: LocalDateTime): LocalDateTime = startDateTime.plusSeconds(giga)
}
