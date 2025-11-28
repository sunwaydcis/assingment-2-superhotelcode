import java.time.{LocalDate, LocalTime}
import java.time.format.DateTimeFormatter

object DateUtil:
  private val FORMAT_MDYYYY = "M/d/yyyy"
  private val FORMAT_HMS = "H:m:s"

  def parseDate(d: String): LocalDate =
    LocalDate.parse(d, DateTimeFormatter.ofPattern(FORMAT_MDYYYY))

  def parseTime(t: String): LocalTime =
    // Avoid error when parsing if having AM or PM in value
    LocalTime.parse(t.replace(" AM", "").replace(" PM", ""), DateTimeFormatter.ofPattern(FORMAT_HMS))
