// Group assignment - Hotel Booking Analysis
object Assignment2:
  def main(args: Array[String]): Unit =
    val analysis = new HotelBookingAnalysis

    // Read csv and load data
    val FILE_NAME = "Hotel_Dataset.csv"
    val dataList: LazyList[List[String]] = analysis.readCsv(
      getClass.getResource("/data/" + FILE_NAME).getPath
    )

    // Check empty data
    if dataList.isEmpty then
      println("No data for analysis")
    else
      // Parse and converting data to a specific class
      analysis.parseCsvData(dataList)

      // Close stream after transforming data
      analysis.closeStream()

      // Print out analysis information
      println("=============== Hotel Booking Analysis ====================\n")

      // Get country has the highest number of booking
      val countryHighestNumberOfBooking = analysis.getCountryHighestNumberOfBooking
      analysis.showAnalysis(s"1. Country has the highest number of booking\nCountry: ${
        countryHighestNumberOfBooking._1
      }\nNumber of booking: ${
        countryHighestNumberOfBooking._2
      }\n")

      println("===========================================================")
    end if
