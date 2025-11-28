// Group assignment - Hotel Booking Analysis
object Assignment2:
  def main(args: Array[String]): Unit =
    val analysis = new HotelBookingAnalysis

    // Read csv and load data
    val FILE_NAME = "Hotel_Dataset.csv"
    val dataList: List[Booking] = analysis.readCsv(
      getClass.getResource("/data/" + FILE_NAME).getPath
    )

    // Check empty data
    if dataList.isEmpty then
      println("No data for analysis")
    else
      // Print out analysis information
      println("=============== Hotel Booking Analysis ====================\n")

      // Get country has the highest number of booking
      analysis.showAnalysis(analysis.getHighestBookingCountry)

      // Get hotel which offers the most economical options
      // for Booking Price, Discount, Profit Margin
      analysis.showAnalysis(analysis.getMostEconomistHotel)

      // Get most profitable hotel when considering the number of visitor and profit margin
      analysis.showAnalysis(analysis.getMostProfitableHotel)
      
      println("===========================================================")
    end if
