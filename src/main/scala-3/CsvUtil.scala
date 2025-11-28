import com.github.tototoshi.csv.CSVReader

abstract class CsvUtil[T]:
    def readCsv(filename: String): List[T] =
      // Open stream
      val reader = CSVReader.open(filename)

      var dataList: List[T] = List.empty
      try {
        // Use map in parallel to enhance performance
        dataList = reader.toLazyList().tail.map(data => {
          // Parse and converting data to a specific class
          parseCsvData(data)
        }).toList
      } finally {
        // Close stream for both success or error
        reader.close()
      }

      dataList

    def parseCsvData(data: List[String]): T
