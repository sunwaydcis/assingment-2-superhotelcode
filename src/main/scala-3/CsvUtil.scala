import com.github.tototoshi.csv.CSVReader

abstract class CsvUtil:
  // Used for closing stream after transforming data to a specific class
  private var reader: CSVReader = _
  // Used for parsing and converting data type
  def readCsv(filename: String): LazyList[List[String]] =
    // Open stream
    reader = CSVReader.open(filename)
    // Use LazyList to reduce memory and only compute data if needed
    reader.toLazyList().drop(1)

  // Parse and convert data to a specific class
  def parseCsvData(dataList: LazyList[List[String]]): Unit
  
  def closeStream(): Unit =
    reader.close()
