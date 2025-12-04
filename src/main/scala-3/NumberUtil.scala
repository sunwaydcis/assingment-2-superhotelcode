object NumberUtil:
  def formatFloatValue(value: Float, delimiter: Int = 3): Float =
    var d = 1
    for (i <- 1 to delimiter) {
      d *= 10
    }
    // e.g. delimiter = 2 -> 123.23, 3 -> 123.235
    Math.round(value * d) / d.toFloat

  def convertNumberToPercentage(value: Float): Float =
    value * 100
