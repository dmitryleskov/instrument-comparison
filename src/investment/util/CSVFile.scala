package investment.util

import java.io.{FileInputStream, InputStreamReader}

import au.com.bytecode.opencsv._

class CSVFile(fileName: String,
              charset: String = "UTF-8",
              separator: Char = ',',
              quote: Char = '"',
              escape: Char = '\'') extends Traversable[Array[String]] {
  override def foreach[U](f: Array[String] => U): Unit = {
    val csvReader = new CSVReader(new InputStreamReader(new FileInputStream(fileName), charset), separator, quote, escape, 1)
    try {
      var next = true
      while (next) {
        val values = csvReader.readNext()
        if (values != null) f(values)
        else next = false
      }
    } finally {
      csvReader.close()
    }
  }
}