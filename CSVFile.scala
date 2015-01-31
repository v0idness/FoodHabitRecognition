import com.opencsv.CSVReader
import java.io.{FileInputStream, InputStreamReader}

class CSVFile(fileName: String, charset: String = "UTF-8") extends Traversable[Array[String]] {

  override def foreach[U](f: Array[String] => U): Unit = {
    val csvReader = new CSVReader(new InputStreamReader(new FileInputStream(fileName), charset))  
    try {
      var next = true
      while (next) {
        val values = csvReader.readNext
        if (values != null) f(values)
        else next = false
      }
    } finally {
      csvReader.close()
    }
  }
}