package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje


import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.HttpClients
import org.apache.http.util.EntityUtils
import java.io.{FileOutputStream, InputStream}
import java.util.Base64

object HTTPDownload {
  def main(args: Array[String])  = {
    val d = HTTPDownload(CobaltExport.testURL,"/tmp/zippie.zip", "jesse", "dedoes")
    d.apply()
  }
}
case class HTTPDownload(url: String, outputPath: String, username: String, password: String) {

  // Encode credentials for basic auth
  val encoding = Base64.getEncoder.encodeToString(s"$username:$password".getBytes("UTF-8"))

  // Create a CloseableHttpClient instance
  val httpClient = HttpClients.createDefault()

  var error = false
  def apply(): Boolean = {

    try {
      // Prepare a GET request
      val httpGet = new HttpGet(url)
      httpGet.setHeader("Authorization", s"Basic $encoding") // Set the authorization header

      // Execute the GET request
      val response = httpClient.execute(httpGet)
      val entity = response.getEntity

      if (entity != null) {
        val inputStream: InputStream = entity.getContent
        val outputStream = new FileOutputStream(outputPath)

        try {
          // Buffer for reading data
          val buffer = new Array[Byte](1024)
          var bytesRead = inputStream.read(buffer)

          // Read from input stream and write to output stream
          while (bytesRead != -1) {
            outputStream.write(buffer, 0, bytesRead)
            bytesRead = inputStream.read(buffer)
          }
          // println(s"File downloaded to $outputPath")
        } catch {
          case e: Exception =>
            e.printStackTrace()
            Console.err.println(s"Exception in download from $url")
            error = true
        } finally {
          inputStream.close()
          outputStream.close()
        }
      }
    }  catch {
      case e:Exception =>e.printStackTrace()
         Console.err.println(s"What the... $url")
         error = true
    }
    finally {
      httpClient.close()
    }
    !error
  }
}
