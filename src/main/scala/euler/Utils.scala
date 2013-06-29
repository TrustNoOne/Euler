package euler

object Utils {
  def resource(fileName: String) =
    io.Source.fromURL(getClass.getResource("/" + fileName))

  def withResource(fileName: String)(block: io.Source => Any) = {
    val source = resource(fileName)
    try {
      block(source)
    } finally {
      source.close()
    }
  }

}