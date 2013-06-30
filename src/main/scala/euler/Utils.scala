package euler

object Utils {
  def resource(fileName: String) =
    io.Source.fromURL(getClass.getResource("/" + fileName))

  def withResource[T](fileName: String)(block: io.Source => T) = {
    val source = resource(fileName)
    try {
      block(source)
    } finally {
      source.close()
    }
  }

}