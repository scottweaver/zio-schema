package zio.schema.codec

import java.nio.file._
import io.swagger.parser.OpenAPIParser
object Quick {

  def main(args: Array[String]): Unit = {

    val raw      = Files.readString(Paths.get(getClass.getResource("/maps.yaml").toURI))
    val petstore = Files.readString(Paths.get(getClass.getResource("/petstore.yaml").toURI))

    val out    = new OpenAPIParser().readContents(raw, null, null)
    val petout = new OpenAPIParser().readContents(petstore, null, null)

    val openAPI    = out.getOpenAPI
    val petOpenAPI = petout.getOpenAPI

    println(OpenAPISchemaDecoder(petOpenAPI).decode("NewPet"))
    println(OpenAPISchemaDecoder(petOpenAPI).decode("Pet"))

  }

}
