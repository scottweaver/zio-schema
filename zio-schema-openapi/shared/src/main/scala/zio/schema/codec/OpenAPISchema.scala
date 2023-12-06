package zio.schema.codec

import zio._
import zio.json.ast._
import zio.json.{DeriveJsonDecoder, _}

private[codec] sealed trait OpenAPISchema

object OpenAPISchema {

  //  final case class Root(components: Components) extends OpenAPISchema
  final case class OpenAPI(openapi: String, components: Components, paths: Json) extends OpenAPISchema

  //  final case class Components(schemas: Json) extends OpenAPISchema
  final case class Components(schemas: Schemas) extends OpenAPISchema

  //  final case class Schemas(schemas: Chunk[Json]) extends OpenAPISchema
  final case class Schemas() extends OpenAPISchema

  implicit val OpenAPIDecoder: JsonDecoder[OpenAPI] = DeriveJsonDecoder.gen[OpenAPI]
  implicit val ComponentsDecoder: JsonDecoder[Components] = JsonDecoder[Json].map(_ => Components(Schemas()))

  //DeriveJsonDecoder.gen[Components]

  implicit val SchemaDecoder: JsonDecoder[Schemas] =
    JsonDecoder[Json].map { json =>
      //      Schemas(json.asArray.getOrElse(Chunk.empty))
      println(json)
      Schemas()
    }

}


