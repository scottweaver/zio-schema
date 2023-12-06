package zio.schema.codec

import zio.schema._
import zio.json._
import zio.json.ast._
import zio.json.yaml._
import OpenAPISchema._

object OpenAPISchemaCodec {

//  def decode(openapi: String): Either[String, Schema[_]] = {
    def decode(openapi: String) = {

     openapi.fromYaml[OpenAPI]
//    println(jsonAst)
//
//    ???


  }

}
