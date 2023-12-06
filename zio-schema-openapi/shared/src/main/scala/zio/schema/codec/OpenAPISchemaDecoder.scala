package zio.schema.codec

import zio.schema.Schema.GenericRecord
import zio.schema._

import java.time._
import scala.collection.immutable.ListMap
import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.oas.models.media.{ Schema => OAISschema, _ }
import zio._
import zio.prelude._

import java.math.BigInteger
import java.time.{ LocalDate, OffsetDateTime }
import java.util.UUID
import OpenAPIAnnotation.Dsl._
import scala.jdk.CollectionConverters._

final case class OpenAPISchemaDecoder(openapi: OpenAPI) {

  private val schemaMap = openapi.getComponents.getSchemas.asScala.toMap

  def decode(schemaName: String): Either[String, Schema[_]] = {
    val oaischemas = schemaMap.toList
    ForEach[List]
      .forEach(oaischemas) {
        case (name, oaischema) if name == schemaName => decode(oaischema).map(List(_))
        case _                                       => Right(List.empty)
      }
      .flatMap { schemas =>
        schemas.flatten.headOption match {
          case Some(value) => Right(value)
          case None =>
            Left(
              s"No schema found matching name '${schemaName}'.  Available schemas ${oaischemas.map(tup => s"'${tup._1}'").mkString(", ")}"
            )
        }

      }
  }

  def decode(oaisSchema: OAISschema[_]): Either[String, Schema[_]] =
    oaisSchema match {
      //      case (oais: StringSchema, StandardType.UnitType) => ???
      case _: BooleanSchema       => Right(Schema.primitive[Boolean])
      case Numbers(schema)        => Right(schema)
      case Integers(schema)       => Right(schema)
      case Strings(schema)        => Right(schema)
      case Maps(result)           => result
      case Record(schema)         => schema
      case ComposedSchema(schema) => schema
      case Ref(schema)            => schema
      case other                  => Left(s"I was given an OpenAPI schema definition that I cannot decode, '${other}'.")
    }

  private object Numbers {

    def unapply(numberSchema: NumberSchema): Option[Schema[_]] = {

      val schema = Option(numberSchema.getFormat) match {
        case Some("float")  => Schema.primitive[Float] @@ fmt("float")
        case Some("double") => Schema.primitive[Double] @@ fmt("double")
        case Some(custom)   => Schema.primitive[java.math.BigDecimal] @@ fmt(custom)
        case _              => Schema.primitive[java.math.BigDecimal]
      }

      Some(schema @@ minimum(numberSchema) @@ maximum(numberSchema))

    }
  }

  private object ComposedSchema {

    def unapply(oaiSchema: ComposedSchema): Option[Either[String, Schema[_]]] = {

      val schemas = ForEach[Chunk].forEach(Chunk.fromIterable(oaiSchema.getAllOf.asScala))(decode)
      val fields = schemas.map { schemas =>
        schemas.collect {
          case Schema.GenericRecord(_, fields, _) => fields.toChunk
        }.flatten
      }

      Some(fields.map(fields => GenericRecord(TypeId.fromTypeName(oaiSchema.getName), FieldSet(fields: _*))))

    }
  }

  private object ExtractBigD {
    def unapply(arg: java.math.BigDecimal): Option[Int] = Some(arg.intValue())
  }

  private object Integers {

    def unapply(intSchema: IntegerSchema): Option[Schema[_]] = {

      val schema: Schema[_] =
        (Option(intSchema.getMinimum), Option(intSchema.getMaximum), Option(intSchema.getFormat)) match {
          case (Some(ExtractBigD(-128)), Some(ExtractBigD(127)), None) =>
            Schema.primitive[Byte]
          case (_, _, format @ Some("int8" | "byte")) =>
            Schema.primitive[Byte] @@ fmt(format.get)
          case (Some(ExtractBigD(-32768)), Some(ExtractBigD(32767)), None) =>
            Schema.primitive[Short] @@ fmt("int16")
          case (_, _, Some("int16"))       => Schema.primitive[Short] @@ fmt("int16")
          case (_, _, Some("int32"))       => Schema.primitive[Int] @@ fmt("int32")
          case (_, _, Some("int64"))       => Schema.primitive[Long] @@ fmt("int64")
          case (_, _, Some("day-of-week")) => Schema.primitive[DayOfWeek] @@ fmt("day-of-week")
          case (_, _, Some("month"))       => Schema.primitive[Month] @@ fmt("month")
          case (_, _, Some("year"))        => Schema.primitive[Year] @@ year @@ fmt("year")
          case (_, _, Some(format))        => Schema.primitive[BigInteger] @@ fmt(format)
          case _                           => Schema.primitive[BigInteger]
        }

      Some(schema @@ minimum(intSchema) @@ maximum(intSchema))
    }
  }

  private object Strings {

    def unapply(stringSchema: StringSchema): Option[Schema[_]] = {
      val schema: Schema[_] = Option(stringSchema.getFormat) match {
        case Some("date")        => Schema.primitive[LocalDate] @@ fmt("date") @@ date
        case Some("date-time")   => Schema.primitive[OffsetDateTime] @@ fmt("date-time") @@ dateTime
        case Some("byte")        => Schema.primitive[Chunk[Byte]] @@ fmt("byte") @@ bytes
        case Some("binary")      => Schema.primitive[Chunk[Byte]] @@ fmt("binary")
        case Some("uuid")        => Schema.primitive[UUID] @@ fmt("uuid") @@ uuid
        case Some("char")        => Schema.primitive[Char] @@ fmt("char")
        case Some("month-day")   => Schema.primitive[MonthDay] @@ fmt("month-day") @@ monthDay
        case Some("period")      => Schema.primitive[Period] @@ fmt("period") @@ period
        case Some("year-month")  => Schema.primitive[YearMonth] @@ fmt("year-month") @@ yearMonth
        case Some("zone-id")     => Schema.primitive[ZoneId] @@ fmt("zone-id") @@ zoneId
        case Some("zone-offset") => Schema.primitive[ZonedDateTime] @@ fmt("zone-offset") @@ zoneOffset
        case Some("duration")    => Schema.primitive[Duration] @@ fmt("duration") @@ duration
        case Some("instant")     => Schema.primitive[Instant] @@ fmt("instant") @@ instant
        case Some("time")        => Schema.primitive[LocalTime] @@ fmt("time") @@ localTime
        case Some("local-date-time") =>
          Schema.primitive[LocalDateTime] @@ fmt("local-date-time") @@ localDateTime
        case Some("zoned-date-time") =>
          Schema.primitive[ZonedDateTime] @@ fmt("zoned-date-time") @@ zonedDateTime
        case Some(format) => Schema.primitive[String] @@ fmt(format)
        case _            => Schema.primitive[String]
      }

      Some(schema @@ minLength(stringSchema) @@ maxLength(stringSchema) @@ pattern(stringSchema))
    }
  }

  /**
   *
   * See: https://swagger.io/docs/specification/data-models/dictionaries/
   */
  private object Maps {

    def unapply(arg: MapSchema): Option[Either[String, Schema[_]]] = {
      val valueSchema = Option(arg.getAdditionalProperties)
        .toRight(s"'additionalProperties' defining the value schema are required by OpenAPI Maps.")
        .flatMap {
          case schema: OAISschema[_] => decode(schema).map(Schema.Map(Schema.primitive[String], _))
          case jsonSchema: OAISschema[_] if jsonSchema.getTypes == null =>
            Left("zio-schema-openapi does not support free-form objects.")
        }
      Some(valueSchema.map(valueSchema => Schema.Map(Schema.primitive[String], valueSchema)))

    }
  }

  private object Record {

    def unapply(openapi: ObjectSchema): Option[Either[String, Schema[_]]] = {

      val kvs = ForEach[List].forEach(openapi.getProperties.asScala.toList) {
        case (name, schema) => decode(schema).map(name -> _)
      }

      val fields = kvs.map {
        _.map {
          case (name, schema) =>
            Schema.Field[ListMap[String, _], Any](
              name0 = name,
              schema0 = schema.asInstanceOf[Schema[Any]],
              get0 = _.apply(name),
              set0 = (lm, a) => lm + (name -> a)
            )
        }
      }

      Some(
        fields.map { fields =>
          Schema.GenericRecord(TypeId.fromTypeName(openapi.getName), FieldSet(fields: _*))
        }
      )

    }
  }

  private object Ref {

    def unapply(oaiSchema: OAISschema[_]): Option[Either[String, Schema[_]]] = {
      val ref = oaiSchema.get$ref()
      if (ref != null) {
        Some(ref.split("/").lastOption match {
          case Some(name) =>
            schemaMap.get(name) match {
              case Some(oaischema) => decode(oaischema)
              case None =>
                Left(
                  s"The ref schema, '$name', was not found.  Available schemas: ${schemaMap.keys.map(name => s"'${name}''").mkString(",")}"
                )
            }
          case None => Left(s"Unable to determine local schema name from ref '${ref}'.")
        })

      } else None

    }
  }

}
