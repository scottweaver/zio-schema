package zio.schema.codec

import io.swagger.v3.oas.models.media.{ Schema => OAISchema }
import zio.Chunk
import zio.schema.Schema

import java.time._
import java.time.format.{ DateTimeFormatter, ResolverStyle }
import java.util.{ Base64, UUID }
import scala.util.Try
import scala.util.matching.Regex
import zio.prelude._

import scala.annotation.StaticAnnotation

sealed trait OpenAPIAnnotation extends StaticAnnotation with Serializable with Product

object OpenAPIAnnotation {
  private def safe[A](op: => A): Either[String, A] =
    Try(op).toEither.left.map(_.getMessage)

  object Dsl {
    private val base64Decoder = Base64.getDecoder

    private val rfc3339 =
      DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss[.SSS]XXX").withResolverStyle(ResolverStyle.LENIENT)

    def bytes: OpenAPIAnnotation = Decoder.make[String, Chunk[Byte]](in => Chunk.fromArray(base64Decoder.decode(in)))

    def date: OpenAPIAnnotation = Decoder.make[String, LocalDate](LocalDate.parse)

    def dateTime: OpenAPIAnnotation = Decoder.make[String, LocalDateTime](LocalDateTime.parse(_, rfc3339))

    def instant: OpenAPIAnnotation = Decoder.make[String, Instant](Instant.parse)

    def duration: OpenAPIAnnotation = Decoder.make[String, Duration](Duration.parse)

    def fmt(format: String): OpenAPIAnnotation = Format(format)

    def localTime: OpenAPIAnnotation = Decoder.make[String, LocalTime](LocalTime.parse)

    def localDateTime: OpenAPIAnnotation = Decoder.make[String, LocalDateTime](LocalDateTime.parse)

    def maxLength(oais: OAISchema[_]): OpenAPIAnnotation =
      Option(oais.getMaxLength).map(i => MaxLength(i.toInt)).getOrElse(Empty)

    def maximum(oais: OAISchema[_]): OpenAPIAnnotation =
      Option(oais.getMaximum)
        .map(i => Maximum(i, Option[Boolean](oais.getExclusiveMaximum).getOrElse(false)))
        .getOrElse(Empty)

    def minimum(oais: OAISchema[_]): OpenAPIAnnotation =
      Option(oais.getMinimum)
        .map(i => Minimum(i, Option[Boolean](oais.getExclusiveMinimum).getOrElse(false)))
        .getOrElse(Empty)

    def minLength(oais: OAISchema[_]): OpenAPIAnnotation =
      Option(oais.getMinLength).map(i => MinLength(i.toInt)).getOrElse(Empty)

    def monthDay: OpenAPIAnnotation = Decoder.make[String, MonthDay](MonthDay.parse(_))

    def pattern(oais: OAISchema[_]): OpenAPIAnnotation =
      Option(oais.getPattern).map(Pattern.fromString).getOrElse(Empty)

    def period: OpenAPIAnnotation = Decoder.make[String, Period](Period.parse(_))

    def uuid: OpenAPIAnnotation = Decoder.make[String, UUID](UUID.fromString)

    def year: OpenAPIAnnotation = Decoder.make[Int, Year](Year.of)

    def yearMonth: OpenAPIAnnotation = Decoder.make[String, YearMonth](YearMonth.parse)

    def zonedDateTime: OpenAPIAnnotation = Decoder.make[String, ZonedDateTime](ZonedDateTime.parse)

    def zoneId: OpenAPIAnnotation = Decoder.make[String, ZoneId](ZoneId.of)

    def zoneOffset: OpenAPIAnnotation = Decoder.make[String, ZoneOffset](ZoneOffset.of)

    implicit final class Syntax(schema: Schema[_]) {

      def @@(openAPIAnnotation: OpenAPIAnnotation): Schema[_] = openAPIAnnotation match {
        case Empty      => schema
        case annotation => schema.annotate(annotation)
      }

    }

  }

  private[codec] case object Empty extends OpenAPIAnnotation

  final private[codec] case class Format(name: String) extends OpenAPIAnnotation

  final private[codec] case class MinLength(min: Int) extends OpenAPIAnnotation

  final private[codec] case class MaxLength(max: Int) extends OpenAPIAnnotation

  final private[codec] case class Minimum(min: BigDecimal, exclusive: Boolean) extends OpenAPIAnnotation

  final private[codec] case class Maximum(max: BigDecimal, exclusive: Boolean) extends OpenAPIAnnotation

  final private[codec] case class Pattern(regex: Regex) extends OpenAPIAnnotation

  object Pattern {
    def fromString(pattern: String): Pattern = Pattern(pattern.r)
  }

  final case class Decoder[In, Out](decode: In => Either[String, Out]) extends OpenAPIAnnotation

  object Decoder {

    def make[In, Out](map: In => Out): Decoder[In, Out] = new Decoder(in => safe(map(in)))
  }

}
