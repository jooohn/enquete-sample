import cats.implicits._
import io.circe._
import io.circe.generic.extras.{Configuration, ConfiguredJsonCodec}
import io.circe.generic.semiauto._
import io.circe.parser._

object Main {

  def main(args: Array[String]): Unit = {
    import Lib._

    // 20代または30代 かつ (身長-体重)>110
    val modsJson = decode[List[Mod]](
      """
        |[
        |  {
        |    "type": "AND",
        |    "attributes": {
        |      "conditions": [
        |        {
        |          "type": "ONE_OF",
        |          "attributes": {
        |            "target": { "type": "RAW", "attributes": { "index": 0 } },
        |            "candidates": [
        |              { "type": "CONST", "attributes": { "value": "20代" } },
        |              { "type": "CONST", "attributes": { "value": "30代" } }
        |            ]
        |          }
        |        },
        |        {
        |          "type": "GREATER_THAN",
        |          "attributes": {
        |            "left": {
        |              "type": "MINUS",
        |              "attributes": {
        |                "left": { "type": "RAW", "attributes": { "index": 2 } },
        |                "right": { "type": "RAW", "attributes": { "index": 3 } }
        |              }
        |            },
        |            "right": {
        |              "type": "CONST",
        |              "attributes": { "value": 110 }
        |            }
        |          }
        |        }
        |      ]
        |    }
        |  }
        |]
      """.stripMargin)

    val mods: List[Mod] = modsJson match {
      case Right(ms) => ms
      case Left(e) => throw new Exception(e.getMessage)
    }

    // 年代,性別,身長(cm),体重(kg)
    val sampleData = List(
      Vector(
        Value.StringValue("20代"),
        Value.StringValue("男性"),
        Value.LongValue(170),
        Value.LongValue(80)
      ),
      Vector(
        Value.StringValue("40代"),
        Value.StringValue("女性"),
        Value.LongValue(155),
        Value.LongValue(40)
      ),
      Vector(
        Value.StringValue("30代"),
        Value.StringValue("女性"),
        Value.LongValue(175),
        Value.LongValue(50)
      )
    )

    val computeRow = compute(mods) _
    sampleData.foreach { row =>
      println(
        computeRow(row).map {
          case Left(e) => println(s"[ERROR]: ${e.message}")
          case Right(value) => value.toString
        }.mkString(",")
      )
    }
  }
}

object Lib {
  import Value._

  implicit val config: Configuration = Configuration.default.withSnakeCaseKeys

  trait Error { def message: String }
  case object OutOfIndex extends Error {
    override def message: String = "out of index"
  }
  case object InvalidCast extends Error {
    override def message: String = "invalid cast"
  }

  sealed trait Value {
    def asLong: ErrorOr[Long] = Left(InvalidCast)
    def asString: ErrorOr[String] = Left(InvalidCast)
    def asBoolean: ErrorOr[Boolean] = Left(InvalidCast)
  }

  object Value {
    case class LongValue(value: Long) extends Value {
      override def toString: String = value.toString
      override def asLong: ErrorOr[Long] = Right(value)
    }

    case class StringValue(value: String) extends Value {
      override def toString: String = value.toString
      override def asString: ErrorOr[String] = Right(value)
    }

    case class BooleanValue(value: Boolean) extends Value {
      override def toString: String = value.toString
      override def asBoolean: ErrorOr[Boolean] = Right(value)
    }

    def fromLong(value: Long): Value = LongValue(value)
    def fromString(value: String): Value = StringValue(value)
    def fromBoolean(value: Boolean): Value = BooleanValue(value)

    implicit val valueEncoder: Encoder[Value] = {
      case LongValue(v) => Json.fromLong(v)
      case StringValue(v) => Json.fromString(v)
      case BooleanValue(v) => Json.fromBoolean(v)
    }
    implicit val valueDecoder: Decoder[Value] =
      Decoder.decodeLong.map(fromLong) or
        Decoder.decodeString.map(fromString) or
        Decoder.decodeBoolean.map(fromBoolean)
  }

  type Index = Int
  type ErrorOr[A] = Either[Error, A]

  type RawValues = Vector[Value]
  type ComputedValue = ErrorOr[Value]
  type ComputedValues = Vector[ComputedValue]

  trait Mod {
    def compute(computedValues: ComputedValues): ComputedValue
  }

  object Mod {
    case class Struct(`type`: String, attributes: Json)
    implicit val decodeStruct: Decoder[Struct] = deriveDecoder[Struct]

    val decodeConst: Decoder[Const] = deriveDecoder[Const]
    val decodeRaw: Decoder[Raw] = deriveDecoder[Raw]
    val decodeOneOf: Decoder[OneOf] = deriveDecoder[OneOf]
    val decodeMinus: Decoder[Minus] = deriveDecoder[Minus]
    val decodeGreaterThan: Decoder[GreaterThan] = deriveDecoder[GreaterThan]
    val decodeAnd: Decoder[And] = deriveDecoder[And]

    val modDecoder: Decoder[Mod] = Decoder[Struct].emap {
      case Struct("RAW", attributes) => decodeRaw.decodeJson(attributes).leftMap(_.message)
      case Struct("CONST", attributes) => decodeConst.decodeJson(attributes).leftMap(_.message)
      case Struct("ONE_OF", attributes) => decodeOneOf.decodeJson(attributes).leftMap(_.message)
      case Struct("MINUS", attributes) => decodeMinus.decodeJson(attributes).leftMap(_.message)
      case Struct("GREATER_THAN", attributes) => decodeGreaterThan.decodeJson(attributes).leftMap(_.message)
      case Struct("AND", attributes) => decodeAnd.decodeJson(attributes).leftMap(_.message)
      case Struct(t, _) => Left(s"Invalid type $t")
      case _ => Left("Invalid json")
    }
    implicit val decodeMod: Decoder[Mod] = modDecoder

  }

  case class Raw(index: Index) extends Mod {
    override def compute(computedValues: ComputedValues): ComputedValue =
      computedValues.lift(index).getOrElse(Left(OutOfIndex))
  }

  case class Const(value: Value) extends Mod {
    override def compute(computedValues: ComputedValues): ComputedValue = Right(value)
  }

  // NOTE: Might be too much.
  case class OneOf(target: Mod, candidates: List[Mod]) extends Mod {
    override def compute(computedValues: ComputedValues): ComputedValue =
      for {
        targetValue <- target.compute(computedValues)
        candidateValues <- candidates.traverse(_.compute(computedValues))
      } yield BooleanValue(candidateValues.toSet(targetValue))
  }

  case class Minus(left: Mod, right: Mod) extends Mod {
    override def compute(computedValues: ComputedValues): ComputedValue =
      for {
        l <- left.compute(computedValues).asLong
        r <- right.compute(computedValues).asLong
      } yield LongValue(l - r)
  }

  case class GreaterThan(left: Mod, right: Mod) extends Mod {
    override def compute(computedValues: ComputedValues): ComputedValue =
      for {
        l <- left.compute(computedValues).asLong
        r <- right.compute(computedValues).asLong
      } yield BooleanValue(l > r)
  }

  case class And(conditions: List[Mod]) extends Mod {
    override def compute(computedValues: ComputedValues): ComputedValue =
      conditions.traverse(_.compute(computedValues).asBoolean).map { results =>
        BooleanValue(results.forall(identity))
      }
  }

  def compute(mods: List[Mod])(rawValues: RawValues): ComputedValues =
    mods.foldLeft(rawValues.map(_.asRight[Error])) { (computedValues, mod) =>
      computedValues :+ mod.compute(computedValues)
    }

  implicit class RichComputedValue(computedValue: ComputedValue) {
    def asLong: ErrorOr[Long] = computedValue.flatMap(_.asLong)
    def asString: ErrorOr[String] = computedValue.flatMap(_.asString)
    def asBoolean: ErrorOr[Boolean] = computedValue.flatMap(_.asBoolean)
  }
}
