package starwars

import play.api.libs.json.JsValue
import sangria.macros.derive._
import sangria.marshalling.playJson._
import sangria.parser.QueryParser
import sangria.schema.AstSchemaBuilder.{FieldName, TypeName}
import sangria.schema._
import starwars.StarWarsData.{Episode, Repo}

import scala.io.Source

object AddResolversToGeneratedSchema {

  val schema: Schema[Repo, Any] = {
    implicit val episodeType: EnumType[Episode] = deriveEnumType[Episode]()
    implicit lazy val characterType: ObjectType[Repo, Character] =
      deriveObjectType[Repo, Character](
        ReplaceField(
          "friends",
          Field("friends", ListType(characterType), resolve = c => ???)
        )
      )
    val episodeArg = Argument("episode", OptionInputType(episodeType))
    val QueryType = ObjectType(
      "Query",
      fields[Repo, Unit](
        Field(
          "hero",
          characterType,
          arguments = episodeArg :: Nil,
          resolve = c ⇒ ???, //c.ctx.hero(episode)
          deprecationReason = Some("Use `human` or `droid` fields instead")
        ))
    )

    val builder = AstSchemaBuilder.resolverBased[Repo](
      FieldResolver {
        case (TypeName("Query"), FieldName("hero")) =>
          ctx =>
            val episodeO: Option[Episode] = ctx.args
              .argOpt[String]("episode")
              .map(Episode.apply)
            episodeO match {
              case Some(episode) => ctx.ctx.hero(episode)
              case None          => ctx.ctx.mainHero
            }
      },
      FieldResolver.defaultInput[Repo, JsValue]
    )

    val staticSchema = Schema(QueryType)
    Schema.buildFromAst[Repo](staticSchema.toAst, builder)
  }

  private def compareSchemaWithFile(): Unit = {
    val schemaFileUrl = Thread.currentThread.getContextClassLoader
      .getResource("starwars.graphql")
    val schemaFileSource = Source.fromURL(schemaFileUrl)
    val schemaFileContents = schemaFileSource.mkString
    val schemaFileAst = QueryParser
      .parse(schemaFileContents)
      .get
    assert(CompareAst.areEquivalent(schemaFileAst, schema.toAst))
  }
  compareSchemaWithFile()

}
