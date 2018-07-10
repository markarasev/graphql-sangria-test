package starwars

import play.api.libs.json.JsValue
import sangria.marshalling.playJson._
import sangria.parser.QueryParser
import sangria.schema.AstSchemaBuilder.{FieldName, TypeName}
import sangria.schema.{AstSchemaBuilder, FieldResolver, Schema}
import starwars.StarWarsData.{Episode, Repo}

import scala.io.Source

object AddResolversToStringSchema {

  val schema: Schema[Repo, Any] = {
    val schemaUrl = Thread.currentThread.getContextClassLoader
      .getResource("starwars.graphql")
    val source = Source.fromURL(schemaUrl)
    val stringSchema = source.mkString

    val ast = QueryParser.parse(stringSchema).get

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
    Schema.buildFromAst[Repo](ast, builder)
  }

}
