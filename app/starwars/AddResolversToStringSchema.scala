package starwars

import play.api.libs.json.JsValue
import sangria.ast.Document
import sangria.execution.Executor
import sangria.macros._
import sangria.marshalling.playJson._
import sangria.parser.QueryParser
import sangria.schema.AstSchemaBuilder.{FieldName, TypeName}
import sangria.schema.{AstSchemaBuilder, FieldResolver, Schema}
import starwars.StarWarsData.{Episode, Repo}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.io.Source

object AddResolversToStringSchema extends App {

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

  val schema = Schema.buildFromAst[Repo](ast, builder)

  def runQuery(query: Document): Unit = {
    val future = Executor.execute(schema, query, new Repo)
    val result = Await.result(future, Duration.Inf)
    println(result)
  }

  runQuery(gql"""
         {
           hero {
             name
             friends {
               name
             }
             appearsIn
           }
         }
       """)

  runQuery(gql"""
    {
      hero(episode: JEDI) {
        name
        friends {
          name
        }
        appearsIn
      }
    }
  """)

}
