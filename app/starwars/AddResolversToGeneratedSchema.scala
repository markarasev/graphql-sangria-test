package starwars

import play.api.libs.json.JsValue
import sangria.ast.Document
import sangria.execution.Executor
import sangria.macros._
import sangria.macros.derive._
import sangria.marshalling.playJson._
import sangria.parser.QueryParser
import sangria.schema.AstSchemaBuilder.{FieldName, TypeName}
import sangria.schema._
import starwars.StarWarsData.{Episode, Repo}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.io.Source

object AddResolversToGeneratedSchema extends App {

  implicit val episodeType: EnumType[Episode] = deriveEnumType[Episode]()
  implicit val characterType: ObjectType[Repo, Character] =
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
  val schema = Schema.buildFromAst[Repo](staticSchema.toAst, builder)

  def compareSchemaWithFile(): Unit = {
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
