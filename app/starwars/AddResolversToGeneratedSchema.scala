package starwars

import play.api.libs.json.JsValue
import sangria.macros.derive._
import sangria.marshalling.playJson._
import sangria.schema.AstSchemaBuilder.{FieldName, TypeName}
import sangria.schema._
import starwars.StarWarsData.{Droid, Episode, Human, Repo}

object AddResolversToGeneratedSchema {

  val schema: Schema[Repo, Any] = {
    implicit val episodeType: EnumType[Episode] = deriveEnumType[Episode]()
    lazy val characterType: InterfaceType[Repo, Character] =
      InterfaceType(
        "Character",
        () =>
          fields[Repo, Character](
            Field(
              "id",
              StringType,
              resolve = c => ???
            ),
            Field(
              "name",
              StringType,
              resolve = _ => ???
            ),
            Field(
              "friends",
              ListType(characterType),
              resolve = ctx ⇒ ???
            ),
            Field(
              "appearsIn",
              ListType(episodeType),
              resolve = c => ???
            )
        )
      )
    val humanType =
      deriveObjectType[Repo, Human](
        Interfaces(PossibleInterface(characterType)),
        ReplaceField(
          "friends",
          Field("friends", ListType(characterType), resolve = c => ???)
        )
      )
    val droidType =
      deriveObjectType[Repo, Droid](
        Interfaces(PossibleInterface(characterType)),
        ReplaceField(
          "friends",
          Field("friends", ListType(characterType), resolve = c => ???)
        )
      )
    val episodeArg = Argument("episode", OptionInputType(episodeType))
    val idArg = Argument("id", StringType)
    val QueryType = ObjectType(
      "Query",
      fields[Repo, Unit](
        Field(
          "hero",
          characterType,
          arguments = episodeArg :: Nil,
          resolve = c ⇒ ???, //c.ctx.hero(episode)
          deprecationReason = Some("Use `human` or `droid` fields instead")
        ),
        Field(
          "human",
          OptionType(humanType),
          arguments = idArg :: Nil,
          resolve = c => ???
        ),
        Field(
          "droid",
          OptionType(droidType),
          arguments = idArg :: Nil,
          resolve = c => ???
        )
      )
    )

    val builder = AstSchemaBuilder.resolverBased[Repo](
      FieldResolver {
        case (TypeName("Query"), FieldName(fn)) =>
          fn match {
            case "hero" =>
              ctx =>
                val episodeO = ctx.args
                  .argOpt[String]("episode")
                  .map(Episode.apply)
                episodeO match {
                  case Some(episode) => ctx.ctx.hero(episode)
                  case None          => ctx.ctx.mainHero
                }
            case "human" =>
              ctx =>
                val id = ctx.args.arg[String]("id")
                ctx.ctx.human(id)
            case "droid" =>
              ctx =>
                val id = ctx.args.arg[String]("id")
                ctx.ctx.droid(id)
          }
      },
      FieldResolver.defaultInput[Repo, JsValue]
    )

    val staticSchema = Schema(QueryType)
    Schema.buildFromAst[Repo](staticSchema.toAst, builder)
  }

}
