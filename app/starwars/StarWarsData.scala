package starwars

import play.api.libs.json._
import play.api.libs.json.{JsValue, Writes}

object StarWarsData {

  case class Character(
      id: String,
      name: String,
      friends: List[Character],
      appearsIn: List[Episode]
  )

  sealed trait Episode {
    val name: String
  }

  object Episode {

    implicit object EpisodeWrites extends Writes[Episode] {
      override def writes(episode: Episode): JsValue = Json.toJson(episode.name)
    }

    def apply(str: String): Episode = str match {
      case NEWHOPE.name => NEWHOPE
      case EMPIRE.name  => EMPIRE
      case JEDI.name    => JEDI
    }

  }

  case object NEWHOPE extends Episode {
    override val name: String = "NEWHOPE"
  }
  case object EMPIRE extends Episode {
    override val name: String = "EMPIRE"
  }
  case object JEDI extends Episode {
    override val name: String = "JEDI"
  }

  object Character {

    import play.api.libs.functional.syntax._

    implicit lazy val characterWrites: Writes[Character] = (
      (__ \ "id").write[String] and
        (__ \ "name").write[String] and
        (__ \ "friends").lazyWrite(Writes.list[Character](characterWrites)) and
        (__ \ "appearsIn").write[List[Episode]]
    )(unlift(Character.unapply))

  }

  class Repo {

    val (artoo, luke) = {
      var artoo =
        Character("1", "R2D2", List.empty, List(NEWHOPE, EMPIRE, JEDI))
      val luke =
        Character("2", "Luke", List(artoo), List(NEWHOPE, EMPIRE, JEDI))
      artoo = artoo.copy(friends = List(luke))
      (artoo, luke)
    }

    private val characters = List(artoo, luke)

    def mainHero: JsValue = Character.characterWrites.writes(artoo)

    def hero(episode: Episode): JsValue = {
      val hero = episode match {
        case JEDI => luke
        case _    => artoo
      }
      Character.characterWrites.writes(hero)
    }

  }

}
