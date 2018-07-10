package starwars

import play.api.libs.functional.FunctionalBuilder
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.{JsValue, Writes}

object StarWarsData {

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

  sealed trait Character {
    val id: String
    val name: String
    val friends: List[Character]
    val appearsIn: List[Episode]
  }

  object Character {

    val baseWrites: FunctionalBuilder[OWrites]#CanBuild4[String,
                                                         String,
                                                         List[Character],
                                                         List[Episode]] =
      (__ \ "id").write[String] and
        (__ \ "name").write[String] and
        (__ \ "friends").lazyWrite(
          Writes.list[Character](Character.CharacterWrites)) and
        (__ \ "appearsIn").write[List[Episode]]

    implicit object CharacterWrites extends Writes[Character] {
      override def writes(character: Character): JsValue = character match {
        case human: Human => Human.humanWrites.writes(human)
        case droid: Droid => Droid.droidWrites.writes(droid)
      }
    }

  }

  case class Droid(
      override val id: String,
      override val name: String,
      override val friends: List[Character],
      override val appearsIn: List[Episode],
      primaryFunction: String
  ) extends Character

  object Droid {
    implicit lazy val droidWrites: Writes[Droid] = (
      Character.baseWrites and (__ \ "primaryFunction").write[String]
    )(unlift(Droid.unapply))
  }

  case class Human(
      override val id: String,
      override val name: String,
      override val friends: List[Character],
      override val appearsIn: List[Episode],
      homePlanet: String
  ) extends Character

  object Human {
    implicit lazy val humanWrites: Writes[Human] = (
      Character.baseWrites and (__ \ "homePlanet").write[String]
    )(unlift(Human.unapply))
  }

  class Repo {

    private val (artoo, luke, leia) = {
      var artoo =
        Droid("1", "R2D2", List.empty, List(NEWHOPE, EMPIRE, JEDI), "astromech")
      var luke =
        Human("2", "Luke", List(artoo), List(NEWHOPE, EMPIRE, JEDI), "Tatooine")
      val leia = Human(
        "3",
        "Leïa",
        List(artoo, luke),
        List(NEWHOPE, EMPIRE, JEDI),
        "Alderaan"
      )
      artoo = artoo.copy(friends = List(luke, leia))
      luke = luke.copy(friends = List(artoo, leia))
      (artoo, luke, leia)
    }

    private val humans = List(luke, leia)
    private val droids = List(artoo)

    def mainHero: JsValue = Json.toJson(artoo)(Character.CharacterWrites)

    def hero(episode: Episode): JsValue = {
      val hero = episode match {
        case JEDI => luke
        case _    => artoo
      }
      Json.toJson(hero)(Character.CharacterWrites)
    }

    def human(id: String): JsValue = {
      import Human.humanWrites
      Json.toJson(humans.find(_.id == id))
    }

    def droid(id: String): JsValue = {
      import Droid.droidWrites
      Json.toJson(droids.find(_.id == id))
    }

  }

}
