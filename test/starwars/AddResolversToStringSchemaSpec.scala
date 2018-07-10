package starwars

import sangria.macros._
import sangria.marshalling.playJson._
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.Json
import sangria.ast.Document
import sangria.execution.Executor
import starwars.StarWarsData.Repo

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class AddResolversToStringSchemaSpec extends WordSpec with Matchers {

  val schema = AddResolversToStringSchema.schema

  "The graphql API" should {

    "be able to query the main hero" in {
      runQuery(
        gql"""
         {
           hero {
             name
             friends {
               name
             }
             appearsIn
           }
         }
       """
      ) shouldBe Json.parse("""
         |{"data":{"hero":{"name":"R2D2","friends":[
         |  {
         |    "name": "Luke"
         |  },
         |  {
         |    "name": "Leïa"
         |  }
         |],"appearsIn":["NEWHOPE","EMPIRE","JEDI"]}}}
       """.stripMargin)
    }

    "be able to query the main hero by episode" in {
      runQuery(
        gql"""
        {
          hero(episode: JEDI) {
            name
            friends {
              name
            }
            appearsIn
          }
        }
        """
      ) shouldBe Json.parse(
        """
          |{"data":{"hero":{"name":"Luke","friends":[
          |  {
          |    "name":"R2D2"
          |  },
          |  {
          |    "name": "Leïa"
          |  }
          |],"appearsIn":["NEWHOPE","EMPIRE","JEDI"]}}}
        """.stripMargin
      )
    }

    "be able to query a human by its id" in {
      runQuery(
        gql"""
        {
          human(id: "2") {
            name
            homePlanet
          }
        }
        """
      ) shouldBe Json.parse("""
          |{
          |  "data": {
          |    "human": {
          |      "name": "Luke",
          |      "homePlanet": "Tatooine"
          |    }
          |  }
          |}
      """.stripMargin)
    }

    "be able to query a droid by its id" in {
      runQuery(
        gql"""
        {
          droid(id: "1") {
            name
            primaryFunction
          }
        }
        """
      ) shouldBe Json.parse("""
                              |{
                              |  "data": {
                              |    "droid": {
                              |      "name": "R2D2",
                              |      "primaryFunction": "astromech"
                              |    }
                              |  }
                              |}
                            """.stripMargin)
    }

  }

  def runQuery(query: Document): Any = {
    val future = Executor.execute(schema, query, new Repo)
    Await.result(future, Duration.Inf)
  }

}
