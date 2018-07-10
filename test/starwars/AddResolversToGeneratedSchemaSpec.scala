package starwars

import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.Json
import sangria.ast.Document
import sangria.execution.Executor
import sangria.macros._
import sangria.marshalling.playJson._
import starwars.StarWarsData.Repo

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class AddResolversToGeneratedSchemaSpec extends WordSpec with Matchers {

  val schema = AddResolversToGeneratedSchema.schema

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
      ) shouldBe Json.parse(
        """
          |{"data":{"hero":{"name":"R2D2","friends":[{"name":"Luke"}],"appearsIn":["NEWHOPE","EMPIRE","JEDI"]}}}
        """.stripMargin)
    }

    "be able to query the maine hero by episode" in {
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
          |{"data":{"hero":{"name":"Luke","friends":[{"name":"R2D2"}],"appearsIn":["NEWHOPE","EMPIRE","JEDI"]}}}
        """.stripMargin
      )
    }

  }

  def runQuery(query: Document): Any = {
    val future = Executor.execute(schema, query, new Repo)
    Await.result(future, Duration.Inf)
  }

}
