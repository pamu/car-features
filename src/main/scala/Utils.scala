import java.io.PrintWriter

import org.jsoup.Jsoup
import play.api.libs.json.{JsObject, Json}
import play.api.libs.ws.WSResponse
import scala.collection.JavaConversions._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scala.concurrent.duration._

/**
  * Created by pnagarjuna on 03/11/15.
  */
object Utils {
  def getMakes: List[Make] = {
    val html =
      """
        |	<option value="10" data-bind="value: makeId, text: makeName">Maruti Suzuki</option>
        |
        |
        |	<option value="8" data-bind="value: makeId, text: makeName">Hyundai</option>
        |
        |
        |	<option value="7" data-bind="value: makeId, text: makeName">Honda</option>
        |
        |
        |	<option value="17" data-bind="value: makeId, text: makeName">Toyota</option>
        |
        |
        |	<option value="16" data-bind="value: makeId, text: makeName">Tata</option>
        |
        |
        |	<option value="9" data-bind="value: makeId, text: makeName">Mahindra</option>
        |
        |
        |	<option value="5" data-bind="value: makeId, text: makeName">Ford</option>
        |
        |
        |	<option value="1" data-bind="value: makeId, text: makeName">BMW</option>
        |
        |
        |	<option value="2" data-bind="value: makeId, text: makeName">Chevrolet</option>
        |
        |
        |	<option value="15" data-bind="value: makeId, text: makeName">Skoda</option>
        |
        |
        |	<option value="11" data-bind="value: makeId, text: makeName">Mercedes-Benz</option>
        |
        |
        |	<option value="20" data-bind="value: makeId, text: makeName">Volkswagen</option>
        |
        |
        |	<option value="18" data-bind="value: makeId, text: makeName">Audi</option>
        |
        |
        |	<option value="4" data-bind="value: makeId, text: makeName">Fiat</option>
        |
        |
        |	<option value="12" data-bind="value: makeId, text: makeName">Mitsubishi</option>
        |
        |
        |	<option value="45" data-bind="value: makeId, text: makeName">Renault</option>
        |
        |
        |	<option value="21" data-bind="value: makeId, text: makeName">Nissan</option>
        |
        |
        |	<option value="23" data-bind="value: makeId, text: makeName">Land Rover</option>
        |
        |
        |	<option value="44" data-bind="value: makeId, text: makeName">Jaguar</option>
        |
        |
        |	<option value="19" data-bind="value: makeId, text: makeName">Porsche</option>
        |
        |
        |	<option value="54" data-bind="value: makeId, text: makeName">Ssangyong</option>
        |
        |
        |	<option value="37" data-bind="value: makeId, text: makeName">Volvo</option>
        |
        |
        |	<option value="51" data-bind="value: makeId, text: makeName">Mini</option>
        |
        |
        |	<option value="22" data-bind="value: makeId, text: makeName">Bentley</option>
        |
        |
        |	<option value="14" data-bind="value: makeId, text: makeName">Premier</option>
        |
        |
        |	<option value="50" data-bind="value: makeId, text: makeName">Force Motors</option>
        |
        |
        |	<option value="33" data-bind="value: makeId, text: makeName">Ferrari</option>
        |
        |
        |	<option value="53" data-bind="value: makeId, text: makeName">Ashok Leyland</option>
        |
        |
        |	<option value="49" data-bind="value: makeId, text: makeName">Aston Martin</option>
        |
        |
        |	<option value="47" data-bind="value: makeId, text: makeName">Bugatti</option>
        |
        |
        |	<option value="56" data-bind="value: makeId, text: makeName">Datsun</option>
        |
        |
        |	<option value="38" data-bind="value: makeId, text: makeName">ICML</option>
        |
        |
        |	<option value="61" data-bind="value: makeId, text: makeName">Isuzu</option>
        |
        |
        |	<option value="30" data-bind="value: makeId, text: makeName">Lamborghini</option>
        |
        |
        |	<option value="36" data-bind="value: makeId, text: makeName">Maserati</option>
        |
        |
        |	<option value="25" data-bind="value: makeId, text: makeName">Rolls-Royce</option>
        |
        |
        |	<option value="66" data-bind="value: makeId, text: makeName">DC</option>
        |
        |
        |	<option value="67" data-bind="value: makeId, text: makeName">Eicher Polaris</option>
      """.stripMargin

    val elements = Jsoup.parse(html).getElementsByTag("option").toList
    elements.map { elem =>
      Make(elem.text(), elem.`val`().toInt)
    }
  }

  def getModels(makeId: Int): Future[List[Model]] = {
    val modelsLink = s"""http://www.carwale.com/webapi/carmodeldata/GetCarModelsByType/?type=new&makeId=$makeId"""
    WS.client.url(modelsLink).withHeaders(
      "Content-Type" -> "application/json; charset=utf-8"
    ).get() map { res =>
      val modelsJson = Json.parse(res.body.toString)
      val list = modelsJson.as[List[JsObject]]
      list.map { obj =>
        println(obj)
        Model((obj \ "ModelName").as[String], (obj \ "ModelId").as[Int], (obj \ "MaskingName").as[String])
      }
    }
  }

  def getVersions(modelId: Int): Future[List[Version]] = {
    val versionsLink = s"""http://www.carwale.com/webapi/carversionsdata/GetCarVersions/?type=new&modelId=$modelId"""
    WS.client.url(versionsLink).withHeaders(
      "Content-Type" -> "application/json; charset=utf-8"
    ).get() map { res =>
      val versionsJson = Json.parse(res.body.toString)
      val list = versionsJson.as[List[JsObject]]
      list.map { obj =>
        println(obj)
        Version((obj \ "Name").as[String], (obj \ "ID").as[Int])
      }
    }
  }

  def getLink(make: Make, model: Model, version: Version): String =
    s"""http://www.carwale.com/${make.name.toLowerCase}-cars/${model.maskingName.toLowerCase}/${version.name.toLowerCase.split("\\s+").map(_.trim).reduce(_ + _).split("[.]").map(_.trim).reduce(_ + _)}-${version.id}"""

  def getWebPage(link: String): Future[WSResponse] = {
    WS.client.url(link).withFollowRedirects(true).get()
  }

  def links: Unit = {
    val data = new PrintWriter(s"${System.getProperty("user.home")}/Desktop/car-features.csv")
    val errors = new PrintWriter(s"${System.getProperty("user.home")}/Desktop/not-ok.csv")
    getMakes.map { make =>
      val modelsF = getModels(make.id)
      Await.ready(modelsF, 1 minute)
      modelsF onComplete {
        case Success(sModels) =>
          sModels.map { model =>
            val versionsF = getVersions(model.id)
            Await.ready(versionsF, 1 minute)
            versionsF onComplete {
              case Success(sVersions) =>
                sVersions.map { version =>
                  println("Getting link")
                  val link = getLink(make, model, version)
                  println(s"link => $link")
                  val fWebPage = getWebPage(link)
                  Await.ready(fWebPage, 1 minute)
                  fWebPage onComplete {
                    case Success(sValues) =>
                      if (sValues.status == 200) {

                      } else {

                      }
                    case Failure(fValues) =>

                  }
                }
              case Failure(fVersions) =>
                None
            }
          }
        case Failure(fModels) =>
          None
      }
    }
  }
}
