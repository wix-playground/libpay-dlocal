package com.wix.pay.dlocal

import com.wix.hoopoe.http.testkit.EmbeddedHttpProbe
import org.json4s.DefaultFormats
import org.json4s.native.Serialization
import spray.http._

class DLocalDriver(port: Int) {

  private val probe = new EmbeddedHttpProbe(port, EmbeddedHttpProbe.NotFoundHandler)

  private implicit val formats = DefaultFormats

  def reset(): Unit = probe.reset()

  def start(): Unit = probe.doStart()

  def stop(): Unit = probe.doStop()

  def aSaleRequest() = new SaleRequest

  def anAuthorizeRequest() = new AuthorizeRequest

  def aCaptureRequest() = new CaptureRequest

  abstract class BaseRequest {
    def failsWith(errorCode: String, description: String): Unit = respondWith(responseWithError(errorCode, description))

    def failsWith(statusCode: StatusCode): Unit = respondWith(Map.empty, statusCode)

    def isRejectedWith(description: String): Unit = returns(resultStatus = "8", resultDescription = description)

    def isPending: Unit = returns(resultStatus = "7", resultDescription = "in_process")

    def returns(resultStatus: String, resultDescription: String)
  }

  class SaleRequest extends BaseRequest {
    def returns(documentId: String): Unit =
      respondWith(saleOkResponse(resultStatus = "9", resultDescription = "approved", documentId = documentId))

    def returns(resultStatus: String, resultDescription: String): Unit =
      respondWith(saleOkResponse(resultStatus = resultStatus, resultDescription = resultDescription))
  }

  class AuthorizeRequest extends BaseRequest{
    def returns(authId: String, invoiceId: String, currency: String): Unit =
      respondWith(authOkResponse(resultStatus = "11", resultDescription = "TODO", authId, invoiceId, currency))

    def returns(resultStatus: String, resultDescription: String): Unit =
      respondWith(authOkResponse(resultStatus = resultStatus, resultDescription = resultDescription))
  }

  class CaptureRequest extends BaseRequest {
    def returns(documentId: String): Unit =
      respondWith(saleOkResponse(resultStatus = "9", resultDescription = "approved", documentId = documentId))

    def returns(resultStatus: String, resultDescription: String): Unit =
      respondWith(saleOkResponse(resultStatus = resultStatus, resultDescription = resultDescription))
  }


  private def saleOkResponse(resultStatus: String,
                             resultDescription: String,
                             documentId: String = "93148038") = Map(
    "status" -> "OK",
    "desc" -> resultDescription,
    "control" -> "39BD42F98E7E8D7D451C851A1D06B030D010AF93A721BDCBFCD7F4E7852E9955",
    "result" -> resultStatus,
    "x_invoice" -> "Invoice1234",
    "x_iduser" -> "",
    "x_description" -> "new shoes",
    "x_document" -> documentId,
    "x_amount" -> "10.01",
    "x_currency" -> "BRL",
    "cc_token" -> "",
    "x_amount_paid" -> "10.01",
    "cc_descriptor" -> "Wix"
  )

  private def authOkResponse(resultStatus: String,
                             resultDescription: String,
                             authId: String = "some auth id",
                             invoiceId: String = "some invoice id ",
                             currency: String = "BRL") = Map(
    "status" -> "OK",
    "desc" -> resultDescription,
    "control" -> "39BD42F98E7E8D7D451C851A1D06B030D010AF93A721BDCBFCD7F4E7852E9955",
    "result" -> resultStatus,
    "x_invoice" -> invoiceId,
    "x_auth_id" -> authId,
    "x_amount" -> "10.01",
    "x_currency" -> currency,
    "x_amount_paid" -> "10.01",
    "cc_descriptor" -> "Wix"
  )

  private def responseWithError(errorCode: String, description: String) = Map(
    "status" -> "ERROR",
    "desc" -> description,
    "error_code" -> errorCode
  )

  def lastRequest = probe.requests.last

  protected def respondWith(content: Map[String, Any], status: StatusCode = StatusCodes.OK): Unit = {
    probe.handlers += {
      case HttpRequest(_, _, _, _, _) =>
        HttpResponse(status = status, entity = Serialization.write(content), headers = List(HttpHeaders.`Content-Type`(ContentTypes.`application/json`)))
    }
  }
}
