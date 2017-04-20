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


  class SaleRequest {
    def returns(documentId: String): Unit = returns(transactionStatusCode = "9", description = "approved", documentId = documentId)

    def returns(transactionStatusCode: String, description: String, documentId: String = "93148038"): Unit =
      respondWith(saleOkResponse(transactionStatusCode, description, documentId))

    def isRejectedWith(description: String): Unit = returns(transactionStatusCode = "8", description = description)

    def isPending: Unit = returns(transactionStatusCode = "7", description = "in_process")

    def failsWith(errorCode: String, description: String): Unit = respondWith(responseWithError(errorCode, description))

    def failsWith(statusCode: StatusCode): Unit = respondWith(Map.empty, statusCode)
  }

  class AuthorizeRequest {
    def returns(authId: String, invoiceId: String, currency: String): Unit =
      respondWith(authOkResponse(result = "11", resultDescription = "TODO", authId, invoiceId, currency))

    def returns(transactionStatusCode: String, description: String): Unit =
      respondWith(authOkResponse(transactionStatusCode, description))

    def isRejectedWith(description: String): Unit = returns(transactionStatusCode = "8", description = description)

    def isPending: Unit = returns(transactionStatusCode = "7", description = "in_process")

    def failsWith(errorCode: String, description: String): Unit = respondWith(responseWithError(errorCode, description))

    def failsWith(statusCode: StatusCode): Unit = respondWith(Map.empty, statusCode)
  }

  private def saleOkResponse(result: String,
                             resultDescription: String,
                             documentId: String = "93148038") = Map(
    "status" -> "OK",
    "desc" -> resultDescription,
    "control" -> "39BD42F98E7E8D7D451C851A1D06B030D010AF93A721BDCBFCD7F4E7852E9955",
    "result" -> result,
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

  private def authOkResponse(result: String,
                             resultDescription: String,
                             authId: String = "some auth id",
                             invoiceId: String = "some invoice id ",
                             currency: String = "BRL") = Map(
    "status" -> "OK",
    "desc" -> resultDescription,
    "control" -> "39BD42F98E7E8D7D451C851A1D06B030D010AF93A721BDCBFCD7F4E7852E9955",
    "result" -> result,
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
