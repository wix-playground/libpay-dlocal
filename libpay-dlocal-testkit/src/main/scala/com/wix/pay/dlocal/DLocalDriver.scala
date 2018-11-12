package com.wix.pay.dlocal


import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model._
import org.json4s.{DefaultFormats, Formats}
import org.json4s.native.Serialization
import com.wix.e2e.http.api.StubWebServer
import com.wix.e2e.http.server.WebServerFactory.aStubWebServer


class DLocalDriver(port: Int) {
  implicit val formats: Formats = DefaultFormats
  private val server: StubWebServer = aStubWebServer.onPort(port).build

  def start(): Unit = server.start
  def stop(): Unit = server.stop
  def reset(): Unit = {
    server.replaceWith()
    server.clearRecordedRequests()
  }

  def aSaleRequest() = new SaleRequest
  def anAuthorizeRequest() = new AuthorizeRequest
  def aCaptureRequest() = new CaptureRequest
  def aVoidAuthorizationRequest() = new VoidAuthorizationRequest

  abstract class BaseRequest {
    val path: String

    def returnsWithNotExpected(resultStatus: String, resultDescription: String, documentid: Option[String])
    def failsWith(httpStatusCode: StatusCode): Unit = respondWith(Map.empty, httpStatusCode)
    def failsWith(errorCode: String, description: String, documentId: Option[String] = None): Unit = {
      respondWith(responseWithError(errorCode, description, documentId))
    }

    def getsRejectedWith(description: String, documentId: Option[String] = None): Unit = {
      returnsWithNotExpected(resultStatus = "8", resultDescription = description, documentId)
    }

    def getsPendingWith(description: String, documentid: Option[String] = None): Unit = {
      returnsWithNotExpected(resultStatus = "7", resultDescription = description, documentid = documentid)
    }

    protected def respondWith(content: Map[String, Any], status: StatusCode = StatusCodes.OK): Unit = {
      server.appendAll {
        case HttpRequest(_, Path(`path`), _, _, _) =>
          HttpResponse(status = status, entity = Serialization.write(content))
      }
    }
  }

  class SaleRequest extends BaseRequest {
    val path = "/api_curl/cc/sale"

    def returns(documentId: String): Unit =
      respondWith(saleOkResponse(documentId = Some(documentId)))

    def returnsWithNotExpected(resultStatus: String, resultDescription: String, documentId: Option[String] = None): Unit =
      respondWith(saleOkResponse(resultStatus = resultStatus, resultDescription = resultDescription, documentId = documentId))
  }

  class AuthorizeRequest extends BaseRequest {
    val path = "/api_curl/cc/auth"

    def returns(authId: String, invoiceId: String, currency: String): Unit =
      respondWith(authOkResponse(authId = authId, invoiceId = invoiceId, currency = currency, documentId = None))

    def returnsWithNotExpected(resultStatus: String, resultDescription: String, documentId: Option[String] = None): Unit =
      respondWith(authOkResponse(resultStatus = resultStatus, resultDescription = resultDescription, documentId = documentId))
  }

  class CaptureRequest extends BaseRequest {
    val path = "/api_curl/cc/capture"

    def returns(documentId: String): Unit =
      respondWith(captureOkResponse(documentId = Some(documentId)))

    def returnsWithNotExpected(resultStatus: String, resultDescription: String, documentId: Option[String] = None): Unit =
      respondWith(captureOkResponse(resultStatus = resultStatus, resultDescription = resultDescription, documentId = documentId))
  }

  class VoidAuthorizationRequest extends BaseRequest {
    val path = "/api_curl/cc/cancel"

    def returns(authId: String): Unit =
      respondWith(voidAuthOkResponse(authId = authId, documentId = None))

    def returnsWithNotExpected(resultStatus: String, resultDescription: String, documentId: Option[String] = None): Unit =
      respondWith(voidAuthOkResponse(resultStatus = resultStatus, resultDescription = resultDescription, documentId = documentId))
  }


  private def saleOkResponse(resultStatus: String = "9",
                             resultDescription: String = "approved",
                             documentId: Option[String] = None) = Map(
    "status" -> "OK",
    "desc" -> resultDescription,
    "control" -> "39BD42F98E7E8D7D451C851A1D06B030D010AF93A721BDCBFCD7F4E7852E9955",
    "result" -> resultStatus,
    "x_invoice" -> "Invoice1234",
    "x_iduser" -> "",
    "x_description" -> "new shoes",
    "x_amount" -> "10.01",
    "x_currency" -> "BRL",
    "cc_token" -> "",
    "x_amount_paid" -> "10.01",
    "cc_descriptor" -> "Wix") ++ documentId.map("x_document" -> _)

  private def authOkResponse(resultStatus: String = "11",
                             resultDescription: String = "authorized",
                             authId: String = "some auth id",
                             invoiceId: String = "some invoice id ",
                             currency: String = "BRL",
                             documentId: Option[String]) = Map(
    "status" -> "OK",
    "desc" -> resultDescription,
    "control" -> "AF47C6B4C072FF4EF60E8924371EB0CFCB555B51C992333A45B8A3856467AFE3",
    "result" -> resultStatus,
    "x_invoice" -> invoiceId,
    "x_iduser" -> "",
    "x_description" -> "",
    "x_auth_id" -> authId,
    "x_amount" -> "10.01",
    "x_currency" -> currency,
    "x_amount_paid" -> "10.01",
    "cc_descriptor" -> "Wix") ++ documentId.map("x_document" -> _)

  private def captureOkResponse(resultStatus: String = "9",
                                resultDescription: String = "approved",
                                documentId: Option[String] = None) = Map(
    "status" -> "OK",
    "desc" -> resultDescription,
    "control" -> "6758AC25DCF51413DEC114DA67CEAC587CAB9ADAED439B6BEB0E36D685391BA1",
    "result" -> resultStatus,
    "x_invoice" -> "Invoice1234",
    "x_iduser" -> "",
    "x_description" -> "",
    "x_auth_id" -> "93161775",
    "x_amount" -> "10.01",
    "x_currency" -> "BRL",
    "x_amount_captured" -> "10.01") ++ documentId.map("x_document" -> _)

  private def voidAuthOkResponse(resultStatus: String = "1",
                                 resultDescription: String = "cancelled",
                                 authId: String = "some auth id",
                                 invoiceId: String = "some invoice id ",
                                 documentId: Option[String]) = Map(
    "status" -> "OK",
    "desc" -> resultDescription,
    "control" -> "39BD42F98E7E8D7D451C851A1D06B030D010AF93A721BDCBFCD7F4E7852E9955",
    "result" -> resultStatus,
    "x_invoice" -> invoiceId,
    "x_auth_id" -> authId) ++ documentId.map("x_document" -> _)

  private def responseWithError(errorCode: String, description: String, documentId: Option[String]) =
    Map(
    "status" -> "ERROR",
    "desc" -> description,
    "error_code" -> errorCode,
  ) ++ documentId.map("x_document" -> _)

  def lastRequest: HttpRequest = server.recordedRequests.last
}
