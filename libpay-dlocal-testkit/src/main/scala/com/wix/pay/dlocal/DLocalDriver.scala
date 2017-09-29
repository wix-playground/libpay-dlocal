package com.wix.pay.dlocal

import com.wix.hoopoe.http.testkit.EmbeddedHttpProbe
import org.json4s.DefaultFormats
import org.json4s.native.Serialization
import spray.http.Uri.Path
import spray.http._

class DLocalDriver(probe: EmbeddedHttpProbe) {
  def this(port: Int) = this(new EmbeddedHttpProbe(port, EmbeddedHttpProbe.NotFoundHandler))

  def reset(): Unit = probe.reset()
  def start(): Unit = probe.doStart()
  def stop(): Unit = probe.doStop()

  def aSaleRequest() = new SaleRequest
  def anAuthorizeRequest() = new AuthorizeRequest
  def aCaptureRequest() = new CaptureRequest
  def aVoidAuthorizationRequest() = new VoidAuthorizationRequest

  abstract class BaseRequest {

    val path: String

    def returnsWithNotExpected(resultStatus: String, resultDescription: String)

    def failsWith(httpStatusCode: StatusCode): Unit = respondWith(Map.empty, httpStatusCode)

    def failsWith(errorCode: String, description: String): Unit = respondWith(responseWithError(errorCode, description))

    def isRejectedWith(description: String): Unit = returnsWithNotExpected(resultStatus = "8", resultDescription = description)

    def isPendingWith(description: String): Unit = returnsWithNotExpected(resultStatus = "7", resultDescription = description)

    protected def respondWith(content: Map[String, Any], status: StatusCode = StatusCodes.OK): Unit = {
      implicit val formats = DefaultFormats

      probe.handlers prepend {
        case HttpRequest(_, uri, _, _, _) if uri.path == Path(path) =>
          HttpResponse(status = status, entity = Serialization.write(content))
      }
    }
  }

  class SaleRequest extends BaseRequest {
    val path = "/api_curl/cc/sale"

    def returns(documentId: String): Unit =
      respondWith(saleOkResponse(documentId = documentId))

    def returnsWithNotExpected(resultStatus: String, resultDescription: String): Unit =
      respondWith(saleOkResponse(resultStatus = resultStatus, resultDescription = resultDescription))
  }

  class AuthorizeRequest extends BaseRequest {
    val path = "/api_curl/cc/auth"

    def returns(authId: String, invoiceId: String, currency: String): Unit =
      respondWith(authOkResponse(authId = authId, invoiceId = invoiceId, currency = currency))

    def returnsWithNotExpected(resultStatus: String, resultDescription: String): Unit =
      respondWith(authOkResponse(resultStatus = resultStatus, resultDescription = resultDescription))
  }

  class CaptureRequest extends BaseRequest {
    val path = "/api_curl/cc/capture"

    def returns(documentId: String): Unit =
      respondWith(captureOkResponse(documentId = documentId))

    def returnsWithNotExpected(resultStatus: String, resultDescription: String): Unit =
      respondWith(captureOkResponse(resultStatus = resultStatus, resultDescription = resultDescription))
  }

  class VoidAuthorizationRequest extends BaseRequest {
    val path = "/api_curl/cc/cancel"

    def returns(authId: String): Unit =
      respondWith(voidAuthOkResponse(authId = authId))

    def returnsWithNotExpected(resultStatus: String, resultDescription: String): Unit =
      respondWith(voidAuthOkResponse(resultStatus = resultStatus, resultDescription = resultDescription))
  }


  private def saleOkResponse(resultStatus: String = "9",
                             resultDescription: String = "approved",
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

  private def authOkResponse(resultStatus: String = "11",
                             resultDescription: String = "authorized",
                             authId: String = "some auth id",
                             invoiceId: String = "some invoice id ",
                             currency: String = "BRL") = Map(
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
    "cc_descriptor" -> "Wix"
  )

  private def captureOkResponse(resultStatus: String = "9",
                                resultDescription: String = "approved",
                                documentId: String = "93148038") = Map(
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
    "x_amount_captured" -> "10.01",
    "x_document" -> documentId
  )

  private def voidAuthOkResponse(resultStatus: String = "1",
                                 resultDescription: String = "cancelled",
                                 authId: String = "some auth id",
                                 invoiceId: String = "some invoice id ") = Map(
    "status" -> "OK",
    "desc" -> resultDescription,
    "control" -> "39BD42F98E7E8D7D451C851A1D06B030D010AF93A721BDCBFCD7F4E7852E9955",
    "result" -> resultStatus,
    "x_invoice" -> invoiceId,
    "x_auth_id" -> authId
  )

  private def responseWithError(errorCode: String, description: String) = Map(
    "status" -> "ERROR",
    "desc" -> description,
    "error_code" -> errorCode
  )

  def lastRequest = probe.requests.last
}
