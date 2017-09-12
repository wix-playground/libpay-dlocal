package com.wix.pay.dlocal

import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope
import spray.http._

class DLocalGatewayIT extends SpecWithJUnit {
  sequential

  val probePort = 10001
  val driver = new DLocalDriver(probePort)

  step {
    driver.start()
  }

  "sale" should {
    "send right http request" in new ctx {
      sale()

      lastRequest must beRequestWith(method = HttpMethods.POST)
      lastRequest must beRequestWith(url = saleUrl)
      lastRequest must beRequestThat(containsAllUrlEncodedParams = Seq(
        "x_login" -> setting.login,
        "x_trans_key" -> setting.transKey,
        "x_version" -> "4",
        "x_invoice" -> someDeal.invoiceId.get))
    }

    "return x_document if transaction approved" in new ctx {
      givenSaleRequest returns documentId

      sale() must beSucceedTryWith(documentId)
    }

    "fail if dLocal answers with error" in new ctx {
      givenSaleRequest failsWith(someErrorCode, someErrorDescription)

      sale() must beFailedTransactionWith(someErrorCode, someErrorDescription)
    }

    "fail if transaction rejected" in new ctx {
      givenSaleRequest isRejectedWith someRejectionDescription

      sale() must beRejectedWith(someRejectionDescription)
    }

    "fail if transaction pending" in new ctx {
      givenSaleRequest isPendingWith somePendingDescription

      sale() must failWith(pendingFlowIsNotSupportedMessage)
    }

    "fail if transaction is not approved" in new ctx {
      givenSaleRequest returnsWithNotExpected(someTransactionStatusCode, someDescription)

      sale() must failWith(s"Transaction is not Approved(9), but ($someTransactionStatusCode): $someDescription")
    }

    "handle http error" in new ctx {
      givenSaleRequest failsWith StatusCodes.InternalServerError

      sale() must failWith(internalServerErrorMessage)
    }
  }

  "authorize" should {
    "send right http request" in new ctx {
      authorize()

      lastRequest must beRequestWith(method = HttpMethods.POST)
      lastRequest must beRequestWith(url = authorizeUrl)
      lastRequest must beRequestThat(containsAllUrlEncodedParams = Seq(
        "x_login" -> setting.login,
        "x_trans_key" -> setting.transKey,
        "x_version" -> "4",
        "x_invoice" -> someDeal.invoiceId.get))
    }

    "return authorization if transaction authorized" in new ctx {
      givenAuthorizeRequest returns(authorization.authId, authorization.invoiceId, authorization.currency)

      authorize() must beSucceedTryWith(authorizationAsString)
    }

    "fail if dLocal answers with error" in new ctx {
      givenAuthorizeRequest failsWith(someErrorCode, someErrorDescription)

      authorize() must beFailedTransactionWith(someErrorCode, someErrorDescription)
    }


    "fail if transaction rejected" in new ctx {
      givenAuthorizeRequest isRejectedWith someRejectionDescription

      authorize() must beRejectedWith(someRejectionDescription)
    }

    "fail if transaction pending" in new ctx {
      givenAuthorizeRequest isPendingWith somePendingDescription

      authorize() must failWith(pendingFlowIsNotSupportedMessage)
    }

    "fail if transaction is not authorized" in new ctx {
      givenAuthorizeRequest returnsWithNotExpected(someTransactionStatusCode, someDescription)

      authorize() must failWith(s"Transaction is not Authorized(11), but ($someTransactionStatusCode): $someDescription")
    }

    "handle http error" in new ctx {
      givenAuthorizeRequest failsWith StatusCodes.InternalServerError

      authorize() must failWith(internalServerErrorMessage)
    }
  }

  "capture" should {
    "send right http request" in new ctx {
      capture()

      lastRequest must beRequestWith(method = HttpMethods.POST)
      lastRequest must beRequestWith(url = captureUrl)
      lastRequest must beRequestThat(containsAllUrlEncodedParams = Seq(
        "x_login" -> setting.login,
        "x_trans_key" -> setting.transKey,
        "x_version" -> "4",
        "x_invoice" -> authorization.invoiceId,
        "x_amount" -> somePayment.amount.toString,
        "x_currency" -> authorization.currency,
        "x_auth_id" -> authorization.authId,
        "type" -> "json"
      ))
    }

    "return x_document if transaction approved" in new ctx {
      givenCaptureRequest returns documentId

      capture() must beSucceedTryWith(documentId)
    }

    "fail if dLocal answers with error" in new ctx {
      givenCaptureRequest failsWith(someErrorCode, someErrorDescription)

      capture() must beFailedTransactionWith(someErrorCode, someErrorDescription)
    }

    "fail if transaction rejected" in new ctx {
      givenCaptureRequest isRejectedWith someRejectionDescription

      capture() must beRejectedWith(someRejectionDescription)
    }

    "fail if transaction is not approved" in new ctx {
      givenCaptureRequest returnsWithNotExpected(someTransactionStatusCode, someDescription)

      capture() must failWith(s"Transaction is not Approved(9), but ($someTransactionStatusCode): $someDescription")
    }

    "handle http error" in new ctx {
      givenCaptureRequest failsWith StatusCodes.InternalServerError

      capture() must failWith(internalServerErrorMessage)
    }
  }

  "void authorization" should {
    "send right http request" in new ctx {
      voidAuthorization()

      lastRequest must beRequestWith(method = HttpMethods.POST)
      lastRequest must beRequestWith(url = voidAuthorizationUrl)
      lastRequest must beRequestThat(containsAllUrlEncodedParams = Seq(
        "x_login" -> setting.login,
        "x_trans_key" -> setting.transKey,
        "x_version" -> "4",
        "x_invoice" -> authorization.invoiceId,
        "x_auth_id" -> authorization.authId,
        "type" -> "json"
      ))
    }

    "return x_auth_id if transaction canceled" in new ctx {
      givenVoidAuthorizationRequest returns authorization.authId

      voidAuthorization() must beSucceedTryWith(authorization.authId)
    }

    "fail if dLocal answers with error" in new ctx {
      givenVoidAuthorizationRequest failsWith(someErrorCode, someErrorDescription)

      voidAuthorization() must beFailedTransactionWith(someErrorCode, someErrorDescription)
    }

    "fail if transaction rejected" in new ctx {
      givenVoidAuthorizationRequest isRejectedWith someRejectionDescription

      voidAuthorization() must beRejectedWith(someRejectionDescription)
    }

    "fail if transaction is not canceled" in new ctx {
      givenVoidAuthorizationRequest returnsWithNotExpected(someTransactionStatusCode, someDescription)

      voidAuthorization() must failWith(s"Transaction is not Canceled(1), but ($someTransactionStatusCode): $someDescription")
    }

    "handle http error" in new ctx {
      givenVoidAuthorizationRequest failsWith StatusCodes.InternalServerError

      voidAuthorization() must failWith(internalServerErrorMessage)
    }
  }

  "authorize/capture" should {
    "work" in new ctx {
      givenAuthorizeRequest returns(authorization.authId, authorization.invoiceId, authorization.currency)
      givenCaptureRequest returns documentId

      authorize() must beSucceedTryWith(authorizationAsString)
      capture(authorizationAsString) must beSucceedTryWith(documentId)
    }
  }

  "authorize/void" should {
    "work" in new ctx {
      givenAuthorizeRequest returns(authorization.authId, authorization.invoiceId, authorization.currency)
      givenVoidAuthorizationRequest returns authorization.authId

      authorize() must beSucceedTryWith(authorizationAsString)
      voidAuthorization() must beSucceedTryWith(authorization.authId)
    }
  }

  trait ctx extends Scope with DLocalTestSupport {
    val dbLocalUrl = s"http://localhost:$probePort"
    val saleUrl = s"$dbLocalUrl/api_curl/cc/sale"
    val authorizeUrl = s"$dbLocalUrl/api_curl/cc/auth"
    val captureUrl = s"$dbLocalUrl/api_curl/cc/capture"
    val voidAuthorizationUrl = s"$dbLocalUrl/api_curl/cc/cancel"

    driver.reset()

    val setting = DLocalGatewaySettings(url = dbLocalUrl, sandboxUrl = dbLocalUrl, login = "some login", transKey = "some key", secretKey = "secret key")
    val gateway = new DLocalGateway(setting)

    def givenSaleRequest = driver.aSaleRequest()
    def givenAuthorizeRequest = driver.anAuthorizeRequest()
    def givenCaptureRequest = driver.aCaptureRequest()
    def givenVoidAuthorizationRequest = driver.aVoidAuthorizationRequest()

    def sale() = gateway.sale(merchantAsString, someCreditCard, somePayment, Some(someCustomer), Some(someDeal))
    def authorize() = gateway.authorize(merchantAsString, someCreditCard, somePayment, Some(someCustomer), Some(someDeal))
    def capture(authorization: String = authorizationAsString) = gateway.capture(merchantAsString, authorization, somePayment.amount)
    def voidAuthorization(authorization: String = authorizationAsString) = gateway.voidAuthorization(merchantAsString, authorization)

    def lastRequest = {
      val request = driver.lastRequest
      request must not(beNull)
      request
    }
  }

}
