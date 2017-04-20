package com.wix.pay.dlocal

import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope
import spray.http._

import scala.util.Try

class DLocalGatewayIT extends SpecWithJUnit {

  val probePort = 10001
  val driver = new DLocalDriver(probePort)

  step {
    driver.start()
  }

  sequential

  "sale" should {

    "send right http request" in new ctx {
      Try(sale())

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

      sale() must failWith(s"Transaction failed($someErrorCode): $someErrorDescription")
    }


    "fail if transaction rejected" in new ctx {
      givenSaleRequest isRejectedWith someRejectionDescription

      sale() must beRejectedWith(someRejectionDescription)
    }

    "fail if transaction pending" in new ctx {
      givenSaleRequest isPending

      sale() must failWith(pendingFlowIsNotSupportedMessage)
    }

    "fail if transaction is not approved" in new ctx {
      givenSaleRequest returns (someTransactionStatusCode, someDescription)

      sale() must failWith(s"Transaction is not approved($someTransactionStatusCode): $someDescription")
    }

    "handle http error" in new ctx {
      givenSaleRequest failsWith StatusCodes.InternalServerError

      sale() must failWith("500 Internal Server Error")
    }
  }

  "authorize" should {
    "send right http request" in new ctx {
      Try(authorize())

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

      authorize() must failWith(s"Transaction failed($someErrorCode): $someErrorDescription")
    }


    "fail if transaction rejected" in new ctx {
      givenAuthorizeRequest isRejectedWith someRejectionDescription

      authorize() must beRejectedWith(someRejectionDescription)
    }

    "fail if transaction pending" in new ctx {
      givenAuthorizeRequest isPending

      authorize() must failWith(pendingFlowIsNotSupportedMessage)
    }

    "fail if transaction is not authorized" in new ctx {
      givenAuthorizeRequest returns (someTransactionStatusCode, someDescription)

      authorize() must failWith(s"Transaction is not authorized($someTransactionStatusCode): $someDescription")
    }

    "handle http error" in new ctx {
      givenAuthorizeRequest failsWith StatusCodes.InternalServerError

      authorize() must failWith("500 Internal Server Error")
    }
  }
  trait ctx extends Scope with DLocalTestSupport {
    val dbLocalUrl = s"http://localhost:$probePort"
    val saleUrl = s"$dbLocalUrl/api_curl/cc/sale"
    val authorizeUrl = s"$dbLocalUrl/api_curl/cc/auth"

    val setting = DLocalGatewaySettings(url = dbLocalUrl, login = "some login", transKey = "some key", secretKey = "secret key")
    val gateway = new DLocalGateway(setting)

    driver.reset()

    def givenSaleRequest = driver.aSaleRequest()
    def givenAuthorizeRequest = driver.anAuthorizeRequest()

    def sale() = gateway.sale(merchantAsString, someCreditCard, somePayment, Some(someCustomer), Some(someDeal))
    def authorize() = gateway.authorize(merchantAsString, someCreditCard, somePayment, Some(someCustomer), Some(someDeal))

    def lastRequest = {
      val request = driver.lastRequest
      request must not(beNull)
      request
    }
  }
}
