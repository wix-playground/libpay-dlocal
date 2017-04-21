package com.wix.pay.dlocal

import javax.crypto.spec.SecretKeySpec

import com.google.api.client.http._
import com.google.api.client.http.javanet.NetHttpTransport
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.dlocal.DLocalGateway.{CheckSumEncoding, CheckSumHashingAlgorithm, ControlSumFields}
import com.wix.pay.model.{Customer, Deal, Payment}
import com.wix.pay.{PaymentErrorException, PaymentException, PaymentGateway}
import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods

import scala.collection.JavaConversions.mapAsJavaMap
import scala.util.Try

class DLocalGateway(settings: DLocalGatewaySettings) extends PaymentGateway {

  private val requestFactory: HttpRequestFactory = new NetHttpTransport().createRequestFactory()

  private val commonRequestFields = Map(
    "x_version" -> "4",
    "type" -> "json",
    "x_login" -> settings.login,
    "x_trans_key" -> settings.transKey
  )

  override def sale(merchantKey: String, creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    val merchant = DLocalMerchant.parse(merchantKey)

    execute(
      url = s"${settings.url}/api_curl/cc/sale",
      request = DLocalSaleRequest(merchant, creditCard, payment, customer, deal),
      responseHandler = DLocalSaleResponseHandler
    )
  }

  override def authorize(merchantKey: String, creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    val merchant = DLocalMerchant.parse(merchantKey)

    execute(
      url = s"${settings.url}/api_curl/cc/auth",
      request = DLocalSaleRequest(merchant, creditCard, payment, customer, deal), // authorize requests looks the same as sale,
      responseHandler = DLocalAuthorizeResponseHandler
    )
  }

  override def capture(merchantKey: String, authorizationKey: String, amount: Double): Try[String] = {
    val authorization = DLocalAuthorization.parse(authorizationKey)

    execute(
      url = s"${settings.url}/api_curl/cc/capture",
      request = DLocalCaptureRequest(authorization, amount),
      responseHandler = DLocalCaptureResponseHandler
    )
  }

  override def voidAuthorization(merchantKey: String, authorizationKey: String): Try[String] = {
    val authorization = DLocalAuthorization.parse(authorizationKey)

    execute(
      url = s"${settings.url}/api_curl/cc/cancel",
      request = DLocalVoidAuthorizationRequest(authorization),
      responseHandler = DLocalVoidAuthorizationResponseHandler
    )
  }

  def execute(url: String, request: DLocalRequest, responseHandler: DLocalResponseHandler): Try[String] = {
    Try {
      val allFields = sign(commonRequestFields ++ request.fields)

      val httpRequest = requestFactory.buildPostRequest(
        new GenericUrl(url),
        new UrlEncodedContent(mapAsJavaMap(allFields))
      )
      val httpResponse = httpRequest.execute()

      try {
        implicit val formats = DefaultFormats
        val jValue = JsonMethods.parse(httpResponse.parseAsString)
        responseHandler.handle(field => (jValue \ field).extractOpt[String])
      } finally {
        httpResponse.disconnect()
      }
    }.recover {
      case e: PaymentException => throw e
      case e: Exception => throw PaymentErrorException(e.getMessage, e)
    }
  }

  private def sign(fields: Map[String, String]): Map[String, String] = {
    val controlSumFieldValues = ControlSumFields.flatMap(fields.get).mkString

    val controlSum = hash(controlSumFieldValues, settings.secretKey)

    fields + ("control" -> controlSum)
  }

  private def hash(message: String, key: String): String = {
    val secret = new SecretKeySpec(key.getBytes(CheckSumEncoding), CheckSumHashingAlgorithm)
    val mac = javax.crypto.Mac.getInstance(CheckSumHashingAlgorithm)

    mac.init(secret)
    mac.doFinal(message.getBytes(CheckSumEncoding)).map("%02X" format _).mkString
  }
}

object DLocalGateway {
  private val CheckSumEncoding = "UTF-8"
  private val CheckSumHashingAlgorithm = "HmacSHA256"

  private val ControlSumFields = Seq(
    "x_invoice",
    "x_auth_id",
    "x_amount",
    "x_currency",
    "x_email",
    "cc_number",
    "cc_exp_month",
    "cc_cvv",
    "cc_exp_year",
    "x_cpf",
    "x_country",
    "cc_token"
  )
}
