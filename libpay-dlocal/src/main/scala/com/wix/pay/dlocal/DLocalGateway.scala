package com.wix.pay.dlocal

import com.google.api.client.http._
import com.google.api.client.http.javanet.NetHttpTransport
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.{Customer, Deal, Payment}
import com.wix.pay.{PaymentErrorException, PaymentException, PaymentGateway, PaymentRejectedException}
import org.json4s.{DefaultFormats, JValue}
import org.json4s.native.JsonMethods.parse

import scala.collection.JavaConversions.mapAsJavaMap
import scala.util.Try

class DLocalGateway(settings: DLocalGatewaySettings) extends PaymentGateway {

  private implicit val formats = DefaultFormats
  private val requestFactory: HttpRequestFactory = new NetHttpTransport().createRequestFactory()

  override def sale(merchantKey: String, creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    val merchant = JsonDLocalMerchantParser.parse(merchantKey)
    val saleRequest = DLocalSaleRequest(settings, merchant, creditCard, payment, customer, deal)

    execute(
      url = s"${settings.url}/api_curl/cc/sale",
      request = saleRequest,
      extract = response => (response \ "x_document").extract[String]
    )
  }

  def execute(url: String, request: DLocalRequest, extract: JValue => String): Try[String] = Try {
    val response: HttpResponse = requestFactory.buildPostRequest(
      new GenericUrl(url),
      new UrlEncodedContent(mapAsJavaMap(request.asMap))
    ).execute()

    try {
      extract(parseAndVerify(response))
    } finally {
      response.disconnect()
    }
  }.recover {
    case e: PaymentException => throw e
    case e: Exception => throw PaymentErrorException(e.getMessage, e)
  }

  private def parseAndVerify(response: HttpResponse) = {
    val responseContent = parse(response.parseAsString())

    if (!(responseContent \ "status").extractOpt[String].contains("OK")) { // not OK
      val errorCode = (responseContent \ "error_code").extractOpt[String].getOrElse("NA")
      val description = (responseContent \ "desc").extractOpt[String].getOrElse("NA")
      throw PaymentErrorException(s"Transaction failed($errorCode): $description")
    }

    if ((responseContent \ "result").extractOpt[String].contains("8")) { // 8 - rejected
      val description = (responseContent \ "desc").extractOpt[String].getOrElse("NA")
      throw PaymentRejectedException(description)
    }

    if ((responseContent \ "result").extractOpt[String].contains("7")) { // 7 - pending
      throw PaymentErrorException("Pending transactions are not supported")
    }

    if (!(responseContent \ "result").extractOpt[String].contains("9")) { // not 9 - not approved
      val result = (responseContent \ "result").extractOpt[String].getOrElse("NA")
      val description = (responseContent \ "desc").extractOpt[String].getOrElse("NA")
      throw PaymentErrorException(s"Transaction is not approved($result): $description")
    }

    if ((responseContent \ "x_document").extractOpt[String].isEmpty) {
      throw PaymentErrorException("No x_document in response")
    }

    responseContent
  }

  override def authorize(merchantKey: String, creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal]): Try[String] = ???

  override def capture(merchantKey: String, authorizationKey: String, amount: Double): Try[String] = ???

  override def voidAuthorization(merchantKey: String, authorizationKey: String): Try[String] = ???
}
