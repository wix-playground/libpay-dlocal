package com.wix.pay.dlocal

import com.google.api.client.http._
import com.google.api.client.http.javanet.NetHttpTransport
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.{Customer, Deal, Payment}
import com.wix.pay.{PaymentErrorException, PaymentException, PaymentGateway}
import org.json4s.native.JsonMethods.parse

import scala.collection.JavaConversions.mapAsJavaMap
import scala.util.Try

class DLocalGateway(settings: DLocalGatewaySettings) extends PaymentGateway {

  private val requestFactory: HttpRequestFactory = new NetHttpTransport().createRequestFactory()
  private val saleUrl = s"${settings.url}/api_curl/cc/sale"
  private val authorizationUrl = s"${settings.url}/api_curl/cc/auth"

  override def sale(merchantKey: String, creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    val merchant = JsonDLocalMerchantParser.parse(merchantKey)
    val saleRequest = DLocalSaleRequest(settings, merchant, creditCard, payment, customer, deal)

    execute(
      url = saleUrl,
      request = saleRequest,
      responseHandler = DLocalSaleResponseHandler
    )
  }

  override def authorize(merchantKey: String, creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    val merchant = JsonDLocalMerchantParser.parse(merchantKey)
    val authorizationRequest = DLocalSaleRequest(settings, merchant, creditCard, payment, customer, deal) // authorize requests looks the same as sale

    execute(
      url = authorizationUrl,
      request = authorizationRequest,
      responseHandler = DLocalAuthorizeResponseHandler
    )
  }

  def execute(url: String, request: DLocalRequest, responseHandler: DLocalResponseHandler): Try[String] = Try {
    requestFactory.buildPostRequest(
      new GenericUrl(url),
      new UrlEncodedContent(mapAsJavaMap(request.asMap))
    ).execute()
  }.map {
    responseHandler.handle
  }.recover {
    case e: PaymentException => throw e
    case e: Exception => throw PaymentErrorException(e.getMessage, e)
  }

  override def capture(merchantKey: String, authorizationKey: String, amount: Double): Try[String] = ???

  override def voidAuthorization(merchantKey: String, authorizationKey: String): Try[String] = ???
}
