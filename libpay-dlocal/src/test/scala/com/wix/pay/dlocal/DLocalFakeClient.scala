package com.wix.pay.dlocal

import java.util.logging.{ConsoleHandler, Level, Logger}
import javax.crypto.spec.SecretKeySpec

import com.google.api.client.http._
import com.google.api.client.http.javanet.NetHttpTransport
import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods.parse
import org.json4s.native.Serialization

import scala.collection.JavaConversions._


object DLocalFakeClient extends App {

  implicit val formats = DefaultFormats

  // login to API, reused for all merchant
  val merchantLogin = "***"
  // password to API, reused for all merchant
  val merchantPassword = "***"
  // hashing key to sign requests to API
  val merchantSecretKey = "***"

  // unique merchant id, generated on our side, INTEGER, > 0, <= 999_999_999, should be unique by api doc, may be not unique by sandbox API
  // is used to register merchant
    val subCode = s"${System.currentTimeMillis() % 100000}"
//  val subCode = 98621

  // unique merchant email, should be unique by api doc, should not be unique by api doc, must be unique by sandbox API
  // is used to register merchant
  val email = s"***+$subCode@gmail.com"

  val url = "https://sandbox.dlocal.com/api_curl/cc"
  val saleUrl = s"$url/sale"
  val authUrl = s"$url/auth"
  val captureUrl = s"$url/capture"
  val voidAuthUrl = s"$url/cancel"

  val requestFactory: HttpRequestFactory = new NetHttpTransport().createRequestFactory()

  val creditCard = CreditCard("4312522698854138", YearMonth(2020, 9),
    Some(CreditCardOptionalFields.withFields(
      csc = Some("153"),
      holderName = Some("Some name"))

    ))

  turnOnHttpClientLogging()

  // unique merchant id, generated on dLocal side during registration
//    val subMerchantId = registerSubMerchant()
//  val subMerchantId = "K-K-6114e53f-5d74-4ee6-a1bb-44f8c06b0620"
//  val subMerchantId = "555"
  // ********************************** MERCHANT REGISTRATION **********************************
  // 200: {"sub_merchant_id":"K-087abead-a51f-47db-9bf6-c8050ca63426","x_sub_code":1}
  // 422: {"result":false,"message":"Error saving submerchant"} <- email is not unique

  sale(creditCard)
  // ********************************** SALE FLOW **********************************
  // 200: {"status":"OK","desc":"approved","control":"39BD42F98E7E8D7D451C851A1D06B030D010AF93A721BDCBFCD7F4E7852E9955","result":"9","x_invoice":"Invoice1234","x_iduser":"","x_description":"new shoes","x_document":"93148032","x_amount":"10.01","x_currency":"BRL","cc_token":"","x_amount_paid":"10.01","cc_descriptor":"Wix"}
  // 200: {"status":"ERROR","desc":"Empty params x_amount","error_code":"301"}
  // 200: {"status":"ERROR","desc":"Invalid params x_login","error_code":"300"}
  // 200: {"status":"ERROR","desc":"Invalid credentials","error_code":"401"}
  // 200: {"status":"ERROR","desc":"Invalid control string","error_code":"302"}
  // 200: {"status":"OK","desc":"cc_rejected_other_reason","control":"B1A4A98CFEF0765766F9A9B0A22EE5F1EA5F27BA186B28DA8BB2D2C68F142A83","result":"8","x_invoice":"Invoice1234","x_iduser":"","x_description":"new shoes","x_document":"93148033","x_amount":"10.01","x_currency":"BRL","cc_token":"","x_amount_paid":"10.01","cc_descriptor":"Wix"}
  // PEND => 200: {"status":"OK","desc":"in_process","control":"C3F319FF83E069ABF01D2C5AA40959B288EED1D3851D3D9B7DE1ACE2821399EA","result":"7","x_invoice":"Invoice1234","x_iduser":"","x_description":"new shoes","x_document":"93148035","x_amount":"10.01","x_currency":"BRL","cc_token":"","x_amount_paid":"10.01","cc_descriptor":"Wix"}
  // CONT => 200: {"status":"OK","desc":"in_process","control":"DD8436FAD9BF9D5919D78FE05ACA5D4EB48D6FDE470EFDA69EC1ACF2D25BA3A7","result":"7","x_invoice":"Invoice1234","x_iduser":"","x_description":"new shoes","x_document":"93148036","x_amount":"10.01","x_currency":"BRL","cc_token":"","x_amount_paid":"10.01","cc_descriptor":"Wix"}
  // REJE => 200: {"status":"OK","desc":"cc_rejected_other_reason","control":"9AD110E1FAEAAF093EFEE32ECD30552A90C3994DC4CB125B627F36318E2DF03C","result":"8","x_invoice":"Invoice1234","x_iduser":"","x_description":"new shoes","x_document":"93148034","x_amount":"10.01","x_currency":"BRL","cc_token":"","x_amount_paid":"10.01","cc_descriptor":"Wix"}
  // CALL => 200: {"status":"OK","desc":"cc_rejected_call_for_authorize","control":"D771A47D2E4442A5485164E9779276D487430FA38C58290372D486428CEBD445","result":"8","x_invoice":"Invoice1234","x_iduser":"","x_description":"new shoes","x_document":"93148037","x_amount":"10.01","x_currency":"BRL","cc_token":"","x_amount_paid":"10.01","cc_descriptor":"Wix"}
  // FUND => 200: {"status":"OK","desc":"cc_rejected_insufficient_amount","control":"31BCD8EAA5BDF54D0F5D85D79446747E8AD6F3D5876C03F159D1EF71DD330ECA","result":"8","x_invoice":"Invoice1234","x_iduser":"","x_description":"new shoes","x_document":"93148038","x_amount":"10.01","x_currency":"BRL","cc_token":"","x_amount_paid":"10.01","cc_descriptor":"Wix"}


  //  authorize(creditCard)
  // ********************************** AUTH FLOW **********************************
  // 200: {"status":"OK","desc":"authorized","control":"AF47C6B4C072FF4EF60E8924371EB0CFCB555B51C992333A45B8A3856467AFE3","result":"11","x_invoice":"Invoice1234","x_iduser":"","x_description":"","x_auth_id":"93161769","x_amount":"10.01","x_currency":"BRL","x_amount_paid":"10.01","cc_descriptor":"Wix"}
  // 200: {"status":"ERROR","desc":"Empty params x_currency","error_code":"301"}

  // capture(authorize(creditCard))
  // ********************************** AUTH/CAPTURE FLOW **********************************
  // 200: {"status":"OK","desc":"approved","control":"6758AC25DCF51413DEC114DA67CEAC587CAB9ADAED439B6BEB0E36D685391BA1","result":"9","x_invoice":"Invoice1234","x_iduser":"","x_description":"","x_auth_id":"93161775","x_amount":"10.01","x_currency":"BRL","x_amount_captured":"10.01","x_document":"93161776"}
  // 200: {"status":"ERROR","desc":"Empty params x_auth_id","error_code":"301"}


  //  voidAuthorization(authorize(creditCard))
  // ********************************** AUTH/CANCEL FLOWS **********************************
  // 200: {"status":"OK","desc":"cancelled","control":"C602BF0BBECCA7BADD33DBAEABA9392BC95AAF06C0D1D040FED39A546CE22E29","result":"1","x_invoice":"Invoice1234","x_auth_id":"93161777","x_currency":"","x_amount_canceled":""}
  // 200: {"status":"ERROR","desc":"Empty params x_invoice","error_code":"301"}


  def registerSubMerchant(): String = {
    val content = Map(
      "x_login" -> merchantLogin,
      "x_trans_key" -> merchantPassword,
      "x_sub_code" -> subCode,
      "x_email" -> email,
      "x_name" -> "Taras Inc."
    )

    val response = postJson("https://sandbox.dlocal.com/api_curl/submerchants/sub", content)
    (response \ "sub_merchant_id").extract[String]
  }

  def sale(creditCard: CreditCard): Unit = {
    val content = Map(
      "x_login" -> merchantLogin,
      "x_trans_key" -> merchantPassword,
      "x_version" -> "4",
      "x_invoice" -> "NA",
      "x_amount" -> "10.01",
      "x_currency" -> "BRL",
      //      "x_description" -> "new shoes", // sandbox does not require it
      //      "x_device_id" -> "54hj4h5jh46hasjd", // sandbox does not require it
      "x_country" -> "BR",
      "x_cpf" -> "00003456789",
      "x_name" -> creditCard.holderName.get,
      "x_email" -> "testing2@dlocal.com",
      "cc_number" -> creditCard.number,
      "cc_exp_month" -> creditCard.expiration.month,
      "cc_exp_year" -> creditCard.expiration.year,
      "cc_cvv" -> creditCard.csc.get,

      // "x_merchant_id" -> subMerchantId, // merchant id is not needed, dLocal correlate merchant by sub code
      "x_sub_code" -> subCode,

      "type" -> "json"
    )
    postUrlEncoding(saleUrl, content)
  }

  def authorize(creditCard: CreditCard): DLocalAuthorization = {
    val content = Map(
      "x_login" -> merchantLogin,
      "x_trans_key" -> merchantPassword,
      "x_version" -> "4",
      "x_invoice" -> "Invoice1234",
      "x_amount" -> "10.01",
      "x_currency" -> "BRL",
      //      "x_description" -> "new shoes", // sandbox does not require it
      //      "x_device_id" -> "54hj4h5jh46hasjd", // sandbox does not require it
      "x_country" -> "BR",
      "x_cpf" -> "00003456789",
      "x_name" -> creditCard.holderName.get,
      "x_email" -> "testing2@dlocal.com",
      "cc_number" -> creditCard.number,
      "cc_exp_month" -> creditCard.expiration.month,
      "cc_exp_year" -> creditCard.expiration.year,
      "cc_cvv" -> creditCard.csc.get,

      // "x_merchant_id" -> subMerchantId, // merchant id is not needed, dLocal correlate merchant by sub code
      "x_sub_code" -> subCode,

      "type" -> "json"
    )
    val response = postUrlEncoding(authUrl, content)

    DLocalAuthorization(
      authId = (response \ "x_auth_id").extract[String],
      invoiceId = (response \ "x_invoice").extract[String],
      currency = (response \ "x_currency").extract[String]
    )
  }

  def capture(authorization: DLocalAuthorization): Unit = {
    val content = Map(
      "x_login" -> merchantLogin,
      "x_trans_key" -> merchantPassword,
      "x_version" -> "4",
      "x_invoice" -> authorization.invoiceId,
      //      "x_auth_id" -> authorization.authId,
      "x_amount" -> "10.01",
      "x_currency" -> authorization.currency,
      "type" -> "json" //accept does not work...
    )
    postUrlEncoding(captureUrl, content)
  }

  def voidAuthorization(authorization: DLocalAuthorization): Unit = {
    val content = Map(
      "x_login" -> merchantLogin,
      "x_trans_key" -> merchantPassword,
      "x_version" -> "4",
      "x_invoice" -> authorization.invoiceId,
      "x_auth_id" -> authorization.authId,
      "type" -> "json" //accept does not work...
    )
    postUrlEncoding(voidAuthUrl, content)
  }

  private def postUrlEncoding(url: String, content: Map[String, Any]) = {
    val request = requestFactory.buildPostRequest(
      new GenericUrl(url),
      new UrlEncodedContent(mapAsJavaMap(contentWithControl(content)))
    )
    execute(request)
  }

  private def postJson(url: String, content: Map[String, Any]) = {
    val request = requestFactory.buildPostRequest(
      new GenericUrl(url),
      new ByteArrayContent("application/json;charset=utf-8", Serialization.write(content).getBytes("UTF-8"))
    )
    execute(request)
  }

  private def execute(request: HttpRequest) = {
    val response = request
      .setHeaders(new HttpHeaders().setAccept("application/json"))
      .execute()
    val str = response.parseAsString()
    parse(str)
  }

  private def contentWithControl(content: Map[String, Any]): Map[String, Any] = {
    val message = Seq("x_invoice", "x_auth_id", "x_amount", "x_currency", "x_email", "cc_number", "cc_exp_month", "cc_cvv", "cc_exp_year", "x_cpf", "x_country", "cc_token")
      .flatMap(content.get)
      .mkString

    val control = hmac(message, merchantSecretKey)

    content + ("control" -> control.toUpperCase)
  }

  def hmac(message: String, key: String): String = {
    val secret = new SecretKeySpec(key.getBytes("UTF-8"), "HmacSHA256")

    val mac = javax.crypto.Mac.getInstance("HmacSHA256")

    mac.init(secret)
    mac.doFinal(message.getBytes("UTF-8"))
      .map("%02X" format _)
      .mkString
  }

  def turnOnHttpClientLogging(): Unit = {
    val consoleHandler = new ConsoleHandler()
    consoleHandler.setLevel(Level.CONFIG)

    val logger = Logger.getLogger(classOf[HttpTransport].getName)
    logger.addHandler(consoleHandler)
    logger.setLevel(Level.CONFIG)
  }
}
