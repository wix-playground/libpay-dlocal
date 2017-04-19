package com.wix.pay.dlocal

import java.util.UUID
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

  val merchantLogin = "***"
  val merchantPassword = "***"
  val merchantSecretKey = "***"

  val url = "https://sandbox.dlocal.com/api_curl/cc"
//  val url = "https://sandbox.astropaycard.com/api_curl/cc"
  val saleUrl = s"$url/sale"

  val requestFactory: HttpRequestFactory = new NetHttpTransport().createRequestFactory()

  val creditCard = CreditCard("4111111111111111", YearMonth(2017, 12),
    Some(CreditCardOptionalFields.withFields(
      csc = Some("111"),
      holderName = Some("Pedro"))

    ))

  turnOnHttpClientLogging()
  val subMerchantId = registerSubMerchant()

//  sale(creditCard)

  //CONFIG: {"status":"OK","desc":"approved","control":"39BD42F98E7E8D7D451C851A1D06B030D010AF93A721BDCBFCD7F4E7852E9955","result":"9","x_invoice":"Invoice1234","x_iduser":"","x_description":"new shoes","x_document":"93148032","x_amount":"10.01","x_currency":"BRL","cc_token":"","x_amount_paid":"10.01","cc_descriptor":"Wix"}
  //CONFIG: {"status":"ERROR","desc":"Empty params x_amount","error_code":"301"}
  //CONFIG: {"status":"ERROR","desc":"Invalid params x_login","error_code":"300"}
  //CONFIG: {"status":"ERROR","desc":"Invalid credentials","error_code":"401"}
  //CONFIG: {"status":"ERROR","desc":"Invalid control string","error_code":"302"}
  //CONFIG: {"status":"OK","desc":"cc_rejected_other_reason","control":"B1A4A98CFEF0765766F9A9B0A22EE5F1EA5F27BA186B28DA8BB2D2C68F142A83","result":"8","x_invoice":"Invoice1234","x_iduser":"","x_description":"new shoes","x_document":"93148033","x_amount":"10.01","x_currency":"BRL","cc_token":"","x_amount_paid":"10.01","cc_descriptor":"Wix"}

  //PEND => CONFIG: {"status":"OK","desc":"in_process","control":"C3F319FF83E069ABF01D2C5AA40959B288EED1D3851D3D9B7DE1ACE2821399EA","result":"7","x_invoice":"Invoice1234","x_iduser":"","x_description":"new shoes","x_document":"93148035","x_amount":"10.01","x_currency":"BRL","cc_token":"","x_amount_paid":"10.01","cc_descriptor":"Wix"}
  //CONT => CONFIG: {"status":"OK","desc":"in_process","control":"DD8436FAD9BF9D5919D78FE05ACA5D4EB48D6FDE470EFDA69EC1ACF2D25BA3A7","result":"7","x_invoice":"Invoice1234","x_iduser":"","x_description":"new shoes","x_document":"93148036","x_amount":"10.01","x_currency":"BRL","cc_token":"","x_amount_paid":"10.01","cc_descriptor":"Wix"}
  //REJE => CONFIG: {"status":"OK","desc":"cc_rejected_other_reason","control":"9AD110E1FAEAAF093EFEE32ECD30552A90C3994DC4CB125B627F36318E2DF03C","result":"8","x_invoice":"Invoice1234","x_iduser":"","x_description":"new shoes","x_document":"93148034","x_amount":"10.01","x_currency":"BRL","cc_token":"","x_amount_paid":"10.01","cc_descriptor":"Wix"}
  //CALL => CONFIG: {"status":"OK","desc":"cc_rejected_call_for_authorize","control":"D771A47D2E4442A5485164E9779276D487430FA38C58290372D486428CEBD445","result":"8","x_invoice":"Invoice1234","x_iduser":"","x_description":"new shoes","x_document":"93148037","x_amount":"10.01","x_currency":"BRL","cc_token":"","x_amount_paid":"10.01","cc_descriptor":"Wix"}
  //FUND => CONFIG: {"status":"OK","desc":"cc_rejected_insufficient_amount","control":"31BCD8EAA5BDF54D0F5D85D79446747E8AD6F3D5876C03F159D1EF71DD330ECA","result":"8","x_invoice":"Invoice1234","x_iduser":"","x_description":"new shoes","x_document":"93148038","x_amount":"10.01","x_currency":"BRL","cc_token":"","x_amount_paid":"10.01","cc_descriptor":"Wix"}


  def registerSubMerchant():String = {
      val content = Map(
        "x_login" -> merchantLogin,
        "x_trans_key" -> merchantPassword,
        "x_sub_code" -> UUID.randomUUID().toString,
        "x_email" -> "tarasp@wix.com",
        "x_name" -> "Taras Inc."
      )

      val result = postJson("https://sandbox.dlocal.com/submerchants/sub", content)
      ""
  }

  def sale(creditCard: CreditCard): Unit = {
    val content = Map(
      "x_login" -> merchantLogin,
      "x_trans_key" -> merchantPassword,
      "x_version" -> "4",
      "x_invoice" -> "Invoice1234",
      "x_amount" -> "10.01",
      "x_currency" -> "BRL",
      "x_description" -> "new shoes", // * was not required by responses
//      "x_device_id" -> "54hj4h5jh46hasjd", // * was not required by responses
      "x_country" -> "BR",
      "x_cpf" -> "00003456789",
      "x_name" -> creditCard.holderName.get,
      "x_email" -> "testing2@dlocal.com",
      "cc_number" -> creditCard.number,
      "cc_exp_month" -> creditCard.expiration.month,
      "cc_exp_year" -> creditCard.expiration.year,
      "cc_cvv" -> creditCard.csc.get,

      "x_merchant_id" -> "1",
      "x_sub_code" -> "2",

      "type" -> "json" //accept does not work...
//      "x_bdate" -> "19840304",
//      "x_zip" -> "0750000",
//      "x_address" -> "dLocal 1234",
//      "x_city" -> "Santa Isabel",
//      "x_state" -> "RJ",
//      "x_phone" -> "11987659876"
    )
    post(saleUrl, content)
  }

  private def post(url: String, content: Map[String, Any]) = {
    val request = requestFactory.buildPostRequest(
      new GenericUrl(url),
      new UrlEncodedContent(mapAsJavaMap(contentWithControl(content)))
    )

    val response = request
      .setHeaders(new HttpHeaders().setAccept("application/json"))
      .execute()
    val str = response.parseAsString()
    parse(str)
  }
  private def postJson(url: String, content: Map[String, Any]) = {
    val request = requestFactory.buildPostRequest(
      new GenericUrl(url),
      new ByteArrayContent("application/json;charset=utf-8", Serialization.write(content).getBytes("UTF-8"))
    )

    val response = request
      .setHeaders(new HttpHeaders().setAccept("application/json"))
      .execute()
    val str = response.parseAsString()
    println(str)
    parse(str)
  }

  private def contentWithControl(content: Map[String, Any]): Map[String, Any] = {
    //$message = $invoice.$amount.$currency.$email.$number.$month.$cvv.$year.$cpf.$country.$token;
    //$control = strtoupper(hash_hmac('sha256', pack('A*', $message), pack('A*',$secretkey)));

    val message = Seq("x_invoice", "x_amount", "x_currency", "x_email", "cc_number", "cc_exp_month", "cc_cvv", "cc_exp_year", "x_cpf", "x_country", "cc_token")
      .map(content.getOrElse(_, ""))
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
