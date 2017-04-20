package com.wix.pay

import javax.crypto.spec.SecretKeySpec

import com.google.api.client.http.HttpResponse
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.{Customer, Deal, Payment}
import org.json4s.native.JsonMethods
import org.json4s.{DefaultFormats, JValue}

package object dlocal {

  private val ApiVersion = "4"
  private val ResponseContentType = "json"

  private val SuccessResponseStatus = "OK"
  private val NotPresent = "NA"

  private val CanceledTransactionStatus = "1"
  private val PendingTransactionStatus = "7"
  private val RejectedTransactionStatus = "8"
  private val ApprovedTransactionStatus = "9"
  private val AuthorizedTransactionStatus = "11"

  private val CheckSumEncoding = "UTF-8"
  private val CheckSumHashingAlgorithm = "HmacSHA256"

  private implicit val formats = DefaultFormats

  private[dlocal] trait DLocalRequest {
    def asMap: Map[String, String]
  }

  private[dlocal] trait DLocalResponseHandler {
    def handle(response: HttpResponse): String
  }

  private[dlocal] trait DLocalBaseRequest extends DLocalRequest {
    val settings: DLocalGatewaySettings

    val mandatoryFields: Map[String, String]
    val optionalFields: Map[String, Option[String]]
    val controlSumFields: Seq[String]

    val commonFields = Map(
      "x_version" -> ApiVersion,
      "type" -> ResponseContentType,
      "x_login" -> settings.login,
      "x_trans_key" -> settings.transKey
    )

    def asMap: Map[String, String] = {
      sign(commonFields ++ mandatoryFields ++ unbox(optionalFields))
    }

    private def sign(fields: Map[String, String]): Map[String, String] = {
      val controlSumFieldValues = controlSumFields.flatMap(fields.get).mkString

      val controlSum = hash(controlSumFieldValues, settings.secretKey)

      fields + ("control" -> controlSum)
    }
  }

  private[dlocal] case class DLocalSaleRequest(settings: DLocalGatewaySettings,
                                               merchant: DLocalMerchant,
                                               creditCard: CreditCard,
                                               payment: Payment,
                                               customer: Option[Customer],
                                               deal: Option[Deal]) extends DLocalBaseRequest {

    val cardPublicFields = creditCard.additionalFields.flatMap(_.publicFields)
    val billingAddress = cardPublicFields.flatMap(_.billingAddressDetailed)

    val mandatoryFields = Map(
      "x_merchant_id" -> merchant.merchantId,
      "x_sub_code" -> merchant.subCode,
      "x_invoice" -> deal.flatMap(_.invoiceId) || failWithMissingField("Invoice Id"),
      "x_amount" -> payment.currencyAmount.amount.toString,
      "x_currency" -> payment.currencyAmount.currency,
      "x_description" -> deal.flatMap(_.description) || failWithMissingField("Deal Description"),
      "x_country" -> billingAddress.flatMap(_.countryCode).map(_.getCountry) || failWithMissingField("Billing Country"),
      "x_cpf" -> cardPublicFields.flatMap(_.holderId) || failWithMissingField("Card Holder Id"),
      "x_name" -> creditCard.holderName || failWithMissingField("Card Holder Name"),
      "x_email" -> customer.flatMap(_.email) || failWithMissingField("Customer Email"),
      "cc_number" -> creditCard.number,
      "cc_exp_month" -> creditCard.expiration.month.toString,
      "cc_exp_year" -> creditCard.expiration.year.toString,
      "cc_cvv" -> creditCard.csc || failWithMissingField("Card CSC")
    )

    val optionalFields = Map(
      "cc_installments" -> Some(payment.installments).map(_.toString),
      "x_ip" -> customer.flatMap(_.ipAddress),
      "x_address" -> billingAddress.map(_.composedAddress),
      "x_zip" -> billingAddress.flatMap(_.postalCode),
      "x_city" -> billingAddress.flatMap(_.city),
      "x_state" -> billingAddress.flatMap(_.state),
      "x_phone" -> customer.flatMap(_.phone)
    )

    val controlSumFields = Seq(
      "x_invoice",
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

  private[dlocal] case class DLocalCaptureRequest(settings: DLocalGatewaySettings,
                                                  authorization: DLocalAuthorization,
                                                  amount: Double) extends DLocalBaseRequest {
    val mandatoryFields: Map[String, String] = Map(
      "x_invoice" -> authorization.invoiceId,
      "x_currency" -> authorization.currency,
      "x_auth_id" -> authorization.authId
    )
    val optionalFields: Map[String, Option[String]] = Map(
      "x_amount" -> Some(amount).map(_.toString)
    )
    val controlSumFields: Seq[String] = Seq(
      "x_invoice",
      "x_auth_id",
      "x_amount",
      "x_currency"
    )
  }

  private[dlocal] case class DLocalVoidAuthorizationRequest(settings: DLocalGatewaySettings,
                                                            authorization: DLocalAuthorization) extends DLocalBaseRequest {
    val mandatoryFields: Map[String, String] = Map(
      "x_invoice" -> authorization.invoiceId,
      "x_auth_id" -> authorization.authId
    )
    val optionalFields: Map[String, Option[String]] = Map.empty

    val controlSumFields: Seq[String] = Seq(
      "x_invoice",
      "x_auth_id"
    )
  }

  private[dlocal] trait DLocalBaseResponseHandler extends DLocalResponseHandler {

    def assertResponseIsOk(responseContent: JValue): Unit

    def extractResult(responseContent: JValue): String

    def handle(response: HttpResponse): String = {
      try {
        val responseContent = JsonMethods.parse(response.parseAsString())
        commonAssertResponseIsOk(responseContent)
        assertResponseIsOk(responseContent)
        extractResult(responseContent)
      } finally {
        response.disconnect()
      }
    }

    private def commonAssertResponseIsOk(responseContent: JValue): Unit = {
      if (!(responseContent \ "status").extractOpt[String].contains(SuccessResponseStatus)) {
        val errorCode = (responseContent \ "error_code").extractOpt[String].getOrElse(NotPresent)
        val description = (responseContent \ "desc").extractOpt[String].getOrElse(NotPresent)
        throw PaymentErrorException(s"Transaction failed($errorCode): $description")
      }

      if ((responseContent \ "result").extractOpt[String].contains(RejectedTransactionStatus)) {
        val description = (responseContent \ "desc").extractOpt[String].getOrElse(NotPresent)
        throw PaymentRejectedException(description)
      }

      if ((responseContent \ "result").extractOpt[String].contains(PendingTransactionStatus)) {
        throw PaymentErrorException("Pending transactions are not supported")
      }
    }
  }

  private[dlocal] object DLocalSaleResponseHandler extends DLocalBaseResponseHandler {
    def assertResponseIsOk(responseContent: JValue): Unit = {
      if (!(responseContent \ "result").extractOpt[String].contains(ApprovedTransactionStatus)) {
        val result = (responseContent \ "result").extractOpt[String].getOrElse(NotPresent)
        val description = (responseContent \ "desc").extractOpt[String].getOrElse(NotPresent)
        throw PaymentErrorException(s"Transaction is not approved($result): $description")
      }
    }

    def extractResult(responseContent: JValue): String = {
      extractOrFail(responseContent)("x_document")
    }
  }

  private[dlocal] object DLocalAuthorizeResponseHandler extends DLocalBaseResponseHandler {
    def assertResponseIsOk(responseContent: JValue): Unit = {
      if (!(responseContent \ "result").extractOpt[String].contains(AuthorizedTransactionStatus)) {
        val result = (responseContent \ "result").extractOpt[String].getOrElse(NotPresent)
        val description = (responseContent \ "desc").extractOpt[String].getOrElse(NotPresent)
        throw PaymentErrorException(s"Transaction is not authorized($result): $description")
      }
    }

    def extractResult(responseContent: JValue): String = {
      def value = extractOrFail(responseContent) _

      val authorization = DLocalAuthorization(value("x_auth_id"), value("x_invoice"), value("x_currency"))
      JsonDLocalAuthorizationParser.stringify(authorization)
    }
  }

  private[dlocal] object DLocalCaptureResponseHandler extends DLocalBaseResponseHandler {
    def assertResponseIsOk(responseContent: JValue): Unit = {
      if (!(responseContent \ "result").extractOpt[String].contains(ApprovedTransactionStatus)) {
        val result = (responseContent \ "result").extractOpt[String].getOrElse(NotPresent)
        val description = (responseContent \ "desc").extractOpt[String].getOrElse(NotPresent)
        throw PaymentErrorException(s"Transaction is not approved($result): $description")
      }
    }

    def extractResult(responseContent: JValue): String = {
      extractOrFail(responseContent)("x_document")
    }
  }

  private[dlocal] object DLocalVoidAuthorizationResponseHandler extends DLocalBaseResponseHandler {
    def assertResponseIsOk(responseContent: JValue): Unit = {
      if (!(responseContent \ "result").extractOpt[String].contains(CanceledTransactionStatus)) {
        val result = (responseContent \ "result").extractOpt[String].getOrElse(NotPresent)
        val description = (responseContent \ "desc").extractOpt[String].getOrElse(NotPresent)
        throw PaymentErrorException(s"Transaction is not canceled($result): $description")
      }
    }

    def extractResult(responseContent: JValue): String = {
      extractOrFail(responseContent)("x_auth_id")
    }
  }

  private def unbox[T](request: Map[String, Option[T]]): Map[String, T] = request.filter(_._2.isDefined).mapValues(_.get)

  private def hash(message: String, key: String): String = {
    val secret = new SecretKeySpec(key.getBytes(CheckSumEncoding), CheckSumHashingAlgorithm)
    val mac = javax.crypto.Mac.getInstance(CheckSumHashingAlgorithm)

    mac.init(secret)
    mac.doFinal(message.getBytes(CheckSumEncoding)).map("%02X" format _).mkString
  }

  private implicit class `(String, Option[String]) --> (String, String)`(entry: (String, Option[String])) {
    def ||(default: => String): (String, String) = {
      (entry._1, entry._2.getOrElse(default))
    }
  }

  private def failWithMissingField(fieldName: String): String = {
    throw new IllegalArgumentException(s"'$fieldName' must be given")
  }

  private def extractOrFail(responseContent: JValue)(field: String): String = {
    (responseContent \ field).extractOpt[String].getOrElse {
      throw PaymentErrorException(s"No $field in response")
    }
  }
}
