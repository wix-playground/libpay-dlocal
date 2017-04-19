package com.wix.pay

import javax.crypto.spec.SecretKeySpec

import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.{Customer, Deal, Payment}

package object dlocal {

  private[dlocal] trait DLocalRequest {
    val settings: DLocalGatewaySettings

    val mandatoryFields: Map[String, String]
    val optionalFields: Map[String, Option[String]]
    val controlSumFields: Seq[String]

    val commonFields = Map(
      "x_version" -> "4",
      "type" -> "json",
      "x_login" -> settings.login,
      "x_trans_key" -> settings.transKey
    )

    def asMap: Map[String, String] = {
      sign(commonFields ++ mandatoryFields ++ unbox(optionalFields))
    }

    private def sign(allFields: Map[String, String]): Map[String, String] = {
      val controlSumFieldValues = controlSumFields.flatMap(allFields.get).mkString

      val controlSum = hmac(controlSumFieldValues, settings.secretKey)

      allFields + ("control" -> controlSum)
    }
  }

  private[dlocal] case class DLocalSaleRequest(settings: DLocalGatewaySettings,
                                               merchant: DLocalMerchant,
                                               creditCard: CreditCard,
                                               payment: Payment,
                                               customer: Option[Customer],
                                               deal: Option[Deal]) extends DLocalRequest {

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

    val controlSumFields = Seq( // TODO think about dsl to mark mandatory fields to be used for checksum
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

  private def unbox[T](request: Map[String, Option[T]]): Map[String, T] = request.filter(_._2.isDefined).mapValues(_.get)

  private def hmac(message: String, key: String): String = {
    val secret = new SecretKeySpec(key.getBytes("UTF-8"), "HmacSHA256")
    val mac = javax.crypto.Mac.getInstance("HmacSHA256")

    mac.init(secret)
    mac.doFinal(message.getBytes("UTF-8")).map("%02X" format _).mkString
  }

  private def failWithMissingField(fieldName: String): String = {
    throw new IllegalArgumentException(s"'$fieldName' must be given")
  }

  private implicit class `(String, Option[String]) --> (String, String)`(entry: (String, Option[String])) {
    def ||(default: => String): (String, String) = {
      (entry._1, entry._2.getOrElse(default))
    }
  }

}


