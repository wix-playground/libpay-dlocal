package com.wix.pay

import javax.crypto.spec.SecretKeySpec

import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.{Customer, Deal, Payment}

package object dlocal {

  private[dlocal] trait DLocalRequest {
    val settings: DLocalGatewaySettings

    val mandatoryFields: Map[String, String]
    val optionalFields: Map[String, Option[String]]
    val controlFields: Seq[String]

    val commonFields = Map(
      "x_version" -> "4",
      "type" -> "json",
      "x_login" -> settings.login,
      "x_trans_key" -> settings.transKey
    )

    def asMap: Map[String, String] = {
      sign(commonFields ++ mandatoryFields ++ unbox(optionalFields))
    }

    private def sign(request: Map[String, String]): Map[String, String] = {
      val message = controlFields
        .map(field => request.get(field))
        .filter(_.isDefined)
        .map(_.get)
        .mkString

      val control = hmac(message, settings.secretKey)

      request + ("control" -> control)
    }

    def failWithMissingField(fieldName: String): String = {
      throw new IllegalArgumentException(s"'$fieldName' must be given")
    }
  }

  private[dlocal] case class DLocalSaleRequest(settings: DLocalGatewaySettings,
                                               merchant: DLocalMerchant,
                                               creditCard: CreditCard,
                                               payment: Payment,
                                               customer: Option[Customer],
                                               deal: Option[Deal]) extends DLocalRequest {
    val mandatoryFields = Map(
      "x_merchant_id" -> merchant.merchantId,
      "x_sub_code" -> merchant.subCode,

      "x_invoice" -> deal.flatMap(_.invoiceId).getOrElse(failWithMissingField("Invoice Id")),
      "x_amount" -> payment.currencyAmount.amount.toString,
      "x_currency" -> payment.currencyAmount.currency,
      "x_description" -> deal.flatMap(_.description).getOrElse(failWithMissingField("Deal Description")),
      "x_country" -> creditCard.additionalFields.flatMap(_.publicFields).flatMap(_.billingAddressDetailed).flatMap(_.countryCode).map(_.getCountry).getOrElse(failWithMissingField("Billing Country")),
      "x_cpf" -> creditCard.additionalFields.flatMap(_.publicFields).flatMap(_.holderId).getOrElse(failWithMissingField("Card Holder Id")),
      "x_name" -> creditCard.holderName.getOrElse(failWithMissingField("Card Holder Name")),
      "x_email" -> customer.flatMap(_.email).getOrElse(failWithMissingField("Customer Email")),
      "cc_number" -> creditCard.number,
      "cc_exp_month" -> creditCard.expiration.month.toString,
      "cc_exp_year" -> creditCard.expiration.year.toString,
      "cc_cvv" -> creditCard.csc.getOrElse(failWithMissingField("Card CSC"))
    )

    val optionalFields = Map(
      "cc_installments" -> Some(payment.installments).map(_.toString),
      "x_ip" -> customer.flatMap(_.ipAddress),
      "x_address" -> creditCard.additionalFields.flatMap(_.billingAddress),
      "x_zip" -> creditCard.additionalFields.flatMap(_.billingPostalCode),
      "x_city" -> creditCard.additionalFields.flatMap(_.publicFields).flatMap(_.billingAddressDetailed).flatMap(_.city),
      "x_state" -> creditCard.additionalFields.flatMap(_.publicFields).flatMap(_.billingAddressDetailed).flatMap(_.state),
      "x_phone" -> customer.flatMap(_.phone)
    )

    val controlFields = Seq(
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

  private def unbox[T](request: Map[String, Option[T]]): Map[String, T] = request
    .filter { case (_, optional) => optional.isDefined }
    .mapValues(_.get)

  private def hmac(message: String, key: String): String = {
    val secret = new SecretKeySpec(key.getBytes("UTF-8"), "HmacSHA256")
    val mac = javax.crypto.Mac.getInstance("HmacSHA256")

    mac.init(secret)
    mac.doFinal(message.getBytes("UTF-8")).map("%02X" format _).mkString
  }
}


