package com.wix.pay.dlocal

import com.wix.pay.PaymentGateway
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.{Customer, Deal, Payment}

import scala.util.Try

class DLocalGatewayWithLiveAndSandbox(live: DLocalGateway, sandbox: DLocalGateway) extends PaymentGateway {
  def this(settings: DLocalSettings) = this(new DLocalGateway(settings.live), new DLocalGateway(settings.sandbox))

  override def sale(merchantKey: String, creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal]): Try[String] =
    withEnvironment(merchantKey) { gateway =>
      gateway.sale(merchantKey, creditCard, payment, customer, deal)
    }

  override def authorize(merchantKey: String, creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal]): Try[String] =
    withEnvironment(merchantKey) { gateway =>
      gateway.authorize(merchantKey, creditCard, payment, customer, deal)
    }

  override def capture(merchantKey: String, authorizationKey: String, amount: Double): Try[String] =
    withEnvironment(merchantKey) { gateway =>
      gateway.capture(merchantKey, authorizationKey, amount)
    }


  override def voidAuthorization(merchantKey: String, authorizationKey: String): Try[String] =
    withEnvironment(merchantKey) { gateway =>
      gateway.voidAuthorization(merchantKey, authorizationKey)
    }

  private def withEnvironment[T](credentials: String)(body: DLocalGateway => Try[T]): Try[T] = {
    Try {
      val merchant = DLocalMerchant.parse(credentials)
      if (merchant.testMode) sandbox else live
    } flatMap body
  }
}
