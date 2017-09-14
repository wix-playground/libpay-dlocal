package com.wix.pay.dlocal

import org.specs2.mock.Mockito
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

import scala.util.Success

class DLocalGatewayWithLiveAndSandboxTest extends SpecWithJUnit with Mockito {

  "DLocalGatewayWithLiveAndSandbox" should {
    "redirect sale request with live credentials to live env" in new ctx {
      live.sale(liveCredentials, someCreditCard, somePayment, Some(someCustomer), Some(someDeal)) returns Success(expectedAuthKey)

      gateway.sale(liveCredentials, someCreditCard, somePayment, Some(someCustomer), Some(someDeal)) must
        beSucceedTryWith(expectedAuthKey)
    }

    "redirect sale request with sandbox credentials to sandbox env" in new ctx {
      sandbox.sale(sandboxCredentials, someCreditCard, somePayment, Some(someCustomer), Some(someDeal)) returns Success(expectedAuthKey)

      gateway.sale(sandboxCredentials, someCreditCard, somePayment, Some(someCustomer), Some(someDeal)) must
        beSucceedTryWith(expectedAuthKey)
    }

    "return Failure for invalid credentials" in new ctx {
      gateway.sale(invalidCredentials, someCreditCard, somePayment, Some(someCustomer), Some(someDeal)) must beAFailedTry
    }
  }

  trait ctx extends Scope with DLocalTestSupport {
    val live = mock[DLocalGateway]
    val sandbox = mock[DLocalGateway]

    val gateway = new DLocalGatewayWithLiveAndSandbox(live, sandbox)

    val liveCredentials = DLocalMerchant.stringify(DLocalMerchant("liveId", "liveSubCode", "liveEmail"))
    val sandboxCredentials = DLocalMerchant.stringify(DLocalMerchant("sandboxId", "sandboxSubCode", "sandboxEmail", testMode = true))
    val invalidCredentials = "aaa"

    val expectedAuthKey = "expected"
  }
}
