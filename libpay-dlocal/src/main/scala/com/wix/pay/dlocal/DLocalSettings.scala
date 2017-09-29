package com.wix.pay.dlocal

case class DLocalSettings(live: DLocalGatewaySettings, sandbox: DLocalGatewaySettings)

case class DLocalGatewaySettings(url: String, apiUrl: String, login: String, transKey: String, secretKey: String)