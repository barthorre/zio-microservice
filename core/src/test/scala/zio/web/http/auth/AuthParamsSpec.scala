package zio.web.http.auth

import zio.test.{ DefaultRunnableSpec, Gen, assert, check }
import zio.test.Assertion.{ equalTo, isNone, isSome, not }
import zio.web.http.auth.BasicAuth.{ AuthParams, Sensitive }

object AuthParamsSpec extends DefaultRunnableSpec {

  def spec = suite("AuthParamsSpec")(
    testM("return None when header is malformed") {
      check(Gen.alphaNumericString) { header =>
        val r1 = AuthParams.create("realm", header)
        val r2 = AuthParams.create("realm", "")
        val r3 = AuthParams.create("realm", encodeToString(header))

        assert(r1)(isNone) &&
        assert(r2)(isNone) &&
        assert(r3)(isNone)
      }
    },
    testM("parse a well formed header") {
      check(Gen.alphaNumericStringBounded(1, 5), Gen.alphaNumericStringBounded(1, 5)) { (user, password) =>
        val realm  = "realm"
        val result = AuthParams.create(realm, headerValue(user, password))

        assert(result.map(_.realm))(isSome(equalTo(realm))) &&
        assert(result.map(_.user))(isSome(equalTo(user))) &&
        assert(result.map(_.password.get))(isSome(equalTo(password)))
      }
    },
    testM("Sensitive does not show value") {
      check(Gen.alphaNumericStringBounded(1, 5)) { password =>
        val sensitive = Sensitive(password)
        assert(sensitive.toString)(not(equalTo(password))) &&
        assert(sensitive.get)(equalTo(password)) &&
        assert(sensitive.value.toString)(not(equalTo(password)))
      }
    }
  )

  private[web] def authHeader(user: String, password: String) =
    "Authorization" -> headerValue(user, password)

  private def headerValue(user: String, password: String) = s"Basic ${encodeToString(s"$user:$password")}"

  private def encodeToString(s: String) = java.util.Base64.getEncoder.encodeToString(s.getBytes())
}
