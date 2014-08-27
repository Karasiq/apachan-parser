import com.pidorashque.htmlparsers.apachan.write.PostSender
import org.scalatest._

class PostSenderTest extends FeatureSpec with GivenWhenThen with WebClientTest {
  feature("Post sending") {
    val testingThread = new {
      val url = "http://apachan.net/5255956.html"
      lazy val page = htmlPage(url)
      lazy val postSender = PostSender(page)
      lazy val captchaPage = postSender.sendPost("TEST")
      lazy val testingPost = captchaPage.post
    }
    import testingThread._
    Given(s"Testing thread: $url")

    scenario("Post submitted") {
      When("Captcha appeared")
      Then("It should contain 9 answers")
      info(s"Variants: ${captchaPage.variants}")
      assert(captchaPage.variants.size == 9)
    }

    scenario("Deleting testing post") {
      Given(s"Testing post ID: ${testingPost.get.id}")
      Then("Deleting post")
      assert(testingPost.exists(_.deletePost()))
    }
  }
}
