import com.pidorashque.htmlparsers.apachan.read._
import org.scalatest._

class ThreadParserTest extends FeatureSpec with GivenWhenThen with WebClientTest {
  feature("Parsing thread") {
    val testingThread = new {
      val url = "http://apachan.net/5154698.html"
      lazy val page = htmlPage(url)
      lazy val thread = Thread(page)
    }
    import testingThread._

    Given(s"Testing thread: $url")

    scenario("Thread with only last page loaded") {
      When("One page loaded")
      Then("Thread should contain 50 answers")
      assert(thread.posts.tail.length == 50)
    }

    scenario("Entire thread loaded") {
      When("All pages loaded")
      Then("Thread should contain 56 answers")
      assert(thread.withAllPages.posts.tail.length == 56)
    }
  }

  feature("Parsing /b/ section") {
    val url = "http://apachan.net/b.html"
    Given(s"Section: $url")
    val section = Section(htmlPage(url))
    When("One section page loaded")
    Then("Section should contain 50 posts")
    assert(section.posts.length == 50)
  }
}
