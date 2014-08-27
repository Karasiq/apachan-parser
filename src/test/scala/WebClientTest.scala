import java.util.logging.Level

import com.gargoylesoftware.htmlunit.WebClient
import com.gargoylesoftware.htmlunit.html.HtmlPage
import com.pidorashque.networkutils.HtmlUnitUtils
import org.apache.commons.logging.LogFactory
import org.scalatest._

trait WebClientTest extends BeforeAndAfterAll { this: Suite =>
  LogFactory.getFactory.setAttribute("org.apache.commons.logging.Log", "org.apache.commons.logging.impl.NoOpLog")
  java.util.logging.Logger.getLogger("com.gargoylesoftware.htmlunit").setLevel(Level.OFF)
  java.util.logging.Logger.getLogger("org.apache.commons.httpclient").setLevel(Level.OFF)

  val webClient: WebClient = HtmlUnitUtils.newWebClient()

  def htmlPage(url: String) = webClient.getPage[HtmlPage](url)

  override protected def afterAll(): Unit = {
    webClient.closeAllWindows()
  }
}
