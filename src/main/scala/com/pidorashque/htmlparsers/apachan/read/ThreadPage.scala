package com.pidorashque.htmlparsers.apachan.read

import com.gargoylesoftware.htmlunit.html._
import com.pidorashque.networkutils.HtmlUnitUtils._



abstract class ThreadPage {
  def posts: Seq[Post]

  def currentPage: Option[Int]

  def pagination: Map[Int, HtmlAnchor]

  def pagesCount: Int = if (currentPage.nonEmpty) pagination.size + 1 else pagination.size

  override def hashCode(): Int = posts.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case t: ThreadPage => posts.equals(t.posts)
    case _ => false
  }

  override def toString: String = s"ThreadPage(${posts.mkString(", ")})"
}


object ThreadPage {
  /**
   * Parses thread HTML page
   * @param htmlPage HTML page
   * @return Parsed thread page
   */
  def apply(htmlPage: HtmlPage): ThreadPage = new ThreadPage {
    override val posts: Seq[Post] = {
      val p = htmlPage.byXPath[HtmlTableBody]("/html/body/span[1]/table[@width='780']/tbody|/html/body/table[@width='780']/tbody")
      p.map(Post.apply)
    }

    private def getPageNumber(a: HtmlElement): Int = "#([\\d]+)".r.findFirstMatchIn(a.getTextContent).fold(0)(_.group(1).toInt)

    override val pagination: Map[Int, HtmlAnchor] =
      htmlPage.byXPath[HtmlAnchor]("/html/body/span[1]/center/a")
        .map(a => getPageNumber(a) -> a)
        .toMap

    override val currentPage: Option[Int] =
      htmlPage.byXPath[HtmlFont]("/html/body/span[1]/center/font").headOption
        .map(getPageNumber)
  }
}
