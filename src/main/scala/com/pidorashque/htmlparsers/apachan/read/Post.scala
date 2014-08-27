package com.pidorashque.htmlparsers.apachan.read

import java.util.Locale

import com.gargoylesoftware.htmlunit.html._
import com.pidorashque.common.StringUtils
import com.pidorashque.networkutils.HtmlUnitUtils._
import org.apache.commons.lang3.StringEscapeUtils
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.jsoup.Jsoup
import org.jsoup.safety.Whitelist

object Post {
  private[read] def removeTags(s: String) = StringEscapeUtils.unescapeHtml4(Jsoup.clean(s, Whitelist.basic()))
  def apply(postTableBody: HtmlTableBody) = new PostImpl(postTableBody)
}

abstract class Post {
  private[apachan] def postAnchor: HtmlAnchor
  private[apachan] def postHtmlPage: HtmlPage = {
    import com.pidorashque.networkutils.HtmlUnitUtils._
    postAnchor.webClient
      .flatMap(_.htmlPageOption(postAnchor.fullHref)).get
  }

  def id: Long
  def date: DateTime
  def rating: Int
  def thumb: Option[HtmlImage]
  private[apachan] def fullImageLink: Option[HtmlAnchor]
  def fullImage: Option[HtmlImage] = fullImageLink.flatMap(a => {
   val page = a.click[HtmlPage]()
   page.byXPath[HtmlImage]("//div[@id='image_div']/a/img").headOption
  })
  def title: String

  /**
  * Message in HTML
  */
  def text: String

  def answersCount: Int
  def answerToId: Option[Long]

  private[apachan] def plusButton: Option[HtmlAnchor]
  private[apachan] def minusButton: Option[HtmlAnchor]
  private[apachan] def deletionButton: Option[HtmlAnchor]

  def ratePost(rate: Int): Boolean = {
    val rateButton = if (rate == 1) plusButton else minusButton
    rateButton
      .filter(_.webClient.exists(_.getOptions.isJavaScriptEnabled))
      .map(btn => btn.click[HtmlPage]()).nonEmpty
  }

  def deletePost(): Boolean = {
    deletionButton.flatMap(btn => {
      val webClient = btn.webClient.get
      webClient.withJavaScript()(_.withConfirmAllHandler { _ =>
        deletionButton.map(_.click[HtmlPage]())
      })
    }).exists(_.asText().contains("Удалено"))
  }

  override def equals(obj: scala.Any): Boolean = obj match {
   case p: Post => p.id == this.id
   case _ => false
  }

  override def hashCode(): Int = id.hashCode()

  override def toString: String = {
   s"Post($id, $title, $text)"
  }
}

class PostImpl(postTableBody: HtmlTableBody) extends Post {
  private val firstTrTd = postTableBody \ classOf[HtmlTableRow] \ classOf[HtmlTableDataCell]
  private val thirdTrTd = postTableBody \\ classOf[HtmlTableRow] #\ 2 \ classOf[HtmlTableDataCell]

  override private[apachan] val postAnchor: HtmlAnchor = (thirdTrTd \\ classOf[HtmlAnchor] *\ (!_.getHrefAttribute.startsWith("javascript"))).element.get

  override val date: DateTime = thirdTrTd.fold(DateTime.now())(td => {
    val text = StringUtils.htmlTrim(td.asText().split("\\[#[\\d]+\\] ").last.split("(?<=[\\d]{2}:[\\d]{2}) ").head)
    DateTimeFormat.forPattern("dd MMM,yy HH:mm").withLocale(Locale.ENGLISH).parseDateTime(text)
  })

  override val text: String = (firstTrTd \ classOf[HtmlDivision]).fold("")(div => Post.removeTags(div.asXml()))

  override private[apachan] val plusButton: Option[HtmlAnchor] = thirdTrTd \\ classOf[HtmlAnchor] *\ (_.getId.startsWith("vt1_"))

  override private[apachan] val minusButton: Option[HtmlAnchor] = thirdTrTd \\ classOf[HtmlAnchor] *\ (_.getId.startsWith("vt2_"))

  override private[apachan] val deletionButton: Option[HtmlAnchor] = thirdTrTd \\ classOf[HtmlAnchor] *\ (_.getTextContent == "[удалить]")

  override val rating: Int = thirdTrTd.flatMap(e => "Рейтинг: ([\\d]+)".r.findFirstMatchIn(e.getTextContent)).fold(0)(_.group(1).toInt)

  override val title: String = (firstTrTd \ classOf[HtmlBold]).fold("")(_.asText())

  override private[apachan] val fullImageLink: Option[HtmlAnchor] = firstTrTd \ classOf[HtmlDivision] \ classOf[HtmlAnchor]

  override val thumb: Option[HtmlImage] = (fullImageLink \ classOf[HtmlImage]).orElse(firstTrTd \ classOf[HtmlDivision] \ classOf[HtmlImage])

  override val id: Long = "[\\d]+".r.findFirstIn(postAnchor.getTextContent).fold(0.asInstanceOf[Long])(_.toLong)

  override val answersCount: Int =
    (thirdTrTd \ classOf[HtmlBold]).flatMap(b => "[\\d]+".r.findFirstIn(b.getTextContent)).fold(0)(_.toInt)

  override val answerToId: Option[Long] = {
    (thirdTrTd \\ classOf[HtmlAnchor] *\ (_.getTextContent == "на что?"))
      .flatMap(a => "[\\d]+".r.findFirstIn(a.getHrefAttribute).map(_.toLong))
  }
}