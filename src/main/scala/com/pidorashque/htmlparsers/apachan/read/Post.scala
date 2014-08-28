package com.pidorashque.htmlparsers.apachan.read

import java.util.Locale
import javax.imageio.ImageReader

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
  def apply(postTableBody: HtmlTableBody): Post = new HtmlPostImpl(postTableBody)
}

abstract class Post {
  def id: Long
  def date: DateTime
  def rating: Int

  def loadThumb(): Option[ImageReader]
  def loadFullImage(): Option[ImageReader]
  def deletePost(): Boolean
  def ratePost(rate: Int): Boolean

  def title: String

  /**
  * Message in HTML
  */
  def text: String

  def answersCount: Option[Int]
  def answerToId: Option[Long]

  override def equals(obj: scala.Any): Boolean = obj match {
   case p: Post => p.id == this.id
   case _ => false
  }

  override def hashCode(): Int = id.hashCode()

  override def toString: String = {
   s"Post($id, $title, $text)"
  }
}

private[apachan] trait HtmlPost { this: Post =>
  def postAnchor: HtmlAnchor
  def postHtmlPage: HtmlPage = {
    import com.pidorashque.networkutils.HtmlUnitUtils._
    postAnchor.webClient
      .flatMap(_.htmlPageOption(postAnchor.fullHref)).get
  }

  def plusButton: Option[HtmlAnchor]
  def minusButton: Option[HtmlAnchor]
  override def ratePost(rate: Int): Boolean = {
    val rateButton = if (rate == 1) plusButton else minusButton
    rateButton
      .filter(_.webClient.exists(_.getOptions.isJavaScriptEnabled))
      .map(btn => btn.click[HtmlPage]()).nonEmpty
  }

  def deletionButton: Option[HtmlAnchor]
  override def deletePost(): Boolean = {
    deletionButton.flatMap(btn => {
      val webClient = btn.webClient.get
      webClient.withJavaScript()(_.withConfirmAllHandler { _ =>
        deletionButton.map(_.click[HtmlPage]())
      })
    }).exists(_.asText().contains("Удалено"))
  }

  def fullImageLink: Option[HtmlAnchor]
  override def loadFullImage(): Option[ImageReader] = fullImageLink.flatMap(a => {
    val page = a.click[HtmlPage]()
    page.byXPath[HtmlImage]("//div[@id='image_div']/a/img").headOption.map(_.getImageReader)
  })

  def thumb: Option[HtmlImage]
  override def loadThumb(): Option[ImageReader] = {
    thumb.map(_.getImageReader)
  }
}

private[apachan] class HtmlPostImpl(postTableBody: HtmlTableBody) extends Post with HtmlPost {
  private val firstTrTd = postTableBody \ classOf[HtmlTableRow] \ classOf[HtmlTableDataCell]
  private val thirdTrTd = postTableBody \\ classOf[HtmlTableRow] #\ 2 \ classOf[HtmlTableDataCell]

  override val postAnchor: HtmlAnchor = (thirdTrTd \\ classOf[HtmlAnchor] *\ (!_.getHrefAttribute.startsWith("javascript"))).element.get

  override val date: DateTime = thirdTrTd.fold(DateTime.now())(td => {
    val text = StringUtils.htmlTrim(td.asText().split("\\[#[\\d]+\\] ").last.split("(?<=[\\d]{2}:[\\d]{2}) ").head)
    DateTimeFormat.forPattern("dd MMM,yy HH:mm").withLocale(Locale.ENGLISH).parseDateTime(text)
  })

  override val text: String = (firstTrTd \ classOf[HtmlDivision]).fold("")(div => Post.removeTags(div.asXml()))

  override val plusButton: Option[HtmlAnchor] = thirdTrTd \\ classOf[HtmlAnchor] *\ (_.getId.startsWith("vt1_"))

  override val minusButton: Option[HtmlAnchor] = thirdTrTd \\ classOf[HtmlAnchor] *\ (_.getId.startsWith("vt2_"))

  override val deletionButton: Option[HtmlAnchor] = thirdTrTd \\ classOf[HtmlAnchor] *\ (_.getTextContent == "[удалить]")

  override val rating: Int = thirdTrTd.flatMap(e => "Рейтинг: ([\\d]+)".r.findFirstMatchIn(e.getTextContent)).fold(0)(_.group(1).toInt)

  override val title: String = (firstTrTd \ classOf[HtmlBold]).fold("")(_.asText())

  override val fullImageLink: Option[HtmlAnchor] = firstTrTd \ classOf[HtmlDivision] \ classOf[HtmlAnchor]

  override val thumb: Option[HtmlImage] = (fullImageLink \ classOf[HtmlImage]).orElse(firstTrTd \ classOf[HtmlDivision] \ classOf[HtmlImage])

  override val id: Long = "[\\d]+".r.findFirstIn(postAnchor.getTextContent).fold(0.asInstanceOf[Long])(_.toLong)

  override val answersCount: Option[Int] =
    (thirdTrTd \ classOf[HtmlBold]).flatMap(b => "[\\d]+".r.findFirstIn(b.getTextContent)).map(_.toInt)

  override val answerToId: Option[Long] = {
    (thirdTrTd \\ classOf[HtmlAnchor] *\ (_.getTextContent == "на что?"))
      .flatMap(a => "[\\d]+".r.findFirstIn(a.getHrefAttribute).map(_.toLong))
  }
}