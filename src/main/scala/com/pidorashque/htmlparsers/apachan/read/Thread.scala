package com.pidorashque.htmlparsers.apachan.read

import com.gargoylesoftware.htmlunit.html._
import com.pidorashque.networkutils.HtmlUnitUtils._

abstract class Thread {
  def posts: IndexedSeq[Post]
  def originalPost: Option[Post] = posts.headOption

  /**
   * Finds post that post `post` answered to or returns OP-post
   * @param post Post
   * @return `post` answered to post or [[originalPost]]
   */
  def answerTo(post: Post): Option[Post] =
    if (post.answerToId.nonEmpty) post.answerToId.flatMap(id => posts.find(_.id == id))
    else originalPost filter (_ != post)

  /**
   * Finds answers to post `post`
   * @param post Post
   * @return Answers
   */
  def answers(post: Post): Seq[Post] =
    posts.filter(p => answerTo(p).contains(post))
}

trait PagedThread { this: Thread =>
  assert(pages.nonEmpty, "Cannot create empty thread")

  def pages: Set[ThreadPage]

  def pagesCount = pages.head.pagesCount

  override lazy val posts: IndexedSeq[Post] = pages.flatMap(_.posts).toIndexedSeq.sortBy(_.id)

  def page(i: Int): Option[ThreadPage]

  def pagesSequence: IndexedSeq[ThreadPage] = {
    pages.foldLeft(1 -> IndexedSeq(firstPage))((seq, post) => {
      if (post.currentPage.contains(seq._1 + 1)) (seq._1 + 1, seq._2 ++ Some(post))
      else seq
    })._2
  }

  /**
   * Gets first page
   * @return First page of the thread
   */
  def firstPage: ThreadPage =
    page(1).getOrElse(pages.head)
}

case class ThreadImpl(pages: Set[ThreadPage]) extends Thread with PagedThread {
  private[apachan] lazy val pagination: Map[Int, HtmlAnchor] = {
    pages.toSeq
      .collect { case htmlThreadPage: HtmlThreadPage => htmlThreadPage.pagination }.flatten.toMap
  }

  /**
   * Load page with number `i`
   * @param i Page number
   * @return `i` page of the thread
   */
  override def page(i: Int): Option[ThreadPage] = pages.find(_.currentPage.contains(i)).orElse {
    pagination.get(i)
      .flatMap(p => p.webClient.flatMap(_.htmlPageOption(p.fullHref)))
      .map(ThreadPage.apply)
  }

  def fromFirstPage = copy(pages = Set(firstPage))

  /**
   * Loads page `i` and return Thread copy with this page
   * @param i Page number
   * @return Thread with page `i`
   */
  def withPage(i: Int): ThreadImpl = {
    val p = page(i)
    if (p.nonEmpty) copy(pages = this.pages + p.get) else this
  }

  def withAllPages: ThreadImpl = copy(pages = {
    this.pages ++ pagination.view.flatMap(v => page(v._1))
  })

  def sequencedThread: ThreadImpl = copy(pages = this.pagesSequence.toSet)

  override def toString: String = s"Thread(${pages.mkString(", ")}})"
}

class Section(pages: Set[ThreadPage]) extends ThreadImpl(pages) {
  /**
   * Threads not answering to other posts
   * @param th Post
   * @return `None`
   */
  override def answerTo(th: Post): Option[Post] = None

  /**
   * Loads answers to thread `th`
   * @param th Thread
   * @return Answers
   */
  override def answers(th: Post): Seq[Post] = {
    Thread(th).answers(th)
  }

  override def toString: String = s"Section(${pages.mkString(", ")}})"
}

object Thread {
  def apply(htmlPage: HtmlPage): ThreadImpl = ThreadImpl(Set(ThreadPage(htmlPage)))

  def apply(post: Post): ThreadImpl = post match {
    case htmlPost: HtmlPost => apply(htmlPost.postHtmlPage)
    case p => ThreadImpl(Set(ThreadPage(p)))
  }
}

object Section {
  def apply(htmlPage: HtmlPage): Thread = new Section(Set(ThreadPage(htmlPage)))
}
