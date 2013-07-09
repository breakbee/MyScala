package twitter

import twitter4j.Twitter
import twitter4j.TwitterFactory
import twitter4j.Query
import twitter4j.Status

import scala.collection.JavaConversions._

object SearchSample extends App {

  val text = "ヤマト"
  
  // 現在のページから取得
  val list1 = search(text, false)
  for(tweet <- list1) println(tweet.getUser().getName() + ":" + tweet.getText)
  
  // 利用可能な全ページから取得
  val list2 = search(text, true)
  // 取得件数
  println("Size : " + list2.size)
  // 該当件数
  var counts = 0
  for(tweet <- list2){
    if(tweet.getText.contains(text)) counts += 1
  }
  println("Contains : " + counts)
  
  // 最もRTされたツイートの表示
  val famousTweet = max((x: Status, y: Status) => x.getRetweetCount < y.getRetweetCount)(list2).getRetweetedStatus()
  println("Famous Text => " + famousTweet.getUser.getName + ":" + famousTweet.getText)
  println("RT counts => " + famousTweet.getRetweetCount)
  
  /**
   * ツイートを検索する.<br>
   * 現在のページからのみ検索するか, 取得可能な全ページから検索するかどうかを選択可能.
   * @param text クエリ
   * @param isAll 取得可能な全ページから検索するかどうか 
   */
  def search(text : String, isAll : Boolean) : List[Status] = {
    val twitter = new TwitterFactory().getInstance()
    // 1ページの最大取得件数
    val maxCount = 100
    
    val query = new Query(text)
    query.setCount(maxCount)
    var results = twitter.search(query)
    var list = List[twitter4j.Status]()
    list = list ++ results.getTweets
    if(isAll){
      // 次ページの取得は QueryResult.nextQuery
      while(results.hasNext()){
        results = twitter.search(results.nextQuery)
        list = list ++ results.getTweets
      }
    }
    list
  }
  
  /**
   * 任意の型のリストの最大値を計算する.
   * less は関数オブジェクトで, less(x, y) => x < y をみたす.
   * list は処理対象である.
   */
  def max[T](less: (T, T) => Boolean)(list: List[T]) : T = {
    if(list.size==0) throw new IllegalArgumentException
    var maxElem = list.head
    for(elem <- list.tail) if(less(maxElem, elem)) maxElem = elem
    maxElem
  }
  
}