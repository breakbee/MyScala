package twitter

import twitter4j.Twitter
import twitter4j.TwitterFactory
import twitter4j.Query
import twitter4j.Status
import twitter4j.Trend

object TrendSample extends App {

  // 東京の WOEIDs
  val tokyoID = 1118370
  for(trend <- trends(tokyoID)) println(trend.getName())
  
  /**
   * 現在のトレンドを取得する.
   * locale は Yahoo! WOEIDs (where on earth Identifiers).
   */
  def trends(locale : Int) : Array[Trend] = {
    val twitter = new TwitterFactory().getInstance()
    twitter.getPlaceTrends(locale).getTrends
  }
  
}