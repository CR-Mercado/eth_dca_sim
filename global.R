library(jsonlite)

price_to_date <- function(coin, date1 = "2020-05-01", date2 = "2021-01-21"){ 
  gecko_string <- paste0(
    "https://api.coingecko.com/api/v3/coins/",
    coin,
    "/market_chart/range?vs_currency=usd&from=",
    as.numeric(as.POSIXct(date1)),
    "&to=",
    as.numeric(as.POSIXct(date2))
  )
  
  gecko_pull <- jsonlite::fromJSON(gecko_string)
  print(gecko_string)
  return(gecko_pull)
}


coingecko_daily_eth_prices <- price_to_date("ethereum",
                                            date1 = "2015-08-07",
                                            date2 = Sys.Date())

eth <- as.data.frame(coingecko_daily_eth_prices$prices)
colnames(eth) <- c("date","price")

eth$date <- as.Date(as.POSIXct(eth$date/1000, 
                               origin="1970-01-01"))

tradingview <- function(){ 
  HTML(
'
<!-- TradingView Widget BEGIN -->
<div class="tradingview-widget-container">
  <div id="tradingview_16e8d"></div>
  <div class="tradingview-widget-copyright"><a href="https://www.tradingview.com/symbols/ETHUSD/?exchange=COINBASE" rel="noopener" target="_blank"><span class="blue-text">ETHUSD Chart</span></a> by TradingView</div>
  <script type="text/javascript" src="https://s3.tradingview.com/tv.js"></script>
  <script type="text/javascript">
  new TradingView.widget(
  {  "symbol": "COINBASE:ETHUSD",
  "interval": "D",
  "timezone": "Etc/UTC",
  "theme": "light",
  "style": "1",
  "locale": "en",
  "toolbar_bg": "#f1f3f6",
  "enable_publishing": false,
  "withdateranges": true,
  "allow_symbol_change": true,
  "save_image": false,
  "container_id": "tradingview_16e8d"
}
  );
  </script>
</div>
<!-- TradingView Widget END -->
'
  )
  }