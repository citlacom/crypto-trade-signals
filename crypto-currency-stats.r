library(data.table)
library(quantmod)

expandPath <- function(path) {
  normalizePath(paste(getwd(), path, sep=''))
}

# Load symbol historical data in data.table structure with fast/slow moving averages.
getSymbolHistoricalDataTable <- function(dataFile, maFast=20, maSlow=50, dateFormat="%Y-%m-%d", firstDayDate='1970-01-01') {
  filename <- expandPath(paste("/data/", dataFile, ".csv", sep=''))
  symbol <- fread(filename)
  symbol[, Date := as.Date(as.character(Date), format=dateFormat)]
  symbol[, Year := as.character(format(Date, "%Y"))]
  # Sort by Date and create an index.
  setkey(symbol, 'Date')
  symbol <- symbol[Date >= firstDayDate,]
  # Calculate moving averages from average (Mid) candle price.
  symbol[, Mid := (High + Low + Close + Open) / 4]
  symbol[, MidMAF := SMA(Mid, n=maFast)]
  symbol[, MidMAS := SMA(Mid, n=maSlow)]
  symbol[, priceDiff:= MidMAF-MidMAS]

  if (all(symbol$priceDiff== 0)) {
    stop("Undetermined symbol price direction")
  }

  # Label price direction based on price difference.
  symbol <- symbol[!is.na(priceDiff)]
  symbol[, Eff := cut(priceDiff, c(-50000, 0, 50000), labels=c('loss', 'profit'), right=FALSE)]
  return(symbol)
}

btcusdDataTable <- getSymbolHistoricalDataTable("BTC-USD")
summary(btcusdDataTable$priceDiff)