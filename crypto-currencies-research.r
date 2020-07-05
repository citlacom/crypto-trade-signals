library(data.table)
library(quantmod)

maxretry <- 3
# Past date that cover the oldest cyptos historical data.
startDate = as.Date("2010-01-01")

# Get crypto symbol historical data from Yahoo finance source and save as CSV file.
getSymbolHistoricalData <- function(symbol) {
  for(t in 1:maxretry) {
    tryCatch({
      cat("Downloading ", symbol, "\t\t Attempt: ", t , "/", maxretry, "\n")
      symbol.df <- getSymbols(symbol, src="yahoo", from=startDate, env=NULL, return.class='data.frame')
      filename <- paste('./data/', symbol, ".csv", sep='')
      symbol.df <- cbind(rownames(symbol.df), symbol.df)
      # Process any needed price adjusts.
      symbol.df[, 2:7] <- adjustOHLC(symbol.df[, 2:7], use.Adjusted=T)
      names(symbol.df) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adj Close')
      write.csv(symbol.df, file=filename, row.names=F)
      cat("Sucessfully saved historical data to ", filename, "\n")
      return(1)
    }, error = function(e) {
      print(e)
      return(0)
    })
  }
}

getSymbolHistoricalData("BTC-USD")