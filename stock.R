createStock <- function(stockName) {
  
  # from RAVA
  url <- paste0("https://www.rava.com/empresas/precioshistoricos.php?e=", stockName, "&csv=1")
  
  stock <- read.csv(url)
  stock <- read.zoo(stock, sep = ",", header = TRUE)
  stock <- as.xts(stock)
  stock <- convertIndex(stock, "POSIXct")
  stock <- na.omit(stock)
  stock <- window(stock, start = startDate)
  
  names(stock) <- paste0(stockName, c(".Open", ".High", ".Low", ".Close", ".Volume", ".Adjusted"))
  
  return(stock)
}
