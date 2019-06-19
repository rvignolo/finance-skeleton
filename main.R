# My Finances:
#   Account & Portfolio analysis
#   Ramiro Vignolo (rvignolo): ramirovignolo@gmail.com

setwd("~/run/finance")

# clean environments
rm(list = ls())
# rm_instruments(keep.currencies = FALSE)
try(rm(list = ls(envir = .blotter), envir = .blotter), silent = TRUE)
try(rm(list = ls(envir = FinancialInstrument:::.instrument), envir = FinancialInstrument:::.instrument), silent = TRUE)

library(blotter)
library(readxl)

source("fx.R")
source("fees.R")
source("stock.R")
source("returns.R")
source("auxiliary.R")
source("transactions.R")

# handlers
initEq <- 1
initDate  <- "2018-07-25"
startDate <- "2018-07-26"
currencyTypes <- c("USD", "ARS")
stockNames <- c("CEDEARVALE", "CEDEAGOOGL", "MIRG", "CEPU", "PAMP", "SUPV", "CEDEARAAPL")

# define currencies
for (currencyType in currencyTypes)
  currency(currencyType)

# define exchange rates
for (foreing in currencyTypes) {
  for (domestic in currencyTypes) {
    if (foreing != domestic)
      exchange_rate(paste0(foreing, domestic))
  }
}

# define stocks
for (stockName in stockNames)
  stock(stockName, currency = "ARS", multiplier = 1)

# set time zone for POSIXct class
Sys.setenv(TZ = "UTC")

# get exchange rates
for (foreing in currencyTypes) {
  for (domestic in currencyTypes) {
    if (foreing != domestic) {
      fxName <- paste0(foreing, domestic)
      # create blotter variable
      assign(fxName, createFx(foreing, domestic))
    }
  }
}

# get stocks
for (stockName in stockNames) {
  # create blotter variable
  assign(stockName, createStock(stockName))
}

# init portfolio
portfolioName <- initPortf("usd.port", symbols = stockNames, currency = "USD", initDate = initDate)

# init account
accountName <- initAcct("usd.acct", portfolios = portfolioName, currency = "USD", initDate = initDate, initEq = initEq)

# transactions
readTxns(portfolioName)

# update portfolios
updatePortf(Portfolio = portfolioName)

# update account
updateAcct(name = accountName)

# handlers
usd.port <- getPortfolio(portfolioName)
usd.acct <- getAccount(accountName)

# compute stocks returns
totalReturns_t <- xts()
for (stockName in stockNames) {

  # handler
  stock <- usd.port$symbols[[stockName]]

  # compute historical returns
  returns <- computeReturns(stock)
  totalReturns_t <- merge(totalReturns_t, returns$totalReturn)
}
names(totalReturns_t) <- stockNames

# compute portfolio returns
returns <- computeReturns(usd.port)
totalReturns_t <- merge(totalReturns_t, returns$totalReturn)

autoplot.zoo(totalReturns_t, facets = NULL)
