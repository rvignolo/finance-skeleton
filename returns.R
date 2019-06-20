computeTotalReturn <- function(instrument) {
  
  # handle portfolios and stocks
  txn <- list()
  if (is.portfolio(instrument)) {
    isPortfolio <- TRUE
    for (i in 1:length(instrument$symbols))
      txn[[i]] <- instrument$symbols[[i]]$posPL.USD$Txn.Value
    instrument <- instrument$summary
  } else {
    # assume it is a stock
    isPortfolio <- FALSE
    txn[[1]] <- instrument$posPL.USD$Txn.Value
    instrument <- instrument$posPL.USD
  }
  
  # la primera comision pagada y sera la fecha inicial donde el 
  # calculo del total return tiene sentido. No uso Txn.Value xq
  # la tabla de summary para portfolios no la tiene.
  ti <- start(instrument[instrument$Txn.Fees != 0])
  instrument <- instrument[paste0(ti, "::")]
  for (i in 1:length(txn))
    txn[[i]] <- txn[[i]][index(instrument)]
  
  # profit and loss
  PnL_t <- cumsum(instrument$Net.Trading.PL)
  
  # compute the total amount of money invested
  investment_t <- xts(rep(0, length(PnL_t)), order.by = index(PnL_t))
  for (i in 1:length(txn)) {
    # transactions term
    Positive.Txn.Value <- ifelse(txn[[i]]$Txn.Value > 0, txn[[i]]$Txn.Value, 0)
    investment_t <- investment_t + Positive.Txn.Value
  }
  # fees term
  investment_t <- investment_t + abs(instrument$Txn.Fees)
  investment_t <- cumsum(investment_t)
  
  totalReturn_t <- 100 * PnL_t / investment_t
  
  return(totalReturn_t)
}