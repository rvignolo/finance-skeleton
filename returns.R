computeReturns <- function(instrument, year = NULL) {
  
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
  
  # FIXME: apply SPOT rule ASAP!
  Initial.Pos.Value <- 0
  if (!is.null(year)) {
    
    # use only relevant data
    year <- as.character(year)
    instrument <- instrument[year]
    for (i in 1:length(txn))
      txn[[i]] <- txn[[i]][year]
    
    # cuando nos dan un anyo, la fecha inicial es el 20XX-01-01
    # si es que tenemos una posicion o la fecha del trade inicial
    # en el anyo si es que no tenemos ninguna posicion.
    if (!isPortfolio) {
      if (instrument$Pos.Qty[1] != 0) {
        ti <- index(instrument[1])
        Initial.Pos.Value <- instrument$Pos.Value[1]
      } else {
        # el primer trade va a ser de compra si o si
        # ya que yo no me shorteo
        ti <- start(instrument[instrument$Txn.Fees != 0])
        
        # clean the zeros...
        instrument <- instrument[paste0(ti, "::")]
        for (i in 1:length(txn))
          txn[[i]] <- txn[[i]][index(instrument)]
      }
    } else {
      # aplicamos el mismo procedimiento
      if (instrument$Long.Value[1] != 0){
        ti <- index(instrument[1])
        Initial.Pos.Value <- instrument$Long.Value[1]
      } else {
        ti <- start(instrument[instrument$Txn.Fees != 0])
        
        # clean the zeros...
        instrument <- instrument[paste0(ti, "::")]
        for (i in 1:length(txn))
          txn[[i]] <- txn[[i]][index(instrument)]
      }
    }
  } else {
    
    # si no nos piden un anyo particular, es suficiente con buscar
    # la primera comision pagada y sera la fecha inicial donde el 
    # calculo del total return tiene sentido. No uso Txn.Value xq
    # la tabla de summary para portfolios no la tiene.
    ti <- start(instrument[instrument$Txn.Fees != 0])
    instrument <- instrument[paste0(ti, "::")]
    for (i in 1:length(txn))
      txn[[i]] <- txn[[i]][index(instrument)]
  }
  
  # profit and loss
  PnL_t <- cumsum(instrument$Net.Trading.PL)
  
  # compute the total amount of money invested
  investment_t <- xts(rep(0, length(PnL_t)), order.by = index(PnL_t))
  # Hint: la Initial.Pos.Value va a ser nula si el primer dia habia posicion, por 
  # lo que no estariamos sumando dos veces en el siguiente loop si coincide con
  # fecha de transaccion
  investment_t[1] <- Initial.Pos.Value
  for (i in 1:length(txn)) {
    # transactions term
    Positive.Txn.Value <- ifelse(txn[[i]]$Txn.Value > 0, txn[[i]]$Txn.Value, 0)
    investment_t <- investment_t + Positive.Txn.Value
  }
  # fees term
  investment_t <- investment_t + abs(instrument$Txn.Fees)
  investment_t <- cumsum(investment_t)
  
  t <- index(instrument)
  totalReturn_t <- PnL_t / investment_t
  annualizedReturn_t <- (totalReturn_t + 1) ^ (365 / as.numeric(difftime(t, ti, units = "days"))) - 1
  
  returns <- list()
  returns$totalReturn_t <- totalReturn_t * 100
  returns$annualizedReturn_t <- annualizedReturn_t * 100
  
  return(returns)
}