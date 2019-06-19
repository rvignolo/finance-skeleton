
# this function works given that txns are added to the a 
# variable stored in the blotter env.

readTxns <- function(portfolioName) {
  
  addTxn(Portfolio = portfolioName, Symbol = "CEDEARVALE", TxnDate = "2018-07-26", TxnPrice =  190.90, TxnQty = +400, TxnFees = hipotecarioFee, verbose = FALSE)
  
}
