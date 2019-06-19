hipotecarioFee <- function(TxnQty, TxnPrice, ...) {

  fee_pct <- 0.60
  DMB_pct <- 0.08

  # DMB: Derecho de Mercado Bursatils
  total <- abs(TxnPrice * TxnQty)
  fee <- max(75.00, fee_pct / 100 * total) + DMB_pct / 100 * total
  fee <- addIVA(fee)

  return(-fee)
}

addIVA <- function(value) {
  return(1.21 * value)
}
