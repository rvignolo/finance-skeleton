
createFx <- function(foreing, domestic) {
  
  # from BCRA
  download.file(url = "https://www.bcra.gob.ar/Pdfs/PublicacionesEstadisticas/com3500.xls", destfile = "usdars.xls", quiet = TRUE)
  a <- read_excel("usdars.xls", range = "C4:D8000")
  
  fx <- zoo(x = na.omit(a$`Tipo de Cambio de Referencia - en Pesos - por Dólar`), order.by = na.omit(a$Fecha))
  fx <- as.xts(fx)
  fx <- convertIndex(fx, "POSIXct")
  fx <- na.omit(fx)
  fx <- window(fx, start = startDate)
  
  names(fx) <- paste(foreing, domestic, sep = ".")
  
  if (foreing == "ARS")
    fx <- 1 / fx
  
  return(fx)
}
