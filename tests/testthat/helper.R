#' Get cfa fit for Holzinger and Swineford (1939)
#'
#' Shortcut function that runs cfa to Holzinger and Swineford 1939 data with default model setup
#' 
#' @return fit: a lavaan object
#'
#' @examples fit <- HS_cfa()
HS_cfa <- function() {
  H.S.model <- "visual  =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed   =~ x7 + x8 + x9"
  cfa(H.S.model, data = HolzingerSwineford1939)
}