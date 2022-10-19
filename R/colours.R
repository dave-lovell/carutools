## Church Army Colours

ca_black <- function() "#000000"

ca_greys <- function() c("#262626", "#595959", "#8c8c8c", "#bfbfbf", "d9d9d9", "#e5e5e5", "#f2f2f2")

ca_grey <- function(k = c("85", "65", "45", "25", "15", "10", "5")){
  k = match.arg(k)
  k.index <- which(c("85", "65", "45", "25", "15", "10", "5") == k)

  grey <- carutools::ct_greys()[k.index]
}
