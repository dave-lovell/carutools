# Church Army Colours #

#' Church Army Colours
#'
#' These functions each return the hexadecimal code pertaining to a colour in Church Army's brand
#'
#' These functions are wrappers around character vectors of length one. They take no arguments and always return the same string.
#'
#' @returns
#' A single character vector of length one.
#' @export
#' @rdname ca_cols
ca_black <- function() "#000000"
#' @rdname ca_cols
#' @export
ca_white <- function() "#FFFFFF"
#' @rdname ca_cols
#' @export
ca_orange <- function() "#E84619"
#' @rdname ca_cols
#' @export
ca_lime   <- function() "#E1DE00"
#' @rdname ca_cols
#' @export
ca_gold   <- function() "#FBC900"
#' @rdname ca_cols
#' @export
ca_green  <- function() "#509E2F"
#' @rdname ca_cols
#' @export
ca_maroon <- function() "#910048"
#' @rdname ca_cols
#' @export
ca_cyan   <- function() "#0092BC"
#' @rdname ca_cols
#' @export
ca_light_teal <- function() "#6C8B93"
#' @rdname ca_cols
#' @export
ca_dark_teal  <- function() "#006272"
#' @rdname ca_cols
#' @export
ca_brown      <- function() "#6F2C3F"

## PALETTES ####################################################################

# Pallettes proper -------------------------------------------------------------
oranges      <- c("#E84619", "#ED6B47", "#F19074", "#F6B6A4", "#FADAD1")
golds        <- c("#FBC900", "#FCD446", "#FCDF74", "#FDE9A1", "#FEF4D0")
light_teals  <- c("#6C8B93", "#8AA2A9", "#A6B9BF", "#C4D1D3", "#E2E9E9")
cyans        <- c("#0092BC", "#36AFDA", "#68C4E2", "#9AD6EB", "#CCE9F3")
dark_teals   <- c("#006272", "#35818D", "#67A1AB", "#9AC0C8", "#CCDFE3")
maroons      <- c("#9A054A", "#AE396F", "#C26A93", "#D89CBB", "#EBCED8")
purples      <- c("#523178", "#765B94", "#9784AE", "#BBAEC9", "#DCD6E4")
browns       <- c("#652430", "#85515A", "#A37C83", "#C1A7AD", "#E1D4D7")
greens       <- c("#2AB04A", "#5EB66D", "#84C491", "#ACD6B5", "#D5E9D9")

# Palet functions --------------------------------------------------------------
#' Church Army Colour Pallets
#'
#' These functions each return a colour pallet from hurch Army's brand
#'
#' These functions are wrappers around character vectors of length five.
#' They take no arguments and always return the same charcter vector.
#'
#' @returns
#' A single character vector of length five.
#'
#' @rdname ca_pals
#' @export
ca_pal_orange     <- function() return(oranges)

#' @rdname ca_pals
#' @export
ca_pal_gold       <- function() return(golds)

#' @rdname ca_pals
#' @export
ca_pal_light_teal <- function() return(light_teals)

#' @rdname ca_pals
#' @export
ca_pal_cyan       <- function() return(cyans)

#' @rdname ca_pals
#' @export
ca_pal_dark_teal  <- function() return(dark_teals)

#' @rdname ca_pals
#' @export
ca_pal_maroon     <- function() return(maroons)

#' @rdname ca_pals
#' @export
ca_pal_purple     <- function() return(purples)

#' @rdname ca_pals
#' @export
ca_pal_brown      <- function() return(browns)

#' @rdname ca_pals
#' @export
ca_pal_green      <- function() return(green)

