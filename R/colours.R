# Church Army Colours #

## Wrappers & Helpers ###############################################################################


ca_pallete <- function(pallette){

  if(is.function(palette)) palette <- palette()

  rlang::new_function(
    args = rlang::pairlist2(x = 1),
    body =
      quote({
        n_pal <- length(palette)
        stopifnot(all(x <= n_pal), all(x > 0))

        out_col <- palette[x]
        return(out_col)
        }))}

## PRIMARY #########################################################################################

### Black  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

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

## SECONDARY ###### ################################################################################

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

## PALETTES #########################################################################################
#
# oranges <- c("#E84619", "#ED6B47", "#F19074", "#F6B6A4", "#FADAD1")
# golds   <- c("#FBC900", "#FCD446", "#FCDF74", "#FDE9A1", "#FEF4D0")
# lighttea  <- c("#")
