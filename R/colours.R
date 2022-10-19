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
ca_black <- function() "#000000"
ca_white <- function() "#FFFFFF"

## SECONDARY ###### ################################################################################

ca_orange <- function() "#E84619"
ca_lime   <- function() "#E1DE00"
ca_gold   <- function() "#FBC900"
ca_green  <- function() "#509E2F"
ca_maroon <- function() "#910048"
ca_cyan   <- function() "#0092BC"

## TERTIARY #########################################################################################

ca_light_teal <- function() "#6C8B93"
ca_dark_teal  <- function() "#006272"
ca_brown      <- function() "#6F2C3F"

## PALETTES #########################################################################################
