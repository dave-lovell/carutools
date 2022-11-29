#' Discrete colour/fill scales for ggplot2
#'
#' Add Church Army colour/fill scales to ggplot objects
#'
#' These functions are wrappers around \link[ggplot2]{scale_colour_discrete} that add Church Army colours to the aesthetics of ggplot objects.
#' Note that, since CA is a UK/ROI based charity, there are no americanised aliases for 'colour'.
#'
#' These functions take a single argument `ca_palette`. If its length is one, it is passed to \link[carutools]{ca_pal} to specify a palette.
#' Otherwise, it is passed to \link[carutools]{ca_cols} to specify individual colours.`

#' @param ca_palette The CA palette to use for the scale. A character vector. If length one, it must be one of \link[carutools]{ca_sample_pal}.
#' Otherwise, every value must be one of \link[carutools]{ca_cols}
#'
#' @param ... Other arguments passed to \link[ggplot2]{scale_colour_discrete}/\link[ggplot2]{scale_fill_discrete}
#'
#' @returns
#' A single character vector of length one.
#' @export
#' @rdname ca_scale_colour_discrete
ca_scale_colour_discrete <- function(ca_palette = "mix", ...){
  if(length(ca_palette) == 1) ggplot2::scale_colour_discrete(type = ca_pal(ca_palette), ...)
  else                        ggplot2::scale_colour_discrete(values = ca_col(ca_palette))
}

#'@export
#'@rdname ca_scale_colour_discrete
ca_scale_fill_discrete <- function(ca_palette = "mix", ...){
  if(length(ca_palette) == 1) ggplot2::scale_fill_discrete(type = ca_pal(ca_palette), ...)
  else                        ggplot2::scale_fill_discrete(values = ca_col(ca_palette))
}
