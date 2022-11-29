#' Discrete colour/fill scales for ggplot2
#'
#' Add Church Army colour/fill scales to ggplot objects
#'
#' These functions are wrappers around \link[ggplot2]{scale_colour_discrete} that add Church Army colours to the aesthetics of ggplot objects.
#' Note that, since CA is a UK/ROI based charity, there are no americanised aliases for 'colour'.
#'
#' @param ca_palette The CA palette to use for the scale. A character; must be one of \link[carutools]{ca_sample_pal}
#'
#' @param ... Other arguments passed to \link[ggplot2]{scale_colour_discrete}/\link[ggplot2]{scale_fill_discrete}
#'
#' @returns
#' A single character vector of length one.
#' @export
#' @rdname ca_scale_fill/colour_discrete
ca_scale_colour_discrete <- function(ca_palette = "mix", ...){
  ggplot2::scale_colour_discrete(type = ca_palette, ...)
}

#'@export
#'@rdname ca_scale_fill/colour_discrete
ca_scale_fill_discrete <- function(ca_pallette = "mix", ...){
  ggplot2::scale_fill_discrete(type = ca_pal(ca_pallette), ...)
}

