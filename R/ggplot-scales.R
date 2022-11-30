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
#' @export
#' @rdname ca_scale_colour_discrete
ca_scale_colour_discrete <- function(ca_palette = "mix", ...){
  if(length(ca_palette) == 1) ggplot2::scale_colour_discrete(type = ca_pal(ca_palette), ...)
  else                        ggplot2::scale_colour_discrete(values = ca_cols(ca_palette))
}

#'@export
#'@rdname ca_scale_colour_discrete
ca_scale_fill_discrete <- function(ca_palette = "mix", ...){
  if(length(ca_palette) == 1) ggplot2::scale_fill_discrete(type = ca_pal(ca_palette), ...)
  else                        ggplot2::scale_fill_discrete(values = ca_cols(ca_palette))
}


#' Continuous colour/fill scales for ggplot2
#'
#' Add Church Army colour/fill scales to ggplot objects
#'
#' These functions are wrappers around \link[ggplot2]{scale_colour_gradientn} that add Church Army colours to the aesthetics of ggplot objects.
#' Note that, since CA is a UK/ROI based charity, there are no americanised aliases for 'colour'.
#'
#' These functions take a single argument `ca_palette`, which is passed to \link[carutools]{ca_pal} to specify a palette.
#' The values of the palette are used to generate a continuous scale.

#' @param ca_palette The CA palette to use for the scale. Must be one of \link[carutools]{ca_sample_pals}
#'
#' @param ... Other arguments passed to \link[ggplot2]{scale_colour_gradientn}/\link[ggplot2]{scale_fill_discrete}
#'
#' @export
#' @rdname ca_scale_colour_continuous
ca_scale_colour_continuous <- function(ca_palette = "orange", ...){
  if(ca_palette == "mix") rlang::warn("This plot probably looks ugly.", "Consider not using 'mix' lol.")
  pal <- ca_pal(ca_palette)
  ggplot2::scale_colour_gradientn(colours = pal, ...)
}

#' @export
#' @rdname ca_scale_colour_continuous
ca_scale_fill_continuous <- function(ca_palette, ...){
  if(ca_palette == "mix") rlang::warn("This plot probably looks ugly.", "Consider not using 'mix' lol.")
  pal <- ca_pal(ca_palette)
  ggplot2::scale_fill_gradientn(colours = pal, ...)
}


#' Continuous colour/fill scales for ggplot2
#'
#' Create continuous colour gradients from Church Army colours, and use these as scales for ggplot objects
#'
#' These functions are wrappers around \link[ggplot2]{scale_colour_gradient}/\link[ggplot2]{scale_colour_gradient2} that add Church Army colours to the aesthetics of ggplot objects.
#' Note that, since CA is a UK/ROI based charity, there are no americanised aliases for 'colour'.
#'
#' Arguments for `low`, `high` and `mid` are passed to  \link[carutools]{ca_col} to specify a Church Army colour that is forwarded to the gradient-making function.
#' The values of the palette are used to generate a continuous scale.
#'
#' @param ... Other arguments passed to \link[ggplot2]{scale_colour_gradientn}/\link[ggplot2]{scale_fill_discrete}
#'
#' @export
#' @rdname ca_scale_colour_gradient
ca_scale_colour_gradient <- function(low = "cyan", high = "green", ...){
  low  <- ca_col(low)
  high <- ca_col(high)
  ggplot2::scale_colour_gradient(low, high, ...)
}

#' @export
#' @rdname ca_scale_colour_gradient
ca_scale_fill_gradient <- function(low = "cyan", high = "green", ...){
  low  <- ca_col(low)
  high <- ca_col(high)
  ggplot2::scale_fill_gradient(low, high, ...)
}

#' @export
#' @rdname ca_scale_colour_gradient
ca_scale_colour_gradient2 <- function(low = "cyan", high = "green", mid = "white", ...){
  low <- ca_col(low)
  high <- ca_col(high)
  mid  <- ca_col(mid)

  ggplot2::scale_colour_gradient2(low = low, mid = mid, high = high, ...)
}

#' @export
#' @rdname ca_scale_colour_gradient
ca_scale_fill_gradient2 <- function(low = "cyan", high = "green", mid = "white", ...){
  low <- ca_col(low)
  high <- ca_col(high)
  mid  <- ca_col(mid)

  ggplot2::scale_fill_gradient2(low = low, mid = mid, high = high, ...)
}
