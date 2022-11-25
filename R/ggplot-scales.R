ca_scale_fill_discrete <- function(ca_pallette = "mix", ...){
  ggplot2::scale_fill_discrete(type = ca_pal(ca_pallette), ...)
}
ca_scale_colour_discrete <- function(ca_palette = "mix", ...){
  ggplot2::scale_colour_discrete(type = ca_palette, ...)
}


