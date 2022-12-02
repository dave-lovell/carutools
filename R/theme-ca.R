theme_ca <- function(colour = "orange"){

  colour <- ca_col(colour)
  font <- windowsFont("Trebuchet MS")
  windowsFonts(trebuchet = "Trebuchet MS")
  family <- "trebuchet"

  theme <-
    ggplot2::`%+replace%`(
      theme_minimal(base_family = "Trebuchet MS"),
      theme(
        plot.title = element_text(
          colour = colour
          )
        ))

  return(theme)
}

