## ggpallettes
make_caru_ggpalette <- function(pal_name){

  ## Get CA palette by name
  pal <- ca_pal(pal_name)

  ## Construct palette
  palette <-
    rlang::new_function(
      rlang::exprs(n = ),
      rlang::expr({
        scale <- !!pal
        index <- 1:n
        out <- scale[index]
        return(out)
      }),
      rlang::caller_env()
    )

  return(palette)
}


ca_scale_fill_discrete <- function(ca_pallette = "orange", ...){
  ggplot2::scale_fill_discrete(type = ca_pal(ca_pallette), ...)
  }


