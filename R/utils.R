#' Convert temperature between Celsius, Fahrenheit, and Kelvin
#' @param x numeric vector
#' @param from character vector, one of "C", "F", or "K"
#' @param to character vector, one of "C", "F", or "K"
#' @return numeric vector
#' @export
#' @author Eva Marques
convert_temp <- function(x, from, to) {
  stopifnot(
    "from should be \"C\", \"F\" or \"K\"" =
      from %in% c("C", "F", "K"),
    "to should be \"C\", \"F\" or \"K\"" =
      to %in% c("C", "F", "K"),
    "x is not numeric" = is.numeric(x)
  )
  if (from == "C") {
    ifelse(to == "F",
      return((x * 9 / 5) + 32),
      return(x + 273.15)
    )
  } else if (from == "F") {
    ifelse(to == "C",
      return((x - 32) * 5 / 9),
      return((x - 32) * 5 / 9 + 273.15)
    )
  } else if (from == "K") {
    ifelse(to == "C",
      return(x - 273.15),
      return((x - 273.15) * 9 / 5 + 32)
    )
  }
}

#' Find my home-made palettes
#' @param name Name of the palette
#' @return A vector with colors
#' @export
#' @author Eva Marques
my_pal <- function(name) {
  stopifnot(
    "name should be one of \"temp\", \"sw\", \"reds\", \"prior\", or \"uhi\"" =
      name %in% c("temp", "sw", "reds", "prior", "uhi")
  )
  # -- define palettes
  if (name == "temp") {
    return(fields::tim.colors(n = 64, alpha = 1.0))
  } else if (name == "sw") {
    return(RColorBrewer::brewer.pal(10, "RdYlBu"))
  } else if (name == "reds") {
    return(c("white", "firebrick4"))
  } else if (name == "prior") {
    return(RColorBrewer::brewer.pal(10, "RdBu"))
  } else if (name == "uhi") {
    return(c(
      "cyan4",
      "grey",
      "yellow",
      "orange",
      "red",
      "firebrick",
      "black"
    ))
  }
}
