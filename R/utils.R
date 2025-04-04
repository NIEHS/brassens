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
      to %in% c("C", "F", "K")
  )
  if (from == "C") {
    ifelse(to == "F",
      (x * 9 / 5) + 32,
      x + 273.15
    )
  } else if (from == "F") {
    ifelse(to == "C",
      (x - 32) * 5 / 9,
      (x - 32) * 5 / 9 + 273.15
    )
  } else if (from == "K") {
    ifelse(to == "C",
      x - 273.15,
      (x - 273.15) * 9 / 5 + 32
    )
  }
}

#' Find my home-made palettes
#' @param name Name of the palette
#' @return A vector with colors
#' @importFrom grDevices rgb
#' @export
#' @author Eva Marques
my_pal <- function(name) {
  stopifnot(
    "name should be one of \"temp\", \"sw\", \"reds\", \"prior\", or \"uhi\"" =
      name %in% c("temp", "sw", "reds", "prior", "uhi", "rh_ipcc", "temp_ipcc")
  )
  # -- define palettes
  if (name == "temp") {
    fields::tim.colors(n = 64, alpha = 1.0)
  } else if (name == "rh_ipcc") {
    pal_ipcc <- list(
      c(140, 81, 10),
      c(191, 129, 45),
      c(223, 194, 125),
      c(246, 232, 195),
      c(245, 245, 245),
      c(199, 234, 229),
      c(128, 205, 193),
      c(53, 151, 143),
      c(1, 102, 94)
    ) |>
      lapply(function(x) rgb(x[1], x[2], x[3], maxColorValue = 255)) |>
      rev()
    pal_ipcc
  } else if (name == "temp_ipcc") {
    pal_ipcc <- list(
      c(103, 0, 31),
      c(178, 24, 43),
      c(214, 96, 77),
      c(244, 165, 130),
      c(253, 219, 199),
      c(247, 247, 247),
      c(209, 229, 240),
      c(146, 197, 222),
      c(67, 147, 195),
      c(33, 102, 172),
      c(5, 48, 97)
    ) |>
      lapply(function(x) rgb(x[1], x[2], x[3], maxColorValue = 255)) |>
      rev()
    pal_ipcc
  } else if (name == "sw") {
    RColorBrewer::brewer.pal(10, "RdYlBu")
  } else if (name == "reds") {
    c("white", "firebrick4")
  } else if (name == "prior") {
    RColorBrewer::brewer.pal(10, "RdBu")
  } else if (name == "uhi") {
    c(
      "cyan4",
      "grey",
      "yellow",
      "orange",
      "red",
      "firebrick",
      "black"
    )
  }
}
