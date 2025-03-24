#' Timeseries plot of temperature
#' @param data data frame or sftime of cws observations
#' @param ts datetime, start time
#' @param te datetime, end time
#' @param var continuous variable to plot (in general: temp)
#' @return Timeseries plot
#' @import ggplot2
#' @export
#' @author Eva Marques
timeseries <- function(data, ts, te, var) {
  stopifnot(
    "data is not a data.frame, data.table, sf" =
      inherits(data, c("sf", "data.table", "data.frame"))
  )
  stopifnot(
    "missing variable in data" =
      c("time", "site_id", "network") %in% colnames(data)
  )
  time <- site_id <- network <- NULL
  data_p <- data[which(dplyr::between(data$time, ts, te)), ]
  min <- floor(quantile(data_p[[deparse(substitute(var))]],
    na.rm = TRUE,
    probs = 0.0
  ))
  max <- ceiling(quantile(data_p[[deparse(substitute(var))]],
    na.rm = TRUE,
    probs = 1
  ))
  p <- ggplot2::ggplot(data_p) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = time,
        y = {{ var }},
        group = site_id,
        color = network
      )
    ) +
    ggplot2::scale_x_datetime(
      date_labels = "%m-%d %Hh",
      date_breaks = "6 hours",
      date_minor_breaks = "1 hour",
      limits = c(ts, te)
    ) +
    ggplot2::xlab("time (UTC)") +
    ggplot2::scale_y_continuous(
      limits = c(min, max),
      breaks = seq(min, max, 1)
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      plot.caption = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 12),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey"),
      panel.grid.minor = ggplot2::element_line(colour = "azure2"),
    )
  return(p)
}


#' Hourly boxplot of temperature for per network (network)
#' @param data data frame with all the network, contains "network"
#' column
#' @param ts datetime, start time
#' @param te datetime, end time
#' @param var continuous variable to boxplot (in general: temp)
#' @return Hourly boxplots per network
#' @export
#' @import ggplot2
#' @importFrom stats quantile
#' @author Eva Marques
hourly_boxplot <- function(data, ts, te, var) {
  stopifnot(
    "data is not a data.frame, data.table, sf" =
      inherits(data, c("sf", "data.table", "data.frame"))
  )
  stopifnot(
    "missing variable in data" =
      c("time", "network") %in% colnames(data)
  )
  network <- NULL
  data_p <- data[which(dplyr::between(data$time, ts, te)), ]
  min <- floor(quantile(data_p[[deparse(substitute(var))]],
    na.rm = TRUE,
    probs = 0.0
  ))
  max <- ceiling(quantile(data_p[[deparse(substitute(var))]],
    na.rm = TRUE,
    probs = 1
  ))
  p <- ggplot2::ggplot(
    data = data_p,
    ggplot2::aes(
      x = network,
      y = {{ var }},
      color = network,
      group = network
    )
  ) +
    ggplot2::geom_boxplot(outlier.shape = 3, fill = "white", width = .8) +
    ggplot2::facet_wrap(
      ~ lubridate::floor_date(time, unit = "hours"),
      ncol = 12
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(min, max, by = 5),
      minor_breaks = seq(min, max, by = 1),
      limits = c(min, max)
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      plot.caption = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 12),
      panel.background = ggplot2::element_rect(fill = "gray95"),
      panel.grid.major = ggplot2::element_line(colour = "grey"),
      panel.grid.minor = ggplot2::element_line(colour = "white")
    )
  return(p)
}


#' Tile plot with temperature through time for all cws
#' @param data data frame of cws observations
#' @param ts datetime, start time
#' @param te datetime, end time
#' @param var continuous variable to plot (in general: temp)
#' @param palname name of the palette to use
#' @return Tile plot
#' @export
#' @import ggplot2
#' @author Eva Marques
tile_ts <- function(data, ts, te, var, palname = "temp_ipcc") {
  stopifnot(
    "data is not a data.frame, data.table, sf" =
      inherits(data, c("sf", "data.table", "data.frame"))
  )
  stopifnot(
    "missing variable in data" =
      c("time", "site_id") %in% colnames(data)
  )
  time <- site_id <- NULL
  data_p <- data[which(dplyr::between(data$time, ts, te)), ]
  min <- floor(quantile(data_p[[deparse(substitute(var))]],
    na.rm = TRUE,
    probs = 0.0
  ))
  max <- ceiling(quantile(data_p[[deparse(substitute(var))]],
    na.rm = TRUE,
    probs = 1
  ))
  p <- ggplot2::ggplot(
    data_p,
    ggplot2::aes(
      y = as.factor(site_id),
      x = lubridate::floor_date(time, unit = "hours"),
      fill = {{ var }}
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_stepsn(
      colours = my_pal(palname),
      n.breaks = 15,
      guide = ggplot2::guide_legend(),
      limits = c(min, max)
    ) +
    ggplot2::labs(
      x = "time",
      y = "weather station"
    ) +
    ggplot2::scale_x_datetime(
      date_labels = "%m-%d %Hh",
      date_breaks = "12 hours",
      date_minor_breaks = "1 hour"
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(barwidth = 0.7, barheight = 25)
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      plot.caption = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 12),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey"),
      panel.grid.minor = ggplot2::element_line(colour = "azure2"),
      legend.position = "right"
    )
  return(p)
}

#' map observations with imperviousness
#' @param data data frame or sftime of cws observations
#' @param datetime datetime to plot
#' @param var continuous variable to plot (in general: temp)
#' @param imp imperviousness raster
#' @param palname name of the palette to use
#' @param netw_shape shape values for the network
#' @param title title of the plot
#' @import ggplot2
#' @import ggspatial
#' @importFrom tidyterra geom_spatraster scale_color_whitebox_c
map_stations <- function(
    data,
    datetime,
    var,
    imp,
    palname = "temp_ipcc",
    netw_shape = c(
      "WU" = 17,
      "PA" = 18,
      "NCEI/ASOS/AWOS" = 16,
      "NCEI/US CRN" = 15
    ),
    title) {
  geometry <- network <- NULL
  data_p <- data[which(data$time == datetime), ]
  min <- floor(min(data_p[[deparse(substitute(var))]],
    na.rm = TRUE
  ))
  max <- ceiling(max(data_p[[deparse(substitute(var))]],
    na.rm = TRUE
  ))
  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = imp) +
    ggplot2::geom_sf(
      data = data_p,
      ggplot2::aes(geometry = geometry, color = {{ var }}, shape = network),
      size = 3,
    ) +
    ggplot2::xlim(min(data_p$lon), max(data_p$lon)) +
    ggplot2::ylim(min(data_p$lat), max(data_p$lat)) +
    ggplot2::scale_shape_manual(values = netw_shape) +
    ggplot2::scale_fill_gradientn(colours = c("white", "grey"), na.value = NA) +
    ggplot2::scale_color_gradientn(
      colours = my_pal(palname),
      limits = c(min, max)
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Imperviousness")) +
    ggplot2::labs(
      title = title,
      subtitle = datetime
    ) +
    ggspatial::annotation_scale(
      location = "bl", pad_x = ggplot2::unit(1, "cm"),
      pad_y = ggplot2::unit(1, "cm"),
      height = ggplot2::unit(0.30, "cm"),
      text_cex = 1
    ) +
    ggspatial::annotation_north_arrow(
      location = "br",
      which_north = "true",
      pad_x = ggplot2::unit(0.2, "cm"),
      pad_y = ggplot2::unit(0.2, "cm")
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      plot.caption = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 12),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )
  return(p)
}
