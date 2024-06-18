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
  time <- site_id <- network <- NULL
  data_p <- data[which(between(data$time, ts, te)), ]
  min <- floor(quantile(data_p[[deparse(substitute(var))]],
    na.rm = TRUE,
    probs = 0.0
  ))
  max <- ceiling(quantile(data_p[[deparse(substitute(var))]],
    na.rm = TRUE,
    probs = 1
  ))
  p <- ggplot2::ggplot(data_p) +
    geom_line(aes(
      x = time,
      y = {{ var }},
      group = site_id,
      color = network
    )) +
    scale_x_datetime(
      date_labels = "%m-%d %Hh",
      date_breaks = "6 hours",
      date_minor_breaks = "1 hour",
      limits = c(ts, te)
    ) +
    xlab("time (UTC)") +
    scale_y_continuous(
      limits = c(min, max),
      breaks = seq(min, max, 1)
    ) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      plot.caption = element_text(size = 10),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey"),
      panel.grid.minor = element_line(colour = "azure2"),
    )
  return(p)
}


#' Hourly boxplot of temperature for per network (network)
#' @param data data frame or sftime with all the network, contains "network"
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
  network <- NULL
  data_p <- data[which(between(data$time, ts, te)), ]
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
    aes(
      x = network,
      y = {{ var }},
      color = network,
      group = network
    )
  ) +
    geom_boxplot(outlier.shape = 3, fill = "white", width = .8) +
    facet_wrap(~ lubridate::floor_date(time, unit = "hours"), ncol = 12) +
    scale_y_continuous(
      breaks = seq(min, max, by = 5),
      minor_breaks = seq(min, max, by = 1),
      limits = c(min, max)
    ) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      plot.caption = element_text(size = 10),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      panel.background = element_rect(fill = "gray95"),
      panel.grid.major = element_line(colour = "grey"),
      panel.grid.minor = element_line(colour = "white")
    )
  return(p)
}


#' Tile plot with temperature through time for all cws
#' @param data data frame or sftime of cws observations
#' @param ts datetime, start time
#' @param te datetime, end time
#' @param var continuous variable to plot (in general: temp)
#' @param palname name of the palette to use
#' @return Tile plot
#' @export
#' @import ggplot2
#' @author Eva Marques
tile_ts <- function(data, ts, te, var, palname = "temp_ipcc") {
  time <- site_id <- NULL
  data_p <- data[which(between(data$time, ts, te)), ]
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
    aes(
      y = as.factor(site_id),
      x = lubridate::floor_date(time, unit = "hours"),
      fill = {{ var }}
    )
  ) +
    geom_tile() +
    scale_fill_stepsn(
      colours = my_pal(palname),
      n.breaks = 15,
      guide = guide_legend(),
      limits = c(min, max)
    ) +
    labs(
      x = "time",
      y = "site_id"
    ) +
    scale_x_datetime(
      date_labels = "%m-%d %Hh",
      date_breaks = "12 hours",
      date_minor_breaks = "1 hour"
    ) +
    guides(fill = guide_colourbar(barwidth = 0.7, barheight = 25)) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      plot.caption = element_text(size = 10),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey"),
      panel.grid.minor = element_line(colour = "azure2"),
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
map_stations <- function(data,
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
      aes(geometry = geometry, color = {{ var }}, shape = network),
      size = 3,
    ) +
    xlim(min(data_p$lon), max(data_p$lon)) +
    ylim(min(data_p$lat), max(data_p$lat)) +
    scale_shape_manual(values = netw_shape) +
    scale_fill_gradientn(colours = c("white", "grey"), na.value = NA) +
    scale_color_gradientn(
      colours = my_pal(palname),
      limits = c(min, max)
    ) +
    guides(fill = guide_legend(title = "Imperviousness")) +
    labs(
      title = title,
      subtitle = datetime
    ) +
    ggspatial::annotation_scale(
      location = "bl", pad_x = unit(1, "cm"),
      pad_y = unit(1, "cm"),
      height = unit(0.30, "cm"),
      text_cex = 1
    ) +
    ggspatial::annotation_north_arrow(
      location = "br",
      which_north = "true",
      pad_x = unit(0.2, "cm"),
      pad_y = unit(0.2, "cm")
    ) +
    theme(
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )
  return(p)
}
