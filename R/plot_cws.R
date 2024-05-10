#' Timeseries plot of temperature
#' @param data data frame or sftime of cws observations
#' @param ts datetime, start time
#' @param te datetime, end time
#' @return A ggplo2 object with the timeseries
#' @import ggplot2
#' @export
#' @author Eva Marques
plot_ts <- function(data, ts, te) {
  p <- ggplot2::ggplot(data) +
    geom_line(aes(
      x = time,
      y = temp,
      group = as.factor(site_id),
      color = as.factor(site_id)
    )) +
    scale_x_datetime(
      date_labels = "%m-%d %Hh",
      date_breaks = "6 hours",
      date_minor_breaks = "1 hour",
      limits = c(ts, te)
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
      legend.position = "none"
    )
  return(p)
}

#' @import ggplot2
plot_ts_net <- function(data, ts, te) {
  min <- floor(quantile(data$temp,
    na.rm = TRUE,
    probs = 0.01
  ))
  max <- ceiling(quantile(data$temp,
    na.rm = TRUE,
    probs = 1
  ))
  p <- ggplot2::ggplot(
    data,
    aes(
      x = time,
      y = temp,
      group = as.factor(site_id),
      color = network
    )
  ) +
    geom_line() +
    scale_x_datetime(
      date_labels = "%m-%d %Hh",
      date_breaks = "6 hours",
      date_minor_breaks = "1 hour",
      limits = c(ts, te)
    ) +
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
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey"),
      panel.grid.minor = element_line(colour = "azure2"),
      legend.position = "right"
    )
  return(p)
}


#' Timeseries plot of temperature with reference network
#' @param data data frame or sftime of cws observations
#' @param ref data frame or sftime, reference network
#' @param ts datetime, start time
#' @param te datetime, end time
#' @return A ggplo2 object with the timeseries
#' @import ggplot2
#' @export
#' @author Eva Marques
plot_ts_ref <- function(data, ref, ts, te) {
  p <- ggplot2::ggplot(
    data,
    aes(
      x = time,
      y = temp,
      group = as.factor(site_id),
      color = as.factor(site_id)
    )
  ) +
    geom_line() +
    geom_line(data = ref, color = "red", size = 1) +
    scale_x_datetime(
      date_labels = "%m-%d %Hh",
      date_breaks = "6 hours",
      date_minor_breaks = "1 hour",
      limits = c(ts, te)
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
      legend.position = "none"
    )
  return(p)
}

#' Tile plot with temperature through time for all cws
#' @param data data frame or sftime of cws observations
#' @return A ggplo2 object with the tileplot
#' @export
#' @import ggplot2
#' @author Eva Marques
tile_ts <- function(data) {
  plot <- ggplot2::ggplot(
    data,
    aes(
      y = as.factor(site_id),
      x = lubridate::floor_date(time, unit = "hours"),
      fill = temp
    )
  ) +
    geom_tile() +
    scale_fill_stepsn(
      colours = rev(RColorBrewer::brewer.pal(10, "RdBu")),
      n.breaks = 15,
      guide = guide_legend()
    ) +
    labs(
      x = "time",
      y = "site_id",
      fill = "temperature"
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
  return(plot)
}

#' Hourly boxplot of temperature for per network (network)
#' @param data data frame or sftime with all the network, contains "network"
#' column
#' @param var continuous variable to boxplot (in general: temp)
#' @return A ggplo2 object with the boxplots
#' @export
#' @import ggplot2
#' @author Eva Marques
hourly_boxplot_networks <- function(data, var) {
  min <- floor(quantile(data[[deparse(substitute(var))]],
    na.rm = TRUE,
    probs = 0.01
  ))
  max <- ceiling(quantile(data[[deparse(substitute(var))]],
    na.rm = TRUE,
    probs = 0.997
  ))
  ggplot2::ggplot(
    data = data,
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
      panel.grid.minor = element_line(colour = "white"),
      legend.position = "none"
    )
}

#' @import ggplot2
map_observations <- function(data,
                             var,
                             background,
                             date,
                             shape_values,
                             title) {
  pal <- c("cyan4", "yellow", "orange", "red", "firebrick")

  ggplot2::ggplot() +
    tidyterra::geom_spatvector(data = background) +
    ggplot2::geom_sf(
      data = data[which(between(
        data$time,
        date,
        date + lubridate::hours(1)
      )), ],
      aes(geometry = geometry, fill = {{ var }}, shape = network),
      color = "black",
      size = 2,
    ) +
    xlim(min(data$lon), max(data$lon)) +
    ylim(min(data$lat), max(data$lat)) +
    scale_shape_manual(values = shape_values) +
    scale_fill_gradientn(colours = pal, na.value = NA) +
    labs(
      title = title,
      subtitle = date
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
}

#' @import ggplot2
map_observations_imp <- function(data,
                                 var,
                                 imp,
                                 date,
                                 shape_values,
                                 title) {
  pal <- c("cyan4", "yellow", "orange", "red", "firebrick")

  ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = imp) +
    ggplot2::geom_sf(
      data = data[which(between(
        data$time,
        date,
        date + lubridate::hours(1)
      )), ],
      aes(geometry = geometry, color = {{ var }}, shape = network),
      size = 3,
    ) +
    xlim(min(data$lon), max(data$lon)) +
    ylim(min(data$lat), max(data$lat)) +
    scale_shape_manual(values = shape_values) +
    #scale_color_gradientn(colours = pal, na.value = NA) +
    scale_fill_gradientn(colours = c("white", "grey"), na.value = NA) +
    tidyterra::scale_color_whitebox_c(
      palette = "muted",
      labels = scales::label_number(suffix = paste0("ºC")),
      n.breaks = 12,
      guide = guide_legend(reverse = TRUE)
    ) +
    guides(fill = guide_legend(title = "Imperviousness (%)")) +
    labs(
      title = title,
      subtitle = date
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
}


#' @import ggplot2
map_observations_hw <- function(data,
                                var,
                                imp,
                                hw,
                                date,
                                shape_values,
                                title) {
  pal <- c(
    "blue",
    "cyan",
    "lemonchiffon",
    "yellow",
    "orange",
    "red",
    "firebrick"
  )
  data_p <- data[which(between(
    data$time,
    date,
    date + lubridate::hours(1)
  )), ]
  tmin <- quantile(data_p[[deparse(substitute(var))]],
    na.rm = TRUE,
    probs = 0.05
  )
  tmax <- quantile(data_p[[deparse(substitute(var))]],
    na.rm = TRUE,
    probs = 0.95
  )
  ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = hw) +
    tidyterra::geom_spatraster_contour(
      data = imp,
      breaks = seq(0, 100, 20),
      alpha = .3
    ) +
    ggplot2::geom_sf(
      data = data_p,
      aes(geometry = geometry, fill = {{ var }}, shape = network),
      size = 3,
      stroke = NA
    ) +
    xlim(min(data$lon), max(data$lon)) +
    ylim(min(data$lat), max(data$lat)) +
    scale_shape_manual(values = shape_values) +
    scale_fill_gradientn(colours = pal, na.value = NA, limits = c(tmin, tmax)) +
    labs(
      title = title,
      subtitle = date
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
}
