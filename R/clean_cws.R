format_pa <- function(raw) {
  x <- raw
  x$time_stamp_est <- as.POSIXct(x$time_stamp, "EST")
  
  return(x)
}

format_wu <- function(raw) {
  
}


compute_daily_trend <- function(cws){
  
  # -- stations metadata
  cws_meta <- unique(as.data.frame(cws)[, c("id", "lat", "lon")])
  cws_ts <- merge(as.data.frame(cws), cws_meta, by = c("id", "lat", "lon"))
  
  # -- hourly aggregation (if not already done)
  cws_ts <- cws_ts |>
    group_by(time = lubridate::floor_date(time, "1 hour"), 
             id = id,
             lat = lat,
             lon = lon) |>
    dplyr::summarize(temp = median(temp, na.rm = T)) |>
    data.frame()
  
  # -- remove NA 
  cws_ts <- cws_ts[which(!(is.na(cws_ts$temp))), ]
  
  # -- hourly trends from cws aggregation
  cws_trend <- cws_ts[,c("time", "temp")] |>
    group_by(time = time) |>
    dplyr::summarize(temp_med = median(temp, na.rm = T),
                     temp_min = min(temp, na.rm = T),
                     temp_dec = quantile(temp, probs = 0.1, na.rm = T)) |>
    data.frame()       
  
  cws_trend_dailycycle <- cws_trend |>
    group_by(time = lubridate::hour(time)) |>
    dplyr::summarize(temp_med = median(temp_med, na.rm = T), 
                     temp_min = median(temp_min, na.rm = T), 
                     temp_dec = median(temp_dec, na.rm = T)) |>
    data.frame()  
  
  # -- compute diff to neighborhood ref
  cws_ts <- merge(cws_ts, cws_trend, by='time')
  cws_ts$d_temp_min <- cws_ts$temp - cws_ts$temp_min
  cws_ts$d_temp_dec <- cws_ts$temp - cws_ts$temp_dec
  cws_ts$d_temp_med <- cws_ts$temp - cws_ts$temp_med
  
  # -- compute for each cws its daily trend
  cws_day <- cws_ts |>
    group_by(time = lubridate::hour(time), id = id) |>
    dplyr::summarize(d_temp_min = median(d_temp_min, na.rm = T), 
                     d_temp_dec = median(d_temp_dec, na.rm = T),
                     d_temp_med = median(d_temp_med, na.rm = T), 
                     temp = median(temp, na.rm = T), n = dplyr::n()) |>
    data.frame()  
  return(cws_day)
}

manage_na <- function(data, na_thresh = 0.1) {
  # remove lines where temp is na
  output <- data[which(!(is.na(data$temp))), ]
  # timeserie length
  n_tot <- as.numeric(difftime(max(output$time),
                               min(output$time),
                               unit = "hour")) + 1
  n_thresh <- n_tot * (1 - na_thresh)
  # remove stations with more than na_tresh % of na
  n <- output |>
    group_by(id) |>
    dplyr::summarize(n = dplyr::n()) |>
    data.frame()
  keep_id <- unique(n[which(n$n >= n_thresh), c("id")])
  output <- output[which(output$id %in% keep_id), ]
  return(output)
}

plot_daily_trend <- function(cws_daily_trend) {
    p <- ggplot(cws_daily_trend) +
      geom_line(aes(x = time,
                    y = d_temp_dec,
                    group = as.factor(id),
                    color = as.factor(id))) +
      geom_hline(yintercept = 0, color = "red", size = 1) +
      scale_x_continuous(breaks = seq(0, 23, 6),
                         minor_breaks = seq(0, 23, 1),
                         limits = c(0, 23)) +
      scale_y_continuous(breaks = seq(-6, 10, 2),
                         minor_breaks = seq(-6, 10, 1),
                         limits = c(-6, 10)) +
      xlab("Time of the day (h)") +
      ylab(latex2exp::TeX("$T_{cws}-T_{q_{0.1}}$ (°C)")) +
      theme(
        axis.text = element_text(size = 12),
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


create_curves <- function(daily_trend, seed = 50, type = "raw") {
  set.seed(seed)
  myts <- daily_trend
  ids <- unique(myts$id)
  stations <- data.frame("id" = ids, "num" = seq(1, length(ids), 1))
  myts <- merge(myts, stations, by = "id")
  
  # reshaping to matrix signals (each row corresponds to a timeserie)
  mdata <- reshape2::melt(myts[, c('time', 'd_temp_dec', 'num')], 
                          id = c('time', 'num'))
  signals <- as.matrix(reshape2::acast(mdata, num ~ time)[, -1])
  signals <- scale(signals)
  if (type == "raw") {
    set <- signals
  } else {
    # wavelet decomposition
    nlevels     <- 4 # 2 ** 4 = 16
    coefs       <- NULL # table de coefficients d'ondelettes
    threshcoefs <- NULL # table de coefficients d'ondelettes seuillés
    
    for (station in 1:nrow(signals[, 4:(2^nlevels + 3)])) {
      x <- signals[station, 4:(2^nlevels + 3)]
      wt <- waveslim::dwt(x, wf = "haar", n.levels = nlevels)
      threshwt <- waveslim::universal.thresh(wt, max.level = 4, hard = T)
      coefs <- rbind(coefs, unlist(wt))
      threshcoefs <- rbind(threshcoefs, unlist(threshwt))
    }
    coefs <- data.frame(coefs)
    threshcoefs <- data.frame(threshcoefs)
    
    # -- feature selection
    set1 <- coefs # -- all wavelets coef
    set2 <- coefs[, 1:2^nlevels / 2] # -- thin coefs
    set3 <- coefs[, (2^nlevels / 2 + 2):(2^nlevels)] # -- coarse coefs
    set4 <- threshcoefs # -- coef thresh
    if (type == "coefs") {
      set <- set1
    } else if (type == "thincoefs") {
      set <- set2
    } else if (type == "coarsecoefs") {
      set <- set3
    } else if (type == "coefsthresh") {
      set <- set4
    } else {
      set <- NA
    }
  }
  set <- set |>
    #scale() |> 
    data.frame()
  rownames(set) <- stations$id
  return(set)
}


clustering_ts <- function(curves, k) {
  factoextra::fviz_nbclust(curves, FUN = kmeans, 'wss', k.max = 25)
  km_res <- kmeans(curves, centers = k, iter.max = 10, nstart = 1)
  output <- data.frame("id" = names(km_res$cluster), "clust" = km_res$cluster)
  return(output)
}


# -------------------------
# add neigh info
# -------------------------

mat.dist.cws <- function(cws){
  # cws: dataframe with colnames c("lat", "lon", "id")
  
  # RESULT 
  # return haversine distance matrix between each station (m)
  
  # turn into spatialPoints 
  # station metadata (one station is not dupplicated) 
  cws.sp <- na.omit(unique(cws[, c("lon", "lat", "id")]))
  cws.sp <- aggregate(cbind(lon=lon, lat=lat) ~ id, data=cws.sp, FUN="mean")
  coordinates(cws.sp) <- ~lon+lat
  proj4string(cws.sp) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs")
  cws.sp$id <- as.character(cws.sp$id)
  
  # d_ij : haversine distance between ith et la jth station (m)
  require(geosphere)
  d <- distm(cws.sp, fun=distHaversine)
  
  cws.sp <- as.data.frame(cws.sp)
  
  colnames(d) <- cws.sp[,c("id")]
  rownames(d) <- cws.sp[,c("id")]
  
  return(d)
}



add_neigh.info.cws <- function(cws, k=NA, radius){
  
  # cws: dataframe with colnames c("lat", "lon", "temp", "time", "id")
  # k: number of neighbors to consider
  # radius: buffer radius in meters
  
  # RESULT 
  # same cws dataframe with a boolean column far.from.knn 
  
  require(dplyr)
  options(dplyr.summarise.inform = FALSE)
  require(FNN)
  
  df.list <- vector(mode = "list", length = length(time))
  
  # time is a vector with each hour referenced in cws
  ts   <- as.POSIXct(min(cws$time), tz="UTC")
  te   <- as.POSIXct(max(cws$time), tz="UTC")
  time <- seq(from = ts, to = te, by = "1 hour")
  
  for (i in seq(1, length(time), by=1)) {
    
    hour <- as.POSIXct(time[i], tz='UTC')
    cws.h <- cws[which((between(cws$time, hour, hour+hours(1)-seconds(1)))),]
    
    if (nrow(cws.h)<k){next}   
    
    # ---------------------------------------
    # METHOD 1: choose a number of neighbors
    # ---------------------------------------
    if(F){
      knn <- get.knnx(cws[, c("lon", "lat")], cws[, c("lon", "lat")], k=k+1, 					algo="kd_tree")	
      # we drop first column because it is the station itself		
      vector <- cws.h[knn$nn.index[,2:k+1],c("temp")] 
      # row(matrix)=obs, col(matrix)=temp of knn
      matrix <- matrix(vector, nrow=nrow(cws.h), ncol=k) 
      # compute the median to add it in temp.knn column
      # note: we can create here a weighted mean function of the distance
      cws.h$temp.knn <- apply(matrix, 2, function(x) median(x))			
    }
    
    # ---------------------------------------
    # METHOD 2: choose a buffer around obs
    # and compute the median of neighbors
    # ---------------------------------------
    
    require(sp)
    d 	    <- mat.dist.cws(cws.h)
    pairs.index <- arrayInd(which(d<=radius & d!=0), dim(d)) 
    pairs.id    <- data.frame(id=rownames(d)[pairs.index[,1]], 
                              neigh=colnames(d)[pairs.index[,2]])
    pairs.id    <- merge(pairs.id, cws.h[,c("id", "temp")], 
                         by.x="neigh", by.y="id", all.x=T)
    groupby     <- pairs.id |> group_by(id) |> 
      dplyr::summarise(nb.neigh=n(), temp_med_neigh=median(temp))
    groupby     <- as.data.frame(groupby)
    cws.h       <- merge(cws.h, groupby, by="id", all.x=T)
    
    # register all in a new dataframe	
    df.list[[i]] <- cws.h
  }
  
  cws <- ldply(df.list, data.frame)
  #cws.h$far.from.knn <- abs(cws.h$temp - cws.h$temp.knn)>1
  cws$far.from.nn <- abs(cws$temp - cws$temp_med_neigh)>1
  cws$nb.neigh[which(is.na(cws$nb.neigh))] <- 0
  
  return(list(cws=cws))		
  
}




