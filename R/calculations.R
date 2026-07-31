# The numerical core of the Assay Calculator.  Everything in this file is a
# plain function of its arguments, so it can be tested without a Shiny session.

# Normalise one well of a calcium measurement.  Every value is expressed
# relative to the luminescence still remaining in the well (L/Lmax), which
# depends on the interval between two measurement points.
norm.well <- function(x, interval = 10){
  if(sum(x) == 0) return(x) #avoids errors in empty sets
  cs <- rev(cumsum(rev(x))); #cumulative sum
  xn <- x/(interval*(cs))*1000;
  return(xn)
}

# Read, sort and normalise one measurement file.
#
#   dc          number of discharge measurement points to cut off at the end
#   data_type   1 = show raw data, 2 = show normalized data
#   assay_type  1 = calcium, 2 = ROS
#   interval    seconds between two measurement points
calculate_data <- function(inputfile, dc, data_type, assay_type, interval = 10){

  rawdata <- read_measurement_file(inputfile)

  if (is.null(rawdata))
    return(NULL)

  rawdata <- rawdata[ , mixedsort(names(rawdata))] #this sorts the rawdata for well names, requires gtools

  normdata <- colwise(norm.well)(rawdata, interval = interval) # colwise needs library(plyr)
  ms_rawdata <- rawdata[1:(nrow(rawdata)-dc),] # isolates MS values of raw data
  ms_normdata <- normdata[1:(nrow(normdata)-dc),] # isolates MS values of normalized data
  ms_normdata[is.nan(colSums(ms_normdata))] <- 0 #first replace all colSums resulting in NaNs with zeros

  ms_rawdata$time <- measurement_time(nrow(ms_rawdata), interval)
  ms_normdata$time <- measurement_time(nrow(ms_normdata), interval)

  ms_rd_melted <- melt(ms_rawdata, id.vars = "time", variable.name = "well") # formatting for ggplot
  ms_nd_melted <- melt(ms_normdata, id.vars = "time", variable.name = "well") # formatting for ggplot

  plate_mean <- data.frame(time = ms_normdata$time,
                           values = well_rowmeans(ms_normdata))

  if (data_type == 1){
    chosen_set <- ms_rd_melted
  } else if (assay_type == 2){
    chosen_set <- ms_rd_melted
  }else {
    chosen_set <- ms_nd_melted
  }

  if (ncol(rawdata) > 96){
    well_plate <- 384
  } else {
    well_plate <- 96
  }

  calc_output <- list("ms_rawdata" = ms_rawdata,
                      "ms_normdata" = ms_normdata,
                      "ms_rd_melted" = ms_rd_melted,
                      "ms_nd_melted" = ms_nd_melted,
                      "chosen_set" = chosen_set,
                      "yrange" = c(min(chosen_set$value), max(chosen_set$value)),
                      "xrange" = c(min(chosen_set$time), max(chosen_set$time)),
                      "well_plate" = well_plate,
                      "interval" = interval,
                      "plate_mean" = plate_mean)

  return(calc_output)
}

# Time stamps of n measurement points, in seconds.
measurement_time <- function(n, interval = 10){
  return(seq(0, (n - 1) * interval, interval))
}

# The names of the well columns of a measurement data frame.
well_columns <- function(data){
  return(setdiff(colnames(data), "time"))
}

# Mean over all wells of a measurement data frame, per time point.  Wells that
# were never filled contribute nothing.
well_rowmeans <- function(data){
  wells <- data[well_columns(data)]
  wells[colSums(wells, na.rm = TRUE) == 0] <- NA
  return(rowMeans(wells, na.rm = TRUE))
}

# Mean curves, peak heights and the sheets of the Excel export for a calcium
# measurement, grouped by genotype x elicitor.
#
#   exclude4max  number of measurement points to skip before the peak search
group_means_maxima <- function(data, plate_layout, exclude4max = 4, interval = 10,
                               excluded_wells = NULL){

  if ((is.null(data))||(is.null(plate_layout)))
    return(NULL)

  raw_plate_layout <- annotated_wells(plate_layout)

  ms_normdata <- data$ms_normdata
  ms_normdata[colSums(ms_normdata) == 0] <- NA #fill empty columns with NA so they dont count into the mean

  # label every well with its genotype and elicitor for the export sheet
  ms_normdata_names <- ms_normdata
  annotation <- match(colnames(ms_normdata_names), plate_layout$well)
  labelled <- !is.na(annotation)
  colnames(ms_normdata_names)[labelled] <-
    paste(plate_layout$genotype[annotation[labelled]],
          plate_layout$elicitor[annotation[labelled]])

  groups <- well_groups(raw_plate_layout,
                        setdiff(colnames(ms_normdata), excluded_wells))

  if (length(groups) == 0)
    return(NULL)

  time <- measurement_time(nrow(ms_normdata), interval)

  all_means <- matrix(nrow = length(groups), ncol = 4, dimnames = NULL)
  all_means_graph <- matrix(nrow = length(groups)*nrow(ms_normdata), ncol = 5, dimnames = NULL)
  all_means_shaped <- matrix(nrow = 1, ncol = length(groups), dimnames = NULL)
  sd_shaped <- all_means_shaped
  graphs_shaped <- matrix(nrow = nrow(ms_normdata), ncol = length(groups), dimnames = NULL)
  graphs_shaped_sd <- graphs_shaped

  means <- matrix(nrow = nrow(ms_normdata), ncol = ncol(ms_normdata))
  colnames(means) <- colnames(ms_normdata)
  means[,"time"] <- time

  info <- c()

  i = 1
  i2 = 1

  for(group in groups){
    current_set <- ms_normdata[colnames(ms_normdata) %in% group$wells]

    current_rowmeans <- rowMeans(current_set, na.rm = TRUE)
    current_rowsds <- apply(current_set, 1, sd, na.rm = TRUE)

    # the first measurement points of a calcium assay are dominated by the
    # injection itself, so the peak is searched behind them
    search_from <- min(12 + exclude4max, length(current_rowmeans))
    peak <- which.max(current_rowmeans[search_from:length(current_rowmeans)]) + search_from - 1

    if(length(peak) == 1){
      current_max <- current_rowmeans[peak]
      current_sd <- current_rowsds[peak]
    } else {
      # every well of this group was empty
      current_max <- NA_real_
      current_sd <- NA_real_
    }

    all_means[i,] <- c(group$elicitor, group$genotype, current_max, current_sd)
    all_means_shaped[1,i] <- current_max
    sd_shaped[1,i] <- current_sd
    all_means_graph[i2:(i2+length(current_rowmeans)-1),1] <- rep(group$elicitor, length(current_rowmeans))
    all_means_graph[i2:(i2+length(current_rowmeans)-1),2] <- rep(group$genotype, length(current_rowmeans))
    all_means_graph[i2:(i2+length(current_rowmeans)-1),3] <- current_rowmeans
    all_means_graph[i2:(i2+length(current_rowmeans)-1),4] <- current_rowsds
    all_means_graph[i2:(i2+length(current_rowmeans)-1),5] <- time
    graphs_shaped[,i] <- current_rowmeans
    graphs_shaped_sd[,i] <- current_rowsds

    means[,colnames(means) %in% group$wells] <- current_rowmeans

    info <- append(info, group$label)
    i = i+1
    i2 = i2 + length(current_rowmeans)
  }

  df_mean <- data.frame(means, check.names = FALSE)
  mean_melted <- melt(df_mean, id.vars = "time", variable.name = "well") # formatting for ggplot

  colnames(graphs_shaped) <- info
  colnames(graphs_shaped_sd) <- rep("SD", ncol(graphs_shaped_sd))
  colnames(all_means_shaped) <- info
  colnames(sd_shaped) <- rep("SD", ncol(sd_shaped))
  graphs_shaped_w_sd <- interleave_with_spread(graphs_shaped, graphs_shaped_sd, time)
  all_means_shaped_w_sd <- interleave_with_spread(all_means_shaped, sd_shaped)
  graphs_shaped <- cbind(time, graphs_shaped)
  colnames(graphs_shaped)[1] <- "time"

  total_output <- list("all_means" = all_means,
                       "all_means_shaped" = all_means_shaped,
                       "all_means_shaped_w_sd" = all_means_shaped_w_sd,
                       "all_means_graph" = all_means_graph,
                       "graphs_shaped" = graphs_shaped,
                       "graphs_shaped_w_sd" = graphs_shaped_w_sd,
                       "mean_melted" = mean_melted,
                       "ms_normdata_names" = ms_normdata_names)

  return(total_output)
}

# Every genotype x elicitor combination that actually has wells on the plate,
# in the order they appear in the layout.
well_groups <- function(raw_plate_layout, available_wells = NULL){

  groups <- list()

  for(eli in unique(raw_plate_layout$elicitor)){
    for(gen in unique(raw_plate_layout$genotype)){
      wells <- raw_plate_layout$well[raw_plate_layout$elicitor == eli &
                                     raw_plate_layout$genotype == gen]
      if(is.null(available_wells) == FALSE){
        wells <- wells[wells %in% available_wells]
      }
      if(length(wells) == 0) next

      groups[[length(groups) + 1]] <- list("elicitor" = eli,
                                           "genotype" = gen,
                                           "label" = paste(gen, eli, sep = " "),
                                           "wells" = wells)
    }
  }

  return(groups)
}

# Put a value column and its spread column next to each other, the shape the
# Excel sheets use.
interleave_with_spread <- function(values, spread, time = NULL){
  combined <- cbind(values, spread)
  combined <- combined[, c(matrix(1:ncol(combined), nrow = 2, byrow = TRUE)), drop = FALSE]
  if(is.null(time) == FALSE){
    combined <- cbind(time, combined)
    colnames(combined)[1] <- "time"
  }
  return(combined)
}

# Normalise the single wells of a ROS measurement.  Every well is blanked with
# the mean of the control wells of its own genotype and then corrected for its
# own background.  Averaging these wells per group reproduces the mean curves
# of ros_normalize() exactly.
ros_well_values <- function(rawdata, raw_plate_layout,
                            bg_values = 10, how_many_bg_values = 5){
  wells <- raw_plate_layout$well[raw_plate_layout$well %in% colnames(rawdata)]
  values <- rawdata[wells]

  for(gen in unique(raw_plate_layout$genotype)){
    control_wells <- raw_plate_layout$well[raw_plate_layout$genotype == gen &
                                           raw_plate_layout$elicitor == "control"]
    control_wells <- control_wells[control_wells %in% colnames(values)]
    if(length(control_wells) == 0) next

    blank <- rowMeans(values[control_wells])

    genotype_wells <- raw_plate_layout$well[raw_plate_layout$genotype == gen]
    genotype_wells <- genotype_wells[genotype_wells %in% colnames(values)]
    values[genotype_wells] <- values[genotype_wells] - blank
  }

  background_rows <- background_window(bg_values, how_many_bg_values)
  background <- colMeans(values[background_rows, , drop = FALSE])
  values <- as.data.frame(sweep(as.matrix(values), 2, background))

  return(values)
}

# The measurement points that are averaged into the background.
#
# Note that this window ends one point before the elicitation: with the
# defaults (10 points recorded before the elicitor is added, 5 of them used)
# it covers the points 5 to 9, not 6 to 10.  That is how the original
# implementation behaved and it is kept deliberately, so that switching to
# this version does not silently move every ROS result.
background_window <- function(bg_values, how_many_bg_values){
  how_many_bg_values <- max(1, min(how_many_bg_values, bg_values - 1))
  return((bg_values - how_many_bg_values):(bg_values - 1))
}

# Mean curves and peak heights of a ROS measurement, blanked against the
# control wells of the same genotype and corrected for the background.
ros_normalize <- function(rawdata, plate_layout, exclude4max = 4,
                          bg_values = 10, how_many_bg_values = 5,
                          excluded_wells = NULL){

  if ((is.null(rawdata))||(is.null(plate_layout)))
    return(NULL)

  raw_plate_layout <- annotated_wells(plate_layout)
  groups <- well_groups(raw_plate_layout,
                        setdiff(colnames(rawdata), excluded_wells))

  if (length(groups) == 0)
    return(NULL)

  if (!("control" %in% raw_plate_layout$elicitor))
    return(NULL)

  sdom.calculate <- function(x){
    return(sd(x)/sqrt(length(x)))
  }

  labels <- vapply(groups, function(group) group$label, character(1))

  dataset_mean <- matrix(nrow = nrow(rawdata), ncol = length(groups), dimnames = NULL)
  dataset_sd <- dataset_mean
  dataset_info <- matrix(nrow = 2, ncol = length(groups), dimnames = NULL)

  for(i in seq_along(groups)){
    current_set <- rawdata[colnames(rawdata) %in% groups[[i]]$wells]
    dataset_mean[,i] <- rowMeans(current_set)
    dataset_sd[,i] <- apply(current_set, 1, sdom.calculate)
    dataset_info[1,i] <- groups[[i]]$genotype
    dataset_info[2,i] <- groups[[i]]$elicitor
  }

  colnames(dataset_mean) <- labels
  colnames(dataset_sd) <- labels
  colnames(dataset_info) <- labels
  rownames(dataset_info) <- c("genotype", "elicitor")

  # 1 subtract the control of the same genotype
  normdata <- dataset_mean
  for(i in seq_along(groups)){
    control <- which((dataset_info["genotype",] == groups[[i]]$genotype) &
                     (dataset_info["elicitor",] == "control"))
    if(length(control) == 0) next
    normdata[,i] <- dataset_mean[,i] - dataset_mean[,control[1]]
  }

  # 2 subtract the background of the measurement points before the elicitation
  background_rows <- background_window(bg_values, how_many_bg_values)
  background <- colMeans(normdata[background_rows, , drop = FALSE])
  normdata <- sweep(normdata, 2, background)

  # 3 prepare for plotting with ggplot2
  time <- seq(-bg_values, nrow(normdata) - bg_values - 1)
  rownames(normdata) <- time
  rownames(dataset_sd) <- time

  normdata_melted <- melt(normdata, varnames = c("time", "name"),
                          value.name = "values", as.is = TRUE)
  normdata_melted$genotype <- dataset_info["genotype", normdata_melted$name]
  normdata_melted$elicitor <- dataset_info["elicitor", normdata_melted$name]

  sd_melted <- melt(dataset_sd, varnames = c("time", "name"),
                    value.name = "sd", as.is = TRUE)
  normdata_melted$sd <- sd_melted$sd
  class(normdata_melted$time) <- "numeric"

  # peak height of every mean curve, the controls are the blank and carry none
  search_from <- min(bg_values + exclude4max + 1, nrow(normdata))
  peaks <- apply(normdata[search_from:nrow(normdata), , drop = FALSE], 2, which.max) +
    search_from - 1
  keep <- dataset_info["elicitor",] != "control"

  maxima <- normdata[cbind(peaks, seq_along(peaks))][keep]
  maxima_sd <- dataset_sd[cbind(peaks, seq_along(peaks))][keep]
  names(maxima) <- labels[keep]
  names(maxima_sd) <- labels[keep]

  maxima_melted <- data.frame(
    values = unname(maxima),
    name = labels[keep],
    genotype = unname(dataset_info["genotype", keep]),
    elicitor = unname(dataset_info["elicitor", keep]),
    sd = unname(maxima_sd),
    stringsAsFactors = FALSE
  )

  sd2 <- dataset_sd
  colnames(sd2) <- replicate(ncol(sd2), "SDOM")
  normdata_w_sd <- interleave_with_spread(normdata, sd2, time)
  normdata <- cbind(time, normdata)
  colnames(normdata)[1] <- "time"

  output <- list("normdata_melted" = normdata_melted,
                 "normdata" = normdata,
                 "normdata_sd" = dataset_sd,
                 "normdata_w_sd" = normdata_w_sd,
                 "maxima_melted" = maxima_melted,
                 "maxima" = maxima,
                 "maxima_sd" = maxima_sd,
                 "maxima_w_sd" = cbind(maxima, maxima_sd))

  return(output)
}

# Area under one curve between two positions of the x axis.
#
# "trapezoid" integrates the curve over x, so the result carries the unit of
# the x axis and stays correct if the measurement interval ever changes.
# "sum" simply adds up the values inside the window.
area_under_curve <- function(x, y, from = min(x), to = max(x), method = "trapezoid"){
  inside <- !is.na(x) & !is.na(y) & x >= from & x <= to
  x <- x[inside]
  y <- y[inside]

  if(length(y) == 0) return(NA_real_)
  if(method == "sum") return(sum(y))
  if(length(y) == 1) return(0)

  return(sum(diff(x) * (y[-length(y)] + y[-1]) / 2))
}

# Area under the curve of every well, summarised per genotype and elicitor.
# The wells are integrated one by one so that the spread between the replicates
# can be reported next to the mean, the same way the maxima carry their SD.
group_auc <- function(well_values, time, raw_plate_layout, from, to,
                      method = "trapezoid", spread = "sd"){

  genotypes <- unique(raw_plate_layout$genotype)
  result <- NULL

  for(group in well_groups(raw_plate_layout, colnames(well_values))){
    current_set <- well_values[colnames(well_values) %in% group$wells]

    aucs <- vapply(current_set, function(values){
      area_under_curve(time, values, from, to, method)
    }, numeric(1))
    aucs <- aucs[!is.na(aucs)]
    if(length(aucs) == 0) next

    current_sd <- sd(aucs)
    if(spread == "sdom") current_sd <- current_sd/sqrt(length(aucs))

    result <- rbind(result, data.frame(
      elicitor = group$elicitor,
      genotype = group$genotype,
      values = mean(aucs),
      sd = current_sd,
      n = length(aucs),
      stringsAsFactors = FALSE
    ))
  }

  if(is.null(result)) return(NULL)

  result$genotype <- factor(result$genotype, levels = genotypes)
  rownames(result) <- NULL
  return(result)
}

# Express the areas relative to one of the groups, which becomes 100 percent.
relative_auc <- function(auc, reference){

  if((is.null(auc))||(is.null(reference))||(!nzchar(reference)))
    return(auc)

  labels <- paste(auc$genotype, auc$elicitor)
  index <- match(reference, labels)

  if(is.na(index)) return(auc)

  base <- auc$values[index]
  if((is.na(base))||(base == 0)) return(auc)

  auc$values <- auc$values / base * 100
  auc$sd <- auc$sd / abs(base) * 100
  return(auc)
}

# Axis label for the area under the curve.
auc_label <- function(assay_type, method, reference = NULL){

  if((is.null(reference) == FALSE) && nzchar(reference))
    return(paste0("% of ", reference))

  unit <- if(assay_type == 1) "L/Lmax" else "Relative Luminescence"
  if(method == "sum") return(paste("Sum of", unit))
  x_unit <- if(assay_type == 1) "s" else "measurement points"
  return(paste(unit, "x", x_unit))
}

# Scale and label the time axis of the kinetics plots.  Calcium measurements
# carry their time in seconds, ROS measurements are counted in measurement
# points and need the interval between two points to be shown as a time.
scale_time_axis <- function(time, assay_type, unit, interval = 60){
  if((assay_type != 1) && (unit == "points")){
    return(list("time" = time, "label" = "Measurement points"))
  }

  seconds <- if(assay_type == 1) time else time * interval

  if(unit == "min"){
    return(list("time" = seconds/60, "label" = "Time [min]"))
  }

  return(list("time" = seconds, "label" = "Time [s]"))
}

# The mean curves of the current plate, in the shape of a reference file.
plate_reference_curves <- function(complete_data, assay_type){

  curves <- if(assay_type == 1) complete_data$graphs_shaped else complete_data$normdata

  if (is.null(curves))
    return(NULL)

  curves <- as.data.frame(curves, check.names = FALSE, stringsAsFactors = FALSE)
  colnames(curves)[1] <- "time"
  rownames(curves) <- NULL
  return(curves)
}
