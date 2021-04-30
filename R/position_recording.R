## @knitr position_recording

# POSITIONS RECORDING USING HATA-OKUMURA + NLS MODELS

# ARGUMENTS:
# rssi_data: data.frame of RSSI observations from all sensors
# location: integer, recording id
# recordings_xy: data.frame of recording x, y coordinates + ids
# sensors_xy: data.frame of sensor x, y coordinates
# site_map: ggplot of site map where to plot sensors, RSSI circles, predicted positions and more. If NA no map is plotted or returned.
# ghz: character, one of "2.4Ghz", "5Ghz", "BLE", the frequency of recordings, used for Hata-Okumura constants
# rssi_statistic: function, function to apply to RSSI data frame for summarizing
# arrows: logical, plot RSSI as arrows? If false circles are used.
# rows_to_use: logical, how many rows of RSSI observations to use as a random sample for summarizing, if NA all observations/rows are used
# omni: logical, include omnidirectional sensor in positioning
# verbose: logical, print map and RSSI summarization table
# confidence_level: numeric, a numeric value between (0,1), the confidence level for ellipses plotted around predicted positions, NA if no ellipses wanted
# sensor_weight: character, one of "none", "inverse_sum_of_areas", "inverse_variance", weight given to sensors when fittin NLS model
# plot_all_observations: logical, plot predictions for all of the observations in RSSI data frame (instead of summarizing). if TRUE circles are not plotted.
# antenna_offset: numeric, how much the circle center should be dragged back to the direction of the antenna
#                 and simultaneously how much the circle radius should be extended
#                 value between 0 (no offset) to 0.5 (makes the antenna behave as omnidirectional).
#                 Passed on to calculate_sensor method.
# separate_antennas: logical, use separate path loss parameters for each antenna, passed on to calculate_sensor method.
# X: numeric, override X value of get_propmodel_parameters() if no X_sd is set
# N: numeric, overrice N value of get_propmodel_parameters()
# extra_verbose: logical, print extensive output
# mle: logical, use MLE-based (unbiased distance calcuation), passed on to calculate_sensor
# linearization: logical, use linearizationn instead of NLS estimation
# reference_power: numeric vector of length 2, directed and omni reference powers, if NA use Friis' equation, passed on to calculate_sensor methods

# RETURNS
# a list consisting of:
# all_circles, data frame, the x, y, radius data frame used for fitting NLS model
# site_map_edit, ggplot, site map with RSSI / predictions plotted in it
# positioning_error, numeric, the positioning error in meters, if plot_all_observations is used the median of positioning errors
position_recording <- function(rssi_data, location, recordings_xy, sensors_xy, site_map = NA, ghz = "2.4Ghz", rssi_statistic = median, arrows = F, rows_to_use = NA, omni = F, verbose = T, confidence_level = NA, sensor_weight = "none", plot_all_observations = F, antenna_offset = 0, separate_antennas = F, circle_weight = "none", N = NA, X = NA, N_X = NA, extra_verbose = F, linearization = F, mle = F, reference_power = c(NA, NA)) {
  
  # Copy reference power vector to avoid namespace conflicts
  reference_power_input <- reference_power
  
  # If no location found, return NA
  if(length(rssi_data$location[rssi_data$location==location])==0){
    return(NA)  
  }
  
  # Save the original (non-summarized RSSI data if plotting all obsevations is requested)
  # Plus split data to directed and omni
  # Create indices used for splitting
  omni_index <- seq(1, ncol(rssi_data), by=nrow(sensors_xy))
  directional_index_matrix <- outer(X=seq(7, ncol(rssi_data), nrow(sensors_xy)), Y=c(0:3), FUN="+")
  directional_index <- c(1:5, c(t(directional_index_matrix)))
  rssi_data_original <- rssi_data
  rssi_data_original <- rssi_data_original %>% select(directional_index)
  rssi_original <- rssi_data_original[rssi_data_original$location==location,]
  omni_original <- rssi_data %>% select(omni_index)
  omni_subset_original <- omni_original[omni_original$location == location,]
  
  # Select rows based on recording
  omni_rssi <- rssi_data %>% select(seq(1, ncol(rssi_data), by=nrow(sensors_xy)))
  rssi_data <- rssi_data %>% select(directional_index)
  rssi <- rssi_data[rssi_data$location==location,]
  
  # Remove omnidirectional antenna as it will result in NA columns
  # then remove NA rows, original kept aside for evaluation pruposes
  omni_subset <- omni_rssi[omni_rssi$location == location,]
  # If no sensor 4 obs
  if(nrow(omni_subset) == 0) {
    omni_subset[1,1:(nrow(sensors_xy)+1)] <- c(location, rep(0, nrow(sensors_xy)))
  }
  
  # Sample dataset
  if(!is.na(rows_to_use) && !plot_all_observations) {
    if(rows_to_use > nrow(rssi)) {
      rows_to_use <- nrow(rssi)
    }
    rssi_sample <- sample_n(rssi, rows_to_use)
  } else {
    rssi_sample <- rssi
  }
  
  # How many locations to plot
  if(plot_all_observations) {
    if(is.na(rows_to_use)) {
      locations_to_plot <- nrow(rssi_original)
    } else {
      locations_to_plot <- rows_to_use
    }
  } else { 
    locations_to_plot <- 1}
  
  # Create vector/scalar of positioning errors
  positioning_error <- c()
  predicted_locations <- as.data.frame(matrix(nrow=0, ncol=2))
  colnames(predicted_locations) <- c("x", "y")
  
  #Highlight recording on the map
  #If no site map is provided, printing map is disabled
  if(!is.na(site_map)) {
    site_map_edit <- site_map + annotate("point", x=recordings_xy[location,"x"], y=recordings_xy[location,"y"], size=5, color="black") + annotate("text", x=recordings_xy[location,"x"], y=recordings_xy[location,"y"], label=location, size=3, color="white")
  } else {
    verbose <- F
    site_map_edit <- ggplot()
  }
  
  # Plot either 1 or n recording observations
  for(i in 1:locations_to_plot) {
    # Calculate mean for each antenna, print table if requested
    
    if(!plot_all_observations) {
      rssi_mean <- apply(rssi_sample[,2:ncol(rssi_sample)], 2, rssi_statistic, na.rm = T)
      rssi_mean_omni <- apply(omni_subset[,2:ncol(omni_subset)], 2, rssi_statistic, na.rm = T)
      rssi_mean_omni[is.infinite(rssi_mean_omni)] <- -110
      if(verbose) {
        rssi_mean_table <- as.data.frame(matrix(ncol=nrow(sensors_xy), nrow=5))
        colnames(rssi_mean_table) <- paste(LETTERS[1:nrow(sensors_xy)], unique(substr(names(rssi_mean), 1, 17)))
        rownames(rssi_mean_table) <- seq(0, 4)
        for(rssi_mean_table_i in 1:nrow(rssi_mean_table)) {
          rssi_mean_table[,rssi_mean_table_i] <- c(rssi_mean[(1+((rssi_mean_table_i-1)*4)):(4+((rssi_mean_table_i-1)*4))], rssi_mean_omni[rssi_mean_table_i])
        }
        pander(rssi_mean_table, align="l")
      }
    } else {
      rssi_mean <- as.numeric(rssi_original[i,2:ncol(rssi_original)]) %>% set_names(colnames(rssi_original[i,2:ncol(rssi_original)]))
      rssi_mean_omni <- as.numeric(omni_subset_original[i,2:ncol(omni_subset_original)]) %>% set_names(colnames(omni_subset_original[i,2:ncol(omni_subset_original)]))
      # If no sensor 4 obs found
      if(is.na((sum(rssi_mean_omni)))){
        rssi_mean_omni <- rep(0, nrow(sensors_xy))
      }
    }
    
    # Gets X either from the parameters or if set at X_sd.
    N_input <- N
    X_input <- X
    mle_input <- mle
    
    # For each sensor, sort by RSSI
    all_circles <- c()
    for(sensor_no in 1:nrow(sensors_xy)) {
      sensor_data <- sort(rssi_mean[(1+((sensor_no-1)*4)):(4+((sensor_no-1)*4))], decreasing = TRUE)
      sensor_data_omni <- rssi_mean_omni[sensor_no]
      N_X_input <- N_X
      sensor_result <- calculate_sensor(site_map_edit, sensors_xy, sensor_data, ghz, arrows, antenna_offset, separate_antennas, X=X_input, N=N_input, N_X=N_X_input, mle=mle_input, reference_power = reference_power_input[1])
      site_map_edit <- sensor_result[[1]]
      sensor_circles <- sensor_result[[2]]
      
      # If omni, calculate and add omni circles
      if (omni) {
        # If omni included
        omni_result <- calculate_sensor_omni(site_map_edit, sensors_xy, sensor_data_omni, ghz, separate_antennas, N_X_input, reference_power = reference_power_input[2])
        if(!is.na(site_map) && !plot_all_observations) {
          site_map_edit <- omni_result[[1]]
        }
        omni_circles <- omni_result[[2]]
        sensor_circles <- rbind(sensor_circles, omni_circles)
      }
      
      # Add sensor ID, drop NA results
      sensor_circles$sensor <- rep(sensor_no, nrow(sensor_circles))
      sensor_circles <- sensor_circles %>% drop_na()
      
      # Add circles to data frame
      if(is.null(all_circles)){
        all_circles <- sensor_circles
      } else {
        all_circles <- bind_rows(all_circles, sensor_circles)
      }
    }
    
    # Use only the strongest antennas
    #all_circles <- all_circles %>% group_by(sensor) %>% summarize(rssi=max(rssi)) %>% left_join(all_circles)
    # If two antennas equally strong, use the one with least variance
    all_circles <- all_circles %>% group_by(sensor) %>% filter(variance == min(variance))
    
    # Use only three antennas with highest RSSI
    all_circles <- arrange(all_circles, desc(rssi))[1:3,]
    
    # Weight by sensors
    if(sensor_weight == "none") {
      sensor_weights <- rep(1, nrow(all_circles))
    }
    if(sensor_weight == "inverse_sum_of_areas") {
      all_circles <- all_circles %>% left_join(all_circles %>% group_by(sensor) %>% summarize(statistic=sum(r_meters^2*pi)))
      sensor_weights <- 1/all_circles$statistic
    }
    if(sensor_weight == "inverse_variance") {
      all_circles <- all_circles %>% left_join(all_circles %>% group_by(sensor) %>% summarize(statistic=var(r_meters)))
      sensor_weights <- 1/all_circles$statistic
    }
    
    # TODO/NOTE: SITE-SPECIFIC
    site_map_edit <- site_map_edit + coord_cartesian(xlim=c(22.29454, 22.29498), ylim=c(60.44821, 60.44851))
    
    # Fit location model
    if(!linearization) {
      
      location_model <- fit_nls_model(as.data.frame(all_circles), sensor_weights = sensor_weights, circle_weight)
      
      if(!is.null(location_model)) {
        location_x <- location_model$m$getAllPars()[2]
        location_y <- location_model$m$getAllPars()[1]
      } else {
        # If no model could be fitted, skip this locations
        next
      }
    } else {
      location_model <- fit_ls_model(all_circles)
    }
    
    
    # Add predicted point + confidence ellipse to the map (note the mapping where x and y are reversed, this is because geosphere model puts lng before lat)
    if(!is.na(site_map)) {
      site_map_edit <- site_map_edit + annotate("point", x=location_x, y=location_y, size=2, color="red", alpha=0.8)
      if(!is.na(confidence_level) && confidence_level!=0) {
        confidence_ellipse <- confidenceEllipse(location_model, levels=confidence_level, draw=FALSE)
        confidence_ellipse <- as.data.frame(confidence_ellipse) %>% add_column(group = rep(88, nrow(confidence_ellipse)))
        site_map_edit <- site_map_edit + geom_polygon(confidence_ellipse, mapping=aes(x=y, y=x, group=group), linetype="dashed", color="purple", fill=NA)
      }
    }
    
    # Calculate error
    if(!plot_all_observations){
      positioning_error <- abs(distHaversine(c(location_y, location_x), c(recordings_xy[location,"y"], recordings_xy[location,"x"])))
      predicted_locations[1,"x"] <- location_x 
      predicted_locations[1,"y"] <- location_y 
    } else {
      predicted_locations[nrow(predicted_locations)+1,"x"] <- location_x 
      predicted_locations[nrow(predicted_locations),"y"] <- location_y 
      positioning_error[i] <- abs(distHaversine(c(location_y, location_x), c(recordings_xy[location,"y"], recordings_xy[location,"x"])))
    }
  }
  
  if(!is.na(site_map) && verbose) {
    plot(site_map_edit)
    cat("\n\n\\pagebreak\n")
  }
  
  # If all observations plotted, show median positioning error
  positioning_error <- median(positioning_error)
  # Returns circles data frame plus edited map
  return(list(all_circles, site_map_edit, positioning_error, predicted_locations, location_model))
}

# Custom function that annotates nice circles
gg_circle <- function(r, xc, yc, color="black", fill=NA, ...) {
  x <- xc + r*cos(seq(0, pi, length.out=100))
  ymax <- yc + r*sin(seq(0, pi, length.out=100))
  ymin <- yc + r*sin(seq(0, -pi, length.out=100))
  annotate("ribbon", x=x, ymin=ymin, ymax=ymax, color=color, fill=fill, ...)
}

# Custom function that annotates nice ellipses
gg_ellipse <- function(data, fill){
  edata <- as.matrix(data)
  ehull <- ellipsoidhull(edata, maxit=5000)
  phull <- as.data.frame(predict(ehull))
  data.frame(
    x=phull$V1, 
    y=phull$y, 
    fill=rep(fill, nrow(phull)),
    group=rep(10, nrow(phull))
  )
}