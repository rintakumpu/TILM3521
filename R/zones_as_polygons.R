## @knitr zones_as_polygons

# ARGUMENTS
# zones: tibble created from CSV dump of (subset of) zones admin table
# RETURNS
# zones_for_plotting: tibble of zone polygons in ggplot2 friendly format
zones_as_polygons <- function(zones) {
  # LOADS LIBRARIES IF NEEDED
  require(tidyverse)
  require(rgeos)
  
  # INTERMEDIATE STEP
  # CONVERT PGSQL POLYGON COORDINATES TO WKT
  # Denote polygons as such
  zones$coordinates <- paste("POLYGON ", zones$coordinates, sep = "")
  # Remove pgsql characters not used, first the extra commas
  # From between negative lon coordinates
  zones$coordinates <- gsub(",-", " -", zones$coordinates)
  # The between positive (could be aggregated to single expression above)
  zones$coordinates <- gsub(",([0-9])", " \\1", zones$coordinates)
  # Then between coordinate pairs
  zones$coordinates <- gsub("\\),\\(", ",", zones$coordinates)
  
  # Swap coordinates
  zones$coordinates <- gsub("(-?[0-9]{2,3}\\.[0-9]+)( )(-?[0-9]{2,3}\\.[0-9]+)", " \\3 \\1", zones$coordinates)
  # And close the polygons by copying the last coordinate as the first
  # What this does is a non-greedy lookforward till a coordinate with two closing parentheses
  # and then pastes that in the start, after POLYGON (( and rest of the string. Works like regexp.
  # Just don't touch it.
  zones$coordinates <- gsub("(POLYGON \\(\\( )((.+?)(?=(-?[0-9]{2,3}\\.[0-9]+ -?[0-9]{2,3}\\.[0-9]+\\)\\))))(-?[0-9]{2,3}\\.[0-9]+ -?[0-9]{2,3}\\.[0-9]+)(\\)\\))", "POLYGON \\(\\(\\5, \\2\\4", zones$coordinates, perl=TRUE)
  # And now we have nice WKT strings we can actually use as coordinates for plotting sites.
  # Boy this is easy. I wonder if postgresql would have some "export WKT" option :\\ well, it's too
  # late now, I already wasted my afternoon with this shit.
  
  # CREATE COORDINATE MATRIX
  zones_for_plotting <- as.data.frame(matrix(nrow=0, ncol=6))
  colnames(zones_for_plotting) <- c("x", "y", "id", "name", "label_position_x", "label_position_y")
  for (i in 1:length(zones$coordinates)) {
    # Parse WKT
    wkt <- readWKT(zones$coordinates[i])
    # Get the indices where in the data frame to save polygon information
    no_of_coordinates <- length(wkt@polygons[[1]]@Polygons[[1]]@coords[,1])
    starting_index <- length(zones_for_plotting$x)+1
    # Get polygons, save ID, name and label position too
    zones_for_plotting[starting_index:(starting_index+no_of_coordinates-1),1] <- wkt@polygons[[1]]@Polygons[[1]]@coords[,1]
    zones_for_plotting[starting_index:(starting_index+no_of_coordinates-1),2] <- wkt@polygons[[1]]@Polygons[[1]]@coords[,2]
    zones_for_plotting[starting_index:(starting_index+no_of_coordinates-1),3] <- rep(zones$id[i], no_of_coordinates)
    zones_for_plotting[starting_index:(starting_index+no_of_coordinates-1),4] <- rep(zones$name[i], no_of_coordinates)
    zones_for_plotting[starting_index:(starting_index+no_of_coordinates-1),5] <- wkt@polygons[[1]]@Polygons[[1]]@labpt[1]
    zones_for_plotting[starting_index:(starting_index+no_of_coordinates-1),6] <- wkt@polygons[[1]]@Polygons[[1]]@labpt[2]
  }
  
  # RETURNS ZONES FOR PLOTTING
  return(zones_for_plotting)
  
}