## @knitr points_as_points

# CONVERTS A DATAFRAME WITH PGSQL COORDINATES TO GGPLOT2 FRIENDLY COORDINATES
# ARGUMENTS
# points_tibble: tibble including a coordinate column
# points_column: character variable denoting the points column in tibble 
# RETURNS
# points_vector: 1-column tibble with coordinate column where coordinates are converted
points_as_points <- function(points_tibble, points_column) {
  # LOADS LIBRARIES IF NEEDED
  require(tidyverse)
  require(rgeos)
  
  # Get points column as a vector
  points_vector <- points_tibble %>% rename("points"=points_column) %>% select(points)
  # CONVERT PGSQL POINT COORDINATES TO WKT
  # Denote polygons as such
  points_vector$points <- paste("POINT ", points_vector$points, sep = "")
  # Remove pgsql characters not used, first the extra commas
  # From between negative lon coordinates
  points_vector$points <- gsub(",-", " -", points_vector$points)
  # The between positive (could be aggregated to single expression above)
  points_vector$points <- gsub(",([0-9])", " \\1", points_vector$points)
  # Then between coordinate pairs
  points_vector$points <- gsub("\\),\\(", ",", points_vector$points)
  
  # Swap coordinates
  points_vector$points <- gsub("(-?[0-9]{2,3}\\.[0-9]+)( )(-?[0-9]{2,3}\\.[0-9]+)", "\\3 \\1", points_vector$points)
  
  # CREATE COORDINATE MATRIX
  points_df <- as.data.frame(matrix(nrow=0, ncol=2))
  colnames(points_df) <- c("x", "y")
  for (i in 1:length(points_vector$points)) {
    # Parse WKT
    wkt <- readWKT(points_vector$points[i])
    points_df[i, "x"] <- wkt$x
    points_df[i, "y"] <- wkt$y
  }
  
  # ADD ID if available
  if("id" %in% colnames(points_tibble)) {
    points_df$id <- points_tibble$id
  }

  # PUT THE POINTS COLUMN BACK TO ITS PLACE AND RETURN TIBBLE
  return(points_df)
}