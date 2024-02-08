library(ncdf4); library(ncdf4.helpers)
library(terra); library(data.table)

#' Process NetCDF data and extract values for specified locations
#'
#' This function reads NetCDF files from a specified directory, extracts relevant
#' information for specified locations, and returns a list of data tables with columns for time,
#' location_id, and corresponding values
#'
#' @param data_directory path to the directory containing the files
#' @param file_pattern file pattern to match NetCDF files
#' @param location_ids vector of location IDs to extract values
#'
#' @return a list of data tables split by location_id
#'
#' @examples
#' extract_values_from_nc(data_directory = "./data", file_pattern = ".nc", location_ids = c(327, 328, 329))
#'
#' @export
extract_values_from_nc <- function(data_directory = "./data",
                                   file_pattern = ".nc",
                                   location_ids)  {
  # Checking if the directory exists
  if (!dir.exists(data_directory)) {
    message("Specified directory does not exist")
    return(NULL)
  }

  # Listing NetCDF files
  files <- list.files(path = data_directory,
                      recursive = TRUE,
                      pattern = file_pattern,
                      full.names = TRUE)

  # Checking if any files match the pattern
  if (length(files) == 0) {
    stop("No files matching the specified pattern were found")
  }

  # Using lapply to read and process each file
  data_all <- lapply(
    X = files,
    FUN = function(file) {
      e <- try({
        nc <- nc_open(filename = file)

        # Extracting relevant variables
        lon <- ncvar_get(nc = nc, varid = "lon")
        lat <- ncvar_get(nc = nc, varid = "lat")
        pr <- ncvar_get(nc = nc, varid = "pr")
        time <- as.POSIXct(nc.get.time.series(f = nc), format = "%Y-%m-%d %H:%M:%S")

        nc_close(nc = nc)

        #c Creating a terra raster object
        r <- rast(x = pr)
        ext(x = r) <- c(range(lon), range(lat))
        crs(x = r) <- "epsg:4326"

        # Extracting values for specified location IDs
        xy <- xyFromCell(object = r, cell = location_ids)
        val <- t(x = extract(x = r, y = xy))

        # Creating a data.table with time and values
        data <- data.table(time = time, value = val)
      }, silent = TRUE)

      if (inherits(x = e, what = "try-error")) {
        return(NULL)
      } else {
        return(data)
      }
    }
  )

  # Combining the list of data tables into a single data table
  data_all <- rbindlist(l = data_all)

  # Reshaping from wide to long format
  data_all_m <- melt(data = data_all,
                     id.vars = "time",
                     variable.name = "location_id")

  #c Calculating the maximum value for each location and year
  max_values <- data_all_m[, .(max_value = max(value)),
                           by = .(location_id, year(x = time))]

  # Splitting table by location_id
  split_data <- split(x = data_all_m,
                      f = data_all_m$location_id)

  # Returning the list of data tables split by location_id
  return(split_data)

}
