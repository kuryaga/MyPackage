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
  if (!dir.exists(data_directory)) {
    message("Specified directory does not exist")
    return(NULL)
  }

  files <- base::list.files(path = data_directory,
                            recursive = TRUE,
                            pattern = file_pattern,
                            full.names = TRUE)

  data_all <- base::lapply(
    X = files,
    FUN = function(file) {
      e <- base::try({
        nc <- ncdf4::nc_open(filename = file)

        lon <- ncdf4::ncvar_get(nc = nc, varid = "lon")
        lat <- ncdf4::ncvar_get(nc = nc, varid = "lat")
        pr <- ncdf4::ncvar_get(nc = nc, varid = "pr")
        time <- base::as.POSIXct(ncdf4.helpers::nc.get.time.series(f = nc), format = "%Y-%m-%d %H:%M:%S")

        ncdf4::nc_close(nc = nc)

        r <- terra::rast(x = pr)
        terra::ext(x = r) <- c(base::range(lon), base::range(lat))
        terra::crs(x = r) <- "epsg:4326"

        xy <- terra::xyFromCell(object = r, cell = location_ids)
        val <- base::t(x = terra::extract(x = r, y = xy))

        data <- data.table::data.table(time = time, value = val)
      }, silent = TRUE)

      if (base::inherits(x = e, what = "try-error")) {
        return(NULL)
      } else {
        return(data)
      }
    }
  )

  data_all <- data.table::rbindlist(l = data_all)

  data_all_m <- data.table::melt(data = data_all,
                                 id.vars = "time",
                                 variable.name = "location_id")

  max_values <- data_all_m[, .(max_value = base::max(value)),
                           by = .(location_id, lubridate::year(x = time))]

  split_data <- base::split(x = data_all_m,
                            f = data_all_m$location_id)

  return(split_data)
}
