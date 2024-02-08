library(CoSMoS)

#' Calculate Intensity-Duration-Frequency (IDF) quantiles and generate plot
#'
#' This function calculates IDF quantiles based on input data and parameters, and
#' generates a plot for visual representation of the IDF curve
#'
#' @param data_list a list of data tables containing time series data with columns "time", "value", and "location_id"
#' @param return_periods vector of return periods for which IDF quantiles are calculated
#' @param durations vector of durations for which IDF quantiles are calculated (in hours)
#' @param aggregation_function aggregation function for calculating rolling statistics (default is "mean")
#' @param distribution distribution to fit for IDF modeling (default is "gev")
#'
#' @return a list containing two elements:
#'   - 'data': a data table with columns for location id, duration, return period, and corresponding IDF values
#'   - 'plot': a ggplot object visualizing the IDF curve
#'
#' @examples
#' idf_result <- calculate_idf(data_list = my_data, return_periods = c(2, 5, 10, 25, 50, 100), durations = c(1, 2, 5, 10, 24, 48))
#' print(idf_result$data)
#' print(idf_result$plot)
#' @export
calculate_idf <- function(data_list,
                          return_periods = c(2, 5, 10, 25, 50, 100),
                          durations = c(1, 2, 5, 10, 24, 48),
                          aggregation_function = "mean",
                          distribution = "gev", ...) {

  # Initializing an empty list to store results
  idf_data <- list()

  # Processing each data table in the list
  for (i in seq_along(data_list)) {
    tryCatch({
      data <- data_list[[i]]

      # Calculating rolling statistics for each duration
      agg <- lapply(
        X = durations,
        FUN = function(d) {
          out <- data[, .(time = as.POSIXct(time, origin = "1970-01-01"),
                          val = do.call(what = paste0("froll", aggregation_function),
                                        args = list(x = value,
                                                    n = d,
                                                    align = "center",
                                                    fill = 0)))]
          out
        }
      )

      # Calculating maximum values by year for each duration
      quant <- lapply(
        X = agg,
        FUN = function(a) {
          mx <- a[, .(mx = max(x = val,
                               na.rm = TRUE)),
                  by = year(x = time)]

          para <- fitDist(data = mx$mx,
                          dist = distribution,
                          n.points = 10,
                          norm = "N4",
                          constrain = FALSE)

          prob <- 1 - 1/return_periods

          q <- qgev(p = prob,
                    loc = para$loc,
                    scale = para$scale,
                    shape = para$shape)

          names(x = q) <- return_periods

          as.list(x = q)
        }
      )

      # Assigning names to quantiles
      names(x = quant) <- durations

      # Combining quantiles into a single table
      quant_all <- rbindlist(l = quant,
                             idcol = "dur")

      # Reshaping for plotting
      quant_idf <- melt(data = quant_all,
                        id.vars = "dur",
                        variable.name = "rp")

      # Appending the result to the list
      idf_data[[i]] <- quant_idf
    }, error = function(e) {
      warning(paste("Error processing data table", i, ": ", conditionMessage(e)))
    })
  }

  # Combining the results from each data table
  idf_results <- rbindlist(l = idf_data,
                           idcol = "location_id")

  # Creating the ggplot object for visualization
  idf_plot <- ggplot(data = idf_results,
                     mapping = aes(x = as.numeric(x = durations),
                                   y = value,
                                   colour = return_periods)) +
    geom_line() +
    geom_point() +
    scale_colour_manual(name = "Return\nperiod",
                        values = c("red", "magenta2", "yellow",
                                   "green4", "blue", "purple4")) +
    labs(x = "Duration (hours)",
         y = "Intensity (mm/h)",
         title = "IDF curve") +
    theme_light() +
    facet_wrap(facets = ~location_id)

  # Return the calculated values
  return(list(data = idf_results, plot = idf_plot))
}
