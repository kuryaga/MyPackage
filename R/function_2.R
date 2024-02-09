library(CoSMoS); library(data.table)

#' Calculate Intensity-Duration-Frequency (IDF) quantiles and generate plot
#'
#' This function calculates IDF quantiles based on input data and parameters, and
#' generates a plot for visual representation of the curve
#'
#' @param data_list a list of data tables containing time series data with columns "time", "value", and "location_id"
#' @param return_periods vector of return periods for which IDF quantiles are calculated
#' @param durations vector of durations for which IDF quantiles are calculated (in hours)
#' @param aggregation_function aggregation function for calculating rolling statistics (default is "mean")
#' @param distribution distribution to fit for IDF modeling (default is "gev")
#'
#' @return a list containing two elements:
#'   - 'data': a data table
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

  idf_dta <- list()

  for (i in seq_along(data_list)) {
    tryCatch({
      x <- data_list[[i]]

      agg <- lapply(
        X = durations,
        FUN = function(d) {
          out <- x[, .(time = as.POSIXct(time, origin = "1970-01-01"),
                       val = do.call(what = paste0("froll", aggregation_function),
                                     args = list(x = value,
                                                 n = d,
                                                 align = "center",
                                                 fill = 0)))]
          out
        }
      )

      quant <- lapply(
        X = agg,
        FUN = function(a) {
          mx <- a[, .(mx = max(x = val,
                               na.rm = TRUE)),
                  by = year(x = time)]

          para <- CoSMoS::fitDist(data = mx$mx,
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

      names(x = quant) <- durations

      quant_all <- rbindlist(l = quant,
                             idcol = "dur")

      quant_idf <- melt(data = quant_all,
                        id.vars = "dur",
                        variable.name = "rp")

      idf_dta[[i]] <- quant_idf
    }, error = function(e) {
      warning(paste("Error processing data table", i, ": ", conditionMessage(e)))
    })
  }

  idf_results <- data.table::rbindlist(l = idf_dta,
                                       idcol = "location_id")

  idf_plot <- ggplot2::ggplot(data = idf_results,
                              mapping = aes(x = as.numeric(x = dur),
                                            y = value,
                                            colour = rp)) +
    geom_line() +
    geom_point() +
    scale_colour_manual(name = "Return\nperiod",
                        values = c("red1", "magenta", "yellow3",
                                   "green", "blue", "purple")) +
    labs(x = "Duration (hours)",
         y = "Intensity (mm/h)",
         title = "IDF curve") +
    theme_bw() +
    facet_wrap(facets = ~location_id)

  return(list(data = idf_results, plot = idf_plot))
}

