#' Estimates the minimum number of ignitions that should be used for the calibration process
#' @description Estimates the minimum number of ignitions that should be used for the calibration process
#' @param fuelmap Fuel map raster file.
#' @param unburnable Numeric. Value of the fuel model(s) considered unburnable.
#'
#' @return Returns the value of the minimum number of ignitions to use in the calibration process.
#' @export
#'
#' @examples
#' \dontrun{get_number_ign(fuelmap="C:/fuel_model_map.asc",
#' unburnable=c(97,98))}
#'
get_number_ign <- function (fuelmap, unburnable){
  my_fuelmap <- raster(fuelmap)
  my_fuelmap_df <- getValues(my_fuelmap)
  my_fuelmap_df <- subset(my_fuelmap_df, !(my_fuelmap_df %in% unburnable))
  min_ign <- round(length(my_fuelmap_df)/50)
  cat(paste("Minimum number of ignitions: ",min_ign,".\n\nThe value was calculated using the rule of thumb outlined in Aparício et al. 2023 (doi:).",sep=""))
}
