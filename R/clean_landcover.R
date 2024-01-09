#' Clean landcover data to match ABMI species model coefficients
#'
#' @description Create veg, hf, and soil summaries from GIS back-filled data in wide format that align with the ABMI species models
#'
#' @param data.in Output from \code{make_landcover_wide()}
#' @param landscape.lookup Defines the lookup table used for aggregating the landcover data. Default is provided lookup
#' @param type Character; Defines if the landcover data is "Soil" or "Vegetation"
#' @param class.in Input column that matches classes from make_landcover_wide
#' @param class.out Output classes that match the coefficient
#'
#' @export
#'
#' @examples
#' \dontrun{
#' landcover.out <- clean_landcover(data.in = d.wide,
#'                                  landscape.lookup = landcover.coef.lookup,
#'                                  type = "vegetation",
#'                                  class.in = "ID",
#'                                  class.out = "COEF")
#' }
#'
clean_landcover <- function(data.in, landscape.lookup = landcover.coef.lookup,
                            type = "Vegetation", class.in = "ID", class.out = "COEF") {

  # Define the lookup table
  if(type == "Vegetation") {

    landscape.lookup <- landscape.lookup$Vegetation

  }

  if(type == "Soil") {

    landscape.lookup <- landscape.lookup$Soil

  }

  # Matching of lookup tables and merging native features
  landscape.lookup <- landscape.lookup[landscape.lookup[, class.in] %in% colnames(data.in), ]
  landscape.clean <- matrix(nrow = nrow(data.in), ncol = length(unique(landscape.lookup[, class.out])))

  for (abmi.coef in 1:length(unique(landscape.lookup[, class.out]))) {

    coef.temp <- as.character(landscape.lookup[landscape.lookup[, class.out] %in% as.character(unique(landscape.lookup[, class.out]))[abmi.coef], class.in])

    if(length(coef.temp) == 1) {

      landscape.clean[, abmi.coef] <- data.in[, coef.temp]

    } else {

      landscape.clean[, abmi.coef] <- rowSums(data.in[, coef.temp])

    }

  }

  colnames(landscape.clean) <- as.character(unique(landscape.lookup[, class.out]))
  rownames(landscape.clean) <- rownames(data.in)

  return(landscape.clean)

}
