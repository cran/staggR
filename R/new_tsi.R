#' Constructor for a tsi object
#'
#' @param df A data frame containing times since intervention
#'
#' @return An object of class `tsi`
#' @noRd

new_tsi <- function(df) {
  structure(
    list(data = df),
    class = "tsi"
  )
}
