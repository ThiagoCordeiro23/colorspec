#' fixspec function
#'
#' This function convert to rspecdata, normalize, fix negative values and applies smoothing. Please, see procspec and as.rspec functions from 'pavo' package
#'
#' @param rspecdata A data frame, possibly of class rspec, which contains a column containing a wavelength range, named 'wl', and spectra data in remaining columns.
#'
#'
#' @examples
#' data(sicalis)
#' sicalis_fix <- fixspec(sicalis)
#'
#' @export
fixspec <- function(rspecdata,...){

  reflet <- pavo::procspec(rspecdata, opt = 'smooth',fixneg = 'addmin',span = 0.25, bins = 5,...)
  reflet <- pavo::as.rspec(reflet,lim=c(300,700))

  print(reflet)
}
