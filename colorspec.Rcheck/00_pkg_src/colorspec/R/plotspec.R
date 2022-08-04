#' plot average spectra
#'
#' Calc and plot the average (central line) and standard deviation (shadow area) of a given spectra, according to a vector of identities. Please, see aggplot function from 'pavo' package.
#'
#' @param rspecdata A data frame, possibly of class rspec, which contains a column containing a wavelength range, named 'wl', and spectra data in remaining columns.
#' @param ylim set the limits of y axis.
#' @param main plot's title.
#'
#' @examples
#' #example 1 ## not run
#' data(refletancias)
#' refletancias <- fixspec(refletancias)
#' plotspec(refletancias)
#'
#' @return It is a plot.
#'
#' @export
plotspec <- function(rspecdata){

  bysic <- gsub("[0-9]*", "", names(rspecdata)[-1])
  plot  <- pavo::aggplot(rspecdata,
                         bysic,
                         alpha  = 0.3,
                         legend = T)
  return(plot)
}
