#' Calculate chromatic and achromatic contrasts for trichromatic human vision
#'
#' This function calculates chromatic and achromatic contrasts for trichromatic human vision based on reflectance spectra data.
#'
#' @param rspecdata A dataframe containing reflectance spectra data. Each row represents a spectrum, and each column represents a wavelength with corresponding reflectance values.
#' @param background A character string specifying the ID of the background spectrum used for calculations.
#' @param illum A character string specifying the illuminant used for calculations.
#' @return A dataframe containing calculated chromatic and achromatic contrasts along with other relevant information.
#' @export
#' @examples
#' library(pavo)
#' # Generate example data
#' rspecdata <- data.frame(Wavelength = seq(300, 700, by = 10), Spectra = runif(41))
#' background <- "background_spectrum"
#' illum <- "D65"
#' # Calculate chromatic and achromatic contrasts
#' vis.human.tri(rspecdata, background, illum)
#'
#' @seealso \code{\link[pavo:sensmodel]{sensmodel}}, \code{\link[pavo:vismodel]{vismodel}}, \code{\link[pavo:coldist]{coldist}}
#'
#' @export
vis.human.tri <- function(rspecdata, background, illum = c("D65", "bluesky", "forestshade")) {

  # Trichromatic sensibility
  sens <- pavo::sensmodel(c(420, 530, 565), range = c(300, 700)) # Visual peaks = (Pessoa et al., 2014)
  sens <- pavo::as.rspec(sens, lim = c(300, 700))

  # Vismodel
  QI   <- pavo::vismodel(rspecdata, qcatch = "Qi", visual = sens, achromatic = "ml", illum = illum, trans = "ideal", vonkries = FALSE, scale = 1, relative = FALSE)
  JND  <- pavo::coldist(QI, qcatch = NULL, noise = "neural", subset = background, achro = TRUE, n = c(1, 6, 13), weber.ref = 'l', weber = c(0.08, 0.02, 0.02), weber.achro = TRUE) # weber = (Perini et al., 2009); photoreceptors proportions (Roorda & Williams, 1999)

  JND2 <- JND %>%
    dplyr::mutate(patch2 = ifelse(patch2 == background, patch1, patch2)) %>%
    dplyr::rename(ID = patch2)%>%
    dplyr::select(-patch1)

  QI2 <- QI %>%
    tibble::rownames_to_column(var = "ID") %>%
    dplyr::filter(ID != background)

  result <- dplyr::left_join(QI2, JND2, by = "ID") %>%
    dplyr::rename(chromatic_contrast = dS,
                  achromatic_contrast = dL,
                  luminance = lum) %>%
    dplyr::mutate(vismodel = "Trichromatic Human") %>%
    dplyr::mutate(iluminante = illum) %>%
    dplyr::mutate(substrato = background)

  return(result)

}

