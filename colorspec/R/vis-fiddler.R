#' Visual Contrast Analysis Function for Fiddler Crabs
#'
#' This function calculates the quantum catch (Qi), chromatic contrast, and
#' achromatic contrast in Just Noticeable Difference (JND) units for fiddler crabs
#' vision. The visual model parameters are adapted from Silva et al. (2022) and
#' utilize settings from the 'pavo' package.
#'
#' @param rspecdata A dataset, possibly of rspec class, containing a 'wl' column with wavelength range information and spectral data in the remaining columns.
#' @param background Choose one column of spectral data to be used as the background in the RNL model.
#'
#' @examples
#' Example Usage:
#' data(leptodactyla)
#' vis.fiddler(leptodactyla, background = "X00_background")
#'
#' @export
vis.fiddler <- function(rspecdata, background){

  #vismodel
  sens_uca <- pavo::sensmodel(c(430, 520), range = c(300, 700))
  sens_uca <- pavo::as.rspec(sens_uca, lim =c(300, 700))

  QI_uca   <- pavo::vismodel(rspecdata, qcatch = "Qi",visual = sens_uca, achromatic = "l",illum = "D65",trans = "ideal", scale = 1, relative = FALSE)
  JND_uca  <- pavo::coldist(QI_uca, qcatch = NULL, noise = "neural", subset = background, achro=TRUE, n = c(1, 1), weber.ref='longest', weber = 0.12, weber.achro = TRUE)

  QI_uca <- QI_uca %>%
    tibble::rownames_to_column(var = "ID") %>%
    dplyr::filter(ID != background)

  result <- dplyr::bind_cols(QI_uca, JND_uca)

  result <- dplyr::select(result, -patch1, -patch2) %>%
    dplyr::rename(chromatic_contrast = dS,
                  achromatic_contrast = dL,
                  luminance = lum,
                  s = lmax430,
                  m = lmax520) %>%
    dplyr::mutate(vismodel = "Fiddler crab")

  return(result)

}
