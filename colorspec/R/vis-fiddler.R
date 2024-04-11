#' Visual Contrast Analysis Function for Fiddler Crabs
#'
#' This function calculates the quantum catch (Qi), chromatic contrast, and
#' achromatic contrast in Just Noticeable Difference (JND) units for fiddler crabs
#' vision. The visual model parameters are adapted from Silva et al. (2022) and
#' utilize settings from the 'pavo' package.
#'
#' @param rspecdata A dataset, possibly of rspec class, containing a 'wl' column with wavelength range information and spectral data in the remaining columns.
#' @param background Choose one column of spectral data to be used as the background in the RNL model.
#' @param illum you can chose the followings illuminantes: "D65", "bluesky", and "forestshade".
#'
#' @examples
#' Example Usage:
#' data(leptodactyla)
#' vis.fiddler(leptodactyla, background = "X00_background", illum = "D65")
#'
#' @export
vis.fiddler <- function(rspecdata, background, illum = c("D65", "bluesky", "forestshade")){

  #vismodel
  sens <- pavo::sensmodel(c(430, 520), range = c(300, 700))
  sens <- pavo::as.rspec(sens, lim =c(300, 700))

  QI   <- pavo::vismodel(rspecdata, qcatch = "Qi",visual = sens, achromatic = "l",illum = illum,trans = "ideal", scale = 1, relative = FALSE)
  JND  <- pavo::coldist(QI, qcatch = NULL, noise = "neural", subset = background, achro=TRUE, n = c(1, 1), weber.ref='longest', weber = 0.12, weber.achro = TRUE)

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
    dplyr::mutate(vismodel = "Fiddler crab") %>%
    dplyr::mutate(iluminante = illum) %>%
    dplyr::mutate(substrato = background)

  return(result)

}
