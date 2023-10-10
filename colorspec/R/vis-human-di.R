#' Visual Contrast Analysis Function for Dichromatic Humans (S & L cones)
#'
#' This function calculates the quantum catch (Qi), chromatic contrast (dS), and achromatic
#' contrast in Just Noticeable Difference (JND) units for dichromatic humans based
#' on spectral reflectance data. The visual model employs settings
#' from the 'pavo' package.
#'-------------------------------------
#' sens_hum <- pavo::sensmodel(c(420, 565), range = c(300, 700))
#' sens_hum <- pavo::as.rspec(sens_hum, lim =c(300, 700))
#' quantum_catch   <- pavo::vismodel(rspecdata, qcatch ="Qi",visual = sens_hum, achromatic = "l", illum = "D65", trans = "ideal", vonkries = FALSE, scale = 1, relative = FALSE)
#' dS  <- pavo::coldist(quantum_catch, qcatch = NULL, noise = "neural", subset = background, achro = TRUE, n = c(1,19), weber.ref = 'l', weber = c(0.08, 0.014), weber.achro = TRUE)
#'-------------------------------------
#' @param rspecdata A dataset, possibly of rspec class, containing a 'wl' column with wavelength range information and spectral data in the remaining columns.
#' @param background Choose one column of spectral data to be used as the background in the RNL model.
#'
#' @examples
#' # Example Usage:
#' data(leptodactyla)
#' vis.human.di(leptodactyla, background = "X00_background")
#' @export
vis.human.di <- function(rspecdata, background){

  #Dichromatic sensibility
  sens_hum <- pavo::sensmodel(c(420, 565), range = c(300, 700)) #visual peaks = (Pessoa et al., 2014)
  sens_hum <- pavo::as.rspec(sens_hum, lim =c(300, 700))

  #vismodel
  QI_human   <- pavo::vismodel(rspecdata, qcatch ="Qi",visual = sens_hum, achromatic = "l", illum = "D65", trans = "ideal", vonkries = FALSE, scale = 1, relative = FALSE)
  JND_human  <- pavo::coldist(QI_human, qcatch = NULL, noise = "neural", subset = background, achro = TRUE, n = c(1,19), weber.ref = 'l', weber = c(0.08, 0.014), weber.achro = TRUE) #'weber' from (Perini et al., 2009); 'n' from Roorda & Williams (1999)

  QI_human <- QI_human %>%
    tibble::rownames_to_column(var = "ID") %>%
    dplyr::filter(ID != background)

  result <- dplyr::bind_cols(QI_human, JND_human)

  result <- dplyr::select(result, -patch1, -patch2) %>%
    dplyr::rename(chromatic_contrast = dS,
                  achromatic_contrast = dL,
                  luminance = lum,
                  s = lmax420,
                  l = lmax565) %>%
    dplyr::mutate(vismodel = "Colorblind Human")

  return(result)

}
