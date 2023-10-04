#' Fiddler crab vismodel
#'
#' Calculate quantum catch (Qi), chromatic and achromatic contrast, given in JND units.
#' The visual peaks based in Pessoa et al. (2014) and weber based on Perini et al. (2009)
#' Photorreceptors density: (???)
#' This package uses:
#' pavo::vismodel parameters: qcatch = "Qi",visual = c(420,530, 565), achromatic = "ml",illum = "D65",trans = "ideal")
#' pavo::coldist parameters: qcatch = NULL, noise = "neural", n = c(1, 10), weber.ref= 'longest', weber = 0.
#'
#' @param rspecdata A data frame, possibly of class rspec, which contains a column containing a wavelength range, named 'wl', and spectra data in remaining columns.
#' @param background choice one column spectrum data to be the background in RNL model.
#'
#' @examples
#' #example ## not run
#' data(leptodactyla)
#' vis.human.tri <- vis.human.tri(leptodactyla, background = "X00_background")
#' @export
vis.human.tri <- function(rspecdata, background){

  #Dichromatic sensibility
  sens_hum <- pavo::sensmodel(c(420, 530, 565), range = c(300, 700)) #sensmodel = (Pessoa et al., 2014)
  sens_hum <- pavo::as.rspec(sens_hum, lim =c(300, 700))

  #vismodel
  QI_human   <- pavo::vismodel(rspecdata, qcatch ="Qi",visual = sens_hum, achromatic = "ml", illum = "D65", trans = "ideal", vonkries = FALSE, scale = 1, relative = FALSE)
  JND_human  <- pavo::coldist(QI_human, qcatch = NULL, noise = "neural", subset = background, achro = TRUE, n = c(1,4,9.2), weber.ref = 'l', weber = c(0.08, 0.02, 0.02), weber.achro = TRUE) #weber = (Perini et al., 2009); proportions (?)

  QI_human <- QI_human %>%
    tibble::rownames_to_column(var = "ID") %>%
    dplyr::filter(ID != background)

  result <- dplyr::bind_cols(QI_human, JND_human)

  result <- dplyr::select(result, -patch1, -patch2) %>%
    dplyr::rename(chromatic_contrast = dS,
                  achromatic_contrast = dL,
                  luminance = lum,
                  s = lmax420,
                  m = lmax530,
                  l = lmax565) %>%
    dplyr::mutate(vismodel = "Trichromatic Human")

  return(result)

}
