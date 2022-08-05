#' Fiddler crab vismodel
#'
#' Calculate quantum catch (Qi), chromatic and achromatic contrast, given in JND units.
#' This package uses:
#' pavo::vismodel parameters: qcatch = "Qi",visual = c(430,520), achromatic = "l",illum = "D65",trans = "ideal")
#' pavo::coldist parameters: qcatch = NULL, noise = "neural", n = c(1, 1), weber.ref= 'longest', weber = 0.12 (Apis melifera)
#'
#' @param rspecdata A data frame, possibly of class rspec, which contains a column containing a wavelength range, named 'wl', and spectra data in remaining columns.
#' @param background choice one column spectrum data to be the background in RNL model.
#'
#' @examples
#' #example ## not run
#' data(leptodactyla)
#' vis.fiddler <- vis.fiddler(leptodactyla, background = "X00_background")
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
                  luminance = lum)

  return(result)

}
