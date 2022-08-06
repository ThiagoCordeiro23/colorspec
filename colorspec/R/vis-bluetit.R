#' Bluetit vismodel
#'
#' Calculate quantum catch (Qi), chromatic and achromatic contrast, given in
#' JND units. The vismodel parameters is based in Silva et al (2022).
#' pavo::vismodel Parameters: qcatch = "Qi",visual = "bluetit", achromatic = "bt.dc",illum = "D65",trans = "bluetit", scale = 1, relative = FALSE.
#' pavo::coldist parameters: qcatch = NULL, noise = "neural", n = c(1,1.9,2.7,2.7), weber.ref ='longest', weber = 0.1
#' To set the parameters manualy see vismodel() and coldist() functions of 'pavo' package.
#'
#' @param rspecdata A data frame, possibly of class rspec, which contains a column containing a wavelength range, named 'wl', and spectra data in remaining columns.
#' @param background choice one column spectrum data to be the background in RNL model.
#'
#' @examples
#' #example ## not run
#' data(leptodactyla)
#' leptodactyla <- fixspec(leptodactyla)
#' vis.bt <- vis.bluetit(leptodactyla, background = "X00_background")
#'
#' @export
vis.bluetit <- function(rspecdata, background){

  #vismodel
  QI_tit   <- pavo::vismodel(rspecdata, qcatch = "Qi",visual = "bluetit", achromatic = "bt.dc",illum = "D65",trans = "bluetit", scale = 1, relative = FALSE)
  JND_tit  <- pavo::coldist(QI_tit, qcatch = NULL, noise = "neural", subset = background, achro=TRUE, n = c(1,1.9,2.7,2.7), weber.ref='longest', weber = 0.1, weber.achro = TRUE)

  QI_tit <- QI_tit %>%
    tibble::rownames_to_column(var = "ID") %>%
     dplyr::filter(ID != background)

  result <- dplyr::bind_cols(QI_tit, JND_tit)

  result <- dplyr::select(result, -patch1, -patch2) %>%
    dplyr::rename(chromatic_contrast = dS,
           achromatic_contrast = dL,
           luminance = lum) %>%
    dplyr::mutate(vismodel = "Bluetit")

  return(result)

}
