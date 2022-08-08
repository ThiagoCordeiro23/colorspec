#' Peafowl vismodel
#'
#' Calculate quantum catch (Qi), chromatic and achromatic contrast, given in
#' JND units. The vismodel parameters of this function are based in Silva et al. (2022).
#' Parameters: pavo::vismodel(rspecdata, qcatch ="Qi",visual = "pfowl", achromatic = "ch.dc", illum = "D65", trans = transmit, vonkries = FALSE, scale = 1, relative = FALSE)
#' pavo::coldist(noise = "neural", achro = TRUE, n = c(1,1.9,2.2,2.1), weber.ref = 'longest', weber = 0.1, weber.achro = TRUE)
#'
#' @param rspecdata A data frame, possibly of class rspec, which contains a column containing a wavelength range, named 'wl', and spectra data in remaining columns.
#' @param background choice one column spectrum data to be the background in RNL model.
#'
#' @examples
#' #example ## not run
#' data(leptodactyla)
#' leptodactyla <- fixspec(leptodactyla)
#' vis.peafowl <- vis.peafowl(leptodactyla, background = "X00_background")
#'
#' @export
vis.peafowl <- function(rspecdata, background){

  #vismodel
  #Import transmittance from Hart
  transmit  <- pavo::as.rspec(transmit,lim=c(300,700))

  QI_pav   <- pavo::vismodel(rspecdata, qcatch ="Qi",visual = "pfowl", achromatic = "ch.dc", illum = "D65", trans = transmit, vonkries = FALSE, scale = 1, relative = FALSE)
  JND_pav  <- pavo::coldist(QI_pav, qcatch = NULL, noise = "neural", subset = background, achro = TRUE, n = c(1,1.9,2.2,2.1), weber.ref = 'longest', weber = 0.1, weber.achro = TRUE)

  QI_pav <- QI_pav %>%
    tibble::rownames_to_column(var = "ID") %>%
     dplyr::filter(ID != background)

  result <- dplyr::bind_cols(QI_pav, JND_pav)

  result <- dplyr::select(result, -patch1, -patch2) %>%
    dplyr::rename(chromatic_contrast = dS,
           achromatic_contrast = dL,
           luminance = lum) %>%
    dplyr::mutate(vismodel = "Peafowl")

  return(result)

}
