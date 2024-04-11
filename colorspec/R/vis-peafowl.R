#' Peafowl vismodel
#'
#' Calculate quantum catch (Qi), chromatic and achromatic contrast, given in
#' JND units. The vismodel parameters of this function are based in Silva et al. (2022).
#' Parameters: pavo::vismodel(rspecdata, qcatch ="Qi",visual = "pfowl", achromatic = "ch.dc", illum = "D65", trans = transmit, vonkries = FALSE, scale = 1, relative = FALSE)
#' pavo::coldist(noise = "neural", achro = TRUE, n = c(1,1.9,2.2,2.1), weber.ref = 'longest', weber = 0.1, weber.achro = TRUE)
#'
#' @param rspecdata A data frame, possibly of class rspec, which contains a column containing a wavelength range, named 'wl', and spectra data in remaining columns.
#' @param background choice one column spectrum data to be the background in RNL model.
#' @param illum you can chose the followings illuminantes: "D65", "bluesky", and "forestshade".
#'
#' @examples
#' #example ## not run
#' data(leptodactyla)
#' leptodactyla <- fixspec(leptodactyla)
#' vis.peafowl <- vis.peafowl(leptodactyla, background = "X00_background", illum = "D65")
#'
#' @export
vis.peafowl <- function(rspecdata, background, illum = c("D65", "bluesky", "forestshade")){

  #vismodel
  #Import transmittance from Hart
  transmit  <- pavo::as.rspec(transmit,lim=c(300,700))

  QI   <- pavo::vismodel(rspecdata, qcatch ="Qi",visual = "pfowl", achromatic = "ch.dc", illum = illum, trans = transmit, vonkries = FALSE, scale = 1, relative = FALSE)
  JND  <- pavo::coldist(QI, qcatch = NULL, noise = "neural", subset = background, achro = TRUE, n = c(1,1.9,2.2,2.1), weber.ref = 'longest', weber = 0.1, weber.achro = TRUE)

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
    dplyr::mutate(vismodel = "Peafowl") %>%
    dplyr::mutate(iluminante = illum) %>%
    dplyr::mutate(substrato = background)

  return(result)

}
