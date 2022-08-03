#' bluetit vismodel
#'
#' Calculate properties of color (saturation, mean brightness and hue), quantum catch (Qi), chromatic and achromatic contrast, given in JND units. The iluminant is D65. If you want to set the parameters, see vismodel and coldist functions of 'pavo' package.
#'
#' @param rspecdata A data frame, possibly of class rspec, which contains a column containing a wavelength range, named 'wl', and spectra data in remaining columns.
#' @param background choice one column spectrum data to be the background in RNL model.
#'
#' @examples
#' #example ## not run
#' data(flowers)
#' vis <- vis.bluetit(flowers, background = "Goodenia_heterophylla")
#'
#' @export
vis.bluetit <- function(rspecdata, background,...){
  #calculating saturation, mean brightness and hue of spectra
  summary_cor <- summary(rspecdata, subset = c("S8","B2","H1")) %>%
    rownames_to_column(var = "ID")%>%
    filter(ID != background)

  #vismodel
  QI_tit   <- pavo::vismodel(rspecdata, qcatch = "Qi",visual = "bluetit", achromatic = "bt.dc",illum = "D65",trans = "bluetit", scale = 1, relative = FALSE)
  JND_tit  <- pavo::coldist(QI_tit, qcatch = NULL, noise = "neural", subset = background, achro=TRUE, n = c(1,1.9,2.7,2.7), weber.ref='longest', weber = 0.1, weber.achro = TRUE)
  QI_tit <- QI_tit %>%
    rownames_to_column(var = "ID2") %>%
    filter(ID2 != background)
  result <- bind_cols(summary_cor, QI_tit, JND_tit)
  result <- select(result, -patch1, -patch2, -ID2) %>%
    rename(chromatic_contrast = dS,
           achromatic_contrast = dL,
           luminance = lum,
           saturation = S8,
           mean_brightness = B2,
           hue = H1)

  return(result)

}
