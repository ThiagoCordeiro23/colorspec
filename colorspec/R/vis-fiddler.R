#' Fiddler crab vismodel
#'
#' Calculate properties of color (saturation, mean brightness and hue), quantum catch (Qi), chromatic and achromatic contrast, given in JND units.
#'
#' @param rspecdata A data frame, possibly of class rspec, which contains a column containing a wavelength range, named 'wl', and spectra data in remaining columns.
#' @param background choice one column spectrum data to be the background in RNL model.
#'
#' @examples
#' #example ## not run
#' data(flowers)
#' vis <- vis.fiddler(flowers, background = "Goodenia_heterophylla")
#'
#' @export
vis.fiddler <- function(rspecdata, background,...){
  #calculating saturation, mean brightness and hue of spectra
  summary_cor <- summary(rspecdata, subset = c("S8","B2","H1")) %>%
    rownames_to_column(var = "ID")%>%
    filter(ID != background)

  #vismodel
  sens_uca <- pavo::sensmodel(c(430, 520), range = c(300, 700))
  sens_uca <- pavo::as.rspec(sens_uca, lim =c(300, 700))
  QI_uca   <- pavo::vismodel(rspecdata, qcatch = "Qi",visual = sens_uca, achromatic = "l",illum = "D65",trans = "ideal", scale = 1, relative = FALSE)
  JND_uca  <- pavo::coldist(QI_uca, qcatch = NULL, noise = "neural", subset = background, achro=TRUE, n = c(1, 1), weber.ref='longest', weber = 0.12, weber.achro = TRUE)
  QI_uca <- QI_uca %>%
    rownames_to_column(var = "ID2") %>%
    filter(ID2 != background)
  result <- bind_cols(summary_cor, QI_uca, JND_uca)
  result <- select(result, -patch1, -patch2, -ID2) %>%
    rename(chromatic_contrast = dS,
           achromatic_contrast = dL,
           luminance = lum,
           saturation = S8,
           mean_brightness = B2,
           hue = H1)

  return(result)

}
