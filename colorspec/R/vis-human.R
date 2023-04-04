
#' @export
vis.human <- function(rspecdata, background){

  #vismodel
  QI_human   <- pavo::vismodel(rspecdata, qcatch ="Qi",visual = "cie10", achromatic = "ml", illum = "D65", trans = "ideal", vonkries = FALSE, scale = 1, relative = FALSE)
  JND_human  <- pavo::coldist(QI_human, qcatch = NULL, noise = "neural", subset = background, achro = TRUE, n = c(1,10,30), weber.ref = 'ml', weber = c(0.08, 0.02, 0.02), weber.achro = TRUE) #Peter Olsson, Olle Lind, Almut Kelber (2018)

  QI_human <- QI_human %>%
    tibble::rownames_to_column(var = "ID") %>%
    dplyr::filter(ID != background)

  result <- dplyr::bind_cols(QI_human, JND_human)

  result <- dplyr::select(result, -patch1, -patch2) %>%
    dplyr::rename(chromatic_contrast = dS,
                  achromatic_contrast = dL,
                  luminance = lum) %>%
    dplyr::mutate(vismodel = "Human")

  return(result)

}
