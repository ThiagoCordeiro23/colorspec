#' Extract Hexadecimal Color Codes from Spectral Data
#'
#' This function extracts hexadecimal color codes from imagens (jpg,png). It takes data as input, along with a species name (images name), and returns either a summary of hexadecimal color codes for all species in the dataset or the hexadecimal color code for a specific species.
#'
#' @param data Imported images.
#' @param species A character string (images name) specifying the name of the species (which can be anything). Use "all" to return hexadecimal color codes for all species.
#'
#' @return If \code{species} is "all", the function returns a dataframe containing a summary of hexadecimal color codes for all species in the dataset. If \code{species} is a specific species name, the function returns the hexadecimal color code for that species. If the specified species does not exist in the dataset, an error message is returned.
#'
#' @details This function uses the \code{pavo} package to classify spectral data and extract RGB color values. It then converts these RGB values to hexadecimal color codes.
#'
#' @examples
#' library(pavo)
#' library(tidyverse)
#' data(iris)
#'
#' # Import images
#' bf <- pavo::getimg("C:/Users/Diogo Silva/Documents/GitHub/butterfly_spectra/butterspectra/img")
#'
#' # Extract hexadecimal color code for a specific species
#' delia <- gethex(bf, species = "delias_aganippe")
#'
#' # Plot using the extracted color code
#' ggplot(iris, aes(Sepal.Length, Petal.Width, color = Species)) +
#'   geom_point() +
#'   scale_colour_manual(values = delia)
#'
#' @seealso
#' \code{\link{classify}}
#'
#' @export
gethex <- function(data, species) {

  library(pavo)
  library(dplyr)

  kcols_rep <- rep(6, length.out = ncol(data))

  bf_class <- classify(data, kcols = c(kcols_rep))
  df <- summary(bf_class)

  # Função para converter valores RGB para hexadecimal
  rgb_to_hex <- function(r, g, b) {
    rgb_code <- rgb(r, g, b, maxColorValue = 1)
    return(rgb_code)
  }

  # Aplicar a função a cada linha do dataframe
  df$hex_code <- apply(df[, c("R", "G", "B")], 1, function(x) rgb_to_hex(x[1], x[2], x[3]))

  if (species == "all") {
    return(df)
  } else {
    # Verificar se a espécie existe no dataframe
    if (!(species %in% df$ID)) {
      error_msg <- "Sorry, I couldn't find that species in database!"
      return(error_msg)
    } else {
      # Filtrar por espécie
      subdata <- df %>% dplyr::filter(ID == species)
      subdata_hexcode <- subdata$hex_code
      return(subdata_hexcode)
    }
  }
}
