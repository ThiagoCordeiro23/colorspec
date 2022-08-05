## code to prepare `leptodactyla` dataset goes here

#import data ----
transmit <- read.csv("data-raw/transmit.csv", sep = ",")

#salvar para o pacote ----
usethis::use_data(transmit, overwrite = TRUE)
