## code to prepare `leptodactyla` dataset goes here

#package ----
library(dplyr)

#import data ----
leptodactyla <- read.csv("data-raw/leptodactyla.csv", sep = ";")

#salvar para o pacote ----
usethis::use_data(leptodactyla, overwrite = TRUE)
