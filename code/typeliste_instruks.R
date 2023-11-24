library(plyr)
library(dplyr)
library(stringr)
library(tidyverse)
library(readxl)

typeliste_instruks <- read_excel("input/naturtypeliste kartleggingsinstruks 2021-2023.xlsx", 
                              sheet = 1)

head(typeliste_instruks)

# separate the Definisjon column into info on naturtype and variables
typeliste_instruks <- typeliste_instruks %>% separate_rows(Definisjon, sep=";") %>%
  separate(col = Definisjon,
           into = c("Naturtype_NiN", "Variabeltrinn"),
           sep = " ",
           remove=F) %>%
  mutate(Naturtype_NiN = as.factor(Naturtype_NiN)) %>%
  subset(select = -c(Definisjon,Definisjon2,Definisjon_orig))

typeliste_instruks <- as.data.frame(typeliste_instruks)
head(typeliste_instruks)

typeliste_instruks <- typeliste_instruks %>% mutate(Naturtype_NiN=gsub("V3-C-1_","V3-C-1 ",Naturtype_NiN)) %>%
  mutate(Naturtype_NiN=gsub("V3-C-2_","V3-C-2 ",Naturtype_NiN)) %>%
  separate(col = Naturtype_NiN,
           into = c("Naturtype_NiN", "unntatt"),
           sep = " ",
           remove=F) %>%
  mutate(Naturtype_NiN = as.factor(Naturtype_NiN))
  
# read inn NiN-species data
NiN_species <- readRDS("P:/153054_fou_areal_for_pollinerende_insekter/Data/artslister NiN/NiN_species.RDS")

# merge 

# save as RDS
saveRDS(typeliste_instruks, "P:/153054_fou_areal_for_pollinerende_insekter/Data/typeliste NiN/typeliste_instruks.RDS")
