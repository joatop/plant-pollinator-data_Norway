library(plyr)
library(dplyr)
library(stringr)
library(tidyverse)
library(readxl)

#### upload species lists ####

# generalized species lists NiN
load("P:/41201785_okologisk_tilstand_2022_2023/data/functional plant indicators//reference from NiN/Eco_State.RData")
str(Eco_State)

# reference lists natopen
natopen_NiN_ref <- read_excel("P:/41201785_okologisk_tilstand_2022_2023/data/functional plant indicators/reference from NiN/Masterfil_artslister_organisert.xlsx", 
                             sheet = 1)
natopen_NiN_ref_spInfo <- read_excel("P:/41201785_okologisk_tilstand_2022_2023/data/functional plant indicators/reference from NiN/Masterfil_artslister_organisert.xlsx", 
                              sheet = 2)


#### data handling ####

#### NiN
str(Eco_State)

# sp
Eco_State$Concept_Data$Species$Species_List$species
# env
t(Eco_State$Concept_Data$Env$Env_Data)
# abun
t(Eco_State$Concept_Data$Species$Species_Data)

# transposing abundance data for bootstrapping
NiN.sp <- t(Eco_State$Concept_Data$Species$Species_Data)
NiN.sp <- as.data.frame(NiN.sp)
NiN.sp$sp <- as.factor(as.vector(Eco_State$Concept_Data$Species$Species_List$species))
# only genus and species name
NiN.sp$sp <- word(NiN.sp$sp, 1,2)
# add species groups
NiN.sp$spgr <- as.factor(as.vector(Eco_State$Concept_Data$Species$Species_List$art.code))

head(NiN.sp)

# environment data NiN
NiN.env <- Eco_State$Concept_Data$Env$Env_Data
NiN.env

### add info on basic ecosystem types (mapping units)
NiN.env$grunntype <- NA
# strandeng
NiN.env[c(3:7),"grunntype"] <- c("T12-C2","T12-C2","T12-C2","T12-C2","T12-C1")
# kystlynghei
NiN.env[c(11:27),"grunntype"] <- c('T34-C1',
                                   'T34-C2','T34-C2','T34-C2',
                                   NA,
                                   'T34-C3',
                                   'T34-C4','T34-C4','T34-C4','T34-C4',
                                   'T34-C5','T34-C5','T34-C5',
                                   'T34-C6','T34-C6','T34-C6',
                                   NA
                                   )
# skog
NiN.env[c(28:53,70:76),"grunntype"] <- c('T4-C1','T4-C5','T4-C9','T4-C13',
                                         NA,NA,NA,NA,
                                         'T4-C2','T4-C6','T4-C10','T4-C14',
                                         'T4-C3','T4-C7','T4-C11','T4-C15',
                                         'T4-C4','T4-C8','T4-C12','T4-C16',
                                         'T4-C17','T4-C18','T4-C19','T4-C18','T4-C19','T4-C20',
                                         'T30-C1','T30-C2','T30-C3',
                                         NA,NA,NA,
                                         'T30-C4')
# fjell
NiN.env[c(85:122),"grunntype"] <- c('T14-C1','T14-C2',
                                    'T3-C3C6','T3-C9C12','T3-C2C5','T3-C8C11',
                                    'T3-C1','T3-C1','T3-C4','T3-C7',
                                    'T3-C10','T3-C13','T3-C14',
                                    'T7-C1','T7-C2','T7-C3','T7-C6','T7-C8',
                                    NA,
                                    'T7-C12','T7-C13C14','T7-C4','T7-C7C9','T7-C5','T7-C10',
                                    NA,NA,NA,NA,NA,NA,NA,
                                    'T22-C1C3','T22-C1C3','T22-C2C4',
                                    NA,NA,NA)
# våtmark
NiN.env[(123:179),"grunntype"] <- c('V3-C1','V3-C1','V3-C1','V3-C1','V3-C1',
                                     'V1-C1','V1-C1','V1-C1','V1-C1','V1-C1',
                                     'V1-C2','V1-C2','V1-C2','V1-C2',
                                     'V1-C3','V1-C3','V1-C3','V1-C3',
                                     'V1-C4','V1-C4','V1-C4','V1-C4',
                                     'V1-C4','V1-C4','V1-C4','V1-C4',
                                     'V3-C2','V1-C5',
                                     'V1-C6','V1-C6',
                                     'V1-C7','V1-C7',
                                     'V1-C8','V1-C8',
                                     'V2-C1','V2-C1',
                                     'V2-C2','V2-C2',
                                     'V2-C3','V2-C3',
                                     'V4-C2','V4-C3',
                                     NA,NA,
                                     'V8-C1','V8-C2','V8-C3',
                                     rep(NA,10)
                                     )
# seminat
NiN.env[(180:205),"grunntype"] <- c(NA,NA,rep('beiteskog',2),rep('hagemark',3),
                                    'T32-C1C2','T32-C3C4',
                                    'T32-C5C20',
                                    'T32-C7C8',
                                    'T32-C5C20',
                                    'T32-C9',
                                    'T32-C9',
                                    'T32-C15',
                                    'T32-C21C6',
                                    'T32-C21C6',
                                    'T32-C10',
                                    'T32-C10',
                                    'T32-C16',
                                    'T41','T41',
                                    'T45-C1C2','T45-C3',
                                    'V10-C1C2','V10-C3'
                                    )

names(NiN.sp)[1:205] <- NiN.env$grunntype
# duplicate columns that cover two grunntyper
NiN.sp[,c(87:90,105,107,117:119,187:191,195,196,202,204)] 
NiN.sp <- cbind(NiN.sp,NiN.sp[,c(87:90,105,107,117:119,187:191,195,196,202,204)])
names(NiN.sp)[c(87:90,105,107,117:119,187:191,195,196,202,204)]
names(NiN.sp)[c(87:90,105,107,117:119,187:191,195,196,202,204,208:(208+length(c(87:90,105,107,117:119,187:191,195,196,202,204))-1) )] <-
  c("T3-C3","T3-C9","T3-C2","T3-C8","T7-C13","T7-C7","T22-C1","T22-C1","T22-C2","T32-C1","T32-C3",
    "T32-C5","T32-C7","T32-C5","T32-C21","T32-C21","T45-C1","V10-C1",
    
    "T3-C6","T3-C12","T3-C5","T3-C11","T7-C14","T7-C9","T22-C3","T22-C3","T22-C4","T32-C2","T32-C4",
    "T32-C20","T32-C8","T32-C20","T32-C6","T32-C6","T45-C2","V10-C2")

# omit NA-columns and reorder columns
NiN.sp <- NiN.sp[!is.na(names(NiN.sp))]
NiN.sp <- 
  NiN.sp %>% 
  select(spgr, everything()) %>%
  select(sp, everything())
# make sense of the codes in the species groups column (spgr)
levels(NiN.sp$spgr)
NiN.sp$spgr <- 
  recode(NiN.sp$spgr,
         a1a = "tree",
         a1b = "fern",
         a1c = "forb_graminoid",
         a2l = "lichen",
         a2lb = "lichen",
         a2m = "moss")
# collect the ecosystem type columns into one column
names(NiN.sp)
dim(NiN.sp)
NiN.sp <- NiN.sp %>%
  pivot_longer(cols = 3:162,
               names_to = "grunntype",
               values_to = "abun")
names(NiN.sp)
dim(NiN.sp)
head(NiN.sp)
# get rid of the .1 and such in column grunntype
NiN.sp$grunntype <- gsub("\\..*","",NiN.sp$grunntype)
unique(NiN.sp$grunntype)

unique(NiN.sp$abun)
# remove all lines with abun = 0
NiN.sp <- 
  NiN.sp %>%
  filter(abun != 0)
# abun = 11 must be a typo, the scale only goes to 6! Change it to 1
NiN.sp$abun <- dplyr::case_when(NiN.sp$abun %in% 11 ~ 1, TRUE ~ as.numeric(NiN.sp$abun))

#### natopen ref lists
natopen.sp <- cbind(natopen_NiN_ref, natopen_NiN_ref_spInfo[,"Phylum"])
names(natopen.sp)
dim(natopen.sp)
natopen.sp <- natopen.sp %>%
  pivot_longer(cols = 2:71,
               names_to = "grunntype",
               values_to = "abun")
names(natopen.sp)
dim(natopen.sp)
head(natopen.sp)

unique(natopen.sp$abun)
# remove all lines with abun = NA
natopen.sp <- 
  natopen.sp %>%
  filter(!is.na(abun))

unique(natopen.sp$grunntype)
# remove 2nd hyphen
natopen.sp$grunntype <- gsub("C-", "C", natopen.sp$grunntype)


#### merge nin.sp and natopen.sp

# nin.sp and natopen.sp have different species information, we add some information on that in an extra column

head(NiN.sp)
head(natopen.sp)

NiN.sp$group_type <- "trivial"
natopen.sp$group_type <- "Phylum"

names(NiN.sp) <- c("species", "group", "grunntype", "abun", "group_type")
names(natopen.sp) <- c("species", "group", "grunntype", "abun", "group_type")

NiN.species <- rbind(NiN.sp,natopen.sp)

head(NiN.species)

# add column on hovedtype
NiN.species$hovedtype <- substr(NiN.species$grunntype,1, 3)
NiN.species$hovedtype <- gsub("-", "", NiN.species$hovedtype)
NiN.species$hovedtype <- gsub("_", "", NiN.species$hovedtype)
unique(NiN.species$hovedtype)
NiN.species$hovedtype <- 
  recode(NiN.species$hovedtype,
         bei = "beiteskog",
         hag = "hagemark")
unique(NiN.species$hovedtype)
head(NiN.species)

# save as RDS
saveRDS(NiN.species, "P:/153054_fou_areal_for_pollinerende_insekter/Data/artslister NiN/NiN_species.RDS")
