library(dplyr)
library(tidyr)
library(data.table)
library(mgcv)
library(gratia)
library(purrr)
library(janitor)
library(ggplot2)

#sheet for calculating abundance indices for WF and predators/competitors 
#use bottom analyses ending in .rds "NEFSC_species_allship_catchrate_.rds" that accounts for tows without fish <- now the only thing in this script 

nefsc_all_spp <- read.csv("~/Desktop/StockAssessmentPracticum/WF_NaturalMortality/nefsc_all_spp.csv")

#replace NA w/0s
nefsc_all_spp_ordered <-  nefsc_all_spp[order(nefsc_all_spp$YEAR),] %>% 
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) #%>% filter(SVVESSEL == "AL") 
#View(nefsc_all_spp_ordered)

#list of predators/competitors 
species <- c("WINTER FLOUNDER", "SILVER HAKE", "GOOSEFISH", "YELLOWTAIL FLOUNDER", "LONGHORN SCULPIN" ,
             "SEA RAVEN", "AMERICAN PLAICE", "WITCH FLOUNDER", "WHITE HAKE", "RED HAKE", "HADDOCK",
             "SPINY DOGFISH", "OCEAN POUT", "ATLANTIC COD", "CUNNER", "SCUP", "SUMMER FLOUNDER", 
             "BLUEFISH", "STRIPED BASS", "WINDOWPANE", "FOURSPOT FLOUNDER", "WINTER SKATE", 
             "BARNDOOR SKATE", "LITTLE SKATE", "SMOOTH SKATE", "THORNY SKATE", "CLEARNOSE SKATE",
             "ROSETTE SKATE", "STRIPED SEAROBIN", "SPOTTED HAKE", "WEAKFISH")

############### MEAN NUMBER PER TOW TOTAL ######################
# accounts for tows with no fish 

################ GEORGES BANK ################### 

#GB both surveys

GB_list <- list()

#determine number of tows in a given year for each stock region/season based on unique lat, long, year 
tow_number_GB <- nefsc_all_spp_ordered %>%
  filter(Stock == "GB") 
unique_tow_number_GB <- tow_number_GB[!duplicated(tow_number_GB[, c("YEAR", "DECDEG_BEGLAT", "DECDEG_BEGLON")]), ] %>%
  group_by(YEAR) %>% summarize(count = n())
#View(unique_tow_number_GB)

for (i in 1:length(species)){
  GB_list[[i]] <- nefsc_all_spp_ordered %>% 
    filter(COMNAME == species[i]) %>%
    filter(Stock == "GB") %>% 
    select(YEAR, EXPCATCHNUM, EXPCATCHWT) %>%
    group_by(YEAR) %>% 
    summarise(across(c(EXPCATCHNUM, EXPCATCHWT), sum, na.rm = TRUE)) %>% #divide numerator counts by total number of tows
    rename(!!paste0(species[i], " NUM") := EXPCATCHNUM, !!paste0(species[i], " WT") := EXPCATCHWT) %>% 
    left_join(unique_tow_number_GB, by = c("YEAR")) 
    GB_list[[i]] <- GB_list[[i]] %>% mutate(!!names(GB_list[[i]])[2] := .[[2]]/count, !!names(GB_list[[i]])[3] := .[[3]]/count ) %>%
    clean_names()
}
GB_list2 <- reduce(GB_list, left_join, by = c("year", "count")) %>%
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>% select(-count)
#View(GB_list2)

#GB spring survey  

#determine number of tows in a given year for each stock region/season based on unique lat, long, year 
tow_number_GB_spring <- nefsc_all_spp_ordered %>%
  filter(Stock == "GB", SEASON == "SPRING") 
unique_tow_number_GB_spring <- tow_number_GB_spring[!duplicated(tow_number_GB_spring[, c("YEAR", "DECDEG_BEGLAT", "DECDEG_BEGLON")]), ] %>%
  group_by(YEAR) %>% summarize(count = n())
#View(unique_tow_number_GB_spring)

GB_springlist <- list()

for (i in 1:length(species)){
  GB_springlist[[i]] <- nefsc_all_spp_ordered %>% 
    filter(COMNAME == species[i]) %>%
    filter(Stock == "GB") %>%
    filter(SEASON == "SPRING") %>% 
    select(YEAR, EXPCATCHNUM, EXPCATCHWT) %>%
    group_by(YEAR) %>%
    summarise(across(c(EXPCATCHNUM, EXPCATCHWT), sum, na.rm = TRUE)) %>% #divide numerator counts by total number of tows
    rename(!!paste0(species[i], " NUM") := EXPCATCHNUM, !!paste0(species[i], " WT") := EXPCATCHWT) %>% 
    left_join(unique_tow_number_GB_spring, by = c("YEAR")) 
  GB_springlist[[i]] <- GB_springlist[[i]] %>% mutate(!!names(GB_springlist[[i]])[2] := .[[2]]/count, !!names(GB_springlist[[i]])[3] := .[[3]]/count ) %>%
    clean_names()
}
GB_springlist2 <- reduce(GB_springlist, left_join, by = c("year", "count")) %>%
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>% select(-count)
#View(GB_springlist2)

#GB fall survey

#determine number of tows in a given year for each stock region/season based on unique lat, long, year 
tow_number_GB_fall <- nefsc_all_spp_ordered %>%
  filter(Stock == "GB", SEASON == "FALL") 
unique_tow_number_GB_fall <- tow_number_GB_fall[!duplicated(tow_number_GB_fall[, c("YEAR", "DECDEG_BEGLAT", "DECDEG_BEGLON")]), ] %>%
  group_by(YEAR) %>% summarize(count = n())
#View(unique_tow_number_GB_fall)

GB_falllist <- list()

for (i in 1:length(species)){
  GB_falllist[[i]] <- nefsc_all_spp_ordered %>% 
    filter(COMNAME == species[i]) %>%
    filter(Stock == "GB") %>% 
    filter(SEASON == "FALL") %>% 
    select(YEAR, EXPCATCHNUM, EXPCATCHWT) %>%
    group_by(YEAR) %>%
    summarise(across(c(EXPCATCHNUM, EXPCATCHWT), sum, na.rm = TRUE)) %>% #divide numerator counts by total number of tows
    rename(!!paste0(species[i], " NUM") := EXPCATCHNUM, !!paste0(species[i], " WT") := EXPCATCHWT) %>% 
    left_join(unique_tow_number_GB_fall, by = c("YEAR")) 
  GB_falllist[[i]] <- GB_falllist[[i]] %>% mutate(!!names(GB_falllist[[i]])[2] := .[[2]]/count, !!names(GB_falllist[[i]])[3] := .[[3]]/count ) %>%
    clean_names()
}
GB_falllist2 <- reduce(GB_falllist, left_join, by = c("year", "count")) %>%
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>% select(-count)
#View(GB_falllist[[i]])

################ Gulf of Maine ################### 

#GOM both surveys

#determine number of tows in a given year for each stock region/season based on unique lat, long, year 
tow_number_GOM <- nefsc_all_spp_ordered %>%
  filter(Stock == "GOM") 
unique_tow_number_GOM <- tow_number_GOM[!duplicated(tow_number_GOM[, c("YEAR", "DECDEG_BEGLAT", "DECDEG_BEGLON")]), ] %>%
  group_by(YEAR) %>% summarize(count = n())
#View(unique_tow_number_GOM)

GOM_list <- list()

for (i in 1:length(species)){
  GOM_list[[i]] <- nefsc_all_spp_ordered %>% 
    filter(COMNAME == species[i]) %>%
    filter(Stock == "GOM") %>%
    select(YEAR, EXPCATCHNUM, EXPCATCHWT) %>%
    group_by(YEAR) %>%
    summarise(across(c(EXPCATCHNUM, EXPCATCHWT), sum, na.rm = TRUE)) %>% #divide numerator counts by total number of tows
    rename(!!paste0(species[i], " NUM") := EXPCATCHNUM, !!paste0(species[i], " WT") := EXPCATCHWT) %>% 
    left_join(unique_tow_number_GOM, by = c("YEAR")) 
    GOM_list[[i]] <- GOM_list[[i]] %>% mutate(!!names(GOM_list[[i]])[2] := .[[2]]/count, !!names(GOM_list[[i]])[3] := .[[3]]/count ) %>%
    clean_names()
}
GOM_list2 <- reduce(GOM_list, left_join, by = c("year", "count")) %>%
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>% select(-count)

#GOM spring survey  

#determine number of tows in a given year for each stock region/season based on unique lat, long, year 
tow_number_GOM_spring <- nefsc_all_spp_ordered %>%
  filter(Stock == "GOM", SEASON == "SPRING") 
unique_tow_number_GOM_spring <- tow_number_GOM_spring[!duplicated(tow_number_GOM_spring[, c("YEAR", "DECDEG_BEGLAT", "DECDEG_BEGLON")]), ] %>%
  group_by(YEAR) %>% summarize(count = n())
#View(unique_tow_number_GOM_spring)

GOM_springlist <- list()

for (i in 1:length(species)){
  GOM_springlist[[i]] <- nefsc_all_spp_ordered %>% 
    filter(COMNAME == species[i]) %>%
    filter(Stock == "GOM") %>% 
    filter(SEASON == "SPRING") %>% 
    select(YEAR, EXPCATCHNUM, EXPCATCHWT) %>%
    group_by(YEAR) %>%
    summarise(across(c(EXPCATCHNUM, EXPCATCHWT), sum, na.rm = TRUE)) %>% #divide numerator counts by total number of tows
    rename(!!paste0(species[i], " NUM") := EXPCATCHNUM, !!paste0(species[i], " WT") := EXPCATCHWT) %>% 
    left_join(unique_tow_number_GOM_spring, by = c("YEAR")) 
    GOM_springlist[[i]] <- GOM_springlist[[i]] %>% mutate(!!names(GOM_springlist[[i]])[2] := .[[2]]/count, !!names(GOM_springlist[[i]])[3] := .[[3]]/count ) %>%
    clean_names()
}
GOM_springlist2 <- reduce(GOM_springlist, left_join, by = c("year", "count")) %>%
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>% select(-count)

#GOM fall survey

#determine number of tows in a given year for each stock region/season based on unique lat, long, year 
tow_number_GOM_fall <- nefsc_all_spp_ordered %>%
  filter(Stock == "GOM", SEASON == "FALL") 
unique_tow_number_GOM_fall <- tow_number_GOM_fall[!duplicated(tow_number_GOM_fall[, c("YEAR", "DECDEG_BEGLAT", "DECDEG_BEGLON")]), ] %>%
  group_by(YEAR) %>% summarize(count = n())
#View(tow_number_GOM_fall)
#View(unique_tow_number_GOM_fall)

GOM_falllist <- list()

for (i in 1:length(species)){
  GOM_falllist[[i]] <- nefsc_all_spp_ordered %>% 
    filter(COMNAME == species[i]) %>%
    filter(Stock == "GOM") %>% 
    filter(SEASON == "FALL") %>% 
    select(YEAR, EXPCATCHNUM, EXPCATCHWT) %>%
    group_by(YEAR) %>%
    summarise(across(c(EXPCATCHNUM, EXPCATCHWT), sum, na.rm = TRUE)) %>% #divide numerator counts by total number of tows
    rename(!!paste0(species[i], " NUM") := EXPCATCHNUM, !!paste0(species[i], " WT") := EXPCATCHWT) %>% 
    left_join(unique_tow_number_GOM_fall, by = c("YEAR")) 
  GOM_falllist[[i]] <- GOM_falllist[[i]] %>% mutate(!!names(GOM_falllist[[i]])[2] := .[[2]]/count, !!names(GOM_falllist[[i]])[3] := .[[3]]/count ) %>%
    clean_names()
}
GOM_falllist2 <- reduce(GOM_falllist, left_join, by = c("year", "count")) %>%
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>% select(-count)
#View(GOM_falllist[[i]])

################ Southern New England Mid Atlantic ################### 

#SNEMA both surveys

#determine number of tows in a given year for each stock region/season based on unique lat, long, year 
tow_number_SNEMA <- nefsc_all_spp_ordered %>%
  filter(Stock == "SNEMA") 
unique_tow_number_SNEMA <- tow_number_SNEMA[!duplicated(tow_number_SNEMA[, c("YEAR", "DECDEG_BEGLAT", "DECDEG_BEGLON")]), ] %>%
  group_by(YEAR) %>% summarize(count = n())
#View(unique_tow_number_SNEMA)

SNEMA_list <- list()

for (i in 1:length(species)){
  SNEMA_list[[i]] <- nefsc_all_spp_ordered %>% 
    filter(COMNAME == species[i]) %>%
    filter(Stock == "SNEMA") %>% 
    select(YEAR, EXPCATCHNUM, EXPCATCHWT) %>%
    group_by(YEAR) %>%
    summarise(across(c(EXPCATCHNUM, EXPCATCHWT), sum, na.rm = TRUE)) %>% #divide numerator counts by total number of tows
    rename(!!paste0(species[i], " NUM") := EXPCATCHNUM, !!paste0(species[i], " WT") := EXPCATCHWT) %>% 
    left_join(unique_tow_number_SNEMA, by = c("YEAR")) 
  SNEMA_list[[i]] <- SNEMA_list[[i]] %>% mutate(!!names(SNEMA_list[[i]])[2] := .[[2]]/count, !!names(SNEMA_list[[i]])[3] := .[[3]]/count ) %>%
    clean_names()
}
SNEMA_list2 <- reduce(SNEMA_list, left_join, by = c("year", "count")) %>%
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>% select(-count)


#SNEMA spring survey  

#determine number of tows in a given year for each stock region/season based on unique lat, long, year 
tow_number_SNEMA_spring <- nefsc_all_spp_ordered %>%
  filter(Stock == "SNEMA", SEASON == "SPRING") 
unique_tow_number_SNEMA_spring <- tow_number_SNEMA_spring[!duplicated(tow_number_SNEMA_spring[, c("YEAR", "DECDEG_BEGLAT", "DECDEG_BEGLON")]), ] %>%
  group_by(YEAR) %>% summarize(count = n())
#View(tow_number_SNEMA_spring %>% filter(YEAR == 2023))
#View(unique_tow_number_SNEMA_spring)

SNEMA_springlist <- list()

for (i in 1:length(species)){
  SNEMA_springlist[[i]] <- nefsc_all_spp_ordered %>% 
    filter(COMNAME == species[i]) %>%
    filter(Stock == "SNEMA") %>% 
    filter(SEASON == "SPRING") %>% 
    select(YEAR, EXPCATCHNUM, EXPCATCHWT) %>%
    group_by(YEAR) %>%
    summarise(across(c(EXPCATCHNUM, EXPCATCHWT), sum, na.rm = TRUE)) %>% #divide numerator counts by total number of tows
    rename(!!paste0(species[i], " NUM") := EXPCATCHNUM, !!paste0(species[i], " WT") := EXPCATCHWT) %>% 
    left_join(unique_tow_number_SNEMA_spring, by = c("YEAR")) 
  SNEMA_springlist[[i]] <- SNEMA_springlist[[i]] %>% mutate(!!names(SNEMA_springlist[[i]])[2] := .[[2]]/count, !!names(SNEMA_springlist[[i]])[3] := .[[3]]/count ) %>%
    clean_names()
}
SNEMA_springlist2 <- reduce(SNEMA_springlist, left_join, by = c("year", "count")) %>%
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>% select(-count)

#SNEMA fall survey

#why were there only fall three tows in 2017, even though some of them caught massive quantities of a species?
#tow_number_SNEMA_fall <- nefsc_all_spp_ordered %>% filter(Stock == "SNEMA", SEASON == "FALL", YEAR == 2017)

#determine number of tows in a given year for each stock region/season based on unique lat, long, year 
tow_number_SNEMA_fall <- nefsc_all_spp_ordered %>%
  filter(Stock == "SNEMA", SEASON == "FALL") 
unique_tow_number_SNEMA_fall <- tow_number_SNEMA_fall[!duplicated(tow_number_SNEMA_fall[, c("YEAR", "DECDEG_BEGLAT", "DECDEG_BEGLON")]), ] %>%
  group_by(YEAR) %>% summarize(count = n())
#View(unique_tow_number_SNEMA_fall)

SNEMA_falllist <- list()

for (i in 1:length(species)){
  SNEMA_falllist[[i]] <- nefsc_all_spp_ordered %>% 
    filter(COMNAME == species[i]) %>%
    filter(Stock == "SNEMA") %>% 
    filter(SEASON == "FALL") %>% 
    select(YEAR, EXPCATCHNUM, EXPCATCHWT) %>%
    group_by(YEAR) %>%
    summarise(across(c(EXPCATCHNUM, EXPCATCHWT), sum, na.rm = TRUE)) %>% #divide numerator counts by total number of tows
    rename(!!paste0(species[i], " NUM") := EXPCATCHNUM, !!paste0(species[i], " WT") := EXPCATCHWT) %>% 
    left_join(unique_tow_number_SNEMA_fall, by = c("YEAR")) 
  SNEMA_falllist[[i]] <- SNEMA_falllist[[i]] %>% mutate(!!names(SNEMA_falllist[[i]])[2] := .[[2]]/count, !!names(SNEMA_falllist[[i]])[3] := .[[3]]/count ) %>%
    clean_names()
}
SNEMA_falllist2 <- reduce(SNEMA_falllist, left_join, by = c("year", "count")) %>%
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>% select(-count)

NEFSC_species <- list(
  GB_all =  GB_list2,
  GB_spring = GB_springlist2,
  GB_fall =  GB_falllist2,
  GOM_all =  GOM_list2,
  GOM_spring = GOM_springlist2,
  GOM_fall =  GOM_falllist2,
  SNEMA_all = SNEMA_list2,
  SNEMA_spring = SNEMA_springlist2,
  SNEMA_fall = SNEMA_falllist2
)

#View(NEFSC_species$GB_spring)

#save file for subsequent analyses 
#saveRDS(NEFSC_species, "~/Desktop/StockAssessmentPracticum/WF_NaturalMortality/NEFSC_species_allship_catchrate_.rds")

