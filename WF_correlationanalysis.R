library(dplyr)
library(tidyr)
library(data.table)
library(mgcv)
library(gratia)
library(purrr)
library(janitor)
library(ggplot2)

#sheet for initial correlation analyses for 2025 WF RT TOR1
#use bottom analyses ending in .rds "NEFSC_species_allship_catchrate_.rds" that accounts for tows without fish <- now the only thing in this script 

nefsc_all_spp <- read.csv("~/Desktop/StockAssessmentPracticum/WF_NaturalMortality/nefsc_all_spp.csv")

#replace NA w/0s
nefsc_all_spp_ordered <-  nefsc_all_spp[order(nefsc_all_spp$YEAR),] %>% 
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) #%>% filter(SVVESSEL == "AL") 
#View(nefsc_all_spp_ordered)

#test functions for extracting info 
tow_number_GB_test <- nefsc_all_spp_ordered %>%
  filter(Stock == "GB", SEASON == "FALL", STRATUM %in% c(1130:1210)) 
unique_tow_number_GB_test <- tow_number_GB_test[!duplicated(tow_number_GB_test[, c("YEAR", "DECDEG_BEGLAT", "DECDEG_BEGLON")]), ] %>%
  group_by(YEAR) %>% summarize(count = n())
#View(unique_tow_number_GB_test)

nefsc_all_spp_yt_GBfall <- nefsc_all_spp_ordered %>% 
  filter(Stock == "GB", COMNAME == "YELLOWTAIL FLOUNDER", 
         SEASON == "SPRING", STRATUM %in% c(1130:1210), 
         YEAR %in% c(1963:2013) 
         ) %>%
  group_by(YEAR) %>% 
  summarise(across(c(EXPCATCHNUM, EXPCATCHWT), sum, na.rm = TRUE)) %>% 
  left_join(unique_tow_number_GB_test, by = c("YEAR")) %>% 
  mutate(EXPCATCHNUM = EXPCATCHNUM/count, EXPCATCHWT = EXPCATCHWT/count)
#View(nefsc_all_spp_yt_GBfall)
plot(nefsc_all_spp_yt_GBfall$EXPCATCHNUM~nefsc_all_spp_yt_GBfall$YEAR, type = "b")
plot(nefsc_all_spp_yt_GBfall$EXPCATCHWT~nefsc_all_spp_yt_GBfall$YEAR, type = "b")

nefsc_all_spp_sd_GBfall <- nefsc_all_spp_ordered %>% 
  filter(Stock == "GB", COMNAME == "SPINY DOGFISH", 
         SEASON == "SPRING", STRATUM %in% c(1130:1210), 
         YEAR %in% c(1963:2013) 
  ) %>%
  group_by(YEAR) %>% 
  summarise(across(c(EXPCATCHNUM, EXPCATCHWT), sum, na.rm = TRUE)) %>% 
  left_join(unique_tow_number_GB_test, by = c("YEAR")) %>% 
  mutate(EXPCATCHNUM = EXPCATCHNUM/count, EXPCATCHWT = EXPCATCHWT/count)
#View(nefsc_all_spp_sd_GBfall)
plot(nefsc_all_spp_sd_GBfall$EXPCATCHNUM~nefsc_all_spp_sd_GBfall$YEAR, type = "b")
plot(nefsc_all_spp_sd_GBfall$EXPCATCHWT~nefsc_all_spp_yt_GBfall$YEAR, type = "b")

plot(nefsc_all_spp_sd_GBfall$EXPCATCHNUM~nefsc_all_spp_yt_GBfall$EXPCATCHNUM)
plot(nefsc_all_spp_sd_GBfall$EXPCATCHWT~nefsc_all_spp_yt_GBfall$EXPCATCHWT)

#nefsc_all_spp_ordered_scup <-  nefsc_all_spp[order(nefsc_all_spp$YEAR),] %>% 
#  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>% filter(SEASON == "FALL", COMNAME == "SCUP") 
#View(nefsc_all_spp_ordered_scup)

# COMPARE ALBATROSS W/BIGELOW
nefsc_all_spp_ordered_AL <-  nefsc_all_spp[order(nefsc_all_spp$YEAR),] %>% 
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>% group_by(COMNAME) %>%
  filter(SVVESSEL == "AL") %>% filter(COMNAME %in% species) %>% 
  summarise(across(c(EXPCATCHNUM, EXPCATCHWT), mean, na.rm = TRUE)) %>%
  rename(EXPCATCHNUM_AL = EXPCATCHNUM, EXPCATCHWT_AL = EXPCATCHWT)
hist(nefsc_all_spp_ordered_AL$EXPCATCHNUM_AL)

nefsc_all_spp_ordered_BIG <-  nefsc_all_spp[order(nefsc_all_spp$YEAR),] %>% 
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>% group_by(COMNAME) %>%
  filter(SVVESSEL == "HB") %>% filter(COMNAME %in% species) %>% 
  summarise(across(c(EXPCATCHNUM, EXPCATCHWT), mean, na.rm = TRUE)) %>%
  rename(EXPCATCHNUM_BIG = EXPCATCHNUM, EXPCATCHWT_BIG = EXPCATCHWT)
hist(nefsc_all_spp_ordered_BIG$EXPCATCHNUM_BIG)

nefsc_all_spp_ordered_compare <- nefsc_all_spp_ordered_BIG %>% left_join(nefsc_all_spp_ordered_AL, by = "COMNAME")
#View(nefsc_all_spp_ordered_compare)

#plot albatross vs. bigelow 
plot(nefsc_all_spp_ordered_compare$EXPCATCHNUM_BIG ~ nefsc_all_spp_ordered_compare$EXPCATCHNUM_AL) + abline(a=0, b=1)
plot(nefsc_all_spp_ordered_compare$EXPCATCHNUM_BIG ~ nefsc_all_spp_ordered_compare$EXPCATCHNUM_AL, xlim = c(0,40), ylim = c(0,100)) + abline(a=0, b=1)

ggplot() + geom_point(nefsc_all_spp_ordered_compare, mapping = aes(x=EXPCATCHNUM_AL, 
            y = EXPCATCHNUM_BIG, col = COMNAME)) + geom_abline(slope = 1, intercept = 0) +
            xlim(0,40) +  ylim(0,100)

#PRINT NAMES 
print(unique(nefsc_all_spp$COMNAME))

#From lit review: name, number of entries, total # caught; don't trust these specific numbers I just wanted to get a rough sense of how many there were
 #silver hake 15493; 2955185
 #goosefish 6126; 23961
 #american plaice 5045; 159673
 #longhorn sculpin 8959; 297885
 #sea raven 6210; 24206
 #witch flounder 3475; 30963
 #white hake 4544; 44091
 #red hake 10987; 446799
 #haddock 6260; 1286061
 #spiny dogfish 15862; 1238606
 #ocean pout 6535; 63730
 #atlantic cod 6456; 78888
 #cunner 1189; 8063
 #scup 2978; 885068
 #summer flounder 4750; 26016
 #bluefish 1581; 12980
 #striped bass 196; 1175
 #clearnose skate 846; 6299
 #windowpane 8285; 113089
 #yellowtail flounder 8165; 210730
 #fourspot flounder 8651; 172230
 #winter skate -> might aggregate all skates into one category 8297; 171747
 #barndoor skate 2416; 12719
 #little skate 13468; 606886
 #smooth skate 1289; 4300
 #thorny skate 3005; 12297
 #rosette skate 348; 1397
 #striped searobin 1218; 10538
 #spotted hake 5095; 181694
 #weakfish 594; 45343

species <- c("WINTER FLOUNDER", "SILVER HAKE", "GOOSEFISH", "YELLOWTAIL FLOUNDER", "LONGHORN SCULPIN" ,
             "SEA RAVEN", "AMERICAN PLAICE", "WITCH FLOUNDER", "WHITE HAKE", "RED HAKE", "HADDOCK",
             "SPINY DOGFISH", "OCEAN POUT", "ATLANTIC COD", "CUNNER", "SCUP", "SUMMER FLOUNDER", 
             "BLUEFISH", "STRIPED BASS", "WINDOWPANE", "FOURSPOT FLOUNDER", "WINTER SKATE", 
             "BARNDOOR SKATE", "LITTLE SKATE", "SMOOTH SKATE", "THORNY SKATE", "CLEARNOSE SKATE",
             "ROSETTE SKATE", "STRIPED SEAROBIN", "SPOTTED HAKE", "WEAKFISH")

#just to see if any species that we would not expect to have any kind of relationship still show up as having them
#species <- c("WINTER FLOUNDER", "SILVER HAKE", "GOOSEFISH", "YELLOWTAIL FLOUNDER", "LONGHORN SCULPIN" ,
#             "SEA RAVEN", "AMERICAN PLAICE", "WITCH FLOUNDER", "WHITE HAKE", "RED HAKE", "HADDOCK",
#             "SPINY DOGFISH", "OCEAN POUT", "ATLANTIC COD", "CUNNER", "SCUP", "SUMMER FLOUNDER", 
#             "BLUEFISH", "STRIPED BASS", "WINDOWPANE", "FOURSPOT FLOUNDER", "WINTER SKATE", 
#             "BARNDOOR SKATE", "LITTLE SKATE", "SMOOTH SKATE", "THORNY SKATE", "CLEARNOSE SKATE",
             
#             "SEA SCALLOP", "ATLANTIC MACKEREL", "DAUBED SHANNY", "NORTHERN SHORTFIN SQUID", "BUTTERFISH",
#             "SMOOTH DOGFISH", "SILVER ANCHOVY", "CAPELIN", "POLLOCK",
#             "JONAH CRAB", "ATLANTIC HERRING", 
#             "ALEWIFE", "BLUEBACK HERRING", "AMERICAN SHAD", "NORTHERN SAND LANCE", "ATLANTIC HAGFISH", 
#             "CUSK")


#maybe for literature review look into redfish and cusk? Pollock? 

#make sure all the proper names are correctly filtered out 
#aggregate by total number per year, stock unit, stratum, etc. 
#left join the other species' annual abundances by year 

#winterflounder <- nefsc_all_spp %>% filter(COMNAME %in% c("WINTER FLOUNDER")) %>% mutate(Species = "Winter Flounder")
#winterflounder <- winterflounder[order(winterflounder$YEAR),]
#View(winterflounder)

#winterflounder_GB_spring <- winterflounder %>% filter(Stock == "GB") %>% 
#  filter(SEASON == "SPRING") %>% 
#  select(YEAR, EXPCATCHNUM, EXPCATCHWT) %>%
#  group_by(YEAR) %>%
#  summarise(across(c(EXPCATCHNUM, EXPCATCHWT), sum, na.rm = TRUE)) %>% 
#  rename(WinterFlounder_Num = EXPCATCHNUM, WinterFlounder_Wt = EXPCATCHWT)

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
saveRDS(NEFSC_species, "~/Desktop/StockAssessmentPracticum/WF_NaturalMortality/NEFSC_species_allship_catchrate_.rds")

