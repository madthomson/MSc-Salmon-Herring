#Merge GSI results into a single data frame and do some basic cleaning

#Load packages -----------------------------------------------------------------
library(openxlsx)
library(tidyverse)

#Load GSI data -----------------------------------------------------------------

#GSI results 2024
gsi24.results <- read.xlsx("./Core-Fieldwork/Field-Data/Raw-Data/GSI-Results/PID20250125(1)_Survival_BN_Herring(24)_sc735_2026-02-27_NF.xlsx", 5)
gsi24.info <- read.xlsx("./Core-Fieldwork/Field-Data/Raw-Data/GSI-Results/PID20250125(1)_Survival_BN_Herring(24)_sc735_2026-02-27_NF.xlsx", 9)
gsi24.species <- read.xlsx("./Core-Fieldwork/Field-Data/Raw-Data/GSI-Results/PID20250125(1)_Survival_BN_Herring(24)_sc735_2026-02-27_NF.xlsx", 10)

gsi24 <- full_join(gsi24.info, gsi24.results)
gsi24 <- full_join(gsi24, gsi24.species)

colnames(gsi24)

gsi24 <- gsi24 %>%
  dplyr::select(indiv, Vial, ID_Source, collection, mixture_collection,
                PBT_brood_year, PBT_brood_group, PBT_brood_collection,
                repunit.1, prob.1, species, notes)


#GSI results 2025 (part 1)
gsi25.1.results <- read.xlsx("./Core-Fieldwork/Field-Data/Raw-Data/GSI-Results/PID20250125(2)_Survival_BN_Herring(25)_sc736_2026-03-10_NF.xlsx", 5)
gsi25.1.info <- read.xlsx("./Core-Fieldwork/Field-Data/Raw-Data/GSI-Results/PID20250125(2)_Survival_BN_Herring(25)_sc736_2026-03-10_NF.xlsx", 9)
gsi25.1.species <- read.xlsx("./Core-Fieldwork/Field-Data/Raw-Data/GSI-Results/PID20250125(2)_Survival_BN_Herring(25)_sc736_2026-03-10_NF.xlsx", 10)


gsi25.1 <- full_join(gsi25.1.info, gsi25.1.results)
gsi25.1 <- full_join(gsi25.1, gsi25.1.species)

colnames(gsi25.1)

gsi25.1 <- gsi25.1 %>%
  dplyr::select(indiv, Vial, ID_Source, collection, mixture_collection,
                PBT_brood_year, PBT_brood_group, PBT_brood_collection,
                repunit.1, prob.1, species, notes)


#GSI results 2025 (part 2)
gsi25.2.results <- read.xlsx("./Core-Fieldwork/Field-Data/Raw-Data/GSI-Results/PID20250125(3)_Survival_BN_Herring(25)_b3_sc778_2026-08-24_NF.xlsx", 5)
gsi25.2.info <- read.xlsx("./Core-Fieldwork/Field-Data/Raw-Data/GSI-Results/PID20250125(3)_Survival_BN_Herring(25)_b3_sc778_2026-08-24_NF.xlsx", 9)
gsi25.2.species <- read.xlsx("./Core-Fieldwork/Field-Data/Raw-Data/GSI-Results/PID20250125(3)_Survival_BN_Herring(25)_b3_sc778_2026-08-24_NF.xlsx", 10)


gsi25.2 <- full_join(gsi25.2.info, gsi25.2.results)
gsi25.2 <- full_join(gsi25.2, gsi25.2.species)

colnames(gsi25.2)

gsi25.2 <- gsi25.2 %>%
  dplyr::select(indiv, Vial, ID_Source, collection, mixture_collection,
                PBT_brood_year, PBT_brood_group, PBT_brood_collection,
                repunit.1, prob.1, species, notes)

#Merge GSI data ----------------------------------------------------------------

#combine GSI data
gsi <- rbind(gsi24, gsi25.1, gsi25.2)

#check no weird columns added
colnames(gsi)

#check number of rows is logical
nrow(gsi) - nrow(gsi24) - nrow(gsi25.1) - nrow(gsi25.2)

#Check for duplicated results --------------------------------------------------

#check for duplicates
gsi <- gsi %>% mutate(duplicated = case_when(
  duplicated(Vial) | duplicated(Vial, fromLast = TRUE) ~ "Yes",
  TRUE ~ "No"))
dup <- gsi %>% filter(duplicated == "Yes")
nrow(dup)

#if the result is duplicated, take the second instance (prob from the re-run)
gsi <- gsi %>% filter(
  !(duplicated == "Yes" & !is.na(indiv) & 
      collection %in% c("Survival_BN_Herring(25)", "Survival_BN_Herring(24)")))

#check for duplicates again
gsi <- gsi %>% mutate(duplicated = case_when(
  duplicated(Vial) | duplicated(Vial, fromLast = TRUE) ~ "Yes",
  TRUE ~ "No"))
dup <- gsi %>% filter(duplicated == "Yes")
nrow(dup)

#looks good, remove duplicated column
gsi <- gsi %>% dplyr::select(-duplicated)

#Update species formatting -----------------------------------------------------

unique(gsi$species_original)
unique(gsi$species)

#look at notes for fish with multiple species listed
test <- gsi %>% filter(species %in% c("chinook; sockeye", "coho; chinook"))

gsi <- gsi %>% mutate(species = case_when(
  species == "none" ~ "None",
  species == "chinook" ~ "Chinook",
  species == "coho" ~ "Coho",
  species == "chum" ~ "Chum",
  notes == "Strong Chinook ID, likely genotyping error, assume Chinook" ~ "Chinook",
  notes == "Strong Chinok ID, likely genotyping error, assume Chinook" ~ "Chinook",
  notes == "Species ID ambiguous, use caution with result" ~ "Ambiguous",
  notes == "Likely Coho, removed from analysis" ~ "Coho"))

#check that species names were corrected
unique(gsi$species)

#Save results ------------------------------------------------------------------

write.csv(gsi, paste0("./Core-Fieldwork/Field-Data/Raw-Data/GSI-Results/Merged/PSF-A1-Core-Fieldwork-GSI-Results-", Sys.Date(), ".csv"))


# 
# #Check that results have been received for all fish ----------------------------
# 
# #read field data
# field <- read.csv("./Epicollect-Field-Data/Merged-Data/2026-09-03-psf-herring-A1-field-full-catch-data.csv")
# 
# #select minimal columns of interest
# field <- field %>% dplyr::select(general_location, date, event_type, scale_book, scale_no, species)
# 
# #rename species column "species_orginal" to distinguish from GSI result
# field <- field %>% rename(species_original = species)
# 
# #select only rows with scale data
# field <- field %>% filter(!is.na(scale_no))
# 
# #create scale ID column to match "Vial" column in genetics data
# field <- field %>% mutate(Vial = paste(scale_book, sprintf("%02d", scale_no), sep = "-"))
# head(sort(field$Vial))
# 
# #join with GSI data
# gsi <- full_join(field, gsi)
# 
# #relocate species column
# gsi <- gsi %>% relocate(species_original, .before = species)
# 
# #filter out coho that we don't expect to have a GSI code (typically not submitted)
# gsi <- gsi %>%  filter(!(is.na(indiv) & species_original == "Coho"))
# 
# #check for duplicates
# gsi <- gsi %>% mutate(duplicated = case_when(
#   duplicated(Vial) | duplicated(Vial, fromLast = TRUE) ~ "Yes",
#   TRUE ~ "No"))
# 
# #look at the duplicates
# dup <- gsi %>% filter(duplicated == "Yes")
# nrow(dup)
# 
# #fish that are duplicated with no results were probably were filtered out of the 
# #initial selection - get rid of these
# gsi <- gsi %>% filter(!(duplicated == "Yes" & is.na(indiv)))
# 
# ##look at the duplicates again
# dup <- gsi %>% filter(duplicated == "Yes")
# nrow(dup)
# 
# #looks good, remove duplicated column
# gsi <- gsi %>% dplyr::select(-duplicated)
# 
# #check for missing results
# test <- gsi %>% filter(is.na(indiv))
# nrow(test)
# 
# #PSF0092-46 no results, but not in original selection (not sure why...)
# gsi <- gsi %>% filter(Vial != "PSF0092-46" | is.na(Vial))
# 
# #check for results with no metadata
# test <- gsi %>% filter(is.na(date))
# nrow(test)
# #none
# 
# #no samples not run..? wow