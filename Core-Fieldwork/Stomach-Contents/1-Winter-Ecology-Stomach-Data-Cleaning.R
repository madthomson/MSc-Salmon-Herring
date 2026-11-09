#REVIEW THIS CODE CAREFULLY WHEN ADDING NEW DATA
#if new taxa were recorded, they will need to be added

##ADD QC isotope codes

#load packages -----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(lubridate)

#load data ---------------------------------------------------------------------

stom_data <- read.csv("./Core-Fieldwork/Stomach-Contents/Raw-Data/Winter-Ecology/2026-04-24_Year6_Stomach_data_KI.csv", sep=",", na.strings=c("","NA"))
colnames(stom_data)
head(stom_data)

stom_data <- stom_data %>% dplyr::select(-c(X, X.1, X.2, X.3, X.4, X.5, X.6, X.7))

stom_data <- stom_data %>% filter(!is.na(RowID))

sort(unique(stom_data$Taxon)) 

#extract the year, month, and date ---------------------------------------------

stom_data <- stom_data %>%
  mutate(year = year(CaptureDate_YYYY.MM.DD),
         month = month(CaptureDate_YYYY.MM.DD),
         day = day(CaptureDate_YYYY.MM.DD)) %>%
  relocate(year, month, day, .after = CaptureDate_YYYY.MM.DD) 

#filter only data from September (these are from herring project) --------------

unique(stom_data$month)
stom_data <- stom_data %>%
  filter(!month %in% c(10, 11, 12, 1, 2, 3, 4))


#create taxon labels that match herring data -----------------------------------

stom_data <- stom_data %>%
  mutate(prey = case_when(
    Taxon %in% c("Diptera")               ~ "Diptera",
    Taxon %in% c("Larval fish")           ~ "LarvalFish",
    Taxon %in% c("Fish fragments")        ~ "UnidFish",
    Taxon %in% c("Larval flatfish")       ~ "Flatfish",
    Taxon %in% c("Herring", 
                 "Herring ", 
                 "Herring fragments", 
                 "Pacific Herring")       ~ "Herring",
    Taxon %in% c("Northern Anchovy")      ~ "Anchovy",
    Taxon %in% c("Sand lance")            ~ "Sandlance",
    Taxon %in% c("Myctophid")             ~ "Myctophid",
    Taxon %in% c("Anomuran megalopae", 
                 "Anomuran megalopaee", 
                 "Brachyuran megalopae", 
                 "Cancrid megalopae",
                 "Decapod megalopae",
                 "Squat lobster",
                 "Anomuran megalopaea",
                 "Brachyuran crab",
                 "Pagurid megalopa",
                 "Cancrid megalopa")       ~ "CrabMegalops",
    Taxon %in% c("Brachyuran zoeae", 
                 "Porcellanid zoeae", 
                 "Decapod zoeae",
                 "brachyuran zoeae")      ~ "CrabZoea",
    Taxon %in% c("Barnacle cirri")        ~ "CirripedeExuvium",
    Taxon %in% c("Amphipod", 
                 "Amphipod fragments")    ~ "Amphipods", 
    Taxon %in% c("Hyperiid", 
                 "Hyperiid ",
                 "Hyperiids")             ~ "Hyperiids",
    Taxon %in% c("Hyperia")               ~ "Hyperia",
    Taxon %in% c("Hyperoche")             ~ "Hyperoche",
    Taxon %in% c("Themisto")              ~ "Themisto",
    Taxon %in% c("Primno",
                 "Primno ")               ~ "Primno",
    Taxon %in% c("Cyphocaris")            ~ "Cyphocaris",
    Taxon %in% c("Euphausiid fragments")  ~ "Euphausiids",
    Taxon %in% c("Euphausia pacifica")    ~ "Euphausia",
    Taxon %in% c("Thysanoessa spinifera", 
                 "Thysanoessa")           ~ "Thysanoessa",
    Taxon %in% c("Paraeuchaeta", 
                 "Copepod")               ~ "CalanoidCopepod",
    Taxon %in% c("Copepod Sea Louse", 
                 "Parasitic copepod")     ~ "CopepodParasite",
    Taxon %in% c("Pinnotherid")           ~ "PinnotheridCrab",
    Taxon %in% c("Nematode parasite")     ~ "Nematodes",
    Taxon %in% c("Plant material")        ~ "Plant",
    Taxon %in% c("Unidentified material") ~ "UnidMaterial",
    Taxon %in% c("Shrimp fragments", 
                 "Shrimp larvae", 
                 "Shrimp")                ~ "Shrimp",
    Taxon %in% c("Crustacean fragments")  ~ "Crustaceans",
    Taxon %in% c("Gammarid")              ~ "Gammarids",
    Taxon %in% c("Fish scales",
                 "Fish scale")            ~ "FishScales",
    Taxon %in% c("Limacina")              ~ "Pteropods",
    Taxon %in% c("Cephalopod")            ~ "CephalopodLarva",
    Taxon %in% c("Squid", 
                 "Squid larvae",
                 "Squid fragments")       ~ "SquidLarva",
    Taxon %in% c("Octopus")               ~ "OctopusLarva",
    Taxon %in% c("Polychaete")            ~ "Polychaetes",
    Taxon %in% c("Larval poacher")        ~ "Poacher",
    Taxon %in% c("Plastic")               ~ "Plastic",
    Taxon %in% c("Decapod larvae", 
                 "Decapod fragments")     ~ "Decapods",
    Taxon %in% c("Mysid", 
                 "Mysid fragments")       ~ "Mysids",
    Taxon %in% c("Insect", 
                 "Insect fragments", 
                 "Insects")               ~ "Insects",
    Taxon %in% c("Parasitic isopod")      ~ "IsopodParasite",
    Taxon %in% c("Corophid")              ~ "Corophids",
    Taxon %in% c("Caprellid")             ~ "Caprellids",
    Taxon %in% c("Snail")                 ~ "Molluscs"
    )) %>%
  relocate(prey, .after = Taxon)

#check for taxon not assigned to new categories
test <- stom_data %>% filter(is.na(prey))
nrow(test)
unique(test$Taxon)

#rename columns
stom_data <- stom_data %>%
  rename(analysis_date = AnalysisDate_YYYY.MM.DD,
         date = CaptureDate_YYYY.MM.DD,
         lavage_bag = Stomach_Bag,
         scale_book = Scale_Book,
         scale_no = Scale,
         site_name = Site,
         mass_g = Weight_g,
         digestion_score = Digestion,
         id_features = Hard_Parts_Codes,
         fish_SL_mm = Prey_Length_mm,
         isotope_code = SI_Sample,
         otolith_count = Otolith_Num,
         otolith_vial = Otolith_ID,
         analyst = Analyst,
         comments = Notes,
         prey_we = Taxon,
         count = Count
         ) %>%
  dplyr::select(-c(RowID, Fish_ID))


# #replace blanks with NAs in select columns
# stom_data <- stom_data %>%
#   mutate(scale_no = case_when(scale_no == "" ~ NA, TRUE ~ scale_no)) %>%
#   mutate(scale_book = case_when(scale_book == "" ~ NA, TRUE ~ scale_book)) %>%
#   mutate(isotope_code = case_when(isotope_code == "" ~ NA, TRUE ~ isotope_code)) %>%
#   mutate(comments = case_when(comments == "" ~ NA, TRUE ~ comments)) %>%
#   mutate(id_features = case_when(id_features == "" ~ NA, TRUE ~ id_features))

stom_data <- stom_data %>%
  mutate(mass_g = as.numeric(mass_g))

#add sampling type
stom_data <- stom_data %>%
  mutate(event_type = case_when(
    date == "2025-07-18" ~ "Purse Seine",
    TRUE ~"Microtroll")) %>%
  relocate(event_type, .after = analysis_date)

#add digestion 5 for anything that includes the phrase "fragment"
stom_data <- stom_data %>%
  mutate(digestion_score = case_when(grepl(pattern = "fragment", x = prey_we) ~ 5,
                               TRUE ~ digestion_score))

#make general locations match herring data
unique(stom_data$site_name)
stom_data <- stom_data %>%
  mutate(general_location = case_when(
    site_name == "Comox" ~ "Comox",
    site_name == "Cowichan" ~ "SGI",
    site_name == "Nanaimo" ~ "Nanaimo",
    site_name %in% c("Campbell River North", 
                     "North Campbell River ") ~ "Campbell River",
    TRUE ~ site_name)) %>%
  mutate(survey_name = "BCSRIF Herring") %>%
  relocate(general_location, .after = date) 

unique(stom_data$general_location)


#replace capital letters in ID features
stom_data$id_features <- tolower(stom_data$id_features)



#QC - check for improperly recorded weights ------------------------------------

bad_weight_stom <- stom_data %>% 
  filter(mass_g < 0 | mass_g == 0 & !is.na(prey) | is.na(mass_g) & !is.na(prey) | mass_g > 20) 
nrow(bad_weight_stom)

#only one row with a bad weight. I recall this was just otoliths that Maeve
#set aside for Micah. Assign weight as the minimum weight (0.0001)

stom_data <- stom_data %>%
  mutate(mass_g = case_when(ID == "45924-74-Campbell River" ~ 0.0001,
                            TRUE ~ mass_g))

#take a look at all the bad weights to see if any of them can be fixed

#create a list of bad weights - add a column to any printed data frame 
#that specifies to use these stomachs for presence-absence analysis only
#and replace weights with NA

#NOTE: this doesn't filter out weights greater than 20 g - this should be 
#assessed on a case-by-case basis


stom_data <- stom_data %>%
  #group by variables that uniquely identify a stomach
  group_by(date, general_location, lavage_bag) %>%
  #create a column with bad_weight = TRUE if the weight is negative, zero, or missing
  mutate(
    bad_weight = case_when(
      (mass_g < 0) ~ TRUE,
      (mass_g == 0 & !is.na(prey)) ~ TRUE,
      (is.na(mass_g) & !is.na(prey)) ~ TRUE,
      TRUE ~ FALSE)) %>%
  #update column so that the entire stomach has bad_weight = TRUE
  #if any of the prey rows from that stomach have bad_weight = TRUE
  mutate(bad_weight = any(bad_weight))


#general stomach summary -------------------------------------------------------

#create groups to match herring data (inverts all grouped together)
stom_summary <- stom_data %>%
  mutate(group = case_when(
    prey %in% c("CrabMegalops", "CrabZoea", "CirripedeExuvium", "Amphipods",
                "Hyperiids", "Hyperia", "Hyperoche", "Themisto", "Primno",
                "Euphausiids", "Euphausia", "Thysanoessa", "CalanoidCopepod",
                "CopepodParasite", "PinnotheridCrab", "Nematodes", "Plant",
                "UnidMaterial", "Shrimp", "Crustaceans", "Gammarids", "Pteropods",
                "CephalopodLarva", "SquidLarva", "OctopusLarva", "Polychaetes",
                "Plastic", "Decapods", "Mysids", "Insects", "IsopodParasite", 
                "Corophids", "Caprellids", "Diptera", "Molluscs", "Cyphocaris") ~ "Invertebrate",
    TRUE ~ prey))

#check over groups
unique(stom_summary$group)

#create a a column with a comma separated list of all prey
stom_summary <- stom_summary %>%
  group_by(date, general_location, lavage_bag) %>%
  mutate(all_prey = paste(prey, collapse = ", "))

#get the total mass for each stomach (additional grouping variables are just there 
#so that they are retained in the summary data)
stom_summary <- stom_summary %>%
  group_by(analysis_date, event_type, general_location, 
           year, month, day, date, lavage_bag, group, all_prey, analyst, salmon_sample_type) %>%
  summarise(mass_g_sum = sum(mass_g, na.rm = TRUE))

#pivot wider
stom_summary <- stom_summary %>%
  pivot_wider(names_from = group, values_from = mass_g_sum, names_prefix = "mass_g_")

#check for duplicated stomach bags / stomach bags with data entry inconsistencies
test <- stom_summary %>%
  group_by(date, general_location, lavage_bag) %>%
  summarise(count = n()) %>%
  filter(count > 1 | is.na(count))
nrow(test)

write.csv(stom_summary, paste0("./Core-Fieldwork/Stomach-Contents/Raw-Data/Winter-Ecology/Reformatted/", Sys.Date(), "-psf-winter-A1-stom-stomach-summary.csv"))




#detailed data on fish prey ----------------------------------------------------

stom_fish <- stom_data %>%
  filter(prey %in% c("UnidFish", "FishScales", "Flatfish", "Myctophid",
                     "Herring", "Anchovy", "LarvalFish", "Poacher", "Sandlance"))

#check that this captures all fish categories!!
#here is a list of the remaining prey categories not captured
setdiff(unique(stom_data$prey), unique(stom_fish$prey))


stom_fish <- stom_fish %>%
  rename(fish_mass_g = mass_g,
         fish_species = prey,
         fish_comments = comments,
         fish_code_SI = isotope_code) %>%
  dplyr::select(-prey_we)

write.csv(stom_fish, paste0("./Core-Fieldwork/Stomach-Contents/Raw-Data/Winter-Ecology/Reformatted/", Sys.Date(), "-psf-winter-A1-stom-fish-prey.csv"))

#isotope samples ---------------------------------------------------------------

stom_isotope <- stom_data %>%
  filter(!is.na(isotope_code))

stom_isotope <- stom_isotope %>%
  rename(prey_taxa = prey,
         sample_number = isotope_code) %>%
  mutate(comments = paste0(prey_we, "; ", comments),
         comments = str_replace(comments, "NA; |; NA", ""),
         sample_type = case_when(count > 1 ~ "Whole Organism (Multiple)",
                                 count == 1 ~ "Whole Organism (Single)")) %>%
  dplyr::select(-prey_we, count)

write.csv(stom_isotope, paste0("./Core-Fieldwork/Stomach-Contents/Raw-Data/Winter-Ecology/Reformatted/", Sys.Date(), "-psf-winter-A1-stom-isotope-prey.csv"))
