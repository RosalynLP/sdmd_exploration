## Data wrangling for shiny app


substrRight <- function(x, n){
  substr(x, 1, n)
}

# Population figures
population <- read.csv("hb2019_pop_est_15072022.csv") %>% 
  # Aggregate age groups
  mutate(`All` = rowSums(select(., starts_with("Age")), na.rm=TRUE),
         `Under 20yrs old` = rowSums(select(., c("Age1", "Age2", "Age3", "Age4", "Age5",
                                                 "Age6", "Age7", "Age8", "Age9", "Age10",
                                                 "Age11", "Age12", "Age13", "Age14", "Age15",
                                                 "Age16", "Age17", "Age18", "Age19")), na.rm=TRUE),
         `20 to 24 yrs old` = rowSums(select(., c("Age20", "Age21", "Age22", "Age23", "Age24")), na.rm=TRUE),
         `25 to 29 yrs old` = rowSums(select(., c("Age25", "Age26", "Age27", "Age28", "Age29")), na.rm=TRUE),
         `30 to 34 yrs old` = rowSums(select(., c("Age30", "Age31", "Age32", "Age33", "Age34")), na.rm=TRUE),
         `35 to 39 yrs old` = rowSums(select(., c("Age35", "Age36", "Age37", "Age38", "Age39")), na.rm=TRUE),
         `40 to 44 yrs old` = rowSums(select(., c("Age40", "Age41", "Age42", "Age43", "Age44")), na.rm=TRUE),
         `45+ yrs old` = rowSums(select(., c("Age45", "Age46", "Age47", "Age48", "Age49",
                                             "Age50", "Age51", "Age52", "Age53", "Age54",
                                             "Age55", "Age56", "Age57", "Age58", "Age59",
                                             "Age60", "Age61", "Age62", "Age63", "Age64",
                                             "Age65", "Age66", "Age67", "Age68", "Age69",
                                             "Age70", "Age71", "Age72", "Age73", "Age74",
                                             "Age75", "Age76", "Age77", "Age78", "Age79",
                                             "Age80", "Age81", "Age82", "Age83", "Age84",
                                             "Age85", "Age86", "Age87", "Age88", "Age89",
                                             "Age90plus")), na.rm=TRUE)) %>% 
  select(-c(starts_with("Age"), "AllAges", "HBQF", "SexQF")) %>% 
  mutate(HBRfull = phsmethods::match_area(as.character(HB))) %>% 
  # Pick closest financial year to match on
  mutate(YearBeginning = Year) %>% 
  pivot_longer(cols = c("Under 20yrs old",
                        "20 to 24 yrs old",
                        "25 to 29 yrs old",
                        "30 to 34 yrs old",
                        "35 to 39 yrs old",
                        "40 to 44 yrs old",
                        "45+ yrs old",
                        "All"),
               names_to = "AgeGroup", values_to = "Population") %>% 
  select(YearBeginning, HBRfull, AgeGroup, Sex, Population)


population_sub <- population %>% 
  filter(Sex == "All", AgeGroup == "All") %>% 
  select(YearBeginning, HBRfull, Population)


# SDMD demographics
sdmd <- read.csv("demographics_sdmd_healthboard.csv") %>% 
  mutate(HBRfull = phsmethods::match_area(as.character(HBR)),
         YearBeginning = substrRight(as.character(FinancialYear), 4),
         AgeGroup = factor(as.character(AgeGroup), levels=c("Under 20yrs old",
                                                            "20 to 24 yrs old",
                                                            "25 to 29 yrs old",
                                                            "30 to 34 yrs old",
                                                            "35 to 39 yrs old",
                                                            "40 to 44 yrs old",
                                                            "45+ yrs old",
                                                            "Unknown",
                                                            "All"))) %>% 
  merge(population) %>% 
  # Calculate rate
  mutate(Rate = 1000*NumberAssessed / Population)

# SDMD substance breakdown
sdmd_drugs <- read.csv("treatment_group_sdmd_healthboard.csv") %>% 
  mutate(HBRfull = phsmethods::match_area(as.character(HBR)),
         YearBeginning = substrRight(as.character(FinancialYear), 4),
         Substance = case_when(as.character(TreatmentGroup) == "All" ~ "All",
                               as.character(TreatmentGroup) == "Reporting any illicit drug" ~ "Any illicit",
                               as.character(TreatmentGroup) == "Illicit drug - Heroin" ~ "Heroin",
                               as.character(TreatmentGroup) == "Illicit drug - Diazepam" ~ "Diazepam",
                               as.character(TreatmentGroup) == "Illicit drug - Cannabis" ~ "Cannabis",
                               as.character(TreatmentGroup) == "Reporting any prescribed drug" ~ "Any prescribed",
                               as.character(TreatmentGroup) == "Prescribed drug - Methadone" ~ "Methadone",
                               as.character(TreatmentGroup) == "Current injectors" ~ "Injectors") ) %>% 
  merge(population_sub) %>% 
  # Calculate rate
  mutate(Rate = 1000*NumberAssessed / Population)



