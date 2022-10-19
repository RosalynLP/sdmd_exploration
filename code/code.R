##########################################################
# Band 6 presentation 
# extraction & analysis
# Written/run on RStudio server 1.1.463 and R 3.6.1
# Author: Rosalyn Pearson
##########################################################


### 1 - Housekeeping ----
#   loading packages
source("code/packages.R") 
#   defining functions
source("code/functions.R") # Remove this line if defining functions here



### 2 Load data ----
drug_demo <- read.csv("data/basefiles/drug_treatment_demographics.csv")
drug_presenting <- read.csv("data/basefiles/drug_treatment_present_council_agesex.csv")
drug_treatment <- read.csv("data/basefiles/drug_treatment_type.csv")

sdmd <- read.csv("data/basefiles/demographics_sdmd_healthboard.csv")
hospital <- read.csv("data/basefiles/open-datadrug_related_hospital_stays_healthboard.csv")


### 3 Analysis ----
mf <- drug_demo %>% 
  dplyr::filter(Measure == "Number",
                SubstanceType == "All") %>% 
  select("Quarter", "SexMale", "SexFemale") %>% 
  dplyr::mutate(Quarter = factor(Quarter, levels=c("Q1 2021/2022", "Q2 2021/2022", "Q3 2021/2022",
                                                   "Q4 2021/2022", "Q1 2022/2023")))

fig <- mf  %>% 
  plot_ly(x = ~Quarter, y = ~SexFemale, type = 'bar', name = 'Female') %>% 
  add_trace(y = ~SexMale, name = 'Male') %>% 
  layout(yaxis = list(title = 'Count'), barmode = 'stack')

fig

df <- drug_demo %>% 
  dplyr::filter(Measure == "Number") %>% 
  #select("Quarter", "SubstanceType",  dplyr::starts_with("Age")) %>% 
  pivot_longer(cols = dplyr::starts_with("Age"), names_to = "Age", values_to = "Number") %>% 
  dplyr::mutate(Quarter = factor(Quarter, levels=c("Q1 2021/2022", "Q2 2021/2022", "Q3 2021/2022",
                                                   "Q4 2021/2022", "Q1 2022/2023"))) %>% 
  dplyr::mutate(Age = case_when(Age == "AgeUnder20" ~ "<20",
                                Age == "Age20_29" ~ "20-29",
                                Age == "Age30_39" ~ "30-39",
                                Age == "Age40_49" ~ "40-49",
                                Age == "Age50_59" ~ "50-59",
                                Age == "Age60Plus" ~ "60+")) %>% 
  dplyr::mutate(Age = factor(Age, levels = c("<20", "20-29", "30-39", "40-49", "50-59", "60+")))

plot1 <- df %>% 
  filter(SubstanceType != "All",
         ServiceType == "Community-based service") %>% 
  ggplot(aes(x=Quarter, y=Number, fill=Age)) +
  geom_bar(stat="identity") +
  facet_grid(SubstanceType~Age) +
  scale_fill_manual(values=as.character(phs_palettes$all))

ggplotly(plot1)


plot2 <- sdmd %>% 
  select(FinancialYear, HBR, AgeGroup, Sex, NumberAssessed) %>% 
  filter(Sex=="All",
         HBR!="S92000003") %>% 
  ggplot(aes(x=FinancialYear, y=NumberAssessed, fill=AgeGroup)) +
  geom_bar(stat="identity") +
  facet_wrap(~HBR)

ggplotly(plot2)


### END OF SCRIPT ###
