#
# Shiny app for my B6 interview
# Exploring Scottish drug misuse data
#
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(tidyverse)
library(bslib)

substrRight <- function(x, n){
  substr(x, 1, n)
}

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
                                              "All")))

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
                               as.character(TreatmentGroup) == "Current injectors" ~ "Injectors") )

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = bs_theme(version = 4, bootswatch = "minty"),
   
   # Application title
   titlePanel("Exploring the Scottish Drug Misuse Database"),
   
   # Sidebar with a slider input for number of bins 
   shinydashboard::tabBox( width=NULL, height="1700px", side="right",
      tabPanel("Demographics",
       tagList(
         sliderInput("years",
                     "Select financial year(s) beginning:",
                     min = 2006,
                     max = 2020,
                     sep = "",
                     value = c(2006, 2020)),
         pickerInput("ageorsex",
                     "Select breakdown by age or sex:",
                     choices = c("Age", "Sex"),
                     selected = "Age"),
         pickerInput("numberorpc",
                     "Select whether to use absolute number or percentage:",
                     choices = c("Number", "Percentage"),
                     selected = "Number"),
         pickerInput("hbs",
                     "Select up to 3 health boards:",
                     choices = unique(sdmd$HBRfull),
                     selected = c("Scotland"),
                     multiple = TRUE, 
                     options = pickerOptions(
                       liveSearch=TRUE,
                       maxOptions = 3,
                       maxOptionsText="Choose up to three options")
                     )
                        ),
         plotlyOutput("mainPlot")
      ),
      tabPanel("Substance",
               tagList(sliderInput("years2",
                                   "Select financial year(s) beginning:",
                                   min = 2006,
                                   max = 2020,
                                   sep = "",
                                   value = c(2006, 2020)),
                       pickerInput("numberorpc2",
                                   "Select whether to use absolute number or percentage:",
                                   choices = c("Number", "Percentage"),
                                   selected = "Number"),
                       pickerInput("hbs2",
                                   "Select up to 3 health boards:",
                                   choices = unique(sdmd_drugs$HBRfull),
                                   selected = c("Scotland"),
                                   multiple = TRUE, 
                                   options = pickerOptions(
                                     liveSearch=TRUE,
                                     maxOptions = 3,
                                     maxOptionsText="Choose up to three options")),
                       plotlyOutput("drugPlot")
               ) # tagList
            ) # tabPanel
        ) #tabBox
      
) #fluidpage

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   ## Demographics plot
   output$mainPlot <- renderPlotly({
     
     # Checking that some health boards have been chosen
     validate(need(length(input$hbs)>0, "Please select some health boards"))

     p <- sdmd %>% filter(YearBeginning %in% seq(input$years[1], input$years[2]),
                          HBRfull %in% input$hbs) %>% 
       mutate(YearBeginning = as.Date(paste0(YearBeginning, "-01-01")))
     
     if (input$ageorsex == "Age"){
       p <- p %>% filter(Sex == "All",
                         !(AgeGroup %in% c("Unknown", "All")))
     } else {
       p <- p %>% filter(AgeGroup == "All",
                        Sex != "All")
     }
     
     if (input$numberorpc == "Number"){
       ylabel <- "Number assessed"
       p <- p %>% 
         ggplot(aes(x=YearBeginning, y=NumberAssessed)) 
     } else {
       ylabel <- "Percentage assessed"
       p <- p %>% 
         ggplot(aes(x=YearBeginning, y=PercentAssessed)) 
     }
     if(input$ageorsex == "Age"){
       p <- p + 
         geom_bar(stat="identity", aes(fill=AgeGroup)) +
         facet_grid(HBRfull~AgeGroup, switch="y", labeller = labeller(HBRfull = label_wrap_gen(width = 16)))
     } else {
       p <- p +
         geom_bar(stat="identity", aes(fill=Sex)) +
         facet_grid(HBRfull~Sex, switch="y", labeller = labeller(HBRfull = label_wrap_gen(width = 16)))
     }
     
     p <- p + scale_fill_manual(values=as.character(phs_palettes$all)) +
       scale_x_date(breaks="3 years", date_labels="%Y") +
       xlab("") +
       ylab(ylabel) +
       theme(legend.position = "none",
             text = element_text(size=12),
             axis.title = element_text(colour=phs_colours("phs-purple"), size=14),
             panel.background = element_blank(),
             axis.line.x = element_line(colour="black"),
             axis.line.y = element_line(colour="black"),
             strip.text.y = element_text(angle = 0, color=phs_colours("phs-purple"), face="bold", size=14),
             strip.text.x = element_text(angle = 0, color=phs_colours("phs-purple"), face="bold", size=14),
             strip.background = element_blank(),
             panel.grid.major.y = element_line(colour = "light grey")) 
     
     ggplotly(p) %>% 
       layout(margin = list(b = 80, t = 50, r=150, l=50))
     
   })
   
   ## Drug plot
   output$drugPlot <- renderPlotly({
     
     # Checking that some health boards have been chosen
     validate(need(length(input$hbs2)>0, "Please select some health boards"))
     
     p <- sdmd_drugs %>% filter(YearBeginning %in% seq(input$years2[1], input$years2[2]),
                          HBRfull %in% input$hbs2) %>% 
       mutate(YearBeginning = as.Date(paste0(YearBeginning, "-01-01"))) %>% 
       filter(Substance %in% c("Any illicit", "Heroin", "Diazepam", "Cannabis"))
     
     if (input$numberorpc2 == "Number"){
       ylabel <- "Number assessed"
       p <- p %>% 
         ggplot(aes(x=YearBeginning, y=NumberAssessed)) 
     } else {
       ylabel <- "Percentage assessed"
       p <- p %>% 
         ggplot(aes(x=YearBeginning, y=PercentAssessed)) 
     }
     
     p <- p +
       geom_bar(stat="identity", aes(fill=Substance)) +
       facet_grid(HBRfull~Substance, switch="y", labeller = labeller(HBRfull = label_wrap_gen(width = 16)))
     
     p <- p + scale_fill_manual(values=as.character(phs_palettes$all)) +
       scale_x_date(breaks="3 years", date_labels="%Y") +
       xlab("") +
       ylab(ylabel) +
       theme(legend.position = "none",
             text = element_text(size=12),
             axis.title = element_text(colour=phs_colours("phs-purple"), size=14),
             panel.background = element_blank(),
             axis.line.x = element_line(colour="black"),
             axis.line.y = element_line(colour="black"),
             strip.text.y = element_text(angle = 0, color=phs_colours("phs-purple"), face="bold", size=14),
             strip.text.x = element_text(angle = 0, color=phs_colours("phs-purple"), face="bold", size=14),
             strip.background = element_blank(),
             panel.grid.major.y = element_line(colour = "light grey")) 
     
     ggplotly(p) %>% 
       layout(margin = list(b = 80, t = 50, r=150, l=50))
     
   })
   

   
}

# Run the application 
shinyApp(ui = ui, server = server)

