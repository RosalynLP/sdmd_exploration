#
# Shiny app for my B6 interview
# Exploring Scottish drug misuse data
#

## Load libraries ----
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(tidyverse)
library(bslib)
library(glue)
library(phsverse)

# Wrangle data ----
source("data_wrangling.R")

# UI ----
ui <- fluidPage(
  
  theme = bs_theme(version = 4, bootswatch = "minty"),
   
   # Title panel ----
   titlePanel("Exploring the Scottish Drug Misuse Database"),
   
   # Sidebar with a slider input for number of bins 
   shinydashboard::tabBox( width=NULL, height="1700px", side="right",
         ## Notes ----
         tabPanel("Notes",
                  tagList(
                    br(),
                    p(strong("Use the tabs above to navigate around the app.")),
                    h4("Notes"),
                    br(),
                    tags$b(),
                    # Data sources
                    h5("Data sources"),
                    tags$li("All data are sourced from NHS Scottish open data"),
                    tags$li(tags$a(
                      href = "https://www.opendata.nhs.scot/dataset/population-estimates/resource/27a72cc8-d6d8-430c-8b4f-3109a9ceadb1",
                      "Population data for calculating rates",
                      class = "externallink")),
                    tags$li(tags$a(
                      href = "https://www.opendata.nhs.scot/dataset/scottish-drug-misuse-database/resource/e096573f-b828-4e8d-abf2-84f94345a751",
                      "Demographic SDMD data",
                      class = "externallink")),
                    tags$li(tags$a(
                      href = "https://www.opendata.nhs.scot/dataset/scottish-drug-misuse-database/resource/aebb18ee-40c3-4520-9521-d0800e749567",
                      "Treatment group SDMD data",
                      class = "externallink")),
                    br(),
                    h5("Source code"),
                    tags$li("The source code for this app can be found in ", 
                            tags$a(href="https://github.com/RosalynLP/sdmd_exploration", 
                                   "this Github repository", class="externallink")),
                    br(),
                    h5("Scottish Drug Misuse Database (SDMD)"),
                    tags$li("The SDMD was set up in 1990"),
                    tags$li("Data are collected when an individual makes contact with 
                            structured community or residential treatment"),
                    tags$li("Data are binned by financial year (Apr-Apr)"),
                    tags$li("For 2013/14 there were issues with data collection so data for some
       boards and aggregated data are not present"),
                    tags$li("Breakdown by NHS Health Board was considered the most robust geographical
       breakdown, so that has been chosen for this app"),
                    tags$li("Orkney 2021 data are missing"),
                    tags$li("For more information see the ", 
                            tags$a(href="https://publichealthscotland.scot/media/13552/2022-05-17-sdmd-report.pdf",
                                   "Public Health Scotland SDMD report", class="externallink")),
                    br(),
                    h5("Demographics"),
                    tags$li("'Number' is the number of individuals assessed for treatment"),
                    tags$li("'Percentage' is the percentage of individuals assessed for treatment, where
       the denominator is the number of individuals assessed within that NHS Health Board"),
                    tags$li("'Rate' is the number of individuals assessed for treatment per 1,000 members of
       the population. Population estimates are broken down by age group, sex, NHS 
       Health Board and year. Note that there is a three month difference between the 
       population estimate bins (Jan-Jan) and the SDMD data bins (Apr-Apr). However, 
       population change is slow moving so this should not have a significant effect."),
                    br(),
                    h5("Substance"),
                    tags$li("Substance is the illicit substance which individuals are referred
                            to treatment for. Note that each individual can be treated for 
                            more than one substance."),
                    tags$li("'Number' is the number of individuals reporting taking that substance on assessment"),
                    tags$li("'Percentage' is the percentage of individuals referred to treatment for 
       that substance, where the denominator is the number of individuals assessed for treatment
       within that NHS Health Board"),
                    tags$li("'Rate' is the number of individuals assessed for treatment per 1,000 members of
       the population. Population estimates are broken down by age group, sex, NHS 
       Health Board and year. Note that there is a three month difference between the 
       population estimate bins (Jan-Jan) and the SDMD data bins (Apr-Apr). However, 
       population change is slow moving so this should not have a significant effect.")
                    
                    ) # tagList
   ), # tabPanel                     
      ## Demographics ----                     
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
                     "Select whether to use absolute number, percentage or population rate:",
                     choices = c("Number", "Percentage", "Rate"),
                     selected = "Rate"),
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
         h4("Individuals assessed for treatment"),
         plotlyOutput("mainPlot")
      ),
      ## Substance treated for ----
      tabPanel("Substance",
         tagList(sliderInput("years2",
                             "Select financial year(s) beginning:",
                             min = 2006,
                             max = 2020,
                             sep = "",
                             value = c(2006, 2020)),
                 pickerInput("numberorpc2",
                             "Select whether to use absolute number, percentage or population rate:",
                             choices = c("Number", "Percentage", "Rate"),
                             selected = "Rate"),
                 pickerInput("hbs2",
                             "Select up to 3 health boards:",
                             choices = unique(sdmd_drugs$HBRfull),
                             selected = c("Scotland"),
                             multiple = TRUE, 
                             options = pickerOptions(
                               liveSearch=TRUE,
                               maxOptions = 3,
                               maxOptionsText="Choose up to three options")),
                 h4("Individuals assessed for treatment by treatment substance"),
                 plotlyOutput("drugPlot")
         ) # tagList
            ) # tabPanel
     
        ) #tabBox
      
) #fluidpage

## Server ----
server <- function(input, output) {
   
   ## Demographics plot ----
   output$mainPlot <- renderPlotly({
     
     # Checking that some health boards have been chosen
     validate(need(length(input$hbs)>0, "Please select some health boards"))
     
     if(length(seq(input$years[1], input$years[2])) > 8) {
       yeargap = "5 years"
     } else if (length(seq(input$years[1], input$years[2])) > 5) {
       yeargap = "3 years"
     } else {
       yeargap = "1 year"
     }

     p <- sdmd %>% filter(YearBeginning %in% seq(input$years[1], input$years[2]),
                          HBRfull %in% input$hbs) %>% 
       mutate(YearBeginning = as.Date(paste0(YearBeginning, "-01-01")))
     
     if (input$ageorsex == "Age"){
       tooltip_list <- list("Age group", "text")
       p <- p %>% filter(Sex == "All",
                         !(`Age group` %in% c("Unknown", "All")))
     } else {
       tooltip_list <- list("Sex", "text")
       p <- p %>% filter(`Age group` == "All",
                        Sex != "All")
     }
     
     if (input$numberorpc == "Number"){
       ylabel <- "Number assessed"
       p <- p %>% 
         ggplot(aes(x=YearBeginning, y=NumberAssessed, 
                    text = paste0("Financial year: ", FinancialYear, "\n",
                                  "Health Board: ", HBRfull, "\n",
                                  "Number assessed: ", format(NumberAssessed, big.mark=',')
                                 )
                    )) 
     } else if (input$numberorpc == "Percentage"){
       ylabel <- "Percentage assessed"
       p <- p %>% 
         ggplot(aes(x=YearBeginning, y=PercentAssessed,
                    text = paste0("Financial year: ", FinancialYear, "\n",
                                  "Health Board: ", HBRfull, "\n",
                                  "Percentage assessed: ", paste0(round(PercentAssessed, 2), '%')
                    )
         ))
     } else {
       ylabel <- "Rate assessed per 1,000 population"
       p <- p %>% 
         ggplot(aes(x=YearBeginning, y=Rate,
                    text = paste0("Financial year: ", FinancialYear, "\n",
                                  "Health Board: ", HBRfull, "\n",
                                  "No. assessed per 1,000 population: ", round(Rate, 2)
                    )
         )) 
     }
     if(input$ageorsex == "Age"){
       p <- p + 
         geom_bar(stat="identity", aes(fill=`Age group`)) +
         facet_grid(HBRfull~`Age group`, switch="y", labeller = labeller(HBRfull = label_wrap_gen(width = 16)))
     } else {
       p <- p +
         geom_bar(stat="identity", aes(fill=Sex)) +
         facet_grid(HBRfull~Sex, switch="y", labeller = labeller(HBRfull = label_wrap_gen(width = 16)))
     }
     
     p <- p + scale_fill_manual(values=as.character(phs_palettes$all)) +
       scale_x_date(breaks=yeargap, date_labels="%Y") +
       scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
       xlab("") +
       ylab(ylabel) +
       theme(legend.position = "none",
             text = element_text(size=12),
             axis.title = element_text(colour=phs_colours("phs-purple"), size=12),
             panel.background = element_blank(),
             axis.line.x = element_line(colour="black"),
             axis.line.y = element_line(colour="black"),
             strip.text.y = element_text(angle = 0, color=phs_colours("phs-purple"), face="bold", size=12),
             strip.text.x = element_text(angle = 0, color=phs_colours("phs-purple"), face="bold", size=12),
             strip.background = element_blank(),
             panel.grid.major.y = element_line(colour = "light grey")) 
     
     ggplotly(p, tooltip = tooltip_list) %>% 
       layout(margin = list(b = 80, t = 50, r=150, l=100))
     
   })
   
   ## Substance plot ----
   output$drugPlot <- renderPlotly({
     
     # Checking that some health boards have been chosen
     validate(need(length(input$hbs2)>0, "Please select some health boards"))
     
     if(length(seq(input$years2[1], input$years2[2])) > 8) {
       yeargap2 = "5 years"
     } else if (length(seq(input$years2[1], input$years2[2])) > 5) {
       yeargap2 = "3 years"
     } else {
       yeargap2 = "1 year"
     }
     
     p <- sdmd_drugs %>% filter(YearBeginning %in% seq(input$years2[1], input$years2[2]),
                          HBRfull %in% input$hbs2) %>% 
       mutate(YearBeginning = as.Date(paste0(YearBeginning, "-01-01"))) %>% 
       filter(Substance %in% c("Any illicit", "Heroin", "Diazepam", "Cannabis"))
     
     if (input$numberorpc2 == "Number"){
       ylabel <- "Number treated"
       p <- p %>% 
         ggplot(aes(x=YearBeginning, y=NumberAssessed,
                    text = paste0("Financial year: ", FinancialYear, "\n",
                                  "Health Board: ", HBRfull, "\n",
                                  "Number treated: ", format(NumberAssessed, big.mark=',')
                    )
         )) 
     } else if (input$numberorpc2 == "Percentage"){
       ylabel <- "Percentage treated"
       p <- p %>% 
         ggplot(aes(x=YearBeginning, y=PercentAssessed,
                    text = paste0("Financial year: ", FinancialYear, "\n",
                                  "Health Board: ", HBRfull, "\n",
                                  "Percentage treated: ", paste0(round(PercentAssessed, 2), '%')
                    )
         ))
     } else {
       ylabel <- "Rate treated per 1,000 population"
       p <- p %>% 
         ggplot(aes(x=YearBeginning, y=Rate,
                    text = paste0("Financial year: ", FinancialYear, "\n",
                                  "Health Board: ", HBRfull, "\n",
                                  "No. treated per 1,000 population: ", round(Rate, 2)
                    )
         )) 
     }
     
     p <- p +
       geom_bar(stat="identity", aes(fill=Substance)) +
       facet_grid(HBRfull~Substance, switch="y", labeller = labeller(HBRfull = label_wrap_gen(width = 16)))
     
     p <- p + scale_fill_manual(values=as.character(phs_palettes$all)) +
       scale_x_date(breaks=yeargap2, date_labels="%Y") +
       scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
       xlab("") +
       ylab(ylabel) +
       theme(legend.position = "none",
             text = element_text(size=12),
             axis.title = element_text(colour=phs_colours("phs-purple"), size=12),
             panel.background = element_blank(),
             axis.line.x = element_line(colour="black"),
             axis.line.y = element_line(colour="black"),
             strip.text.y = element_text(angle = 0, color=phs_colours("phs-purple"), face="bold", size=12),
             strip.text.x = element_text(angle = 0, color=phs_colours("phs-purple"), face="bold", size=12),
             strip.background = element_blank(),
             panel.grid.major.y = element_line(colour = "light grey")) 
     
     ggplotly(p, tooltip = list("Substance", "text")) %>% 
       layout(margin = list(b = 80, t = 50, r=150, l=100))
     
   })
   

   
}

# Run the application ----
shinyApp(ui = ui, server = server)

## END OF APP ----