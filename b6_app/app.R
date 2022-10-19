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
library(glue)


# Wrangle data
source("data_wrangling.R")

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
         h4("People assessed in each financial year starting"),
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
                       h4("People using a given drug on assessment in each financial year starting"),
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
     
     if(length(seq(input$years[1], input$years[2])) > 6) {
       yeargap = "3 years"
     } else {
       yeargap = "1 year"
     }

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
     } else if (input$numberorpc == "Percentage"){
       ylabel <- "Percentage assessed"
       p <- p %>% 
         ggplot(aes(x=YearBeginning, y=PercentAssessed)) 
     } else {
       ylabel <- "Rate assessed per 1000 population"
       p <- p %>% 
         ggplot(aes(x=YearBeginning, y=Rate))
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
       scale_x_date(breaks=yeargap, date_labels="%Y") +
       scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
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
       layout(margin = list(b = 80, t = 50, r=150, l=100))
     
   })
   
   ## Drug plot
   output$drugPlot <- renderPlotly({
     
     # Checking that some health boards have been chosen
     validate(need(length(input$hbs2)>0, "Please select some health boards"))
     
     if(length(seq(input$years2[1], input$years2[2])) > 6) {
       yeargap2 = "3 years"
     } else {
       yeargap2 = "1 year"
     }
     
     p <- sdmd_drugs %>% filter(YearBeginning %in% seq(input$years2[1], input$years2[2]),
                          HBRfull %in% input$hbs2) %>% 
       mutate(YearBeginning = as.Date(paste0(YearBeginning, "-01-01"))) %>% 
       filter(Substance %in% c("Any illicit", "Heroin", "Diazepam", "Cannabis"))
     
     if (input$numberorpc2 == "Number"){
       ylabel <- "Number assessed"
       p <- p %>% 
         ggplot(aes(x=YearBeginning, y=NumberAssessed)) 
     } else if (input$numberorpc2 == "Percentage"){
       ylabel <- "Percentage assessed"
       p <- p %>% 
         ggplot(aes(x=YearBeginning, y=PercentAssessed)) 
     } else {
       ylabel <- "Rate assessed per 1000 population"
       p <- p %>% 
         ggplot(aes(x=YearBeginning, y=Rate))
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
             axis.title = element_text(colour=phs_colours("phs-purple"), size=14),
             panel.background = element_blank(),
             axis.line.x = element_line(colour="black"),
             axis.line.y = element_line(colour="black"),
             strip.text.y = element_text(angle = 0, color=phs_colours("phs-purple"), face="bold", size=14),
             strip.text.x = element_text(angle = 0, color=phs_colours("phs-purple"), face="bold", size=14),
             strip.background = element_blank(),
             panel.grid.major.y = element_line(colour = "light grey")) 
     
     ggplotly(p) %>% 
       layout(margin = list(b = 80, t = 50, r=150, l=100))
     
   })
   

   
}

# Run the application 
shinyApp(ui = ui, server = server)

