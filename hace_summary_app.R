#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Name of file: hace_shiny_app
# Original author: Catriona Haddow
#   
# Written/run on Posit Workbench - RStudio R4.1.2
# 
# This app creates the summary dashboard of the HACE Survey 2022
# 
# Approximate run time: < 1 minute
# Approximate memory usage: 342 MiB

#to do: 
#check phs guidance
#order all variables
#maintain comparator when scotland is selected?
#explore use of plotly to allow selection of question?
#add more levels
#add tabs for additional dashboards

library(shiny)
library(tidyverse)


# Define UI for application that draws a histogram
ui_hace <- fluidPage(

    # Application title
  titlePanel("2022 - Summary of results"),
  p("This dashboard presents a high level summary for some of the overarching questions from the survey. At Scotland, 
    NHS Board and Health and Social Care Partnership level the results are displayed for headline questions from each 
    section of the survey. At GP Practice and GP Cluster level, the results are displayed for six key questions."),
  hr(),

    # Generate the layout
    fluidRow(      
      
      # Define the dropdowns
      column(4,
        selectInput(inputId = "Level",label = "Select report level:", 
                    choices=unique(rag_chart_pct$Level),
                    selected = "Scotland"),
        selectInput(inputId = "Report_Area",label = "Select a specific report:", 
                    choices=unique(rag_chart_pct$Report_Area),
                    selected = "Scotland")
            ),
      # Include the summary table
      column(3,
            strong("Summary statistics for this specific report:"),
            br(),
            br(),
            tableOutput("summary_table")
             )),
      
      # Create a spot for the question barplot
  h3("Percentage of positive, neutral and negative responses"),
  fluidRow(),
  fluidRow(column(8,
          plotOutput("percent_pnn_plot"))),     
   
  br(),
  fluidRow(      
    # Define the question dropdown
    column(12,
           strong("Select a question to compare to Scotland and display a time trend for all surveys since 2014 where questions are comparable:"),
           selectInput(inputId = "Label",label = "", 
                       choices=unique(rag_chart_pct$question_labels),
                       selected = "10: Overall, how would you rate the care provided by your GP practice?")
          )),
  # header for the comparison section
  h3(textOutput("question_text")),
    fluidRow(   
          column(2,
                 span(textOutput("header_Scotland"),style = "color:blue; font-size:14px; font-family:arial; font-style:bold"),
                 br(),
                 textOutput("significance_scot"),
                ),
          column(2,
                 plotOutput("Scotland_plot")
                ),
          column(2,
               span(textOutput("header_trend"),style = "color:blue; font-size:14px; font-family:arial; font-style:bold"),
               br(),
               textOutput("significance_2020"),
               br(),
               p("Blank values for years 2014 - 2020 in the time trend indicate that the question was not asked in that survey or that the question was not comparable")
                ),
            column(2,
               plotOutput("trend_plot")
               )
          ))



# Define server logic####

server_hace <- function(input, output, session) {

#create response rate table####
output$summary_table <- renderTable({
  summary_data <- data.frame(
    "indicator" = c("Response rate", "Number of responses","Number of forms sent out"),
    "value" = c(paste0(round(as.numeric(mean(rag_chart_pct$Response_Rate_perc[rag_chart_pct$Report_Area == input$Report_Area])),0),"%"),
                format(first(rag_chart_pct$N_IncludedResponses[rag_chart_pct$Report_Area == input$Report_Area]),nsmall=1, big.mark=","),
                format(first(rag_chart_pct$sample_size[rag_chart_pct$Report_Area == input$Report_Area]),nsmall=0, big.mark=","))
  )
  tibble::tibble(summary_data)
}, colnames = FALSE,)
  
#create question chart####
output$percent_pnn_plot <- renderPlot({
    selectedData <- rag_chart_pct%>%
      filter(Report_Area == input$Report_Area & Level == input$Level & year == "2022")
      
    # Render the question barplot
    ggplot(selectedData,  aes(x=question_labels,y=Wgt_Percent,fill = PNN)) +
      labs(title = selectedData$Report_Area,
           caption = "The number of responses is given in blue") +
      geom_col() + 
      scale_fill_manual(values=c("#0078D4","#AF69A9","#3F3685"))+
      geom_text(data=selectedData, aes(x = question_labels, y = cumulative_percent-Wgt_Percent/2-2,
                                    label = paste0(round(Wgt_Percent,0),"%"),colour = PNN, hjust = 0,size = 18),show.legend = FALSE)+
      annotate(geom = "text", x = selectedData$question_labels, y = -0.5, 
               label = format(selectedData$N_IncludedResponses, nsmall=1, big.mark=","), hjust = +1.2, vjust = 0, size = 5,colour = "#0078D4") +
      scale_colour_manual(values=c("black","black","white"))+
      coord_flip(clip = 'off', expand = 0)  + 
      theme(legend.position = "top",
            legend.direction = "horizontal",
            legend.spacing.x = unit(1.0, 'cm'),
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_blank(),
            axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x=element_blank(),
            text=element_text(size=18),
            axis.text.y = element_text(margin = margin(r = 70)),
            plot.caption = element_text(color = "#0078D4"),
            plot.title.position = "plot",   
            plot.title = element_text(hjust = 0.8))+
      guides(fill = guide_legend(reverse = TRUE))
  })


#define reactive text####
output$header_Scotland <- renderText({
  paste0("Comparison to Scotland (per cent positive results): ",input$Report_Area)
}) 
output$question_text <- renderText({
    input$Label
  }) 
  
output$significance_scot <- renderText({
  paste0(input$Report_Area,": ",as.character(rag_chart_pct$Significance_Scot[rag_chart_pct$PNN == "Positive" & rag_chart_pct$question_labels == input$Label 
                                              & rag_chart_pct$year == "2022" & rag_chart_pct$Report_Area == input$Report_Area]))
  }) 

output$header_trend <- renderText({
  paste0("Time trend (per cent positive results): ",input$Report_Area)
}) 

output$significance_2020 <- renderText({
  paste0(input$Report_Area,": ",as.character(rag_chart_pct$Significance_2020[rag_chart_pct$PNN == "Positive" & rag_chart_pct$question_labels == input$Label 
                                               & rag_chart_pct$year == "2022" & rag_chart_pct$Report_Area == input$Report_Area]))
}) 

output$question_text_trend <- renderText({
  input$Label
}) 

#create comparison to Scotland chart####
output$Scotland_plot <- renderPlot({
  scot_compare <- rag_chart_pct %>%
    filter(PNN == "Positive" & question_labels == input$Label & year == "2022" &
             ((Report_Area == input$Report_Area & Level == input$Level)|
                (Report_Area == "Scotland" & Level == "Scotland")))
  ggplot(scot_compare, aes(x=Report_Area,y=Wgt_Percent,fill = Level)) +
    geom_col(width = 0.5) + 
    scale_fill_manual(values=c("#655E9D","#3F3685"),expand=0)+
    labs(title = scot_compare$year) +
    geom_text(data=scot_compare, aes(x = Report_Area, y = Wgt_Percent/2,
                                     label = paste0(round(Wgt_Percent,0),"%"),colour = "white",  hjust = +0.5,size = 14),show.legend = FALSE)+
    scale_colour_manual(values="white") +
    theme(panel.background = element_rect(fill='transparent'),
          axis.line.y = element_line("grey"),
          axis.line.x = element_line("grey"),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          text=element_text(size=16),
          plot.title.position = "plot",   
          plot.title = element_text(hjust = 0.5))+
    guides(fill="none")
})


#create trend chart####
output$trend_plot <- renderPlot({
    trend_data <- rag_chart_pct %>%
      filter(PNN == "Positive" & question_labels == input$Label &
               (Report_Area == input$Report_Area & Level == input$Level))
    
    ggplot(trend_data, aes(x=year,y=Wgt_Percent,fill = Level)) +
      geom_col(width = 0.9) + 
      labs(title = trend_data$Report_Area) +
      scale_fill_manual(values=c("#3F3685","#3F3685"))+
      geom_text(data=trend_data, aes(x = year, y = Wgt_Percent/2,
                                     label = paste0(round(Wgt_Percent,0),"%"),colour = "white", hjust = +0.5,size = 14),show.legend = FALSE)+
      scale_colour_manual(values="white")+
      theme(panel.background = element_rect(fill='transparent'),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_blank(),
            axis.line.y = element_line("grey"),
            axis.line.x = element_line("grey"),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            text=element_text(size=16),
            plot.title.position = "plot",   
            plot.title = element_text(hjust = 0.5))+
      guides(fill="none")
  })


#update report area dropdown boxes to reflect report level selection####
observe({
    updateSelectInput(session, "Report_Area", choices = as.character(rag_chart_pct$Report_Area[rag_chart_pct$Level == input$Level]))
  })
}
# Run the application 
shinyApp(ui = ui_hace, server = server_hace)
