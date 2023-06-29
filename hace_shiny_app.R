#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#to do: 
#tidy response rates
#add question trends
#check phs guidance
#order all variables
#add Scotland comparison
#add scotland comparison title

library(shiny)
library(tidyverse)


# Define UI for application that draws a histogram
ui_hace <- fluidPage(

    # Application title
  titlePanel("2022 Summary of results"),
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
            tableOutput("summary_table")
             )),
      
      # Create a spot for the question barplot
  h3("Percentage of positive, neutral and negative responses"),
  fluidRow(),
  fluidRow(      
    plotOutput("percent_pnn_plot", height = 400, width = 700,
               click = "percent_pnn_click", )
          ),
  fluidRow(      
    # Define the dropdowns
    column(10,
           selectInput(inputId = "Label",label = "Select a question to compare to Scotland and display a time trend for all surveys since 2014 where questions are comparable.", 
                       choices=unique(rag_chart_pct$question_labels),
                       selected = "10: Overall, how would you rate the care\nprovided by your GP practice?")
          )),
  # header for the comparison to scotland section
  h3("Comparison to Scotland (per cent positive results)"),
    fluidRow(   
      column(5,
             verbatimTextOutput("question_text"),
             verbatimTextOutput("significance_scot"),
             verbatimTextOutput("percent_pnn_click_output"),
            ),
      column(5,
                 plotOutput("Scotland_plot")
                 ),

          ),
# Create a spot for the trend plot
h3("Time trend (per cent positive results)"),
fluidRow(  
  column(5,
         verbatimTextOutput("question_text_trend"),
         verbatimTextOutput("significance_2020"),
         p("Blank values for years 2014 - 2020 in the time trend indicate that the question was not asked in that survey or that the question was not comparable")
  ),
  column(5,
         plotOutput("trend_plot")
  ),
  
)
)


# Define server logic required to draw a histogram

server_hace <- function(input, output, session) {
  # Fill in the spot we created for a plot
output$response_rate <- renderPrint({
    paste0(round(as.numeric(mean(rag_chart_pct$Response_Rate_perc[rag_chart_pct$Report_Area == input$Report_Area])),0),"%")
    }) 
output$response_number <- renderPrint({
    as.numeric(first(rag_chart_pct$N_IncludedResponses[rag_chart_pct$Report_Area == input$Report_Area]))
  }) 
output$forms_number <- renderPrint({
    as.numeric(first(rag_chart_pct$sample_size[rag_chart_pct$Report_Area == input$Report_Area]))
  }) 
  # Fill in the spot we created for a plot
output$percent_pnn_plot <- renderPlot({
    selectedData <- rag_chart_pct%>%
      filter(Report_Area == input$Report_Area & Level == input$Level & year == "2022")
      
    # Render the question barplot
    ggplot(selectedData,  aes(x=question_labels,y=Wgt_Percent,fill = PNN)) +
      labs(title = selectedData$Report_Area) +
      geom_col() + 
      scale_fill_manual(values=c("#0078D4","#AF69A9","#3F3685"))+
      geom_text(data=selectedData, aes(x = question_labels, y = cumulative_percent-Wgt_Percent/2-2,
                                    label = paste0(round(Wgt_Percent,0),"%"),colour = PNN, hjust = 0,size = 14),show.legend = FALSE)+
      annotate(geom = "text", x = selectedData$question_labels, y = -0.5, 
               label = selectedData$N_IncludedResponses, hjust = +1.2, vjust = 0, size = 5) +
      scale_colour_manual(values=c("black","black","white"))+
      coord_flip(clip = 'off', expand = 0)  + 
      theme(legend.position = "top",
            legend.direction = "horizontal",
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_blank(),
            axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x=element_blank(),
            text=element_text(size=16),
            axis.text.y = element_text(margin = margin(r = 70))) +
      guides(fill = guide_legend(reverse = TRUE))
  })
  
output$Scotland_plot <- renderPlot({
    scot_compare <- rag_chart_pct %>%
      filter(PNN == "Positive" & question_labels == input$Label & year == "2022" &
               ((Report_Area == input$Report_Area & Level == input$Level)|
                 (Report_Area == "Scotland" & Level == "Scotland")))
ggplot(scot_compare, aes(x=Report_Area,y=Wgt_Percent,fill = Level)) +
      geom_col() + 
      scale_fill_manual(values=c("#655E9D","#3F3685"),expand=0)+
      geom_text(data=scot_compare, aes(x = Report_Area, y = Wgt_Percent/2,
                                       label = paste0(round(Wgt_Percent,0),"%"),colour = "white",  hjust = +0.5,size = 14),show.legend = FALSE)+
      scale_colour_manual(values="white")+
      coord_cartesian(ylim=c(0,70), expand = FALSE ) +
      theme(panel.background = element_rect(fill='transparent'),
            axis.line.y = element_line("grey"),
            axis.line.x = element_line("grey"),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            text=element_text(size=16))+
      guides(fill="none")
  })
output$question_text <- renderPrint({
    as.character(rag_chart_pct$question_labels[rag_chart_pct$PNN == "Positive" & rag_chart_pct$question_labels == input$Label 
                                               & rag_chart_pct$year == "2022" & rag_chart_pct$Report_Area == input$Report_Area])
  }) 
  
output$significance_scot <- renderPrint({
    as.character(rag_chart_pct$Significance_Scot[rag_chart_pct$PNN == "Positive" & rag_chart_pct$question_labels == input$Label 
                                              & rag_chart_pct$year == "2022" & rag_chart_pct$Report_Area == input$Report_Area])
  }) 
output$trend_plot <- renderPlot({
    trend_data <- rag_chart_pct %>%
      filter(PNN == "Positive" & question_labels == input$Label &
               (Report_Area == input$Report_Area & Level == input$Level))
    
    ggplot(trend_data, aes(x=year,y=Wgt_Percent,fill = Level)) +
      geom_col() + 
      labs(title = trend_data$Report_Area) +
      scale_fill_manual(values=c("#3F3685","#3F3685"))+
      geom_text(data=trend_data, aes(x = year, y = Wgt_Percent/2,
                                     label = paste0(round(Wgt_Percent,0),"%"),colour = "white", hjust = +0.5,size = 14),show.legend = FALSE)+
      scale_colour_manual(values="white")+
      theme(axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_blank(),
            axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            text=element_text(size=16))+
      guides(fill="none")
  })
output$question_text_trend <- renderPrint({
  as.character(rag_chart_pct$question_labels[rag_chart_pct$PNN == "Positive" & rag_chart_pct$question_labels == input$Label 
                                             & rag_chart_pct$year == "2022" & rag_chart_pct$Report_Area == input$Report_Area])
}) 
output$significance_2020 <- renderPrint({
  as.character(rag_chart_pct$Significance_2020[rag_chart_pct$PNN == "Positive" & rag_chart_pct$question_labels == input$Label 
                                               & rag_chart_pct$year == "2022" & rag_chart_pct$Report_Area == input$Report_Area])
  
output$summary_table <- renderTable({
  summary_data <- data.frame(
    "indicator" = c("Response rate", "Number of responses","Number of forms sent out"),
    "value" = c(paste0(round(as.numeric(mean(rag_chart_pct$Response_Rate_perc[rag_chart_pct$Report_Area == input$Report_Area])),0),"%"),
                as.numeric(first(rag_chart_pct$N_IncludedResponses[rag_chart_pct$Report_Area == input$Report_Area])),
                as.numeric(first(rag_chart_pct$sample_size[rag_chart_pct$Report_Area == input$Report_Area])))
                            )
    tibble::tibble(summary_data)
    })
}) 
observe({
    updateSelectInput(session, "Report_Area", choices = as.character(rag_chart_pct$Report_Area[rag_chart_pct$Level == input$Level]))
  })
}
# Run the application 
shinyApp(ui = ui_hace, server = server_hace)
