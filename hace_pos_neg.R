# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Name of file: hace_shiny_app_
# Original author: Catriona Haddow
#   
# Written/run on Posit Workbench - RStudio R4.1.2
# 
# This app creates the most positive and negative dashboard of the HACE Survey 2022
# 
# Approximate run time: < 1 minute
# Approximate memory usage: 342 MiB

#to do: 

library(shiny)
library(tidyverse)


# Define UI for application that draws a histogram
ui_hace <- fluidPage(
  
  # Application title
  titlePanel("2022 - Most positive and negative experience ratings"),
  p("This dashboard presents the questions in the 2022 survey which received the five most positive and the five most negative experience ratings for selected report areas.
  For each question, responses have been categories as positive, neutral or negative. For further information, please refer to the technical report."),
  br(),
  p("The survey sections included in this dashboard are: The GP Practice; Treatemnt or Advice from the GP Practice; Out of Hours Healthcare; Care, Support and Help with Everyday Living; Caring Responsibilities."),
  br(),
  p("At Scotland, NHS Board and Health and Social Care Partnership level, results are available for all sections."),
  br(),
  p("At GP Practice and GP Cluster level, results are available for questions: The GP Practice; Treatment or Advice from the GP Practice."),
  hr(),
  
  # Generate the layout
  fluidRow(      
    
    # Define the dropdowns
    column(4,
           selectInput(inputId = "Level",label = "Select report level:", 
                       choices=unique(Pos_neg$Level),
                       selected = "Scotland"),
           selectInput(inputId = "Report_Area",label = "Select a specific report:", 
                       choices=unique(Pos_neg$Report_Area),
                       selected = "Scotland")
    ),
    # Include the summary table
    column(3,
           strong("Summary statistics for this specific report:"),
           br(),
           br(),
           tableOutput("summary_table")
    )),
  
  # Create a spot for the most positive barplot
  h3("Most positive results"),
  fluidRow(),
  fluidRow(column(8,
                  plotOutput("most_pos_plot"))),
  
  # Create a spot for the most positive barplot
  h3("Most negative results"),
  fluidRow(),
  fluidRow(column(8,
                  plotOutput("most_neg_plot"))),
)


# Define server logic####

server_hace <- function(input, output, session) {
  
  #create response rate table####
  output$summary_table <- renderTable({
    summary_data <- data.frame(
      "indicator" = c("Response rate", "Number of responses","Number of forms sent out"),
      "value" = c(paste0(round(as.numeric(mean(Pos_neg$Response_Rate_perc[Pos_neg$Report_Area == input$Report_Area])),0),"%"),
                  format(first(Pos_neg$N_IncludedResponses[Pos_neg$Report_Area == input$Report_Area]),nsmall=1, big.mark=","),
                  format(first(Pos_neg$sample_size[Pos_neg$Report_Area == input$Report_Area]),nsmall=0, big.mark=","))
    )
    tibble::tibble(summary_data)
  }, colnames = FALSE,)
  
  #create most positive chart####
  output$most_pos_plot <- renderPlot({
    selected_data <- Pos_neg %>% 
      filter(Report_Area == input$Report_Area & Level == input$Level & year == "2022" & positive_rank<= 5)
    
    ggplot(selected_data,  aes(x=question_labels,y=Wgt_Percent,fill = PNN)) +
      labs(title = selected_data$Report_Area,
           caption = "The number of responses is given in blue") +
      geom_col() + 
      scale_fill_manual(values=c("#0078D4","#AF69A9","#3F3685"))+
      geom_text(data=selected_data, aes(x = question_labels, y = cumulative_percent-Wgt_Percent/2-2,
                                        label = paste0(round(Wgt_Percent,0),"%"),colour = PNN, hjust = 0,size = 18),show.legend = FALSE)+
      annotate(geom = "text", x = selected_data$question_labels, y = -0.5, 
               label = format(selected_data$N_IncludedResponses, nsmall=1, big.mark=","), hjust = +1.2, vjust = 0, size = 5,colour = "#0078D4") +
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
  #create most negative chart####
  output$most_neg_plot <- renderPlot({
    selected_data <- Pos_neg %>% 
      filter(Report_Area == input$Report_Area & Level == input$Level & year == "2022" & negative_rank<= 5)
    
    ggplot(selected_data,  aes(x=question_labels,y=Wgt_Percent,fill = PNN)) +
      labs(title = selected_data$Report_Area,
           caption = "The number of responses is given in blue") +
      geom_col() + 
      scale_fill_manual(values=c("#0078D4","#AF69A9","#3F3685"))+
      geom_text(data=selected_data, aes(x = question_labels, y = cumulative_percent-Wgt_Percent/2-2,
                                        label = paste0(round(Wgt_Percent,0),"%"),colour = PNN, hjust = 0,size = 18),show.legend = FALSE)+
      annotate(geom = "text", x = selected_data$question_labels, y = -0.5, 
               label = format(selected_data$N_IncludedResponses, nsmall=1, big.mark=","), hjust = +1.2, vjust = 0, size = 5,colour = "#0078D4") +
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
  #update report area dropdown boxes to reflect report level selection####
  observe({
    updateSelectInput(session, "Report_Area", choices = as.character(Pos_neg$Report_Area[Pos_neg$Level == input$Level]))
  })
} 

