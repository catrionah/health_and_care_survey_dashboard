
# Name of file: pos_neg_ui.R
# Original author: Catriona Haddow
#   
# Written/run on Posit Workbench - RStudio R4.1.2
# 
# This app creates the most positive and negative dashboard of the HACE Survey 2022
# 
# Approximate run time: < 1 minute
# Approximate memory usage: 342 MiB


# Define UI for most positive and negative page
posnegTab <- tabPanel("Most positive and most negative", icon = icon("list-ul"), value = "posneg",
  
  # Application title
  titlePanel("2022 - Most positive and negative experience ratings"),
  p("This dashboard presents the questions in the 2022 survey which received the five most positive and the five most negative experience ratings for selected report areas.
  For each question, responses have been categorised as positive, neutral or negative. For further information, please refer to the technical report."),
  br(),
  p("The survey sections included in this dashboard are: The GP Practice; Treatment or Advice from the GP Practice; Out of Hours Healthcare; Care, Support and Help with Everyday Living; Caring Responsibilities."),
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
           tableOutput("summary_table_pn")
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


