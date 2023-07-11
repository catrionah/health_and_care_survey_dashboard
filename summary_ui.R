# Name of file: summary_ui.R
# Original author: Catriona Haddow
#   
# Written/run on Posit Workbench - RStudio R4.1.2
# 
# This app creates the summary dashboard of the HACE Survey 2022
# 
# Approximate run time: < 1 minute
# Approximate memory usage: 342 MiB

# Define UI for application that draws a histogram
summaryTab <- tabPanel("Summary", icon = icon("list-ul"), value = "summary",

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




