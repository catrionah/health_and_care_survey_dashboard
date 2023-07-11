#This is the overarching script to create the tabs within the HACE shiny dashboard

library(shiny)
library(tidyverse)

#CH to do:
#to do: 
#check phs guidance
#order all variables
#maintain comparator when scotland is selected?
#explore use of plotly to allow selection of question?
#add more levels
#add tabs for additional dashboards


# Source files with UI code for each tab --------------
source(file.path('summary_ui.R'),  local = TRUE)$value
source(file.path('pos_neg_ui.R'),  local = TRUE)$value

# define the UI structure -------------------------------
tagList(
  navbarPage( 
    
    # add PHS logo to navigation bar 
    title = div(style = "position: relative; 
                       top: -15px; 
                       margin-left: 10px; 
                       margin-top: 5px;",
                tags$a(img(src = "phs-logo-updated.png", 
                           width = 120, 
                           alt = "link to Public Health Scotland website"),
                       href = "https://www.publichealthscotland.scot/",
                       target = "_blank")
    ),
    
    # make navigation bar collapse on smaller screens
    windowTitle = "Health and Care Experience Survey",
    collapsible = TRUE,
    
    # order of tabs --------------------------------
    summaryTab,
    posnegTab,
    navbarMenu("Info")
    
  ) # close navbarPage
) # close taglist


## END


