###############################################
#
# User interface for the HACE Shiny app
#
##############################################

#CH to do:
#This is intended to be the overarching script to create the tabs within the HACE shiny dashboard. I haven't spent much time on it and it doesn't work at all!
# Source files with UI code for each tab --------------
source(file.path('hace_summary_app.R'),  local = TRUE)$value
source(file.path('hace_pos_neg.R'),  local = TRUE)$value

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
    windowTitle = "Health and care Experience Survey",
    collapsible = TRUE,
    
    header = tags$head(
      # sourcing css style sheet 
      includeCSS("www/styles.css"),
               ),
    
    # order of tabs --------------------------------
    homepageTab,
    summaryTab,
    trendTab,
    rankTab,
    inequalitiesTab,
    dataTab,
    navbarMenu("Info",
               aboutTab,
               definitionsTab)
    
  ) # close navbarPage
) # close taglist


## END

