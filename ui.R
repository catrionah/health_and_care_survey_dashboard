#This is the overarching script to create the tabs within the HACE shiny dashboard

library(shiny)
library(tidyverse)

#CH to do:
#to do: 
#check phs guidance
#order all variables
#maintain comparator when scotland is selected?
#explore use of plotly to allow selection of question?
#add more levels. Correctly!
#consider using server-side selectize
#add tabs for additional dashboards

#import HACE data. CH note - we shouldn't do this here, but it wasn't available to the ui script when only done in server. Not sure where best to do this,
rag_chart <- readRDS("/conf/bss/survey_shiny/data/rag_chart.rds")
rag_chart <- rag_chart %>%
  pivot_longer(names_to = "Indicator",values_to = "Wgt_Percent",cols = starts_with("Wgt_Percent"))%>%
  mutate(year = substr(Indicator,regexpr("[[:digit:]]",Indicator),regexpr("[[:digit:]]",Indicator)+4),
         PNN = substr(Indicator,regexpr("Percent",Indicator)+7,regexpr("_[[:digit:]]",Indicator)-1))%>%
  filter(!PNN == "Positive_Scot")%>%
  filter(Question_2022 %in% c("10","13b","13d","27","33","38a"))%>%
  group_by(Level, Report_Area,Question_2022)%>%
  mutate(cumulative_percent = cumsum(Wgt_Percent),
         question_labels = str_wrap(paste0(Question_2022,": ",Question_text), width = 80))%>%
  arrange(Level, Report_Area,question_labels) %>% 
  mutate(question_labels=fct_reorder(question_labels,Question_2022))

#import most positive and negative data
Pos_neg <- readRDS("/conf/bss/survey_shiny/data/Pos_neg.rds")


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


