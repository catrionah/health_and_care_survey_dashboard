library(tidyverse)

#import HACE data
rag_chart <- readRDS("/conf/bss/pat-exp-surveys/health-and-care/202122/output/tableau/rag_chart.rds")
rag_chart_pct <- rag_chart %>%
  pivot_longer(names_to = "Indicator",values_to = "Wgt_Percent",cols = starts_with("Wgt_Percent"))%>%
  mutate(year = substr(Indicator,regexpr("[[:digit:]]",Indicator),regexpr("[[:digit:]]",Indicator)+4),
         PNN = substr(Indicator,regexpr("Percent",Indicator)+7,regexpr("_[[:digit:]]",Indicator)-1))%>%
  filter(!PNN == "Positive_Scot")%>%
  filter(Level %in% c("Scotland","Health Board"))%>%
  filter(Question_2022 %in% c("10","13b","13d","27","33","38a"))%>%
  group_by(Level, Report_Area,Question_2022)%>%
  mutate(cumulative_percent = cumsum(Wgt_Percent),
         question_labels = str_wrap(paste0(Question_2022,": ",Question_text), width = 80))%>%
  arrange(Level, Report_Area,question_labels) %>% 
  mutate(question_labels=fct_reorder(question_labels,Question_2022))


# question_labels = str_wrap(paste0(Question_2022,": ",Question_text), width = 40))
# https://statisticsglobe.com/sprintf-r-function-example
