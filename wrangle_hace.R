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
         question_labels = str_wrap(paste0(Question_2022,": ",Question_text), width = 40))%>%
  arrange(Level, Report_Area,question_labels) %>% 
  mutate(question_labels=fct_reorder(question_labels,Question_2022))


#pull out year, remove Scotland, pull out positive, negative, neutral
percent_pnn_plot <- ggplot(rag_chart_pct[rag_chart_pct$Report_Area == "NHS Borders",], aes(x=question_labels,y=Wgt_Percent,fill = PNN)) +
  geom_col() + 
  scale_fill_manual(values=c("#0078D4","#AF69A9","#3F3685"))+
  geom_text(data=rag_chart_pct[rag_chart_pct$Report_Area == "NHS Borders",], aes(x = question_labels, y = cumulative_percent-Wgt_Percent/2-2,
                                                                                 label = paste0(round(Wgt_Percent,0),"%"),colour = PNN, hjust = 0,size = 14),show.legend = FALSE)+
  annotate(geom = "text", x = rag_chart_pct$question_labels[rag_chart_pct$Report_Area == "NHS Borders"], y = -0.5, 
           label = rag_chart_pct$N_IncludedResponses[rag_chart_pct$Report_Area == "NHS Borders"], hjust = +1.2, vjust = 0, size = 6) +
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
  
percent_pnn_plot

response <- paste0(as.numeric(first(rag_chart_pct$Response_Rate_perc[rag_chart_pct$Report_Area == "NHS Borders"])),"%")
paste0(as.numeric(first(rag_chart_pct$N_IncludedResponses[rag_chart_pct$Report_Area == "NHS Borders"])))

#comparison to Scotland
scot_compare <- rag_chart_pct %>%
                filter(Report_Area %in% c("NHS Borders","Scotland") & year == "2022" & PNN == "Positive" & Question_2022 == "38a")


Scotland_plot <- ggplot(scot_compare, aes(x=Report_Area,y=Wgt_Percent,fill = Level)) +
  geom_col() + 
  scale_fill_manual(values=c("#655E9D","#3F3685"))+
  geom_text(data=scot_compare, aes(x = Report_Area, y = Wgt_Percent/2,
                      label = paste0(round(Wgt_Percent,0),"%"),colour = "white", hjust = 0,size = 14),show.legend = FALSE)+
  scale_colour_manual(values="white")+
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        text=element_text(size=16))+
  guides(fill="none")
Scotland_plot 

question_text <- as.character(rag_chart_pct$question_labels[rag_chart_pct$PNN == "Positive" & rag_chart_pct$Question_2022 == "38a" &
                                                  rag_chart_pct$Report_Area == "Scotland"])
list_data <- list("a" = 9.43365584411011,1.03824189741523,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
c(x = 375, y = 365),                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
c(x = 375, y = 365),                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
c(x = 1, y = 1) ,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
c(x = "Wgt_Percent", y = "question_labels", fill = "PNN") ,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
c(left = -0.5, right = 100.01, bottom = 0.55, top = 6.45, 
  discrete_limits = list(y = list("10: Overall, how would you rate the care\\nprovided by your GP practice?", "13b: Experience of your GP Practice: I\\nwas listened to\", \"13d: Experience of your GP Practice:\\nI was treated with compassion and\\nunderstanding", 
                                  "27: Overall, how would you rate the care\\nyou experienced from this Out of Hours\\nservice?", "33: Overall, how would you rate your\\nhelp, care or support services? Please\\nexclude the care and help you get from\\nfriends and family", 
                                  "38a: I have a good balance between\\ncaring and other things in my life"))),
c(left = 339.957658779577, right = 694.520547945206, bottom = 391.780821917808, top = 68.1567123287671),                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
c(x = NULL, y = NULL) )    

list_data[[]]

rag_chart_pct$question_labels[1]

#create trend plot
trend_data <- rag_chart_pct %>%
  filter(Report_Area %in% c("NHS Borders") & PNN == "Positive" & Question_2022 == "38a")


trend_plot <- ggplot(trend_data, aes(x=year,y=Wgt_Percent,fill = Level)) +
  geom_col() + 
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
trend_plot 


selectedData <- rag_chart_pct%>%
  select(Level,Report_Area, question_labels,PNN,Wgt_Percent,cumulative_percent) %>% 
  filter(Report_Area == "NHS Borders" & year == "2022")

data <- reactive(  
  data.frame(
    group = sample(c("A", "B"), 100, replace = TRUE),
    var1 = round(runif(100, min = 0, max = 100), 0),
    var2 = sample(c("A", "B"), 100, replace = TRUE)
  )
)

server <- function(input, output) {
  output$text1 <- renderTable({
    tibble::enframe(summary(data()$var1))
  })
  output$text2 <- renderTable({
    tibble::tibble(!!!summary(data()$var1))
  })
}

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
