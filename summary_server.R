
#import HACE data
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

# Define server logic####
#create response rate table####
  output$summary_table <- renderTable({
    summary_data <- data.frame(
      "indicator" = c("Response rate", "Number of responses","Number of forms sent out"),
      "value" = c(paste0(round(as.numeric(mean(rag_chart$Response_Rate_perc[rag_chart$Report_Area == input$Report_Area])),0),"%"),
                  format(first(rag_chart$N_IncludedResponses[rag_chart$Report_Area == input$Report_Area]),nsmall=1, big.mark=","),
                  format(first(rag_chart$sample_size[rag_chart$Report_Area == input$Report_Area]),nsmall=0, big.mark=","))
    )
    tibble::tibble(summary_data)
  }, colnames = FALSE,)
  
  #create question chart####
  output$percent_pnn_plot <- renderPlot({
    selectedData <- rag_chart%>%
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
    paste0(input$Report_Area,": ",as.character(rag_chart$Significance_Scot[rag_chart$PNN == "Positive" & rag_chart$question_labels == input$Label 
                                                                               & rag_chart$year == "2022" & rag_chart$Report_Area == input$Report_Area]))
  }) 
  
  output$header_trend <- renderText({
    paste0("Time trend (per cent positive results): ",input$Report_Area)
  }) 
  
  output$significance_2020 <- renderText({
    paste0(input$Report_Area,": ",as.character(rag_chart$Significance_2020[rag_chart$PNN == "Positive" & rag_chart$question_labels == input$Label 
                                                                               & rag_chart$year == "2022" & rag_chart$Report_Area == input$Report_Area]))
  }) 
  
  output$question_text_trend <- renderText({
    input$Label
  }) 
  
  #create comparison to Scotland chart####
  output$Scotland_plot <- renderPlot({
    scot_compare <- rag_chart %>%
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
    trend_data <- rag_chart %>%
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
    updateSelectInput(session, "Report_Area", choices = as.character(rag_chart$Report_Area[rag_chart$Level == input$Level]))
  })
