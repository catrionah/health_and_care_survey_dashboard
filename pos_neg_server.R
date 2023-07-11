#import most positive and negative data
Pos_neg <- readRDS("/conf/bss/survey_shiny/data/Pos_neg.rds")


Pos_neg <- Pos_neg %>%
  pivot_longer(names_to = "Indicator",values_to = "Wgt_Percent",cols = starts_with("Wgt_Percent"))%>%
  mutate(year = substr(Indicator,regexpr("[[:digit:]]",Indicator),regexpr("[[:digit:]]",Indicator)+4),
         PNN = substr(Indicator,regexpr("Percent",Indicator)+7,regexpr("_[[:digit:]]",Indicator)-1))%>%
  filter(!PNN == "Positive_Scot")%>%
  filter(Level %in% c("Scotland","Health Board"))%>%
  group_by(Level, Report_Area,Question_2022)%>%
  mutate(cumulative_percent = cumsum(Wgt_Percent),
         question_labels = str_wrap(paste0(Question_2022,": ",Question_text), width = 80))%>%
  arrange(Level, Report_Area,question_labels) %>% 
  mutate(question_labels=fct_reorder(question_labels,Question_2022))


# Define server logic####

#create response rate table####
  output$summary_table_pn <- renderTable({
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
  
  #update report area dropdown boxes to reflect report level selection####
  observe({
    updateSelectInput(session, "Report_Area", choices = as.character(Pos_neg$Report_Area[Pos_neg$Level == input$Level]))
  })





