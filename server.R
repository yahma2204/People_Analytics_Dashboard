library(shiny)

shinyServer(function(session, input, output) {
  
      output$guide_base1 <- renderUI({
        div(h1("Guide : How to Interpret The Individual Plot", align="center"),
        br())
      })
      
      output$guide_base2 <- renderImage({
        list(src = 'WWW/5.png',
             contentType = 'image/png',
             width = 1000,
             height = 500,
             alt = "File doesn't exist!")
        },deleteFile = FALSE)
      
      output$guide_base3 <- renderUI({
        
        div(
          br(),
          h4("1. Case"),
          p("No of observations that we predict."),
          h4("2. Label"),
          p("The text `Label: Yes` shows what value of target variable is being explained. In this guide, we use employee 
                                                                  attrition prediction case as example. So, if the 
          `Label: Yes`, it means that the employee predicted to leave company.", align = "justify"),
          h4("3. Probability"),
          p("The `Probability` shows the probability of the observation belongs to the label. In this example, the probability
                                                                  of Case 510 to be predicted as `Yes` is 90%.", align = "justify"),
          h4("4. Explanation Fit"),
          p("The next element is Explanation Fit. These values indicate how good LIME (the interpretation package) explain our model (in this dashboard we
                                                                  use Random Forest model).", align = "justify"),
          h4("5. Plot"),
          p("Below all of those label, there is a bar plot, with y-axis shows each selected features while x-axis is the weight 
                                                                  of each respective features. The color of each bar represent whether the features support or contradict if the observations 
                                                                  labeled as yes. The interpretation is quite simple. For example, for observation 510, EducationField = Technical Degree has the biggest weight to 
                                                                  support the attrition to be yes. This means that the employee has education field in technical degree and more likely to turnover. 
                                                                  On the other hand, the Department = Research and Development contradicts the likelihood to resign, suggesting that the employee comfortable with her/his 
                                                                  department so it makes her/him want to stay in the company.", align = "justify")
          
        )
        
      })
      
      observeEvent(input$guide, {
        
        showModal(modalDialog(
          fluidRow(
              box(width = 12,uiOutput("guide_base1")),
              box(width = 12,imageOutput("guide_base2")), 
              box(width = 12,uiOutput("guide_base3"))),
          footer = actionButton("confirm", label = "Close"),size="xl",
          easyClose = TRUE
        ))
      })
      
      observeEvent(input$confirm, {
        removeModal()
      })
      
  observeEvent(input$jump_att, {
    updateTabsetPanel(session, "inTabset",
                      selected = "attrition")
  })
  
  observeEvent(input$jump_promoted, {
    updateTabsetPanel(session, "inTabset",
                      selected = "promotion")
  })
  
  observeEvent(input$jump_absent, {
    updateTabsetPanel(session, "inTabset",
                      selected = "absent")
  })
  
  #EMPLOYEE ATTRITION ------------------------------------------------
  
  output$full_employ <- renderText({
    att <- employ_train  
    print(n_distinct(att))
  })
  
  output$full_attrition <- renderText({
    att <- employ_train %>% 
      filter(Attrition == "Yes")  %>% 
      n_distinct()
    paste0(att ," or ", round((att/n_distinct(employ_train))*100), "%")
  })
  
  output$full_stay <- renderText({
    att <- employ_train %>% 
      filter(Attrition == "No") %>% 
      n_distinct()
    paste0(att ," or ", round((att/n_distinct(employ_train))*100), "%")
  })
  
  output$demography <- renderPlotly({
    
    d1_viz <- employ_train %>% 
      select(Gender) %>% 
      count(Gender) %>% 
      mutate(label = glue('Gender :{Gender}
                      Total Employees : {n}')) %>% 
      ggplot(aes(x = reorder(Gender,-n), y = n, text = label))+
      geom_col(position = "dodge", fill = "#D90416")+
      labs(x = NULL, y = "Total Employees")+
      scale_x_discrete(labels = wrap_format(10))
    
    d1_plotly <- ggplotly(d1_viz, tooltip = "text")%>% add_annotations(
      text = "Gender",
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(size = 15)
    )
    
    
    d2_viz <- employ_train %>% 
      select(Education) %>% 
      count(Education) %>% 
      mutate(label = glue('Education :{Education}
                      Total Employees : {n}')) %>% 
      ggplot(aes(x = reorder(Education,-n), y = n, fill = "#D90416", text = label))+
      geom_col(position = "dodge", fill = "#D90416")+
      labs(x = NULL, y = "Total Employees", fill = "Attrition")+
      scale_x_discrete(labels = wrap_format(10))
    
    d2_plotly <- ggplotly(d2_viz, tooltip = "text")%>% add_annotations(
      text = "Education",
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(size = 15)
    )
    
    
    d3_viz <- employ_train %>% 
      select(Department) %>% 
      count(Department) %>% 
      mutate(label = glue('Department :{Department}
                      Total Employees : {n}')) %>% 
      ggplot(aes(x = reorder(Department,-n), y = n, fill = "#D90416", text = label))+
      geom_col(position = "dodge", fill = "#D90416")+
      labs(x = NULL, y = "Total Employees", fill = "Attrition")+
      scale_x_discrete(labels = wrap_format(20))
    
    d3_plotly <- ggplotly(d3_viz, tooltip = "text")%>% add_annotations(
      text = "Department",
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(size = 15)
    )
    
    d4_viz <- employ_train %>% 
      select(JobRole) %>% 
      count(JobRole) %>% 
      mutate(label = glue('JobRole :{JobRole}
                      Total Employees : {n}')) %>% 
      ggplot(aes(x = reorder(JobRole,-n), y = n, fill = "#D90416", text = label))+
      geom_col(position = "dodge", fill = "#D90416")+
      labs(x = NULL, y = "Total Employees", fill = "Attrition")+
      scale_x_discrete(labels = wrap_format(10))+
      scale_colour_viridis_d(option = "inferno")
    
    d4_plotly <- ggplotly(d4_viz, tooltip = "text")%>% add_annotations(
      text = "Job Role",
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(size = 15)
    )
    
    
    d5_viz <- employ_train %>% 
      select(Age) %>% 
    ggplot(aes(y=Age
               , fill = "#D90416"))+
      geom_boxplot(fill = "#D90416")+
      labs(x = NULL, y = "Total Employees")
    
    d5_plotly <- ggplotly(d5_viz, tooltip = "text")%>% add_annotations(
      text = "Age",
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(size = 15)
    )
    
    d6_viz <- employ_train %>% 
      select(JobLevel) %>% 
      count(JobLevel) %>% 
      mutate(label = glue('JobLevel :{JobLevel}
                      Total Employees : {n}')) %>% 
      ggplot(aes(x = factor(JobLevel, levels=c(1,2,3,4,5)), y = n, fill = "#D90416", text = label))+
      geom_col(position = "dodge", fill = "#D90416")+
      labs(x = NULL, y = "Total Employees", fill = "Attrition")+
      scale_x_discrete(labels = wrap_format(10))
    
    d6_plotly <- ggplotly(d6_viz, tooltip = "text") %>% add_annotations(
      text = "Job Level",
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(size = 15)
    )
    
    
    s1 <- subplot(d1_plotly, d2_plotly, widths = c(0.3,0.7),shareY=T)
    s2 <- subplot(d6_plotly,d3_plotly)
    s3 <- subplot(s1, d5_plotly)
    s5 <- subplot(s3, s2, d4_plotly, nrows = 3,margin = 0.08) %>%
      layout(showlegend = F,
             plot_bgcolor='#e5ecf6', 
             xaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'), 
             yaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'), hovermode = 'x')
    s5
    
    
  })
  
 
  output$experiences <- renderPlotly({
    plot_exp1 <- employ_train %>% select(TotalWorkingYears) %>% 
      ggplot(aes(x=TotalWorkingYears
                 , text = TotalWorkingYears, fill = "#D90416")) +
      geom_density(fill = "#D90416") 
    
    d1_plotly <- ggplotly(plot_exp1, tooltip = "text") %>% add_annotations(
      text = "Total Working Experience",
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(size = 15)
    )
    
    # 2.NumCompaniesWorked
    plot_exp2 <- employ_train %>% select(NumCompaniesWorked) %>% 
      ggplot(aes(x=NumCompaniesWorked
                 , fill = "#D90416", text = NumCompaniesWorked)) +
      geom_density(fill = "#D90416") 
    
      d2_plotly <- ggplotly(plot_exp2, tooltip = "text") %>% add_annotations(
        text = "Total Company Worked",
        x = 0.5,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "center",
        yanchor = "top",
        yshift = 20,
        showarrow = FALSE,
        font = list(size = 15)
      )
      
      
    # 3.YearsAtCompany
    plot_exp3 <- employ_train %>% select(YearsAtCompany) %>% 
      ggplot(aes(x=YearsAtCompany
                 , text=YearsAtCompany)) +
      geom_density(fill = "#D90416")
      
    d3_plotly <- ggplotly(plot_exp3, tooltip = "text") %>% add_annotations(
      text = "Years at Company",
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(size = 15)
    )
    
    
    # 4.NumCompaniesWorked
    plot_exp4 <- employ_train %>% select(TrainingTimesLastYear) %>% 
      ggplot(aes(x=TrainingTimesLastYear,fill = "#D90416",text=TrainingTimesLastYear
                 )) +
      geom_density(fill = "#D90416")
      d4_plotly <- ggplotly(plot_exp4, tooltip = "text") %>% add_annotations(
        text = "Total Completed Training",
        x = 0.5,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "center",
        yanchor = "top",
        yshift = 20,
        showarrow = FALSE,
        font = list(size = 15)
      )
      
      
      s5 <- subplot(d1_plotly, d3_plotly,d2_plotly,d4_plotly, nrows = 2, heights = c(0.5,0.5), margin = 0.08, shareY = F) %>%
        layout(showlegend = F,
               plot_bgcolor='#e5ecf6', 
               xaxis = list( 
                 zerolinecolor = '#ffff', 
                 zerolinewidth = 2, 
                 gridcolor = 'ffff'), 
               yaxis = list( 
                 zerolinecolor = '#ffff', 
                 zerolinewidth = 2, 
                 gridcolor = 'ffff'), hovermode = 'x')
      s5
      
  })
  
output$company_survey <- renderPlotly({
    #1. EnvironmentSatisfaction
    plot_sv1 <- employ_train %>% select(EnvironmentSatisfaction) %>% 
      count(EnvironmentSatisfaction) %>% 
      mutate(label = glue('Environment Satisfaction : {EnvironmentSatisfaction}
                          Total Employee : {n}')) %>% 
      ggplot(aes(x=reorder(factor(EnvironmentSatisfaction),n), y=n, fill = "#D90416",text = label), fill = "#D90416") +
      geom_segment( aes(xend=EnvironmentSatisfaction, yend=0), fill = "#D90416") +
      geom_point( size=2, color ="#D90416",fill = "#D90416") +
      coord_flip()
    
    d1_plotly <- ggplotly(plot_sv1, tooltip = "text") %>% add_annotations(
      text = "Environment Satisfaction",
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(size = 15)
    )
    
    #2. JobSatisfaction
    plot_sv2 <- employ_train %>% select(JobSatisfaction) %>% 
      count(JobSatisfaction) %>% 
      mutate(label = glue('Job Satisfaction : {JobSatisfaction}
                          Total Employee : {n}')) %>% 
      ggplot(aes(x=reorder(factor(JobSatisfaction),n), y=n, fill = "#D90416",text = label)) +
      geom_segment( aes(xend=JobSatisfaction, yend=0)) +
      geom_point( size=2, color ="#D90416",fill = "#D90416") +
      coord_flip() 
    
    d2_plotly <- ggplotly(plot_sv2, tooltip = "text") %>% add_annotations(
      text = "Job Satisfaction",
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(size = 15)
    )
    
    #3. JobInvolvement
    plot_sv3 <- employ_train %>% select(JobInvolvement) %>% 
      count(JobInvolvement) %>% 
      mutate(label = glue('Job Involvement : {JobInvolvement}
                          Total Employee : {n}')) %>% 
      ggplot(aes(x=reorder(factor(JobInvolvement),n), fill = "#D90416",y=n, text = label)) +
      geom_segment( aes(xend=JobInvolvement, yend=0)) +
      geom_point( size=2, color ="#D90416",fill = "#D90416") +
      coord_flip()
    
    d3_plotly <- ggplotly(plot_sv3, tooltip = "text") %>% add_annotations(
      text = "Job Involvement",
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(size = 15)
    )

    #4. WorkLifeBalance
    plot_sv4 <- employ_train %>% select(WorkLifeBalance) %>% 
      count(WorkLifeBalance) %>% 
      mutate(label = glue('Work Life Balance : {WorkLifeBalance}
                          Total Employee : {n}')) %>%
      ggplot(aes(x=reorder(factor(WorkLifeBalance),n), y=n, fill = "#D90416",text = label)) +
      geom_segment( aes(xend=WorkLifeBalance, yend=0)) +
      geom_point( size=2, color ="#D90416",fill = "#D90416") +
      coord_flip() 
    
      d4_plotly <- ggplotly(plot_sv4, tooltip = "text")%>% add_annotations(
        text = "Work Life Balance",
        x = 0.5,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "center",
        yanchor = "top",
        yshift = 20,
        showarrow = FALSE,
        font = list(size = 15)
      )
      
      
      s5 <- subplot(d1_plotly, d2_plotly,d3_plotly,d4_plotly, nrows = 2, heights = c(0.5,0.5), margin = 0.08) %>%
        layout(showlegend = F,
               plot_bgcolor='#e5ecf6', 
               xaxis = list( 
                 zerolinecolor = '#ffff', 
                 zerolinewidth = 2, 
                 gridcolor = 'ffff'), 
               yaxis = list( 
                 zerolinecolor = '#ffff', 
                 zerolinewidth = 2, 
                 gridcolor = 'ffff'))
      s5
  })

  output$var_imp_employ <- renderPlotly({
    var_imp_employ <- data.frame(varImp(model_forest)$importance) %>% 
      rename(Importance = Overall)
    var_imp_employ$Variable <- rownames(var_imp_employ)
    rownames(var_imp_employ) <- NULL
    
    var_imp_plot <- var_imp_employ %>% head(10) %>% 
      mutate(Importance = round(Importance, 2),
               label = glue("Variable = {Variable}
                          Importance = {Importance}")) %>% 
      ggplot(aes(y = reorder(Variable,Importance), x = Importance, text = label)) +
      geom_col(fill = "#A60303")+
      labs(y = NULL) +
        scale_fill_viridis_c()+
        theme_minimal()+
        scale_y_discrete(labels = wrap_format(15))
    ggplotly(var_imp_plot, tooltip="text")
      
  })
  
  output$bagi_att_or_not <- renderPlotly({
    
    plot_employ <- employ_train %>% 
      select(is_Attrition) %>% 
      count(is_Attrition) %>% 
      mutate(label = glue("Attrition? : {is_Attrition}
                          Total Employee : {n}")) 
    colors <- c('#D90416','#260101')
    fig <- plot_ly(plot_employ, labels = ~is_Attrition, values = ~n, type = 'pie',
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',
                   text = ~paste('Total Employee : ', n),
                   marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)),
                   showlegend = FALSE)
    fig <- fig %>% layout(
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
  })
  
  output$no_attrition <- renderText({
    
    att <- employ_train %>% 
      filter(is_Attrition == "Yes")  %>% 
      filter(Department %in% input$department) 
    print(n_distinct(att))
    
  })
  
  output$stay <- renderText({
    
    att_stay <- employ_train %>% 
      filter(is_Attrition == "No")  %>% 
      filter(Department %in% input$department) 
    print(n_distinct(att_stay))
    
  })
  
  observe({
    employ_train$prob_Attrition <- predict(object = model_forest, newdata = employ_train, type = "prob")[,input$att_or_not]
    
    updateSelectInput(
      inputId = "variable",
      label = "Select Variable :",
      choices = employ_train %>% select(-Attrition) %>% names(), 
      selected = c("EmployeeNumber", "is_Attrition", "prob_Attrition"))
  })
  
  
  output$PredOutput <-  renderDataTable({
    employ_train$prob_Attrition <- predict(object = model_forest, newdata = employ_train, type = "prob")[,input$att_or_not]
    
    employ_train <- employ_train %>% 
      filter(is_Attrition == input$att_or_not) %>% 
      filter(Department %in% input$department) %>% 
      arrange(-prob_Attrition) %>% 
      select(c(input$variable))
    if(is.null(employ_train)){
      return(NULL)
      }
    employ_train
    
  },
  extensions = "Buttons", options = list(lengthMenu = list(c(5,10,-1), c(5, 10, "All")), pageLength = 5, scrollX = TRUE
))

   
   observe({
     att <- employ_train %>% 
       filter(is_Attrition == input$att_or_not)  %>% 
       filter(Department %in% input$department)
     updateSelectizeInput(session, inputId = "employ_id",
                       label = "Select Employee ID: (Max. 4)",
                       choices = unique(att$EmployeeNumber),
                       selected = sample(unique(att$EmployeeNumber),1),
                       options = list(maxItems = 4))
   })
   
   output$individual <- renderPlot({
     
     set.seed(100)
     explainer <- explain(x = employ_testing[input$employ_id,], 
                          explainer = explain, 
                          n_labels = 1, 
                          n_features = 10, 
                          n_permutation = 600,
                          dist_fun = "manhattan")
     
     plot_features(explainer)
     
   })
   
   # Update Dataset
   reactive_att<-reactive({
     req(input$file_att)
     data_test_att <- read.csv(input$file_att$datapath)
     return(data_test_att)
   })
   
   observe({
     data_test_att <- reactive_att()
     updateSelectizeInput(session, inputId = "att_test_id",
                       label = "Select Employee ID: (Max. 4)",
                       choices = unique(data_test_att$EmployeeNumber),
                       selected = sample(unique(data_test_att$EmployeeNumber),1),
                       options = list(maxItems = 4))
   })
   
   output$data_Test_Attrition <- renderDataTable({
     file_to_read_att = input$file_att
     if(is.null(file_to_read_att)){
       return(NULL)
     }
     
     data_test_att <- read.csv(file_to_read_att$datapath)
     data_test_att
     
   }, options = list(lengthMenu = list(c(5,10,-1), c(5, 10, "All")), pageLength = 5, scrollX = TRUE))
   
   output$data_Test_Attrition_example <- renderDataTable({
     employ_test
     
   }, options = list(lengthMenu = list(c(5,10,-1), c(5, 10, "All")), pageLength = 5, scrollX = TRUE))
   
   output$sim_att_plot <- renderPlotly({
     employ_test <- reactive_att()
     employ_test$is_Attrition <- predict(object = model_forest, newdata = employ_test, type = "raw")
     
     plot_employ <- employ_test %>% 
       select(is_Attrition) %>% 
       count(is_Attrition) %>% 
       mutate(label = glue("Attrition? : {is_Attrition}
                          Total Employee : {n}")) 
     colors <- c('#D90416','#260101')
     fig <- plot_ly(plot_employ, labels = ~is_Attrition, values = ~n, type = 'pie',
                    textposition = 'inside',
                    textinfo = 'label+percent',
                    insidetextfont = list(color = '#FFFFFF'),
                    hoverinfo = 'text',
                    text = ~paste('Total Employee : ', n),
                    marker = list(colors = colors,
                                  line = list(color = '#FFFFFF', width = 1)),
                    showlegend = FALSE)
     fig <- fig %>% layout(
       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
     
     fig
   })
   
   output$sim_att_plot_example <- renderPlotly({
     employ_test$is_Attrition <- predict(object = model_forest, newdata = employ_test, type = "raw")
     
       plot_employ <- employ_test %>% 
         select(is_Attrition) %>% 
         count(is_Attrition) %>% 
         mutate(label = glue("Attrition? : {is_Attrition}
                          Total Employee : {n}")) 
       colors <- c('#D90416','#260101')
       fig <- plot_ly(plot_employ, labels = ~is_Attrition, values = ~n, type = 'pie',
                      textposition = 'inside',
                      textinfo = 'label+percent',
                      insidetextfont = list(color = '#FFFFFF'),
                      hoverinfo = 'text',
                      text = ~paste('Total Employee : ', n),
                      marker = list(colors = colors,
                                    line = list(color = '#FFFFFF', width = 1)),
                      showlegend = FALSE)
       fig <- fig %>% layout(
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
       
       fig
   })
   
     
    output$sim_att_data <-  renderDataTable({
    employ_test <- reactive_att()
    employ_test$is_Attrition <- predict(object = model_forest, newdata = employ_test, type = "raw")
    employ_test$Attrition_No <- predict(object = model_forest, newdata = employ_test, type = "prob")[,1]
    employ_test$Attrition_Yes <- predict(object = model_forest, newdata = employ_test, type = "prob")[,2]
    
     employ_test <- employ_test  %>% 
       arrange(-Attrition_Yes) 
     if(is.null(employ_test)){
       return(NULL)
     }
   },
   extensions = "Buttons", options = list(lengthMenu = list(c(5,10,-1), c(5, 10, "All")), pageLength = 5, scrollX = TRUE
   ))
    
    output$sim_att_data_example <-  renderDataTable({
     
      employ_test$is_Attrition <- predict(object = model_forest, newdata = employ_test, type = "raw")
      employ_test$Attrition_No <- predict(object = model_forest, newdata = employ_test, type = "prob")[,1]
      employ_test$Attrition_Yes <- predict(object = model_forest, newdata = employ_test, type = "prob")[,2]
      
      employ_test <- employ_test %>% 
        arrange(-c(Attrition_Yes)) 
      if(is.null(employ_test)){
        return(NULL)
      }
      employ_test
      
    },
    extensions = "Buttons", options = list(lengthMenu = list(c(5,10,-1), c(5, 10, "All")), pageLength = 5, scrollX = TRUE
                                           # ,dom = 'Bfrtip', buttons = list('copy', 'csv')
    ))
   
   output$att_test_plot <- renderPlot({
     data_test_att <- reactive_att()
     rownames(data_test_att) <- data_test_att$EmployeeNumber
     data_test_att <- data_test_att %>% 
       select(c(Age, BusinessTravel,DailyRate, Department,DistanceFromHome,Education, 
                EducationField, EnvironmentSatisfaction,Gender,HourlyRate, JobInvolvement, 
                JobLevel,JobRole, JobSatisfaction, MaritalStatus, MonthlyIncome, 
                MonthlyRate, NumCompaniesWorked, OverTime,PercentSalaryHike,
                PerformanceRating,RelationshipSatisfaction,StockOptionLevel,
                TotalWorkingYears, TrainingTimesLastYear, WorkLifeBalance,YearsAtCompany, 
                YearsInCurrentRole,YearsSinceLastPromotion,YearsWithCurrManager))
     
     explain_att <- lime(x = data_test_att, model = model_forest)
     
     set.seed(100)
     explainer_test_att <- explain(x = data_test_att[c(input$att_test_id),], 
                                   explainer = explain_att, 
                                   n_labels = 1, 
                                   n_features = 10, 
                                   n_permutation = 600,
                                   dist_fun = "manhattan")
     plot_features(explainer_test_att)
   })
   
   output$att_test_plot_example <- renderPlot({
     employ_test <- employ_test %>% 
       select(c(Age, BusinessTravel,DailyRate, Department,DistanceFromHome,Education, 
                EducationField, EnvironmentSatisfaction,Gender,HourlyRate, JobInvolvement, 
                JobLevel,JobRole, JobSatisfaction, MaritalStatus, MonthlyIncome, 
                MonthlyRate, NumCompaniesWorked, OverTime,PercentSalaryHike,
                PerformanceRating,RelationshipSatisfaction,StockOptionLevel,
                TotalWorkingYears, TrainingTimesLastYear, WorkLifeBalance,YearsAtCompany, 
                YearsInCurrentRole,YearsSinceLastPromotion,YearsWithCurrManager))
     
     explain_att <- lime(x = employ_test, model = model_forest)
     
     set.seed(100)
     explainer_test_att <- explain(x = employ_test[c(input$att_test_id_example),], 
                                   explainer = explain_att, 
                                   n_labels = 1, 
                                   n_features = 10, 
                                   n_permutation = 600,
                                   dist_fun = "manhattan")
     plot_features(explainer_test_att)
   })
 
#EMPLOYEE PROMOTION ------------------------------------------------
   
   output$promote_all <- renderText({
     
     att_promote <- promoteClean %>% 
       n_distinct()
     print(att_promote)
     
   })
   
   output$full_promoted <- renderText({
     
     att_promote <- promoteClean %>% 
       filter(promoted == "Yes") %>% 
       n_distinct()
     paste0(att_promote, " or ", round((att_promote/n_distinct(promoteClean))*100), "%")
     
   })
   
   output$full_not_promoted <- renderText({
     
     att_promote_no <- promoteClean %>% 
       filter(promoted == "No")  %>% 
       n_distinct()
     paste0(att_promote_no, " or ", round((att_promote_no/n_distinct(promoteClean))*100), "%")
     
   })
   

output$demography_promote <- renderPlotly({
  
  d1_viz <- promote_train %>% 
    select( department) %>% 
    group_by(department) %>% 
    count() %>% 
    mutate(label = glue('Department : {department}
                      Total Employee : {n}')) %>% 
    ggplot(aes(x=reorder(department,-n),y=n, fill = "#D90416", text = label))+
    geom_col(position = "dodge", fill = "#D90416")+
    labs(x = NULL, y = "Total Employees", fill = "Promoted?")+
    scale_x_discrete(labels = wrap_format(10))
  
  d1_plotly <-  ggplotly(d1_viz, tooltip = "text")%>% add_annotations(
    text = "Department",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  
  d2_viz <- promote_train %>% 
    select(education) %>% 
    count(education) %>% 
    mutate(label = glue('Education :{education}
                      Total Employees : {n}')) %>% 
    ggplot(aes(x = reorder(education,n), y = n, fill = "#D90416", text = label))+
    geom_col(position = "dodge", fill = "#D90416")+
    labs(x = NULL, y = "Total Employees", fill = "is_promoted")+
    scale_x_discrete(labels = wrap_format(10))+
    coord_flip()
  
  d2_plotly <- ggplotly(d2_viz, tooltip = "text")%>% add_annotations(
    text = "Education",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  
  d3_viz <- promote_train %>% 
    select(region) %>% 
    count(region) %>% 
    mutate(label = glue('Region :{region}
                      Total Employees : {n}')) %>% 
    ggplot(aes(x = reorder(region,-n), y = n, fill = "#D90416", text = label))+
    geom_col(position = "dodge", fill = "#D90416")+
    scale_x_discrete(labels = wrap_format(20))+
    theme(axis.text.x = element_text(angle = 90))
  
  d3_plotly <- ggplotly(d3_viz, tooltip = "text")%>% add_annotations(
    text = "Region",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  
  d4_viz <- promote_train %>% 
    select(gender) %>% 
    count(gender) %>% 
    mutate(gender = factor(gender, levels = c("f","m"),labels = c("Female","Male")),
          label = glue('Gender :{gender}
                      Total Employees : {n}')) %>% 
    ggplot(aes(x = gender, y = n, fill = "#D90416", text = label))+
    geom_col(position = "dodge",fill="#D90416")+
    labs(x = NULL, y = "Total Employees", fill = "is_promoted")+
    scale_x_discrete(labels = wrap_format(10))+
    coord_flip()
  
  d4_plotly <- ggplotly(d4_viz, tooltip = "text")%>% add_annotations(
    text = "Gender",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  
  d5_viz <- promote_train %>% 
    select(age) %>% 
    ggplot(aes(x=age
               , fill = "#D90416", text = age))+
    geom_histogram(fill="#D90416")+
    labs(x = NULL, y = "Total Employees", fill = "is_promoted")
  
  d5_plotly <- ggplotly(d5_viz, tooltip = "text")%>% add_annotations(
    text = "Age",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  s1 <- subplot(d1_plotly, d3_plotly, nrows = 2, margin = 0.16)
  s4 <- subplot(d2_plotly,d4_plotly, margin=0.06)
  s3 <- subplot(s4, d5_plotly,widths = c(0.6,0.4),margin = 0.03)
  s5 <- subplot(s3, s1, nrows = 2, margin = 0.08, heights = c(0.4,0.6)) %>%
    layout(showlegend = F,
           plot_bgcolor='#e5ecf6', 
           xaxis = list( 
             zerolinecolor = '#ffff', 
             zerolinewidth = 2, 
             gridcolor = 'ffff'), 
           yaxis = list( 
             zerolinecolor = '#ffff', 
             zerolinewidth = 2, 
             gridcolor = 'ffff'), hovermode = 'x')
  s5
  
})

   
output$experiences_promote <- renderPlotly({
  
  d1_viz <- promote_train %>% 
    select(recruitment_channel) %>% 
    group_by(recruitment_channel) %>% 
    count() %>% 
    mutate(label = glue('Recruitment Channel : {recruitment_channel}
                      Total Employee : {n}')) %>% 
    ggplot(aes(x=reorder(recruitment_channel,n),y=n, fill = "#D90416", text = label))+
    geom_col(position = "dodge",fill="#D90416")+
    labs(x = NULL, y = "Total Employees", fill = "Promoted?")+
    scale_x_discrete(labels = wrap_format(10))+
    coord_flip()
  
  d1_plotly <-  ggplotly(d1_viz, tooltip = "text")%>% add_annotations(
    text = "Recruitment Channel",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  
  d2_viz <- promote_train %>% 
    select(no_of_trainings) %>% 
    count(no_of_trainings) %>% 
    mutate(no_of_trainings = as.factor(
      ifelse(no_of_trainings == 1, "1",
             ifelse(no_of_trainings == 2, "2",
                    ifelse(no_of_trainings == 3, "3",
                           ifelse(no_of_trainings == 4, "4",
                                  ifelse(no_of_trainings == 5, "5",">5"
                                         )
                                  )
                           )
                           )
                    )
             )
      
    )  %>% 
    group_by(no_of_trainings) %>% 
    count(no_of_trainings) %>% 
    mutate(label = glue('Number of Trainings :{no_of_trainings}
                      Total Employees : {n}')) %>% 
    ggplot(aes(x = factor(no_of_trainings, levels = c("1","2","3","4","5",">5")), y = n, fill = "#D90416", text = label))+
    geom_col(position = "dodge",fill="#D90416")+
    labs(x = NULL, y = "Total Employees", fill = "is_promoted")+
    scale_x_discrete(labels = wrap_format(10))
  
  
  d2_plotly <- ggplotly(d2_viz, tooltip = "text")%>% add_annotations(
    text = "Number of Trainings",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  
  d3_viz <- promote_train %>% 
    select(previous_year_rating) %>% 
    count(previous_year_rating) %>% 
    mutate(label = glue('Previous Year Rating :{previous_year_rating}
                      Total Employees : {n}')) %>% 
    ggplot(aes(x = factor(previous_year_rating), y = n, fill = "#D90416", text = label))+
    geom_col(position = "dodge",fill="#D90416")+
    scale_x_discrete(labels = wrap_format(20))
  
  d3_plotly <- ggplotly(d3_viz, tooltip = "text")%>% add_annotations(
    text = "Previous Year Rating",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  
  d4_viz <- promote_train %>% 
    select(length_of_service) %>% 
    count(length_of_service) %>% 
    mutate(length_of_service = as.factor(
      ifelse(length_of_service < 2, "<2",
             ifelse(((length_of_service >= 2) & (length_of_service <= 5)), "2-5",
                    ifelse(((length_of_service > 5) & (length_of_service <= 8)), "5-10",
                           ifelse(((length_of_service > 10) & (length_of_service <= 15)), "10-15",
                                  ifelse(((length_of_service > 15) & (length_of_service <= 20)), "15-20",
                                         ifelse(((length_of_service > 20) & (length_of_service <= 25)), "20-25",
                                                ifelse(((length_of_service > 25) & (length_of_service <= 30)), "25-30",
                                                       ifelse(((length_of_service > 30) & (length_of_service <= 35)), "30-35", ">35"
                                                       )
                                                )
                                         )
                                  )
                           )
                    )
             )
      )
    ) 
    ) %>% 
    group_by(length_of_service) %>% 
    count(length_of_service) %>% 
    mutate(label = glue('Length of Service :{length_of_service}
                      Total Employees : {n}')) %>% 
    ggplot(aes(x = factor(length_of_service, levels = c("<2","2-5","5-10","10-15","15-20"
                                                        ,"20-25","25-30","30-35", ">35")), y = n, fill = "#D90416", text = label))+
    geom_col(position = "dodge",fill="#D90416")+
    labs(x = NULL, y = "Total Employees", fill = "is_promoted")+
    scale_x_discrete(labels = wrap_format(10))
  
  d4_plotly <- ggplotly(d4_viz, tooltip = "text")%>% add_annotations(
    text = "Length of Service",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  
  d5_viz <- promote_train %>% 
    select(avg_training_score) %>% 
    mutate(avg_training_score = as.factor(
      ifelse(avg_training_score < 20, "<20",
             ifelse((avg_training_score >= 20) & (avg_training_score <= 35), "20-35",
                    ifelse((avg_training_score >= 35) & (avg_training_score <= 45), "35-45",
                           ifelse((avg_training_score >= 45) & (avg_training_score <= 55), "45-55",
                                  ifelse((avg_training_score >= 55) & (avg_training_score <= 65), "55-65",
                                         ifelse((avg_training_score >= 65) & (avg_training_score <= 75), "65-75",
                                                ifelse((avg_training_score >= 75) & (avg_training_score <= 85), "75-85",
                                                       ifelse((avg_training_score >= 85) & (avg_training_score <= 95), "85-95", ">95"
                                                       )
                                                )
                                         )
                                  )
                           )
                    )
             )
      )
    ) 
    ) %>% 
    group_by(avg_training_score) %>% 
    count(avg_training_score) %>% 
    mutate(label = glue('Average Training Score:{avg_training_score}
                      Total Employees : {n}')) %>% 
    ggplot(aes(x=factor(avg_training_score, levels = c("<20","20-35","35-45","45-55","55-65",
                                                       "65-75","75-85","85-95", ">95")), y = n, fill = "#D90416", text = label))+
    geom_col(position = "dodge",fill="#D90416")+
    labs(x = NULL, y = "Total Employees", fill = "is_promoted")
  
  d5_plotly <- ggplotly(d5_viz, tooltip = "text")%>% add_annotations(
    text = "Average Training Score",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  d6_viz <- promote_train %>% 
    select(awards_won.) %>% 
    count(awards_won.) %>% 
    mutate(label = glue('Won any awards? :{awards_won.}
                      Total Employees : {n}')) %>% 
    ggplot(aes(x = reorder(awards_won.,n), y = n, fill = "#D90416", text = label))+
    geom_col(position = "dodge",fill="#D90416")+
    labs(x = NULL, y = "Total Employees", fill = "is_promoted")+
    scale_x_discrete(labels = wrap_format(10))+
    coord_flip()
  
  d6_plotly <- ggplotly(d6_viz, tooltip = "text")%>% add_annotations(
    text = "Won Any Awards?",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  s0 <- subplot(d6_plotly, d1_plotly, margin = 0.15, nrows = 2,shareY = T)
  s1 <- subplot(s0, d2_plotly,shareY = F, widths = c(0.4,0.6), margin = 0.03)
  s2 <- subplot(d3_plotly, d4_plotly, widths = c(0.4,0.6),shareY = F)
  s5 <- subplot(s1, s2, nrows = 2,margin = 0.08, heights = c(0.5,0.5)) %>%
    layout(showlegend = F,
           plot_bgcolor='#e5ecf6', 
           xaxis = list( 
             zerolinecolor = '#ffff', 
             zerolinewidth = 2, 
             gridcolor = 'ffff'), 
           yaxis = list( 
             zerolinecolor = '#ffff', 
             zerolinewidth = 2, 
             gridcolor = 'ffff'), hovermode = 'x')
  s5
  
})

output$var_imp_promote <- renderPlotly({
  var_imp_promote <- data.frame(varImp(promote_rf)$importance) %>% 
    rename(Importance = Overall)
  var_imp_promote$Variable <- rownames(var_imp_promote)
  rownames(var_imp_promote) <- NULL
  
  var_imp_plot <- var_imp_promote %>% head(10) %>% 
    mutate(Importance = round(Importance, 2),
           label = glue("Variable = {Variable}
                          Importance = {Importance}")) %>% 
    ggplot(aes(y = reorder(Variable,Importance), x = Importance, text = label)) +
    geom_col(fill = "#A60303")+
    labs(y = NULL) +
    scale_fill_viridis_c()+
    theme_minimal()+
    scale_y_discrete(labels = wrap_format(15))
  ggplotly(var_imp_plot, tooltip="text")
  
})

output$bagi_promote_or_not <- renderPlotly({
  
  plot_promote <- promoteClean %>% 
    select(promoted) %>% 
    count(promoted) %>% 
    mutate(label = glue("Promoted? : {promoted}
                          Total Employee : {n}")) 
  colors <- c('#D90416','#260101')
  fig <- plot_ly(plot_promote, labels = ~promoted, values = ~n, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#FFFFFF'),
                 hoverinfo = 'text',
                 text = ~paste('Total Employee : ', n),
                 marker = list(colors = colors,
                               line = list(color = '#FFFFFF', width = 1)),
                 showlegend = FALSE)
  fig <- fig %>% layout(
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  fig
})


   output$no_promote <- renderText({
     
     att_promote <- promoteClean %>% 
       filter(promoted == "Yes") %>% 
       filter(department %in% input$department_promote) 
     print(n_distinct(att_promote))
     
   })
   
   output$no_not_promoted <- renderText({
     
     att_promote <- promoteClean %>% 
       filter(promoted == "No") %>% 
       filter(department %in% input$department_promote) 
     print(n_distinct(att_promote))
     
   })
   
   output$PredOutput2 <-  renderDataTable({
     
     promoteClean$prob_promoted <- predict(object = promote_rf, newdata = promoteClean, type = "prob")[,input$promote_or_not]
     
     promoteClean <- promoteClean %>% 
       filter(promoted == input$promote_or_not) %>% 
       filter(department %in% input$department_promote) %>% 
       arrange(-prob_promoted) %>% 
       select(c(input$variable2))
     if(is.null(promoteClean)){
       return(NULL)
     }
     promoteClean
     
   }, options = list(lengthMenu = list(c(5,10,-1), c(5, 10, "All")), pageLength = 5, scrollX = TRUE))
   
   observe({
     att2 <- promoteClean %>% 
       filter(promoted == input$promote_or_not) %>% 
       filter(department %in% input$department_promote)
     updateSelectizeInput(session, inputId = "promote_id",
                       label = "Select Employee ID: (Max. 4)",
                       choices = unique(att2$employee_id),
                       selected = sample(unique(att2$employee_id),1),
                       options = list(maxItems = 4))
   })
   
   observe({
     promoteClean$prob_promoted <- predict(object = promote_rf, newdata = promoteClean, type = "prob")[,input$promote_or_not]
     
     updateSelectInput(
       inputId = "variable2",
       label = "Select Variable :",
       choices = promoteClean %>% select(-is_promoted) %>% names(), 
       selected = c("employee_id", "prob_promoted","promoted"))
   })
   
   output$individual_promote <- renderPlot({
     
     set.seed(100)
     explainer_promote <- explain(x = test_x_promote[c(input$promote_id),], 
                                  explainer = explain_promote, 
                                  n_labels = 1, 
                                  n_features = 10, 
                                  n_permutation = 20,
                                  dist_fun = "manhattan")
     plot_features(explainer_promote)
     
     
   })
   
   reactive_promote<-reactive({
     req(input$file_promote)
     data_test_promote <- read.csv(input$file_promote$datapath)
     data_test_promote <- data_test_promote %>%
       mutate(awards_won. = factor(awards_won., levels = c(0,1),labels = c("No", "Yes"))) %>% 
       na.omit()
    
     return(data_test_promote)
   })
   
   observe({
     data_test_promote <- reactive_promote()
     updateSelectizeInput(session, inputId = "promote_test_id",
                       label = "Select Employee ID: (Max. 4)",
                       choices = unique(data_test_promote$employee_id),
                       selected = sample(unique(data_test_promote$employee_id),1),
                       options = list(maxItems = 4))
   })
   
   output$data_Test_Promote <- renderDataTable({
     file_to_read_promote = input$file_promote
     if(is.null(file_to_read_promote)){
       return(NULL)
     }
     
     data_test_promote <- read.csv(file_to_read_promote$datapath)
     data_test_promote <- data_test_promote %>%
       mutate(awards_won. = factor(awards_won., levels=c(0,1), labels = c("No","Yes"))) %>% 
       na.omit()

     data_test_promote$promoted <- predict(object = promote_rf, newdata =data_test_promote, type = "raw")
     data_test_promote$Promoted_No <- predict(object = promote_rf, newdata = data_test_promote, type = "prob")[,1]
     data_test_promote$Promoted_Yes <- predict(object = promote_rf, newdata = data_test_promote, type = "prob")[,2]
     
     data_test_promote <- data_test_promote  %>% 
       arrange(-Promoted_Yes) 
     if(is.null(data_test_promote)){
       return(NULL)
     }
     data_test_promote
     
   }, options = list(lengthMenu = list(c(5,10,-1), c(5, 10, "All")), pageLength = 5, scrollX = TRUE))
   
   output$data_Test_Promote_example <- renderDataTable({
    promote_test <- promote_test_data %>%
       mutate(awards_won. = factor(awards_won., levels=c(0,1),labels = c("No","Yes"))) %>% 
       na.omit()
     
     promote_test$Promoted <- predict(object = promote_rf, newdata = promote_test, type = "raw")
     promote_test$Promoted_No <- predict(object = promote_rf, newdata = promote_test, type = "prob")[,1]
     promote_test$Promoted_Yes <- predict(object = promote_rf, newdata = promote_test, type = "prob")[,2]
     
     promote_test <- promote_test  %>% 
       arrange(-Promoted_Yes) 
     if(is.null(promote_test)){
       return(NULL)
     }
     promote_test
     
   }, options = list(lengthMenu = list(c(5,10,-1), c(5, 10, "All")), pageLength = 5, scrollX = TRUE))
   
   output$sim_test_promote <- renderPlotly({
     promote_test <- reactive_promote()
     promote_test$promoted <- predict(object = promote_rf, newdata = promote_test, type = "raw")
     
     plot_employ <- promote_test %>% 
       select(promoted) %>% 
       count(promoted) 
     colors <- c('#D90416','#260101')
     fig <- plot_ly(plot_employ, labels = ~promoted, values = ~n, type = 'pie',
                    textposition = 'inside',
                    textinfo = 'label+percent',
                    insidetextfont = list(color = '#FFFFFF'),
                    hoverinfo = 'text',
                    text = ~paste('Total Employee : ', n),
                    marker = list(colors = colors,
                                  line = list(color = '#FFFFFF', width = 1)),
                    showlegend = FALSE)
     fig <- fig %>% layout(
       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
     
     fig
   })
   
   output$sim_test_promote_example <- renderPlotly({
     promote_test_data <- promote_test_data %>%
       mutate(awards_won. = factor(awards_won., levels=c(0,1),labels = c("No","Yes"))) %>% 
       na.omit()
     promote_test_data$promoted <- predict(object = promote_rf, newdata = promote_test_data, type = "raw")
     
     plot_employ <- promote_test_data %>% 
       select(promoted) %>% 
       count(promoted)
     colors <- c('#D90416','#260101')
     fig <- plot_ly(plot_employ, labels = ~promoted, values = ~n, type = 'pie',
                    textposition = 'inside',
                    textinfo = 'label+percent',
                    insidetextfont = list(color = '#FFFFFF'),
                    hoverinfo = 'text',
                    text = ~paste('Total Employee : ', n),
                    marker = list(colors = colors,
                                  line = list(color = '#FFFFFF', width = 1)),
                    showlegend = FALSE)
     fig <- fig %>% layout(
       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
     
     fig
   })
   
   output$promote_test_plot <- renderPlot({
     data_test_promote <- reactive_promote()
     rownames(data_test_promote) <- data_test_promote$employee_id
     data_test_promote <- data_test_promote %>% select(c(department, region, education,
                                                         no_of_trainings, age, previous_year_rating, 
                                                         awards_won.,avg_training_score))
     
     explain_promote <- lime(x = data_test_promote, model = promote_rf)
     
     set.seed(100)
     explainer_test_promote <- explain(x = data_test_promote[input$promote_test_id,], 
                                   explainer = explain_promote, 
                                   n_labels = 1, 
                                   n_features = 10, 
                                   n_permutation = 20,
                                   dist_fun = "manhattan")
     plot_features(explainer_test_promote)
   })
   
   observe({
     promote_test_data <- promote_test_data  %>% 
       select(c(employee_id,department, region, education,
                no_of_trainings, age, previous_year_rating, 
                awards_won.,avg_training_score)) %>% 
       mutate(awards_won. = factor(awards_won., levels = c(0,1), labels=c("No","Yes")))
     updateSelectizeInput(session,"promote_test_id_example",
                      label = "Select Employee Number: (Max. 4)",
                      choices = unique(promote_test_data$employee_id),
                      selected = sample(unique(promote_test_data$employee_id),1), server = TRUE
     )
   })
   
   output$promote_test_plot_example <- renderPlot({
     rownames(promote_test_data) <- promote_test_data$employee_id
     promote_test_data <- promote_test_data  %>% 
       select(c(department, region, education,
              no_of_trainings, age, previous_year_rating, 
              awards_won.,avg_training_score)) %>% 
       mutate(awards_won. = factor(awards_won., levels = c(0,1), labels=c("No","Yes")))
     
     explain_promote <- lime(x = promote_test_data, model = promote_rf)
     
     set.seed(100)
     explainer_test_promote <- explain(x = promote_test_data[input$promote_test_id_example,], 
                                       explainer = explain_promote, 
                                       n_labels = 1, 
                                       n_features = 10, 
                                       n_permutation = 20,
                                       dist_fun = "manhattan")
     plot_features(explainer_test_promote)
   })
   
# ABSENTEEISM AT WORK
   output$absent_none <- renderText({
     
     absent <- absent_train %>% 
       filter(Absenteeism.time.in.hours == "None") %>% 
       n_distinct()
     paste0(absent," or ",round((absent/n_distinct(absent_train))*100),"%")
     
   })
   
   output$absent1_2 <- renderText({
     
     absent <- absent_train %>% 
       filter(Absenteeism.time.in.hours == "1-2 Hours") %>% 
       n_distinct()
     paste0(absent," or ",round((absent/n_distinct(absent_train))*100),"%")
     
   })
   
   output$absent3_4 <- renderText({
     
     absent <- absent_train %>% 
       filter(Absenteeism.time.in.hours == "3-4 Hours") %>% 
       n_distinct()
     paste0(absent," or ",round((absent/n_distinct(absent_train))*100),"%")
     
   })
   
   output$absent5_7 <- renderText({
     
     absent <- absent_train %>% 
       filter(Absenteeism.time.in.hours == "5-7 Hours") %>% 
       n_distinct()
     paste0(absent," or ",round((absent/n_distinct(absent_train))*100),"%")
     
   })
   
   output$absent_off <- renderText({
     
     absent <- absent_train %>% 
       filter(Absenteeism.time.in.hours == "Off Work") %>% 
       n_distinct()
     paste0(absent," or ",round((absent/n_distinct(absent_train))*100),"%")
     
   })
   
  output$time <- renderPlotly({
    data_viz <- absent_train %>% 
      filter(Absenteeism.time.in.hours == "None") %>% 
      select(Month.of.absence) %>% 
      group_by(Month.of.absence) %>% 
      count() %>% 
      mutate(label = glue('Month :{Month.of.absence}
                      Total Employees in Office : {n}'))%>% 
      ggplot(aes(x=Month.of.absence,y=n,group = 1,color="#A60303", text = label))+
      geom_line(color="#A60303")+
      geom_point(color="#A60303", alpha = 0.8)+
      labs(x = NULL, y = "Total Employees in Office", color = "Absenteeism", alpha = NULL)
    
    d1_plotly <- ggplotly(data_viz, tooltip = "text")%>% add_annotations(
      text = "Month",
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(size = 15)
    )
    
    data_viz2 <- absent_train %>% 
      filter(Absenteeism.time.in.hours == "None") %>% 
      select(Day.of.the.week) %>% 
      group_by(Day.of.the.week) %>% 
      count() %>% 
      mutate(label = glue('Day :{Day.of.the.week}
                      Total Employees in Office : {n}'))%>% 
      ggplot(aes(x=Day.of.the.week,y=n,group=1,color="#A60303", text = label))+
      geom_line(color="#A60303")+
      geom_point(color="#A60303", alpha = 0.8)+
      labs(x = NULL, y = "Total Employees in Office", color = "Absenteeism")
    
    
    d2_plotly <- ggplotly(data_viz2, tooltip = "text")%>% add_annotations(
      text = "Day of Week",
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(size = 15)
    )
    
    data_viz3 <- absent_train %>% 
      filter(Absenteeism.time.in.hours == "None") %>% 
      select(Seasons) %>% 
      group_by(Seasons) %>% 
      count() %>% 
      mutate(label = glue('Season :{Seasons}
                      Total Employees in Office: {n}'))%>% 
      ggplot(aes(x=Seasons,y=n, group = 1,color="#A60303", text = label))+
      geom_line(color="#A60303")+
      geom_point(color="#A60303", alpha = 0.8)+
      labs(x = NULL, y = "Total Employees", color = "Absenteeism")
    d3_plotly <- ggplotly(data_viz3, tooltip = "text")%>% add_annotations(
      text = "Season",
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(size = 15)
    )
    
    s1 <- subplot(d3_plotly,d2_plotly, widths = c(0.4,0.6))
    s5 <- subplot(d1_plotly, s1, nrows = 2, margin = 0.08) %>%
      layout(showlegend = F,
             plot_bgcolor='#e5ecf6', 
             xaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'), 
             yaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'), hovermode = 'x')
    s5
  }) 
   
output$demography_absent <- renderPlotly({
    data_viz <- absent_train %>% 
      select(Age) %>% 
      mutate(Age = as.factor(
        ifelse(Age < 20, "18-19",
               ifelse((Age >= 20) & (Age <= 25), "20-25",
                      ifelse((Age >= 26) & (Age <= 30), "26-30",
                             ifelse((Age >= 31) & (Age <= 35), "31-35",
                                    ifelse((Age >= 36) & (Age <= 40), "36-40",
                                           ifelse((Age >= 41) & (Age <= 45), "41-45",
                                                  ifelse((Age >= 46) & (Age <= 50), "46-50",
                                                         ifelse((Age >= 51) & (Age <= 55), "51-55", ">55"
                                                         )
                                                  )
                                           )
                                    )
                             )
                      )
               )
        )
      ) 
      ) %>% 
      count(Age) %>%
      mutate(label = glue('Age :{Age}
                      Total Employee : {n}'))%>% 
      ggplot(aes(y=reorder(Age,n),x=n,  text = label))+
      geom_col(position = "dodge",fill="#D90416")+
      labs(y = NULL, x = "Total Employees", fill = "Absenteeism")
    
    d1_plotly <- ggplotly(data_viz, tooltip = "text")%>% add_annotations(
      text = "Age",
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(size = 15)
    )
    
    data_viz2 <- absent_train %>% 
      select(Education) %>% 
      group_by(Education) %>% 
      count() %>% 
      mutate(label = glue('Education :{Education}
                      Total Employees : {n}'))%>% 
      ggplot(aes(y= reorder(Education,n),x = n,text = label))+
      geom_col(position = "dodge",fill="#D90416")+
      labs(y = NULL, x = "Total Employees", fill = "Absenteeism")+
      scale_y_discrete(labels = wrap_format(15))
    d2_plotly <- ggplotly(data_viz2, tooltip = "text")%>% add_annotations(
      text = "Education",
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(size = 15)
    )
    
    s5 <- subplot(d1_plotly, d2_plotly, nrows = 2,margin = 0.08) %>%
      layout(showlegend = F,
             plot_bgcolor='#e5ecf6', 
             xaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'), 
             yaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'), hovermode = 'x')
    s5
  }) 

output$absence_history <- renderPlotly({
  data_viz <- absent_train %>% 
    select(Absenteeism.time.in.hours, Reason.for.absence) %>% 
    group_by(Reason.for.absence,Absenteeism.time.in.hours) %>% 
    count() %>% 
    mutate(label = glue('Reason for absence :{Reason.for.absence}
                      Absenteeism : {Absenteeism.time.in.hours}
                      Total Employee : {n}'))%>% 
    ggplot(aes(y=Reason.for.absence,x=n, fill = Absenteeism.time.in.hours, text = label))+
    geom_col(position = "dodge")+
    labs(x = NULL, y = "Total Employees", fill = "Absenteeism")+
    scale_y_discrete(labels = wrap_format(100))
  
  d1_plotly <- ggplotly(data_viz, tooltip = "text")%>% add_annotations(
    text = "Reason for absence",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15), hovermode = 'x'
  )
  
  d1_plotly
}) 

output$employee_profile <- renderPlotly({
  
  data_viz1 <- absent_train %>% 
    select(Body.mass.index)  %>% 
    ggplot(aes(x=Body.mass.index, text = Body.mass.index))+
    geom_density(fill="#D90416")+
    labs(x = NULL, y=NULL, fill = "Absenteeism")
  d1_plotly <- ggplotly(data_viz1, tooltip = "text")%>% add_annotations(
    text = "Body Mass Index",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  data_viz2 <- absent_train %>% 
    select(Social.drinker) %>% 
    group_by(Social.drinker) %>% 
    count() %>% 
    mutate(label = glue('Social drinker? :{Social.drinker}
                      Total Employees : {n}'))%>% 
    ggplot(aes(x=reorder(Social.drinker,-n),y=n, text = label))+
    geom_col(position = "dodge",fill="#D90416")+
    labs(x = NULL, y = "Total Employees", fill = "Absenteeism")
  d2_plotly <- ggplotly(data_viz2, tooltip = "text")%>% add_annotations(
    text = "Social drinker",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  data_viz3 <- absent_train %>% 
    select(Social.smoker) %>% 
    group_by(Social.smoker) %>% 
    count() %>% 
    mutate(label = glue('Social smoker? :{Social.smoker}
                      Total Employees : {n}'))%>% 
    ggplot(aes(x=Social.smoker,y=n, text = label))+
    geom_col(position = "dodge", fill = "#D90416")+
    labs(x = NULL, y = "Total Employees", fill = "Absenteeism")
  d3_plotly <- ggplotly(data_viz3, tooltip = "text")%>% add_annotations(
    text = "Social smoker",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  data_viz4 <- absent_train %>% 
    select(Weight) %>% 
    ggplot(aes(x=Weight, text = Weight))+
    geom_density(fill="#D90416")+
    labs(x = "Weight", y = "Total Employees", color = "Absenteeism")
  d4_plotly <- ggplotly(data_viz4, tooltip = "text")%>% add_annotations(
    text = "Weight",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  data_viz5 <- absent_train %>% 
    select( Height) %>% 
    ggplot(aes(x=Height, text = Height))+
    geom_density(fill = "#D90416")+
    labs(x = "Height", y = "Total Employees", fill = "Absenteeism")
  d5_plotly <- ggplotly(data_viz5, tooltip = "text")%>% add_annotations(
    text = "Height",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  data_viz6 <- absent_train %>% 
    select(Distance.from.Residence.to.Work) %>% 
    ggplot(aes(x=Distance.from.Residence.to.Work,text = Distance.from.Residence.to.Work))+
    geom_density(fill = "#D90416")+
    labs( fill = "Absenteeism")
  d6_plotly <- ggplotly(data_viz6, tooltip = "text")%>% add_annotations(
    text = "Distance from Residence",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  data_viz7 <- absent_train %>% 
    select(Son) %>% 
    group_by(Son) %>% 
    count() %>% 
    mutate(label = glue('Total Children :{Son}
                      Total Employees : {n}'))%>% 
    ggplot(aes(x=Son,y=n, text = label))+
    geom_col(position = "dodge", fill = "#D90416")+
    labs(x = NULL, y = "Total Employees", fill = "Absenteeism")
  d7_plotly <- ggplotly(data_viz7, tooltip = "text")%>% add_annotations(
    text = "Total Children",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  data_viz8 <- absent_train %>% 
    select(Pet) %>% 
    group_by(Pet) %>% 
    count() %>% 
    mutate(label = glue('Total Pet :{Pet}
                      Total Employees : {n}'))%>% 
    ggplot(aes(x=Pet,y=n, text = label))+
    geom_col(position = "dodge", fill = "#D90416")+
    labs(x = NULL, y = "Total Employees", fill = "Absenteeism")
  d8_plotly <- ggplotly(data_viz8, tooltip = "text")%>% add_annotations(
    text = "Total Pet",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  data_viz9 <- absent_train %>% 
    select( Transportation.expense) %>% 
    ggplot(aes(x=Transportation.expense, text = Transportation.expense))+
    geom_density(fill = "#D90416")+
    labs(x = NULL, y = "Total Employees", fill = "Absenteeism")
  d9_plotly <- ggplotly(data_viz9, tooltip = "text")%>% add_annotations(
    text = "Transportation Expense",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  data_viz10 <- absent_train %>% 
    select(Work.load.Average.day) %>% 
    ggplot(aes(x=Work.load.Average.day,text =Work.load.Average.day))+
    geom_density(fill = "#D90416")+
    labs( fill = "Absenteeism")
  d10_plotly <- ggplotly(data_viz10, tooltip = "text")%>% add_annotations(
    text = "Work load Average day",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  data_viz11 <- absent_train %>% 
    select(Disciplinary.failure) %>% 
    group_by(Disciplinary.failure) %>% 
    count() %>% 
    mutate(label = glue('Disciplinary failure? :{Disciplinary.failure}
                      Total Employees : {n}'))%>% 
    ggplot(aes(x=Disciplinary.failure,y=n, text = label))+
    geom_col(position = "dodge", fill = "#D90416")+
    labs(x = NULL, y = "Total Employees", fill = "Absenteeism")
  d11_plotly <- ggplotly(data_viz11, tooltip = "text")%>% add_annotations(
    text = "Disciplinary failure",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  data_viz12 <- absent_train %>% 
    select(Hit.target) %>%  
    ggplot(aes(x=Hit.target, text = Hit.target))+
    geom_density(fill = "#D90416")+
    labs(x = NULL, y = "Total Employees", fill = "Absenteeism")
  d12_plotly <- ggplotly(data_viz12, tooltip = "text")%>% add_annotations(
    text = "Hit Target",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  data_viz13 <- absent_train %>% 
    select(Service.time) %>% 
    ggplot(aes(x=Service.time,text = Service.time))+
    geom_density(fill = "#D90416")+
    labs(x = NULL, y = "Total Employees", fill = "Absenteeism")
  d13_plotly <- ggplotly(data_viz13, tooltip = "text")%>% add_annotations(
    text = "Service time",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  s1 <- subplot(d1_plotly,d4_plotly,d5_plotly, d10_plotly,nrows = 4, margin = 0.06)
  s2 <- subplot(d9_plotly,d6_plotly,d12_plotly, d13_plotly,nrows = 4, margin = 0.06)
  s3 <- subplot(d3_plotly,d2_plotly,d11_plotly,shareY = T)
  s4 <- subplot(d8_plotly,d7_plotly,shareY = T)
  s5 <- subplot(s3,s4,nrows = 2, margin = 0.05)
  s6 <- subplot(s1,s2,s5,margin = 0.02,widths = c(0.2,0.2,0.6)) %>%
    layout(showlegend = F,
           plot_bgcolor='#e5ecf6', 
           xaxis = list( 
             zerolinecolor = '#ffff', 
             zerolinewidth = 2, 
             gridcolor = 'ffff'), 
           yaxis = list( 
             zerolinecolor = '#ffff', 
             zerolinewidth = 2, 
             gridcolor = 'ffff'), hovermode = 'x')
  s6
}) 

output$about_employee <- renderPlotly({
  
  data_viz1 <- absent_train %>% 
    select(Distance.from.Residence.to.Work) %>% 
    ggplot(aes(x=Distance.from.Residence.to.Work,text = Distance.from.Residence.to.Work))+
    geom_density(fill="#D90416")+
    labs( fill = "Absenteeism")
  d1_plotly <- ggplotly(data_viz1, tooltip = "text")%>% add_annotations(
    text = "Distance from Residence",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
 
  data_viz2 <- absent_train %>% 
    select(Absenteeism.time.in.hours, Son) %>% 
    group_by(Son,Absenteeism.time.in.hours) %>% 
    count() %>% 
    mutate(label = glue('Total Children :{Son}
                      Absenteeism : {Absenteeism.time.in.hours}
                      Total Employees : {n}'))%>% 
    ggplot(aes(x=Son,y=n, fill = Absenteeism.time.in.hours, text = label))+
    geom_col(position = "dodge")+
    labs(x = NULL, y = "Total Employees", fill = "Absenteeism")
  d2_plotly <- ggplotly(data_viz2, tooltip = "text")%>% add_annotations(
    text = "Total Children",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  data_viz3 <- absent_train %>% 
    select(Absenteeism.time.in.hours, Pet) %>% 
    group_by(Pet,Absenteeism.time.in.hours) %>% 
    count() %>% 
    mutate(label = glue('Total Pet :{Pet}
                      Absenteeism : {Absenteeism.time.in.hours}
                      Total Employees : {n}'))%>% 
    ggplot(aes(x=Pet,y=n, fill = Absenteeism.time.in.hours, text = label))+
    geom_col(position = "dodge")+
    labs(x = NULL, y = "Total Employees", fill = "Absenteeism")
  d3_plotly <- ggplotly(data_viz3, tooltip = "text")%>% add_annotations(
    text = "Total Pet",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  data_viz4 <- absent_train %>% 
    select(Absenteeism.time.in.hours, Transportation.expense) %>% 
    group_by(Transportation.expense,Absenteeism.time.in.hours) %>% 
    count() %>% 
    mutate(label = glue('Transportation Expense :{Transportation.expense}
                      Absenteeism : {Absenteeism.time.in.hours}
                      Total Employees : {n}'))%>% 
    ggplot(aes(x=Transportation.expense,group = Absenteeism.time.in.hours, fill = Absenteeism.time.in.hours, text = Absenteeism.time.in.hours))+
    geom_density(alpha=0.4)+
    labs(x = NULL, y = "Total Employees", fill = "Absenteeism")
  d4_plotly <- ggplotly(data_viz4, tooltip = "text")%>% add_annotations(
    text = "Transportation Expense",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  s1 <- subplot(d1_plotly,d4_plotly, nrows = 2, margin = 0.05)
  s2 <- subplot(d2_plotly,d3_plotly)
  s5 <- subplot(s1,s2,nrows = 2,heights = c(0.6,0.4),margin = 0.02) %>%
    layout(showlegend = F,
           plot_bgcolor='#e5ecf6', 
           xaxis = list( 
             zerolinecolor = '#ffff', 
             zerolinewidth = 2, 
             gridcolor = 'ffff'), 
           yaxis = list( 
             zerolinecolor = '#ffff', 
             zerolinewidth = 2, 
             gridcolor = 'ffff'), hovermode = 'x')
  s5
}) 

output$experience_absent <- renderPlotly({
  
  data_viz1 <- absent_train %>% 
    select(Absenteeism.time.in.hours, Work.load.Average.day) %>% 
    group_by(Work.load.Average.day,Absenteeism.time.in.hours) %>% 
    count() %>% 
    mutate(label = glue('Work load Average day :{Work.load.Average.day}
                      Absenteeism : {Absenteeism.time.in.hours}'))%>% 
    ggplot(aes(x=Work.load.Average.day,group = Absenteeism.time.in.hours, fill = Absenteeism.time.in.hours, text = Absenteeism.time.in.hours))+
    geom_density(alpha=0.4)+
    labs( fill = "Absenteeism")
  d1_plotly <- ggplotly(data_viz1, tooltip = "text")%>% add_annotations(
    text = "Work load Average day",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  data_viz2 <- absent_train %>% 
    select(Absenteeism.time.in.hours, Disciplinary.failure) %>% 
    group_by(Disciplinary.failure,Absenteeism.time.in.hours) %>% 
    count() %>% 
    mutate(label = glue('Disciplinary failure? :{Disciplinary.failure}
                      Absenteeism : {Absenteeism.time.in.hours}
                      Total Employees : {n}'))%>% 
    ggplot(aes(x=Disciplinary.failure,y=n, fill = Absenteeism.time.in.hours, text = label))+
    geom_col(position = "dodge")+
    labs(x = NULL, y = "Total Employees", fill = "Absenteeism")
  d2_plotly <- ggplotly(data_viz2, tooltip = "text")%>% add_annotations(
    text = "Disciplinary failure",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  data_viz3 <- absent_train %>% 
    select(Absenteeism.time.in.hours, Hit.target) %>% 
    group_by(Hit.target,Absenteeism.time.in.hours) %>% 
    count() %>% 
    mutate(label = glue('Hit target :{Hit.target}
                      Absenteeism : {Absenteeism.time.in.hours}
                      Total Employees : {n}'))%>% 
    ggplot(aes(x=Hit.target,group = Absenteeism.time.in.hours, fill = Absenteeism.time.in.hours, text = Absenteeism.time.in.hours))+
    geom_density(alpha=0.4)+
    labs(x = NULL, y = "Total Employees", fill = "Absenteeism")
  d3_plotly <- ggplotly(data_viz3, tooltip = "text")%>% add_annotations(
    text = "Hit Target",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  data_viz4 <- absent_train %>% 
    select(Absenteeism.time.in.hours, Service.time) %>% 
    mutate(label = glue('Service time :{Service.time}
                      Absenteeism : {Absenteeism.time.in.hours}'))%>% 
    ggplot(aes(x=Service.time,group = Absenteeism.time.in.hours, fill = Absenteeism.time.in.hours, text = label))+
    geom_density(alpha=0.4)+
    labs(x = NULL, y = "Total Employees", fill = "Absenteeism")
  d4_plotly <- ggplotly(data_viz4, tooltip = "text")%>% add_annotations(
    text = "Service time",
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  
  s5 <- subplot(d1_plotly,d4_plotly,d3_plotly,d2_plotly, nrows = 4, margin = 0.05) %>%
    layout(showlegend = F,
           plot_bgcolor='#e5ecf6', 
           xaxis = list( 
             zerolinecolor = '#ffff', 
             zerolinewidth = 2, 
             gridcolor = 'ffff'), 
           yaxis = list( 
             zerolinecolor = '#ffff', 
             zerolinewidth = 2, 
             gridcolor = 'ffff'), hovermode = 'x')
  s5
}) 
  
   
   # Absence Prediction
   
   output$var_imp_absent <- renderPlotly({
     var_imp_absent <- data.frame(varImp(absent_rf)$importance) %>% 
       rename(Importance = Overall)
     var_imp_absent$Variable <- rownames(var_imp_absent)
     rownames(var_imp_absent) <- NULL
     
     var_imp_plot <- var_imp_absent %>% head(5) %>% 
       mutate(Importance = round(Importance, 2),
              label = glue("Variable = {Variable}
                          Importance = {Importance}")) %>% 
       ggplot(aes(y = reorder(Variable,Importance), x = Importance, text = label)) +
       geom_col(fill = "#A60303")+
       labs(y = NULL) +
       scale_fill_viridis_c()+
       theme_minimal()+
       scale_y_discrete(labels = wrap_format(35))
     ggplotly(var_imp_plot, tooltip="text")
     
   })
   
   output$bagi_absent_or_not <- renderPlotly({
     
     plot_absent <- absent_train %>% 
       select(pred_absent) %>% 
       count(pred_absent) %>% 
       mutate(label = glue("Absenteeism? : {pred_absent}
                          Total Employee : {n}")) 
     colors <- c('#D90416','#590202','#A60303','#F20505','#260101')
     fig <- plot_ly(plot_absent, labels = ~pred_absent, values = ~n, type = 'pie',
                    textposition = 'inside',
                    textinfo = 'label+percent',
                    insidetextfont = list(color = '#FFFFFF'),
                    hoverinfo = 'text',
                    text = ~glue("Absenteeism? : {pred_absent}
                          Total Employee : {n}"),
                    marker = list(colors = colors,
                                  line = list(color = '#FFFFFF', width = 1)),
                    showlegend = FALSE)
     fig <- fig %>% layout(
       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
     
     fig
     
   })
   
   output$absent_pred_none <- renderText({
     
     abs <- absent_train %>% 
       filter(Absenteeism.time.in.hours == "None") %>% 
       filter(Month.of.absence %in% input$month_pred) 
     print(n_distinct(abs))
     
   })
   
   output$absent_pred_1_2 <- renderText({
     
     abs <- absent_train %>% 
       filter(Absenteeism.time.in.hours == "1-2 Hours") %>% 
       filter(Month.of.absence %in% input$month_pred) 
     print(n_distinct(abs))
     
   })
   
   output$absent_pred_3_4 <- renderText({
     
     abs <- absent_train %>% 
       filter(Absenteeism.time.in.hours == "3-4 Hours") %>% 
       filter(Month.of.absence %in% input$month_pred) 
     print(n_distinct(abs))
     
   })
   
   output$absent_pred_5_7 <- renderText({
     
     abs <- absent_train %>% 
       filter(Absenteeism.time.in.hours == "5-7 Hours") %>% 
       filter(Month.of.absence %in% input$month_pred) 
     print(n_distinct(abs))
     
   })
   
   output$absent_pred_off <- renderText({
     
     abs <- absent_train %>% 
       filter(Absenteeism.time.in.hours == "Off Work") %>% 
       filter(Month.of.absence %in% input$month_pred) 
     print(n_distinct(abs))
     
   })
   
   observe({
     
     absent_train$prob_absent <- predict(object = absent_rf, newdata = absent_train, type = "prob")[,input$absent_or_not]
     
     updateSelectInput(
       inputId = "variable_absent",
       label = "Select Variable :",
       choices = names(absent_train), 
       selected = c("absent_id","ID", "Absenteeism.time.in.hours", "prob_absent"))
   })
   
   output$PredOutputAbsent <-  renderDataTable({
     
     absent_train$prob_absent <- predict(object = absent_rf, newdata = absent_train, type = "prob")[,input$absent_or_not]
     
     absent_train <- absent_train %>% 
       filter(Absenteeism.time.in.hours == input$absent_or_not) %>% 
       filter(Month.of.absence %in% input$month_pred) %>% 
       arrange(-prob_absent) %>% 
       select(c(input$variable_absent))
     
     if(is.null(absent_train)){
       return(NULL)
     }
     absent_train
     
   },
   extensions = "Buttons", options = list(lengthMenu = list(c(5,10,-1), c(5, 10, "All")), pageLength = 5, scrollX = TRUE
                                          ))
   
   
   observe({
     abs <- absent_train %>% 
       filter(Absenteeism.time.in.hours == input$absent_or_not) %>% 
       filter(Month.of.absence %in% input$month_pred)
     updateSelectizeInput(session, inputId = "absent_id",
                          label = "Select Absent ID: (Max. 4)",
                          choices = unique(abs$absent_id),
                          selected = sample(unique(abs$absent_id),1),
                          options = list(maxItems = 4))
   })
   
   output$individual_absent <- renderPlot({
     set.seed(100)
     explainer_absent <- explain(x = test_x_absent[c(input$absent_id),],
                                 explainer = explain_absent,
                                 n_labels = 1,
                                 n_features = 10,
                                 n_permutation = 20,
                                 dist_fun = "manhattan")
     
     
     plot_features(explainer_absent)
     
   })
   
     
     reactive_absent<-reactive({
       req(input$file_absent)
       data_test_absent <- read.csv(input$file_absent$datapath)
       data_test_absent$absent_id <- rownames(data_test_absent)
       return(data_test_absent)
     })
     
     observe({
       data_test_absent <- reactive_absent()
       updateSelectizeInput(session, inputId = "absent_test_id",
                         label = "Select Absent ID: (Max. 4)",
                         choices = unique(data_test_absent$absent_id),
                         selected = sample(unique(data_test_absent$absent_id),1),
                         options = list(maxItems = 4))
     })
     
     
     output$data_Test_Absent <- renderDataTable({
       file_to_read_absent = input$file_absent
       if(is.null(file_to_read_absent)){
         return(NULL)
       }
       
       data_test_absent <- read.csv(input$file_absent$datapath)
       data_test_absent$Absent <- predict(object = absent_rf, newdata = data_test_absent, type = "raw")
       data_test_absent$pred_absent_None <- predict(object = absent_rf, newdata = data_test_absent, type = "prob")[,1]
       data_test_absent$pred_absent_1_2Hours <- predict(object = absent_rf, newdata = data_test_absent, type = "prob")[,2]
       data_test_absent$pred_absent_3_4Hours <- predict(object = absent_rf, newdata = data_test_absent, type = "prob")[,3]
       data_test_absent$pred_absent_5_7Hours <- predict(object = absent_rf, newdata = data_test_absent, type = "prob")[,4]
       data_test_absent$pred_absent_OffWork <- predict(object = absent_rf, newdata = data_test_absent, type = "prob")[,5]
       
       data_test_absent <- data_test_absent %>% 
         arrange(-pred_absent_5_7Hours)
       if(is.null(data_test_absent)){
         return(NULL)
       }
       data_test_absent
       
     }, options = list(lengthMenu = list(c(5,10,-1), c(5, 10, "All")), pageLength = 5, scrollX = TRUE))
     
 output$data_Test_Absent_example <- renderDataTable({
       
       data_test_absent <- absent_test
       
       data_test_absent$Absent <- predict(object = absent_rf, newdata = data_test_absent, type = "raw")
       data_test_absent$pred_absent_None <- predict(object = absent_rf, newdata = data_test_absent, type = "prob")[,1]
       data_test_absent$pred_absent_1_2Hours <- predict(object = absent_rf, newdata = data_test_absent, type = "prob")[,2]
       data_test_absent$pred_absent_3_4Hours <- predict(object = absent_rf, newdata = data_test_absent, type = "prob")[,3]
       data_test_absent$pred_absent_5_7Hours <- predict(object = absent_rf, newdata = data_test_absent, type = "prob")[,4]
       data_test_absent$pred_absent_OffWork <- predict(object = absent_rf, newdata = data_test_absent, type = "prob")[,5]
       
       data_test_absent <- data_test_absent %>% 
         arrange(-pred_absent_5_7Hours)
       if(is.null(data_test_absent)){
         return(NULL)
       }
       data_test_absent
       
     }, options = list(lengthMenu = list(c(5,10,-1), c(5, 10, "All")), pageLength = 5, scrollX = TRUE))
     
 output$sim_test_absent <- renderPlotly({
   promote_test <- reactive_absent()
   promote_test$Absent <- predict(object = absent_rf, newdata = promote_test, type = "raw")
   
   plot_employ <- promote_test %>% 
     select(Absent) %>% 
     count(Absent) 
   colors <- c('#D90416','#590202','#A60303','#F20505','#260101')
   fig <- plot_ly(plot_employ, labels = ~Absent, values = ~n, type = 'pie',
                  textposition = 'inside',
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',
                  text = ~glue("Absenteeism? : {Absent}
                          Total Employee : {n}"),
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 1)),
                  showlegend = FALSE)
   fig <- fig %>% layout(
     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
   
   fig
 })
 
 output$sim_test_absent_example <- renderPlotly({
   promote_test_data <- absent_test
   promote_test_data$Absent <- predict(object = absent_rf, newdata = promote_test_data, type = "raw")
   
   plot_employ <- promote_test_data %>% 
     select(Absent) %>% 
     count(Absent)
   colors <- c('#D90416','#590202','#A60303','#F20505','#260101')
   fig <- plot_ly(plot_employ, labels = ~Absent, values = ~n, type = 'pie',
                  textposition = 'inside',
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',
                  text = ~glue("Absenteeism? : {Absent}
                          Total Employee : {n}"),
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 1)),
                  showlegend = FALSE)
   fig <- fig %>% layout(
     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
   
   fig
 })
 
 output$absent_test_plot <- renderPlot({
       data_test_absent <- reactive_absent()
       data_test_absent <- data_test_absent %>% 
         select(c(Reason.for.absence,
                   Day.of.the.week, Distance.from.Residence.to.Work, Age, Month.of.absence,
                   Disciplinary.failure, Education, Son, Social.smoker, Body.mass.index))
         
       explain_absent <- lime(x = data_test_absent, model = absent_rf)
       
       set.seed(100)
       explainer_test_absent <- explain(x = data_test_absent[c(input$absent_test_id),], 
                                         explainer = explain_absent, 
                                         n_labels = 1, 
                                         n_features = 10, 
                                         n_permutation = 20,
                                         dist_fun = "manhattan")
       plot_features(explainer_test_absent)
     })
     
     observe({
       absent_test$absent_id <- rownames(absent_test) 
       updateSelectizeInput(session, inputId = "absent_test_id_example",
                            label = "Select Absent ID: (Max. 4)",
                            choices = unique(absent_test$absent_id),
                            selected = sample(unique(absent_test$absent_id),1),
                            options = list(maxItems = 4))
     })
     
     output$absent_test_plot_example<- renderPlot({
       absent_test <- absent_test %>% 
         select(c(Reason.for.absence,
                   Day.of.the.week, Distance.from.Residence.to.Work, Age, Month.of.absence,
                   Disciplinary.failure, Education, Son, Social.smoker, Body.mass.index)) 
       
       explain_absent <- lime(x = absent_test, model = absent_rf)
       set.seed(100)
       explainer_test_absent <- explain(x = absent_test[c(input$absent_test_id_example),], 
                                        explainer = explain_absent, 
                                        n_labels = 1, 
                                        n_features = 10, 
                                        n_permutation = 20,
                                        dist_fun = "manhattan")
       plot_features(explainer_test_absent)
     })
   
})
