library(shiny)

ui <- navbarPage(title = "People Analytics", id = "inTabset",
           theme = bs_theme(
                            version = 4, 
                            bg = "white",
                            fg = "black",
                            primary = "maroon",
                            secondary = "maroon",
                            success = "green",
                            base_font = font_google("BIZ UDPGothic")
                            
                            
           ),
           # TAB 1 -------------------------------------------------------------------
      tabPanel(title = "Home",
                           tags$head(
                             tags$style(HTML("
                                  @media screen and (max-width: 1020px){
                                        .carousel-caption h1 {
                                          font-size : 20px !important;
                                        }
                                        .carousel-caption p {
                                            font-size : 15px !important;
                                        }
                                        .carousel-caption h1 br {
                                              display: none;
                                        }
                                        .carousel-caption button {
                                                white-space: normal;
                                                margin-top: 1vw;
                                                padding:20px;
                                        }
                                        .carousel-inner button{
                                              height: 50%;
                                            }
                                  }
                                   
                                  @media screen and (max-width: 849px){
                                        .carousel-caption h1 {
                                          font-size : 15px !important;
                                        }
                                        .carousel-caption p {
                                            font-size : 10px !important;
                                        }
                                        .carousel-caption h1 br {
                                              display: none;
                                        }
                                        .carousel-inner button{
                                              height: 50%;
                                        }
                                  }
                                   
                                  @media screen and (max-width: 715px){
                                        .carousel-caption {
                                          bottom: 50px !important;
                                        }
                                        .carousel-caption h1 {
                                          font-size : 10px !important;
                                        }
                                        .carousel-caption p {
                                            font-size : 5px !important;
                                        }
                                        .carousel-caption h1 br{
                                              display: none;
                                        }
                                        .carousel-inner button{
                                              height: 50%;
                                        }
                                  }
                                  
                                  @media screen and (max-width: 663px){
                                        .carousel-caption {
                                          bottom: 50px !important;
                                        }
                                        .carousel-caption h1 {
                                          font-size : 10px !important;
                                        }
                                        .carousel-caption p {
                                            font-size : 5px !important;
                                        }
                                        .carousel-caption h5 br{
                                              display: none;
                                        }
                                        .carousel-caption h1 br{
                                              display: none;
                                        }
                                        .carousel-caption h3 br{
                                              display: none;
                                        }
                                        .carousel-inner .button{
                                              height: 5px;
                                        }
                                   }
                                   
                                @media screen and (max-width: 370px){
                                        .carousel-caption {
                                          bottom: 50px !important;
                                        }
                                        .carousel-caption h1 {
                                          font-size : 6px !important;
                                        }
                                        .carousel-caption p {
                                            font-size : 3px !important;
                                        }
                                        .carousel-caption br {
                                              display: none;
                                        }
                                        .carousel-inner .button{
                                              height: 2px;
                                        }
                                }
                                @media screen and (max-width: 320px){
                                     .carousel-caption {
                                       bottom: 50px !important;
                                     }
                                     .carousel-caption h1 {
                                       font-size : 5px !important;
                                     }
                                     .carousel-caption p {
                                         font-size : 2px !important;
                                     }
                                     .carousel-caption br {
                                          display: none;
                                     }
                                     .carousel-caption h3 br {
                                          display: none;
                                     }
                                     .carousel-caption h5 br {
                                          display: none;
                                     }
                                     .carousel-inner .button{
                                          height: 2px;
                                     }
                                }
                                   
                                @media screen and (max-width: 258px){
                                      .carousel-caption {
                                        bottom: 50px !important;
                                      }
                                      .carousel-caption h1 {
                                        font-size : 4px !important;
                                      }
                                      .carousel-caption p {
                                          font-size : 2px !important;
                                      }
                                      .carousel-caption h3 br {
                                            display: none;
                                      }
                                      .carousel-caption h5 br {
                                            display: none;
                                      }
                                      .carousel-caption h1 br {
                                            display: none;
                                      }
                                      .carousel-caption br {
                                            display: none;
                                      }
                                      .carousel-caption button{
                                            top:50%;
                                            height:5px !important;
                                      }
                                      .carousel-indicators {
                                                top: -5em;
                                      }
                                 }
                                 @media screen and (max-width: 1180px){
                                      .modal-body { 
                                            max-height: 100%;
                                            overflow: auto;
                                            font-size: 100% !important;
                                      }
                                      .modal-body img {
                                            max-width: 100%; height: auto;
                                      }
                                      .modal-body img br {
                                            display: none;
                                      }
                                      .modal-content br {
                                            display: none;
                                      }
                                 }"
                                 ))),
                           
                           tags$div( id="carouselExampleIndicators", class="carousel slide", "data-ride"="carousel",
                             tags$ol(class="carousel-indicators",
                             tags$li( "data-target"="#carouselExampleIndicators", "data-slide-to"="0", class="active"),
                             tags$li("data-target"="#carouselExampleIndicators", "data-slide-to"="1"),
                             tags$li( "data-target"="#carouselExampleIndicators" ,"data-slide-to"="2"),
                             tags$li( "data-target"="#carouselExampleIndicators" ,"data-slide-to"="3"),
                             tags$li( "data-target"="#carouselExampleIndicators" ,"data-slide-to"="4")),
                             
                             tags$div(class="carousel-inner",
                                      tags$div(class="carousel-item active",
                                               tags$img(class="d-block w-100", src="0.jpg", alt="First slide"),
                                               tags$div(class = "carousel-caption",
                                                        h1("Welcome to","People Analytics Dashboard!", align = "center"),
                                                        p("People analytics is a data-driven approach to managing people at work. Instead of (or in addition to) relying on gut feeling, 
                                            people analytics helps organizations to rely on data. This data helps us make better decisions. By analyzing the data, decisions 
                                            can be made based on facts and numbers.",
                                            h1(br(),br(),br()),br(),br(),
                                            h5(br(),br(),br()),br(), align = "justify")
                                               )),
                                      tags$div(class="carousel-item",
                                               tags$img(class="d-block w-100", src="1.jpg", alt="Second slide"),
                                               tags$div(class = "carousel-caption",
                                                        h1(strong('Employee Attrition Prediction')),
                                                        tags$button(id='jump_att',
                                                                    class = "btn btn-default action-button", 
                                                                    p('Get Started!'), 
                                                                    style = 'color:white;height:35px;padding: 5px 10px;border: 1px solid black;'),
                                                        p("In this use case, the objectives is to predict whether the employee has a chance to leave the company or not. For this case, we will use IBM Dataset from Kaggle ",
                                                          tags$a(href="https://www.kaggle.com/datasets/patelprashant/employee-attrition", strong("(Click here to see the data source)", style = 'color:white;')),".", align = "justify")
                                                        ,h1(br(),br(),br()),
                                                        h5(br(),br(),br(),br()),br(),br(),br()
                                                        
                                               )
                                      ),
                                      tags$div(class="carousel-item",
                                               tags$img(class="d-block w-100", src="2.jpg", alt="Third slide"),
                                               tags$div(class = "carousel-caption",
                                                        h1(strong('Employee Promotion Prediction'),  style = 'color:white;'),
                                                        tags$button(id='jump_promoted',class = "btn btn-default action-button",
                                                                    p('Get Started!'), style = 'color:white;height:35px;padding: 5px;border: 1px solid black;'),
                                                        p("In this use case, the objectives is to predict whether the employee has a chance to be promoted or not. For this case, we will use MNC Corporation Dataset that we get from Kaggle ",
                                                          tags$a(href="https://www.kaggle.com/datasets/arashnic/hr-ana?select=train.csv", strong("(Click here to see the data source)"), style='color:white;'),".", align = "justify"),
                                                        h1(br(),br(),br()),
                                                        h5(br(),br(),br(),br()),br(),br(),br()
                                               )
                                      ),
                                      tags$div(class="carousel-item",
                                               tags$img(class="d-block w-100" ,src="3.jpg" ,alt="Fourth slide"),
                                               tags$div(class = "carousel-caption",
                                                        h1(strong('Absenteeism Prediction'), style = 'color:white;'),
                                                        tags$button(id='jump_absent',class = "btn btn-default action-button",
                                                                    p('Get Started!'), style = 'color:white;height:35px;padding: 5px;border: 1px solid black;'),
                                                        p("In this use case, the objectives is to predict how many hours each employees tend to absence. For this case, we will use Absenteeism at Work Dataset that we can get from UCI Machine Learning ",
                                                          tags$a(href="https://archive.ics.uci.edu/ml/datasets/Absenteeism+at+work", strong("(Click here to see the data source)"), style = 'color:white;'),".", align = "justify")
                                                        ,h1(br(),br(),br())
                                                        ,h5(br(),br(),br(),br()),br(),br(),br()
                                               )
                                      ),
                                      tags$div(class="carousel-item",
                                               tags$img(class="d-block w-100" ,src="4.jpg" ,alt="Fifth slide"),
                                               tags$div(class = "carousel-caption",
                                                        h1(strong('Guide : Analysis Individual Plot'), style = 'color:white;'),
                                                        tags$button(id='guide',class = "btn btn-default action-button", 
                                                                    p('Start to Read!'), style = 'color:white;height:35px;padding: 5px;border: 1px solid black;'),
                                                        p("We will learn to understand how to interpret our analysis individual plot.")
                                                        ,h1(br(),br(),br()),
                                                        h5(br(),br(),br(),br(),br()),br(),br(),br(),br()
                                               )
                                      )
                             ),
                             tags$a(class="carousel-control-prev" ,href="#carouselExampleIndicators", role="button" ,"data-slide"="prev",onclick="$('#carouselExampleIndicators').carousel('prev'); return false;",
                                tags$span(class="carousel-control-prev-icon", 'aria-hidden'="true"),
                                tags$span(class="sr-only", "Previous")
                             ),
                             tags$a(class="carousel-control-next", href="#carouselExampleIndicators", role="button", "data-slide"="next",onclick="$('#carouselExampleIndicators').carousel('next'); return false;",
                                tags$span(class="carousel-control-next-icon","aria-hidden"="true"),
                                tags$span(class="sr-only","Next")
                             )
                           )),
                           

           # TAB 2 ---------------------------------------------------------
       tabPanel(title = "Employee Attrition",value = "attrition",
                tabsetPanel(
                  id = "tabs",
                  tabPanel(title = "Overview",
                     fluidRow(br(),
                      box(title = div("Demography", align = "center",style='color:maroon; font-size:25px;'),
                        width = 12,solidHeader = T,
                          plotlyOutput("demography")),
                             box(title = div("Experience", align = "center",style='color:maroon; font-size:25px;'),
                                 width = 6,solidHeader = T,
                               plotlyOutput("experiences")
                             ),
                      box(title = div("Company Survey", align = "center",style='color:maroon; font-size:25px;'),
                          width = 6,solidHeader = T,
                          plotlyOutput("company_survey"))
                          )),
                  tabPanel(title = "Prediction",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                          conditionalPanel(condition = "input.att_or_not == 'Yes' && input.employ_plot_or_table != 'Variable Importance'",
                                     h2(strong(textOutput("no_attrition"))),
                                     h5("Employees Leave")),
                          conditionalPanel(condition = "input.att_or_not == 'No' && input.employ_plot_or_table != 'Variable Importance'",
                                     h2(strong(textOutput("stay"))),
                                     h5("Employees Stay")),
                                     br(),
                          radioButtons(inputId = "employ_plot_or_table",
                                       choices = c("Variable Importance","Table","Individual Plot"),
                                       label = "Select Output:",
                                       selected = "Variable Importance"),
                          conditionalPanel(condition = "input.employ_plot_or_table != 'Variable Importance'",
                                selectizeInput("department", 
                                                          label = "Select Department: ",
                                                          choices = unique(employ_train$Department),
                                                          selected = "Sales",
                                                          multiple = T
                                                          ,options = list(maxOptions = 3)
                                          ),
                                radioButtons(inputId = "att_or_not",
                                          choices = c("Yes","No"),
                                          label = "Leave Company?",
                                          selected = "Yes")
                              )),
                        mainPanel(width = 9,
                           conditionalPanel("input.employ_plot_or_table == 'Variable Importance'",
                                    fluidRow(
                                      box(width = 5,
                                          title = "Attrition?", solidHeader = TRUE,
                                          background = "teal",
                                          plotlyOutput("bagi_att_or_not")
                                      ),
                                       box(width = 7,
                                           title = "Variable Importance", solidHeader = TRUE,
                                           background = "teal",
                                           plotlyOutput("var_imp_employ"))
                                        )),
                            conditionalPanel("input.employ_plot_or_table != 'Variable Importance'",
                                box(width = 12,
                                  title = "Individual Analysis", solidHeader = TRUE,
                                  background = "teal",
                                  conditionalPanel(condition = "input.employ_plot_or_table == 'Table'",
                                                  selectInput(
                                                    inputId = "variable",
                                                    label = "Select Variable :",
                                                    choices = NULL, 
                                                    multiple = T,
                                                    width = 100000000),
                                                  dataTableOutput("PredOutput"), width = 12),
                                  conditionalPanel(condition = "input.employ_plot_or_table == 'Individual Plot'",
                                                  selectizeInput("employ_id", 
                                                              label = "Select Employee ID",
                                                              choices = NULL,
                                                              multiple = T, width = 1000000),
                                                  plotOutput("individual")))
                            ))
                        )),
                  tabPanel(title = "Simulation",
                    sidebarLayout(
                      sidebarPanel(
                        fileInput("file_att", "Choose CSV File",
                                  multiple = FALSE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        radioButtons(inputId = "test_plot_or_table_att",
                                     choices = c("Prediction","Details", "Individual Analysis"),
                                     label = "Select Output:",
                                     selected = "Prediction"),
                        checkboxInput("att_example", 
                                      label = "use example data",
                                      value = NULL)
                      ),
                      mainPanel(
                        conditionalPanel(condition = "input.test_plot_or_table_att == 'Prediction' && input.att_example == '0'",
                                         fluidRow(
                                         box(title = "Attrition?",solidHeader = T,width = 12,plotlyOutput("sim_att_plot")))
                        ),
                        conditionalPanel(condition = "input.test_plot_or_table_att == 'Details' && input.att_example == '0'",
                                         fluidRow(
                                           box(title = "Details",solidHeader = T,width = 12,dataTableOutput("sim_att_data")))
                        ),
                        conditionalPanel(condition = "input.test_plot_or_table_att == 'Details' && input.att_example == '1'",
                                         fluidRow(
                                           box(title = "Details",solidHeader = T,width = 12,dataTableOutput("sim_att_data_example")))
                        ),
                        conditionalPanel(condition = "input.test_plot_or_table_att == 'Prediction' && input.att_example == '1'",
                                         fluidRow(
                                           box(title = "Attrition?",solidHeader = T,width = 12,plotlyOutput("sim_att_plot_example")))
                        ),
                        conditionalPanel(condition = "input.test_plot_or_table_att == 'Individual Analysis' && input.att_example == '0'",
                                        fluidRow(
                                          box(title = "Individual Analysis",solidHeader = T,width=12,
                                            selectizeInput("att_test_id",
                                                            label = "Select Employee Number", 
                                                            choices = NULL, multiple = T, width = 100000000
                                            ),
                                            plotOutput("att_test_plot")
                                            ))),
                        conditionalPanel(condition = "input.test_plot_or_table_att == 'Individual Analysis' && input.att_example == '1'",
                                        fluidRow(
                                          box(title = "Individual Analysis",solidHeader = T, width = 12,    
                                            selectizeInput("att_test_id_example",
                                                            label = "Select Employee Number",
                                                            choices = unique(employ_test$EmployeeNumber),
                                                            selected = sample(unique(employ_test$EmployeeNumber),1),
                                                            options = list(maxItems = 4), multiple = T
                                            ),
                                            plotOutput("att_test_plot_example")))
                            )
                      )))
                )),
           
           
           # TAB 3 -------------------------------------------------------------------
           
           
           tabPanel(title = "Employee Promotion",value = "promotion",
                    
                  tabsetPanel(
                    tabPanel(title = "Overview",br(),
                    fluidRow(
                             box(title = div("Demography", align = "center",style='color:maroon; font-size:25px;'),
                                    width = 12,solidHeader = T,
                                    plotlyOutput("demography_promote", width = "100%")),
                             box(title = div("Experience", align = "center",style='color:maroon; font-size:25px;'),
                                    width = 12,solidHeader = T,
                                    plotlyOutput("experiences_promote", width = "100%")
                    ))),
                    tabPanel(title = "Prediction",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                   conditionalPanel(condition = "input.promote_or_not == 'Yes' && input.promote_plot_or_table != 'Variable Importance'",
                                     h2(strong(textOutput("no_promote"))),
                                     h5("Employees Promoted")),
                                    conditionalPanel(condition = "input.promote_or_not == 'No' && input.promote_plot_or_table != 'Variable Importance'",
                                     h2(strong(textOutput("no_not_promoted"))),
                                     h5("Employees Haven't Promoted")),
                                     br(),
                          radioButtons(inputId = "promote_plot_or_table",
                                       choices = c("Variable Importance","Table","Individual Plot"),
                                       label = "Select Output:",
                                       selected = "Variable Importance"),
                        conditionalPanel(condition = "input.promote_plot_or_table != 'Variable Importance'",
                          selectInput("department_promote", 
                                      label = "Select Department: ",
                                      choices = unique(promoteClean$department),
                                      selected = sample(promoteClean$department, 1),
                                      multiple = T),
                          radioButtons(inputId = "promote_or_not",
                                       choices = c("Yes","No"),
                                       label = "Promoted?",
                                       selected = "Yes")
                        )),
                     mainPanel(width = 9,
                        conditionalPanel("input.promote_plot_or_table == 'Variable Importance'",
                                                   fluidRow(
                                                     box(width = 5,
                                                         title = "Promoted?", solidHeader = TRUE,
                                                         background = "teal",
                                                         plotlyOutput("bagi_promote_or_not")
                                                     ),
                                                     box(width = 7,
                                                         title = "Variable Importance", solidHeader = TRUE,
                                                         background = "teal",
                                                         plotlyOutput("var_imp_promote"))
                                                   )),
                      conditionalPanel("input.promote_plot_or_table != 'Variable Importance'",
                          box(width = 12,
                            title = "Individual Analysis", solidHeader = TRUE,
                            background = "teal", 
                            conditionalPanel(
                              condition = "input.promote_plot_or_table == 'Table'",
                              selectInput(
                                inputId = "variable2",
                                label = "Select Variable :",
                                choices = NULL, 
                                multiple = T, width = 100000000),
                              dataTableOutput("PredOutput2"), width = 12),
                            conditionalPanel(
                              condition = "input.promote_plot_or_table == 'Individual Plot'",
                              selectizeInput("promote_id", 
                                          label = "Select Employee ID",
                                          choices = NULL,
                                          multiple = T
                                          , width = 100000000
                                          ),
                              plotOutput("individual_promote")
                            ))))
                        )),
                    tabPanel(title = "Simulation",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file_promote", "Choose CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          radioButtons(inputId = "promote_plot_or_table_test",
                                       choices = c("Prediction","Details", "Individual Analysis"),
                                       label = "Select Output:",
                                       selected = "Prediction"),
                          checkboxInput("promote_example", 
                                        label = "use example data",
                                        value = NULL)
                        ),
                        mainPanel(
                          conditionalPanel(condition = "input.promote_plot_or_table_test == 'Prediction' && input.promote_example == '0'",
                                           box(title = "Good to be Promoted?",solidHeader = T,width=12,plotlyOutput("sim_test_promote"))),
                          
                          conditionalPanel(condition = "input.promote_plot_or_table_test == 'Prediction' && input.promote_example == '1'",
                                           box(title = "Good to be Promoted?",solidHeader = T, width=12,plotlyOutput("sim_test_promote_example"))),
                          
                          conditionalPanel(condition = "input.promote_plot_or_table_test == 'Details' && input.promote_example == '0'",
                                           box(title = "Details",solidHeader = T,width=12,dataTableOutput("data_Test_Promote"))),
                          
                          conditionalPanel(condition = "input.promote_plot_or_table_test == 'Details' && input.promote_example == '1'",
                                           box(title = "Details", solidHeader = T,width=12,dataTableOutput("data_Test_Promote_example"))),
                          conditionalPanel(condition = "input.promote_plot_or_table_test == 'Individual Analysis' && input.promote_example == '0'",
                                           box(title = "Individual Analysis",solidHeader = T,width=12,selectizeInput("promote_test_id",
                                               label = "Select Employee ID: (Max. 4)", 
                                               choices = NULL, multiple = T, width = 100000000),
                                           plotOutput("promote_test_plot"))),
                          conditionalPanel(condition = "input.promote_plot_or_table_test == 'Individual Analysis' && input.promote_example == '1'",
                                      box(title = "Individual Analysis",solidHeader = T,width=12,selectizeInput("promote_test_id_example",
                                                          label = NULL,
                                                          choices = NULL, multiple = T
                                                       ,options = list(maxItems = 4)
                                                       , width = 100000000),
                                           plotOutput("promote_test_plot_example")))
                        ))))),
                    
           
           # TAB 4 -------------------------------------------------------------------
           
           
           tabPanel(title = "Absenteeism",value = "absent",
                    tabsetPanel(id = "tabset_absent",
                            tabPanel(title = "Overview",
                              fluidRow(
                                box(title = div("Demography", align = "center",style='color:maroon; font-size:25px;'),
                                      width = 4,solidHeader = T,
                                  plotlyOutput("demography_absent")
                                ),
                                box(title = div("Time Related", align = "center",style='color:maroon; font-size:25px;'),background = "maroon",
                                    width = 8,solidHeader = T,status = "primary",
                                  plotlyOutput("time"), style = 'color:maroon'
                                ),
                                box(title = div("Employee's Profile", align = "center",style='color:maroon; font-size:25px;'),
                                    width = 12,solidHeader = T,
                                  plotlyOutput("employee_profile")))),
                              tabPanel(title = "Prediction",
                                sidebarLayout(
                                  sidebarPanel(width = 3,
                                        conditionalPanel(condition = "input.absent_plot_or_table != 'Variable Importance'",
                                             conditionalPanel(condition = "input.absent_or_not == 'None'",
                                                              h2(strong(textOutput("absent_pred_none"))),
                                                              h5("Haven't Absence"),br()),
                                             conditionalPanel(condition = "input.absent_or_not == '1-2 Hours'",
                                                              h2(strong(textOutput("absent_pred_1_2"))),
                                                              h5("Absence 1-2 Hours"),br()),
                                             conditionalPanel(condition = "input.absent_or_not == '3-4 Hours'",
                                                              h2(strong(textOutput("absent_pred_3_4"))),
                                                              h5("Absence 3-4 Hours"),br()),
                                             conditionalPanel(condition = "input.absent_or_not == '5-7 Hours'",
                                                              h2(strong(textOutput("absent_pred_5_7"))),
                                                              h5("Absence 5-7 Hours"),br()),
                                             conditionalPanel(condition = "input.absent_or_not == 'Off Work'",
                                                              h2(strong(textOutput("absent_pred_off"))),
                                                              h5("Off Work"),br())),
                                              radioButtons(inputId = "absent_plot_or_table",
                                                               choices = c("Variable Importance","Table","Individual Plot"),
                                                               label = "Select Output:",
                                                               selected = "Variable Importance"),
                                        conditionalPanel(condition = "input.absent_plot_or_table != 'Variable Importance'",
                                                      selectInput("month_pred", 
                                                                  label = "Select Month: ",
                                                                  choices = unique(absent_train$Month.of.absence),
                                                                  selected = "March",
                                                                  multiple = T),
                                                      radioButtons(inputId = "absent_or_not",
                                                                  choices = c("None","1-2 Hours","3-4 Hours","5-7 Hours", "Off Work"),
                                                                  label = "Absence:",
                                                                  selected = "1-2 Hours")
                                                     )),
                                  mainPanel(width = 9,
                                            conditionalPanel(condition = "input.absent_plot_or_table == 'Variable Importance'",
                                                             fluidRow(
                                                               box(width = 5,
                                                                   title = "Absent?", solidHeader = TRUE,
                                                                   background = "teal",
                                                                   plotlyOutput("bagi_absent_or_not")
                                                               ),
                                                               box(width = 7,
                                                                   title = "Variable Importance", solidHeader = TRUE,
                                                                   background = "teal",
                                                                   plotlyOutput("var_imp_absent"))
                                                             )),
                                               conditionalPanel(condition = "input.absent_plot_or_table != 'Variable Importance'",
                                                     box(width = 12,
                                                         title = "Individual Analysis", solidHeader = TRUE,
                                                         background = "teal", 
                                                         conditionalPanel(condition = "input.absent_plot_or_table == 'Table'",
                                                                          selectInput(
                                                                            inputId = "variable_absent",
                                                                            label = "Select Variable :",
                                                                            choices = NULL, 
                                                                            multiple = T,
                                                                            selected = c("absent_id","ID", "Absenteeism.time.in.hours", "prob_absent"), width = 100000000),
                                                                          dataTableOutput("PredOutputAbsent"), width = 12),
                                                         conditionalPanel(condition = "input.absent_plot_or_table == 'Individual Plot'",
                                                                          selectizeInput("absent_id", 
                                                                                         label = "Select Employee ID",
                                                                                         choices = NULL,
                                                                                         multiple = T, width = 1000000),
                                                                          plotOutput("individual_absent"))))
                           
                            ))),
                            tabPanel(title = "Simulation",
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput("file_absent", "Choose CSV File",
                                            multiple = FALSE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv")),
                                  radioButtons(inputId = "absent_test_plot_or_table",
                                               choices = c("Prediction","Details","Individual Analysis"),
                                               label = "Select Output:",
                                               selected = "Prediction"),
                                  checkboxInput("absent_example", 
                                                label = "use example data",
                                                value = NULL)
                                ),
                                mainPanel(
                                  conditionalPanel(condition = "input.absent_test_plot_or_table == 'Prediction' && input.absent_example =='0'",
                                                   box(title = "Absent",solidHeader = T, width=12,plotlyOutput("sim_test_absent"))),
                                  conditionalPanel(condition = "input.absent_test_plot_or_table == 'Prediction' && input.absent_example =='1'",
                                                   box(title = "Absent",solidHeader = T, width=12,plotlyOutput("sim_test_absent_example"))),
                                  conditionalPanel(condition = "input.absent_test_plot_or_table == 'Details' && input.absent_example =='0'",
                                                   box(title = "Details",solidHeader = T, width=12,dataTableOutput("data_Test_Absent"))),
                                  conditionalPanel(condition = "input.absent_test_plot_or_table == 'Details' && input.absent_example =='1'",
                                                   box(title = "Details",solidHeader = T, width=12,dataTableOutput("data_Test_Absent_example"))),
                                  conditionalPanel(condition = "input.absent_test_plot_or_table == 'Individual Analysis'&& input.absent_example =='0'",
                                                   box(title = "Individual Analysis",solidHeader = T, width=12,
                                                      selectizeInput("absent_test_id",
                                                                      label = "Select Employee Number", 
                                                                      choices = NULL),
                                                      plotOutput("absent_test_plot"))),
                                  conditionalPanel(condition = "input.absent_test_plot_or_table == 'Individual Analysis' && input.absent_example =='1'",
                                                   box(title = "Individual Analysis",solidHeader = T, width=12,         
                                                      selectizeInput(inputId = "absent_test_id_example",
                                                                       choices=NULL,
                                                                       label = "Select Absent ID: (Max. 4)",
                                                                       options = list(maxItems = 4), multiple = T),
                                                      plotOutput("absent_test_plot_example")))
                                )
                              )
                            ))),
           
           inverse = T
)
