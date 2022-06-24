# dashboard library
library(shiny)
library(shinydashboard)
library(shinyBS)
library(bslib)
library(DT)
library(nlme)

# data wrangling
library(dplyr)
library(lubridate)
library(stringr)
library(glue)
library(tidyr)

# data visualization
library(ggplot2)
library(plotly)
library(scales)
library(egg)
library(hrbrthemes)
library(ggridges)
library(viridis)

# Modelling
library(caret)
library(randomForest)
library(lime)

# EMPLOYEE ATTRITION ---------------------------------------------------------------
      RNGkind(sample.kind = "Rounding")
      # DATA
      employ <- read.csv("data_prediction/WA_Fn-UseC_-HR-Employee-Attrition.csv", stringsAsFactors = T)
      employ_clean <- employ %>% 
        mutate(Education = factor(Education, labels = c('Below College','College', 'Bachelor', 'Master', 'Doctor')),
               EnvironmentSatisfaction = factor(EnvironmentSatisfaction, labels = c('Low','Medium','High','Very High')),
               JobInvolvement = factor(JobInvolvement, labels = c('Low' ,'Medium' ,'High', 'Very High')),
               JobSatisfaction = factor(JobSatisfaction, labels = c('Low', 'Medium','High','Very High')),
               PerformanceRating = factor(PerformanceRating, levels = c(1,2,3,4),labels = c('Low', 'Good','Excellent','Outstanding')),
               RelationshipSatisfaction = factor(RelationshipSatisfaction, labels = c('Low', 'Medium', 'High', 'Very High')),
               WorkLifeBalance = factor(WorkLifeBalance, labels = c('Bad','Good', 'Better','Best')),
                Attrition = factor(Attrition, labels = c('No','Yes')),
               OverTime = factor(OverTime, labels = c("No", "Yes")))%>% 
        select(-c(Over18, StandardHours, EmployeeCount))      
      rownames(employ_clean) <- employ$EmployeeNumber
      
      # Train Test Split
      
      set.seed(123)
      index <-  sample(x = nrow(employ_clean), size = 0.8*nrow(employ_clean))
      employ_train <- employ_clean[index,]
      employ_test <- employ_clean[-index,]
    
      # UPSAMPLE
      employ_up <- upSample(x = employ_train %>% 
                              select(-Attrition),
                            y = employ_train$Attrition, 
                            yname = "Attrition")
      
      # Random Forest Model
      model_forest <- readRDS(url("https://github.com/yahma2204/People_Analytics_Dashboard/blob/main/model_forest.rds?raw=true"))
      
      # Prediction
      employ_train$is_Attrition <- predict(object = model_forest, newdata = employ_train, type = "raw")
      
      
      # Interpretation
      employ_testing <- employ_train %>% 
        select(c(Age, BusinessTravel,DailyRate, Department,DistanceFromHome,Education, 
                 EducationField, EnvironmentSatisfaction,Gender,HourlyRate, JobInvolvement, 
                 JobLevel,JobRole, JobSatisfaction, MaritalStatus, MonthlyIncome, 
                 MonthlyRate, NumCompaniesWorked, OverTime,PercentSalaryHike,
                 PerformanceRating,RelationshipSatisfaction,StockOptionLevel,
                 TotalWorkingYears, TrainingTimesLastYear, WorkLifeBalance,YearsAtCompany, 
                 YearsInCurrentRole,YearsSinceLastPromotion,YearsWithCurrManager))
      explain <- lime(x = employ_testing, model = model_forest)
      
     
        

# EMPLOYEE PROMOTION -------------------------------------------------------------
      
      # DATA 
      promote <- read.csv(url("https://raw.githubusercontent.com/titov-vladislav/HR-promote-predictions/master/raw_data/train.csv"), fileEncoding = "UTF-8",stringsAsFactors = T)
      promote_test_data <- read.csv(url("https://raw.githubusercontent.com/titov-vladislav/HR-promote-predictions/master/raw_data/test.csv"))
      promote_clean <- promote %>% 
        mutate(is_promoted=factor(is_promoted, labels = c("No", "Yes")),
               awards_won. = factor(awards_won., labels = c("No", "Yes"))) %>% 
        select(-KPIs_met..80.) %>% 
        na.omit() %>% 
        filter(education != "")
      rownames(promote_clean) <- promote_clean$employee_id
      promoteClean <- promote_clean 
      
      set.seed(123)
      index_promote <-  sample(x = nrow(promoteClean), size = 0.6*nrow(promoteClean))
      promote_train <- promoteClean[index_promote,]
      promote_test <- promoteClean[-index_promote,]
      
      # UpSample
      promote_up <- upSample(x = promote_train %>% 
                                select(department, region, education,
                                       no_of_trainings, age, previous_year_rating, awards_won.,avg_training_score),
                              y = promote_train$is_promoted, 
                              yname = "is_promoted")
      
      # prediction
      promote_rf <- readRDS("model/promote_rf.rds")
      
      # Model Evaluation
      promoteClean$promoted <- predict(object = promote_rf, newdata = promoteClean, type = "raw")
      
      
      # Output
      test_x_promote <- promoteClean %>% 
        select(c(department, region, education,
               no_of_trainings, age, previous_year_rating, awards_won.,avg_training_score))
      
      explain_promote <- lime(x = test_x_promote, model = promote_rf)
      
      
# ABSENTEEISM AT WORK -------------------------------------------------------------

      # DATA
      absent <- read.csv(url("https://raw.githubusercontent.com/marlhakizi/Absenteeism-time-in-hours/master/Absenteeism_at_work.csv"), sep = ",")
      absent <- absent[!duplicated(absent),]

      #Data Cleaning
      absent_clean1 <- absent  %>%
        mutate(Absenteeism.time.in.hours = as.factor(
          ifelse(Absenteeism.time.in.hours == 0, "None",
                 ifelse((Absenteeism.time.in.hours == 1)|(Absenteeism.time.in.hours == 2), "1-2 Hours",
                        ifelse((Absenteeism.time.in.hours == 3)|(Absenteeism.time.in.hours == 4), "3-4 Hours",
                               ifelse(between(Absenteeism.time.in.hours,5,7), "5-7 Hours",
                                      "Off Work"
                               )))))) %>% 
        na.omit()
      
      absent_clean <- absent_clean1 %>%
        mutate(Reason.for.absence=factor(Reason.for.absence,levels = c(0,1,2,3,4,5,6,7,8,9,
                                                                       10,11,12,13,14,15,16,17,18,
                                                                       19,20,21,22,23,24,25,26,27,28), 
                                         labels = c("None","Certain infectious and parasitic diseases", "Neoplasms","Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
                                                    "Endocrine, nutritional and metabolic diseases",
                                                    "Mental and behavioural disorders",
                                                    "Diseases of the nervous system",
                                                    "Diseases of the eye and adnexa",
                                                    "Diseases of the ear and mastoid process",
                                                    "Diseases of the circulatory system",
                                                    "Diseases of the respiratory system",
                                                    "Diseases of the digestive system",
                                                    "Diseases of the skin and subcutaneous tissue",
                                                    "Diseases of the musculoskeletal system and connective tissue",
                                                    "Diseases of the genitourinary system",
                                                    "Pregnancy, childbirth and the puerperium",
                                                    "Certain conditions originating in the perinatal period",
                                                    "Congenital malformations, deformations and chromosomal abnormalities",
                                                    "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
                                                    "Injury, poisoning and certain other consequences of external causes",
                                                    "External causes of morbidity and mortality",
                                                    "Factors influencing health status and contact with health services.",
                                                    "patient follow-up", "medical consultation", "blood donation", "laboratory examination", "unjustified absence" , "physiotherapy", "dental consultation")),
               Month.of.absence=factor(Month.of.absence, levels = c(1,2,3,4,5,
                                                                    6,7,8,9,10,
                                                                    11,12),
                                       labels = c("January", "February", "March", "April",
                                                  "May", "June", "July", "August",
                                                  "September", "October", "November", "December")),
               Day.of.the.week=factor(Day.of.the.week,labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),
               Seasons = factor(Seasons, labels = c("summer", "autumn", "winter", "spring")),
               Disciplinary.failure=factor(Disciplinary.failure, levels = c(0,1),labels = c("No", "Yes")),
               Education=factor(Education, labels = c("high school", "graduate", "postgraduate", "master and doctor")),
               Social.smoker=factor(Social.smoker, labels = c("No", "Yes")),
               Social.drinker=factor(Social.drinker, labels = c("No", "Yes")),
               Absenteeism.time.in.hours=factor(Absenteeism.time.in.hours, levels = c("None", "1-2 Hours", "3-4 Hours","5-7 Hours","Off Work")),
               ID = as.character(ID), 
               Work.load.Average.day = str_replace_all(Work.load.Average.day, pattern = ',', '.'),
               Work.load.Average.day = as.numeric(Work.load.Average.day)) %>%
        na.omit()
      
      set.seed(123)
      index_absent <-  sample(x = nrow(absent_clean), size = 0.8*nrow(absent_clean))
      absent_train <- absent_clean[index_absent,]
      absent_test <- absent_clean[-index_absent,]

      # UpSampling
      absent_up <- upSample(x = absent_train %>% select(Reason.for.absence,
                                                        Day.of.the.week, Distance.from.Residence.to.Work, Age, Month.of.absence,
                                                        Disciplinary.failure, Education, Son, Social.smoker, Body.mass.index),
                             y = absent_train$Absenteeism.time.in.hours, yname = "Absenteeism.time.in.hours")

      # Model Fitting
      absent_rf <- readRDS("model/absent_rf3.rds")

      # Predict
      absent_train$pred_absent <- predict(object = absent_rf, newdata = absent_train, type = "raw")
      absent_train$absent_id <- rownames(absent_train)
      
      # Output
      test_x_absent <- absent_train %>%
        select(Reason.for.absence,
               Day.of.the.week, Distance.from.Residence.to.Work, Age, Month.of.absence,
               Disciplinary.failure, Education, Son, Social.smoker, Body.mass.index)
      
      explain_absent <- lime(x = test_x_absent, model = absent_rf)