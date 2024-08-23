library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(flexdashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)
library(knitr)
library(RColorBrewer)
library(kableExtra)
library(lubridate)
library(shinyLP)
library(writexl)

df <- read.csv("CleanedDataUntilAugustCorrected.csv", check.names = FALSE)

# Replace "NA" and "NULL" with NA in all columns of the dataframe
for (col in colnames(df)) {
  df[[col]][df[[col]] %in% c("NA", "NULL", "")] <- NA
}

df <- df %>% 
  mutate(BMI = ifelse(is.na(BMI), 0, BMI))
                            
# Columns to convert to numeric
numeric_columns <- c(
  'Weight', 'Height', 'BMI', 'Morning Stiffness (mg)', 'ESR', 'CRP', 'HAQ', 'DAS28', 
  'SDAI', 'WBC', 'Hgb', 'PLT', 'Creatinine', 'FBS', 'AST', 'ALT', 'ALP', 'T.Chol', 
  'LDL', 'HDL', 'TG', 'Uric acid')

# Convert specified columns to numeric
df <- df %>%
  mutate_at(vars(all_of(numeric_columns)), as.numeric)

df <- df %>%
  mutate(
    `Date of Diagnosis` = gsub("-00$", "-01", `Date of Diagnosis`),
    `Visit Date` = as.Date(`Visit Date`),
    `Visit Entry Date` = as.Date(`Visit Entry Date`),
    `Date of Diagnosis` = as.Date(`Date of Diagnosis`),
    `Birth Date` = as.Date(`Birth Date`, format = "%m/%d/%Y")
  )

df$Age <- as.integer(difftime(Sys.Date(), df$`Birth Date`, units = "days") / 365.25)

df$`Disease Duration` <- as.integer(difftime(Sys.Date(), df$`Date of Diagnosis`, units = "days") / 365.25)

df <- df %>%
  filter(complete.cases(`Nationality`,
                        `DAS28`,
                        `CDAI`,
                        `Disease Duration`,
                        `Visit Date`,
                        Age,
                        BMI) &
           `Nationality` != "Israel")

# Create a new column 'BMICategory' based on the custom ranges
df$BMICategory <- cut(df$`BMI`,
                       breaks = c(-Inf, 18.5, 25, 30, 40, Inf),
                       labels = c("Underweight", "Healthy weight", "Overweight", "Obesity", "Severe Obesity"))

df$BMICategory <- as.factor(df$BMICategory)

# Create a new column 'DMARDS'
df$onDMARDS <- apply(df[, c("MTX(DOSE mg)",
                            "SSZ(DOSE mg)",
                            "Leflunomide(DOSE mg)",
                            "HCQ(DOSE mg)",
                            "Imuran(DOSE mg)",
                            "Cyclosporine(DOSE mg)")],
                     1,
                     function(row) {
                       if ("yes" %in% row) {
                         return("Yes")
                       } else {
                         return("No")
                       }
                     })

df$onDMARDS <- as.factor(df$onDMARDS)

# Create a new column 'Biologic'
df$onBiologic <- apply(df[, c("Infliximab(DOSE mg)",
                              "Etanercept(DOSE mg)",
                              "Adalimumab(DOSE mg)",
                              "Rituximab(DOSE mg)",
                              "Abatacept(DOSE mg)",
                              "Tociluzumab(DOSE mg)",
                              "Certolizumab(DOSE mg)",
                              "Tofacitinib(DOSE mg)",
                              "Golimumab(DOSE mg)",
                              "Baricitinib(DOSE mg)",
                              "Amgevita(DOSE mg)",
                              "Upadacitinib(Frequency)",
                              "Other(DOSE mg)",
                              "Other-Biosimilars(DOSE mg)",
                              "Other-tsDMRDs(DOSE mg)",
                              "Other-tsDMRDs(Started On)",
                              "Hyrimoz(DOSE mg)")],
                       1,
                       function(row) {
                         if ("yes" %in% row) {
                           return("Yes")
                         } else {
                           return("No")
                         }
                       })

df$onBiologic <- as.factor(df$onBiologic)

# Create a new column 'Treatment'
df$Treatment <- ifelse(df$onDMARDS == "Yes" & df$onBiologic == "Yes", "Combination",
                       ifelse(df$onDMARDS == "Yes" & df$onBiologic == "No", "Mono DMARDs",
                              ifelse(df$onDMARDS == "No" & df$onBiologic == "Yes", "Mono Biologics",
                                     "None")))

df$Treatment <- as.factor(df$Treatment)

# Create a new column 'BioTreatment'
df$BioTreatment <- ifelse(df$onDMARDS == "Yes" & df$onBiologic == "Yes", "Combination",
                              ifelse(df$onDMARDS == "No" & df$onBiologic == "Yes", "Mono Biologics",
                                     "None"))

df$BioTreatment <- as.factor(df$BioTreatment)

# Create a new column 'DMARDTreatment'
df$DMARDTreatment <- ifelse(df$onDMARDS == "Yes" & df$onBiologic == "Yes", "Combination",
                       ifelse(df$onDMARDS == "Yes" & df$onBiologic == "No", "Mono DMARDs",
                                     "None"))

df$DMARDTreatment <- as.factor(df$DMARDTreatment)

# Create a new column 'BioType'
df$BioType <- apply(df[, c("Infliximab(DOSE mg)",
                           "Etanercept(DOSE mg)",
                           "Adalimumab(DOSE mg)",
                           "Rituximab(DOSE mg)",
                           "Abatacept(DOSE mg)",
                           "Tociluzumab(DOSE mg)",
                           "Certolizumab(DOSE mg)",
                           "Tofacitinib(DOSE mg)",
                           "Golimumab(DOSE mg)",
                           "Baricitinib(DOSE mg)",
                           "Amgevita(DOSE mg)",
                           "Upadacitinib(Frequency)")],
                    1,
                    function(row) {
                      # Check for JAKI
                      if (!any(is.na(row)) && (row["Upadacitinib(Frequency)"] == "yes" ||
                                               row["Baricitinib(DOSE mg)"] == "yes" ||
                                               row["Tofacitinib(DOSE mg)"] == "yes")) {
                        return("JAKI")
                      }
                      # Check for Non-Anti TFN
                      else if (!any(is.na(row)) && (row["Rituximab(DOSE mg)"] == "yes" ||
                                                    row["Abatacept(DOSE mg)"] == "yes" ||
                                                    row["Tociluzumab(DOSE mg)"] == "yes")) {
                        return("Non-Anti TFN")
                      }
                      # Check for ANTI TNF
                      else if (!any(is.na(row)) && (row["Amgevita(DOSE mg)"] == "yes" ||
                                                    row["Golimumab(DOSE mg)"] == "yes" ||
                                                    row["Certolizumab(DOSE mg)"] == "yes" ||
                                                    row["Adalimumab(DOSE mg)"] == "yes" ||
                                                    row["Etanercept(DOSE mg)"] == "yes" ||
                                                    row["Infliximab(DOSE mg)"] == "yes")) {
                        return("ANTI TNF")
                      } else {
                        return("Other")
                      }
                    })


df$BioType <- as.factor(df$BioType)

df <- df %>%
  mutate(DAS28Group = case_when(
    DAS28 < 2.6 ~ "remission",
    DAS28 >= 2.6 & DAS28 < 3.1 ~ "low activity",
    DAS28 >= 3.1 & DAS28 < 5.1 ~ "moderate activity",
    DAS28 >= 5.1 ~ "high activity",
    TRUE ~ NA_character_  # for cases that do not fit any of the above conditions
  ))

df <- df %>%
  mutate(CDAIGroup = case_when(
    CDAI < 2.8 ~ "remission",
    CDAI >= 2.9 & CDAI <= 10 ~ "low activity",
    CDAI >= 10.1 & CDAI <= 22 ~ "moderate activity",
    CDAI >= 22.1 ~ "high activity",
    TRUE ~ NA_character_  # for cases that do not fit any of the above conditions
  ))

# Create a new column 'CDAIBinaryGroup' based on each row
df$CDAIBinaryGroup <- sapply(df$CDAIGroup, function(row) {
  if ("remission" %in% row) {
    return("remission")
  } else {
    return("RA")
  }
})

# Convert 'DAS28BinaryGroup' to factor
df$CDAIBinaryGroup <- as.factor(df$CDAIBinaryGroup)

# Create a new column 'CDAIBinaryGroup' based on each row
df$DAS28BinaryGroup <- sapply(df$DAS28Group, function(row) {
  if ("remission" %in% row) {
    return("remission")
  } else {
    return("RA")
  }
})

# Convert 'DAS28BinaryGroup' to factor
df$DAS28BinaryGroup <- as.factor(df$DAS28BinaryGroup)

excluded_columns <- c(
  "User ID",
  "Hospital",
  "Entry Date",
  "Nationality",
  "Weight",
  "Height",
  "Area",
  "Governorate",
  "Data Entry By",
  "Birth Date",
  "Rheumatologist",
  "Visit Number",
  "Visit Date",
  "Visit Entry Date",
  "User ID",
  "Visit Number",
  "Visit Entry Date",
  "Date of Diagnosis",
  "User ID",
  "Visit Number",
  "Visit Entry Date",
  "patient",
  "DAS28Group",
  "CDAIGroup",
  "BMICategory",
  "Other(DOSE mg)",
  "Other-Biosimilars(DOSE mg)",
  "Other-tsDMRDs(DOSE mg)",
  "Other-tsDMRDs(Started On)",
  "DAS28BinaryGroup",
  "CDAIBinaryGroup",
  "onDMARDS", 
  "onBiologic"
)

# Define UI for the application
ui <- dashboardPage(
  dashboardHeader(disable = FALSE, titleWidth = 300),
  dashboardSidebar(width = 300,
                   minified = F,
                   sidebarMenu(       
                     HTML(paste0(
                       "<br>",
                       "<a href='https://i.postimg.cc/2yKzp8Lm/1-PNG.png' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='https://i.postimg.cc/2yKzp8Lm/1-PNG.png' width = '186'></a>",
                       "<br>",
                       "<p style = 'text-align: center;'><small><a href='https://i.postimg.cc/2yKzp8Lm/1-PNG.png' target='_blank'>ASIA logo disclaimer</a></small></p>",
                       "<br>"
                     )),
                     menuItem("Introduction", tabName = "Intro", icon = icon("circle-info")),
                     menuItem("Rheumatologist", tabName = "Rheumatologist", icon = icon("user-doctor")),
                     menuItem("Patients Characteristics", tabName = "Page1", icon = icon("address-card")),
                     menuItem("Patients Disease Information", tabName = "Page2", icon = icon("notes-medical")),
                     menuItem("Patients Joint Information", tabName = "Page3", icon = icon("person-cane")),
                     menuItem("Patients Blood Test Information", tabName = "Page4", icon = icon("droplet")),
                     menuItem("Medication Usage", tabName = "Page5", icon = icon("staff-snake")),
                     menuItem("Predictions", tabName = "predictions", icon = icon("laptop-code"), 
                              menuSubItem("Binary Group Predictions", tabName = "Page6"),
                              menuSubItem("Scores Predictions", tabName = "Page7")),
                     HTML(paste0(
                       "<br><br><br><br><br><br><br><br><br>",
                       "<table style='margin-left:auto; margin-right:auto;'>",
                       "<tr>",
                       "<td style='padding: 5px;'><a href='https://www.facebook.com/StatisticalConsultancyKuwait/' target='_blank'><i class='fab fa-facebook-square fa-lg'></i></a></td>",
                       "<td style='padding: 5px;'><a href='https://www.instagram.com/acs_kw?utm_source=ig_web_button_share_sheet&igsh=ZDNlZDc0MzIxNw==' target='_blank'><i class='fab fa-instagram fa-lg'></i></a></td>",
                       "<td style='padding: 5px;'><a href='https://www.linkedin.com/company/advancementsolutionsforstatistics/' target='_blank'><i class='fab fa-linkedin fa-lg'></i></a></td>",
                       "<td style='padding: 5px;'><a href='https://x.com/acs_kw?s=11&t=VdemYjzwqFmPLUQi6VNAaw' target='_blank'><i class='fab fa-square-x-twitter fa-lg'></i></a></td>",
                       "</tr>",
                       "</table>",
                       "<br>"),
                       HTML(paste0(
                         "<script>",
                         "var today = new Date();",
                         "var yyyy = today.getFullYear();",
                         "</script>",
                         "<p style = 'text-align: center;'><small>&copy; - <a href='https://www.solutions4statistics.com/' target='_blank'>ASIA Consulting and Private Training Co.</a> - <script>document.write(yyyy);</script></small></p>")
                       ))
                   )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
              /* Change skin colors */
              .skin-blue .main-header .logo,
              .skin-blue .main-header .navbar,
              .skin-blue .main-header .navbar .sidebar-toggle ,
              .skin-blue .main-sidebar,
              .skin-blue .main-footer {
                background-color: #00072d;
              }
              
              /* Change hover color for menu button and items */
              .skin-blue .main-header .navbar .sidebar-toggle:hover,
              .skin-blue .main-header .navbar .nav > li > a:hover,
              .skin-blue .main-header .navbar .nav > li > a:focus {
                background-color: #00072d;
              }
              
              .skin-blue .sidebar-menu > li:hover > a,
              .skin-blue .sidebar-menu > li.active > a {
                background-color: #000726;
              }
              
              .skin-blue .main-header .logo:hover {
                background-color: #00072d;
              }
              
              /* Background color of submenu items */
              .skin-blue .treeview-menu > li > a {
                background-color: #00072d;
              }
              
              .skin-blue .treeview-menu > li:hover > a,
              .skin-blue .treeview-menu > li.active > a {
                background-color: #000726;
              }
              
              body, .content-wrapper {
                background-color: #ffffff; /* White background color */
              }
              
              /* Add custom CSS for horizontal scrolling */
              .dataTables_wrapper {
                overflow-x: auto;
              }
              
            ")),
      tags$style(HTML("
      .jumbotron {
        background-color: #00072d;
        color: white;
        padding-left: 30px; /* Adding padding to the left side */

      }
    "))
    ),
    tabItems(
      tabItem("Intro",
              fluidRow(
                column(
                  width = 12,
                  jumbotron("Welcome to the Rheumatoid Arthritis Patient Care Dashboard!", "",
                            button = FALSE),
                  p(style = "font-size: 16px;", "This interactive dashboard offers comprehensive insights into the characteristics and health status of patients diagnosed with Rheumatoid Arthritis (RA), a chronic autoimmune condition primarily affecting the joints, leading to inflammation, pain, and stiffness. Understanding the demographics, symptoms, and treatment patterns of RA patients is paramount for healthcare professionals to deliver personalized care and enhance patient outcomes. Within this dashboard, a plethora of visualizations and analyses pertaining to RA patients are available, encompassing demographic details, joint symptoms, disease duration, comorbidities, medication usage, and laboratory test results."),
                  h4("Team Members"),
                  p(style = "font-size: 16px;", "Developed by Dr. Ahmad Alsaber, Huda Khraiss, and Amal Anbar, experts in the field of data analysis, this dashboard is a product of ASIA Co. Their collective expertise ensures that healthcare professionals, researchers, clinicians, and policymakers gain valuable insights into Rheumatoid Arthritis (RA) patient management and care. By leveraging advanced data analytics, their work aims to empower stakeholders in the healthcare industry with actionable information for optimizing RA treatment strategies and improving patient outcomes."),
                  fluidRow(
                    column(
                      width = 12,
                      div(style = "margin-top: 20px;")
                    )
                  ),
                  div(
                    style = "text-align: center; margin-top: 20px; margin-bottom: 20px;",
                    p(style = "font-size: 16px;", "This project is sponsored by:")
                  ),
                  div(
                    style = "margin-top: 20px; margin-bottom: 20px; display: flex; justify-content: space-around; align-items: center;",
                    div(
                      style = "text-align: center;",
                      img(src = "https://i.postimg.cc/7hvDP3VP/logo-KRRD.png", style = "max-width: 100%; height: auto;"),
                      p(style = "margin-top: 10px; margin-bottom: 0;", "Kuwait Registry for Rheumatic Diseases (KRRD)")
                    ),
                    div(
                      style = "text-align: center;",
                      img(src = "https://i.postimg.cc/nLnmd7bW/kfas-logo-EC91-F5-BE6-C-seeklogo-com.png", style = "max-width: 100%; height: auto;"),
                      p(style = "margin-top: 10px; margin-bottom: 0;", "Kuwait Foundation for the Advancement of Sciences (KFAS)")
                    ),
                    div(
                      style = "text-align: center;",
                      img(src = "https://i.postimg.cc/CL3MLVcH/American-University-of-Kuwait-svg.png", style = "max-width: 100%; height: auto;"),
                      p(style = "margin-top: 10px; margin-bottom: 0;", "American University of Kuwait (AUK)")
                    )
                  )
                )
              )
      ),
      tabItem("Rheumatologist",
              fluidPage(
                titlePanel("Patients per Hospital and Rheumatologist"),
                fluidRow(
                  tabsetPanel(
                    tabPanel("Hospital Patient Trends - Plot", plotlyOutput("HospitalPatientTrendsPlot")),
                    tabPanel("Hospital Patient Trends - Table", DTOutput("NewPatientsPerHospitalPerYearTable"))
                  )
                ),
                fluidRow(
                  column(width = 12,
                         selectInput("group_by", "Group By:",
                                     choices = c("Rheumatologist", "Data Entry By"),
                                     selected = "Rheumatologist"),
                         selectInput("rheumatologist", "Select Rheumatologist:",
                                     choices = sort(unique(df$Rheumatologist)), 
                                     selected = unique(df$Rheumatologist)[1]),
                         tabsetPanel(
                           tabPanel("Rheumatologist Patient Trends - Plot", plotlyOutput("RheumatologistPatientTrendsPlot")),
                           tabPanel("Rheumatologist Patient Trends - Table", DTOutput("RheumatologistPatientTrendsTable"))
                         )
                  )
                ),
                fluidRow(
                  column(width = 12,
                         selectInput("group_by2", "Group By:",
                                     choices = c("Rheumatologist", "Data Entry By"),
                                     selected = "Rheumatologist"),
                         selectInput("year", "Select Year:",
                                     choices = sort(unique(format(as.Date(df$`Visit Entry Date`), "%Y"))), 
                                     selected = max(unique(format(as.Date(df$`Visit Entry Date`), "%Y")))),
                         tabsetPanel(
                           tabPanel("Rheumatologist Patient Distribution - Plot", plotlyOutput("RheumatologistPatientDistributionPlot")),
                           tabPanel("Rheumatologist Patient Distribution - Table", DTOutput("RheumatologistPatientDistributionTable"))
                         )
                  )
                )
              )
      ),
      tabItem("Page1",
              fluidPage(
                titlePanel("Patients Characteristics"),
                fluidRow(
                  uiOutput("InfoBoxesPC")
                ),
                plotlyOutput("TreemapPC"),
                fluidRow(
                  column(6, plotlyOutput("GenderPieChartPC")),
                  column(6, plotlyOutput("BMIPieChartPC"))
                ),
                plotlyOutput("VisitsHistogramPC"),
                tags$hr(),  # Add a horizontal rule for spacing
                plotlyOutput("DiseaseDurationBoxplotPC")
              )
      ),
      tabItem("Page2",
              sidebarLayout(
                sidebarPanel(
                  width = 3,  # Adjust the width of the sidebar
                  selectInput("GenderInputDI", label = "Gender:", 
                              choices = c("Male" ="male","Female" = "female"),
                              selected = "All", multiple = TRUE),
                  selectInput("NationalityInputDI", label = "Nationality:", 
                              choices = c(unique(df$`Nationality`)),
                              selected = "All", multiple = TRUE),
                  selectInput("RFInputDI", label = "RF:", 
                              choices = c("Positive", "Negative"), 
                              selected = "All", multiple = TRUE),
                  selectInput("AntiCCPInputDI", label = "ANTI CCP:", 
                              choices = c("Positive", "Negative"), 
                              selected = "All", multiple = TRUE),
                  selectInput("ANAInputDI", label = "ANA:", 
                              choices = c("Positive", "Negative"), 
                              selected = "All", multiple = TRUE),
                  selectInput("RheumatoidNodulesInputDI", label = "Rheumatoid Nodules:", 
                              choices = c("Yes", "No"), 
                              selected = "All", multiple = TRUE),
                  selectInput("SICCASymptomsInputDI", label = "SICCA Symptoms:", 
                              choices = c("Yes", "No"), 
                              selected = "All", multiple = TRUE),
                  dateRangeInput("DateInputDI", label = "Date Range:",
                                 start = min(df$`Visit Date`),
                                 end = max(df$`Visit Date`),
                                 format = "yyyy-mm-dd"),
                  sliderInput("AgeSliderInputDI", label = "Age Range:",
                              min = min(df$Age), max = max(df$Age),
                              value = c(min(df$Age), max(df$Age)), step = 1),
                  sliderInput("DiseaseDurationSliderInputDI", label = "Disease Duration Range:",
                              min = min(df$`Disease Duration`), max = max(df$`Disease Duration`),
                              value = c(min(df$`Disease Duration`), max(df$`Disease Duration`)), step = 1),
                  sliderInput("BMISliderInputDI", label = "BMI Range:",
                              min = min(df$BMI), max = max(df$BMI),
                              value = c(min(df$BMI), max(df$BMI)), step = 1),
                  sliderInput("DAS28SliderInputDI", label = "DAS28 Value:",
                              min = min(df$`DAS28`),
                              max = max(df$`DAS28`),
                              value = c(min(df$`DAS28`), max(df$`DAS28`)),
                              step = 0.1),
                  sliderInput("CDAISliderInputDI", label = "CDAI Value:",
                              min = min(df$`CDAI`),
                              max = max(df$`CDAI`),
                              value = c(min(df$`CDAI`), max(df$`CDAI`)),
                              step = 0.1),
                  radioButtons("FilterLogicDI", label = "Filter Logic for Medical Conditions:", 
                               choices = c("Intersection", "Union"), selected = "Union"),
                  checkboxGroupInput("MedicalConditionsInputDI", label = "Medical Conditions:",
                                     choices = c(
                                       "Asthma" = "MedCond-Visits Baseline-B.Asthma",
                                       "CAD" = "MedCond-Visits Baseline-CAD",
                                       "Osteoporosis" = "MedCond-Visits Baseline-Osteoporosis",
                                       "DM" = "MedCond-Visits Baseline-DM",
                                       "Haemoglobinopathy" = "MedCond-Visits Baseline-Haemoglobinopathy",
                                       "HBV" = "MedCond-Visits Baseline-HBV",
                                       "HCV" = "MedCond-Visits Baseline-HCV",
                                       "Hyperlipidemia" = "MedCond-Visits Baseline-Hyperlipidemia",
                                       "Hypertension" = "MedCond-Visits Baseline-Hypertension",
                                       "Thyroid Disease" = "MedCond-Visits Baseline-Thyroid Disease",
                                       "OA" = "MedCond-Visits Baseline-OA",
                                       "PUD" = "MedCond-Visits Baseline-PUD",
                                       "ILD" = "MedCond-Visits Baseline-ILD"),
                                     selected = NULL),
                  downloadButton("DownloadDataDI", "Download")),
                mainPanel(
                  width = 9,
                  fluidRow(
                    column(width = 12, 
                           selectInput("variable", label = "Choose Variable:",
                                       choices = c(
                                         "RF" = "RF",
                                         "ANTI CCP" = "ANTI CCP",
                                         "ANA" = "ANA",
                                         "Rheumatoid Nodules" = "Rheumatoid Nodules",
                                         "SICCA Symptoms" = "SICCA Symptoms"),
                                       selected = "Visits RF")
                    )
                  ),
                  fluidRow(
                    column(width = 12, plotlyOutput("plot1", height = "220px"))
                  ),
                  fluidRow(
                    column(width = 12, 
                           selectInput("Variable2", label = "Choose Variable:",
                                       choices = c(
                                         "CDAI Groups" = "CDAIGroup",
                                         "DAS28 Groups" ="DAS28Group"
                                       ),
                                       selected = "DAS28 Groups")
                    )
                  ),
                  fluidRow(
                    column(width = 12, plotlyOutput("plot2", height = "220px"))
                  ),
                  fluidRow(
                    selectInput("Variable3", label = "Choose Variable:",
                                choices = c("Patient's Global Assessment" = "Patient's Global Assessment", 
                                            "Physician's Global Assessment"= "Physician's Global Assessment"),
                                selected = "Physician's Global Assessment")
                  ),
                  fluidRow(
                    column(width = 12, plotlyOutput("plot3", height = "600px"))
                  )
                )
              )
      ),
      tabItem("Page3",
              sidebarLayout(
                sidebarPanel(
                  width = 3,  # Adjust the width of the sidebar
                  selectInput("GenderInputJI", label = "Gender:", 
                              choices = c("Male" ="male","Female" = "female"),
                              selected = "All", multiple = TRUE),
                  selectInput("NationalityInputJI", label = "Nationality:", 
                              choices = c(unique(df$`Nationality`)),
                              selected = "All", multiple = TRUE),
                  selectInput("RFInputJI", label = "RF:", 
                              choices = c("Positive", "Negative"), 
                              selected = "All", multiple = TRUE),
                  selectInput("AntiCCPInputJI", label = "ANTI CCP:", 
                              choices = c("Positive", "Negative"), 
                              selected = "All", multiple = TRUE),
                  selectInput("ANAInputJI", label = "ANA:", 
                              choices = c("Positive", "Negative"), 
                              selected = "All", multiple = TRUE),
                  selectInput("RheumatoidNodulesInputJI", label = "Rheumatoid Nodules:", 
                              choices = c("Yes", "No"), 
                              selected = "All", multiple = TRUE),
                  selectInput("SICCASymptomsInputJI", label = "SICCA Symptoms:", 
                              choices = c("Yes", "No"), 
                              selected = "All", multiple = TRUE),
                  dateRangeInput("DateInputJI", label = "Date Range:",
                                 start = min(df$`Visit Date`),
                                 end = max(df$`Visit Date`),
                                 format = "yyyy-mm-dd"),
                  sliderInput("AgeSliderInputJI", label = "Age Range:",
                              min = min(df$Age), max = max(df$Age),
                              value = c(min(df$Age), max(df$Age)), step = 1),
                  sliderInput("DiseaseDurationSliderInputJI", label = "Disease Duration Range:",
                              min = min(df$`Disease Duration`), max = max(df$`Disease Duration`),
                              value = c(min(df$`Disease Duration`), max(df$`Disease Duration`)), step = 1),
                  sliderInput("BMISliderInputJI", label = "BMI Range:",
                              min = min(df$BMI), max = max(df$BMI),
                              value = c(min(df$BMI), max(df$BMI)), step = 1),
                  sliderInput("DAS28SliderInputJI", label = "DAS28 Value:",
                              min = min(df$`DAS28`),
                              max = max(df$`DAS28`),
                              value = c(min(df$`DAS28`), max(df$`DAS28`)),
                              step = 0.1),
                  sliderInput("CDAISliderInputJI", label = "CDAI Value:",
                              min = min(df$`CDAI`),
                              max = max(df$`CDAI`),
                              value = c(min(df$`CDAI`), max(df$`CDAI`)),
                              step = 0.1),
                  radioButtons("FilterLogicJI", label = "Filter Logic for Medical Conditions:", 
                               choices = c("Intersection", "Union"), selected = "Union"),
                  checkboxGroupInput("MedicalConditionsInputJI", label = "Medical Conditions:",
                                     choices = c(
                                       "Asthma" = "MedCond-Visits Baseline-B.Asthma",
                                       "CAD" = "MedCond-Visits Baseline-CAD",
                                       "Osteoporosis" = "MedCond-Visits Baseline-Osteoporosis",
                                       "DM" = "MedCond-Visits Baseline-DM",
                                       "Haemoglobinopathy" = "MedCond-Visits Baseline-Haemoglobinopathy",
                                       "HBV" = "MedCond-Visits Baseline-HBV",
                                       "HCV" = "MedCond-Visits Baseline-HCV",
                                       "Hyperlipidemia" = "MedCond-Visits Baseline-Hyperlipidemia",
                                       "Hypertension" = "MedCond-Visits Baseline-Hypertension",
                                       "Thyroid Disease" = "MedCond-Visits Baseline-Thyroid Disease",
                                       "OA" = "MedCond-Visits Baseline-OA",
                                       "PUD" = "MedCond-Visits Baseline-PUD",
                                       "ILD" = "MedCond-Visits Baseline-ILD"),
                                     selected = NULL),
                  downloadButton("DownloadDataJI", "Download")),
                mainPanel(
                  width = 9,
                  fluidRow(
                    column(width = 6, plotlyOutput("JointPainPieChart")),
                    column(width = 6, plotlyOutput("JointSwellingPieChart"))
                  ),
                  fluidRow(
                    column(width = 12, plotlyOutput("VASHistogram", height = "550px"))
                  ),
                  fluidRow(
                    column(width = 12, plotlyOutput("MorningStiffnessDensityPlot", height = "550px"))
                  ),
                  DTOutput("stats_table_joints")
                )
              )
      ),
      tabItem("Page4",
              sidebarLayout(
                sidebarPanel(
                  width = 3,  # Adjust the width of the sidebar
                  selectInput("TestsBT", label = "Tests:",
                              choices = c(
                                "WBC"="WBC", 
                                "Hgb"="Hgb", 
                                "PLT"="PLT",
                                "Creatinine"="Creatinine", 
                                "FBS"="FBS", 
                                "AST"="AST",
                                "ALT"="ALT", 
                                "ALP"="ALP", 
                                "T.Chol"="T.Chol",
                                "LDL"="LDL", 
                                "HDL"="HDL", 
                                "TG"="TG",
                                "Uric Acid"="Uric acid", 
                                "DAS28"="DAS28", 
                                "CDAI"="CDAI"),
                              selected = "WBC",
                              multiple = TRUE),
                  selectInput("MedicationsBT", label = "Medication:",
                              choices = c(
                                "MTX" = "MTX(DOSE mg)",
                                "SSZ" = "SSZ(DOSE mg)",
                                "Leflunomide" = "Leflunomide(DOSE mg)",
                                "HCQ" = "HCQ(DOSE mg)",
                                "Imuran" = "Imuran(DOSE mg)",
                                "Cyclosporine" = "Cyclosporine(DOSE mg)",
                                "Infliximab" = "Infliximab(DOSE mg)",
                                "Etanercept" = "Etanercept(DOSE mg)",
                                "Adalimumab" = "Adalimumab(DOSE mg)",
                                "Rituximab" = "Rituximab(DOSE mg)",
                                "Abatacept" = "Abatacept(DOSE mg)",
                                "Tociluzumab" = "Tociluzumab(DOSE mg)",
                                "Certolizumab" = "Certolizumab(DOSE mg)",
                                "Tofacitinib" = "Tofacitinib(DOSE mg)",
                                "Golimumab" = "Golimumab(DOSE mg)",
                                "Baricitinib" = "Baricitinib(DOSE mg)",
                                "Amgevita" = "Amgevita(DOSE mg)",
                                "Upadacitinib" = "Upadacitinib(Frequency)",
                                "Hyrimoz" = "Hyrimoz(DOSE mg)"
                                ),
                              selected = "Infliximab(DOSE mg)"),
                  selectInput("GenderInputBT", label = "Gender:", 
                              choices = c("Male" ="male","Female" = "female"),
                              selected = "All", multiple = TRUE),
                  selectInput("NationalityInputBT",
                              label = "Nationality:",
                              choices = c(unique(df$`Nationality`)),
                              selected = "All", multiple = TRUE),
                  selectInput("RFInputBT", label = "RF:", 
                              choices = c("Positive", "Negative"), 
                              selected = "All", multiple = TRUE),
                  selectInput("AntiCCPInputBT", label = "ANTI CCP:", 
                              choices = c("Positive", "Negative"), 
                              selected = "All", multiple = TRUE),
                  selectInput("ANAInputBT", label = "ANA:", 
                              choices = c("Positive", "Negative"), 
                              selected = "All", multiple = TRUE),
                  selectInput("RheumatoidNodulesInputBT", label = "Rheumatoid Nodules:", 
                              choices = c("Yes", "No"), 
                              selected = "All", multiple = TRUE),
                  selectInput("SICCASymptomsInputBT", label = "SICCA Symptoms:", 
                              choices = c("Yes", "No"), 
                              selected = "All", multiple = TRUE),
                  dateRangeInput("DateInputBT", label = "Date Range:",
                                 start = min(df$`Visit Date`),
                                 end = max(df$`Visit Date`),
                                 format = "yyyy-mm-dd"),
                  sliderInput("AgeSliderInputBT", label = "Age Range:",
                              min = min(df$Age), max = max(df$Age),
                              value = c(min(df$Age), max(df$Age)), step = 1),
                  sliderInput("DiseaseDurationSliderInputBT", label = "Disease Duration Range:",
                              min = min(df$`Disease Duration`), max = max(df$`Disease Duration`),
                              value = c(min(df$`Disease Duration`), max(df$`Disease Duration`)), step = 1),
                  sliderInput("BMISliderInputBT", label = "BMI Range:",
                              min = min(df$BMI), max = max(df$BMI),
                              value = c(min(df$BMI), max(df$BMI)), step = 1),
                  radioButtons("FilterLogicBT", label = "Filter Logic for Medical Conditions:",
                               choices = c("Intersection", "Union"), selected = "Union"),
                  checkboxGroupInput("MedicalConditionsInputBT", label = "Medical Conditions:",
                                     choices = c(
                                       "Asthma" = "MedCond-Visits Baseline-B.Asthma",
                                       "CAD" = "MedCond-Visits Baseline-CAD",
                                       "Osteoporosis" = "MedCond-Visits Baseline-Osteoporosis",
                                       "DM" = "MedCond-Visits Baseline-DM",
                                       "Haemoglobinopathy" = "MedCond-Visits Baseline-Haemoglobinopathy",
                                       "HBV" = "MedCond-Visits Baseline-HBV",
                                       "HCV" = "MedCond-Visits Baseline-HCV",
                                       "Hyperlipidemia" = "MedCond-Visits Baseline-Hyperlipidemia",
                                       "Hypertension" = "MedCond-Visits Baseline-Hypertension",
                                       "Thyroid Disease" = "MedCond-Visits Baseline-Thyroid Disease",
                                       "OA" = "MedCond-Visits Baseline-OA",
                                       "PUD" = "MedCond-Visits Baseline-PUD",
                                       "ILD" = "MedCond-Visits Baseline-ILD"),
                                     selected = NULL),
                  downloadButton("DownloadDataBT", "Download")),
                mainPanel(
                  width = 9,
                  fluidRow(
                    plotlyOutput("BloodTestsLinePlot", height = "550px"),
                    DTOutput("BloodTestsTable")
                  )
                )
              )
      ),
      tabItem("Page5",
              sidebarLayout(
                sidebarPanel(
                  width = 3,  # Adjust the width of the sidebar
                  selectInput("GenderInputMU", label = "Gender:", 
                              choices = c("Male" ="male","Female" = "female"),
                              selected = "All", multiple = TRUE),
                  selectInput("NationalityInputMU", label = "Nationality:", 
                              choices = c(unique(df$`Nationality`)),
                              selected = "All", multiple = TRUE),
                  selectInput("RFInputMU", label = "RF:", 
                              choices = c("Positive", "Negative"), 
                              selected = "All", multiple = TRUE),
                  selectInput("AntiCCPInputMU", label = "ANTI CCP:", 
                              choices = c("Positive", "Negative"), 
                              selected = "All", multiple = TRUE),
                  selectInput("ANAInputMU", label = "ANA:", 
                              choices = c("Positive", "Negative"), 
                              selected = "All", multiple = TRUE),
                  selectInput("RheumatoidNodulesInputMU", label = "Rheumatoid Nodules:", 
                              choices = c("Yes", "No"), 
                              selected = "All", multiple = TRUE),
                  selectInput("SICCASymptomsInputMU", label = "SICCA Symptoms:", 
                              choices = c("Yes", "No"), 
                              selected = "All", multiple = TRUE),
                  dateRangeInput("DateInputMU", label = "Date Range:",
                                 start = min(df$`Visit Date`),
                                 end = max(df$`Visit Date`),
                                 format = "yyyy-mm-dd"),
                  sliderInput("AgeSliderInputMU", label = "Age Range:",
                              min = min(df$Age), max = max(df$Age),
                              value = c(min(df$Age), max(df$Age)), step = 1),
                  sliderInput("DiseaseDurationSliderInputMU", label = "Disease Duration Range:",
                              min = min(df$`Disease Duration`), max = max(df$`Disease Duration`),
                              value = c(min(df$`Disease Duration`), max(df$`Disease Duration`)), step = 1),
                  sliderInput("BMISliderInputMU", label = "BMI Range:",
                              min = min(df$BMI), max = max(df$BMI),
                              value = c(min(df$BMI), max(df$BMI)), step = 1),
                  sliderInput("DAS28SliderInputMU", label = "DAS28 Value:",
                              min = min(df$`DAS28`),
                              max = max(df$`DAS28`),
                              value = c(min(df$`DAS28`), max(df$`DAS28`)),
                              step = 0.1),
                  sliderInput("CDAISliderInputMU", label = "CDAI Value:",
                              min = min(df$`CDAI`),
                              max = max(df$`CDAI`),
                              value = c(min(df$`CDAI`), max(df$`CDAI`)),
                              step = 0.1),
                  radioButtons("MedicalConditionFilterLogicMU", label = "Filter Logic for Medical Conditions:", 
                               choices = c("Intersection", "Union"), selected = "Union"),
                  checkboxGroupInput("MedicalConditionsInputMU", label = "Medical Conditions:",
                                     choices = c(
                                       "Asthma" = "MedCond-Visits Baseline-B.Asthma",
                                       "CAD" = "MedCond-Visits Baseline-CAD",
                                       "Osteoporosis" = "MedCond-Visits Baseline-Osteoporosis",
                                       "DM" = "MedCond-Visits Baseline-DM",
                                       "Haemoglobinopathy" = "MedCond-Visits Baseline-Haemoglobinopathy",
                                       "HBV" = "MedCond-Visits Baseline-HBV",
                                       "HCV" = "MedCond-Visits Baseline-HCV",
                                       "Hyperlipidemia" = "MedCond-Visits Baseline-Hyperlipidemia",
                                       "Hypertension" = "MedCond-Visits Baseline-Hypertension",
                                       "Thyroid Disease" = "MedCond-Visits Baseline-Thyroid Disease",
                                       "OA" = "MedCond-Visits Baseline-OA",
                                       "PUD" = "MedCond-Visits Baseline-PUD",
                                       "ILD" = "MedCond-Visits Baseline-ILD"),
                                     selected = NULL),
                  radioButtons("MedicationFilterLogicMU", label = "Filter Logic for Medication:", 
                               choices = c("Intersection", "Union"), selected = "Union"),
                  checkboxGroupInput("MedicationInputMU", label = "Medication:",
                                     choices = c(
                                       "MTX" = "MTX(DOSE mg)",
                                       "SSZ" = "SSZ(DOSE mg)",
                                       "Leflunomide" = "Leflunomide(DOSE mg)",
                                       "HCQ" = "HCQ(DOSE mg)",
                                       "Imuran" = "Imuran(DOSE mg)",
                                       "Cyclosporine" = "Cyclosporine(DOSE mg)",
                                       "Infliximab" = "Infliximab(DOSE mg)",
                                       "Etanercept" = "Etanercept(DOSE mg)",
                                       "Adalimumab" = "Adalimumab(DOSE mg)",
                                       "Rituximab" = "Rituximab(DOSE mg)",
                                       "Abatacept" = "Abatacept(DOSE mg)",
                                       "Tociluzumab" = "Tociluzumab(DOSE mg)",
                                       "Certolizumab" = "Certolizumab(DOSE mg)",
                                       "Tofacitinib" = "Tofacitinib(DOSE mg)",
                                       "Golimumab" = "Golimumab(DOSE mg)",
                                       "Baricitinib" = "Baricitinib(DOSE mg)",
                                       "Amgevita" = "Amgevita(DOSE mg)",
                                       "Upadacitinib" = "Upadacitinib(Frequency)",
                                       "Hyrimoz" = "Hyrimoz(DOSE mg)"),
                                     selected = NULL),
                  checkboxGroupInput("TreatmentCheckbox",
                                     label = "Treatment Type:",
                                     choices = unique(df$Treatment),
                                     selected = NULL),
                  checkboxGroupInput("BiologicsCheckbox", "Biologics Type:", 
                                     choices = unique(df$BioType),
                                     selected = NULL),
                  downloadButton("DownloadDataMU", "Download")),
                mainPanel(
                  width = 9,
                  fluidRow(
                    uiOutput("InfoBoxesMU1")
                  ),
                  fluidRow(
                    uiOutput("InfoBoxesMU2")
                  ),
                  fluidRow(
                    column(width = 6, plotlyOutput("DMARDSPieChart")),
                    column(width = 6, plotlyOutput("BiologicsPieChart"))
                  ),
                  tags$style(HTML(".spacer { height: 30px; }")),  # Adding space between rows
                  fluidRow(
                    column(width = 12, div(class = "spacer"))
                  ),
                  fluidRow(
                    # Tabset for DMARD
                    tabsetPanel(
                      tabPanel("DMARDs - Plot", plotlyOutput("DMARDsBarChart", height = "550px")),
                      tabPanel("DMARDs - Table", DT::dataTableOutput("DMARDsTable"))
                    )
                  ),
                  tags$div(style = "height: 30px;"),  # Spacer row
                  fluidRow(
                    # Tabset for biologic
                    tabsetPanel(
                      tabPanel("Biologics - Plot", plotlyOutput("BiologicsBarChart", height = "550px")),
                      tabPanel("Biologics - Table", DT::dataTableOutput("BiologicsTable"))
                    )
                  )
                )
              )
      ),
      tabItem("Page6",
              sidebarLayout(
                sidebarPanel(
                  width = 3,  # Adjust the width of the sidebar
                  selectInput('DependentVariableLOGISTIC', 'Choose Dependent Variable',
                              choices = c("DAS28"="DAS28BinaryGroup", 
                                          "CDAI"="CDAIBinaryGroup"),
                              selected = "CDAI"),
                  selectInput('IndependentVariablesLOGISTIC', 'Choose Independent Variables',
                              choices = setdiff(colnames(df), excluded_columns),
                              selected = c("Joint Pain",
                                           "Joint Swelling", 
                                           "Tender Black Joints",
                                           "Tender White Joints",
                                           "Swollen Black Joints",
                                           "Swollen White Joints"),
                              multiple = TRUE),
                  selectInput("GenderInputLOGISTIC", label = "Gender:", 
                              choices = c("Male" ="male","Female" = "female"),
                              selected = "All", multiple = TRUE),
                  selectInput("NationalityInputLOGISTIC", label = "Nationality:", 
                              choices = c(unique(df$`Nationality`)),
                              selected = "All", multiple = TRUE),
                  selectInput("RFInputLOGISTIC", label = "RF:", 
                              choices = c("Positive", "Negative"), 
                              selected = "All", multiple = TRUE),
                  selectInput("AntiCCPInputLOGISTIC", label = "ANTI CCP:", 
                              choices = c("Positive", "Negative"), 
                              selected = "All", multiple = TRUE),
                  selectInput("anaInputLOGISTIC", label = "ANA:", 
                              choices = c("Positive", "Negative"), 
                              selected = "All", multiple = TRUE),
                  selectInput("RheumatoidNodulesInputLOGISTIC", label = "Rheumatoid Nodules:", 
                              choices = c("Yes", "No"), 
                              selected = "All", multiple = TRUE),
                  selectInput("SICCASymptomsInputLOGISTIC", label = "SICCA Symptoms:", 
                              choices = c("Yes", "No"), 
                              selected = "All", multiple = TRUE),
                  dateRangeInput("DateInputLOGISTIC", label = "Date Range:",
                                 start = min(df$`Visit Date`),
                                 end = max(df$`Visit Date`),
                                 format = "yyyy-mm-dd"),
                  sliderInput("AgeSliderInputLOGISTIC", label = "Age Range:",
                              min = min(df$Age), max = max(df$Age),
                              value = c(min(df$Age), max(df$Age)), step = 1),
                  sliderInput("DiseaseDurationSliderInputLOGISTIC", label = "Disease Duration Range:",
                              min = min(df$`Disease Duration`), max = max(df$`Disease Duration`),
                              value = c(min(df$`Disease Duration`), max(df$`Disease Duration`)), step = 1),
                  sliderInput("BMISliderInputLOGISTIC", label = "BMI Range:",
                              min = min(df$BMI), max = max(df$BMI),
                              value = c(min(df$BMI), max(df$BMI)), step = 1),
                  sliderInput("DAS28SliderInputLOGISTIC", label = "DAS28 Value:",
                              min = min(df$`DAS28`),
                              max = max(df$`DAS28`),
                              value = c(min(df$`DAS28`), max(df$`DAS28`)),
                              step = 0.1),
                  sliderInput("CDAISliderInputLOGISTIC", label = "CDAI Value:",
                              min = min(df$`CDAI`),
                              max = max(df$`CDAI`),
                              value = c(min(df$`CDAI`), max(df$`CDAI`)),
                              step = 0.1),
                  radioButtons("FilterLogicLOGISTIC", label = "Filter Logic for Medical Conditions:", 
                               choices = c("Intersection", "Union"), selected = "Union"),
                  checkboxGroupInput("MedicalConditionsInputLOGISTIC", label = "Medical Conditions:",
                                     choices = c(
                                       "Asthma" = "MedCond-Visits Baseline-B.Asthma",
                                       "CAD" = "MedCond-Visits Baseline-CAD",
                                       "Osteoporosis" = "MedCond-Visits Baseline-Osteoporosis",
                                       "DM" = "MedCond-Visits Baseline-DM",
                                       "Haemoglobinopathy" = "MedCond-Visits Baseline-Haemoglobinopathy",
                                       "HBV" = "MedCond-Visits Baseline-HBV",
                                       "HCV" = "MedCond-Visits Baseline-HCV",
                                       "Hyperlipidemia" = "MedCond-Visits Baseline-Hyperlipidemia",
                                       "Hypertension" = "MedCond-Visits Baseline-Hypertension",
                                       "Thyroid Disease" = "MedCond-Visits Baseline-Thyroid Disease",
                                       "OA" = "MedCond-Visits Baseline-OA",
                                       "PUD" = "MedCond-Visits Baseline-PUD",
                                       "ILD" = "MedCond-Visits Baseline-ILD"),
                                     selected = NULL),
                  downloadButton("DownloadDataLOGISTIC", "Download")),
                mainPanel(
                  width = 9,
                  fluidRow(
                    uiOutput("InfoBoxesLOGISTIC")
                  ),
                  plotlyOutput("CoefficientPlotLOGISTIC", height = "550px"),
                  verbatimTextOutput("ModelSummaryLOGISTIC")
                )
              )
      ),
      tabItem("Page7",
              sidebarLayout(
                sidebarPanel(
                  width = 3,  # Adjust the width of the sidebar
                  selectInput('DependentVariableLINEAR', 'Choose Dependent Variable',
                              choices = c("DAS28"="DAS28", 
                                          "CDAI"="CDAI"),
                              selected = "CDAI"),
                  selectInput('IndependentVariablesLINEAR', 'Choose Independent Variables',
                              choices = setdiff(colnames(df), excluded_columns),
                              selected = c("Joint Pain",
                                           "Joint Swelling", 
                                           "Tender Black Joints",
                                           "Tender White Joints",
                                           "Swollen Black Joints",
                                           "Swollen White Joints"),
                              multiple = TRUE),
                  selectInput("GenderInputLINEAR", label = "Gender:", 
                              choices = c("Male" ="male","Female" = "female"),
                              selected = "All", multiple = TRUE),
                  selectInput("NationalityInputLINEAR", label = "Nationality:", 
                              choices = c(unique(df$`Nationality`)),
                              selected = "All", multiple = TRUE),
                  selectInput("RFInputLINEAR", label = "RF:", 
                              choices = c("Positive", "Negative"), 
                              selected = "All", multiple = TRUE),
                  selectInput("AntiCCPInputLINEAR", label = "ANTI CCP:", 
                              choices = c("Positive", "Negative"), 
                              selected = "All", multiple = TRUE),
                  selectInput("ANAInputLINEAR", label = "ANA:", 
                              choices = c("Positive", "Negative"), 
                              selected = "All", multiple = TRUE),
                  selectInput("RheumatoidNodulesInputLINEAR", label = "Rheumatoid Nodules:", 
                              choices = c("Yes", "No"), 
                              selected = "All", multiple = TRUE),
                  selectInput("SICCASymptomsInputLINEAR", label = "SICCA Symptoms:", 
                              choices = c("Yes", "No"), 
                              selected = "All", multiple = TRUE),
                  dateRangeInput("DateInputLINEAR", label = "Date Range:",
                                 start = min(df$`Visit Date`),
                                 end = max(df$`Visit Date`),
                                 format = "yyyy-mm-dd"),
                  sliderInput("AgeSliderInputLINEAR", label = "Age Range:",
                              min = min(df$Age), max = max(df$Age),
                              value = c(min(df$Age), max(df$Age)), step = 1),
                  sliderInput("DiseaseDurationSliderInputLINEAR", label = "Disease Duration Range:",
                              min = min(df$`Disease Duration`), max = max(df$`Disease Duration`),
                              value = c(min(df$`Disease Duration`), max(df$`Disease Duration`)), step = 1),
                  sliderInput("BMISliderInputLINEAR", label = "BMI Range:",
                              min = min(df$BMI), max = max(df$BMI),
                              value = c(min(df$BMI), max(df$BMI)), step = 1),
                  sliderInput("DAS28SliderInputLINEAR", label = "DAS28 Value:",
                              min = min(df$`DAS28`),
                              max = max(df$`DAS28`),
                              value = c(min(df$`DAS28`), max(df$`DAS28`)),
                              step = 0.1),
                  sliderInput("CDAISliderInputLINEAR", label = "CDAI Value:",
                              min = min(df$`CDAI`),
                              max = max(df$`CDAI`),
                              value = c(min(df$`CDAI`), max(df$`CDAI`)),
                              step = 0.1),
                  radioButtons("FilterLogicLINEAR", label = "Filter Logic for Medical Conditions:", 
                               choices = c("Intersection", "Union"), selected = "Union"),
                  checkboxGroupInput("MedicalConditionsInputLINEAR", label = "Medical Conditions:",
                                     choices = c(
                                       "Asthma" = "MedCond-Visits Baseline-B.Asthma",
                                       "CAD" = "MedCond-Visits Baseline-CAD",
                                       "Osteoporosis" = "MedCond-Visits Baseline-Osteoporosis",
                                       "DM" = "MedCond-Visits Baseline-DM",
                                       "Haemoglobinopathy" = "MedCond-Visits Baseline-Haemoglobinopathy",
                                       "HBV" = "MedCond-Visits Baseline-HBV",
                                       "HCV" = "MedCond-Visits Baseline-HCV",
                                       "Hyperlipidemia" = "MedCond-Visits Baseline-Hyperlipidemia",
                                       "Hypertension" = "MedCond-Visits Baseline-Hypertension",
                                       "Thyroid Disease" = "MedCond-Visits Baseline-Thyroid Disease",
                                       "OA" = "MedCond-Visits Baseline-OA",
                                       "PUD" = "MedCond-Visits Baseline-PUD",
                                       "ILD" = "MedCond-Visits Baseline-ILD"),
                                     selected = NULL),
                  downloadButton("DownloadDataLINEAR", "Download")),
                mainPanel(
                  width = 9,  # Adjust the width of the sidebar
                  fluidRow(
                    uiOutput("InfoBoxesLINEAR")
                  ),
                  plotlyOutput("PredictedActualPlotLINEAR", height = "550px"),
                  plotlyOutput("CoefficientPlotLINEAR", height = "550px"),
                  verbatimTextOutput("ModelSummaryLINEAR")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$HospitalPatientTrendsPlot <- renderPlotly({
    df_plot <- df %>%
      mutate(Year = format(as.Date(`Visit Entry Date`), "%Y")) %>%
      filter(!is.na(Year)) %>%
      group_by(Hospital = `Hospital`, Year) %>%
      summarise(Patients = n_distinct(`Visit Entry Date`)) %>%
      ungroup()
    
    p <- ggplot(df_plot, aes(x = Year, y = Patients, fill = Hospital)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Annual Patients Visits per Hospital",
           x = "Year",
           y = "Number of Patients Visits") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$NewPatientsPerHospitalPerYearTable <- renderDT({
    df_table <- df %>%
      mutate(Year = format(as.Date(`Visit Entry Date`), "%Y")) %>%
      filter(!is.na(Year)) %>%
      group_by(Hospital = `Hospital`, Year) %>%
      summarise(`Patients Entered` = n_distinct(`Visit Entry Date`)) %>%
      ungroup()
    
    datatable(df_table, 
              options = list(pageLength = 10, 
                             autoWidth = TRUE, 
                             dom = 'Bfrtip', 
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })
  
  output$RheumatologistPatientTrendsPlot <- renderPlotly({
    group_col <- ifelse(input$group_by == "Rheumatologist", "Rheumatologist", "Data Entry By")
    group_label <- ifelse(input$group_by == "Rheumatologist", "Patients Trend for", "Patients Entered by")
    y_title <- ifelse(input$group_by == "Rheumatologist", "Number of Patients", "Number of Patients Entered")
    
    df_rheumatologist <- df %>%
      filter(!!sym(group_col) == input$rheumatologist) %>%
      mutate(Year = format(as.Date(`Visit Entry Date`), "%Y")) %>%
      filter(!is.na(Year)) %>%
      group_by(Year) %>%
      summarise(Patients = n_distinct(`Visit Entry Date`)) %>%
      ungroup()
    
    p <- ggplot(df_rheumatologist, aes(x = Year, y = Patients, group = 1)) +
      geom_line(color = "#4393C3") +
      geom_point(color = "#B2182B") +
      labs(title = paste(group_label, input$rheumatologist),
           x = "Year",
           y = y_title) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$RheumatologistPatientTrendsTable <- renderDT({
    group_col <- ifelse(input$group_by == "Rheumatologist", "Rheumatologist", "Data Entry By")
    column_name <- ifelse(input$group_by == "Rheumatologist", "Patients", "Patients Entered")
    
    df_rheumatologist <- df %>%
      filter(!!sym(group_col) == input$rheumatologist) %>%
      mutate(Year = format(as.Date(`Visit Entry Date`), "%Y")) %>%
      filter(!is.na(Year)) %>%
      group_by(Year) %>%
      summarise(!!column_name := n_distinct(`Visit Entry Date`)) %>%
      ungroup()
    
    datatable(df_rheumatologist, 
              options = list(pageLength = 10, 
                             autoWidth = TRUE, 
                             dom = 'Bfrtip', 
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })
  
  output$RheumatologistPatientDistributionPlot <- renderPlotly({
    group_label <- ifelse(input$group_by2 == "Rheumatologist", "Patient Distribution", "Patients Entered")
    
    df_year <- df %>%
      mutate(Year = format(as.Date(`Visit Entry Date`), "%Y")) %>%
      filter(Year == input$year) %>%
      group_by(!!sym(input$group_by2)) %>%
      summarise(`Patients Entered` = n_distinct(`Visit Entry Date`)) %>%
      arrange(desc(`Patients Entered`)) %>%
      ungroup()
    
    p <- ggplot(df_year, aes(x = reorder(!!sym(input$group_by2), -`Patients Entered`), y = `Patients Entered`)) +
      geom_bar(stat = "identity", fill = "#4393C3") +
      coord_flip() +
      labs(title = paste(group_label, "in", input$year),
           x = input$group_by2,
           y = "Number of Patients") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$RheumatologistPatientDistributionTable <- renderDT({
    column_name <- ifelse(input$group_by2 == "Rheumatologist", "Patients", "Patients Entered")
    
    df_year <- df %>%
      mutate(Year = format(as.Date(`Visit Entry Date`), "%Y")) %>%
      filter(Year == input$year) %>%
      group_by(!!sym(input$group_by2)) %>%
      summarise(Patients_Entered = n_distinct(`Visit Entry Date`)) %>%
      arrange(desc(Patients_Entered)) %>%
      ungroup()
    
    colnames(df_year) <- c(input$group_by2, column_name)
    
    datatable(df_year, 
              options = list(pageLength = 10, 
                             autoWidth = TRUE, 
                             dom = 'Bfrtip', 
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })
  
  output$InfoBoxesPC <- renderUI({
    # Create info boxes for each category with counts
    info_boxes <- list(
      infoBox(
        "Number of Patients",
        paste("Count: ", n_distinct(df$`User ID`)),
        icon = icon("hospital-user"),
        fill = TRUE,
        width = 4,
        color = "light-blue"
      ),
      infoBox(
        "Number of Visits",
        paste("Count: ", nrow(df)),
        icon = icon("users"),
        fill = TRUE,
        width = 4,
        color = "light-blue"
      ),
      infoBox(
        "Number of Hospitals",
        paste("Count: ", n_distinct(df %>% pull(`Hospital`))),
        icon = icon("hospital", class = "fa-solid"),  # Specify the Font Awesome icon name and class
        width = 4,
        fill = TRUE,
        color = "light-blue"
      )
    )
    
    # Return the info boxes as a UI element
    fluidRow(info_boxes)
  })
  
  # Render the interactive treemap
  output$TreemapPC <- renderPlotly({
    data_summary <- df %>%
      group_by(`Nationality`) %>%
      summarize(n_rows = n_distinct(`User ID`))
    
    # Filter out rows with zero observations
    data_summary <- data_summary %>%
      filter(n_rows > 0)
    
    # Calculate percentage and round to the nearest whole number
    data_summary <- data_summary %>%
      mutate(percentage = round(n_rows / sum(n_rows) * 100))
    
    # Create the interactive treemap using plot_ly with explicit colors
    interactive_treemap <- plot_ly(
      data_summary,
      labels = ~paste(`Nationality`),
      parents = ~"Nationalities",
      values = ~n_rows,
      text = ~paste("Count: ", n_rows, "<br>Percentage: ", percentage, "%"),
      type = "treemap",
      hoverinfo = "text"
    )
    
    # Set layout options
    layout_options <- list(
      margin = list(l = 0, r = 0, b = 0, t = 0),
      title = "Responses by Nationality"
    )
    
    # Display the interactive treemap
    interactive_treemap %>% layout(layout_options)
  })
  
  # Define the order of BMI categories based on the colors
  ordered_categories <- c("Underweight", "Healthy weight", "Overweight", "Obesity", "Severe Obesity")
  
  # Count unique occurrences of each BMI category based on distinct user IDs
  BMI_counts <- df %>%
    filter(!is.na(BMICategory)) %>%  # Exclude rows where BMICategory is null
    distinct(`User ID`, .keep_all = TRUE) %>%
    count(BMICategory)
  
  # Reorder the levels of BMICategory according to the specified order
  BMI_counts$BMICategory <- factor(BMI_counts$BMICategory, levels = ordered_categories)
  
  output$BMIPieChartPC <- renderPlotly({
    # Define colors for each BMI category
    colors <- c("Underweight" = "#92C5DE", 
                "Healthy weight" = "#4393C3", 
                "Overweight" = "#D6604D", 
                "Obesity" = "#B2182B", 
                "Severe Obesity" = "#67001F")
    
    # Create the pie chart
    BMIPieChartPC <- plot_ly(
      data = BMI_counts,
      labels = ~BMICategory,
      values = ~n,
      type = "pie",
      marker = list(colors = colors)
    ) %>%
      layout(title = "BMI Categories Distribution")
    
    BMIPieChartPC
  })
  
  # Count unique occurrences of each gender based on distinct user IDs
  gender_counts <- df %>%
    distinct(`User ID`, .keep_all = TRUE) %>%
    count(`Gender`)
  
  # Create a pie chart
  output$GenderPieChartPC <- renderPlotly({
    GenderPieChartPC <- plot_ly(
      data = gender_counts,
      labels = ~`Gender`,
      values = ~n,
      type = "pie",
      marker = list(colors = c("pink", "#4393C3"))  # Define colors for genders
    ) %>%
      layout(title = "Gender Distribution")
    
    GenderPieChartPC
  })
  
  # Render the histogram with Plotly
  output$VisitsHistogramPC <- renderPlotly({
    # Create the histogram with ggplot
    VisitsHistogramPC <- ggplot(df, aes(x = `Visit Number`)) +
      geom_histogram(binwidth = 1, fill = "#4393C3", color = "black") +  # Adjust binwidth as needed
      labs(title = "Visits Distribution", x = "Visit Number", y = "Frequency") +
      theme_minimal()
    
    # Convert ggplot to a Plotly object
    plotly_histogram <- ggplotly(VisitsHistogramPC)
    
    # Return the Plotly object
    plotly_histogram
  })
  
  output$DiseaseDurationBoxplotPC <- renderPlotly({
    DiseaseDurationBoxplotPC <- plot_ly(
      data = df,
      x = ~`Disease Duration`,
      type = "box",
      marker = list(color = "#4393C3")  # Set the color of the box plot
    ) %>%
      layout(
        title = "Disease Duration Distribution",
        xaxis = list(title = "Disease Duration (years)"),
        yaxis = list(title = "", showticklabels = FALSE)  # Remove y-axis and its labels
      )
    
    DiseaseDurationBoxplotPC
  })
  
  
  # Disease infomrmation page
  
  filtered_data_DI  <- reactive({
    data <- df
    
    if (!is.null(input$GenderInputDI) && !('All' %in% input$GenderInputDI)) {
      data <- data[data$`Gender` %in% input$GenderInputDI, ]
    }
    
    if (!is.null(input$NationalityInputDI) && !('All' %in% input$NationalityInputDI)) {
      data <- data[data$`Nationality` %in% input$NationalityInputDI, ]
    }
    
    if (!is.null(input$MedicalConditionsInputDI)) {
      if (input$FilterLogicDI == "Intersection") {
        for (condition in input$MedicalConditionsInputDI) {
          data <- data[data[[condition]] == 'Yes', ]
        }
      } else { # Union
        conditions <- sapply(input$MedicalConditionsInputDI, function(condition) data[[condition]] == 'Yes')
        data <- data[rowSums(conditions) > 0, ]
      }
    }
    
    if (!is.null(input$RFInputDI) && !('All' %in% input$RFInputDI)) {
      data <- data[data$`RF` %in% input$RFInputDI, ]
    }
    
    if (!is.null(input$AntiCCPInputDI) && !('All' %in% input$AntiCCPInputDI)) {
      data <- data[data$`ANTI CCP` %in% input$AntiCCPInputDI, ]
    }
    
    if (!is.null(input$ANAInputDI) && !('All' %in% input$ANAInputDI)) {
      data <- data[data$`ANA` %in% input$ANAInputDI, ]
    }
    
    if (!is.null(input$RheumatoidNodulesInputDI) && !('All' %in% input$RheumatoidNodulesInputDI)) {
      data <- data[data$`Rheumatoid Nodules` %in% input$RheumatoidNodulesInputDI, ]
    }
    
    if (!is.null(input$SICCASymptomsInputDI) && !('All' %in% input$SICCASymptomsInputDI)) {
      data <- data[data$`SICCA Symptoms` %in% input$SICCASymptomsInputDI, ]
    }
    
    date_range <- as.POSIXct(input$DateInputDI)
    
    if (!is.null(input$DateInputDI)) {
      data <- data[data$`Visit Date` >= date_range[1] &
                     data$`Visit Date` <= date_range[2], ]
    }
    
    age_range <- input$AgeSliderInputDI
    data <- data[data$Age >= age_range[1] & data$Age <= age_range[2], ]
    
    DD_range <- input$DiseaseDurationSliderInputDI
    data <- data[data$`Disease Duration` >= DD_range[1] & data$`Disease Duration` <= DD_range[2], ]
    
    das28_range <- input$DAS28SliderInputDI
    data <- data[data$`DAS28` >= das28_range[1] & data$`DAS28` <= das28_range[2], ]
    
    cdai_range <- input$CDAISliderInputDI
    data <- data[ 
      data$`CDAI` >= cdai_range[1] & 
        data$`CDAI` <= cdai_range[2], ]
    
    BMI_range <- input$BMISliderInputDI
    data <- data[data$BMI >= BMI_range[1] & data$BMI <= age_range[2], ]
    
    return(data)
  })
  
  # # Download filtered data when download button is clicked
  # output$DownloadDataDI <- downloadHandler(
  #   filename = function() {
  #     "filtered_data_DI.csv"  # Change the file extension to .csv
  #   },
  #   content = function(file) {
  #     # Perform filtering based on user input
  #     filtered_data <- filtered_data_DI()
  #     
  #     # Write filtered data to CSV file
  #     write.csv(filtered_data, file, row.names = FALSE)  # Use write.csv instead of write.xlsx
  #   }
  # )
  
  output$DownloadDataDI <- downloadHandler(
    filename = function() {
      "filtered_data_DI.xlsx"  # Change the file extension to .xlsx
    },
    content = function(file) {
      # Perform filtering based on user input
      filtered_data <- filtered_data_DI()
      
      # Ensure the Visit Date column is in Date format
      filtered_data <- filtered_data %>%
        mutate(`Visit Date` = as.Date(`Visit Date`))
      
      # Create a dataframe with only the first observation of each unique patient based on the date
      first_observation_data <- filtered_data %>% 
        arrange(`Visit Date`) %>%
        group_by(`User ID`) %>%    # Replace 'patient_id' with the appropriate column name
        slice(1) %>% 
        ungroup()
      
      # Create a dataframe with only the last observation of each unique patient based on the date
      last_observation_data <- filtered_data %>%
        arrange(`Visit Date`) %>%
        group_by(`User ID`) %>%    # Replace 'patient_id' with the appropriate column name
        slice(n()) %>% 
        ungroup()
      
      # Create a list of dataframes to write to Excel
      data_to_write <- list(
        "Filtered Data" = filtered_data, 
        "First Observations" = first_observation_data,
        "Last Observations" = last_observation_data
      )
      
      # Write the dataframes to an Excel file with multiple sheets
      write_xlsx(data_to_write, path = file)
    }
  )
  
  output$plot1 <- renderPlotly({
    variable <- input$variable
    df_filtered <- switch(
      variable,
      "RF" = filtered_data_DI()[!is.na(filtered_data_DI()$`RF`), ],
      "ANTI CCP" = filtered_data_DI()[!is.na(filtered_data_DI()$`ANTI CCP`), ],
      "ANA" = filtered_data_DI()[!is.na(filtered_data_DI()$`ANA`), ],
      "Rheumatoid Nodules" = filtered_data_DI()[!is.na(filtered_data_DI()$`Rheumatoid Nodules`), ],
      "SICCA Symptoms" = filtered_data_DI()[!is.na(filtered_data_DI()$`SICCA Symptoms`), ]
    )
    
    # Assuming the variable has two levels
    counts <- table(df_filtered[[variable]])
    df_plot <- as.data.frame(counts)
    names(df_plot) <- c(variable, "Count")
    
    # Define color palette based on variable
    if (variable %in% c("RF", "ANTI CCP", "ANA")) {
      color_palette <- c("Positive" = "#B2182B", "Negative" = "#4393C3")
    } else if (variable %in% c("Rheumatoid Nodules", "SICCA Symptoms")) {
      color_palette <- c("Yes" = "#B2182B", "No" = "#4393C3")
    }
    
    # Create text for hover tooltip
    df_plot$text <- paste(variable, ": ", rownames(df_plot), ", Count: ", df_plot$Count)
    
    p <- ggplot(df_plot, aes(x = "", y = Count, fill = !!sym(variable), text = text)) +  
      geom_bar(stat = "identity", position = "stack", width = 1) +
      coord_flip() +
      labs(x = NULL, y = "Count", title = paste(variable, "Distribution")) +
      theme_minimal() +
      theme(legend.position = "right") +
      scale_fill_manual(values = color_palette)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot2 <- renderPlotly({
    Variable2 <- input$Variable2
    df_filtered <- switch(
      Variable2,
      "DAS28Group" = filtered_data_DI()[!is.na(filtered_data_DI()$`DAS28Group`), ],
      "CDAIGroup" = filtered_data_DI()[!is.na(filtered_data_DI()$`CDAIGroup`), ]
    )
    
    # Assuming the variable has two levels
    counts <- table(df_filtered[[Variable2]])
    df_plot <- as.data.frame(counts)
    names(df_plot) <- c(Variable2, "Count")
    
    # Define color palette based on variable
    color_palette <- c("remission" = "#4393C3", "low activity" = "#FDDBC7", 
                       "moderate activity" = "#F4A582", "high activity" = "#B2182B")
    
    # Create text for hover tooltip
    df_plot$text <- paste(Variable2, ": ", rownames(df_plot), ", Count: ", df_plot$Count)
    
    p <- ggplot(df_plot, aes(x = "", y = Count, fill = !!sym(Variable2), text = text)) +  
      geom_bar(stat = "identity", position = "stack", width = 1) +
      coord_flip() +
      labs(x = NULL, y = "Count", title = paste(Variable2, "Distribution")) +
      theme_minimal() +
      theme(legend.position = "right") +
      scale_fill_manual(values = color_palette)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot3 <- renderPlotly({
    # Determine which variable to use based on user input
    variable <- input$Variable3
    variable_label <- switch(
      variable,
      "Physician's Global Assessment" = filtered_data_DI()[!is.na(filtered_data_DI()$`Physician's Global Assessment`), ],
      "Patient's Global Assessment" = filtered_data_DI()[!is.na(filtered_data_DI()$`Patient's Global Assessment`), ]
    )
    
    # Check the unique values in the selected column
    unique_values <- unique(filtered_data_DI()[[variable]])
    
    # Create a reversed RdBu palette
    reversed_palette <- rev(brewer.pal(length(unique_values), "RdBu"))
    
    # Create a histogram of the selected variable using Plotly
    plot_ly(filtered_data_DI(), x = ~get(variable), type = 'histogram',
            marker = list(color = reversed_palette, line = list(color = 'black'))) %>%
      layout(title = paste("Distribution of", variable_label),
             xaxis = list(title = variable_label),
             yaxis = list(title = "Frequency"))
  })
  
  # Patients Joint Info Page
  
  filtered_data_JI  <- reactive({
    data <- df
    
    if (!is.null(input$GenderInputJI) && !('All' %in% input$GenderInputJI)) {
      data <- data[data$`Gender` %in% input$GenderInputJI, ]
    }
    
    if (!is.null(input$NationalityInputJI) && !('All' %in% input$NationalityInputJI)) {
      data <- data[data$`Nationality` %in% input$NationalityInputJI, ]
    }
    
    if (!is.null(input$MedicalConditionsInputJI)) {
      if (input$FilterLogicJI == "Intersection") {
        for (condition in input$MedicalConditionsInputJI) {
          data <- data[data[[condition]] == 'Yes', ]
        }
      } else { # Union
        conditions <- sapply(input$MedicalConditionsInputJI, function(condition) data[[condition]] == 'Yes')
        data <- data[rowSums(conditions) > 0, ]
      }
    }
    
    if (!is.null(input$RFInputJI) && !('All' %in% input$RFInputJI)) {
      data <- data[data$`RF` %in% input$RFInputJI, ]
    }
    
    if (!is.null(input$AntiCCPInputJI) && !('All' %in% input$AntiCCPInputJI)) {
      data <- data[data$`ANTI CCP` %in% input$AntiCCPInputJI, ]
    }
    
    if (!is.null(input$ANAInputJI) && !('All' %in% input$ANAInputJI)) {
      data <- data[data$`ANA` %in% input$ANAInputJI, ]
    }
    
    if (!is.null(input$RheumatoidNodulesInputJI) && !('All' %in% input$RheumatoidNodulesInputJI)) {
      data <- data[data$`Rheumatoid Nodules` %in% input$RheumatoidNodulesInputJI, ]
    }
    
    if (!is.null(input$SICCASymptomsInputJI) && !('All' %in% input$SICCASymptomsInputJI)) {
      data <- data[data$`SICCA Symptoms` %in% input$SICCASymptomsInputJI, ]
    }
    
    date_range <- as.POSIXct(input$DateInputJI)
    
    if (!is.null(input$DateInputJI)) {
      data <- data[data$`Visit Date` >= date_range[1] &
                     data$`Visit Date` <= date_range[2], ]
    }
    
    age_range <- input$AgeSliderInputJI
    data <- data[data$Age >= age_range[1] & data$Age <= age_range[2], ]
    
    DD_range <- input$DiseaseDurationSliderInputJI
    data <- data[data$`Disease Duration` >= DD_range[1] & data$`Disease Duration` <= DD_range[2], ]
    
    das28_range <- input$DAS28SliderInputJI
    data <- data[!is.na(data$`DAS28`) & 
                   data$`DAS28` >= das28_range[1] & 
                   data$`DAS28` <= das28_range[2], ]
    
    cdai_range <- input$CDAISliderInputJI
    data <- data[!is.na(data$`CDAI`) & 
                   data$`CDAI` >= cdai_range[1] & 
                   data$`CDAI` <= cdai_range[2], ]
    
    BMI_range <- input$BMISliderInputJI
    data <- data[data$BMI >= BMI_range[1] & data$BMI <= age_range[2], ]
    
    return(data)
  })
  
  # # Download filtered data when download button is clicked
  # output$DownloadDataJI <- downloadHandler(
  #   filename = function() {
  #     "filtered_data_JI.csv"  # Change the file extension to .csv
  #   },
  #   content = function(file) {
  #     # Perform filtering based on user input
  #     filtered_data <- filtered_data_JI()
  #     
  #     # Write filtered data to CSV file
  #     write.csv(filtered_data, file, row.names = FALSE)  # Use write.csv instead of write.xlsx
  #   }
  # )
  
  output$DownloadDataJI <- downloadHandler(
    filename = function() {
      "filtered_data_JI.xlsx"  # Change the file extension to .xlsx
    },
    content = function(file) {
      # Perform filtering based on user input
      filtered_data <- filtered_data_JI()
      
      # Ensure the Visit Date column is in Date format
      filtered_data <- filtered_data %>%
        mutate(`Visit Date` = as.Date(`Visit Date`))
      
      # Create a dataframe with only the first observation of each unique patient based on the date
      first_observation_data <- filtered_data %>% 
        arrange(`Visit Date`) %>%
        group_by(`User ID`) %>%    # Replace 'patient_id' with the appropriate column name
        slice(1) %>% 
        ungroup()
      
      # Create a dataframe with only the last observation of each unique patient based on the date
      last_observation_data <- filtered_data %>%
        arrange(`Visit Date`) %>%
        group_by(`User ID`) %>%    # Replace 'patient_id' with the appropriate column name
        slice(n()) %>% 
        ungroup()
      
      # Create a list of dataframes to write to Excel
      data_to_write <- list(
        "Filtered Data" = filtered_data, 
        "First Observations" = first_observation_data,
        "Last Observations" = last_observation_data
      )
      
      # Write the dataframes to an Excel file with multiple sheets
      write_xlsx(data_to_write, path = file)
    }
  )
  
  # Filter out NA values for Joint Pain
  df_pain_filtered <- reactive({
    df_filtered <- filtered_data_JI()[!is.na(filtered_data_JI()$`Joint Pain`), ]
  })
  
  # Render interactive pie chart for Joint Swelling
  output$JointPainPieChart <- renderPlotly({
    # Calculate counts for each category
    joint_pain_counts <- table(df_pain_filtered()$`Joint Pain`)
    
    colors <- c("No" = "#4393C3", "Occasional" = "#F4A582", "Yes" = "#B2182B")
    
    # Create pie chart
    pie_chart <- plot_ly(
      labels = names(joint_pain_counts),
      values = joint_pain_counts,
      type = "pie",
      marker = list(colors = colors)
    ) %>%
      layout(title = "Joint Pain Distribution")
    
    # Return the pie chart
    pie_chart
  })
  
  # Filter out NA values for Joint Swelling
  df_swelling_filtered <- reactive({
    df_filtered <- filtered_data_JI()[!is.na(filtered_data_JI()$`Joint Swelling`), ]
  })
  
  # Render interactive pie chart for Joint Swelling
  output$JointSwellingPieChart <- renderPlotly({
    # Calculate counts for each category
    joint_swelling_counts <- table(df_swelling_filtered()$`Joint Swelling`)
    
    colors <- c("No" = "#4393C3", "Occasional" = "#F4A582", "Yes" = "#B2182B")
    
    # Create pie chart
    pie_chart <- plot_ly(
      labels = names(joint_swelling_counts),
      values = joint_swelling_counts,
      type = "pie",
      marker = list(colors = colors)
    ) %>%
      layout(title = "Joint Swelling Distribution")
    
    # Return the pie chart
    pie_chart
  })
  
  output$VASHistogram <- renderPlotly({
    # Check the unique values in the VAS score column
    unique_vas <- unique(filtered_data_JI()$`VAS`)
    
    # Create a reversed RdBu palette
    reversed_palette <- rev(brewer.pal(length(unique_vas), "RdBu"))
    
    # Create a histogram of VAS scores using Plotly
    plot_ly(filtered_data_JI(), x = ~`VAS`, type = 'histogram', 
            marker = list(color = reversed_palette, line = list(color = 'black'))) %>%
      layout(title = "VAS Scores Distribution",
             xaxis = list(title = "VAS Score"),
             yaxis = list(title = "Frequency"))
  })
  
  # Filter out values greater than 50
  filtered_df_1 <- reactive({
    filtered_data_JI()[filtered_data_JI()$`Morning Stiffness (mg)` <= 50, ]
  })
  
  # Render the density plot
  output$MorningStiffnessDensityPlot <- renderPlotly({
    # Create a density plot of morning stiffness
    ggplot(filtered_df_1(), aes(x = `Morning Stiffness (mg)`)) +
      geom_density(fill = "#4393C3", color = "black", alpha = 0.5) +
      labs(title = "Morning Stiffness Distribution",
           x = "Morning Stiffness (mg)",
           y = "Density")
  })
  
  # Render the descriptive statistics table
  output$stats_table_joints <- renderDT({
    filtered_data <- filtered_data_JI()  # Include filtered_data_JI as a dependency
    joint_vars <- filtered_data[, c("Tender Black Joints", 
                                    "Tender White Joints",
                                    "Swollen Black Joints",
                                    "Swollen White Joints")]
    
    # Generate descriptive statistics for each joint variable
    stats_joints <- apply(joint_vars, 2, function(x) {
      c(min = round(min(x, na.rm = TRUE), 4), 
        quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 
        mean = round(mean(x, na.rm = TRUE), 4), 
        max = round(max(x, na.rm = TRUE), 4), 
        mode = names(sort(table(x), decreasing = TRUE)[1]),  # Calculate mode
        count = sum(!is.na(x)), 
        NA_count = sum(is.na(x)))
    })
    
    # Convert the statistics to a data frame and transpose it for better readability
    stats_df_joints <- t(as.data.frame(stats_joints))
    
    # Assign meaningful column names
    colnames(stats_df_joints) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max", "Mode", "Count", "NA_count")
    
    datatable(stats_df_joints, options = list(pageLength = nrow(stats_df_joints)))  # Display all rows
  })
  
  # Patients Blood Test Information Page
  
  filtered_data_BT  <- reactive({
    data <- df
    
    if (!is.null(input$GenderInputBT) && !('All' %in% input$GenderInputBT)) {
      data <- data[data$`Gender` %in% input$GenderInputBT, ]
    }
    
    if (!is.null(input$NationalityInputBT) && !('All' %in% input$NationalityInputBT)) {
      data <- data[data$`Nationality` %in% input$NationalityInputBT, ]
    }
    
    if (!is.null(input$MedicalConditionsInputBT)) {
      if (input$FilterLogicBT == "Intersection") {
        for (condition in input$MedicalConditionsInputBT) {
          data <- data[data[[condition]] == 'Yes', ]
        }
      } else { # Union
        conditions <- sapply(input$MedicalConditionsInputBT, function(condition) data[[condition]] == 'Yes')
        data <- data[rowSums(conditions) > 0, ]
      }
    }
    
    if (!is.null(input$RFInputBT) && !('All' %in% input$RFInputBT)) {
      data <- data[data$`RF` %in% input$RFInputBT, ]
    }
    
    if (!is.null(input$AntiCCPInputBT) && !('All' %in% input$AntiCCPInputBT)) {
      data <- data[data$`ANTI CCP` %in% input$AntiCCPInputBT, ]
    }
    
    if (!is.null(input$ANAInputBT) && !('All' %in% input$ANAInputBT)) {
      data <- data[data$`ANA` %in% input$ANAInputBT, ]
    }
    
    if (!is.null(input$RheumatoidNodulesInputBT) && !('All' %in% input$RheumatoidNodulesInputBT)) {
      data <- data[data$`Rheumatoid Nodules` %in% input$RheumatoidNodulesInputBT, ]
    }
    
    if (!is.null(input$SICCASymptomsInputBT) && !('All' %in% input$SICCASymptomsInputBT)) {
      data <- data[data$`SICCA Symptoms` %in% input$SICCASymptomsInputBT, ]
    }
    
    date_range <- as.POSIXct(input$DateInputBT)
    
    if (!is.null(input$DateInputBT)) {
      data <- data[data$`Visit Date` >= date_range[1] &
                     data$`Visit Date` <= date_range[2], ]
    }
    
    age_range <- input$AgeSliderInputBT
    data <- data[data$Age >= age_range[1] & data$Age <= age_range[2], ]
    
    DD_range <- input$DiseaseDurationSliderInputBT
    data <- data[data$`Disease Duration` >= DD_range[1] & data$`Disease Duration` <= DD_range[2], ]
    
    BMI_range <- input$BMISliderInputBT
    data <- data[data$BMI >= BMI_range[1] & data$BMI <= age_range[2], ]
    
    return(data)
  })
  
  # # Download filtered data when download button is clicked
  # output$DownloadDataBT <- downloadHandler(
  #   filename = function() {
  #     "filtered_data_BT.csv"  # Change the file extension to .csv
  #   },
  #   content = function(file) {
  #     # Perform filtering based on user input
  #     filtered_data <- filtered_data_BT()
  #     
  #     # Write filtered data to CSV file
  #     write.csv(filtered_data, file, row.names = FALSE)  # Use write.csv instead of write.xlsx
  #   }
  # )
  
  output$DownloadDataBT <- downloadHandler(
    filename = function() {
      "filtered_data_BT.xlsx"  # Change the file extension to .xlsx
    },
    content = function(file) {
      # Perform filtering based on user input
      filtered_data <- filtered_data_BT()
      
      # Ensure the Visit Date column is in Date format
      filtered_data <- filtered_data %>%
        mutate(`Visit Date` = as.Date(`Visit Date`))
      
      # Create a dataframe with only the first observation of each unique patient based on the date
      first_observation_data <- filtered_data %>% 
        arrange(`Visit Date`) %>%
        group_by(`User ID`) %>%    # Replace 'patient_id' with the appropriate column name
        slice(1) %>% 
        ungroup()
      
      # Create a dataframe with only the last observation of each unique patient based on the date
      last_observation_data <- filtered_data %>%
        arrange(`Visit Date`) %>%
        group_by(`User ID`) %>%    # Replace 'patient_id' with the appropriate column name
        slice(n()) %>% 
        ungroup()
      
      # Create a list of dataframes to write to Excel
      data_to_write <- list(
        "Filtered Data" = filtered_data, 
        "First Observations" = first_observation_data,
        "Last Observations" = last_observation_data
      )
      
      # Write the dataframes to an Excel file with multiple sheets
      write_xlsx(data_to_write, path = file)
    }
  )
  
  output$BloodTestsLinePlot <- renderPlotly({
    tests <- input$TestsBT
    medication <- input$MedicationsBT
    
    df_filtered <- filtered_data_BT() %>%
      filter(!rowSums(is.na(select(., all_of(tests)))), .data[[medication]] == "yes") %>%
      group_by(`Visit Number`) %>%
      summarise(across(all_of(tests), mean, na.rm = TRUE))
    
    df_long <- pivot_longer(df_filtered, cols = -`Visit Number`, names_to = "Test", values_to = "Mean")
    
    p <- ggplot(df_long, aes(x = `Visit Number`, y = Mean, color = Test)) +
      geom_line() +
      labs(x = "Visit Number", y = "Mean", title = paste("Mean of Selected Tests per visit for patients on", medication)) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Render the summary table using DataTables
  output$BloodTestsTable <- DT::renderDataTable({
    blood_tests <- filtered_data_BT() %>%
      select(
        "WBC", "Hgb", "PLT",
        "Creatinine", "FBS", "AST",
        "ALT", "ALP", "T.Chol",
        "LDL", "HDL", "TG",
        "Uric acid"
      )
    
    # Generate descriptive statistics for each blood test
    stats <- apply(blood_tests, 2, function(x) {
      rounded_summary <- round(summary(x), 4)  # Round summary statistics to four decimals
      c(rounded_summary, count = sum(!is.na(x)))
    })
    
    # Convert the statistics to a data frame and transpose it for better readability
    stats_df <- t(as.data.frame(stats))
    
    # Assign meaningful column names
    colnames(stats_df) <- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "NA's", "Count")
    
    DT::datatable(stats_df, options = list(pageLength = nrow(stats_df)))  # Display all rows
  })

  # Patients Medication Usage Page
  
  filtered_data_MU  <- reactive({
    data <- df
    
    if (!is.null(input$GenderInputMU) && !('All' %in% input$GenderInputMU)) {
      data <- data[data$`Gender` %in% input$GenderInputMU, ]
    }
    
    if (!is.null(input$NationalityInputMU) && !('All' %in% input$NationalityInputMU)) {
      data <- data[data$`Nationality` %in% input$NationalityInputMU, ]
    }
    
    if (!is.null(input$MedicalConditionsInputMU)) {
      if (input$MedicalConditionFilterLogicMU == "Intersection") {
        for (condition in input$MedicalConditionsInputMU) {
          data <- data[data[[condition]] == 'Yes', ]
        }
      } else { # Union
        conditions <- sapply(input$MedicalConditionsInputMU, function(condition) data[[condition]] == 'Yes')
        data <- data[rowSums(conditions) > 0, ]
      }
    }
    
    if (!is.null(input$MedicationInputMU)) {
      if (input$MedicationFilterLogicMU == "Intersection") {
        for (medication in input$MedicationInputMU) {
          data <- data[data[[medication]] == 'yes', ]
        }
      } else { # Union
        medications <- sapply(input$MedicationInputMU, function(medication) data[[medication]] == 'yes')
        data <- data[rowSums(medications) > 0, ]
      }
    }
    
    if (!is.null(input$RFInputMU) && !('All' %in% input$RFInputMU)) {
      data <- data[data$`RF` %in% input$RFInputMU, ]
    }
    
    if (!is.null(input$AntiCCPInputMU) && !('All' %in% input$AntiCCPInputMU)) {
      data <- data[data$`ANTI CCP` %in% input$AntiCCPInputMU, ]
    }
    
    if (!is.null(input$ANAInputMU) && !('All' %in% input$ANAInputMU)) {
      data <- data[data$`ANA` %in% input$ANAInputMU, ]
    }
    
    if (!is.null(input$RheumatoidNodulesInputMU) && !('All' %in% input$RheumatoidNodulesInputMU)) {
      data <- data[data$`Rheumatoid Nodules` %in% input$RheumatoidNodulesInputMU, ]
    }
    
    if (!is.null(input$SICCASymptomsInputMU) && !('All' %in% input$SICCASymptomsInputMU)) {
      data <- data[data$`SICCA Symptoms` %in% input$SICCASymptomsInputMU, ]
    }
    
    date_range <- as.POSIXct(input$DateInputMU)
    
    if (!is.null(input$DateInputMU)) {
      data <- data[data$`Visit Date` >= date_range[1] &
                     data$`Visit Date` <= date_range[2], ]
    }
    
    age_range <- input$AgeSliderInputMU
    data <- data[data$Age >= age_range[1] & data$Age <= age_range[2], ]
    
    DD_range <- input$DiseaseDurationSliderInputMU
    data <- data[data$`Disease Duration` >= DD_range[1] & data$`Disease Duration` <= DD_range[2], ]
    
    das28_range <- input$DAS28SliderInputMU
    data <- data[!is.na(data$`DAS28`) & 
                   data$`DAS28` >= das28_range[1] & 
                   data$`DAS28` <= das28_range[2], ]
    
    cdai_range <- input$CDAISliderInputMU
    data <- data[!is.na(data$`CDAI`) & 
                   data$`CDAI` >= cdai_range[1] & 
                   data$`CDAI` <= cdai_range[2], ]
    
    if (!is.null(input$BiologicsCheckbox) && length(input$BiologicsCheckbox) > 0) {
      data <- data[data$BioType %in% input$BiologicsCheckbox, ]
    }
    
    # Filter based on Combination checkbox
    if (!is.null(input$TreatmentCheckbox)) {
      data <- data[data$Treatment %in% input$TreatmentCheckbox, ]
    }
    
    BMI_range <- input$BMISliderInputMU
    data <- data[data$BMI >= BMI_range[1] & data$BMI <= age_range[2], ]
    
    return(data)
  })
  
  filtered_data_MU_total <- reactive({
    data <- df
    
    if (!is.null(input$GenderInputMU) && !('All' %in% input$GenderInputMU)) {
      data <- data[data$`Gender` %in% input$GenderInputMU, ]
    }
    
    if (!is.null(input$NationalityInputMU) && !('All' %in% input$NationalityInputMU)) {
      data <- data[data$`Nationality` %in% input$NationalityInputMU, ]
    }
    
    if (!is.null(input$DAS28InputMU) && !('All' %in% input$DAS28InputMU)) {
      data <- data[data$`DAS28Group` %in% input$DAS28InputMU, ]
    }
    
    if (!is.null(input$CDAIInputMU) && !('All' %in% input$CDAIInputMU)) {
      data <- data[data$`CDAIGroup` %in% input$CDAIInputMU, ]
    }
    
    if (!is.null(input$MedicalConditionsInputMU)) {
      if (input$MedicalConditionFilterLogicMU == "Intersection") {
        for (condition in input$MedicalConditionsInputMU) {
          data <- data[data[[condition]] == 'Yes', ]
        }
      } else { # Union
        conditions <- sapply(input$MedicalConditionsInputMU, function(condition) data[[condition]] == 'Yes')
        data <- data[rowSums(conditions) > 0, ]
      }
    }
    
    if (!is.null(input$MedicationInputMU)) {
      if (input$MedicationFilterLogicMU == "Intersection") {
        for (medication in input$MedicationInputMU) {
          data <- data[data[[medication]] == 'yes', ]
        }
      } else { # Union
        medications <- sapply(input$MedicationInputMU, function(medication) data[[medication]] == 'yes')
        data <- data[rowSums(medications) > 0, ]
      }
    }
    
    if (!is.null(input$RFInputMU) && !('All' %in% input$RFInputMU)) {
      data <- data[data$`RF` %in% input$RFInputMU, ]
    }
    
    if (!is.null(input$AntiCCPInputMU) && !('All' %in% input$AntiCCPInputMU)) {
      data <- data[data$`ANTI CCP` %in% input$AntiCCPInputMU, ]
    }
    
    if (!is.null(input$ANAInputMU) && !('All' %in% input$ANAInputMU)) {
      data <- data[data$`ANA` %in% input$ANAInputMU, ]
    }
    
    if (!is.null(input$RheumatoidNodulesInputMU) && !('All' %in% input$RheumatoidNodulesInputMU)) {
      data <- data[data$`Rheumatoid Nodules` %in% input$RheumatoidNodulesInputMU, ]
    }
    
    if (!is.null(input$SICCASymptomsInputMU) && !('All' %in% input$SICCASymptomsInputMU)) {
      data <- data[data$`SICCA Symptoms` %in% input$SICCASymptomsInputMU, ]
    }
    
    date_range <- as.POSIXct(input$DateInputMU)
    
    if (!is.null(input$DateInputMU)) {
      data <- data[data$`Visit Date` >= date_range[1] &
                     data$`Visit Date` <= date_range[2], ]
    }
    
    age_range <- input$AgeSliderInputMU
    data <- data[data$Age >= age_range[1] & data$Age <= age_range[2], ]
    
    DD_range <- input$DiseaseDurationSliderInputMU
    data <- data[data$`Disease Duration` >= DD_range[1] & data$`Disease Duration` <= DD_range[2], ]
    
    das28_range <- input$DAS28SliderInputMU
    data <- data[!is.na(data$`DAS28`) & 
                   data$`DAS28` >= das28_range[1] & 
                   data$`DAS28` <= das28_range[2], ]
    
    cdai_range <- input$CDAISliderInputMU
    data <- data[!is.na(data$`CDAI`) & 
                   data$`CDAI` >= cdai_range[1] & 
                   data$`CDAI` <= cdai_range[2], ]
    
    if (!is.null(input$BiologicsCheckbox) && length(input$BiologicsCheckbox) > 0) {
      data <- data[data$BioType %in% input$BiologicsCheckbox, ]
    }
    
    # Filter based on Combination checkbox
    if (!is.null(input$TreatmentCheckbox)) {
      data <- data[data$Treatment %in% input$TreatmentCheckbox, ]
    }
    
    BMI_range <- input$BMISliderInputMU
    data <- data[data$BMI >= BMI_range[1] & data$BMI <= age_range[2], ]
    
    return(data)
  })
  
  # # Download filtered data when download button is clicked
  # output$DownloadDataMU <- downloadHandler(
  #   filename = function() {
  #     "filtered_data_MU.csv"  # Change the file extension to .csv
  #   },
  #   content = function(file) {
  #     # Perform filtering based on user input
  #     filtered_data <- filtered_data_MU()
  #     
  #     # Write filtered data to CSV file
  #     write.csv(filtered_data, file, row.names = FALSE)  # Use write.csv instead of write.xlsx
  #   }
  # )
  
  output$DownloadDataMU <- downloadHandler(
    filename = function() {
      "filtered_data_MU.xlsx"  # Change the file extension to .xlsx
    },
    content = function(file) {
      # Perform filtering based on user input
      filtered_data <- filtered_data_MU()
      
      # Ensure the Visit Date column is in Date format
      filtered_data <- filtered_data %>%
        mutate(`Visit Date` = as.Date(`Visit Date`))
      
      # Create a dataframe with only the first observation of each unique patient based on the date
      first_observation_data <- filtered_data %>% 
        arrange(`Visit Date`) %>%
        group_by(`User ID`) %>%    # Replace 'patient_id' with the appropriate column name
        slice(1) %>% 
        ungroup()
      
      # Create a dataframe with only the last observation of each unique patient based on the date
      last_observation_data <- filtered_data %>%
        arrange(`Visit Date`) %>%
        group_by(`User ID`) %>%    # Replace 'patient_id' with the appropriate column name
        slice(n()) %>% 
        ungroup()
      
      # Create a list of dataframes to write to Excel
      data_to_write <- list(
        "Filtered Data" = filtered_data, 
        "First Observations" = first_observation_data,
        "Last Observations" = last_observation_data
      )
      
      # Write the dataframes to an Excel file with multiple sheets
      write_xlsx(data_to_write, path = file)
    }
  )
  
  # Render the info boxes for filtered data count, visits count, and unique hospitals count
  output$InfoBoxesMU1 <- renderUI({
    
    # Create info boxes for each category with counts and percentages
    info_boxes <- list(
      infoBox(
        "Number of Patients",
        paste("Count: ", n_distinct(filtered_data_MU()$`User ID`)),
        icon = icon("hospital-user"),
        fill=TRUE,
        width = 4, 
        color = "light-blue"
        
      ),
      infoBox(
        "Number of Visits",
        paste("Count: ", nrow(filtered_data_MU_total())),
        icon = icon("users"),
        fill=TRUE,
        width = 4, 
        color = "light-blue"
        
      ),
      infoBox(
        "Number of Hospitals",
        paste("Count: ", n_distinct(filtered_data_MU_total()$`Hospital`)),
        icon = icon("hospital", class = "fa-solid"),  # Specify the Font Awesome icon name and class
        width = 4, 
        fill=TRUE,
        color = "light-blue"
        
      )
    )
    
    # Return the info boxes as a UI element
    fluidRow(info_boxes)
  })
  
  # Function to calculate counts and percentages for each category
  calculate_category_stats <- function(filtered_data) {
    # Count the occurrences of each category
    counts <- table(filtered_data$onDMARDS, filtered_data$onBiologic)
    
    # Initialize variables to store counts and percentages for each category
    monotherapy_biologics_count <- 0
    monotherapy_dmards_count <- 0
    combination_count <- 0
    none_count <- 0
    
    # Loop through each combination of DMARDS and Biologic
    for (i in 1:nrow(counts)) {
      for (j in 1:ncol(counts)) {
        if (rownames(counts)[i] != "No" && colnames(counts)[j] != "No") {
          combination_count <- combination_count + counts[i, j]
        } else if (rownames(counts)[i] == "No" && colnames(counts)[j] == "No") {
          none_count <- none_count + counts[i, j]
        } else if (rownames(counts)[i] == "No") {
          monotherapy_biologics_count <- monotherapy_biologics_count + counts[i, j]
        } else {
          monotherapy_dmards_count <- monotherapy_dmards_count + counts[i, j]
        }
      }
    }
    
    # Calculate total count
    total_count <- monotherapy_biologics_count + monotherapy_dmards_count + combination_count + none_count
    
    # Calculate percentages
    monotherapy_biologics_percentage <- round(monotherapy_biologics_count / total_count * 100, 0)
    monotherapy_dmards_percentage <- round(monotherapy_dmards_count / total_count * 100, 0)
    combination_percentage <- round(combination_count / total_count * 100, 0)
    none_percentage <- round(none_count / total_count * 100, 0)
    
    # Return counts and percentages
    return(list(
      monotherapy_biologics_count = monotherapy_biologics_count,
      monotherapy_dmards_count = monotherapy_dmards_count,
      combination_count = combination_count,
      none_count = none_count,
      monotherapy_biologics_percentage = monotherapy_biologics_percentage,
      monotherapy_dmards_percentage = monotherapy_dmards_percentage,
      combination_percentage = combination_percentage,
      none_percentage = none_percentage
    ))
  }
  
  # Render four info boxes with counts and percentages based on user inputs
  output$InfoBoxesMU2 <- renderUI({
    filtered_data <- filtered_data_MU()
    
    # Calculate counts and percentages for each category
    category_stats <- calculate_category_stats(filtered_data)
    
    # Create info boxes for each category with counts and percentages
    info_boxes <- list(
      infoBox(
        "Mono Biologics",
        tagList(
          tags$div(paste("Count: ", category_stats$monotherapy_biologics_count)),
          tags$div(paste("Percent: ", category_stats$monotherapy_biologics_percentage, "%"))
        ),
        icon = icon("syringe"),
        fill=TRUE,
        width = 3, 
        color = "light-blue"
        
      ),
      infoBox(
        "Mono DMARDs",
        tagList(
          tags$div(paste("Count: ", category_stats$monotherapy_dmards_count)),
          tags$div(paste("Percent: ", category_stats$monotherapy_dmards_percentage, "%"))
        ),
        icon = icon("pills"),
        width = 3,
        fill=TRUE,
        color = "light-blue"
        
      ),
      infoBox(
        "Combination",
        tagList(
          tags$div(paste("Count: ", category_stats$combination_count)),
          tags$div(paste("Percent: ", category_stats$combination_percentage, "%"))
        ),
        icon = icon("medkit"),
        fill=TRUE,
        width = 3,
        color = "light-blue"
        
      ),
      infoBox(
        "None",
        tagList(
          tags$div(paste("Count: ", category_stats$none_count)),
          tags$div(paste("Percent: ", category_stats$none_percentage, "%"))
        ),
        icon = icon("ban"),
        fill=TRUE,
        width = 3, 
        color = "light-blue"
        
      )
    )
    
    # Return the info boxes as a UI element
    fluidRow(info_boxes)
  })
  
  # Render an interactive pie chart for DMARDS column based on filtered data
  output$DMARDSPieChart <- renderPlotly({
    # Filtered data based on user inputs
    filtered_data <- filtered_data_MU()
    
    # Count the occurrences of each category in the DMARDS column in the filtered data
    dmards_counts <- table(filtered_data$onDMARDS)
    
    # Create a data frame for plotting
    dmards_plot_data <- data.frame(onDMARDS = names(dmards_counts), Count = as.numeric(dmards_counts))
    
    # Define colors for "Yes" and "No"
    colors <- c("Yes" = "#B2182B", "No" = "#4393C3")  # Example colors
    
    # Create a pie chart
    pie_chart <- plot_ly(
      data = dmards_plot_data,
      labels = ~onDMARDS,
      values = ~Count,
      type = "pie",
      marker = list(colors = colors)
    ) %>%
      layout(title = "DMARDs Distribution")
    
    # Return the pie chart
    return(pie_chart)
  })
  
  # Render an interactive pie chart for Biologic column based on filtered data
  output$BiologicsPieChart <- renderPlotly({
    # Filtered data based on user inputs
    filtered_data <- filtered_data_MU()
    
    # Count the occurrences of each category in the Biologic column in the filtered data
    biologic_counts <- table(filtered_data$onBiologic)
    
    # Create a data frame for plotting
    biologic_plot_data <- data.frame(onBiologic = names(biologic_counts), Count = as.numeric(biologic_counts))
    
    # Define colors for "Yes" and "No"
    colors <- c("Yes" = "#B2182B", "No" = "#4393C3")  # Example colors
    
    # Create a pie chart
    pie_chart <- plot_ly(
      data = biologic_plot_data,
      labels = ~onBiologic,
      values = ~Count,
      type = "pie",
      marker = list(colors = colors)
    ) %>%
      layout(title = "Biologics Distribution")
    
    # Return the pie chart
    return(pie_chart)
  })
  
  output$DMARDsBarChart <- renderPlotly({
    filtered_data <- filtered_data_MU()
    
    # Create a reactive expression for medication variables
    medication_data <- filtered_data %>%
      select(
        "MTX(DOSE mg)",
        "SSZ(DOSE mg)",
        "Leflunomide(DOSE mg)",
        "HCQ(DOSE mg)",
        "Imuran(DOSE mg)",
        "Cyclosporine(DOSE mg)"
      )
    
    # Prepare data for plotting
    med_data_long <- tidyr::gather(medication_data, key = "Medication", value = "Dosage", everything())
    
    # Filter the data to include only "Yes" values
    med_data_long <- med_data_long %>%
      filter(Dosage == "yes")
    
    # Create a summary table with count and percentages
    summary_table_dmard <- med_data_long %>%
      group_by(Medication) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = Count / sum(Count) * 100,
             HoverText = paste("Count: ", Count, "<br>Percent: ", round(Percentage, 2), "%")) %>%
      arrange(desc(Percentage))  # Arrange in descending order based on Percentage
    
    # Plotting with plotly for interactivity
    plot_ly(
      data = summary_table_dmard,
      x = ~reorder(Medication, -Percentage),  # Reorder bars based on Percentage
      y = ~Percentage,
      type = 'bar',
      color = I("#4393C3"),  # Set a single color for all bars
      text = ~HoverText,
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Usage Distribution of DMARDs",
        xaxis = list(title = "Medication"),
        yaxis = list(title = "Percent")
      )
  })
  
  # Render the summary table using DataTables
  output$DMARDsTable <- DT::renderDataTable({
    summary_table_dmard <- filtered_data_MU() %>%
      select(
        "MTX(DOSE mg)",
        "SSZ(DOSE mg)",
        "Leflunomide(DOSE mg)",
        "HCQ(DOSE mg)",
        "Imuran(DOSE mg)",
        "Cyclosporine(DOSE mg)"
      ) %>%
      gather(key = "Medication", value = "Dosage") %>%
      filter(Dosage == "yes") %>%
      group_by(Medication) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = round(Count / sum(Count) * 100, digits = 4))
    
    DT::datatable(summary_table_dmard, options = list(pageLength = 5))
  })
  
  output$BiologicsBarChart <- renderPlotly({
    filtered_data <- filtered_data_MU()
    
    # Create a reactive expression for biologic variables
    biologic_data <- filtered_data %>%
      select(
        "Infliximab(DOSE mg)",
        "Etanercept(DOSE mg)",
        "Adalimumab(DOSE mg)",
        "Rituximab(DOSE mg)",
        "Abatacept(DOSE mg)",
        "Tociluzumab(DOSE mg)",
        "Certolizumab(DOSE mg)",
        "Tofacitinib(DOSE mg)",
        "Golimumab(DOSE mg)",
        "Baricitinib(DOSE mg)",
        "Amgevita(DOSE mg)",
        "Upadacitinib(Frequency)",
        "Other(DOSE mg)",
        "Other-Biosimilars(DOSE mg)",
        "Other-tsDMRDs(DOSE mg)",
        "Other-tsDMRDs(Started On)",
        "Hyrimoz(DOSE mg)"
      )
    
    # Prepare data for plotting
    biologic_data_long <- tidyr::gather(biologic_data, key = "onBiologic", value = "Dosage", everything())
    
    # Filter the data to include only "Yes" values
    biologic_data_long <- biologic_data_long %>%
      filter(Dosage == "yes")
    
    # Create a summary table with count and percentages
    summary_table_biologic <- biologic_data_long %>%
      group_by(onBiologic) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = Count / sum(Count) * 100,
             HoverText = paste("Count: ", Count, "<br>Percent: ", round(Percentage, 2), "%")) %>%
      arrange(desc(Percentage))  # Arrange in descending order based on Percentage
    
    # Plotting with plotly for interactivity
    plot_ly(
      data = summary_table_biologic,
      x = ~reorder(onBiologic, -Percentage),  # Reorder bars based on Percentage
      y = ~Percentage,
      type = 'bar',
      color = I("#B2182B"),  # Set a single color for all bars
      text = ~HoverText,
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Usage Distribution of Biologics",
        xaxis = list(title = "Biologic"),
        yaxis = list(title = "Percent")
      )
  })
  
  # Render the summary table using DataTables
  output$BiologicsTable <- DT::renderDataTable({
    summary_table_biologic <- filtered_data_MU() %>%
      select(
        "Infliximab(DOSE mg)",
        "Etanercept(DOSE mg)",
        "Adalimumab(DOSE mg)",
        "Rituximab(DOSE mg)",
        "Abatacept(DOSE mg)",
        "Tociluzumab(DOSE mg)",
        "Certolizumab(DOSE mg)",
        "Tofacitinib(DOSE mg)",
        "Golimumab(DOSE mg)",
        "Baricitinib(DOSE mg)",
        "Amgevita(DOSE mg)",
        "Upadacitinib(Frequency)",
        "Other(DOSE mg)",
        "Other-Biosimilars(DOSE mg)",
        "Other-tsDMRDs(DOSE mg)",
        "Other-tsDMRDs(Started On)",
        "Hyrimoz(DOSE mg)"
      ) %>%
      gather(key = "onBiologic", value = "Dosage") %>%
      filter(Dosage == "yes") %>%
      group_by(onBiologic) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = round(Count / sum(Count) * 100, digits = 4))
    
    DT::datatable(summary_table_biologic, options = list(pageLength = 5))
  })
  
  
  # LOGISTIC Modelling Page
  
  filtered_data_LOGISTIC  <- reactive({
    data <- df
    
    if (!is.null(input$GenderInputLOGISTIC) && !('All' %in% input$GenderInputLOGISTIC)) {
      data <- data[data$`Gender` %in% input$GenderInputLOGISTIC, ]
    }
    
    if (!is.null(input$NationalityInputLOGISTIC) && !('All' %in% input$NationalityInputLOGISTIC)) {
      data <- data[data$`Nationality` %in% input$NationalityInputLOGISTIC, ]
    }
    
    if (!is.null(input$MedicalConditionsInputLOGISTIC)) {
      if (input$FilterLogicLOGISTIC == "Intersection") {
        for (condition in input$MedicalConditionsInputLOGISTIC) {
          data <- data[data[[condition]] == 'Yes', ]
        }
      } else { # Union
        conditions <- sapply(input$MedicalConditionsInputLOGISTIC, function(condition) data[[condition]] == 'Yes')
        data <- data[rowSums(conditions) > 0, ]
      }
    }
    
    if (!is.null(input$RFInputLOGISTIC) && !('All' %in% input$RFInputLOGISTIC)) {
      data <- data[data$`RF` %in% input$RFInputLOGISTIC, ]
    }
    
    if (!is.null(input$AntiCCPInputLOGISTIC) && !('All' %in% input$AntiCCPInputLOGISTIC)) {
      data <- data[data$`ANTI CCP` %in% input$AntiCCPInputLOGISTIC, ]
    }
    
    if (!is.null(input$anaInputLOGISTIC) && !('All' %in% input$anaInputLOGISTIC)) {
      data <- data[data$`ANA` %in% input$anaInputLOGISTIC, ]
    }
    
    if (!is.null(input$RheumatoidNodulesInputLOGISTIC) && !('All' %in% input$RheumatoidNodulesInputLOGISTIC)) {
      data <- data[data$`Rheumatoid Nodules` %in% input$RheumatoidNodulesInputLOGISTIC, ]
    }
    
    if (!is.null(input$SICCASymptomsInputLOGISTIC) && !('All' %in% input$SICCASymptomsInputLOGISTIC)) {
      data <- data[data$`SICCA Symptoms` %in% input$SICCASymptomsInputLOGISTIC, ]
    }
    
    date_range <- as.POSIXct(input$DateInputLOGISTIC)
    
    if (!is.null(input$DateInputLOGISTIC)) {
      data <- data[data$`Visit Date` >= date_range[1] &
                     data$`Visit Date` <= date_range[2], ]
    }
    
    age_range <- input$AgeSliderInputLOGISTIC
    data <- data[data$Age >= age_range[1] & data$Age <= age_range[2], ]
    
    DD_range <- input$DiseaseDurationSliderInputLOGISTIC
    data <- data[data$`Disease Duration` >= DD_range[1] & data$`Disease Duration` <= DD_range[2], ]
    
    das28_range <- input$DAS28SliderInputLOGISTIC
    data <- data[!is.na(data$`DAS28`) & 
                   data$`DAS28` >= das28_range[1] & 
                   data$`DAS28` <= das28_range[2], ]
    
    cdai_range <- input$CDAISliderInputLOGISTIC
    data <- data[!is.na(data$`CDAI`) & 
                   data$`CDAI` >= cdai_range[1] & 
                   data$`CDAI` <= cdai_range[2], ]
    
    BMI_range <- input$BMISliderInputLOGISTIC
    data <- data[data$BMI >= BMI_range[1] & data$BMI <= age_range[2], ]
    
    return(data)
  })
  
  filtered_data_LOGISTIC_total  <- reactive({
    data <- df
    
    if (!is.null(input$GenderInputLOGISTIC) && !('All' %in% input$GenderInputLOGISTIC)) {
      data <- data[data$`Gender` %in% input$GenderInputLOGISTIC, ]
    }
    
    if (!is.null(input$NationalityInputLOGISTIC) && !('All' %in% input$NationalityInputLOGISTIC)) {
      data <- data[data$`Nationality` %in% input$NationalityInputLOGISTIC, ]
    }
    
    if (!is.null(input$MedicalConditionsInputLOGISTIC)) {
      if (input$FilterLogicLOGISTIC == "Intersection") {
        for (condition in input$MedicalConditionsInputLOGISTIC) {
          data <- data[data[[condition]] == 'Yes', ]
        }
      } else { # Union
        conditions <- sapply(input$MedicalConditionsInputLOGISTIC, function(condition) data[[condition]] == 'Yes')
        data <- data[rowSums(conditions) > 0, ]
      }
    }
    
    if (!is.null(input$RFInputLOGISTIC) && !('All' %in% input$RFInputLOGISTIC)) {
      data <- data[data$`RF` %in% input$RFInputLOGISTIC, ]
    }
    
    if (!is.null(input$AntiCCPInputLOGISTIC) && !('All' %in% input$AntiCCPInputLOGISTIC)) {
      data <- data[data$`ANTI CCP` %in% input$AntiCCPInputLOGISTIC, ]
    }
    
    if (!is.null(input$anaInputLOGISTIC) && !('All' %in% input$anaInputLOGISTIC)) {
      data <- data[data$`ANA` %in% input$anaInputLOGISTIC, ]
    }
    
    if (!is.null(input$RheumatoidNodulesInputLOGISTIC) && !('All' %in% input$RheumatoidNodulesInputLOGISTIC)) {
      data <- data[data$`Rheumatoid Nodules` %in% input$RheumatoidNodulesInputLOGISTIC, ]
    }
    
    if (!is.null(input$SICCASymptomsInputLOGISTIC) && !('All' %in% input$SICCASymptomsInputLOGISTIC)) {
      data <- data[data$`SICCA Symptoms` %in% input$SICCASymptomsInputLOGISTIC, ]
    }
    
    date_range <- as.POSIXct(input$DateInputLOGISTIC)
    
    if (!is.null(input$DateInputLOGISTIC)) {
      data <- data[data$`Visit Date` >= date_range[1] &
                     data$`Visit Date` <= date_range[2], ]
    }
    
    age_range <- input$AgeSliderInputLOGISTIC
    data <- data[data$Age >= age_range[1] & data$Age <= age_range[2], ]
    
    DD_range <- input$DiseaseDurationSliderInputLOGISTIC
    data <- data[data$`Disease Duration` >= DD_range[1] & data$`Disease Duration` <= DD_range[2], ]
    
    das28_range <- input$DAS28SliderInputLOGISTIC
    data <- data[!is.na(data$`DAS28`) & 
                   data$`DAS28` >= das28_range[1] & 
                   data$`DAS28` <= das28_range[2], ]
    
    cdai_range <- input$CDAISliderInputLOGISTIC
    data <- data[!is.na(data$`CDAI`) & 
                   data$`CDAI` >= cdai_range[1] & 
                   data$`CDAI` <= cdai_range[2], ]
    
    BMI_range <- input$BMISliderInputLOGISTIC
    data <- data[data$BMI >= BMI_range[1] & data$BMI <= age_range[2], ]
    
    return(data)
  })
  
  # # Download filtered data when download button is clicked
  # output$DownloadDataLOGISTIC <- downloadHandler(
  #   filename = function() {
  #     "filtered_data_LOGISTIC.csv"  # Change the file extension to .csv
  #   },
  #   content = function(file) {
  #     # Perform filtering based on user input
  #     filtered_data <- filtered_data_LOGISTIC()
  #     
  #     # Write filtered data to CSV file
  #     write.csv(filtered_data, file, row.names = FALSE)  # Use write.csv instead of write.xlsx
  #   }
  # )
  
  output$DownloadDataLOGISTIC <- downloadHandler(
    filename = function() {
      "filtered_data_LOGISTIC.xlsx"  # Change the file extension to .xlsx
    },
    content = function(file) {
      # Perform filtering based on user input
      filtered_data <- filtered_data_LOGISTIC()
      
      # Ensure the Visit Date column is in Date format
      filtered_data <- filtered_data %>%
        mutate(`Visit Date` = as.Date(`Visit Date`))
      
      # Create a dataframe with only the first observation of each unique patient based on the date
      first_observation_data <- filtered_data %>% 
        arrange(`Visit Date`) %>%
        group_by(`User ID`) %>%    # Replace 'patient_id' with the appropriate column name
        slice(1) %>% 
        ungroup()
      
      # Create a dataframe with only the last observation of each unique patient based on the date
      last_observation_data <- filtered_data %>%
        arrange(`Visit Date`) %>%
        group_by(`User ID`) %>%    # Replace 'patient_id' with the appropriate column name
        slice(n()) %>% 
        ungroup()
      
      # Create a list of dataframes to write to Excel
      data_to_write <- list(
        "Filtered Data" = filtered_data, 
        "First Observations" = first_observation_data,
        "Last Observations" = last_observation_data
      )
      
      # Write the dataframes to an Excel file with multiple sheets
      write_xlsx(data_to_write, path = file)
    }
  )
  
  filtered_data_LOGISTIC_2 <- reactive({
    data <- filtered_data_LOGISTIC()
    
    if (!is.null(input$IndependentVariablesLOGISTIC) && length(input$IndependentVariablesLOGISTIC) > 0) {
      # Include the chosen dependent variable and the selected independent variables
      data <- data[, c(input$DependentVariableLOGISTIC, input$IndependentVariablesLOGISTIC)]
    }
    
    return(data)
  })
  
  # Render the info boxes for filtered data count, visits count, and unique hospitals count
  output$InfoBoxesLOGISTIC <- renderUI({
    
    # Create info boxes for each category with counts and percentages
    info_boxes <- list(
      infoBox(
        "Number of Patients",
        paste("Count: ", n_distinct(filtered_data_LOGISTIC()$`User ID`)),
        icon = icon("hospital-user"),
        fill=TRUE,
        width = 4, 
        color = "light-blue"
        
      ),
      infoBox(
        "Number of Visits",
        paste("Count: ", nrow(filtered_data_LOGISTIC_total())),
        icon = icon("users"),
        fill=TRUE,
        width = 4, 
        color = "light-blue"
        
      ),
      infoBox(
        "Number of Hospitals",
        paste("Count: ", n_distinct(filtered_data_LOGISTIC_total()$`Hospital`)),
        icon = icon("hospital", class = "fa-solid"),  # Specify the Font Awesome icon name and class
        width = 4, 
        fill=TRUE,
        color = "light-blue"
        
      )
    )
    
    # Return the info boxes as a UI element
    fluidRow(info_boxes)
  })
  
  # Fit logistic regression model on the training set
  interactive_logistic_model <- reactive({
    # Create formula based on selected variables
    selected_variables_2 <- input$IndependentVariablesLOGISTIC
    formula_string <- paste(input$DependentVariableLOGISTIC, " ~",
                            paste(sapply(selected_variables_2, as.name), collapse = " + "))
    formula <- as.formula(formula_string)
    
    # Subset the data frame with selected variables for training data
    model_data <- filtered_data_LOGISTIC_2()
    train_data_subset <- model_data[, c(input$DependentVariableLOGISTIC, selected_variables_2)]
    
    # Convert the response variable to a factor
    train_data_subset[[input$DependentVariableLOGISTIC]] <- factor(train_data_subset[[input$DependentVariableLOGISTIC]])
    
    # Normalize numeric independent variables
    num_vars <- sapply(train_data_subset, is.numeric)
    train_data_subset[, num_vars] <- scale(train_data_subset[, num_vars])
    
    # Fit logistic regression model on training data
    glm(formula, data = train_data_subset, family = binomial)
  })
  
  # Create a reactive expression for feature importance
  interactive_feature_importance <- reactive({
    coefficients <- coef(interactive_logistic_model())
    
    # Create a data frame for feature importance excluding the intercept
    importance_data <- data.frame(
      Feature = names(coefficients)[-1],
      Coefficient = coefficients[-1]      
    )
    
    # Round coefficients to two decimals
    importance_data$Coefficient <- round(importance_data$Coefficient, 4)
    importance_data
  })
  
  # Render the coefficient plot
  output$CoefficientPlotLOGISTIC <- renderPlotly({
    plot_ly(data = interactive_feature_importance(), x = ~Coefficient, y = ~Feature, type = 'bar',
            hoverinfo = 'x+text', marker = list(color = "#B2182B")) %>%
      layout(xaxis = list(title = "Coefficient"),
             yaxis = list(title = "Variable"),
             showlegend = FALSE)
  })
  
  # Render the model summary
  output$ModelSummaryLOGISTIC <- renderPrint({
    summary(interactive_logistic_model())
  })
  
  # LINEAR Modelling Page
  
  filtered_data_LINEAR <- reactive({
    data <- df
    
    if (!is.null(input$GenderInputLINEAR) && !('All' %in% input$GenderInputLINEAR)) {
      data <- data[data$`Gender` %in% input$GenderInputLINEAR, ]
    }
    
    if (!is.null(input$NationalityInputLINEAR) && !('All' %in% input$NationalityInputLINEAR)) {
      data <- data[data$`Nationality` %in% input$NationalityInputLINEAR, ]
    }
    
    if (!is.null(input$MedicalConditionsInputLINEAR)) {
      if (input$FilterLogicLINEAR == "Intersection") {
        for (condition in input$MedicalConditionsInputLINEAR) {
          data <- data[data[[condition]] == 'Yes', ]
        }
      } else { # Union
        conditions <- sapply(input$MedicalConditionsInputLINEAR, function(condition) data[[condition]] == 'Yes')
        data <- data[rowSums(conditions) > 0, ]
      }
    }
    
    if (!is.null(input$RFInputLINEAR) && !('All' %in% input$RFInputLINEAR)) {
      data <- data[data$`RF` %in% input$RFInputLINEAR, ]
    }
    
    if (!is.null(input$AntiCCPInputLINEAR) && !('All' %in% input$AntiCCPInputLINEAR)) {
      data <- data[data$`ANTI CCP` %in% input$AntiCCPInputLINEAR, ]
    }
    
    if (!is.null(input$ANAInputLINEAR) && !('All' %in% input$ANAInputLINEAR)) {
      data <- data[data$`ANA` %in% input$ANAInputLINEAR, ]
    }
    
    if (!is.null(input$RheumatoidNodulesInputLINEAR) && !('All' %in% input$RheumatoidNodulesInputLINEAR)) {
      data <- data[data$`Rheumatoid Nodules` %in% input$RheumatoidNodulesInputLINEAR, ]
    }
    
    if (!is.null(input$SICCASymptomsInputLINEAR) && !('All' %in% input$SICCASymptomsInputLINEAR)) {
      data <- data[data$`SICCA Symptoms` %in% input$SICCASymptomsInputLINEAR, ]
    }
    
    date_range <- as.POSIXct(input$DateInputLINEAR)
    
    if (!is.null(input$DateInputLINEAR)) {
      data <- data[data$`Visit Date` >= date_range[1] &
                     data$`Visit Date` <= date_range[2], ]
    }
    
    age_range <- input$AgeSliderInputLINEAR
    data <- data[data$Age >= age_range[1] & data$Age <= age_range[2], ]
    
    DD_range <- input$DiseaseDurationSliderInputLINEAR
    data <- data[data$`Disease Duration` >= DD_range[1] & data$`Disease Duration` <= DD_range[2], ]
    
    das28_range <- input$DAS28SliderInputLINEAR
    data <- data[!is.na(data$`DAS28`) & 
                   data$`DAS28` >= das28_range[1] & 
                   data$`DAS28` <= das28_range[2], ]
    
    cdai_range <- input$CDAISliderInputLINEAR
    data <- data[!is.na(data$`CDAI`) & 
                   data$`CDAI` >= cdai_range[1] & 
                   data$`CDAI` <= cdai_range[2], ]
    
    BMI_range <- input$BMISliderInputLINEAR
    data <- data[data$BMI >= BMI_range[1] & data$BMI <= age_range[2], ]
    
    return(data)
  })
  
  filtered_data_LINEAR_total <- reactive({
    data <- df
    
    if (!is.null(input$GenderInputLINEAR) && !('All' %in% input$GenderInputLINEAR)) {
      data <- data[data$`Gender` %in% input$GenderInputLINEAR, ]
    }
    
    if (!is.null(input$NationalityInputLINEAR) && !('All' %in% input$NationalityInputLINEAR)) {
      data <- data[data$`Nationality` %in% input$NationalityInputLINEAR, ]
    }
    
    if (!is.null(input$MedicalConditionsInputLINEAR)) {
      if (input$FilterLogicLINEAR == "Intersection") {
        for (condition in input$MedicalConditionsInputLINEAR) {
          data <- data[data[[condition]] == 'Yes', ]
        }
      } else { # Union
        conditions <- sapply(input$MedicalConditionsInputLINEAR, function(condition) data[[condition]] == 'Yes')
        data <- data[rowSums(conditions) > 0, ]
      }
    }
    
    if (!is.null(input$RFInputLINEAR) && !('All' %in% input$RFInputLINEAR)) {
      data <- data[data$`RF` %in% input$RFInputLINEAR, ]
    }
    
    if (!is.null(input$AntiCCPInputLINEAR) && !('All' %in% input$AntiCCPInputLINEAR)) {
      data <- data[data$`ANTI CCP` %in% input$AntiCCPInputLINEAR, ]
    }
    
    if (!is.null(input$ANAInputLINEAR) && !('All' %in% input$ANAInputLINEAR)) {
      data <- data[data$`ANA` %in% input$ANAInputLINEAR, ]
    }
    
    if (!is.null(input$RheumatoidNodulesInputLINEAR) && !('All' %in% input$RheumatoidNodulesInputLINEAR)) {
      data <- data[data$`Rheumatoid Nodules` %in% input$RheumatoidNodulesInputLINEAR, ]
    }
    
    if (!is.null(input$SICCASymptomsInputLINEAR) && !('All' %in% input$SICCASymptomsInputLINEAR)) {
      data <- data[data$`SICCA Symptoms` %in% input$SICCASymptomsInputLINEAR, ]
    }
    
    date_range <- as.POSIXct(input$DateInputLINEAR)
    
    if (!is.null(input$DateInputLINEAR)) {
      data <- data[data$`Visit Date` >= date_range[1] &
                     data$`Visit Date` <= date_range[2], ]
    }
    
    age_range <- input$AgeSliderInputLINEAR
    data <- data[data$Age >= age_range[1] & data$Age <= age_range[2], ]
    
    DD_range <- input$DiseaseDurationSliderInputLINEAR
    data <- data[data$`Disease Duration` >= DD_range[1] & data$`Disease Duration` <= DD_range[2], ]
    
    das28_range <- input$DAS28SliderInputLINEAR
    data <- data[!is.na(data$`DAS28`) & 
                   data$`DAS28` >= das28_range[1] & 
                   data$`DAS28` <= das28_range[2], ]
    
    cdai_range <- input$CDAISliderInputLINEAR
    data <- data[!is.na(data$`CDAI`) & 
                   data$`CDAI` >= cdai_range[1] & 
                   data$`CDAI` <= cdai_range[2], ]
    
    BMI_range <- input$BMISliderInputLINEAR
    data <- data[data$BMI >= BMI_range[1] & data$BMI <= age_range[2], ]
    
    return(data)
  })
  
  # # Download filtered data when download button is clicked
  # output$DownloadDataLINEAR <- downloadHandler(
  #   filename = function() {
  #     "filtered_data_LINEAR.csv"  # Change the file extension to .csv
  #   },
  #   content = function(file) {
  #     # Perform filtering based on user input
  #     filtered_data <- filtered_data_LINEAR()
  #     
  #     # Write filtered data to CSV file
  #     write.csv(filtered_data, file, row.names = FALSE)  # Use write.csv instead of write.xlsx
  #   }
  # )
  
  output$DownloadDataLINEAR <- downloadHandler(
    filename = function() {
      "filtered_data_LINEAR.xlsx"  # Change the file extension to .xlsx
    },
    content = function(file) {
      # Perform filtering based on user input
      filtered_data <- filtered_data_LINEAR()
      
      # Ensure the Visit Date column is in Date format
      filtered_data <- filtered_data %>%
        mutate(`Visit Date` = as.Date(`Visit Date`))
      
      # Create a dataframe with only the first observation of each unique patient based on the date
      first_observation_data <- filtered_data %>% 
        arrange(`Visit Date`) %>%
        group_by(`User ID`) %>%    # Replace 'patient_id' with the appropriate column name
        slice(1) %>% 
        ungroup()
      
      # Create a dataframe with only the last observation of each unique patient based on the date
      last_observation_data <- filtered_data %>%
        arrange(`Visit Date`) %>%
        group_by(`User ID`) %>%    # Replace 'patient_id' with the appropriate column name
        slice(n()) %>% 
        ungroup()
      
      # Create a list of dataframes to write to Excel
      data_to_write <- list(
        "Filtered Data" = filtered_data, 
        "First Observations" = first_observation_data,
        "Last Observations" = last_observation_data
      )
      
      # Write the dataframes to an Excel file with multiple sheets
      write_xlsx(data_to_write, path = file)
    }
  )
  
  filtered_data_LINEAR_2 <- reactive({
    data <- filtered_data_LINEAR()
    
    if (!is.null(input$IndependentVariablesLINEAR) && length(input$IndependentVariablesLINEAR) > 0) {
      # Include the chosen dependent variable and the selected independent variables
      data <- data[, c(input$DependentVariableLINEAR, input$IndependentVariablesLINEAR)]
    }
    
    return(data)
  })
  
  # Render the info boxes for filtered data count, visits count, and unique hospitals count
  output$InfoBoxesLINEAR <- renderUI({
    
    # Create info boxes for each category with counts and percentages
    info_boxes <- list(
      infoBox(
        "Number of Patients",
        paste("Count: ", n_distinct(filtered_data_LINEAR()$`User ID`)),
        icon = icon("hospital-user"),
        fill=TRUE,
        width = 4, 
        color = "light-blue"
        
      ),
      infoBox(
        "Number of Visits",
        paste("Count: ", nrow(filtered_data_LINEAR_total())),
        icon = icon("users"),
        fill=TRUE,
        width = 4, 
        color = "light-blue"
        
      ),
      infoBox(
        "Number of Hospitals",
        paste("Count: ", n_distinct(filtered_data_LINEAR_total()$`Hospital`)),
        icon = icon("hospital", class = "fa-solid"),  # Specify the Font Awesome icon name and class
        width = 4, 
        fill=TRUE,
        color = "light-blue"
        
      )
    )
    
    # Return the info boxes as a UI element
    fluidRow(info_boxes)
  })
  
  # Fit linear regression model on the training set
  interactive_linear_model <- reactive({
    # Create formula based on selected variables
    selected_variables <- input$IndependentVariablesLINEAR
    formula_string <- paste(input$DependentVariableLINEAR, " ~", 
                            paste(sapply(selected_variables, as.name), collapse = " + "))
    formula <- as.formula(formula_string)
    
    # Subset the data frame with selected variables for training data
    model_data <- filtered_data_LINEAR_2()  # Assuming you have this reactive function defined elsewhere
    train_data_subset <- model_data[, c(input$DependentVariableLINEAR, selected_variables)]
    
    # Normalize numeric independent variables
    num_vars <- sapply(train_data_subset, is.numeric)
    train_data_subset[, num_vars] <- scale(train_data_subset[, num_vars])
    
    # Fit linear regression model on training data
    lm(formula, data = train_data_subset)
  })
  
  # Create a reactive expression for coefficient values
  interactive_coefficients <- reactive({
    coefficients <- coef(interactive_linear_model())
    
    # Create a data frame for coefficients excluding the intercept
    coefficients_data <- data.frame(
      Feature = names(coefficients)[-1],
      Coefficient = coefficients[-1]       # Exclude the intercept
    )
    
    # Round coefficients to two decimals
    coefficients_data$Coefficient <- round(coefficients_data$Coefficient, 4)
    coefficients_data
  })
  
  # Render the coefficient plot
  output$CoefficientPlotLINEAR <- renderPlotly({
    plot_ly(data = interactive_coefficients(), x = ~Coefficient, y = ~Feature, type = 'bar',
            hoverinfo = 'x+text', marker = list(color = "#B2182B")) %>%  # Change color to blue
      layout(xaxis = list(title = "Coefficient"),
             yaxis = list(title = "Variable"),
             showlegend = FALSE)
  })
  
  # Render the model summary
  output$ModelSummaryLINEAR <- renderPrint({
    summary(interactive_linear_model())
  })
  
  # Render the predicted vs actual plot
  output$PredictedActualPlotLINEAR <- renderPlotly({
    model <- interactive_linear_model()
    model_data <- filtered_data_LINEAR_2()  # Assuming you have this reactive function defined elsewhere
    actual <- model_data[[input$DependentVariableLINEAR]]
    predicted <- predict(model, newdata = model_data)
    
    plot_ly(x = actual, y = predicted, type = 'scatter', mode = 'markers',
            marker = list(color = '#B2182B', opacity = 0.7)) %>%
      layout(title = "Predicted vs Actual Values",
             xaxis = list(title = "Actual"),
             yaxis = list(title = "Predicted"))
  })


}

shinyApp(ui, server)
