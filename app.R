
# data <- read.csv("export.csv")
# rejects <- read.csv("reject.csv")
## Sadly reject$Signal coded as yes/no instead of gold/silver
## I relabeled them as silver...


# setwd("/Users/kurtisstefan/Documents/Code/ApplicantApp/")
# shinylive::export(appdir = ".", destdir = "docs")
#  shinylive::export(appdir = "/Users/kurtisstefan/Documents/Code/kurtisstefan/kurtisstefan/", destdir = "/Users/kurtisstefan/Documents/Code/kurtisstefan/kurtisstefan/docs")
#  httpuv::runStaticServer("/Users/kurtisstefan/Documents/Code/kurtisstefan/kurtisstefan/docs/", port=8008)
# 
# rejects <- rejects %>% 
#   mutate(Offer = "Reject") %>% 
#   mutate(Signal = Program.Signal)
# data <- data %>% 
#   mutate(Offer = "Interview")
# data <- dplyr::bind_rows(rejects, data)
# data <- data %>%
#   mutate(
#     lower_bound = as.numeric(sub("-.*", "", Step.2.Score)),  # Extract lower bound
#     upper_bound = as.numeric(sub(".*-", "", Step.2.Score)),  # Extract upper bound
#     mid_point = (lower_bound + upper_bound) / 2  # Calculate midpoint
#   ) %>% 
#   mutate(
#     region = case_when(
#       State %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont") ~ "NewEngland",
#       State %in% c("New Jersey", "New York", "Pennsylvania") ~ "MiddleAtlantic",
#       State %in% c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin") ~ "ENC",
#       State %in% c("Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota") ~ "WNC",
#       State %in% c("Delaware", "District of Columbia", "Florida", "Georgia", "Maryland", "North Carolina", "Puerto Rico", "South Carolina", "Virginia", "West Virginia") ~ "SouthAtlantic",
#       State %in% c("Alabama", "Kentucky", "Mississippi", "Tennessee") ~ "ESC",
#       State %in% c("Arkansas", "Louisiana", "Oklahoma", "Texas") ~ "WSC",
#       State %in% c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming") ~ "Mountain",
#       State %in% c("Alaska", "California", "Hawaii", "Oregon", "Washington") ~ "Pacific",
#       TRUE ~ "Unknown"  )) %>% 
#       mutate(Signal = case_when(Signal == "" | Signal == "No Signal" |  Signal == "No"~ "No Signal",
#                                 Signal == "Signalled (Unspecified)" | Signal == "Yes"~ "Silver",
#                                 TRUE ~ Signal)) %>% 
#       mutate(Geographic.Preference = case_when(Geographic.Preference == "" | Geographic.Preference == "No" ~ "No",
#                                               TRUE ~ Geographic.Preference)) %>% 
#     mutate(Signal = paste0("SIGNAL: ", Signal)) %>% 
#     mutate(Geographic.Preference = paste0("GEO_PREF: ", Geographic.Preference)) 
#     
# saveRDS(data, "data.RDS")
data <- readRDS("data.RDS")
data <- data %>% 
  mutate(Program.Name = gsub("\\([CP]\\d{1}\\)", "", Program.Name))

library(shiny)
library(tidyverse)
library(ggplot2)
library(bslib)

ui <- page_navbar(
  # full_screen = TRUE,
  sidebar = sidebar("Data from ResidencyMatch.Net, snapshot as of 1.15.25"),
    #selectInput("InputProgram", label="PROGRAM",
     #           choices=as.character(data$Program.Name), 
    #            multiple=TRUE),
    #selectInput("InputRegion", label="REGION",
     #           choices=as.character(data$region), 
    #            multiple=TRUE),
    #numericInput("MyStep2", label="STEP2", value = 210)),
    
  nav_panel("Signal+GEO+Step2", selectInput("InputProgramStep2", label="PROGRAM",
                                            selected="Loyola Univ Med Ctr-IL",
                                  choices=as.character(data$Program.Name), multiple=TRUE), 
            numericInput("MyStep2_1", label="STEP2", value = 230),
            plotOutput("histogram")), 
  
  nav_panel("Fits within your GEO Regions", selectInput("InputRegion", label="REGION",
                                          choices=as.character(data$region), 
                                          multiple=TRUE),
            numericInput("MyStep2", label="STEP2", value = 240),
            DT::dataTableOutput("mytableregion")),
  
  nav_panel("Signal Behavior of Invited Applicants",
            selectInput("InputRegion2", label="REGION",
                        choices=as.character(data$region), 
                        multiple=TRUE),
            selectInput("InputProgrambyRegion", label="Program Within Region",
                                         choices=NULL, 
                                         multiple=TRUE), 
            plotOutput("SignalNeeded")), 
  nav_panel("Details of Applicants", selectInput("InputProgram", label="PROGRAM",
                                 choices=as.character(data$Program.Name), multiple=TRUE), 
            DT::dataTableOutput("mytable")), 
  nav_panel("Summary of Each Region", DT::dataTableOutput("SummaryData"))
)



# Define server logic
server <- function(input, output, session) {
  filtered_data <- reactive({
    req(input$InputProgram)  # Ensure input is available before filtering
    data %>%
      filter(Program.Name %in% input$InputProgram)  # Filter by selected programs
  })
  
  filtered_dataStep2 <- reactive({
   req(input$InputProgramStep2)  # Ensure input is available before filtering
    data %>%
      filter(Program.Name %in% input$InputProgramStep2)  # Filter by selected programs
  })
  
  f0_data <- reactive({
    req(input$InputRegion2)
    data %>%
      filter(region %in% input$InputRegion2)
  })
  
  observe({
    filtered_df <- f0_data()  # Get filtered data based on region
    updateSelectInput(session, 
                      "InputProgrambyRegion", 
                      choices = as.character(filtered_df$Program.Name))
  })
  
  f1_data <- reactive({
    req(input$InputRegion)  # Ensure input is available before filtering
    req(input$MyStep2)
    data %>%
      filter(Offer == "Interview") %>% 
      filter(region %in% input$InputRegion)  %>% 
      filter(as.numeric(input$MyStep2) >= as.numeric(lower_bound))# Filter by selected programs
  })
  output$mytableregion <- DT::renderDataTable({
    DT::datatable(f1_data())
  })
  
  # Render the filtered data table
  output$mytable <- DT::renderDataTable({
    DT::datatable(filtered_data())
  })
  
  
  output$histogram <- renderPlot({
    ggplot(filtered_dataStep2(), aes(x = mid_point)) +
      geom_histogram(data=subset(filtered_dataStep2(), Offer=="Interview"),  position="identity", bins = 10,color = "black", fill = "blue", alpha = 0.4) +
      geom_histogram(data=subset(filtered_dataStep2(), Offer=="Reject"),  position="identity", bins = 10, color = "black",fill = "red", alpha = 0.4) +
      labs(title = "Distribution of Step2 Scores for Sucessful Interview Invites (Blue) and Rejected Applicants (Red)", x = "Step2 Score", y = "# Interviews") +
      geom_vline(xintercept = input$MyStep2_1) +
      theme_bw() + 
      facet_wrap(Signal~Geographic.Preference+Program.Name)
  })

    output$SignalNeeded <- renderPlot({
    req(input$InputRegion2)
    req(input$InputProgrambyRegion)
    data %>% 
      filter(region %in% input$InputRegion2)  %>% 
      filter(Program.Name %in% input$InputProgrambyRegion) %>% 
      group_by(Program.Name, Signal) %>% 
      count(Program.Name, Signal) %>% 
      ggplot(aes(x=Program.Name, y=n, fill=Signal)) + 
      geom_bar(position="dodge", stat="identity") + theme_classic() + 
      theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = "black"), 
            axis.text.y = element_text(size = 12)) + 
      ylab("Number Applicants Receiving Interviews")
  })
    output$SummaryData <- DT::renderDataTable({
      data %>%  group_by(region) %>% 
        mutate(DistinctPrograms = n_distinct(Program.Name)) %>% 
        mutate(ApplicationNumberPerRegion = n()) %>% 
        group_by(Program.Name) %>% mutate(AppPerProgram = n()) %>% 
        group_by(region) %>%  mutate(MedianAppPerProgram = median(AppPerProgram)) %>% 
        filter(!is.na(mid_point) ) %>% 
         mutate(MedianStep2 = median(as.numeric(as.character(mid_point)))) %>% 
        select(DistinctPrograms,ApplicationNumberPerRegion, MedianAppPerProgram, MedianStep2) %>% 
        distinct(region, .keep_all=TRUE) %>% 
        filter(region != "Unknown")
    })
}
# Run the app
shinyApp(ui = ui, server = server)
# 
# 
# data %>% 
#   filter(mid_point>200) %>% 
# ggplot(aes(x = mid_point)) +
#   geom_histogram(data=subset(data, Offer=="Interview"),  position="identity", bins = 10,color = "black", fill = "blue", alpha = 0.4) +
#   geom_histogram(data=subset(data, Offer=="Reject"),  position="identity", bins = 10, color = "black",fill = "red", alpha = 0.4) +
#   labs(title = "Distribution of Step2 Scores for Sucessful Interview Invites (Blue) and Rejected Applicants (Red)", x = "Step2 Score", y = "# Interviews") +
#   geom_vline(xintercept = 245) +
#   theme_bw() + 
#   facet_wrap(Signal~Geographic.Preference) + 
#   xlim(c(200,300))
