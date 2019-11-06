#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load required packages
library(shiny)
library(ggplot2)
library(dplyr)


#UI
ui <- fluidPage(
  
  titlePanel("StudentFaculty"),
  
  sidebarLayout(
    
    #upload data
    sidebarPanel(
      fileInput("file1", "Upload Data CSV File",
                accept = ".csv"
      ),
      
      #choose departments to analyze
      selectInput(inputId = "Department", 
                  label = h3("Select the Departments:"), 
                  choices = c("CAHSS", "Afric Std", "Amer Std", "Anc Std", "Dance", "Economics", "Emgcy Hlth Srvc",   "English",           "Gend & Wm Std",    
                  "Geog & Env Sys",    "History",           "Med & Comm Std",    "Modern LL & IC",    "Music",             "Philosophy",        "Political Sci",     "Psychology",        "Socy, Anth & Hap", 
                  "Theatre",           "Visual Arts",       "CNMS",              "Biol Sci",          "Chemistry",         "Math & Stat",       "Physics",           "COEIT",             "Chem/Bioc/Env Eng",
                  "Comp Sci & EE",     "Info Sys",          "Mech Eng",          "Erickson",          "Mgt of Ag Srvc",    "Sch of Soc Work",   "Social Work"),
                  multiple = TRUE
      ),
      
      #Choose starting year
      selectInput(inputId = 'year1',
                  label = 'Select the first year to compare', 
                  choices = c('2013', '2015')),
      
      #choose ending year
      selectInput(inputId = 'year2',
                  label = 'Select the second year to compare', 
                  choices = c('2015', '2018', '2019')),
      
      #textual information about the graph
      p('Plans in this report are counts of majors by organization (Fall census counts). If students have multiple majors in different organizations, they will be counted more than once. If students have multiple majors in the same organization, they will be counted once Therefore, this report may not match other OIR reports.'),
      
      p('Students FTES (full-time equivalent) are based on student course registration. Excludes remedial and intercampus registration.'),
      
      p('The semester credit hour divisors for FTES calculations are: 15 for lower and upper graduate, 12 for Grad 1, 10 for Grad 2 and 9 for Grad 3.
        Grad 1 refers to master’s and 1st professional credit hours. Grad 2 refers to post-master’s and doctoral credit hours. Grad 3 refers to master’s and doctoral research supervision (799 and 899)'),
      
      p('Faculty FTE uses IPEDS formula: FT = # FT Faculty + (⅓ * # PT Faculty). Faculty counts based on Fall census freeze.'),
      
      p('The number of minor and UG certificate students are not used in calculating student/Faculty ratios. They may not match other OIR reports, since students can have multiple minors or multiple certificates in the same organization. In these cases, the student will only be counted once  
        ')
      
      ),
    
    #main panel display
    mainPanel(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      textOutput('Department'),
      plotOutput('plot'),
      tableOutput('table')
      
    )
    
    )
  
  )


#Server
server <- function(input, output){

  
  #filter the data based on the used input for year and department
  filterdata <- reactive({
    
    #reading in the data from the user selected file
    req(input$file1)
    trimmed_data_table <- read.csv(input$file1$datapath, header = T, stringsAsFactors = F)

    trimmed_data_table %>%
      
      filter(Dep %in% input$Department) %>%
      select(Dep, ends_with(input$year1), ends_with(input$year2)) 
    

  })
  
  #create a the datatable under the plot
  output$table <- renderTable({
    
    trimmed_data_table <- read.csv(input$file1$datapath, header = T)
    
    trimmed_data_table %>%
      mutate(DeltaMajors =  select(trimmed_data_table, ends_with(paste('MAJORS.', input$year2, sep = '')))[,1] - select(trimmed_data_table, ends_with(paste('MAJORS.', input$year1, sep = '')))[,1]) %>%
      mutate(DeltaFTES =  select(trimmed_data_table, ends_with(paste('FTES.', input$year2, sep = '')))[,1] - select(trimmed_data_table, ends_with(paste('FTES.', input$year1, sep = '')))[,1]) %>%
      mutate(DeltaFaculty =  select(trimmed_data_table, ends_with(paste('FACULTY.', input$year2, sep = '')))[,1] - select(trimmed_data_table, ends_with(paste('FACULTY.', input$year1, sep = '')))[,1]) %>%
      filter(Dep %in% input$Department) %>%
      select(Dep, paste('MAJORS.', input$year1, sep = ''), paste('MAJORS.', input$year2, sep = ''), paste('FTES.', input$year1, sep = ''), paste('FTES.', input$year2, sep = ''), 
             paste('FACULTY.', input$year1, sep = ''), paste('FACULTY.', input$year2, sep = ''), DeltaMajors, DeltaFTES, DeltaFaculty)  
    
  })
  
  #generates the departments in text seen above the plot
  output$Department <- renderText(input$Department)
  
  #creates a plot of the MPF vs FTES ratios
  output$plot <- renderPlot({
    
    q <- ggplot(filterdata()) +
      geom_segment(aes(x = filterdata()[,2], y = filterdata()[,5], xend = filterdata()[,7], yend = filterdata()[,10], col = Dep), arrow = arrow(length = unit(0.3,"cm"))) +
      geom_vline(xintercept = mean(filterdata()[,2]), color = 'blue') +
      geom_text(aes(x=mean(filterdata()[,2]), y=35, label='avg MPF'), size=4, angle=90, vjust=-0.5, hjust = 0.5) +
      geom_hline(yintercept = mean(filterdata()[,5]), color = 'blue') +
      geom_text(aes(y=mean(filterdata()[,5]), x=0, label='avg FTES'), size=4, vjust=-0.4, hjust=0.5) +
      
      #optional code that will plot MPF and FTES averages for second year
#      geom_vline(xintercept = mean(filterdata()[,7]), color = 'red') +
#      geom_hline(yintercept = mean(filterdata()[,10]), color = 'red') +  
      geom_text(aes(x=filterdata()[,2], y=filterdata()[,5], label = Dep), nudge_y = 0.7) +
      
      scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
      xlab(label = "Majors Per FT Faculty") +
      ylab(label = "FTE Students per FT Faculty") +
      theme(legend.position = 'none')
    
    print(q)
  })
  
}



#function call to run app
shinyApp(ui = ui, server = server)