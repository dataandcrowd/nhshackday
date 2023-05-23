library(shiny)
library(shinythemes)
library(DT)

# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # App title
                titlePanel("Hospital Prescription Template"),
                
                # Prescription form
                fluidRow(
                  column(width = 6,
                         textInput("patient_name", "Patient Name:", ""),
                         numericInput("age", "Age (Numerical)", value = 0),
                         selectInput("sex", "Sex: (Choose)",
                                     choices = c("Male", "Female", "Other")),
                         textInput("postcode", "Postcode:", "")
                  ),
                  column(width = 6,
                         textInput("medication", "Medication:", ""),
                         numericInput("dosage", "Dosage:", value = 0),
                         selectInput("frequency", "Frequency:",
                                     choices = c("Once Daily", "Twice Daily", "Three Times Daily", "Four Times Daily")),
                         actionButton("add_btn", "Add Prescription")
                  )
                ),
                
                # Display prescriptions
                fluidRow(
                  column(width = 12,
                         DT::dataTableOutput("prescription_table")
                  )
                ),
                
                # Export as CSV
                fluidRow(
                  column(width = 12,
                         downloadButton("export_btn", "Export as CSV")
                  )
                ),
                
                # Medication distribution plot
                fluidRow(
                  column(width = 12,
                         plotOutput("medication_plot")
                  )
                )
)

# Define server
server <- function(input, output) {
  # Initialize empty reactive value to store prescriptions
  prescriptions <- reactiveVal(data.frame(PatientName = character(),
                                          Age = numeric(),
                                          Sex = character(),
                                          Postcode = character(),
                                          Medication = character(),
                                          Dosage = numeric(),
                                          Frequency = character(),
                                          stringsAsFactors = FALSE))
  
  
  # Function to validate postcode format
  validatePostcode <- function(postcode) {
    postcode_regex <- "^[0-9]{4,5}$"
    grepl(postcode_regex, postcode)
  }
  
  # Function to add a prescription
  addPrescription <- function() {
    name <- input$patient_name
    age <- input$age
    sex <- input$sex
    postcode <- input$postcode
    medication <- input$medication
    dosage <- input$dosage
    frequency <- input$frequency
    
    current_prescriptions <- prescriptions()
    new_prescription <- data.frame(PatientName = name,
                                   Age = age,
                                   Sex = sex,
                                   Postcode = postcode,
                                   Medication = medication,
                                   Dosage = dosage,
                                   Frequency = frequency)
    updated_prescriptions <- rbind(current_prescriptions, new_prescription)
    
    prescriptions(updated_prescriptions)
  }
  
  
  # Validate postcode format
  # if (!validatePostcode(postcode)) {
  #   showErrorMessage("Invalid postcode format!")
  #   return()
  # }
  
  
  # Observe add button click
  observeEvent(input$add_btn, {
    addPrescription()
  })
  
  # Render prescription table
  output$prescription_table <- DT::renderDataTable({
    DT::datatable(prescriptions(), options = list(pageLength = 10))
  })
  
  # Export prescriptions as CSV
  output$export_btn <- downloadHandler(
    filename = function() {
      paste("prescriptions", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(prescriptions(), file, row.names = FALSE)
    }
  )
  
  # Medication distribution plot
  output$medication_plot <- renderPlot({
    medication_counts <- table(prescriptions()$Medication)
    barplot(medication_counts, main = "Medication Distribution", horiz=T, col="#69b3a2",
            xlab = "Medication", ylab = "Count")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
