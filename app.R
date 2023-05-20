library(shiny)
library(shinythemes)
library(DT)

# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # App title
                titlePanel("Hospital Prescription Template"),
                
                # Prescription form
                fluidRow(
                  column(width = 12,
                         textInput("patient_name", "Patient Name:", ""),
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
                )
)

# Define server
server <- function(input, output) {
  # Initialize empty reactive value to store prescriptions
  prescriptions <- reactiveVal(data.frame(PatientName = character(),
                                          Medication = character(),
                                          Dosage = numeric(),
                                          Frequency = character(),
                                          stringsAsFactors = FALSE))
  
  # Function to add a prescription
  addPrescription <- function() {
    name <- input$patient_name
    medication <- input$medication
    dosage <- input$dosage
    frequency <- input$frequency
    
    current_prescriptions <- prescriptions()
    new_prescription <- data.frame(PatientName = name,
                                   Medication = medication,
                                   Dosage = dosage,
                                   Frequency = frequency)
    updated_prescriptions <- rbind(current_prescriptions, new_prescription)
    
    prescriptions(updated_prescriptions)
  }
  
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
}

# Run the application
shinyApp(ui = ui, server = server)
