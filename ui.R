library(shiny)
library(DT)
library(dadjoke)


shinyUI(fluidPage(
  
  titlePanel("REF Compliance Reports"),

  tabsetPanel(
    
    tabPanel("Reports",
    
  sidebarLayout(
    sidebarPanel(
      h3("Step 1: upload the batch file"),
      fileInput("batch_file", "batch.csv", accept = c(".csv")),
      p("Wait for the upload to be complete before moving onto Step 2."),
      p("Here's a dad joke while you wait:"),
      paste(capture.output(dadjoke::dadjoke()), collapse = "\n"),
      h3("Step 2: choose your report"),
      actionButton("permReportBtn", "Correct Version Report"),
      br(), br(),
      actionButton("tempReportBtn", "REF Compliant Embargo Report"),
      br(), br(),
      h3("Step 3: download your report"),
      downloadButton("downloadReport", "Download Report")
    ),
    
    mainPanel(
      h4("Report Output"),
      DTOutput("reportTable")   # <-- interactive DT table
    )
  )
    ),
  
  # Tab 2: About
  tabPanel("About",
           h4("About this App"),
           p("A R/shiny app that checks a Figshare batch download for REF compliance"),
           p("Build file:"),
           tags$ul(
             tags$li("Batch download all public items from repo"),
             tags$li("Excludes items pre-2021"),
             tags$li("Include only items that have an embargo date set in the future / permanent embargo"),
             tags$li("Include only journal articles and conference papers with ISSNs")
           ),
           p("Report 1: Correct version report report"),
           tags$ul(
             tags$li("(Assumed that these items do not have the correct version of the file)"),
             tags$li("Includes all items without an embargo end date"),
             tags$li("Excludes items posted 3+ months after publication"),
             tags$li("Items that are within 0-1 months of publication, flagged as RED"),
             tags$li("Items that are within 2-3 months of publication, flagged as AMBER"),
             tags$li("Items that are 3+ months of publication, flagged as GREY"),
             tags$li("Items with no publication date, flagged as GREEN")
           ),
           p("Report 2: REF compliant embargo report"),
           tags$ul(
             tags$li("Lists all items with a temporary embargo"),
             tags$li("Includes timeline column, difference between embargo date & published date"),
             tags$li("No publication date IGNORE"),
             tags$li("Flagged CONTACT RIO",
                     tags$ul(
                       tags$li("Pre-2026, STEM, 12+ months"),
                       tags$li("Pre-2026, HASS, 24+ months"),
                       tags$li("2026+ STEM, 6+ months"),
                       tags$li("2026+ HASS, 12+ months")
                     )
             ),
             tags$li("Dates before the above timelines COMPLIANT"),
             tags$li("Unsure if STEM or HASS CHECK")
           ),
           p("Code last updated: 2026-03-25")
  )
  
  )
))