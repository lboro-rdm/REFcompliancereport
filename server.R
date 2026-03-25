library(shiny)
library(tidyverse)
library(janitor)
library(DT)

options(shiny.maxRequestSize = 200 * 1024^2)

shinyServer(function(input, output, session) {

  # HASS/STEM school lists
  hass_schools <- c(
    "Business and Economics", "Design", "Design and Creative Arts",
    "Loughborough Business School", "Loughborough University London",
    "Loughborough University, London", "Social Sciences",
    "Social Sciences and Humanities", "The Arts, English and Drama",
    "University Academic and Administrative Support"
  )
  
  stem_schools <- c(
    "Aeronautical, Automotive, Chemical and Materials Engineering",
    "Architecture, Building and Civil Engineering",
    "Mechanical, Electrical and Manufacturing Engineering",
    "Science", "Sport, Exercise and Health Sciences"
  )
  
  # Reactive: read uploaded CSV
  all_data <- reactive({
    req(input$batch_file)  # ensure file is uploaded
    read.csv(input$batch_file$datapath) %>%
      rename(
        "timeline_pub_date" = "publication_date",
        "custom_pub_date" = "Publication.date"
      ) %>%
      clean_names() %>%
      mutate(
        embargo_date = as.Date(embargo_date),
        timeline_pub_date = as.Date(timeline_pub_date)
      )
  })
  
  # Reactive: filtered embargoed data
  embargoed_data <- reactive({
    req(all_data())
    all_data() %>%
      filter(
        is_embargoed == 1,
        is.na(embargo_date) | embargo_date > today(),
        is.na(timeline_pub_date) | timeline_pub_date > as.Date("2021-01-01"),
        item_type %in% c("journal contribution", "conference contribution")
      ) %>%
      arrange(embargo_date)
  })
  
  # Reactive: filtered_data base
  filtered_data <- reactive({
    req(embargoed_data())
    embargoed_data() %>%
      select(
        article_id, title, authors, school, item_type, handle,
        embargo_reason, embargo_date, acceptance_date, timeline_pub_date,
        custom_pub_date, first_online_date, version
      ) %>%
      mutate(
        handle = if_else(
          !is.na(handle),
          paste0("https://hdl.handle.net/", handle),
          NA_character_
        ),
        months_to_embargo = case_when(
          !is.na(embargo_date) ~ interval(today(), embargo_date) %/% months(1),
          TRUE ~ NA_integer_
        )
      )
  })
  
  # Reactive: Permanent embargo report - renamed to Correct Version Report
  perm_embargo <- reactive({
    req(filtered_data())
    filtered_data() %>%
      filter(is.na(embargo_date)) %>%
      mutate(
        flag = case_when(
          is.na(timeline_pub_date) ~ "GREEN",
          interval(timeline_pub_date, Sys.Date()) %/% months(1) <= 1 ~ "RED",
          interval(timeline_pub_date, Sys.Date()) %/% months(1) <= 3 ~ "AMBER",
          interval(timeline_pub_date, Sys.Date()) %/% months(1) > 3 ~ "GREY"
        ),
        flag = factor(flag, levels = c("RED", "AMBER", "GREEN", "GREY")),
        status = "",
        comment = ""
      ) %>%
      arrange(flag)
  })
  
  # Reactive: Temporary embargo report - Renamed to REF Compliant Embargo Report
  temp_embargo <- reactive({
    req(filtered_data())
    filtered_data() %>%
      mutate(
        school_clean = str_remove_all(school, '\\[|\\]|"|\\\\') %>% str_trim(),
        category = case_when(
          school_clean %in% hass_schools ~ "HASS",
          school_clean %in% stem_schools ~ "STEM",
          TRUE ~ "CHECK"
        ),
        timeline = interval(timeline_pub_date, embargo_date) %/% months(1),
        flag = case_when(
          is.na(timeline_pub_date) ~ "IGNORE",
          year(timeline_pub_date) < 2026 & category == "STEM" & timeline >= 12 ~ "CONTACT RIO",
          year(timeline_pub_date) < 2026 & category == "HASS" & timeline >= 24 ~ "CONTACT RIO",
          year(timeline_pub_date) >= 2026 & category == "STEM" & timeline >= 6 ~ "CONTACT RIO",
          year(timeline_pub_date) >= 2026 & category == "HASS" & timeline >= 12 ~ "CONTACT RIO",
          TRUE ~ "COMPLIANT"
        ),
        flag = factor(flag, levels = c("CHECK", "CONTACT RIO", "COMPLIANT", "IGNORE"))
      ) %>%
      arrange(flag)
  })
  
  # Reactive value to store the currently active report
  active_report <- reactiveVal(NULL)
  
  # Buttons to select report
  observeEvent(input$permReportBtn, {
    active_report(perm_embargo())
  })
  
  observeEvent(input$tempReportBtn, {
    active_report(temp_embargo())
  })
  
  # Render table of active report
  output$reportTable <- renderDT({
    req(active_report())
    datatable(
      active_report(),
      options = list(
        pageLength = 25,      # rows per page
        autoWidth = TRUE,
        #scrollX = TRUE,       # horizontal scroll if needed
        orderClasses = TRUE   # highlights sorted column
      ),
      rownames = FALSE
    )
  })
  
  # Download handler for active report
  output$downloadReport <- downloadHandler(
    filename = function() {
      if (identical(active_report(), perm_embargo())) {
        paste0("perm_embargo_report_", Sys.Date(), ".csv")
      } else {
        paste0("temp_embargo_report_", Sys.Date(), ".csv")
      }
    },
    content = function(file) {
      req(active_report())
      write.csv(active_report(), file, row.names = FALSE)
    }
  )
  
})