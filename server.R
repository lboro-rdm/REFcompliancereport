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
  
  # --- Exclusion list: read from CSV in app folder, refreshable ---
  excluded_ids <- reactiveVal(character(0))
  
  excluded_reasons <- reactiveVal(data.frame(article_id = integer(0), reason = character(0)))
  
  load_exclusions <- function() {
    path <- "excluded_articles.csv"
    if (file.exists(path)) {
      raw <- read.csv(path) %>%
        clean_names()
      
      trimmed_handles <- str_trim(as.character(raw$handle))
      ids <- as.integer(str_extract(trimmed_handles, "\\d+$"))  # convert to integer
      
      excluded_ids(ids)
      excluded_reasons(data.frame(
        article_id = ids,
        reason     = str_trim(as.character(raw$reason)),
        stringsAsFactors = FALSE
      ))
    } else {
      excluded_ids(integer(0))
      excluded_reasons(data.frame(article_id = integer(0), reason = character(0)))
    }
  }
  
  # Load on startup
  load_exclusions()
  
## REACTIVE SOT LOAD CSV
  
  # Reactive: read uploaded batch CSV
  all_data <- reactive({
    req(input$batch_file)
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
  
  # Reactive: read uploaded missing pubs CSV
  missing_pubs <- reactive({
    req(input$missing_pubs_file)
    read.csv(input$missing_pubs_file$datapath) %>%
      clean_names()
  })
  
  # Reactive: read uploaded archive CSV
  missing_pubs_archive <- reactive({
    req(input$missing_pubs_archive_file)
    read.csv(input$missing_pubs_archive_file$datapath) %>%
      clean_names() %>%
      rename(handle = id)
  })
  
  # Reactive: read uploaded non-ref compliant CSV
  missing_pubs_ref <- reactive({
    req(input$missing_pubs_ref_file)
    read.csv(input$missing_pubs_ref_file$datapath) %>%
      clean_names() %>%
      rename(handle = id)
  })
  
## REACTIVES TO CLEAN DATA
  
  # Reactive: filtered embargoed data - cleans batch to only include thos that are embargoed and are the right type
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
  
  # Reactive: filtered_data base - creates a report with selected columns instead of the hundreds that are available on batch
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
          paste0("https://hdl.handle.net/", handle), # handles changed to URLs
          NA_character_
        ),
        months_to_embargo = case_when(
          !is.na(embargo_date) ~ interval(today(), embargo_date) %/% months(1),
          TRUE ~ NA_integer_
        )
      )
  })
  
  # Reactive: Correct Version Report
  perm_embargo <- reactive({
    req(filtered_data())
    
    # Strip a trailing ".vN" version suffix so it matches the base `handle` field
    # e.g. "2134/24877332.v1" -> "2134/24877332"
    strip_version <- function(x) {
      sub("\\.v[0-9]+$", "", x)
    }
    
    missing_handles <- if (!is.null(input$missing_pubs_file)) {
      strip_version(missing_pubs()$handle)
    } else {
      character(0)
    }
    
    archive_handles <- if (!is.null(input$missing_pubs_archive_file)) {
      strip_version(missing_pubs_archive()$handle)
    } else {
      character(0)
    }
    
    ref_handles <- if (!is.null(input$missing_pubs_ref_file)) {
      strip_version(missing_pubs_ref()$handle)
    } else {
      character(0)
    }
    
    all_flag_levels <- c(
      "RED", "AMBER", "GREEN", "GREY",
      "GREEN - MISSING PUB", "GREEN - ARCHIVE",
      "GREEN - NON-REF COMPLIANT", "EXCLUDED"
    )
    
    filtered_data() %>%
      filter(is.na(embargo_date)) %>%
      left_join(excluded_reasons(), by = "article_id") %>% 
      mutate(
        days_old = as.numeric(Sys.Date() - timeline_pub_date),
        flag = case_when(
          article_id %in% excluded_ids()               ~ "EXCLUDED",
          is.na(timeline_pub_date)                     ~ "GREEN",
          days_old <= 60                               ~ "AMBER",
          days_old <= 90                               ~ "RED",
          TRUE                                         ~ "GREY"
        ),
        flag = factor(flag, levels = all_flag_levels),
        flag = case_when(
          flag == "GREEN" & handle %in% missing_handles ~ factor("GREEN - MISSING PUB",      levels = all_flag_levels),
          flag == "GREEN" & handle %in% archive_handles ~ factor("GREEN - ARCHIVE",           levels = all_flag_levels),
          flag == "GREEN" & handle %in% ref_handles     ~ factor("GREEN - NON-REF COMPLIANT", levels = all_flag_levels),
          TRUE                                          ~ flag
        ),
        comment = if_else(flag == "EXCLUDED", reason, "")
      ) %>%
      select(-days_old, -reason) %>%
      relocate(comment, .after = flag) %>%
      arrange(flag)
  })
  
  # Reactive: REF Compliant Embargo Report
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
          as.character(article_id) %in% excluded_ids()              ~ "EXCLUDED",
          is.na(timeline_pub_date)                                   ~ "IGNORE",
          year(timeline_pub_date) < 2026 & category == "STEM"  & timeline >= 12 ~ "CONTACT RIO",
          year(timeline_pub_date) < 2026 & category == "HASS"  & timeline >= 24 ~ "CONTACT RIO",
          year(timeline_pub_date) >= 2026 & category == "STEM" & timeline >= 6  ~ "CONTACT RIO",
          year(timeline_pub_date) >= 2026 & category == "HASS" & timeline >= 12 ~ "CONTACT RIO",
          TRUE ~ "COMPLIANT"
        ),
        flag = factor(flag, levels = c("CHECK", "CONTACT RIO", "COMPLIANT", "IGNORE", "EXCLUDED"))
      ) %>%
      arrange(flag)
  })
  
  # Reactive value to store the currently active report
  active_report <- reactiveVal(NULL)
  
  observeEvent(input$permReportBtn, {
    active_report(perm_embargo())
  })
  
  observeEvent(input$tempReportBtn, {
    active_report(temp_embargo())
  })
  
  # Render table
  output$reportTable <- renderDT({
    req(active_report())
    datatable(
      active_report(),
      options = list(
        pageLength = 25,
        autoWidth = TRUE,
        orderClasses = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Download handler
  output$downloadReport <- downloadHandler(
    filename = function() {
      if (identical(active_report(), perm_embargo())) {
        paste0("Correct version report_", Sys.Date(), ".csv")
      } else {
        paste0("REF compliant embargo report_", Sys.Date(), ".csv")
      }
    },
    content = function(file) {
      req(active_report())
      write.csv(active_report(), file, row.names = FALSE)
    }
  )
  
})