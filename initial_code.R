library(tidyverse)
library(janitor)


# School categories -------------------------------------------------------

hass_schools <- c(
  "Business and Economics",
  "Design",
  "Design and Creative Arts",
  "Loughborough Business School",
  "Loughborough University London",
  "Loughborough University, London",
  "Social Sciences",
  "Social Sciences and Humanities",
  "The Arts, English and Drama",
  "University Academic and Administrative Support"
)

stem_schools <- c(
  "Aeronautical, Automotive, Chemical and Materials Engineering",
  "Architecture, Building and Civil Engineering",
  "Mechanical, Electrical and Manufacturing Engineering",
  "Science",
  "Sport, Exercise and Health Sciences"
)

# Initial code ------------------------------------------------------------

all_data <- read.csv("batch.csv")

all_data <- all_data %>% 
  rename("timeline_pub_date" = "publication_date", "custom_pub_date" = "Publication.date") %>% 
  clean_names() %>% 
  mutate(embargo_date = as.Date(embargo_date)) %>% 
  mutate(timeline_pub_date = as.Date(timeline_pub_date))

embargoed_data <- all_data %>%
  filter(
    is_embargoed == 1,
    is.na(embargo_date) | embargo_date > today(),
    is.na(timeline_pub_date) | timeline_pub_date > 2021-01-01,
    item_type %in% c(
      "journal contribution",
      "conference contribution"
    )
  ) %>% 
  arrange(embargo_date)

filtered_data <- embargoed_data %>% 
  select(article_id, title, authors, school, item_type, handle, embargo_reason, embargo_date, acceptance_date, timeline_pub_date, custom_pub_date, first_online_date, version) %>% 
  mutate(
    months_to_embargo = case_when(
      !is.na(embargo_date) ~ interval(today(), embargo_date) %/% months(1),
      TRUE ~ NA_integer_
    )
  )

# Report 1: Permanent embargo report ------------------------------------------------

perm_embargo <- filtered_data %>% 
  filter(is.na(embargo_date)) %>% 
  mutate(
    flag = case_when(
      is.na(timeline_pub_date) ~ "GREEN",
      interval(timeline_pub_date, Sys.Date()) %/% months(1) <= 1 ~ "RED",
      interval(timeline_pub_date, Sys.Date()) %/% months(1) <= 3 ~ "AMBER",
      interval(timeline_pub_date, Sys.Date()) %/% months(1) > 3 ~ "GREY"
    ),
    status = "",
    comment = ""
  )

# Report 2: Change report -------------------------------------------------

### need to get report 1 working first


# Report 3: Temporary report ----------------------------------------------

temp_embargo <- temp_embargo %>%
  mutate(
    timeline = embargo_date - timeline_pub_date, 

    flag = case_when(
      is.na(timeline_pub_date) ~ "GREEN",  # No publication date
      # Pre-2026
      year(timeline_pub_date) < 2026 & category == "STEM" & timeline >= months(12) ~ "RED",
      year(timeline_pub_date) < 2026 & category == "HASS" & timeline >= months(24) ~ "RED",
      # 2026 or later
      year(timeline_pub_date) >= 2026 & category == "STEM" & timeline >= months(6) ~ "RED",
      year(timeline_pub_date) >= 2026 & category == "HASS" & timeline >= months(12) ~ "RED",
      TRUE ~ "AMBER"  # Everything else
    )
  )


