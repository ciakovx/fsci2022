# load packages
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(jsonlite)
library(lubridate)
library(ggplot2)
library(httr)
library(forcats)
library(usethis)
library(anytime)
library(janitor)
library(glue)
library(rorcid)
library(rcrossref)
library(roadoi)
library(inops)


# Education ---------------------------------------------------------------

# you can also get data on people whose degree information includes your university
# then filter that to get current students

# start with my_orcids_data as we collected in 02

my_orcids_data <- read_csv("./data/processed/my_orcids_data.csv",
                           col_types = cols(.default = "c"))

# get education data
# this will take a while, be patient
my_education <- rorcid::orcid_educations(my_orcids_data$orcid_identifier_path)

# then generally follow the steps above, making modifications to variable names as necessary.
my_education_data <- my_education %>%
  purrr::map(., purrr::pluck, "affiliation-group", "summaries") %>% 
  purrr::flatten_dfr() %>%
  janitor::clean_names() %>%
  dplyr::mutate(education_summary_end_date = anytime::anydate(education_summary_end_date/1000),
                education_summary_created_date_value = anytime::anydate(education_summary_created_date_value/1000),
                education_summary_last_modified_date_value = anytime::anydate(education_summary_last_modified_date_value/1000))

names(my_education_data) <- names(my_education_data) %>%
  stringr::str_replace(., "education_summary_", "") %>%
  stringr::str_replace(., "source_source_", "") %>%
  stringr::str_replace(., "organization_disambiguated_", "")

my_education_organizations <- my_education_data %>%
  group_by(organization_name) %>%
  count() %>%
  arrange(desc(n))

# add additional integers from the row numbers in my_education_organizations
# if you want them included
my_education_data_filtered <- my_education_data %>%
  dplyr::filter(organization_name %in% my_education_organizations$organization_name[c(1)])

# subset to include only current students.
# if you want all students, don't run this, and delete 
# all instances of the word current below
my_education_data_filtered_current <- my_education_data_filtered %>%
  dplyr::filter(is.na(end_date_year_value))

current_education_all <- my_education_data_filtered_current %>%
  mutate(orcid_identifier = str_sub(path, 2, 20)) %>%
  select(any_of(c("orcid_identifier",
                  "organization_name",
                  "organization_address_city",
                  "organization_address_region",
                  "organization_address_country",
                  "organization_identifier",
                  "organization_disambiguated_organization_identifier",
                  "organization_disambiguation_source",
                  "department_name",
                  "role_title",
                  "url_value",
                  "display_index",
                  "visibility",
                  "created_date_value",
                  "start_date_year_value",
                  "start_date_month_value",
                  "start_date_day_value",
                  "end_date_year_value",
                  "end_date_month_value",
                  "end_date_day_value")))

unique_orcids <- unique(current_education_all$orcid_identifier) %>%
  na.omit(.)    

# then run the following expression to get all biographical information for those iDs
my_orcid_person <- rorcid::orcid_person(unique_orcids)

# then we construct a data frame from the JSON file. 
# See more on my website for this.

my_orcid_student_data <- my_orcid_person %>% {
  dplyr::tibble(
    created_date = purrr::map_chr(., purrr::pluck, "name", "created-date", "value", .default=NA_character_),
    given_name = purrr::map_chr(., purrr::pluck, "name", "given-names", "value", .default=NA_character_),
    family_name = purrr::map_chr(., purrr::pluck, "name", "family-name", "value", .default=NA_character_),
    orcid_identifier_path = purrr::map_chr(., purrr::pluck, "name", "path", .default = NA_character_))
} %>%
  dplyr::mutate(created_date = anytime::anydate(as.double(created_date)/1000))

# you can write this file to a CSV. Specify the path name inside the quotes
write_csv(my_orcid_student_data, "./data/processed/orcid_student_file.csv")

# if you want to join it back with the employment records and keep only relevant 
# columns, do this:

orcid_student_education_join <- my_orcid_student_data %>%
  left_join(current_education_all, by = c("orcid_identifier_path" = "orcid_identifier"))

# now you can write this file to a CSV
write_csv(orcid_student_education_join, "C:/Users/user/Desktop/orcid_education_file.csv")