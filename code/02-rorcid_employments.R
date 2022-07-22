# load the packages
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

# build the query  --------------------------------------------------------

ringgold_id <- "enter your institution's ringgold" 
email_domain <- "enter your institution's email domain" 
organization_name <- "enter your organization's name"
# grid_id <- "enter your institution's grid ID" 
# ror_id <- "enter your institution's ROR ID"


# example
# ringgold_id <- "7618"
# email_domain <- "@okstate.edu"
# organization_name <- "Oklahoma State University"
# grid_id <- "grid.65519.3e"
# ror_id <- "https://ror.org/01g9vbr38"


# create the query
my_query <- glue('ringgold-org-id:',
                 ringgold_id,
                 ' OR email:*',
                 email_domain,
                 ' OR affiliation-org-name:"',
                 organization_name,
                 '"')

# here is the query you will use if you found your
# institution's GRID and ROR IDs.
# my_query <- glue('ringgold-org-id:', ringgold_id, ' OR grid-org-id:', grid_id, ' OR ror-org-id:"', ror_id, '" OR email:*', email_domain, ' OR affiliation-org-name:"', organization_name, '"')

# examine my_query
my_query

# get the counts
orcid_count <- base::attr(rorcid::orcid(query = my_query),
                          "found")

# create the page vector
my_pages <- seq(from = 0, to = orcid_count, by = 200)

# get the ORCID iDs
my_orcids <- purrr::map(
  my_pages,
  function(page) {
    print(page)
    my_orcids <- rorcid::orcid(query = my_query,
                               rows = 200,
                               start = page)
    return(my_orcids)
  })

# put the ORCID iDs into a single tibble
my_orcids_data <- my_orcids %>%
  map_dfr(., as_tibble) %>%
  janitor::clean_names()

write_csv(my_orcids_data, "./data/processed/my_orcids_data.csv")


# get employment data -----------------------------------------------------

# get the employments from the orcid_identifier_path column

# for the purposes of this class, we will only be retrieving data for the first 50 people.

###################################################
## When you run this on your own after the class,##
############### REMOVE THE [1:50] #################
###################################################

# be patient, this may take a while
my_employment <- rorcid::orcid_employments(my_orcids_data$orcid_identifier_path[1:50])

# View it
View(my_employment)

# you can write the file to json if you want to work with it outside of R
# write_json(my_employment, "./data/processed/employment.json")

# here is how you would read it back in, if necessary
# my_employment <- read_json("./data/processed/employment.json", simplifyVector = TRUE)

# extract the employment data and mutate the dates
my_employment_data <- my_employment %>%
  purrr::map(., purrr::pluck, "affiliation-group", "summaries") %>% 
  purrr::flatten_dfr() %>%
  janitor::clean_names() %>%
  dplyr::mutate(employment_summary_end_date = anytime::anydate(employment_summary_end_date/1000),
                employment_summary_created_date_value = anytime::anydate(employment_summary_created_date_value/1000),
                employment_summary_last_modified_date_value = anytime::anydate(employment_summary_last_modified_date_value/1000))

# View it
View(my_employment_data)


# clean up the column names
names(my_employment_data) <- names(my_employment_data) %>%
  stringr::str_replace(., "employment_summary_", "") %>%
  stringr::str_replace(., "source_source_", "") %>%
  stringr::str_replace(., "organization_disambiguated_", "")

# view the unique institutions in the organization names columns
# keep in mind this will include all institutions a person has in their employments section
my_organizations <- my_employment_data %>%
  group_by(organization_name) %>%
  count() %>%
  arrange(desc(n))

# you can also filter it with a keyword or set of keywords.
# Keep it short, like the state name (e.g. Oklahoma).
# If you are adding more than one keyword, separate them by a pipe (|)
# for example, "Oklahoma|Okla"
my_organizations_filtered <- my_organizations %>%
  filter(str_detect(organization_name, "KEYWORD"))

# filter the dataset to include only the institutions you want. 
# There may be different variants depending on if the person
# hand-entered the data. Referring to the my_organizations_filtered
# file, add in all numbers that match 
my_employment_data_filtered <- my_employment_data %>%
  dplyr::filter(organization_name %in% my_organizations_filtered$organization_name[c(1)])


# finally, filter to include only people who have NA as the end date
my_employment_data_filtered_current <- my_employment_data_filtered %>%
  dplyr::filter(is.na(end_date_year_value))

# note that this will give you employment records ONLY. 
# In other words, each row represents a single employment record for an individual.
# the name_value variable refers specifically to the name of the person or system
# that wrote the record, NOT the name of the individual. 

# To get that, you must first get all the unique ORCID iDs from the dataset:

# There is actually no distinct value identifying the orcid ID of the person.
# The orcid_path value corresponds to the path of the person who added the employment record (which is usually, but not always the same)
# Therefore you have to strip out the ORCID iD from the 'path' variable first and put it in it's own value and use it
# We do this using str_sub from the stringr package
# While we are at it, we can select and reorder the columns we want to keep
current_employment_all <- my_employment_data_filtered_current %>%
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
         

# next, create a new vector unique_orcids that includes only unique ORCID iDs from our filtered dataset.     
unique_orcids <- unique(current_employment_all$orcid_identifier) %>%
  na.omit(.) %>%
  as.character()

# then run the following expression to get all biographical information for those iDs.
# This will take a few seconds to process
my_orcid_person <- rorcid::orcid_person(unique_orcids)

# then we construct a data frame from the response. 
# See more at https://ciakovx.github.io/rorcid.html#Getting_the_data_into_a_data_frame for this.

my_orcid_person_data <- my_orcid_person %>% {
  dplyr::tibble(
    given_name = purrr::map_chr(., purrr::pluck, "name", "given-names", "value", .default=NA_character_),
    created_date = purrr::map_chr(., purrr::pluck, "name", "created-date", "value", .default=NA_integer_),
    last_modified_date = purrr::map_chr(., purrr::pluck, "name", "created-date", "value", .default=NA_character_),
    family_name = purrr::map_chr(., purrr::pluck, "name", "family-name", "value", .default=NA_character_),
    credit_name = purrr::map_chr(., purrr::pluck, "name", "credit-name", "value", .default=NA_character_),
    other_names = purrr::map(., purrr::pluck, "other-names", "other-name", "content", .default=NA_character_),
    orcid_identifier_path = purrr::map_chr(., purrr::pluck, "name", "path", .default = NA_character_),
    biography = purrr::map_chr(., purrr::pluck, "biography", "content", .default=NA_character_),
    researcher_urls = purrr::map(., purrr::pluck, "researcher-urls", "researcher-url", .default=NA_character_),
    emails = purrr::map(., purrr::pluck, "emails", "email", "email", .default=NA_character_),
    keywords = purrr::map(., purrr::pluck, "keywords", "keyword", "content", .default=NA_character_),
    external_ids = purrr::map(., purrr::pluck, "external-identifiers", "external-identifier", .default=NA_character_))
  } %>%
  dplyr::mutate(created_date = anytime::anydate(as.double(created_date)/1000),
                last_modified_date = anytime::anydate(as.double(last_modified_date)/1000))

# Join it back with the employment records
orcid_person_employment_join <- my_orcid_person_data %>%
  left_join(current_employment_all, by = c("orcid_identifier_path" = "orcid_identifier"))

# now you can write this file to a CSV
write_csv(orcid_person_employment_join, "./data/results/orcid_employment_file.csv")


#
# if you are a part of an ORCID member institution, 
# you can get this data more easily from the member portal
# at https://member-portal.orcid.org/ and it's quite comparable
#

# exploring departments ---------------------------------------------------

depts <- orcid_person_employment_join %>%
  mutate(department_name = str_remove(department_name, "[Ss]chool [Oo]f |[Dd]epartment [Oo]f"),
         department_name = tolower(department_name),
         department_name = str_replace_all(department_name, "&", "and"),
         department_name = str_remove_all(department_name, "[[:punct:]]"),
         department_name = str_trim(department_name))

dept_tally <- depts %>%
  group_by(department_name) %>%
  tally() %>%
  arrange(desc(n)) %>%
  filter(!is.na(department_name))

# you might need to adjust the n > depending on what your data looks like
dept_plot <- dept_tally %>%
  filter(n >= 2) %>%
  ggplot(aes(x = fct_reorder(department_name, n), y = n)) + 
  geom_bar(stat = "identity") +
  coord_flip()

print(dept_plot)



