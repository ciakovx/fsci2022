# load the required packages
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

# remove all objects from the environment
# to start with a clean slate
rm(list = ls())

# Read in the orcid person data we collected in 02-rorcid_employments
orcid_ids <- read_csv("./data/results/orcid_employment_file.csv",
                      col_types = cols(.default = "c"))

# create a vector of unique, unduplicated ORCID IDs from that file
my_orcids <- orcid_ids %>%
  filter(!duplicated(orcid_identifier_path)) %>%
  pull(orcid_identifier_path) %>%
  na.omit() %>%
  as.character()

# Call the orcid_works function to collect all works associated with each ID
# This may take a few seconds up to a few minutes
my_works <- rorcid::orcid_works(my_orcids)

# optional: write this file to json
# write_json(my_works, "./data/raw/my_works.json")

# if necessary, read the file from json
## my_works <- read_json("./data/raw/my_works.json", simplifyVector = TRUE)

# turn the JSON file into a unique data frame by looping through the file,
# extracting ("pluck") the object, bind the rows together with(this is the "_dfr" part of map_dfr)
# then clean column names
# and convert the dates from Unix time to yyyy-mm-dd
my_works_data <- my_works %>%
  purrr::map_dfr(pluck, "works") %>%
  janitor::clean_names() %>%
  dplyr::mutate(created_date_value = anytime::anydate(created_date_value/1000),
                last_modified_date_value = anytime::anydate(last_modified_date_value/1000))



# for the purpose of this class, we only want to keep works that have an external identifier
# (specifically, a DOI), so we first filter to keep only objects that have an external_id value
# then unnest those: in other words expand to include a row for every work + external id value
# (in other words, one work might be linked to a DOI, a PubMed ID, an ISSN, etc.)
my_works_externalIDs <- my_works_data %>% 
  dplyr::filter(!purrr::map_lgl(external_ids_external_id, purrr::is_empty)) %>% 
  tidyr::unnest(external_ids_external_id) %>%
  clean_names()


# From those unnested external IDs, we want to keep only those with a DOI, as that is the 
# value we'll use to look up the items in Crossref and Unpaywall.
# We then select a few relevant columns, and finally create a new column DOI that takes the external_id_value column
# and coerces it to lower case, and the orcid_identifier column which strips out the ORCID ID
# from the path variable.
dois <- my_works_externalIDs %>%
  filter(external_id_type == "doi") %>%
  select(type, path, title_title_value, external_id_type, external_id_value, external_id_relationship,
         url_value, publication_date_year_value, publication_date_month_value, publication_date_day_value,
         journal_title_value) %>%
  mutate(doi = tolower(external_id_value),
         orcid_identifier = str_sub(path, 2, 20))

# there are some duplicated values here: we can't just look at duplicate DOIs because some of these
# works were co-authored, and we want to keep that data (i.e. unique orcid + doi combinations)
# This function will let you look at observations where both the orcid ID and the DOI are duplicated in 
# case you want to review them more closely. 
# In our case below, we just keep the first appearance of a unique orcid + doi combination and discard
# all subsequent ones.
dupes <- dois %>%
  get_dupes(orcid_identifier, doi)


# Here we are preparing the orcid dataset for merging to publications. 
# We keep only Orcid ID, first name and last name, remove duplicates, and rename orcid_identifier
orcid_empl_merge <- orcid_ids %>%
  select(orcid_identifier_path, given_name, family_name) %>%
  filter(!duplicated(orcid_identifier_path)) %>%
  rename(orcid_identifier = orcid_identifier_path)

# Finally, we remove the duplicates by creating a new variable that is a combination of
# the orcid ID and the DOI, and keeping only the first instance. We then join that to our 
# cleaned orcid ID file and write to csv
dois_unduped <- dois %>%
  mutate(orcid_doi = paste0(orcid_identifier, doi)) %>%
  filter(!duplicated(orcid_doi)) %>%
  left_join(orcid_empl_merge, by = "orcid_identifier")

# write the csv
write_csv(dois_unduped, "./data/results/orcid_dois.csv")
