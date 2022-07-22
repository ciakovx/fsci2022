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

dois_unduped <- read_csv("./data/results/orcid_dois.csv")

# We start by subsetting our unduped dois to include only the year 2021.
# I had 11,000 DOIs and that's just too many to do all at once. 
# If you have a large amount of data, you might break it down for 2-3 year chunks
dois_2021 <- dois_unduped %>%
  filter(publication_date_year_value >= 2021)

# we wrap the cr_works in slowly and put a delay of 2 seconds after each call
# normally you don't need to do this, but since everyone in the class will be
# using it at once, we'll use it during the class.
# When you're done with the class, you can just call cr_works in the loop below
# rather than slow_cr_works
slow_cr_works <- slowly(cr_works, rate_delay(2))

# This will loop through the column of dois and perform a function that
# prints the doi (this allows you to ensure it's progressing)
# send the request
# return the result
# pause the system for 0.5 seconds
metadata_2021 <- map(dois_2021$doi, function(z) {
  print(z)
  o <- slow_cr_works(dois = z)
  return(o)
})

# optional:  write the json file to disk so you have it
# write_json(metadata_2021, "./data/processed/metadata_2021.json")

# read json, if necessary
# metadata_2021_df <- read_json("./data/processed/metadata_2021.json", simplifyVector = TRUE) %>%
#   pluck("data") %>%
#   bind_rows() %>%
#   clean_names() %>%
#   filter(!duplicated(doi))

# This will loop through each result, extract ("pluck") the object called "data"
# bind it together into a dataframe (the "dfr" part of map_dfr)
# clean the names up and filter to remove any duplicates
metadata_2021_df <- metadata_2021 %>%
  map_dfr(., pluck("data")) %>%
  clean_names() %>%
  filter(!duplicated(doi))

# We next want to prepare our orcid data frame to merge to the crossref data by selecting only the relevant columns
orcid_merge <- dois_2021 %>%
  select(orcid_identifier, doi, given_name, family_name)

# Have a look at the names of the columns to see which ones we might want to keep
View(as.data.frame(names(metadata_2021_df)))

# unnest the link list, keep the links for application/pdf
# this will be used if you choose to do text mining after the class
linklist <- metadata_2021_df %>%
  unnest(link) %>%
  filter(content.type == "application/pdf" | content.type == "unspecified",
         !duplicated(doi)) %>%
  select(doi, URL) %>%
  rename(pdf_url = URL)

metadata_2021_df <- metadata_2021_df %>%
  left_join(linklist, by = "doi")

# select relevant columns
cr_merge <- metadata_2021_df %>%
  select(any_of(c("doi",
                "title",
                "published_print", 
                "published_online", 
                "issued", 
                "container_title",
                "issn",
                "volume",
                "issue",
                "page",
                "publisher",
                "language",
                "isbn",
                "url",
                "type",
                "subject",
                "reference_count",
                "is_referenced_by_count",
                "subject",
                "alternative_id",
                "author",
                "pdf_url")))

# The authors are in a nested list. In order to collect them into a single value, we must
# unnest the list,
# create a new column full_name that pastes together the first name and the last name
# group the dataset by doi so we can collect each author per article
# create a new column all_auths that pastes together the authors from each article and separates them with a comma
# remove duplicates
# select only doi and authors so we can merge this back to the primary data frame
authlist <- cr_merge %>%
  unnest(author) %>%
  mutate(full_name = paste(given, family)) %>% 
  group_by(doi) %>%
  mutate(all_auths = paste(full_name, collapse = ", ")) %>%
  filter(!duplicated(doi)) %>%
  select(doi, all_auths)

# merge and remove the now extraneous nested author column
cr_merge <- cr_merge %>%
  left_join(authlist, by = "doi") %>%
  select(-author)



# now merge to the orcid file
# also there were some items that had two ISSNs separated by a comma
# we separate those into two columns with separate()
# you might get a warning here, but it's fine
# it's just telling you that there were some rows where only one 
# issn existed, so it was unable to separate it into issn1 and issn2
orcid_cr_merge <- orcid_merge %>%
  left_join(cr_merge, by = "doi") %>%
  select(orcid_identifier, doi, given_name, family_name, all_auths, everything()) %>%
  separate(issn, into = c("issn1", "issn2"), sep = ",")

# and write it, this time to results because this is relatively finalized
write_csv(orcid_cr_merge, "./data/results/orcid_cr_merge.csv")




# looking at the data -----------------------------------------------------

# create a table looking at the number of articles per journal
top_journals <- orcid_cr_merge %>%
  filter(!is.na(container_title)) %>%
  group_by(container_title) %>%
  tally() %>%
  arrange(desc(n))

# sort by top cited
top_cited <- orcid_cr_merge %>%
  mutate(is_referenced_by_count = as.integer(is_referenced_by_count)) %>%
  arrange(desc(is_referenced_by_count))

# line plot of publication date (issued)
issued_plot <- orcid_cr_merge %>%
  mutate(issued_fix = str_sub(issued, start = 1, end = 7),
         issued_fix = ym(issued_fix)) %>%
  count(issued_fix) %>%
  ggplot(aes(x = issued_fix, y = n)) + 
  geom_line()
print(issued_plot)

