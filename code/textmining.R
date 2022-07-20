library(rorcid)
library(httr)
library(usethis)
library(anytime)
library(lubridate)
library(janitor)
library(glue)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(jsonlite)
library(ggplot2)
library(forcats)
library(rcrossref)
library(roadoi)
library(inops)

# read in the crossref/orcid/unpaywall data and create a new 
# variable filename that is the first five characters of the author's last name
# and the first five characters of the title
orcid_cr_oa <- read_csv("./data/results/orcid_cr_oa_merge.csv") %>%
  mutate(filename = make_clean_names(paste(str_sub(family_name, 1, 5), str_sub(title, 1, 5), sep = "_")))


# linklist <- metadata_2021_df %>%
#   unnest(link) %>%
#   filter(content.type == "application/pdf" | content.type == "unspecified",
#          !duplicated(doi)) %>%
#   select(doi, URL) %>%
#   rename(pdf_url = URL)
# 
# metadata_2021_df <- metadata_2021_df %>%
#   left_join(linklist, by = "doi")


# wrap GET in safely in order to keep a failed search from terminating the loop
safeget <- safely(GET)

my_pdfs <- orcid_cr_oa %>%
  filter(!is.na(url_for_pdf)) %>%
  slice(1:20) %>%
  pmap(function(...) {
    current <- tibble(...)
    print(current$url_for_pdf)
    n <- safeget(current$url_for_pdf,
                 add_headers(`Accept` = "application/pdf",
                             `Content-Type` = "application/pdf",
                             `X-Content-Type-Options` = "nosniff"),
                 write_disk(paste0("./minedpdfs/", current$filename, ".pdf")))
    return(n)
    Sys.sleep(0.5)
  })


# if any of them don't work, you can try to replace the url_for_pdf variable with pdf_url
# but in my experience these are comparable

