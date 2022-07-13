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
library(tidyr)
library(tibble)
library(gt)

orcid_cr_sherpa <- read_csv("./data/results/orcid_cr_sherpa.csv")


# look at the accepted versions a single person can deposit without paying a fee

orcid_ids <- "0000-0003-0802-6881"

single_user_table <- orcid_cr_sherpa %>%
  filter(orcid_identifier %in% orcid_ids,
         oa_fee == "no",
         str_detect(article_version, "accepted"))

# which published versions can be deposited in the IR?
vor_ir <- orcid_cr_sherpa %>%
  filter(oa_fee == "no",
         str_detect(article_version, "published"),
         is.na(prerequisites),
         str_detect(location, "institutional"))
