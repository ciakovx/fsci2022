# load pcckages
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


# start with a vector of references
# you can paste references from a document into excel and save it as a CSV, then 
# read it into R with read_csv
myrefs <- c("Frosio, G. F. (2014). Open Access Publishing: A Literature Review. SSRN Electronic Journal. https://doi.org/10.2139/ssrn.2697412", 
            "Laakso, M., & Bjork, B.-C. (2016). Hybrid open access: A longitudinal study. Journal of Informetrics, 10(4), 919-932. https://doi.org/10.1016/j.joi.2016.08.002", 
            "Laakso, M., Welling, P., Bukvova, H., Nyman, L., Bjork, B.-C., & Hedlund, T. (2011). The Development of Open Access Journal Publishing from 1993 to 2009. PLoS ONE, 6(6), e20961. https://doi.org/10.1371/journal.pone.0020961",
            "Paulus, F. M., Cruz, N., & Krach, S. (2018). The Impact Factor Fallacy. Frontiers in Psychology, 9. https://doi.org/10.3389/fpsyg.2018.01487",
            "Science, Digital; Hook, Daniel; Hahnel, Mark; Calvert, Ian (2019): The Ascent of Open Access. figshare. Journal contribution. https://doi.org/10.6084/m9.figshare.7618751",
            "Shotton, D. (2018). Funders should mandate open citations. Nature, 553(7687), 129-129. https://doi.org/10.1038/d41586-018-00104-7",
            "Suber, P. (2012). Open access. Cambridge, Mass: MIT Press.",
            "Tennant, J. P., Waldner, F., Jacques, D. C., Masuzzo, P., Collister, L. B., & Hartgerink, C. H. J. (2016). The academic, economic and societal impacts of Open Access: an evidence-based review. F1000Research, 5, 632. https://doi.org/10.12688/f1000research.8460.2")



# query the bibliographic fields for the entire reference
# you can change the limit but that will return more articles per request
my_query <- map(myrefs, function(z) {
  print(z)
  o <- cr_works(flq = c(query.bibliographic = z),
                limit = 2) %>%
    pluck("data")
  return(o)
})

# flatten it into a data frame
my_dat <- my_query %>%
  map_dfr(., bind_rows) %>%
  select(score, title, everything())

# remove invalid rows
# notice that row 13 is a book review, but row 14 is the book
my_dat_clean <- my_dat %>%
  slice(-c(2, 4, 6, 8, 9, 10, 12, 13, 15))  


# write to CSV
write_csv(my_dat_clean, "./data/results/refs.csv")
