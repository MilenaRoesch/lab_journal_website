# 1.0 LIBRARIES ----
#install.packages("data.table")
#install.packages("vroom")
#install.packages("tictoc")


# Tidyverse
library(tidyverse)
library(vroom)

# Data Table
library(data.table)

# Counter
library(tictoc)

# Importing Data ------
# assignee
col_types <- list(
  id = col_character(),
  type = col_skip(),
  name_first = col_skip(),
  name_last = col_skip(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "00_data/04_patents/assignee.tsv", 
  delim      = "\t", 
  col_names = names(col_types),
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

setDT(assignee_tbl)

# patent assignee
col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_skip()
)

patent_assignee_tbl <- vroom(
  file       = "00_data/04_patents/patent_assignee.tsv", 
  delim      = "\t", 
  col_names = names(col_types),
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

setDT(patent_assignee_tbl)

# patent
col_types <- list(
  id = col_character(),
  type = col_skip(),
  number = col_skip(),
  country = col_skip(),
  date = col_date("%Y-%m-%d"),
  abstract = col_skip(),
  title = col_skip(),
  kind = col_skip(),
  num_claims = col_skip(),
  filename = col_skip(),
  withdrawn = col_skip()
 )
 
 patent_tbl <- vroom(
   file       = "00_data/04_patents/patent.tsv", 
   delim      = "\t", 
   col_names  = names(col_types),
   col_types  = col_types,
   na         = c("", "NA", "NULL")
 )
 
 setDT(patent_tbl)
 
# uspc
col_types <- list(
  uuid = col_skip(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_skip(),
  sequence = col_skip()
)

uspc_tbl <- vroom(
  file       = "00_data/04_patents/uspc.tsv",
  delim      = "\t",
  col_names  = names(col_types),
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

setDT(uspc_tbl)

# mainclass description
col_types <- list(
  id = col_character(),
  title = col_character())

mainclass_current_tbl <- vroom(
  file       = "00_data/04_patents/mainclass_current.tsv", 
  delim      = "\t", 
  col_names  = names(col_types),
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
setDT(mainclass_current_tbl)

# Change of column names to avoid misalignment

setnames(patent_assignee_tbl, "assignee_id", "id")
setnames(patent_tbl, "id", "patent_id")
setnames(mainclass_current_tbl, "id", "mainclass_id")

# Joining tables -----
# Combine assignee_tbl with patent_assignee_tbl
combined_patent_patent_assignee_tbl <- merge(x = assignee_tbl, y = patent_assignee_tbl, 
                       by    = "id", 
                       all.x = TRUE, 
                       all.y = FALSE)
setDT(combined_patent_patent_assignee_tbl)
rm(assignee_tbl, patent_assignee_tbl)
gc()

# Combine result with patent_tbl
combined_patent_patent_assignee_patent_tbl <- merge(x = combined_patent_patent_assignee_tbl, y = patent_tbl, 
                                             by    = "patent_id", 
                                             all.x = TRUE, 
                                             all.y = FALSE)
setDT(combined_patent_patent_assignee_patent_tbl)
rm(combined_patent_patent_assignee_tbl, patent_tbl)
gc()

# Combine result with uspc_tbl
combined_patent_patent_assignee_patent_uspc_tbl <- merge(x = combined_patent_patent_assignee_patent_tbl, y = uspc_tbl, 
                                                    by    = "patent_id", 
                                                    all.x = TRUE, 
                                                    all.y = FALSE)
setDT(combined_patent_patent_assignee_patent_uspc_tbl)
rm(combined_patent_patent_assignee_patent_tbl, uspc_tbl)
gc()

# Combine result with mainclass_current_tbl
combined_patent_data_tbl <- merge(x = combined_patent_patent_assignee_patent_uspc_tbl, y = mainclass_current_tbl, 
                                                         by    = "mainclass_id", 
                                                         all.x = TRUE, 
                                                         all.y = FALSE)
setDT(combined_patent_data_tbl)
rm(combined_patent_patent_assignee_patent_uspc_tbl, mainclass_current_tbl)
gc()

# Preparing the data table  -----

setkey(combined_patent_data_tbl, "organization", "date", "mainclass_id")
key(combined_patent_data_tbl)

?setorder()
setorderv(combined_patent_data_tbl, c("organization", "date", "mainclass_id"))

# 1 - Top 10 US organizations with the most patents -----
combined_patent_data_tbl %>%
  group_by(id)%>%
  filter(!is.na(organization))%>%
  count(organization, sort = TRUE)%>%
  head(n = 10)

# 2 - Top 10 US organizations with the most patents in 2019 ----
combined_patent_data_tbl %>%
  group_by(id)%>%
  filter(!is.na(organization))%>%
  filter(year(date) == 2019)%>%
  count(organization, sort = TRUE)%>%
  head(n = 10)

# 3 - Top 5 USPTO Classes of top 10 organizations ----
combined_patent_data_tbl %>%
  select(id, organization, mainclass_id, title)%>%
  group_by(id)%>%
  filter(!is.na(organization))%>%
  filter(!is.na(mainclass_id))%>%
  filter(title != "DOES NOT EXIST") %>%
  filter(title != "unclassified") %>%
  count(organization, title, sort = TRUE)%>%
  head(n = 10)

