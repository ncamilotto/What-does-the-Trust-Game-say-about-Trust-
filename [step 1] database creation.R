library(bibliometrix)
library(tibble)
library(dplyr)

Sys.getenv("MY_FOLDER_PATH")

folder_path <- Sys.getenv("MY_FOLDER_PATH")

files <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE)

# Appliquer convert2df sur chaque fichier, puis fusionner en un seul dataframe

database_WOS <- files %>%
  lapply(function(f) convert2df(f, dbsource = "wos", format = "plaintext")) %>%
  bind_rows() %>%
  rownames_to_column(var = "REF") %>%
  select(REF, AU, CR, DT, PY, SC, PT, SO, TI, SR, Z9, C3, WC, AB) %>%
  mutate(PY = as.integer(PY))

save(database_WOS, file = "database_WOS.RData")