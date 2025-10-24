#step 4 creation df for shiny

# Charger le package
library(readxl)
library(dplyr)

# Définir les chemins complets
fichier1 <- "C:/Users/ncamilotto/What-does-the-Trust-Game-say-about-Trust-/cluster social correlates type of experiment.xlsx"
fichier2 <- "C:/Users/ncamilotto/What-does-the-Trust-Game-say-about-Trust-/cluster social preferences.xlsx"

# Lire les fichiers
df_correlates <- read_excel(fichier1)
df_preferences <- read_excel(fichier2)

# Garder uniquement les colonnes désirées
df_correlates <- df_correlates %>%
  select(Author, Title, Type_of_experiment)

df_preferences <- df_preferences %>%
  select(Author, Title, Type_of_experiment)

df_combined <- bind_rows(df_correlates, df_preferences) %>%
  distinct() %>%
  rename(TI = Title, "Experiment Type" = Type_of_experiment)
  


load("university_location.RData")


university_location_en <- university_location %>%
  select(-c(Pays, Ville))

references_collapse <- references %>%
  group_by(ID_Art) %>%
  summarise(ItemID_Ref = paste(ItemID_Ref, collapse = ", "))

alluv_content_shiny <- alluv_dt %>%
  left_join(nodes, by = "ID_Art") %>%
  left_join(references_collapse, by = "ID_Art") %>%
  rename(University = C3) %>%
  select(intertemporal_name, Author, ID_Art, Label, Year, TI, SO, University, Z9, SC, WC, ItemID_Ref) %>%
  mutate(
    Author = str_to_title(str_to_lower(Author)),
    Author = str_replace_all(Author, ";", ", "),  # remplace ; par , + espace
    Author = str_replace_all(Author, ",\\s*", ", "), # uniformise les espaces après ,
    TI = str_to_title(str_to_lower(TI)),
    SO = str_to_title(str_to_lower(SO)),
    WC = str_to_title(str_to_lower(WC))
  )%>%
  distinct() %>%
  left_join(df_combined, by = c("Author", "TI")) %>%
  distinct()




ref_for_alluv <- ID_AYR %>%
  rename(ItemID_Ref = ID_AYR) %>%
  select(-n_AYR)



