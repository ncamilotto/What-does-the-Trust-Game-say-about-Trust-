library(dplyr)
library(tidyr)
library(stringr)
library(biblionetwork)
library(networkflow)
library(forcats)
library(ggplot2)
library(ggalluvial)


Edges <- biblio_coupling(Edges, "Noeud", "ID_citation", weight_threshold = 1) #Use of the biblio_coupling function to obtain the edges file
Edges <- Edges %>%
				subset(select = c('Source','Target','weight')) 

#In order to keep in the node file only those items that have a link with another article in the network we produce a List of nodes

List_of_nodes <- Edges %>%
  select(Source, Target) %>%
  pivot_longer(cols = everything(), values_to = "ID") %>%
  distinct(ID)

##################################

extract_surname <- function(author) {
  if (is.na(author)) return(NA_character_)
  
  parts <- unlist(strsplit(gsub("\\s+", " ", trimws(author)), " "))
  
  if (length(parts) < 2) return(parts[1])
  
  first <- parts[1]
  
  prefixes <- c("De", "Da", "Dal", "Di", "Dos", "Del", "Della", "Delle", "Van", "Van't", "Von", "Le", "La", "Du")
  
  if (tolower(first) == "van") {
    if (length(parts) >= 3 && grepl("^(Der|Den|De)$", parts[2], ignore.case = TRUE)) {
      return(paste(parts[1:3], collapse = " "))  # Van Der / Van Den / Van De
    } else {
      return(paste(parts[1:2], collapse = " "))  # Van X
    }
  }
  
  if (tolower(first) %in% tolower(prefixes)) {
    return(paste(parts[1:2], collapse = " "))
  }
  
  return(first)
}

nodes <- nodes %>%
  mutate(Surname = sapply(AU, extract_surname),
         Label   = paste(Surname, PY, sep = ", ")) %>%
  select(Noeud, Label, AU, PY, TI, SO, Z9, SC, WC, C3, AB) %>%
  rename(ID = Noeud) %>%
  filter(ID %in% c(Edges$Source, Edges$Target))

#write_xlsx(nodes, path = "nodes-2015-2024.xlsx")
#write_xlsx(Edges, path = "edges-2015-2024.xlsx")


####################################################################################
#######                               Alluvial                               #######
####################################################################################


nodes <- nodes %>% 
			rename(ID_Art = ID, Author = AU, Year = PY)

references <- Edges_before_coupling %>%
								rename(ID_Art = Noeud, ItemID_Ref = ID_citation)	


temporal_networks <- build_dynamic_networks(nodes = nodes,
directed_edges = references,
source_id = "ID_Art",
target_id = "ItemID_Ref",
time_variable = "Year",
cooccurrence_method = "coupling_similarity",
time_window = 10,
edges_threshold = 3,
overlapping_window = TRUE,
filter_components = TRUE)

temporal_networks_cluster <- add_clusters(temporal_networks,
objective_function = "modularity",
clustering_method = "leiden",
n_iterations = 300,
seed = 1111)

temporal_networks_cluster_intertemporal <- merge_dynamic_clusters(temporal_networks_cluster,
cluster_id = "cluster_leiden",
node_id = "ID_Art",
threshold_similarity = 0.51,
similarity_type = "partial")

temporal_networks[[1]]

alluv_dt <- networks_to_alluv(
  graphs = temporal_networks_cluster_intertemporal,
  intertemporal_cluster_column = "dynamic_cluster_leiden",
  node_id = "ID_Art",
  summary_cluster_stats = FALSE
)

alluv_dt <- rename(alluv_dt, intertemporal_name = dynamic_cluster_leiden)

alluv_dt <- alluv_dt %>%
  mutate(intertemporal_name = recode(intertemporal_name,
                                      "cl_48" = "cl_4",
                                      "cl_55" = "cl_5",
                                      "cl_31" = "cl_4",
                                      "cl_32" = "cl_5",
                                      "cl_104" = "cl_5",
                                      "cl_110" = "cl_94"))

alluv_dt <- minimize_crossing_alluvial(
  alluv_dt,
  intertemporal_cluster_column = "intertemporal_name",
  node_id = "ID_Art"
)

alluv_dt <- alluv_dt %>%
  group_by(window, intertemporal_name) %>%
  mutate(stratum_size = sum(y_alluv)) %>%
  ungroup()

alluv_dt <- alluv_dt %>%
  mutate(intertemporal_name = case_when(

    intertemporal_name == "cl_4" & window %in% c("2005-2014", "2004-2013", "2003-2012") ~
      "Socio-demographic Determinants",
    intertemporal_name == "cl_4" ~
      "Social Correlates of Trust",
    
    # Les autres recodages fixes
    intertemporal_name == "cl_6" ~ "Neurocognitive Foundations of Trust",
    intertemporal_name == "cl_30" ~ "Measurement and Validity",
    intertemporal_name == "cl_31" ~ "Socio-demographic Determinants",
    intertemporal_name == "cl_2"  ~ "Social Preferences and Trust",
    intertemporal_name == "cl_14" ~ "Structural Foundations of Trust",
    intertemporal_name %in% c("cl_55", "cl_5", "cl_32", "cl_104") ~ "Social Psychology of Trust",
    intertemporal_name == "cl_101" ~ "Online Social Psychology of Trust",
    intertemporal_name == "cl_125" ~ "Complex Systems", #Confiance et Systèmes Complexes
    intertemporal_name == "cl_111" ~ "Trust at First Sight",
    intertemporal_name == "cl_138" ~ "Actuarial Game Theory",
    intertemporal_name == "cl_94" ~ "The Psychology of Promises",
    intertemporal_name == "cl_17" ~ "Social Thermometer",
    
    # par défaut : on garde la valeur initiale
    TRUE ~ intertemporal_name
  ))


###################LABEL##############################

# Associez chaque nom de cluster (après renommage) à un code de couleur hexadécimal ou un nom de couleur R
my_colors <- c(
  "Neurocognitive Foundations of Trust" = "#1f77b4",
  "Trust at First Sight" = "#51b0f5ff",
  "Social Preferences and Trust" = "#9467bd",
  "The Psychology of Promises" = "#c6b5d6ff",
  "Structural Foundations of Trust" = "#2ca02c",
  "Socio-demographic Determinants" = "#d62728",
  "Measurement and Validity" = "#e1e103ff",
  "Social Correlates of Trust" = "#ff7f0e" ,
  "Social Psychology of Trust" = "#8c564b",
  "Online Social Psychology of Trust" = "#e377c2",
  "Complex Systems" = "#d59b12ff", #Confiance et Systèmes Complexes
  "Actuarial Game Theory" = "#045906ff",
  "Social Thermometer" = "#ffa5a5ff"
  # Ajoutez d'autres clusters et couleurs si nécessaire
)

# Configuration des labels par fenêtre temporelle
# Modifiez cette liste selon vos besoins
label_config <- list(
  "Neurocognitive Foundations of Trust" = c("2008-2017"),
  "Social Correlates of Trust" = c("2010-2019"),
  "Social Preferences and Trust" = c("2009-2018"),
  "Structural Foundations of Trust" = c("2007-2016"),
  "Socio-demographic Determinants" = c("2004-2013", "2011-2020"),
  "Measurement and Validity" = c("2004-2013"),
  "Social Psychology of Trust" = c("2009-2018"),
  "Online Social Psychology of Trust" = c("2013-2022"),
  "Complex Systems" = c("2014-2023"),
  "Social Thermometer" = c("2003-2012"),
  #"Actuarial Game Theory" = c("2015-2024"),
  "Trust at First Sight" = c("2013-2022"),
  "The Psychology of Promises" = c("2013-2022")
)


# 1. Calculer le total de y_alluv pour chaque fenêtre afin de déterminer les proportions
alluv_dt <- alluv_dt %>%
  group_by(window) %>%
  mutate(window_total_size = sum(y_alluv)) %>%
  ungroup()


# 2. Calculer la proportion de chaque strate (cluster) dans sa fenêtre
alluv_dt <- alluv_dt %>%
  mutate(proportion = stratum_size / window_total_size)

# 3. Créer la colonne des étiquettes conditionnelles en ajoutant la règle des 5%
alluv_dt <- alluv_dt %>%
  mutate(cluster_label = case_when(
    # Vos conditions personnalisées existantes (elles sont prioritaires)
    intertemporal_name == "Neurocognitive Foundations of Trust" & window %in% label_config[["Neurocognitive Foundations of Trust"]] ~ "[Psychology] Neurocognitive Foundations of Trust",
    intertemporal_name == "Social Correlates of Trust" & window %in% label_config[["Social Correlates of Trust"]] ~ "[Economics] Social Correlates of Trust",
    intertemporal_name == "Social Preferences and Trust" & window %in% label_config[["Social Preferences and Trust"]] ~ "[Economics] Social Preferences and Trust",
    intertemporal_name == "Structural Foundations of Trust" & window %in% label_config[["Structural Foundations of Trust"]] ~ "[Economics] Structural Foundations of Trust",
    intertemporal_name == "Socio-demographic Determinants" & window %in% label_config[["Socio-demographic Determinants"]] ~ as.character(intertemporal_name),
    intertemporal_name == "Measurement and Validity" & window %in% label_config[["Measurement and Validity"]] ~ as.character(intertemporal_name),
    intertemporal_name == "Social Psychology of Trust" & window %in% label_config[["Social Psychology of Trust"]] ~ "[Psychology] Social Psychology of Trust",
    intertemporal_name == "Online Social Psychology of Trust" & window %in% label_config[["Online Social Psychology of Trust"]] ~ as.character(intertemporal_name),
    intertemporal_name == "Social Thermometer" & window %in% label_config[["Social Thermometer"]] ~ as.character(intertemporal_name),
    intertemporal_name == "Trust at First Sight" & window %in% label_config[["Trust at First Sight"]] ~ as.character(intertemporal_name),
    intertemporal_name == "The Psychology of Promises" & window %in% label_config[["The Psychology of Promises"]] ~ as.character(intertemporal_name),
    intertemporal_name == "Complex Systems" & window %in% label_config[["Complex Systems"]] ~ as.character(intertemporal_name),
    
    #Affiche l'étiquette si le cluster représente plus de x% de la fenêtre
    proportion > 0.9 ~ as.character(intertemporal_name),
    
    # Par défaut, aucune étiquette
    TRUE ~ NA_character_
  ))


#alluv_dt[,y_alluv:=1/.N, window]

alluv_dt <- alluv_dt %>%
  mutate(intertemporal_name = fct_relevel(intertemporal_name,
                                      "Social Correlates of Trust",
                                      "Measurement and Validity",
                                      "Socio-demographic Determinants",
                                      "Social Thermometer",
                                      "The Psychology of Promises",
                                      "Social Preferences and Trust",
                                      "Structural Foundations of Trust",
                                      "Complex Systems",
                                      "cl_1",
                                      "Social Psychology of Trust",
                                      "Online Social Psychology of Trust",
                                      "Neurocognitive Foundations of Trust",
                                      "Trust at First Sight"
                                      ))
                                      
alluv_dt <- alluv_dt %>%
  filter(window != "1998-2007")

alluv_dt <- alluv_dt %>%
  mutate(window = gsub("-", "\n-", window))  # remplace "-" par "\n-"


#########################################################################################################

ggplot(alluv_dt, aes(x = window, y= y_alluv, stratum = intertemporal_name, alluvium = ID_Art, fill = intertemporal_name)) +
  geom_stratum(alpha = 1, size = 1/12) +
  geom_flow() +
  
  # Remplacement de geom_text par geom_label pour créer un cadre stylisé
  geom_label(
    stat = "stratum", 
    aes(label = cluster_label), 
    size = 2.5,
    color = "white",
    colour = "black",
    label.size = 0.5,
    fontface = "bold"
  ) +
  
  scale_fill_manual(values = my_colors) +
  
  theme_minimal() +
  theme(plot.background = element_rect(fill = NA, colour = NA)) +
  ggtitle("") +
  theme(legend.position = "none") +
  
  # Ajout de cette ligne pour modifier le nom de l'axe x
  labs(x = "Time Window") +
  labs(y = "Relative size of the cluster")

#ggsave("alluvial_plot_transparent_best_quality.png", plot = last_plot(), width = 12, height = 6, dpi = 900, limitsize = FALSE)

#########Appendix###################

#alluv_dt <- alluv_dt %>%
#  mutate(window = gsub("-", "\n-", window))  # remplace "-" par "\n-"
#
#ggplot(alluv_dt, 
#       aes(x = window, y = y_alluv, stratum = intertemporal_name, alluvium = ID_Art, fill = intertemporal_name)) +
#  
#  geom_stratum(alpha = 1, size = 1/12) +
#  geom_flow() +
#  scale_fill_manual(values = my_colors) +
#  theme_minimal() +
#  theme(plot.background = element_rect(fill = NA, colour = NA)) +
#  theme(legend.position = "none") +
#  labs(x = "Time Window", y = "Relative size of the cluster")

#ggsave("alluvial_plot_transparent_appendix_with_links.png", plot = last_plot(), width = 12, height = 6, dpi = 600, limitsize = FALSE)


