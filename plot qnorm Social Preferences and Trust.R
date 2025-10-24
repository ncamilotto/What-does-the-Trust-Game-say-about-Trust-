#plot relation inter extra cluster

library(dplyr)
library(tidyr)
library(purrr)
library(combinat)
library(ggplot2)
library(dplyr)

# Fonction pour calculer la matrice cluster × cluster pour une fenêtre donnée
compute_cluster_matrix <- function(win, alluv_dt, Edges) {
  
  # 1. Filtrer les articles de la fenêtre
  network <- alluv_dt %>%
    filter(window == win) %>%
    select(intertemporal_name, ID_Art, window)
  
  # 2. Créer les relations pour cette fenêtre
  df_win <- Edges %>%
    rename(ID_Art = Source) %>%
    merge(network, by = "ID_Art") %>%
    rename(Source = ID_Art, cluster_source = intertemporal_name) %>%
    rename(ID_Art = Target) %>%
    merge(network, by = "ID_Art") %>%
    rename(Target = ID_Art, cluster_target = intertemporal_name) %>%
    select(-c(window.y)) %>%
    rename(window = window.x) %>%
    mutate(
      cluster_source = as.character(cluster_source),
      cluster_target = as.character(cluster_target)
    )
  
  # 3. Créer un df unique d'articles et clusters
  articles_df <- df_win %>%
    select(article = Source, cluster = cluster_source) %>%
    bind_rows(df_win %>% select(article = Target, cluster = cluster_target)) %>%
    distinct()
  
  # 4. Créer toutes les paires possibles d'articles
  if(nrow(articles_df) < 2) {
    return(NULL)  # Pas assez d'articles pour former une paire
  }
  
  all_article_pairs <- t(combn(articles_df$article, 2)) %>%
    as.data.frame(stringsAsFactors = FALSE)
  colnames(all_article_pairs) <- c("Source", "Target")
  
  # 5. Ajouter les clusters
  df_complete <- all_article_pairs %>%
    left_join(articles_df %>% rename(cluster_source = cluster), by = c("Source" = "article")) %>%
    left_join(articles_df %>% rename(cluster_target = cluster), by = c("Target" = "article"))
  
  # 6. Ajouter les poids existants (0 si pas de lien)
  df_complete <- df_complete %>%
    left_join(df_win %>% select(Source, Target, weight), by = c("Source", "Target")) %>%
    mutate(weight = ifelse(is.na(weight), 0, weight))
  
  # 7. Calculer la matrice cluster × cluster
  mat_cluster <- df_complete %>%
    group_by(cluster_source, cluster_target) %>%
    summarise(mean_weight = mean(weight), .groups = "drop") %>%
    pivot_wider(names_from = cluster_target, values_from = mean_weight, values_fill = 0) %>%
    as.data.frame()
  
  rownames(mat_cluster) <- mat_cluster$cluster_source
  mat_cluster$cluster_source <- NULL
  
  return(mat_cluster)
}

# Appliquer sur toutes les fenêtres
windows <- sort(unique(alluv_dt$window))
cluster_matrices <- lapply(windows, compute_cluster_matrix, alluv_dt = alluv_dt, Edges = Edges)
names(cluster_matrices) <- windows


##########################################################################
##########################################################################
##########################################################################


focus_cluster <- "Social Preferences and Trust"

results <- data.frame(window=character(),
                      cluster_target=character(),
                      weight_mean=numeric(),
                      q_norm=numeric(),
                      stringsAsFactors=FALSE)

for(win in names(cluster_matrices)) {
  mat <- cluster_matrices[[win]]
  
  # vérifier que le cluster focus existe
  if(!(focus_cluster %in% rownames(mat))) next
  
  # extraire la ligne et forcer vecteur sans nom
  source_weights <- as.numeric(mat[focus_cluster, ])
  targets <- colnames(mat)
  
  # totaux
  total_source <- sum(source_weights)
  total_target <- colSums(mat)
  
  # normalisation bilatérale
  q_norm <- source_weights / sqrt(total_source * total_target)
  
  # assembler dans un data.frame
  temp <- data.frame(
    window = rep(win, length(targets)),
    cluster_target = targets,
    weight_mean = source_weights,
    q_norm = q_norm,
    stringsAsFactors = FALSE
  )
  
  results <- rbind(results, temp)
}

results

results <- results %>%
  filter(cluster_target %in% c(
    "Neurocognitive Foundations of Trust",
    "Social Preferences and Trust",
    "Structural Foundations of Trust",
    "Social Correlates of Trust",
    "Social Psychology of Trust"
  ))

results <- results %>%
  complete(cluster_target, window, fill = list(q_norm = NA)) %>%
  arrange(cluster_target, window)


#############Création du plot################

library(ggplot2)
library(dplyr)

# Supposons que votre dataframe s'appelle 'results' et est déjà chargé
# Si ce n'est pas le cas, vous pouvez l'importer avec read.csv() ou autre

# Préparation des données pour le plot
# Conversion de la colonne window en facteur ordonné pour l'axe x
# Conversion de la colonne window en facteur ordonné pour l'axe x
results$window <- factor(results$window, 
                        levels = unique(results$window),
                        ordered = TRUE)

# Préparation d'une variable numérique pour la régression
results$window_num <- as.numeric(results$window)

# Création du plot d'évolution de q_norm
p <- ggplot(data = results, 
            aes(x = window, y = q_norm, color = cluster_target, group = cluster_target)) +
  
  # Points d'abord
  geom_point(size = 2, alpha = 0.9) +
  
  # Lignes continues (ggplot cassera automatiquement les lignes aux gaps)
  geom_line(linewidth = 1.2, alpha = 0.8) +
  
  # Pour forcer des lignes discontinues sur les gaps, utilisez cette alternative :
  # geom_path(aes(group = cluster_target), linewidth = 1.2, alpha = 0.8, 
  #           linetype = "solid", na.rm = FALSE) +
  
  # Ajout des lignes de régression
  geom_smooth(aes(x = window_num, y = q_norm), 
              method = "lm", 
              se = FALSE, 
              linetype = "dashed", 
              linewidth = 0.8, 
              alpha = 0.7) +
  
  # Application des couleurs personnalisées
  scale_color_manual(values = my_colors) +
  
  # Personnalisation du thème
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 9),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  ) +
  
  labs(
    title = "Evolution of q_norm by Period",
    subtitle = "Focus on Social Preferences and Trust Cluster",
    x = "Period (time window)",
    y = "q_norm",
    color = "Cluster"
  ) +
  
  # Amélioration des guides de légende
  guides(color = guide_legend(override.aes = list(size = 3),
                             ncol = 2))

# Affichage du plot
print(p)

ggsave("evolution_qnorm_600px.png", plot = p, width = 12, height = 8, dpi = 600)
