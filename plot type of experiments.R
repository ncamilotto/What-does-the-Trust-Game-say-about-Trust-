# Charger les bibliothèques nécessaires
library(readxl)
library(ggplot2)
library(stringr)
library(dplyr)
library(patchwork)

# Fonction pour traiter les données
traiter_donnees <- function(fichier, ordre_categories) {
  donnees <- read_excel(fichier)
  
  donnees <- donnees %>%
    mutate(
      Year = as.integer(Year),
      Type_of_experiment = str_to_title(Type_of_experiment),
      Type_of_experiment = recode(
        Type_of_experiment,
        "Lab-In-The-Field" = "Framed Field Experiment"
      ),
      Year_window = paste0(floor(Year / 5) * 5, "-", floor(Year / 5) * 5 + 4)
    )
  
  # Transformer en facteur avec tous les niveaux, même ceux non présents
  donnees$Type_of_experiment <- factor(donnees$Type_of_experiment, levels = ordre_categories)
  
  compte_window <- donnees %>%
    group_by(Year_window, Type_of_experiment) %>%
    summarise(n = n(), .groups = "drop") %>%
    # Ajouter les combinaisons manquantes pour avoir toutes les catégories
    complete(Year_window, Type_of_experiment, fill = list(n = 0))
  
  return(compte_window)
}

# Définir les chemins de fichiers
fichier_socialpreferences <- "C:/Users/ncamilotto/What-does-the-Trust-Game-say-about-Trust-/cluster social preferences.xlsx"
fichier_socialcorrelates <- "C:/Users/ncamilotto/What-does-the-Trust-Game-say-about-Trust-/cluster social correlates type of experiment.xlsx"

# Ordre unifié des types d'expérience
ordre_unifie <- c(
  "Irrelevant",
  "Unknown",
  "Framed Field Experiment",
  "Artefactual Field Experiment",
  "Conventional Lab Experiment"
)

# Traitement des données
compte_socialpreferences <- traiter_donnees(fichier_socialpreferences, ordre_unifie)
compte_socialcorrelates <- traiter_donnees(fichier_socialcorrelates, ordre_unifie)

# Couleurs fixes
couleurs_fixes <- c(
  "Artefactual Field Experiment" = "#e1e103ff",
  "Framed Field Experiment" = "#ff7f00",
  "Irrelevant" = "#e8eae8",
  "Unknown" = "#CECECE",
  "Conventional Lab Experiment" = "#2ca02c"
)

# Thème personnalisé
theme_custom <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.title = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "grey80")
  )

# 1. Créer un objet pour l'échelle de couleurs pour garantir la cohérence
scale_fill_custom <- scale_fill_manual(
  name = "Type of Experiment",
  values = couleurs_fixes,
  drop = FALSE # Important pour que tous les niveaux soient dans la légende
)

# 2. Graphique Social Preferences (p1) - On garde sa légende
p1 <- ggplot(compte_socialpreferences, 
             aes(x = Year_window, y = n, fill = Type_of_experiment)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(
    title = "Social Preferences and Trust",
    y = "Number of Experiments",
    x = NULL
  ) +
  theme_custom +
  scale_fill_custom # On applique notre échelle de couleurs

# 3. Graphique Social Correlates (p2) - On supprime sa légende
p2 <- ggplot(compte_socialcorrelates, 
             aes(x = Year_window, y = n, fill = Type_of_experiment)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(
    title = "Social Correlates of Trust",
    y = "Number of Experiments",
    x = NULL
  ) +
  theme_custom +
  scale_fill_custom + # On applique LA MÊME échelle
  theme(legend.position = "none") # <-- La ligne clé pour supprimer la 2ème légende

# 4. Combinaison finale
# patchwork va prendre la seule légende disponible (celle de p1) et la gérer
combined_plot <- (p1 + p2) +
  plot_layout(guides = "collect") +
  plot_annotation(
    theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")

# Affichage
combined_plot

ggsave("evolution_experiment_types.png", plot = combined_plot, width = 12, height = 8, dpi = 600)