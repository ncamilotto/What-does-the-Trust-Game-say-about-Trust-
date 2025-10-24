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
  
  compte_window <- donnees %>%
    group_by(Year_window, Type_of_experiment) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(Type_of_experiment = factor(Type_of_experiment, levels = ordre_categories))
  
  return(compte_window)
}

# Social Preferences
fichier_socialpreferences <- "C:/Users/ncamilotto/What-does-the-Trust-Game-say-about-Trust-/cluster social preferences.xlsx"

# Social Correlates
fichier_socialcorrelates <- "C:/Users/ncamilotto/What-does-the-Trust-Game-say-about-Trust-/cluster social correlates type of experiment.xlsx"

# Ordre unifié pour les deux graphiques (celui de Social Correlates)
ordre_unifie <- c(
  "Irrelevant",
  "Unknown",
  "Framed Field Experiment",
  "Artefactual Field Experiment",
  "Conventional Lab Experiment"
)

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
    panel.border = element_rect(fill = NA, color = "grey80"),
    legend.position = "bottom"
  )

# Graphique Social Preferences
p1 <- ggplot(compte_socialpreferences, 
       aes(x = Year_window, y = n, fill = Type_of_experiment)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(
    values = couleurs_fixes, 
    name = "Type of Experiment",
    drop = FALSE  # Garde toutes les catégories dans la légende
  ) +
  labs(
    title = "Social Preferences",
    x = "Year Window",
    y = "Number of Experiments"
  ) +
  theme_custom

# Graphique Social Correlates
p2 <- ggplot(compte_socialcorrelates, 
       aes(x = Year_window, y = n, fill = Type_of_experiment)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(
    values = couleurs_fixes, 
    name = "Type of Experiment",
    drop = FALSE
  ) +
  labs(
    title = "Social Correlates",
    x = "Year Window",
    y = "Number of Experiments"
  ) +
  theme_custom

# Combiner avec une légende commune en bas
p1 + p2 + 
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Evolution of Experiment Types by 5-Year Windows",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
    )
  ) &
  theme(legend.position = "bottom")