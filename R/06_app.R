# ===================================================================
# 1. CHARGER LES BIBLIOTHÈQUES NÉCESSAIRES
# ===================================================================
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(bslib)
library(stringr)

# ===================================================================
# 2. PRÉPARER LES DONNÉES
# ===================================================================

# Charger votre dataframe ici
articles_df <- alluv_content_shiny
ref_for_alluv <- ref_for_alluv
references_collapse <- references_collapse
university_location_en <- university_location_en

# ===================================================================
# 2. PRÉPARER LES DONNÉES POUR LES CONTRÔLES
# ===================================================================

# Créer un vecteur nommé : noms affichés = valeurs "propres", valeurs réelles = valeurs complètes
cluster_full_names <- c(
  "Social Correlates of Trust",
  "Social Preferences and Trust",
  "Structural Foundations of Trust",
  "Social Psychology of Trust",
  "Neurocognitive Foundations of Trust",
  "Complex Systems",
  "Measurement and Validity",
  "Online Social Psychology of Trust",
  "Socio-demographic Determinants",
  "Social Thermometer",
  "The Psychology of Promises",
  "Trust at First Sight"
)

# Extraire le nom "propre" (tout après les crochets)
cluster_display_names <- sub("^\\[.*?\\]\\s*", "", cluster_full_names)

# Créer le vecteur nommé pour le selectInput
cluster_choices <- setNames(cluster_full_names, cluster_display_names)

# ===================================================================
# 3. DÉFINIR L'INTERFACE UTILISATEUR (UI)
# ===================================================================
ui <- navbarPage(
"Online Appendix",
theme = bs_theme(bootswatch = "lumen"),

tabPanel("Home", 
    fluidPage(
        # Ajout d'une feuille de style pour cette page si besoin
        tags$head(
          tags$style(HTML("
            .home-section { 
              padding-top: 20px; 
              padding-bottom: 20px; 
              border-bottom: 1px solid #eee;
            }
            .home-section h2 { 
              color: #0056b3; 
              border-bottom: 2px solid #0056b3; 
              padding-bottom: 10px;
              margin-bottom: 20px;
            }
            .citation-box {
              background-color: #f8f9fa;
              border-left: 5px solid #007bff;
              padding: 15px;
              margin-top: 15px;
              font-style: italic;
            }
          "))
        ),

        # Titre principal de la page
        titlePanel("An Interactive Appendix for 'The Trust Game: A Historical and Methodological Analysis on the Frontier of Experimental and Behavioral Economics'"),

        # --- Section 1: About the Article ---
        div(class = "home-section",
            h2("About the Article"),
            p(strong("Authors:"), "Author 1, Author 2, and Author 3."),
            p(strong("Abstract:")),
            p("This paper provides a life-cycle analysis of the Trust Game, using its trajectory as a lens to clarify the boundaries between experimental and behavioral economics. We first trace its 1995 creation by Berg et al. as a challenge to calculative trust paradigms. A bibliometric study then maps its diffusion, revealing two divergent paths in economics: one, rooted in experimental economics, prioritizes measurement; the other, in behavioral economics, theory-testing. These paths differ in methods and validity standards, constituting an epistemic divide that illuminates the fields' evolving relationship."),
            
            div(class = "citation-box",
                p(strong("How to cite this work:")),
                p("Author, A., Author, B., & Author, C. (Year). Title of the article. ", 
                  tags$em("Journal Name, Volume"), "(Issue), pages."),
                p(strong("DOI: "), 
                  tags$a(href = "https://doi.org/YOUR_DOI_HERE", "https://doi.org/YOUR_DOI_HERE", target = "_blank"))
            )
        ),
        
        # --- Section 2: How to Use This Appendix ---
        div(class = "home-section",
            h2("How to Use This Interactive Appendix"),
            p("This web application is designed to provide an interactive exploration of the research clusters discussed in our paper. 
              It allows you to dynamically filter and view the data behind the figures."),
            p("Navigate to the ", strong("Alluvial Explorer"), " tab to begin. You will find:"),
            tags$ul(
                tags$li(strong("Overall View:"), " Provides a global perspective on the entire corpus of articles, 
                        showing distributions by year, discipline, country, and journal."),
                tags$li(strong("Cluster View:"), " Allows you to select a specific research cluster from the dropdown menu. 
                        All plots and tables will then dynamically update to reflect only the articles belonging to that cluster. 
                        This is useful for in-depth analysis of a particular research stream.")
            )
        ),

        # --- Section 3: Transparency and Resources ---
        div(class = "home-section",
            h2("Code, Data, and Contact"),
            p("We believe in open and reproducible science. All resources related to this project are publicly available."),
            tags$ul(
                tags$li(
                  icon("github"), " ",
                  strong("Shiny App Code:"), " The source code for this interactive application is available on ",
                  tags$a(href = "https://github.com/YOUR_USERNAME/YOUR_APP_REPO", "GitHub.", target = "_blank")
                ),
                tags$li(
                  icon("github"), " ",
                  strong("Data Processing Code:"), " The scripts used for data collection and preparation are available in a separate ",
                  tags$a(href = "https://github.com/YOUR_USERNAME/YOUR_DATA_REPO", "repository on GitHub.", target = "_blank")
                ),
                tags$li(
                  icon("database"), " ",
                  strong("Data Source:"), " The bibliographic data was extracted from the Web of Science Core Collection on [Date]. 
                  A processed version of the data used in this app is available at [Link to Zenodo/OSF/Data Repository if applicable]."
                ),
                tags$li(
                  icon("envelope"), " ",
                  strong("Contact:"), " For any questions, suggestions, or issues, please contact [Your Name] at ",
                  tags$a(href = "mailto:your.email@university.edu", "your.email@university.edu.")
                )
            )
        )
    )
),

tabPanel("Alluvial Explorer",
        fluidPage(
          titlePanel("Evolution of Research Clusters"),
          
          # SIDEBAR + ALLUVIAL
          fluidRow(
            column(3,
                   div(class = "sidebar-panel",
                       wellPanel(
                         h4("Explore Data by:"),
                         radioButtons(
                           inputId = "mode_select",
                           label = "Choose exploration mode:",
                           choices = c("Overall View" = "overall", "Cluster" = "cluster"),
                           selected = "overall"
                         ),
                         conditionalPanel(
                           condition = "input.mode_select == 'cluster'",
                           selectInput(
                             inputId = "cluster_select",
                             label = "Select a cluster:",
                             choices = c("Select a cluster..." = "", cluster_choices)
                           )
                         ),
                         hr(),
                         htmlOutput("selection_info")
                       )
                   )
            ),
            
            column(9,
                   div(class = "main-panel-custom",
                       div(class = "alluvial-container",
                           div(
                             imageOutput("alluvial_image", width = "100%", height = "auto"),
                             style = "text-align: center;"
                           )
                       )
                   )
            )
          ),
          
          # CONTENU PLEINE LARGEUR
          div(class = "full-width-section",
              
              conditionalPanel(
                condition = "(input.mode_select == 'cluster' && input.cluster_select != '') || input.mode_select == 'overall'",
                div(class = "section-title", h4("Article Distribution per Year")),
                plotOutput("year_distribution_plot", height = "400px"),
                br(), hr()
              ),
              
              conditionalPanel(
                condition = "(input.mode_select == 'cluster' && input.cluster_select != '') || input.mode_select == 'overall'",
                fluidRow(
                  column(4,
                         div(class = "section-title",
                             div(style = "display: inline-block;",
                                 h4(style = "display: inline; margin-right: 5px;", "Most Frequent Discipline Tags"),
                                 tooltip(
                                   trigger = icon("info-circle", class = "info-icon"),
                                   HTML("Since articles can have multiple tags, percentages may sum to more than 100%.<br>
                                        Reading example: 46% of articles in the selection contain the tag 'Economics'."),
                                   placement = "top"
                                 )
                             )
                         ),
                         plotOutput("discipline_plot", height = "400px")
                  ),
                  column(4,
                         div(class = "section-title",
                             div(style = "display: inline-block;",
                                 h4(style = "display: inline; margin-right: 5px;", "Most Represented Countries"),
                                 tooltip(
                                   trigger = icon("info-circle", class = "info-icon"),
                                   "Since an article can have authors from multiple countries, percentages may sum to more than 100%. 
                                    Reading example: 46% of articles in the selection have at least one author affiliated with 'United States'.",
                                   placement = "top"
                                 )
                             )
                         ),
                         plotOutput("country_plot", height = "400px")
                  ),
                  column(4,
                         div(class = "section-title",
                             div(style = "display: inline-block;",
                                 h4(style = "display: inline; margin-right: 5px;", "Most Represented Journals"),
                                 tooltip(
                                   trigger = icon("info-circle", class = "info-icon"),
                                   "This shows the top 10 journals that publish articles in this selection. 
                                    Reading example: 15% of articles in the selection are published in 'Journal of Economic Behavior'.",
                                   placement = "top"
                                 )
                             )
                         ),
                         plotOutput("journal_plot", height = "400px")
                  )
                ),
                br(), hr()
              ),
              
              conditionalPanel(
                condition = "(input.mode_select == 'cluster' && input.cluster_select != '') || input.mode_select == 'overall'",
                fluidRow(
                  column(9,
                         div(class = "section-title",
                             div(style = "display: inline-block;",
                                 h4(style = "display: inline; margin-right: 5px;", textOutput("table_title", inline = TRUE)),
                                 tooltip(
                                   trigger = icon("info-circle", class = "info-icon"),
                                   "All articles in the selection are shown here, sorted by citation count. Use search and filters to explore specific entries.",
                                   placement = "top"
                                 )
                             )
                         ),
                         DT::dataTableOutput("cluster_table")
                  ),
                  column(3,
                         div(class = "section-title",
                             div(style = "display: inline-block;",
                                 h4(style = "display: inline; margin-right: 5px;", "Most Frequent References in Bibliographies"),
                                 tooltip(
                                   trigger = icon("info-circle", class = "info-icon"),
                                   HTML("This table lists the references most frequently cited by the articles in the current selection.<br>
                                        Reading example: a value of '46%' means that 46% of articles in the selection cite the reference."),
                                   placement = "top"
                                 )
                             )
                         ),
                         DT::dataTableOutput("top_references_table")
                  )
                )
              )
          )
        )
)
)

# ===================================================================
# 4. DÉFINIR LA LOGIQUE DU SERVEUR (Code complet avec modification)
# ===================================================================
server <- function(input, output, session) {

  get_selection <- reactive({
    if (input$mode_select == "cluster") {
      return(input$cluster_select)
    } else {
      return("")
    }
  })
  
  active_data <- reactive({
    if (input$mode_select == "overall") {
      return(articles_df)
    }
    if (input$mode_select == "cluster" && input$cluster_select != "") {
      return(articles_df[articles_df$intertemporal_name == input$cluster_select, ])
    }
    return(NULL)
  })
  
  active_title_context <- reactive({
    if (input$mode_select == "overall") {
      return("for the Entire Corpus")
    }
    if (input$mode_select == "cluster" && input$cluster_select != "") {
      clean_name <- trimws(sub("^\\[.*?\\]\\s*", "", input$cluster_select))
      return(paste("for Cluster:", clean_name))
    }
    return("")
  })

  output$alluvial_image <- renderImage({
    selection_prefix <- get_selection()
    image_name <- if (is.null(selection_prefix) || selection_prefix == "") {
      "alluvial_plot.png"
    } else {
      paste0("[", selection_prefix, "] alluvial_plot.png")
    }
    image_path <- file.path("www", image_name)
    if (!file.exists(image_path)) {
      image_path <- "www/alluvial_plot.png"
    }

    list(
      src = image_path,
      contentType = "image/png",
      width = "100%",
      height = "auto",
      class = "alluvial-image"
    )
  }, deleteFile = FALSE)

  output$selection_info <- renderUI({
    if (input$mode_select == "overall") {
      HTML("<h4>Overall View</h4><p>You are viewing statistics for the entire corpus of articles.</p>")
    } else if (input$mode_select == "cluster" && input$cluster_select != "") {
      HTML(paste("<h4>You have selected the cluster:</h4><p style='font-size:1.2em; font-weight:bold; color:#007bff;'>", input$cluster_select, "</p>"))
    } else {
      HTML("<p>Select a cluster to explore the data.</p>")
    }
  })

  # Graphique de distribution par année avec médiane
  output$year_distribution_plot <- renderPlot({
    filtered_data <- active_data()
    req(filtered_data, length(unique(filtered_data$ID_Art)) > 0)

    min_year <- min(filtered_data$Year, na.rm = TRUE)
    max_year <- max(filtered_data$Year, na.rm = TRUE)
    all_years <- data.frame(Year = seq(min_year, max_year))

    year_counts <- filtered_data %>%
      group_by(Year) %>%
      summarise(n = n(), .groups = "drop")
    year_counts <- merge(all_years, year_counts, by = "Year", all.x = TRUE)
    year_counts$n[is.na(year_counts$n)] <- 0

    median_year <- median(filtered_data$Year, na.rm = TRUE)
    median_position <- match(median_year, year_counts$Year)
    total_articles <- length(unique(filtered_data$ID_Art))

    ggplot(year_counts, aes(x = as.factor(Year), y = n)) +
      geom_bar(stat = "identity", fill = "#007bff", alpha = 0.7) +
      geom_text(aes(label = n), vjust = -0.5, size = 4) +
      geom_vline(xintercept = median_position, color = "red", linetype = "dashed", size = 1.2) +
      annotate("text", 
               x = median_position + 0.1, 
               y = max(year_counts$n) * 0.9, 
               label = paste("Median:", median_year), 
               color = "red", fontface = "bold", hjust = 0) +
      labs(
        title = paste("Number of Articles per Year", active_title_context()),
        subtitle = paste("Total articles in selection:", total_articles),
        x = "Year",
        y = "Number of Articles"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0, face = "bold"),
        plot.subtitle = element_text(hjust = 0, face = "italic", color = "darkblue")
      )
  })

  # Graphique des disciplines
  output$discipline_plot <- renderPlot({
    filtered_data <- active_data()
    req(filtered_data, length(unique(filtered_data$ID_Art)) > 0)

    tags_df <- filtered_data %>%
      mutate(WC = as.character(WC)) %>%
      separate_rows(WC, sep = ";") %>%
      mutate(WC = trimws(WC),
             WC = sapply(strsplit(WC, ","), `[`, 1)) %>%
      filter(!is.na(WC) & WC != "")

    tag_counts <- tags_df %>%
      group_by(WC) %>%
      summarise(n = n_distinct(ID_Art), .groups = "drop") %>%
      mutate(percentage = round(n / length(unique(filtered_data$ID_Art)) * 100, 1)) %>%
      arrange(desc(percentage)) %>%
      slice_head(n = 10)

    ggplot(tag_counts, aes(x = reorder(WC, percentage), y = percentage)) +
      geom_bar(stat = "identity", fill = "#28a745", alpha = 0.8) +
      geom_text(aes(label = paste0(percentage, "%")), 
                hjust = -0.1, color = "black", size = 4) +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = paste("Most Frequent Discipline Tags", active_title_context()),
        x = "Discipline Tag",
        y = "Percentage of Articles (%)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0, face = "bold"),
        axis.text = element_text(size = 12)
      )
  })

  # Graphique des pays
  output$country_plot <- renderPlot({
    filtered_data <- active_data()
    req(filtered_data, length(unique(filtered_data$ID_Art)) > 0)

    country_data <- filtered_data %>%
      mutate(University = as.character(University)) %>%
      separate_rows(University, sep = ";") %>%
      mutate(University = trimws(University)) %>%
      left_join(university_location_en, by = "University") %>%
      filter(!is.na(Country)) %>%
      distinct(ID_Art, Country) %>%
      group_by(Country) %>%
      summarise(n_articles = n(), .groups = "drop") %>%
      mutate(percentage = round(n_articles / length(unique(filtered_data$ID_Art)) * 100, 1)) %>%
      arrange(desc(percentage)) %>%
      slice_head(n = 10)

    req(length(unique(filtered_data$ID_Art)) > 0)

    ggplot(country_data, aes(x = reorder(Country, percentage), y = percentage)) +
      geom_bar(stat = "identity", fill = "#fd7e14", alpha = 0.8) +
      geom_text(aes(label = paste0(percentage, "% (", n_articles, ")")), 
                hjust = -0.1, color = "black", size = 3.5) +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = paste("Most Represented Countries", active_title_context()),
        x = "Country",
        y = "Percentage of Articles (%)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0, face = "bold"),
        axis.text = element_text(size = 12)
      )
  })

  # Graphique des journaux
  output$journal_plot <- renderPlot({
    filtered_data <- active_data()
    req(filtered_data, length(unique(filtered_data$ID_Art)) > 0)

    journal_data <- filtered_data %>%
      filter(!is.na(SO), SO != "") %>%
      group_by(SO) %>%
      summarise(n_articles = n(), .groups = "drop") %>%
      mutate(percentage = round(n_articles / length(unique(filtered_data$ID_Art)) * 100, 1)) %>%
      arrange(desc(percentage)) %>%
      slice_head(n = 10) %>%
      mutate(SO_wrapped = str_wrap(SO, width = 35))

    req(nrow(journal_data) > 0)

    ggplot(journal_data, aes(x = reorder(SO_wrapped, percentage), y = percentage)) +
      geom_bar(stat = "identity", fill = "#6f42c1", alpha = 0.8) +
      geom_text(aes(label = paste0(percentage, "% (", n_articles, ")")), 
                hjust = -0.1, color = "black", size = 3.5) +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = paste("Most Represented Journals", active_title_context()),
        x = "Journal",
        y = "Percentage of Articles (%)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0, face = "bold"),
        axis.text.y = element_text(size = 10)
      )
  })

  # Table des références les plus citées
  output$top_references_table <- DT::renderDataTable({
    filtered_data <- active_data()
    req(filtered_data, length(unique(filtered_data$ID_Art)) > 0)

    cluster_articles <- filtered_data %>% pull(ID_Art)
    
    cluster_refs <- references_collapse %>%
      filter(ID_Art %in% cluster_articles) 

    if(nrow(cluster_refs) == 0) {
      return(datatable(data.frame(Reference = "No reference data found for this selection"), options = list(pageLength = 5)))
    }

    tryCatch({
      top_refs <- cluster_refs %>%
        mutate(ItemID_Ref = as.character(ItemID_Ref)) %>%
        separate_rows(ItemID_Ref, sep = ",") %>%
        mutate(ItemID_Ref = as.numeric(trimws(ItemID_Ref))) %>%
        filter(!is.na(ItemID_Ref)) %>%
        group_by(ItemID_Ref) %>%
        summarise(article_count = n_distinct(ID_Art), .groups = "drop") %>%
        arrange(desc(article_count))

      total_articles <- length(cluster_articles)
      top_refs <- top_refs %>%
        mutate(Citations = round((article_count / total_articles) * 100, 1))

      final_refs <- top_refs %>%
        left_join(ref_for_alluv, by = "ItemID_Ref") %>%
        mutate(Reference = ifelse(is.na(AYR), paste("Unknown Ref", ItemID_Ref), AYR)) %>%
        select(Reference, Citations) %>%
        mutate(Reference = str_to_title(tolower(Reference))) %>%
        filter(!grepl("^[0-9]", Reference)) %>%
        filter(Citations >= 5) %>%
        rename("Frequency (in %)" = Citations) %>%
        arrange(desc(`Frequency (in %)`))

      datatable(
        final_refs,
        rownames = FALSE,
        options = list(
          pageLength = 25, scrollY = '500px', scrollCollapse = TRUE,
          order = list(list(1, 'desc')),
          columnDefs = list(list(className = 'dt-center', targets = 1))
        )
      )

    }, error = function(e) {
      datatable(data.frame(Reference = paste("Error:", e$message), Frequency = 0), options = list(pageLength = 5))
    })
  })

  # Titre de la table des articles
  output$table_title <- renderText({
    paste("Articles", active_title_context())
  })

  #===================================================================
  # BLOC DE CODE MODIFIÉ CI-DESSOUS
  #===================================================================
  #Table des articles
  output$cluster_table <- DT::renderDataTable({
    filtered_data <- active_data()
    req(filtered_data, length(unique(filtered_data$ID_Art)) > 0)
  
    table_data <- filtered_data %>% 
      mutate(Z9 = as.integer(Z9)) %>%
      arrange(desc(Z9)) %>%
      select(-any_of(c("intertemporal_name", "ID_Art", "SC", "Label", "ItemID_Ref", "University"))) %>%
      rename(Title = TI, Journal = SO, Citations = Z9, Categories = WC) %>%
      unique()
    
    # Définir les clusters où afficher Type_of_experiment
    clusters_with_experiment <- c(
      "Social Correlates of Trust",
      "Social Preferences and Trust"
    )
    
    show_experiment <- input$mode_select == "cluster" && 
                       input$cluster_select %in% clusters_with_experiment
    
    # Retirer la colonne si elle ne doit pas être affichée
    if (!show_experiment && "Experiment Type" %in% names(table_data)) {
      table_data <- table_data %>% select(-"Experiment Type")
    }

    # =========================================================================
    # NOUVEAU : Création de l'en-tête personnalisé avec le tooltip
    # =========================================================================
    
    # Obtenir les noms des colonnes du dataframe final
    column_names <- colnames(table_data)

    # Créer dynamiquement la liste des balises d'en-tête (<th>)
    header_tags <- lapply(column_names, function(col_name) {
      if (col_name == "Experiment Type" && show_experiment) {
        # Si c'est la colonne voulue, on ajoute un tooltip
        tags$th(
          div(style = "display: inline-block;",
              "Experiment Type",
              tooltip(
                trigger = icon("info-circle", class = "info-icon", style = "margin-left: 5px;"),
                HTML("Experiment types in this table were labeled by the author according to the taxonomy of Harrison and List (2004)"),
                placement = "top"
              )
          )
        )
      } else {
        # Sinon, un en-tête simple
        tags$th(col_name)
      }
    })

    # Créer le "squelette" HTML de la table avec notre en-tête personnalisé
    sketch <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          !!!header_tags # Le '!!!' déplie la liste de tags
        )
      )
    ))
    
    # =========================================================================

    datatable(table_data,
              # MODIFIÉ : On utilise notre 'sketch' comme container
              container = sketch,
              rownames = FALSE,
              options = list(
                scrollX = TRUE, scrollY = '500px', scrollCollapse = TRUE,
                pageLength = 25,
                columnDefs = list(list(className = 'dt-center', targets = c(1, 4)))
              ))
  })
}

# ===================================================================
# 5. LANCER L'APPLICATION
# ===================================================================
shinyApp(ui = ui, server = server)