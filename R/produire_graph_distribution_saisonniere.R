produire_graph_distribution_saisonniere <- function(onde_df, code_dpt, annee_choisie = 2026, force_complementaire = FALSE) {
  
  # 1. Calcul des indices ONDE filtrés par département
  indices_df <- onde_df %>% 
    dplyr::filter(code_departement == code_dpt) %>% 
    ondetools::calculer_indice_onde(force_complementaire = force_complementaire)
  
  # 2. Traitement des dates et agrégation mensuelle (mai à septembre)
  ordre_mois_labels <- c("Mai", "Juin", "Juillet", "Aout", "Septembre")
  
  onde_traite <- indices_df %>%
    dplyr::mutate(
      date_ref = as.Date(date_campagne),
      Annee    = lubridate::year(date_ref),
      Num_Mois = lubridate::month(date_ref)
    ) %>%
    # Filtre sur les mois de Mai (5) à Septembre (9)
    dplyr::filter(Num_Mois >= 5 & Num_Mois <= 9) %>%
    # Agrégation pour garantir 1 valeur d'indice moyen par mois et par année
    dplyr::group_by(Annee, Num_Mois) %>%
    dplyr::summarise(Indice = mean(indice, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      Mois = factor(
        dplyr::case_when(
          Num_Mois == 5 ~ "Mai",
          Num_Mois == 6 ~ "Juin",
          Num_Mois == 7 ~ "Juillet",
          Num_Mois == 8 ~ "Aout",
          Num_Mois == 9 ~ "Septembre"
        ),
        levels = ordre_mois_labels
      )
    )
  
  # Sécurité si aucune donnée n'est renvoyée
  if (nrow(onde_traite) == 0) {
    warning(paste("Aucune donnée valide entre mai et septembre pour le département", code_dpt))
    return(NULL)
  }
  
  # 3. Calcul des quartiles et de la médiane mensuelle historique
  stats_mensuelles <- onde_traite %>%
    dplyr::group_by(Mois) %>%
    dplyr::summarise(
      Indice_min    = min(Indice, na.rm = TRUE),
      Indice_q1     = quantile(Indice, 0.25, na.rm = TRUE),
      Indice_median = median(Indice, na.rm = TRUE),
      Indice_q3     = quantile(Indice, 0.75, na.rm = TRUE),
      Indice_max    = max(Indice, na.rm = TRUE),
      .groups       = "drop"
    )
  
  # 4. Isolement des données de l'année cible
  data_annee <- onde_traite %>%
    dplyr::filter(Annee == annee_choisie & !is.na(Indice))
  
  # Détermination dynamique des limites de l'axe Y
  y_min <- floor(min(onde_traite$Indice, na.rm = TRUE))
  y_max <- 10
  
  # 5. Construction du graphique ggplot2
  p <- ggplot2::ggplot() +
    # Quartile 1 : [Min à Q1] - Orange
    ggplot2::geom_ribbon(
      data = stats_mensuelles,
      ggplot2::aes(x = Mois, ymin = Indice_min, ymax = Indice_q1, group = 1, fill = "1er quartile [Min - Q1]"),
      alpha = 0.6
    ) +
    # Quartile 2 : [Q1 à Médiane] - Jaune
    ggplot2::geom_ribbon(
      data = stats_mensuelles,
      ggplot2::aes(x = Mois, ymin = Indice_q1, ymax = Indice_median, group = 1, fill = "2ème quartile [Q1 - Médiane]"),
      alpha = 0.6
    ) +
    # Quartile 3 : [Médiane à Q3] - Vert
    ggplot2::geom_ribbon(
      data = stats_mensuelles,
      ggplot2::aes(x = Mois, ymin = Indice_median, ymax = Indice_q3, group = 1, fill = "3ème quartile [Médiane - Q3]"),
      alpha = 0.6
    ) +
    # Quartile 4 : [Q3 à Max] - Bleu
    ggplot2::geom_ribbon(
      data = stats_mensuelles,
      ggplot2::aes(x = Mois, ymin = Indice_q3, ymax = Indice_max, group = 1, fill = "4ème quartile [Q3 - Max]"),
      alpha = 0.6
    ) +
    # Ligne de la Médiane historique
    ggplot2::geom_line(
      data = stats_mensuelles,
      ggplot2::aes(x = Mois, y = Indice_median, group = 1, color = "Médiane historique"),
      linetype = "solid",
      linewidth = 1.5
    ) +
    # Valeurs textuelles de la Médiane
    ggplot2::geom_text(
      data = stats_mensuelles,
      ggplot2::aes(x = Mois, y = Indice_median, label = sprintf("%.2f", Indice_median)),
      vjust = 1.8, fontface = "bold", color = "red", size = 3.8
    ) +
    # Courbe et points de l'année sélectionnée
    ggplot2::geom_line(
      data = data_annee,
      ggplot2::aes(x = Mois, y = Indice, group = 1, color = paste("Année", annee_choisie)),
      linewidth = 1.3
    ) +
    ggplot2::geom_point(
      data = data_annee,
      ggplot2::aes(x = Mois, y = Indice, group = 1, color = paste("Année", annee_choisie)),
      size = 3.5
    ) +
    # Étiquettes des valeurs de l'année sélectionnée
    ggplot2::geom_text(
      data = data_annee,
      ggplot2::aes(x = Mois, y = Indice, label = sprintf("%.2f", Indice)),
      vjust = -1, fontface = "bold", color = "black", size = 3.8
    ) +
    # Couleurs des bandes de quartiles
    ggplot2::scale_fill_manual(
      name = NULL,
      values = c(
        "1er quartile [Min - Q1]"     = "#fdae6b",
        "2ème quartile [Q1 - Médiane]" = "#ffed6f",
        "3ème quartile [Médiane - Q3]" = "#74c476",
        "4ème quartile [Q3 - Max]"     = "#6baed6"
      )
    ) +
    # Couleurs des repères de lignes
    ggplot2::scale_color_manual(
      name = NULL,
      values = setNames(
        c("black", "red"),
        c(paste("Année", annee_choisie), "Médiane historique")
      )
    ) +
    # Ajustement de l'axe Y
    ggplot2::scale_y_continuous(
      limits = c(y_min - 0.2, y_max + 0.3), 
      breaks = seq(y_min, y_max, by = 0.5)
    ) +
    ggplot2::labs(
      title = paste0("Profil saisonnier ONDE — Année ", annee_choisie, 
                     " (Dép. ", code_dpt, " - Campagnes ", 
                     ifelse(force_complementaire, "toutes", "usuelles"), ")"),
      x = NULL,
      y = "Indice ONDE"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      plot.title = ggplot2::element_text(face = "bold", size = 13, hjust = 0.5),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  return(p)
}