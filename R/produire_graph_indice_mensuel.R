produire_graph_indice_mensuel <- function(onde_df, code_dpt, mois_cible = 5, force_complementaire = FALSE) {
  
  # 1. Calcul des indices ONDE filtrés par département
  indices_df <- onde_df %>% 
    dplyr::filter(code_departement == code_dpt) %>% 
    ondetools::calculer_indice_onde(force_complementaire = force_complementaire)
  
  # 2. Préparation des données
  onde_traite <- indices_df %>%
    dplyr::mutate(
      date_ref = as.Date(date_campagne),
      Annee    = lubridate::year(date_ref),
      Mois     = lubridate::month(date_ref)
    )
  
  # 3. Moyenne interannuelle du mois cible
  moyenne_interannuelle <- onde_traite %>%
    dplyr::group_by(Mois) %>%
    dplyr::summarise(indice_moyen = mean(indice, na.rm = TRUE), .groups = "drop") %>% 
    dplyr::filter(Mois == mois_cible)
  
  # 4. Filtrage et agrégation pour le mois cible
  df_plot <- onde_traite %>%
    dplyr::filter(Mois == mois_cible) %>%
    dplyr::group_by(Annee) %>%
    dplyr::summarise(indice = mean(indice, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      Mois = mois_cible,
      indice_moyen = ifelse(nrow(moyenne_interannuelle) > 0, moyenne_interannuelle$indice_moyen, NA_real_),
      etiquette_derniere = ifelse(Annee == max(Annee), sprintf("%.1f", indice), NA_character_)
    )
  
  # Sécurité si aucune donnée pour le département/mois
  if (nrow(df_plot) == 0 || all(is.na(df_plot$indice))) {
    warning(paste("Aucune donnée valide pour le département", code_dpt, "et le mois", mois_cible))
    return(NULL)
  }
  
  # Extraction sécurisée de la moyenne et calcul des bornes
  valeur_moyenne <- round(unique(df_plot$indice_moyen[!is.na(df_plot$indice_moyen)]), 2)
  
  if (length(valeur_moyenne) == 0) {
    valeur_moyenne <- NA_real_
  }
  
  # Calcul dynamique de y_min et des graduations y
  valeurs_y <- c(df_plot$indice, valeur_moyenne)
  valeurs_y <- valeurs_y[!is.na(valeurs_y)]
  
  y_min <- floor(min(valeurs_y, na.rm = TRUE))
  y_breaks <- sort(unique(c(seq(y_min, 10, by = 1), valeur_moyenne)))
  
  # Gestion des couleurs pour les graduations de l'axe Y (correctif anti-NA)
  couleurs_y <- if (!is.na(valeur_moyenne)) {
    ifelse(!is.na(y_breaks) & y_breaks == valeur_moyenne, "#E41A1C", "black")
  } else {
    "black"
  }
  
  # 5. Construction du graphique ggplot2
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = Annee)) +
    ggplot2::geom_line(ggplot2::aes(y = indice, color = "Indice ONDE"), linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(y = indice, color = "Indice ONDE"), size = 2.5) +
    ggplot2::geom_line(ggplot2::aes(y = indice_moyen, color = "Indice moyen"), linetype = "dashed", linewidth = 1) +
    ggplot2::geom_text(
      ggplot2::aes(y = indice, label = etiquette_derniere),
      vjust = 1.8,
      color = "#1F77B4",
      fontface = "bold",
      size = 4,
      na.rm = TRUE
    ) +
    ggplot2::coord_cartesian(
      xlim = range(df_plot$Annee, na.rm = TRUE),
      ylim = c(y_min - 0.3, 10)
    ) +
    ggplot2::scale_x_continuous(breaks = seq(min(df_plot$Annee, na.rm = TRUE), max(df_plot$Annee, na.rm = TRUE), by = 1)) +
    ggplot2::scale_y_continuous(breaks = y_breaks) +
    ggplot2::scale_color_manual(
      values = c("Indice ONDE" = "#1F77B4", "Indice moyen" = "#E41A1C"),
      name = ""
    ) +
    ggplot2::labs(
      title = paste0("Indice ONDE mensuel - Mois ", sprintf("%02d", mois_cible), 
                     " (Dép. ", code_dpt, " - Campagnes ", 
                     ifelse(force_complementaire, "toutes", "usuelles"), ")"),
      x = "Année",
      y = "Indice ONDE"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(
        face = "bold",
        color = couleurs_y
      ),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  return(p)
}