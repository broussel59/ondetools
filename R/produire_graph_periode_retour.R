library(ondetools)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readr)
library(purrr)
library(tibble)

produire_periodes_retour_indice_onde_courbes_2026 <- function(onde_df,
                                                              code_dpt = "59",
                                                              annee_courante = 2026,
                                                              force_complementaire = FALSE,
                                                              dossier_sortie = "resultats_indice_onde") {
  
  # 1. Filtrage et calcul des indices ONDE pour le département
  indices_df <- onde_df %>%
    dplyr::filter(code_departement == code_dpt) %>%
    ondetools::calculer_indice_onde(force_complementaire = force_complementaire)
  
  if (is.null(indices_df) || nrow(indices_df) == 0) {
    warning(paste("Aucune donnée ONDE valide pour le département", code_dpt))
    return(NULL)
  }
  
  # 2. Traitement des dates et agrégation mensuelle (mai à septembre)
  annees_reference <- 2012:annee_courante
  mois_reference <- 5:9
  ordre_mois <- c("Mai", "Juin", "Juillet", "Aout", "Septembre")
  
  # Probabilités des périodes de retour (sans le record)
  probas_retour <- c(
    "3 ans"  = 0.33,
    "5 ans"  = 0.20,
    "10 ans" = 0.10,
    "20 ans" = 0.05
  )
  
  onde_traite <- indices_df %>%
    dplyr::mutate(
      date_ref = as.Date(date_campagne),
      Annee    = lubridate::year(date_ref),
      Num_Mois = lubridate::month(date_ref)
    ) %>%
    dplyr::filter(
      Annee %in% annees_reference,
      Num_Mois %in% mois_reference
    ) %>%
    dplyr::group_by(Annee, Num_Mois) %>%
    dplyr::summarise(
      indice = mean(indice, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      indice = dplyr::if_else(is.nan(indice), NA_real_, indice),
      Mois   = factor(
        dplyr::case_when(
          Num_Mois == 5 ~ "Mai",
          Num_Mois == 6 ~ "Juin",
          Num_Mois == 7 ~ "Juillet",
          Num_Mois == 8 ~ "Aout",
          Num_Mois == 9 ~ "Septembre"
        ),
        levels = ordre_mois
      )
    )
  
  tableau_annuel <- onde_traite %>%
    dplyr::select(Annee, Mois, indice) %>%
    tidyr::complete(
      Annee = annees_reference,
      Mois = factor(ordre_mois, levels = ordre_mois)
    ) %>%
    dplyr::arrange(Annee, Mois)
  
  # 3. Calcul des quantiles + Calcul du record bas historique réel
  calcul_percentile_inclusif <- function(x, p) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, type = 7, names = FALSE))
  }
  
  tableau_retours <- purrr::map_dfr(
    names(probas_retour),
    function(nom_retour) {
      p <- probas_retour[[nom_retour]]
      valeurs <- lapply(mois_reference, function(m) {
        x <- onde_traite$indice[onde_traite$Num_Mois == m]
        calcul_percentile_inclusif(x, p)
      })
      tibble::tibble(periode = nom_retour, Mai = valeurs[[1]], Juin = valeurs[[2]], Juillet = valeurs[[3]], Aout = valeurs[[4]], Septembre = valeurs[[5]])
    }
  )
  
  # Calcul du Record le plus bas observé par mois
  records_mensuels <- onde_traite %>%
    dplyr::group_by(Mois) %>%
    dplyr::summarise(Record_bas = min(indice, na.rm = TRUE), .groups = "drop")
  
  # Format large pour ggplot2 (geom_ribbon)
  stats_retours_wide <- tableau_retours %>%
    tidyr::pivot_longer(cols = dplyr::all_of(ordre_mois), names_to = "Mois", values_to = "Indice") %>%
    tidyr::pivot_wider(names_from = periode, values_from = Indice) %>%
    dplyr::left_join(records_mensuels, by = "Mois") %>%
    dplyr::mutate(
      Mois = factor(Mois, levels = ordre_mois),
      Max_indice = 10
    )
  
  # Données de l'année courante
  data_annee_courante <- tableau_annuel %>%
    dplyr::filter(Annee == annee_courante, !is.na(indice))
  
  # 4. Construction du graphique
  y_min <- max(0, floor(min(c(stats_retours_wide$Record_bas, data_annee_courante$indice), na.rm = TRUE)))
  
  p <- ggplot2::ggplot(data = stats_retours_wide, ggplot2::aes(x = Mois)) +
    
    # Ruban 1 : [< 20 ans (Très sec)] - Zone entre 0/y_min et la courbe 20 ans
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = `20 ans`, ymax = `10 ans`, group = 1, fill = "10 à 20 ans"),
      alpha = 0.6
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = `10 ans`, ymax = `5 ans`, group = 1, fill = "5 à 10 ans"),
      alpha = 0.6
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = `5 ans`, ymax = `3 ans`, group = 1, fill = "3 à 5 ans"),
      alpha = 0.6
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = `3 ans`, ymax = Max_indice, group = 1, fill = "> 3 ans (Situation usuelle)"),
      alpha = 0.6
    ) +
    
    # Courbe du RECORD HISTORIQUE BAS (Ligne pointillée nette)
    ggplot2::geom_line(
      ggplot2::aes(y = Record_bas, group = 1, linetype = "Record bas historique"),
      color = "#8b0000",
      linewidth = 1
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = Record_bas),
      color = "#8b0000",
      size = 2
    ) +
    
    # Courbe et points 2026
    ggplot2::geom_line(
      data = data_annee_courante,
      ggplot2::aes(x = Mois, y = indice, group = 1, color = paste("Année", annee_courante)),
      linewidth = 1.4
    ) +
    ggplot2::geom_point(
      data = data_annee_courante,
      ggplot2::aes(x = Mois, y = indice, color = paste("Année", annee_courante)),
      size = 3.5
    ) +
    
    # Valeurs textuelles 2026
    ggplot2::geom_text(
      data = data_annee_courante,
      ggplot2::aes(x = Mois, y = indice, label = sprintf("%.2f", indice)),
      vjust = -1,
      fontface = "bold",
      color = "black",
      size = 3.8
    ) +
    
    # Palette des rubans
    ggplot2::scale_fill_manual(
      name = "Périodes de retour",
      values = c(
        "10 à 20 ans"                 = "#fdae61",
        "5 à 10 ans"                  = "#fee08b",
        "3 à 5 ans"                   = "#a6d96a",
        "> 3 ans (Situation usuelle)" = "#4575b4"
      ),
      breaks = c("10 à 20 ans", "5 à 10 ans", "3 à 5 ans", "> 3 ans (Situation usuelle)")
    ) +
    
    # Légendes des lignes
    ggplot2::scale_color_manual(
      name = NULL,
      values = setNames("black", paste("Année", annee_courante))
    ) +
    ggplot2::scale_linetype_manual(
      name = NULL,
      values = c("Record bas historique" = "dashed")
    ) +
    
    ggplot2::scale_y_continuous(
      limits = c(y_min - 0.2, 10.3),
      breaks = seq(y_min, 10, by = 0.5)
    ) +
    ggplot2::labs(
      title = paste0("Périodes de retour de l'indice ONDE — Année ", annee_courante, " (Dép. ", code_dpt, ")"),
      x = NULL,
      y = "Indice ONDE"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13, hjust = 0.5),
      legend.position = "bottom",
      legend.box = "vertical",
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # Exportation
  if (!is.null(dossier_sortie)) {
    dir.create(dossier_sortie, showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(
      filename = file.path(dossier_sortie, paste0("periodes_retour_indice_onde_", code_dpt, ".png")),
      plot = p,
      width = 10,
      height = 6,
      dpi = 300
    )
  }
  
  return(p)
}