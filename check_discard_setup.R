
  
  
  stocks<-c("mac.27.nea")
  stocks<-c( "pil.27.8c9a")
  stocks<-c("mac.27.nea", "pil.27.8c9a")
  
  # ============================================================
  # TARGET STOCKS
  # ============================================================
  stocks <- c("mac.27.nea", "pil.27.8c9a")
  
  # ============================================================
  # MAIN FUNCTION
  # ============================================================
  rcef_diagnosis <- function(
    census,
    grouping_vars       = c("metier_group", "area", "quarter"),
    threshold_discards  = 0.10,   # Escala 0–1 (10%)
    threshold_percent   = "10%",
    filter_main_metiers = TRUE
  ) {
    
    # ----------------------------------------------------------
    # 0. DEPENDENCIAS
    # ----------------------------------------------------------
    for (pkg in c("tidyverse", "ggrepel", "janitor", "rlang")) {
      if (!requireNamespace(pkg, quietly = TRUE))
        stop(paste("Package required but not installed:", pkg))
      library(pkg, character.only = TRUE)
    }
    
    # ----------------------------------------------------------
    # 1. VALIDACIÓN DE COLUMNAS
    # ----------------------------------------------------------
    required_cols <- c(
      "stock", "year", "total", "num_trips",
      "CatchCategory", "area", "quarter", "metier6"
    )
    missing_cols <- setdiff(required_cols, names(census))
    if (length(missing_cols) > 0)
      stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    
    # ----------------------------------------------------------
    # 2. PRE-FILTRADO: Regla del 99% (reduce ruido de métiers)
    # ----------------------------------------------------------
    if (filter_main_metiers) {
      main_metiers <- census %>%
        filter(CatchCategory == "Lan") %>%
        group_by(metier6, stock, area) %>%
        summarise(t = sum(as.numeric(total), na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(t)) %>%
        group_by(stock, area) %>%                      # <-- agrupar ANTES de cumsum
        mutate(cum_p = cumsum(t) / sum(t)) %>%
        filter(cum_p <= 0.99) %>%
        pull(metier6) %>%
        unique()
      
      census <- census %>% filter(metier6 %in% main_metiers)
    }
    
    # ----------------------------------------------------------
    # 3. PREPARACIÓN Y ESTRATIFICACIÓN
    # ----------------------------------------------------------
    census_prepped <- census %>%
      mutate(
        cod.FAO      = toupper(substring(stock, 1, 3)),
        metier_group = ifelse(
          grepl("PS_SPF", metier6),
          substring(metier6, 1, 2),
          substring(metier6, 1, 3)
        ),
        # Estandarización de área para Caballa
        area  = ifelse(cod.FAO == "MAC", substring(area, 1, 6), area),
        total = as.numeric(total)
      )
    
    grouping_syms <- rlang::syms(grouping_vars)
    
    landings_df <- census_prepped %>%
      filter(CatchCategory == "Lan") %>%
      group_by(stock, year, !!!grouping_syms) %>%
      summarise(
        total_landings = sum(total, na.rm = TRUE),
        n_samples      = sum(num_trips, na.rm = TRUE),
        .groups        = "drop"
      )
    
    discards_df <- census_prepped %>%
      filter(CatchCategory != "Lan") %>%
      group_by(stock, year, !!!grouping_syms) %>%
      summarise(
        total_discards = sum(total, na.rm = TRUE),
        .groups        = "drop"
      )
    
    # ----------------------------------------------------------
    # 4. MERGE Y CLASIFICACIÓN DIAGNÓSTICA
    # ----------------------------------------------------------
    setup_analysis <- landings_df %>%
      full_join(discards_df, by = c("stock", "year", grouping_vars)) %>%
      mutate(
        # BUG CORREGIDO: coverage_discards ahora en escala 0–1,
        # consistente con threshold_discards (también 0–1)
        coverage_discards = ifelse(
          !is.na(total_landings) & total_landings > 0,
          total_discards / total_landings,
          NA_real_
        ),
        
        Status = case_when(
          is.na(total_landings) | total_landings == 0 & is.na(total_discards)
          ~ "No Activity",
          total_landings == 0  & total_discards > 0
          ~ "Discard Only: No Landings",
          is.na(total_discards)                   ~ "NO Discard Data",
          coverage_discards > threshold_discards  ~ paste("SAFE: >",  threshold_percent, "Coverage"),
          coverage_discards < threshold_discards                  ~ paste("WARNING: <", threshold_percent, "Coverage"),
          TRUE                                    ~ "Review Manually"
        ),
        
        Raising_Decision = case_when(
          grepl("SAFE",    Status) ~ "Direct Raising (Robust)",
          grepl("WARNING", Status) ~ "Pooled Strata (Borrowing)",
          Status == "NO Discard Data" ~ "Borrowing Required",
          TRUE                        ~ "Review Manually"
        ),
        
        # Columna auxiliar para labels en plots (escala %)
        coverage_pct = round(coverage_discards * 100, 1)
      )
    
    # ----------------------------------------------------------
    # 5. PALETAS DE COLOR (centralizadas y reutilizables)
    # ----------------------------------------------------------
    safe_lbl    <- paste("SAFE: >",    threshold_percent, "Coverage")
    warning_lbl <- paste("WARNING: <", threshold_percent, "Coverage")
    
    status_colors <- c(
      "NO Discard Data"              = "#6BAED6",
      "No Activity"                  = "grey92",
      "Discard Only: No Landings"    = "tomato",
      "Review Manually"              = "plum",
      setNames("#B2DF8A",  safe_lbl),
      setNames("darkorange", warning_lbl)
    )
    
    bubble_colors <- c(
      "NO Discard Data"              = "#FF7F00",
      "No Activity"                  = "#f0f1f1",
      "Discard Only: No Landings"    = "#ffff99",
      "Review Manually"              = "plum3",
      setNames("#2ca25f",  safe_lbl),
      setNames("#1F78B4", warning_lbl)
    )
    
    # ----------------------------------------------------------
    # 6. BUBBLE PLOT
    # ----------------------------------------------------------
    plot_data <- filter(setup_analysis, stock %in% stocks)
    
    bubble_plot <- ggplot(
      plot_data,
      aes(
        x = factor(.data[[grouping_vars[3]]]),
        y = .data[[grouping_vars[1]]]
      )
    ) +
      geom_point(
        aes(
          size = ifelse(is.na(total_landings) | total_landings == 0, 1, total_landings),
          fill = Status
        ),
        shape = 21, color = "black", stroke = 0.6, alpha = 0.85
      ) +
      # X para descartes sin capturas
      geom_point(
        data  = filter(plot_data, is.na(total_landings) | total_landings == 0, total_discards > 0),
        shape = 4, color = "black", size = 3, stroke = 1
      ) +
      geom_text_repel(
        aes(label = ifelse(!is.na(coverage_pct), paste0(coverage_pct, "%"), "")),
        size = 2.8, fontface = "bold", box.padding = 0.5, max.overlaps = 20
      ) +
      facet_grid(stock ~ area, scales = "free_y", space = "free") +
      scale_size_continuous(range = c(4, 16), name = "Landings (t)") +
      scale_fill_manual(
        values = bubble_colors,
        guide  = guide_legend(override.aes = list(size = 9))
      ) +
      theme_bw() +
      labs(
        title    = "Discard Raising Diagnostic — Strategy Setup",
        subtitle = paste("Bubble size = Landings | Label = Coverage % | Threshold:", threshold_percent),
        x        = "Quarter",
        y        = "Metier Group"
      )
    
    # ----------------------------------------------------------
    # 7. HEATMAP PRINCIPAL (con lógica de faceting automática)
    # ----------------------------------------------------------
    n_stocks <- length(unique(plot_data$stock))
    
    heatmap_base <- ggplot(
      plot_data,
      aes(
        x    = factor(.data[[grouping_vars[3]]]),
        y    = .data[[grouping_vars[1]]],
        fill = Status
      )
    ) +
      geom_tile(color = "white", alpha = 0.85) +
      geom_text(
        aes(label = ifelse(!is.na(total_landings) & total_landings > 0,
                           round(total_landings, 0), "")),
        size = 2.5, color = "black", fontface = "bold"
      ) +
      scale_fill_manual(
        values = status_colors,
        name   = "Status",
        guide  = guide_legend(override.aes = list(size = 8))
      ) +
      theme_bw() +
      theme(
        plot.title       = element_text(face = "bold", size = 12),
        strip.text       = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey90", color = NA),
        axis.text.x      = element_text(angle = 0, hjust = 0.5)
      ) +
      labs(
        title    = paste(unique(plot_data$stock), collapse = " / "),
        subtitle = "Discard Coverage Diagnostic — Heatmap",
        x        = "Quarter",
        y        = "Métier",
        caption  = "Label = Landings (t)"
      )
    
    # Faceting automático según número de stocks
    heatmap_final <- if (n_stocks == 1) {
      message("facet_wrap(): single stock detected.")
      heatmap_base + facet_wrap(
        stats::as.formula(paste("~", grouping_vars[2])),
        scales = "fixed"
      )
    } else {
      message("facet_grid(): multiple stocks detected.")
      heatmap_base + facet_grid(
        stats::as.formula(paste("stock ~", grouping_vars[2])),
        scales = "free_y", space = "free_y"
      )
    }
    
    # ----------------------------------------------------------
    # 8. GAP TABLE (Strata que necesitan atención)
    # ----------------------------------------------------------
    gap_table <- setup_analysis %>%
      filter(!grepl("SAFE", Status)) %>%
      select(
        stock, year, area, quarter, metier_group,
        total_landings, total_discards,
        coverage_pct, Status, Raising_Decision
      ) %>%
      arrange(desc(total_landings))
    
    # ----------------------------------------------------------
    # 9. RESUMEN POR CONSOLA
    # ----------------------------------------------------------
    message("\n── Status Summary ──────────────────────────────────")
    print(janitor::tabyl(setup_analysis, Status))
    
    # ----------------------------------------------------------
    # 10. RETURN
    # ----------------------------------------------------------
    return(list(
      diagnosis  = setup_analysis,
      heatmap    = heatmap_final,
      plot       = bubble_plot,
      gap_table  = gap_table
    ))
  }
  
  # ============================================================
  # EJECUCIÓN
  # ============================================================
  setup <- rcef_diagnosis(census)
  
  setup$diagnosis  %>% headtail()
  setup$heatmap
  setup$plot
  setup$gap_table  %>% headtail()
  