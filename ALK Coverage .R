
###############################################################################
### ALK Coverage - CLEAN & ROBUST VERSION
### Author: Pepe 
### Date: 2026-01-27
###############################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(gridExtra)
library(cowplot)
#install.packages("ggpmisc")
library(ggpmisc)

###############################################################################
### STEP 1 — PREPARE ALK (lengths available in ALK)
###############################################################################

prepare_alk <- function(ALK) {
  ALK %>%
    mutate(
      length   = as.numeric(length),
      semester = ifelse(quarter %in% c(1, 2), 1, 2)
    ) %>%
    group_by(year, cod.FAO, semester, area, FMU) %>%
    dplyr::summarise(
      alk_lengths = list(sort(unique(length))),          # ← tallas reales
      alk_seq     = list({                               # ← secuencia para detectar huecos
        raw  <- sort(unique(length))
        step <- if (first(cod.FAO) %in% c("ANE", "PIL")) 0.5 else 1
        seq(min(raw, na.rm = TRUE), max(raw, na.rm = TRUE), by = step)
      }),
      n_otoliths  = n_distinct(n, na.rm = TRUE),
      alk_min     = min(length, na.rm = TRUE),
      alk_max     = max(length, na.rm = TRUE),
      num_samples = max(num_samples, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(year == "2025") %>%
    as.data.frame()
}
###############################################################################
### STEP 2 — PREPARE DISTRIAGE (all observed catch lengths)
###############################################################################
prepare_distriage <- function(Samp) {
  Samp %>%
    mutate(
      semester = ifelse(Q %in% c(1, 2), 1, 2),
      FMU      = ifelse(is.na(FMU) | FMU == "", area, FMU)
    ) %>%
    filter(!is.na(length), length < 60) %>%
    mutate(year = "2025") %>%
    group_by(year, cod.FAO, semester, FMU, area) %>%
    dplyr::summarise(
      min_length    = min(length, na.rm = TRUE),
      max_length    = max(length, na.rm = TRUE),
      catch_lengths = list(sort(unique(length))),        # ← tallas reales también
      .groups = "drop"
    ) %>%
    as.data.frame()
}

###############################################################################
### STEP 3 — CREATE TILE DATA (expand lengths and detect coverage)
###############################################################################
create_tile_data <- function(ALK, Samp) {
  
  cat("\n=== Preparing ALK and DISTRIAGE ===\n")
  
  alk_data  <- prepare_alk(ALK)
  dist_data <- prepare_distriage(Samp)
  
  # Expandir cada fila de dist_data a una fila por talla del rango
  tile_base <- dist_data %>%
    mutate(
      all_lengths = purrr::pmap(
        list(min_length, max_length, cod.FAO),
        function(min_l, max_l, sp) {
          if (is.finite(min_l) && is.finite(max_l)) {
            step <- if (sp %in% c("ANE", "PIL")) 0.5 else 1
            seq(min_l, max_l, by = step)
          } else {
            numeric(0)
          }
        }
      )
    ) %>%
    tidyr::unnest(all_lengths) %>%
    as.data.frame()
  
  tile_data <- tile_base %>%
    left_join(
      alk_data,
      by = c( "cod.FAO", "semester", "area", "FMU")
    ) %>%
    mutate(
      has_alk = !is.na(n_otoliths) & n_otoliths > 0,
      
      in_alk = purrr::map2_lgl(
        all_lengths,
        alk_lengths,
        ~ if (is.null(.y) || length(.y) == 0) FALSE else .x %in% .y
      ),
      
      tile_type = case_when(
        !has_alk  ~ "No ALK",
        in_alk    ~ "✓  Length Coverage by ALK",
        TRUE      ~ "⚠  Partial: Gap in Sizes"
      ),
      
      domain_label = sprintf("S%s - %s", semester, FMU)
    ) %>%
    mutate(
      domain_label = reorder(factor(domain_label),
                             -as.numeric(interaction(semester, area)))
    )
  
  # Tabla resumen de otolitos
  table_df <- alk_data %>%
    filter(!is.na(n_otoliths)) %>%
    group_by(cod.FAO, FMU, area, semester) %>%
    dplyr::summarise(
      Ages    = sum(unique(n_otoliths, na.rm = TRUE)),
      Samples = sum(unique(num_samples, na.rm = TRUE)),
      .groups = "drop"
    )
  
  n_gaps     <- sum(tile_data$tile_type == "⚠  Partial: Gap in Sizes")
  n_covered  <- sum(tile_data$tile_type == "✓  Length Coverage by ALK")
  n_no_alk   <- sum(tile_data$tile_type == "No ALK")
  
  cat(sprintf("\n=== Resumen ===\n"))
  cat(sprintf("✓  Covered:  %d lengths\n", n_covered))
  cat(sprintf("⚠  Gaps:     %d lengths\n", n_gaps))
  cat(sprintf("✗  No ALK:   %d lengths\n\n", n_no_alk))
  
  return(list(tile_data = tile_data, table_df = table_df))
}
###############################################################################
### STEP 4 — PLOT ALK COVERAGE
###############################################################################
plot_alk_coverage <- function(tile_data, table_df) {
  
  tile_data <- tile_data %>%
    mutate(all_lengths = as.numeric(all_lengths))          # ← corrección
  
  p_tile <- ggplot(tile_data, aes(x = all_lengths, y = FMU)) +
    geom_tile(
      aes(fill  = tile_type,
          width = ifelse(cod.FAO %in% c("ANE", "PIL"), 0.49, 0.9)),
      color     = "black",
      linewidth = 0.3
    ) +
    facet_grid(semester ~ cod.FAO, scales = "free_x") +
    scale_x_continuous(                                    # ← continuo, no discreto
      breaks = function(x) {
        x <- x[is.finite(x)]
        if (length(x) == 0) return(numeric(0))
        step <- if (diff(range(x)) <= 15) 1 else 2
        seq(floor(min(x)), ceiling(max(x)), by = step)
      }
    ) +
    scale_fill_manual(
      values = c(
        "✓  Length Coverage by ALK" = "#00ACC1",
        "⚠  Partial: Gap in Sizes"  = "#FFB300",
        "No ALK"                    = "#E53935"
      ),
      name = "Status"
    ) +
    labs(
      title    = "ALK Coverage vs Catch Lengths",
      subtitle = "Cyan = Covered | Amber = Gap in sizes | Red = No ALK",
      x        = "Length (cm)",
      y        = "FMU"
    ) +
    # Anotaciones min/max — ahora y = FMU (consistente con aes principal)
    geom_text(
      aes(x     = min_length - ifelse(cod.FAO %in% c("PIL", "ANE"), 0.75, 1.5),
          y     = FMU,                                     # ← corregido
          label = ifelse(is.finite(min_length),
                         sprintf("←%d", round(min_length)), NA_character_)),
      hjust = 1, size = 2, fontface = "bold", color = "gray30"
    ) +
    geom_text(
      aes(x     = max_length + ifelse(cod.FAO %in% c("PIL", "ANE"), 1, 2),
          y     = FMU,                                     # ← corregido
          label = ifelse(is.finite(max_length),
                         sprintf("%d→", round(max_length)), NA_character_)),
      hjust = 0, size = 2, fontface = "bold", color = "gray30"
    ) +
    theme_bw(11) +
    theme(
      plot.title        = element_text(face = "bold", size = 11),
      plot.subtitle     = element_text(size = 9, color = "gray40"),
      strip.text        = element_text(face = "bold", size = 9),
      strip.background  = element_rect(fill = "gray90", color = NA),
      panel.grid.minor  = element_blank(),
      legend.position   = "right"
    )
  
  table_grob <- gridExtra::tableGrob(
    table_df, rows = NULL,
    theme = gridExtra::ttheme_default(base_size = 5)
  )
  
  cowplot::ggdraw(p_tile) +
    cowplot::draw_grob(table_grob, x = 0.94, y = 0.4, width = 0.13, height = 0.05)
}
###############################################################################
### STEP 5 — ALL-IN-ONE FUNCTION
###############################################################################
analyze_alk_simple <- function(ALK, Samp) {
  
  result    <- create_tile_data(ALK, Samp)              # ← ahora devuelve lista
  tile_data <- result$tile_data
  table_df  <- result$table_df
  
  final_plot <- plot_alk_coverage(tile_data, table_df)  # ← ambos argumentos
  
  gaps <- tile_data %>%
    filter(tile_type == "⚠  Partial: Gap in Sizes") %>%
    group_by(domain_label, cod.FAO, semester, area, FMU) %>%
    dplyr::summarise(
      n_gaps      = n(),
      gap_lengths = paste(sort(unique(all_lengths)), collapse = ", "),
      .groups     = "drop"
    ) %>%
    arrange(desc(n_gaps))
  
  summary_df <- tile_data %>%
    group_by(domain_label, cod.FAO, semester, area, FMU) %>%  # ← quitado Stock
    dplyr::summarise(
      n_total    = n(),
      n_covered  = sum(tile_type == "✓  Length Coverage by ALK"),
      n_gaps     = sum(tile_type == "⚠  Partial: Gap in Sizes"),
      n_no_alk   = sum(tile_type == "No ALK"),
      pct_covered = round(n_covered / n_total * 100, 1),
      n_otoliths = first(n_otoliths),
      .groups    = "drop"
    ) %>%
    arrange(desc(n_gaps))
  
  return(list(
    tile_data  = tile_data,
    table_df   = table_df,
    final_plot = final_plot,
    gaps       = gaps,
    summary    = summary_df
  ))
}
###############################################################################
### USAGE
###############################################################################
results <- analyze_alk_simple(ALK_final, Samp)

# Ver el gráfico
print(results$final_plot)

# Ver resumen por dominio
results$summary

# Ver los huecos (tallas sin cobertura en la ALK)
results$gaps

# Ver los datos completos del tile
results$tile_data

# Ver la tabla de otolitos/muestras
results$table_df





