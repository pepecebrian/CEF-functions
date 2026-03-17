
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
      length = as.integer(length),
      domainAge = paste0(quarter, "_", area)
    ) %>%filter(cod.FAO=="MAC") %>% 
    group_by(domainAge, cod.FAO, quarter, area) %>%
    summarise(
      alk_lengths = list(sort(unique(length))),
      n_otoliths = sum(n, na.rm = TRUE),
      alk_min = min(length, na.rm = TRUE),
      alk_max = max(length, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    as.data.frame()
}

###############################################################################
### STEP 2 — PREPARE DISTRIAGE (all observed catch lengths)
###############################################################################

prepare_distriage <- function(DISTRIAGE) {
  
  DISTRIAGE %>%filter(!area %in% c('27.8.a', '27.8.b')) %>%
    mutate(
      length = case_when(
        cod.FAO == "MAC" ~ round(length, 0),  
        cod.FAO == "PIL" ~ round(length, 1),  
        TRUE             ~ length             
      )
    ) %>% 
    filter(!is.na(length), length < 50  & cod.FAO=="MAC") %>%
    group_by(cod.FAO, gear, quarter, area) %>%
    summarise(
      min_length = min(length),
      max_length = max(length),
      catch_lengths = list(sort(unique(length))),
      .groups = "drop"
    ) %>%
    as.data.frame()
}

###############################################################################
### STEP 3 — CREATE TILE DATA (expand lengths and detect coverage)
###############################################################################

create_tile_data <- function(ALK, DISTRIAGE) {
  
  cat("\n=== Preparing ALK and DISTRIAGE ===\n")
  
  alk_data <- prepare_alk(ALK) 
    dist_data <- prepare_distriage(DISTRIAGE)
   
  tile_base <- dist_data %>%
    rowwise() %>%
    mutate(
      # SAFE seq(): if min or max are NA → empty vector
      all_lengths = list(
        if (is.finite(min_length) && is.finite(max_length)) {
          seq(min_length, max_length, by = 1)
        } else {
          numeric(0)
        }
      )
    ) %>%
    unnest(all_lengths) %>%
    ungroup() %>%
    as.data.frame()
  
  
  tile_data <- tile_base %>%
    left_join(
      alk_data,
      by = c("quarter", "area", "cod.FAO")
    ) %>%
    mutate(
      has_alk = !is.na(n_otoliths) & n_otoliths > 0,
      
      in_alk = map2_lgl(
        all_lengths,
        alk_lengths,
        ~ if (is.null(.y) || length(.y) == 0) FALSE else .x %in% .y
      ),
      
      ## Clasificar
      tile_type = case_when(
        !has_alk ~ "No ALK",
        in_alk ~ "✓  Length Coverage by ALK",
        TRUE ~ "⚠  Partial: Gap in Sizes"
      ),
      
      ## Etiqueta para eje Y
      domain_label = sprintf("Q%s - %s", quarter, area)
    ) %>%
    ## Reordenar dominios
    mutate(
      domain_label = factor(domain_label),
      domain_label = reorder(domain_label, -as.numeric(interaction(quarter, area)))
    )
  table_df <- ALK %>%filter(cod.FAO=="MAC") %>% 
    filter(!is.na( otholits)) %>%
    group_by(cod.FAO,area, quarter) %>%
    summarise(
      #Samples = unique(num_samples, na.rm = TRUE),
      Ages = unique( otholits, na.rm = TRUE),
      samples = unique(num_samples),
      .groups = "drop_last"
    )
  
  ## Contar huecos
  n_gaps <- sum(tile_data$tile_type == "⚠  Partial: Gap in Sizes")
  n_covered <- sum(tile_data$tile_type == "✓  Length Coverage by ALK")
  n_no_alk <- sum(tile_data$tile_type == "No ALK")
  
  cat(sprintf("\n=== Resumen ===\n"))
  cat(sprintf("✓  Length Coverage by ALK: %d lengths\n", n_covered))
  cat(sprintf("⚠  Partial: Gap in Sizes: %d gaps\n", n_gaps))
  cat(sprintf("✗ No ALK: %d lengths\n\n", n_no_alk))
  
  return(tile_data)
}

###############################################################################
### STEP 4 — PLOT ALK COVERAGE
###############################################################################


plot_alk_coverage <- function(tile_data, table_df) {
  
  p_tile <- ggplot(tile_data, aes(x = all_lengths, y = area)) +
    
    geom_tile(aes(fill = tile_type),
              color = "black", linewidth = 0.4) +
    
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
      subtitle = "Green = Covered | Amber = Internal Gap | Red = No ALK",
      x        = "Length (cm)",
      y        = "Domain (Quarter - Area)"
    ) +
    
    facet_grid(gear ~ cod.FAO + quarter, scales = "free_x", space = "free_x") +
    
    theme_bw(11) +
    theme(
      plot.title       = element_text(face = "bold", size = 11),
      plot.subtitle    = element_text(size = 9, color = "gray40"),
      strip.text       = element_text(face = "bold", size = 9),
      strip.background = element_rect(fill = "gray90", color = NA),
      panel.grid.minor = element_blank(),
      legend.position  = "right",
      legend.justification = c("right", "top"),
      legend.background    = element_rect(fill = alpha("white", 0.8),
                                          color = "grey80"),
      legend.box.margin    = margin(2, 2, 2, 2)
    ) +
    
    geom_text(
      data = tile_data,
      aes(
        x = min_length - ifelse(cod.FAO %in% c("PIL", "ANE"), 0.75, 1.5),
        y = area,
        label = ifelse(is.finite(min_length),
                       sprintf("←%d", round(min_length)),
                       NA_character_)
      ),
      hjust = 1, size = 2, fontface = "bold", color = "gray30"
    ) +
    
    geom_text(
      data = tile_data,
      aes(
        x = max_length + ifelse(cod.FAO %in% c("PIL", "ANE"), 2.5, 5),
        y = area,
        label = ifelse(is.finite(max_length),
                       sprintf("%d→", round(max_length)),
                       NA_character_)
      ),
      hjust = 1, size = 2, fontface = "bold", color = "gray30"
    )
  
  table_grob <- tableGrob(
    table_df,
    rows = NULL,
    theme = ttheme_default(base_size = 5)
  )
  
  final_plot <- ggdraw(p_tile) +
    draw_grob(table_grob, x = 0.84, y = 0.4, width = 0.13, height = 0.05)
  
  final_plot
}
###############################################################################
### STEP 5 — ALL-IN-ONE FUNCTION
###############################################################################

analyze_alk_simple <- function(ALK, DISTRIAGE) {
  
  tile_data <- create_tile_data(ALK, DISTRIAGE)
  final_plot <- plot_alk_coverage(tile_data, table_df)
  
  gaps <- tile_data %>%
    filter(tile_type == "⚠  Partial: Gap in Sizes") %>%
    group_by(domain_label, cod.FAO, quarter, area) %>%
    summarise(
      n_gaps = n(),
      gap_lengths = paste(all_lengths, collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(desc(n_gaps)) 
  
  summary <- tile_data %>%
    group_by(domain_label, gear, cod.FAO, quarter, area) %>%
    summarise(
      n_total = n(),
      n_covered = sum(tile_type == "Covered"),
      n_gaps = sum(tile_type == "Gap"),
      n_no_alk = sum(tile_type == "No ALK"),
      pct_covered = round(n_covered / n_total * 100, 1),
      n_otoliths = first(n_otoliths),
      .groups = "drop"
    ) %>%
    arrange(desc(n_gaps))
  
  return(list(
    tile_data = tile_data,
    final_plot = final_plot,
    gaps = gaps,
    summary = summary
  ))
}

###############################################################################
### USAGE
###############################################################################

results <- analyze_alk_simple(ALK_agrupada, DISTRIAGE)
print(results$final_plot)
results$summary
results$gaps

