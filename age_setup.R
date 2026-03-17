

head(DISTRIAGE)
head (ALK)

check_age_setup <- function(census_df, dist_df, ALK_df,
                            threshold_ages = 100) {
  
  # ============================================================================
  # 1. LANDINGS SUMMARY BY DOMAIN
  # ============================================================================
  # Summarises total numbers by stock × species × quarter × area × metier.
  # This is the "demand" side: how many fish need age assignment.
  # ============================================================================
  
  landings_summary <- dist_df %>%
    filter(!area %in% c('27.8.a', '27.8.b')) %>%
    mutate(
      length = case_when(
        cod.FAO == "MAC" ~ round(length, 0),  
        cod.FAO == "PIL" ~ round(length, 1),  
        TRUE             ~ length             
      )
    ) %>% 
    filter(length < 50) %>% 
    mutate(
      cod.FAO = toupper(substr(stock, 1, 3)),
      
      # Simplify metier grouping (PS_SPF_>0_0_0 → PS)
      group_metier = ifelse(
        metier == "PS_SPF_>0_0_0",
        substr(metier, 1, 2),
        substr(metier, 1, 3)
      )
    ) %>%
    group_by(stock, cod.FAO, quarter, area, group_metier) %>%
    summarise(
      Total_Numbers = sum(total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    as.data.frame()
  
  
  # ============================================================================
  # 2. AGE SAMPLING SUMMARY (ALK)
  # ============================================================================
  # Summarises ALK information by domain:
  #   - number of ages (N_Ages)
  #   - number of samples (N_Samples)
  #   - min/max length in ALK
  # ============================================================================
  
  alk_summary <- ALK_df %>%
    group_by(cod.FAO, area, quarter) %>%
    summarise(
      N_Ages    = sum(n, na.rm = TRUE),
      N_Samples = max(num_samples, na.rm = TRUE),
      Min_L     = min(length, na.rm = TRUE),
      Max_L     = max(length, na.rm = TRUE),
      .groups   = "drop"
    )
  
  
  # ============================================================================
  # 3. MAIN DIAGNOSTIC TABLE
  # ============================================================================
    # Joins nc with ALK sampling and classifies each domain:
  #   SAFE     → enough ages
  #   WEAK     → some ages but below threshold
  #   WARNING  → no ages (lengths only)
  # ============================================================================
  
  setup_diagnostic <- landings_summary %>%
    left_join(alk_summary,
              by = c("area", "quarter", "cod.FAO")) %>%
    mutate(
      Has_Age = !is.na(N_Samples),
      
      Age_Status = case_when(
        is.na(N_Samples)           ~ "WARNING: Lengths only \n(Needs ALK)",
        N_Ages >= threshold_ages   ~ "SAFE: >threshold",
        N_Ages <  threshold_ages   ~ "WEAK (Need Borrowing)",
        TRUE                       ~ "check"
      )
    ) %>%
    as.data.frame()
  
  
  # ============================================================================
  # 4. BUBBLE PLOT (LANDINGS × AGE STATUS)
  # ============================================================================
  # Bubble size = Total_Numbers
  # Bubble color = Age_Status
  # ============================================================================
  
  setup_diagnostic_plot <- setup_diagnostic %>%
    group_by(group_metier) %>%
    mutate(mean_numbers = mean(Total_Numbers, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(group_metier = reorder(group_metier, mean_numbers))
  
  bubble_plot <- ggplot(setup_diagnostic_plot,
                        aes(x = factor(quarter), y = group_metier)) +
    
    geom_point(
      aes(size = Total_Numbers, fill = Age_Status),
      shape  = 21,
      color  = "black",
      stroke = 0.7
    ) +
    
    scale_size(range = c(4, 10)) +
    
    facet_grid(stock ~ area) +
    
    scale_fill_manual(
      values = c(
        "SAFE: >threshold"                    = "#1B9E77",   # green
        "WEAK (Need Borrowing)"               = "#D95F02",   # amber
        "WARNING: Lengths only \n(Needs ALK)" = "#A50F15",   # dark red
        "check"                               = "#7570B3"    # purple
      ),
      name = "Age data status",
      guide = guide_legend(override.aes = list(size = 6))
    ) +
    
    labs(
      title = "Age Allocation Setup",
      subtitle = "Bubble size = Total numbers",
      x = "Quarter",
      y = "Metier"
    ) +
    
    theme_bw(base_size = 9) +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0.5),
      strip.text       = element_text(face = "bold", size = 9),
      strip.background = element_rect(fill = "gray90", color = NA),
      panel.grid.minor = element_blank(),
      legend.position  = "right",
      legend.title     = element_text(size = 8),
      legend.text      = element_text(size = 8),
      axis.text.x      = element_text(size = 8),
      axis.text.y      = element_text(size = 8)
    )
  
  
  # ============================================================================
  # 5. HEATMAP (LANDINGS × AGE STATUS)
  # ============================================================================
  # Tile color = Age_Status
  # Tile label = Total_Numbers
  # ============================================================================
  
  heatmap_plot <- ggplot(setup_diagnostic_plot,
                         aes(x = factor(quarter),
                             y = group_metier,
                             fill = Age_Status)) +
    
    geom_tile(color = "black") +
    geom_text(aes(label = round(Total_Numbers, 0)), size = 2.5) +
    
    facet_grid(stock ~ area, scales = "fixed") +
    
    scale_fill_manual(
      values = c(
        "SAFE: >threshold"                    = "#1B9E77",
        "WEAK (Need Borrowing)"               = "#D95F02",
        "WARNING: Lengths only \n(Needs ALK)" = "#A50F15",
        "check"                               = "#7570B3"
      ),
      name = "Age Status"
    ) +
    
    labs(
      title   = "Age Allocation Setup (Heatmap)",
      subtitle = "Label = Total numbers",
      x       = "Quarter",
      y       = "Metier"
    )
  
  
  # ============================================================================
  # 6. RETURN RESULTS
  # ============================================================================
  
  return(list(
    diagnostic   = setup_diagnostic,
    bubble_plot  = bubble_plot,
    heatmap_plot = heatmap_plot
  ))
}
age_setup <- check_age_setup(
 
  dist_df   = DISTRIAGE,
  ALK_df    = ALK,
  threshold_ages = 100
)
age_setup$diagnostic %>% filter(group_metier=="PS" & area =='27.8.c.e') %>% as.data.frame()
age_setup$bubble_plot
age_setup$heatmap_plot
