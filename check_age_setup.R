# ============================================================
# AGE SETUP DIAGNOSTIC FUNCTION
# ============================================================
# Checks whether age data (otoliths / ALK) is sufficient for
# each sampling domain (quarter × metier × area). Returns a
# diagnostic table and two visualisation outputs:
#   - A bubble plot  (landings size + age status colour)
#   - A heatmap      (age status tile + landings label)
#
# Arguments:
#   census_df       : landings / catch data frame
#   dist_df         : length-distribution samples data frame
#   ALK_df          : Age-Length Key data frame (otolith readings)
#   threshold_ages  : minimum number of otoliths to consider a
#                     domain "safe" (default = 100)
#   grouping_vars   : character vector defining the domain axes;
#                     position matters →
#                       [1] x-axis  (typically "quarter")
#                       [2] y-axis  (typically "metier_group")
#                       [3] facet   (typically "area")
# ============================================================

head (ALK,4)
headtail (distributions,2)
check_age_setup <- function(census_df,
                            dist_df,
                            ALK_df,
                            threshold_ages  = 100,
                            threshold_samples = 1,
                            grouping_vars   = c("quarter",
                                                "metier_group",
                                                "area")) {
  
  grouping_syms <- rlang::syms(grouping_vars)
  
  # ----------------------------------------------------------
  # STEP 1 – LANDINGS BY DOMAIN
  # Aggregate total landings (tonnes) from the census table,
  # restricting to official landings (CatchCategory == "Lan").
  # metier_group collapses PS_SPF codes to "PS"; all others
  # are trimmed to their first three characters (e.g. "OTB").
  # ----------------------------------------------------------
  landings_summary <- census_catches %>%
    filter(CatchCategory == "Lan") %>%
    mutate(
      cod.FAO      = toupper(substring(stock, 1, 3)),
      metier_group = ifelse(
        metier6 == "PS_SPF_>0_0_0",
        substring(metier6, 1, 2),   # → "PS"
        substring(metier6, 1, 3)    # → "OTB", "LLS", …
      )
    ) %>%
    group_by(stock , cod.FAO, !!!grouping_syms, CatchCategory) %>%
    summarise(
      Total_Landings = sum(total / 1000, na.rm = TRUE),  # kg → tonnes
      .groups = "drop"
    )
  
  # ----------------------------------------------------------
  # STEP 2 – SAMPLING SUMMARY (length distributions)
  # Count the maximum number of length samples per domain.
  # Used later to distinguish "no sampling at all" from
  # "sampled but without ages".
  # ----------------------------------------------------------
  sampling_summary <- distributions %>%
    filter(variableType == "Number") %>%
    distinct() %>% 
    mutate(
      area = ifelse(
        stock== "mac.27.nea",
        substring(domainBiology, 3, 8),          # caso WGWIDE
        substring(domainBiology, 3, 9)          # resto (incluye WGHANSA, etc.)
      ),
      metier = sub("^[^_]*_[^_]*_", "", domainBiology),  # Remueve "número_área_" y toma el resto como metier
      gear = ifelse(grepl("^PS_SPF", metier), 
                    substring(metier, 1, 2), 
                    substring(metier, 1, 3)),
      cod.FAO = toupper(substr(stock, 1, 3))  
    )  %>% 
    group_by(cod.FAO, !!!grouping_syms) %>%
    summarise(
      N_Samples = max(numSamples, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    as.data.frame()
  sampling_summary <- distriage %>%
    mutate(area = substr(area, 1, 6)) %>%
    
    # keep one value of numSamples per original area
    group_by( cod.FAO, !!!grouping_syms) %>%
    summarise(
      numSamples = first(numSamples),
      .groups = "drop"
    ) %>%
    
    # aggregate to the shortened area
    group_by(cod.FAO, !!!grouping_syms) %>%
    summarise(
      N_Samples = sum(numSamples, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    
    as.data.frame()
  
  # ----------------------------------------------------------
  # STEP 3 – MAIN DIAGNOSTIC TABLE
  # Join landings → ALK summary → sampling summary, then
  # assign a traffic-light Age_Status label to every domain.
  #
  # Age_Status categories:
  #   SAFE            : ≥ threshold_ages otoliths + lengths present
  #   WEAK            : < threshold_ages otoliths + lengths present
  #   WARNING         : lengths available but NO ALK match found
  #   No Length Samp. : domain has landings but zero length samples
  #   Only Landings   : catch reported but nothing else available
  # ----------------------------------------------------------
 
  head (ALK)
   alk_summary <-ALK  %>%
    group_by(SAspeciesCodeFAO, area, quarter) %>%
    mutate(area=substring(area, 1,6)) %>% 
    group_by(area, quarter) %>% 
    mutate(num_measurements= sum(unique(num_measurements))) %>% ungroup() %>% 
    summarise(
      N_Ages = max(num_measurements , na.rm = TRUE),
      Min_L  = min(length,   na.rm = TRUE),
      Max_L  = max(length,   na.rm = TRUE),
      .groups = "drop"
    )
   alk_summary <- ALK %>%
     mutate(area_short = substr(area, 1, 6)) %>%
     
     # keep one value of num_measurements per original area
     group_by(SAspeciesCodeFAO,area, area_short, quarter) %>%
     summarise(num_measurements = first(num_measurements),
               .groups = "drop") %>%
     
     # sum measurements in the aggregated area
     group_by(SAspeciesCodeFAO,area_short, quarter) %>%
     summarise(
       N_Ages = sum(num_measurements, na.rm = TRUE),
       .groups = "drop"
     ) %>% rename(area=area_short)
  
  setup_diagnostic <- landings_summary %>%
    left_join(alk_summary,      by = c("area", "quarter", "cod.FAO" ="SAspeciesCodeFAO")) %>%
    left_join(sampling_summary) %>%
    distinct() %>%
    mutate(
      Age_Status = case_when(
        is.na(N_Ages) & N_Samples > threshold_samples          ~ "WARNING: Lengths only \n(Needs ALK)",
        N_Ages >  threshold_ages & !is.na(N_Samples)           ~ "SAFE — \nSufficient Age Data",
        N_Ages <= threshold_ages & !is.na(N_Samples)           ~ "WEAK \n(Need Borrowing)",
        is.na(N_Samples)  &  is.na(N_Ages)                     ~ "No Length Samplings",
        TRUE                                                   ~ "Only Landings"
      )
    ) %>%
    as.data.frame()
  
  # ----------------------------------------------------------
  # STEP 4 – BUBBLE PLOT
  # Each bubble represents one domain (quarter × metier × area
  # facet). Bubble SIZE encodes total landings; FILL colour
  # encodes Age_Status. Domains with no length sampling are
  # additionally marked with a cross (shape = 4).
  # ggrepel labels show otolith counts for SAFE/WEAK domains
  # and "No ALK" for WARNING domains.
  # ----------------------------------------------------------
  
  status_colors <- c(
    "SAFE — \nSufficient Age Data" ="#B2DF8A",   # Darker green
   # "SAFE — \nSufficient Age Data" =   "#33A02C",   # Lighter green
    "No Length Samplings" = "#FF7F00",     # Darker orange
  #  "WEAK \n(Need Borrowing)" = "#FDBF6F",        # Lighter orange
    "Portugal-trawl" = "#1F78B4",      # Darker blue
    "WEAK \n(Need Borrowing)" = "cornflowerblue"          # Lighter blue
  )
  tabyl(setup_diagnostic, Age_Status)
  
  bubble_plot <- ggplot(
    setup_diagnostic,
    aes(x = factor(.data[[grouping_vars[1]]]),
        y= .data[[grouping_vars[2]]])
  ) +
    # Main bubbles: size = landings, fill = age status
     geom_point(
       #data  = filter(setup_diagnostic, !is.na(N_Samples)),
       aes(size = Total_Landings, fill = Age_Status),
       shape = 21, ,color = "black", stroke = 0.5,
       position = position_jitter(width = 0.1, height = 0)
    
      ) +
     #Cross overlay for domains with no length sampling
      geom_point(
     data  = filter(setup_diagnostic, is.na(N_Samples)),
     aes(x    = factor(.data[[grouping_vars[1]]]),
        y= .data[[grouping_vars[2]]],
       shape = Age_Status),
    color = "black",  size = 3, stroke = 1
    ) +
    facet_grid(~area)+
    scale_size_continuous(range = c(3, 12)) +
    scale_fill_manual(values=status_colors)+
    #facet_grid(vars(.data[[grouping_vars[3]]])) +
  # scale_fill_viridis_d(#option = "turbo", direction = -1,
  #    guide  = guide_legend(override.aes = list(size = 8))
  #    )+
    scale_shape_manual(
      values = c("No Length Samplings" = 4,
                 "WARNING: Lengths only \n(Needs ALK)" = 21,
                 "Only Landings" = 21),
      name  = "Age status",
      guide = "none"
    ) +
  
   # scale_size_continuous(breaks = sort(unique(setup_diagnostic$Total_Landings)))+
    geom_text_repel(
      aes(
        label = case_when(
          Age_Status %in% c("WEAK \n(Need Borrowing)",
                            "SAFE — \nSufficient Age Data") ~
            paste(N_Ages, "otoliths"),
          Age_Status == "WARNING: Lengths only \n(Needs ALK)" ~ "No ALK",
          TRUE ~ ""
        )
      ),
      size               = 3,
      fontface           = "bold",
      force              = 3,
      box.padding        = 0.6,
      point.padding      = 0.4,
      min.segment.length = 0,
      segment.size       = 0.2,
      segment.color      = "grey40",
      lineheight         = 0.8,
      max.overlaps       = Inf
    ) +
  #  theme_bw() +
    labs(
      title    = "Age Diagnostic Bubble Plot",
      subtitle = paste0(
        "Bubble size = Total Landings (t)  |  Label = N otoliths",
        "  (Safe threshold > ", threshold_ages, ")"
      ),
      caption  = "X = No Length Samplings",
      x        = "Quarter",
      y        = "Métier Group"
    )+
    theme(
      # Title elements
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11),
      
      # Axis formatting
      axis.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size =10),
      
      # Panel and grid formatting
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey70"),
      
      # Facet formatting
      strip.text = element_text(size = 12, face = "bold"),
      strip.background = element_rect(fill = "grey95", color = "grey70"),
      
      # No legend needed with facets
      legend.position = "right"
    )
  ggplot(
    setup_diagnostic,
    aes(
      x = factor(.data[[grouping_vars[1]]]),
      y = .data[[grouping_vars[3]]]
    )
  ) +
    
    # Main bubbles
    geom_point(
      aes(
        size = Total_Landings,
         fill = Age_Status
      ),
      shape = 21,
      color = "black",
      stroke = 0.4,
      position = position_jitter(width = 0.08, height = 0)
    ) +
    
    # Crosses for missing sampling
    geom_point(
      data = subset(setup_diagnostic, is.na(N_Samples)),
      aes(
        x = factor(.data[[grouping_vars[1]]]),
        y = .data[[grouping_vars[3]]]
      ),
      shape = 4,
      color = "red",
      size = 2
    ) +
    
    # Facets by gear
    facet_grid(~Age_Status) +
    
    # Bubble size
    scale_size_continuous(range = c(4, 14), name = "Catch") +
    
    # Colors similar to example
    scale_fill_manual(values = status_colors, name = "gear") +
    
    # Reverse year axis
   # scale_y_reverse() +
    
    # X axis formatting
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    geom_text_repel(
      aes(
        label = case_when(
          Age_Status %in% c("WEAK \n(Need Borrowing)"
                           # "SAFE — \nSufficient Age Data"
                            ) ~
            paste(N_Ages, "otoliths"),
          Age_Status == "WARNING: Lengths only \n(Needs ALK)" ~ "No ALK",
          TRUE ~ ""
        )
      ),
      size               = 3,
      fontface           = "bold",
      force              = 3,
      box.padding        = 0.6,
      point.padding      = 0.4,
      min.segment.length = 0,
      segment.size       = 0.2,
      segment.color      = "grey40",
      lineheight         = 0.8,
      max.overlaps       = Inf
    ) +
    labs(
      title = "Age Diagnostic Bubble Plot",
      subtitle = paste0(
        "Bubble size = Total Landings (t) | Label = N otoliths",
        " (Safe threshold > ", threshold_ages, ")"
      ),
      x = "Age",
      y = "Year"
    ) +
    
    theme(
      panel.grid.major = element_line(color = "grey85"),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey90"),
      strip.text = element_text(face = "bold"),
      legend.position = "right"
    )
  # ----------------------------------------------------------
  # STEP 5 – HEATMAP
  # One tile per domain; FILL = Age_Status, TEXT = landings
  # (tonnes, rounded). Faceted by stock × area so each species
  # / area combination gets its own panel.
  # ----------------------------------------------------------
  heatmap_plot <- ggplot(
    setup_diagnostic,
    aes(y    = factor(.data[[grouping_vars[1]]]),
        x    = .data[[grouping_vars[2]]],
        fill = Age_Status)
  ) +
    geom_tile(color = "black", alpha = 0.7) +
    geom_text(
      aes(label = round(Total_Landings, 0)),
      size = 2.8,color="white", fontface = "bold"
    ) +
    facet_grid( .data[[grouping_vars[3]]]~stock ) +
   
    scale_fill_viridis_d(
      option    = "D",
      name      = "Age status",
      #direction = -1,
      guide     = guide_legend(override.aes = list(size = 8))
    ) +
    labs(
      title   = paste(unique(setup_diagnostic$stock),
                      collapse = " / "),
      subtitle = "Age Diagnostic — Heatmap",
      x       = "Quarter",
      y       = "Métier",
      caption = "Label = Landings (t)"
    ) +
    theme_bw() +
    theme(
      plot.title   = element_text(face = "bold", size = 14),
      axis.text.x  = element_text(hjust = 1),
      strip.text   = element_text(face = "bold")
    )
  
  # ----------------------------------------------------------
  # STEP 6 – RETURN
  # Returns a named list so the caller can access each output
  # independently:
  #   result$diagnostic   → data.frame with Age_Status column
  #   result$bubble_plot  → ggplot object
  #   result$heatmap_plot → ggplot object
  # ----------------------------------------------------------
  return(list(
    diagnostic   = setup_diagnostic,
    bubble_plot  = bubble_plot,
    heatmap_plot = heatmap_plot
  ))
}

age_setup <- check_age_setup(
  census_df       = census,        # landings / catch table
  dist_df         = distributions, # length-distribution samples
  ALK_df          = ALK,           # Age-Length Key (otolith readings)
  threshold_ages  = 100            # minimum otoliths to flag a domain as "SAFE"
)


age_setup$diagnostic

age_setup$diagnostic %>%
  filter(metier_group == "PS", area == "27.8.c.e")

# Ver los gráficos
age_setup$bubble_plot
age_setup$heatmap_plot
