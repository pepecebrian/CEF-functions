
# Vector de paquetes requeridos (se instala el que falte y se carga)
packs <- c(
  "dplyr", "data.table", "janitor", "readr", "readxl", "lubridate",
  "stringr", "icesVocab", "icesSD", "tidyr", "FSA", "collapse",
  "ggplot2", "ggrepel"
)

invisible(lapply(packs, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))
census_catches <- read_csv("census_catches_MAC_PIL_2024.csv")
head (census_catches,2)
distributions <- read_csv("distributions_MAC_PIL.csv")
head (distributions,2)
census<-census_catches
# ============================================================
# TARGET STOCKS
# ============================================================
stocks <- c("mac.27.nea", "pil.27.8c9a")
rcef_diagnosis <- function(
    census,
    grouping_vars = c("metier_group", "area", "quarter"),
    threshold_discards = 0.1,
    threshold_percent = "10%",
    filter_main_metiers = TRUE # Nuevo parámetro para limpiar el ruido
) {
  
  require(tidyverse)
  require(ggrepel)
  
  # ============================================================
  # 1. PRE-FILTERING (The 95% Rule)
  # ============================================================
  if(filter_main_metiers){
    main_strata <- census %>%
      filter(CatchCategory == "Lan")  %>%
      group_by(stock,metier6) %>%
      summarise(t = sum(as.numeric(total), na.rm = TRUE)) %>%
      arrange(t) %>%
     arrange(desc(t)) %>%
      mutate(cum_p = cumsum(t)/sum(t)) %>%
    #  arrange(desc(cum_p)) %>%

      filter(cum_p <= 0.999) %>% 
      as.data.frame() # Mantenemos los métiers que suman el 95-96%
    
    census <- census %>% filter(metier6 %in% main_strata$metier6)
  }
  # -----------------------------
  # 1. AGREGACIÓN LANDINGS
  # -----------------------------
landings_df <- census %>%
    filter(CatchCategory == "Lan") %>%
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
  ) %>% 
    group_by(stock, year, !!!grouping_syms) %>%
    summarise(
      total_landings = sum(as.numeric(total), na.rm = TRUE),
      n_samples = sum(num_trips, na.rm = TRUE),
      .groups = "drop"
    )
  
  # -----------------------------
  # 2. AGREGACIÓN DISCARD
  # -----------------------------
  discards_df <- census %>%
    filter(CatchCategory != "Lan") %>%
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
    ) %>% 
    group_by(stock, year, !!!grouping_syms) %>%
    summarise(
      total_discards = sum(as.numeric(total), na.rm = TRUE),
      .groups = "drop"
    )
  
  # -----------------------------
  # 3. JOIN
  # -----------------------------
  setup_analysis <- landings_df %>%
   full_join(discards_df,
              by = c("stock","year", grouping_vars))
  
  # -----------------------------
  # 4. CÁLCULO DE COBERTURA
  # -----------------------------
  setup_analysis <- filter(setup_analysis, stock %in% stocks )%>%
    mutate(
      coverage_discards = (total_discards / total_landings) * 100,
      Status = case_when(
        total_landings == 0 ~ "No Activity",
        is.na(total_discards) ~ "NO Discard Data",
        coverage_discards > threshold_discards ~ paste("SAFE: >", threshold_percent, " Coverage"),
        coverage_discards <= threshold_discards & coverage_discards > 0 ~ paste("WARNING: <", threshold_percent, " Coverage"),
        TRUE ~ "Discard Only: No Landings"
      ),
      Raising_Decision = case_when(
        coverage_discards >= threshold_discards ~ "raising_st_catch",
        # La elevación es directa. Usamos los datos del propio estrato (st_catch) porque son estadísticamente robustos.
        coverage_discards < threshold_discards & coverage_discards > 0 ~ "pooled_strata (Borrowing)",
        # Aquí tenemos datos, pero son insuficientes. Para evitar sesgos por muestras pequeñas, forzamos un Pooling. El estrato 'pide prestada' representatividad a sus vecinos para alcanzar la estabilidad.
        is.na(total_discards) ~ "pooled_strata (Borrowing)",
        TRUE ~ "ACTION REQUIRED"
      )
    ) %>% as.data.frame()
    
  
  # -----------------------------
  # 5. HEATMAP
  # -----------------------------
  status_colors <- c(
    "NO Discard Data" = "lightblue",
    "NO Activity" = "cornflowerblue",
    "Discard Only: No Landings"= "#1F78B4"
  )
  status_colors[paste("SAFE: >", threshold_percent, " Coverage")] <-"#B2DF8A"
  status_colors[paste("WARNING: <", threshold_percent, " Coverage")] <- "#feb24c" 
 
  
  heatmap_base  <- ggplot(setup_analysis,
                         aes(x = factor(.data[[grouping_vars[3]]]), # quarter 
                             y = .data[[grouping_vars[1]]],   # metier_group
                             fill = Status)) +
    geom_tile(color = "black", alpha=0.85) +
    #facet_grid(stock~ .data[[grouping_vars[2]]], space="free_y") +
    geom_text(
      aes(
        label = ifelse(
          total_landings > 99,
          round(total_landings, 0),             # > 99 → 0 decimales
          ifelse(
            total_landings >= 10,
            round(total_landings, 1),           # [10, 99] → 1 decimal
            round(total_landings, 2)            # < 10 → 2 decimales
          )
        )
      ),
      size = 2.5,
      fontface = "bold"
    )+
    scale_fill_manual(values = status_colors) + # Usar el vector dinámico
    
    labs(title = "Discard Raising Setup: Coverage vs Threshold",
         subtitle = paste(unique(setup_analysis$stock), collapse = " / "),
         caption = paste("Labels = Tons of Landings | Threshold =", threshold_percent),
         x = "Quarter", y = "Metier") +
    theme(
      # Títulos
      plot.title      = element_text(face = "bold", size = 14),
      plot.subtitle   = element_text(color = "grey40", margin = margin(b = 8)),
      plot.caption    = element_text(color = "grey50", size = 8, hjust = 0),
      
      # Ejes
      axis.text.x     = element_text(angle = 0, hjust = 1, vjust = 1),
      axis.text.y     = element_text(size = 8),
      axis.ticks      = element_line(linewidth = 0.3),
      
      # Facets
      strip.text      = element_text(face = "bold", size = 9),
      strip.background = element_rect(fill = "grey92", color = NA),
      
      # Panel
      #  panel.grid      = element_blank(),                   # ← innecesario en heatmap
      panel.spacing   = unit(0.3, "lines"),
      
      # Leyenda
      legend.position  = "top",
      legend.title     = element_text(face = "bold", size = 9),
      legend.text      = element_text(size = 8)
    )
  
  n_stocks <- length(unique(setup_analysis$stock))
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
  
  # -----------------------------
  # 6. BUBBLE PLOT
  # -----------------------------
  bubble_colors <- c(
    "NO Discard Data" ='#6BAED6' ,
    "NO Activity" = "#f0f1f1",
    "Discard Only: No Landings"= "#ffff99"
  )
  bubble_colors[paste("SAFE: >", threshold_percent, " Coverage")] <-  "#B2DF8A"
  bubble_colors[paste("WARNING: <", threshold_percent, " Coverage")] <- "darkorange"
  
  bubble_data <- setup_analysis %>%
    mutate(
      # Si landings es 0 pero hay descartes, le damos un tamaño pequeño para que sea visible
      plot_size = ifelse( is.na(total_landings) & total_discards > 0, 1.0, total_landings),
      # Marcamos la etiqueta de texto para estos casos
      label_text = case_when(
        is.na(total_landings) & total_discards > 0 ~ "100%\nDisc",
        is.na(coverage_discards) ~ "",
        TRUE ~ paste0(round(coverage_discards, 1), "%")
      )
    )  
  bubble_plot<-  ggplot(bubble_data, 
                        aes(x = factor(.data[[grouping_vars[3]]]), 
                            y = .data[[grouping_vars[1]]])
                        )  +   # metier_group +
    # 1. Burbujas con borde negro para que resalten sobre el fondo
    geom_point(aes(size = plot_size, fill = Status), 
               shape = 21, color = "black", stroke = 0.8) +
    
    # 2. Marcador visual para descartes sin landings (X)
    geom_point(data = filter(bubble_data, (is.na(total_landings) | total_landings == 0) & total_discards > 0),
               aes(x = factor(quarter), y = metier_group), 
               shape = 4, color = "black", size = 2, stroke = 1) +
    
    
    facet_grid(stock ~ area, scales = "free_y", space = "free") +
    scale_size_continuous(range = c(5, 12), name = "Landings (t)")+
    scale_fill_manual(values = bubble_colors,
                      guide = guide_legend(override.aes = list(size = 9)))+
   # Reducimos un poco el máx para ganar aire
    theme_bw() +
    scale_x_discrete(expand = expansion(0.2)) +
    # 3. Etiquetas inteligentes con ggrepel
    geom_text_repel(aes(label = label_text),
                    size = 2.8,
                    fontface = "bold",
                    force = 3,             # Fuerza para separar etiquetas
                    box.padding = 0.6,      # Espacio para que el texto no toque el punto
                    point.padding = 0.4,    # Espacio desde el centro de la burbuja
                    min.segment.length = 0, # Siempre dibuja una línea si el texto se aleja
                    segment.size = 0.2,     # Línea muy fina
                    segment.color = "grey40",
                    lineheight = 0.8,
                    max.overlaps = Inf) +     # Ajuste para el salto de línea en "100%\nDisc"
    
    # 4. Configuración de escalas y facetas
    
    labs(
      title = "Discard Raising Diagnostic: Coverage vs Landings",
      subtitle = paste0("Bubble size = Landings (t) | Label = Coverage % (Threshold: ", threshold_percent, ")"),
      caption = "Cross (X) indicates Discards present with ZERO official Landings",
      x = "Quarter", y = "Metier",
      fill = "Sampling Status", size = "Landings (t)"
    ) +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0.5),
      strip.text       = element_text(face = "bold", size = 9),
      strip.background = element_rect(fill = "gray90", color = NA),
      #panel.grid.minor = element_blank(),
      #legend.position  = "top",
      legend.title     = element_text(size = 8),
      legend.text      = element_text(size = 8),
      axis.text.x      = element_text(size = 8),
      axis.text.y      = element_text(size = 8)
    )
  
  
  bubble_plot
  # ============================================================
# diagnosis_discards()
# ============================================================
# This function performs a full diagnostic of discard sampling coverage
# for RCEF workflows. It:
#   1. Optionally filters métiers using a 95–99% landings rule
#   2. Aggregates landings and discards by stock/year/strata
#   3. Computes discard coverage and assigns diagnostic categories
#   4. Produces:
#         - Heatmap of coverage vs threshold
#         - Bubble plot of coverage vs landings
#         - Gap table for strata requiring pooling/borrowing
#         - Full diagnostic dataset
#
# Arguments:
#   census                Data frame with census-level landings/discards
#   grouping_vars         Vector of grouping variables (metier_group, area, quarter)
#   threshold_discards    Numeric threshold for discard coverage (e.g., 0.1 = 10%)
#   threshold_percent     Character label for threshold (e.g., "10%")
#   filter_main_metiers   Logical: apply 95–99% métier filtering rule
#
# Returns:
#   A list with:
#       diagnosis   → processed dataset with coverage & decisions
#       heatmap     → ggplot object
#       bubbles     → ggplot object
#       gap_table   → table of strata requiring action
# ============================================================
# Vector de paquetes requeridos (se instala el que falte y se carga)
packs <- c(
  "dplyr", "data.table", "janitor", "readr", "readxl", "lubridate",
  "stringr", "icesVocab", "icesSD", "tidyr", "FSA", "collapse",
  "ggplot2", "ggrepel"
)

invisible(lapply(packs, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))
census_catches <- read_csv("census_catches_MAC_PIL_2024.csv")
head (census_catches,2)
distributions <- read_csv("distributions_MAC_PIL.csv")
head (distributions,2)
census<-census_catches
# ============================================================
# TARGET STOCKS
# ============================================================
stocks <- c("mac.27.nea", "pil.27.8c9a")
diagnosis_discards <- function(
    census,
    grouping_vars = c("metier_group", "area", "quarter"),
    threshold_discards = 0.1,
    threshold_percent = "10%",
    filter_main_metiers = TRUE
) {
  
  require(tidyverse)
  require(ggrepel)
  
  # Convert grouping variables to symbols for tidy evaluation
  grouping_syms <- syms(grouping_vars)
  
  # ============================================================
  # 1. PRE-FILTERING: Keep métiers representing 95–99% of landings
  # ============================================================
  if (filter_main_metiers) {
    main_strata <- census %>%
      filter(CatchCategory == "Lan") %>%
      group_by(stock, metier6) %>%
      summarise(t = sum(as.numeric(total), na.rm = TRUE)) %>%
      arrange(desc(t)) %>%
      mutate(cum_p = cumsum(t) / sum(t)) %>%
      filter(cum_p <= 0.999) %>%   # Keep métiers covering ~95–99%
      as.data.frame()
    
    census <- census %>% filter(metier6 %in% main_strata$metier6)
  }
  
  # -----------------------------
  # 2 AGREGACIÓN LANDINGS
  # -----------------------------
landings_df <- census %>%
    filter(CatchCategory == "Lan") %>%
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
  ) %>% 
    group_by(stock, year, !!!grouping_syms) %>%
    summarise(
      total_landings = sum(as.numeric(total), na.rm = TRUE),
      n_samples = sum(num_trips, na.rm = TRUE),
      .groups = "drop"
    )
  
  # -----------------------------
  # 3 AGREGACIÓN DISCARD
  # -----------------------------
  discards_df <- census %>%
    filter(CatchCategory != "Lan") %>%
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
    ) %>% 
    group_by(stock, year, !!!grouping_syms) %>%
    summarise(
      total_discards = sum(as.numeric(total), na.rm = TRUE),
      .groups = "drop"
    )
  
  # ============================================================
  # 4. JOIN LANDINGS + DISCARDS
  # ============================================================
  setup_analysis <- landings_df %>%
    full_join(discards_df, by = c("stock", "year", grouping_vars)) %>%
    filter(stock %in% stocks) %>%
    mutate(
      coverage_discards = (total_discards / total_landings) * 100,
      Status = case_when(
        total_landings == 0 ~ "No Activity",
        is.na(total_discards) ~ "NO Discard Data",
        coverage_discards > threshold_discards ~ paste("SAFE: >", threshold_percent, " Coverage"),
        coverage_discards <= threshold_discards & coverage_discards > 0 ~ paste("WARNING: <", threshold_percent, " Coverage"),
        TRUE ~ "Discard Only: No Landings"
      ),
      Raising_Decision = case_when(
        coverage_discards >= threshold_discards ~ "raising_st_catch",
        # La elevación es directa. Usamos los datos del propio estrato (st_catch) porque son estadísticamente robustos.
        coverage_discards < threshold_discards & coverage_discards > 0 ~ "pooled_strata (Borrowing)",
        # Aquí tenemos datos, pero son insuficientes. Para evitar sesgos por muestras pequeñas, forzamos un Pooling. El estrato 'pide prestada' representatividad a sus vecinos para alcanzar la estabilidad.
        is.na(total_discards) ~ "pooled_strata (Borrowing)",
        TRUE ~ "ACTION REQUIRED"
      )
    ) %>% as.data.frame()
  
  
    
  
  # -----------------------------
  # 5. HEATMAP
  # -----------------------------
  status_colors <- c(
    "NO Discard Data" = "lightblue",
    "NO Activity" = "cornflowerblue",
    "Discard Only: No Landings"= "#1F78B4"
  )
  status_colors[paste("SAFE: >", threshold_percent, " Coverage")] <-"#B2DF8A"
  status_colors[paste("WARNING: <", threshold_percent, " Coverage")] <- "#feb24c" 
 
  
  heatmap_base  <- ggplot(setup_analysis,
                         aes(x = factor(.data[[grouping_vars[3]]]), # quarter 
                             y = .data[[grouping_vars[1]]],   # metier_group
                             fill = Status)) +
    geom_tile(color = "black", alpha=0.85) +
    #facet_grid(stock~ .data[[grouping_vars[2]]], space="free_y") +
    geom_text(
      aes(
        label = ifelse(
          total_landings > 99,
          round(total_landings, 0),             # > 99 → 0 decimales
          ifelse(
            total_landings >= 10,
            round(total_landings, 1),           # [10, 99] → 1 decimal
            round(total_landings, 2)            # < 10 → 2 decimales
          )
        )
      ),
      size = 2.5,
      fontface = "bold"
    )+
    scale_fill_manual(values = status_colors) + # Usar el vector dinámico
    
    labs(title = "Discard Raising Setup: Coverage vs Threshold",
         subtitle = paste(unique(setup_analysis$stock), collapse = " / "),
         caption = paste("Labels = Tons of Landings | Threshold =", threshold_percent),
         x = "Quarter", y = "Metier") +
    theme(
      # Títulos
      plot.title      = element_text(face = "bold", size = 14),
      plot.subtitle   = element_text(color = "grey40", margin = margin(b = 8)),
      plot.caption    = element_text(color = "grey50", size = 8, hjust = 0),
      
      # Ejes
      axis.text.x     = element_text(angle = 0, hjust = 1, vjust = 1),
      axis.text.y     = element_text(size = 8),
      axis.ticks      = element_line(linewidth = 0.3),
      
      # Facets
      strip.text      = element_text(face = "bold", size = 9),
      strip.background = element_rect(fill = "grey92", color = NA),
      
      # Panel
      #  panel.grid      = element_blank(),                   # ← innecesario en heatmap
      panel.spacing   = unit(0.3, "lines"),
      
      # Leyenda
      legend.position  = "top",
      legend.title     = element_text(face = "bold", size = 9),
      legend.text      = element_text(size = 8)
    )
  
  n_stocks <- length(unique(setup_analysis$stock))
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
  
  # -----------------------------
  # 6. BUBBLE PLOT
  # -----------------------------
  bubble_colors <- c(
    "NO Discard Data" ='#6BAED6' ,
    "NO Activity" = "#f0f1f1",
    "Discard Only: No Landings"= "#ffff99"
  )
  bubble_colors[paste("SAFE: >", threshold_percent, " Coverage")] <-  "#B2DF8A"
  bubble_colors[paste("WARNING: <", threshold_percent, " Coverage")] <- "darkorange"
  
  bubble_data <- setup_analysis %>%
    mutate(
      # Si landings es 0 pero hay descartes, le damos un tamaño pequeño para que sea visible
      plot_size = ifelse( is.na(total_landings) & total_discards > 0, 1.0, total_landings),
      # Marcamos la etiqueta de texto para estos casos
      label_text = case_when(
        is.na(total_landings) & total_discards > 0 ~ "100%\nDisc",
        is.na(coverage_discards) ~ "",
        TRUE ~ paste0(round(coverage_discards, 1), "%")
      )
    )  
  bubble_plot<-  ggplot(bubble_data, 
                        aes(x = factor(.data[[grouping_vars[3]]]), 
                            y = .data[[grouping_vars[1]]])
                        )  +   # metier_group +
    # 1. Burbujas con borde negro para que resalten sobre el fondo
    geom_point(aes(size = plot_size, fill = Status), 
               shape = 21, color = "black", stroke = 0.8) +
    
    # 2. Marcador visual para descartes sin landings (X)
    geom_point(data = filter(bubble_data, (is.na(total_landings) | total_landings == 0) & total_discards > 0),
               aes(x = factor(quarter), y = metier_group), 
               shape = 4, color = "black", size = 2, stroke = 1) +
    
    
    facet_grid(stock ~ area, scales = "free_y", space = "free") +
    scale_size_continuous(range = c(5, 12), name = "Landings (t)")  +
    scale_fill_manual(values = bubble_colors,
                      guide = guide_legend(override.aes = list(size = 9)))+
   # Reducimos un poco el máx para ganar aire
    theme_bw() +
    scale_x_discrete(expand = expansion(0.2)) +
    # 3. Etiquetas inteligentes con ggrepel
    geom_text_repel(aes(label = label_text),
                    size = 2.8,
                    fontface = "bold",
                    force = 3,             # Fuerza para separar etiquetas
                    box.padding = 0.6,      # Espacio para que el texto no toque el punto
                    point.padding = 0.4,    # Espacio desde el centro de la burbuja
                    min.segment.length = 0, # Siempre dibuja una línea si el texto se aleja
                    segment.size = 0.2,     # Línea muy fina
                    segment.color = "grey40",
                    lineheight = 0.8,
                    max.overlaps = Inf) +     # Ajuste para el salto de línea en "100%\nDisc"
    
    # 4. Configuración de escalas y facetas
    
    labs(
      title = "Discard Raising Diagnostic: Coverage vs Landings",
      subtitle = paste0("Bubble size = Landings (t) | Label = Coverage % (Threshold: ", threshold_percent, ")"),
      caption = "Cross (X) indicates Discards present with ZERO official Landings",
      x = "Quarter", y = "Metier",
      fill = "Sampling Status", size = "Landings (t)"
    ) +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0.5),
      strip.text       = element_text(face = "bold", size = 9),
      strip.background = element_rect(fill = "gray90", color = NA),
      #panel.grid.minor = element_blank(),
      #legend.position  = "top",
      legend.title     = element_text(size = 8),
      legend.text      = element_text(size = 8),
      axis.text.x      = element_text(size = 8),
      axis.text.y      = element_text(size = 8)
    )
  
  

  # -----------------------------
  # 7. GAP TABLE
  # -----------------------------
  gap_table <- setup_analysis %>%
    filter(Status != paste("SAFE: >", threshold_percent)) %>%
    arrange(desc(total_landings)) %>%
    select(area, quarter, metier_group, total_landings,coverage_discards , Status) %>%
    mutate(
      Action_Required = case_when(
        Status == "CRITICAL: No Discard Data" ~ "BORROW: Search similar metier/quarter",
        Status == paste("WARNING: <", threshold_percent, " Coverage") ~ "POOL: Group with adjacent quarters",
        TRUE ~ "BORROW: Search similar metier/quarter"
      )
      
    ) %>% as.data.frame()
  
  # -----------------------------
  # 8. RETURN
  # -----------------------------
  return(list(
    diagnosis = setup_analysis,
    heatmap =   heatmap_final,
    bubbles = bubble_plot,
    gap_table = gap_table
  ))
}

setup <- diagnosis_discards(census)

setup$diagnosis %>% headtail()
setup$heatmap
setup$bubbles
setup$gap_table %>% headtail()
tabyl(setup$gap_table,Action_Required)