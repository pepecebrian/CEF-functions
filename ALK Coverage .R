
###############################################################################
### ALK Coverage - CLEAN & ROBUST VERSION
### Author: Pepe 
### Date: 2026-06-21
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
    dplyr::mutate(
      length = as.numeric(length)
    ) %>%
    group_by(year, cod.FAO, semester, area, FMU) %>%
    dplyr::summarise(
      alk_lengths = list({
        raw <- sort(unique(length))
        if (first(cod.FAO) %in% c("ANE", "PIL")) {
          seq(min(raw, na.rm = TRUE), max(raw, na.rm = TRUE), by = 0.5)
        } else {
          seq(min(raw, na.rm = TRUE), max(raw, na.rm = TRUE), by = 1)
        }
      }),
      n_otoliths  = first(otholits, na.rm = TRUE),
      alk_min     = min(length, na.rm = TRUE),
      alk_max     = max(length, na.rm = TRUE),
      num_samples = {
        val <- max(num_samples, na.rm = TRUE)
        ifelse(is.infinite(val), NA_integer_, as.integer(val))
      },
      .groups = "drop"
    ) %>%
    filter(year == "2025") %>%
    as.data.frame()
}
###############################################################################
### STEP 2.1 — PREPARE DISTRIAGE (all observed catch lengths) from RDBES
###############################################################################



prepare_distriage <- function(Samp_ALK) {
  Samp_ALK %>%
    mutate(
      semester = ifelse(Q %in% c(1, 2), 1, 2)  #,
     # FMU      = ifelse(is.na(FMU) | FMU == "", area, FMU)
    ) %>%
   # filter(!is.na(length), length < 60, cod.FAO=="MAC") %>%
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
head (Samp_ALK,2)
      SAid   SSid SamplingScheme CatchCategory speciesCode SAspeciesCodeFAO  SAsex SAcommSizeCat
     <int>  <int>         <char>        <char>      <char>           <char> <char>         <int>
1: 3192203 505296         ES-IEO           Lan      127023              MAC      U             2
2: 3192203 505296         ES-IEO           Lan      127023              MAC      U             2
   SAtotalWeightLive SAsampleWeightLive SAfisheriesManagementUnit cod.FAO   LEid  SLid   SpeciesList
               <int>              <int>                    <char>  <char>  <int> <int>        <char>
1:            240000              13381                  27.8.c.e     MAC 119696   114 IEO_M_SAP_DCF
2:            240000              13381                  27.8.c.e     MAC 119696   114 IEO_M_SAP_DCF
       FMid FMclassMeasured FMnumberAtUnit FMtypeMeasured FMtypeAssessment FMconversionFactorAssessment
      <int>           <int>          <int>         <char>           <char>                        <num>
1: 19510084             330              1    LengthTotal      LengthTotal                            1
2: 19510085             340              3    LengthTotal      LengthTotal                            1
   Country  OSid       metier6   area  unitName     LEdate LOCODE LEfisheriesManagementUnit  year
    <char> <int>        <char> <char>     <int>     <IDat> <char>                    <char> <num>
1:      ES 88871 LHM_SPF_0_0_0 27.8.c 202500597 2025-03-18  ESSVB                            2025
2:      ES 88871 LHM_SPF_0_0_0 27.8.c 202500597 2025-03-18  ESSVB                            2025
        FMU       date month     Q  Year ExpertGroup      Stock      SpeciesName        a        b
     <char>     <Date> <num> <int> <int>      <fctr>     <fctr>           <fctr>    <num>    <num>
1: 27.8.c.e 2025-03-18     3     1  2025      WGWIDE mac.27.nea Scomber scombrus 0.023882 2.669419
2: 27.8.c.e 2025-03-18     3     1  2025      WGWIDE mac.27.nea Scomber scombrus 0.023882 2.669419
   length meanweight totalWeightLive_kg sampleWeightLive_kg     N    Npond
    <num>      <num>              <num>               <num> <int>    <num>
1:     33      0.281                240              13.381     1 17.93588
2:     34      0.304                240              13.381     3 53.80764

###############################################################################
### STEP 2.2 — PREPARE DISTRIAGE (all observed catch lengths) from CEF
###############################################################################
dn_cols <- c(
  "recordType", "vesselFlagCountry", "year", "workingGroup",
  "stock", "speciesCode", "catchCategory", "domainBiology",
  "distributionType", "distributionUnit", "distributionClass",
  "ageGroupPlus", "attributeType", "attributeValue",
  "variableType", "variableUnit", "valueType",
  "value", "PSU", "numPSUs", "numTrips", "numMeasurements", "variance"
)

dn_mac<-fread("IC_2026 DATOS 2025/WGWIDE/mac.27.nea/Landings/dn_mac_2025.csv") ; headtail(dn_mac)
tabyl(dn_mac,area)


prepare_distriage_dn <- function(dn){
  
  dn %>%
    
    # Extraer quarter y FMU desde domainBiology
    dplyr::mutate(cod.FAO="PIL",
      Q = as.integer(stringr::str_extract(domainBiology, "(?<=^Q)\\d")),
      
      FMU = stringr::str_extract(
        domainBiology,
        "(?<=Q\\d_)[0-9]+\\.[0-9]+\\.[a-z]+(?:\\.[a-z])?"
      ),
      
      # área agregada
      area = stringr::str_extract(FMU, "^[0-9]+\\.[0-9]+\\.[a-z]+"),
      
      semester = ifelse(Q %in% c(1,2), 1L, 2L),
      stock=stock
    ) %>%
    
    # Sólo distribuciones de número
    filter(
      cod.FAO == "PIL",
      variableType == "Number",
      !is.na(distributionClass)
    ) %>%
    
    dplyr:: mutate(
      length = as.numeric(distributionClass)
    ) %>%
    
    filter(length < 30) %>%
    
    group_by(year,stock, cod.FAO, semester, FMU, area) %>%
    
    dplyr:: summarise(
      min_length    = min(length, na.rm = TRUE),
      max_length    = max(length, na.rm = TRUE),
      catch_lengths = list(sort(unique(length))),
      .groups = "drop"
    ) %>%
    
    as.data.frame()
  
}


Samp_DN <- prepare_distriage_dn(distribution_dn)


###############################################################################
### STEP 3 — CREATE TILE DATA (expand lengths and detect coverage)
###############################################################################
create_tile_data <- function(ALK, Samp_ALK) {
  
  cat("\n=== Preparing ALK and DISTRIAGE ===\n")

  alk_data  <- prepare_alk(ALK_FINAL)
dist_data <- prepare_distriage(Samp_ALK)
#   dist_data<- prepare_distriage_dn(distribution_dn)
  
  # Expandir cada fila de dist_data a una fila por talla del rango
  tile_base <- dist_data %>%
    mutate(
      year=as.character(year),
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
    tidyr::unnest(all_lengths, keep_empty = TRUE) %>%
    as.data.frame()
  
  alk_data<- alk_data%>%
    mutate(
      year=as.character(year))
  
  alk_by_length <- ALK %>%#filter(cod.FAO=="MAC") %>% 
    mutate(
      year=as.character(year),
      length   = as.numeric(length)
      
    ) %>%
    group_by( cod.FAO,semester, FMU, length) %>%
    dplyr::summarise(n_otoliths_length = sum(n, na.rm = TRUE), .groups = "drop")
  
  head (tile_base)
  head (alk_data)
  
  

  
  tile_data <- tile_base %>%
    left_join(alk_data, by = c("year", "cod.FAO", "semester", "area", "FMU")) %>%
    left_join(alk_by_length, 
              by = c("cod.FAO", "semester", "FMU",
                     "all_lengths" = "length")) %>%
    mutate(
      has_alk   = !is.na(n_otoliths) & n_otoliths > 0,
      in_alk    = !is.na(n_otoliths_length) & n_otoliths_length > 0,  # ← otolitos reales por talla
      tile_type = case_when(
        !has_alk ~ "No ALK",
        in_alk   ~ "✓  Length Coverage by ALK",
        TRUE     ~ "⚠  Partial: Gap in Sizes"   # ← talla en rango pero sin otolitos
      ),
      alk_label = ifelse(in_alk,
                         as.character(n_otoliths_length),
                         NA_character_),
      domain_label = sprintf("S%s - %s", semester, FMU)
    )
  

  # Tabla resumen de otolitos
  table_df <- alk_data %>%
    filter(!is.na(n_otoliths)) %>%
    group_by(cod.FAO,FMU, semester) %>%
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
    mutate(all_lengths = as.numeric(all_lengths))   
  # ← corrección
  otolith_labels <- table_df |>filter(cod.FAO=="MAC") %>% 
    mutate(
      label      = as.character(Ages),
      label_color = ifelse(Ages < 100, "red", "darkgreen")
    ) 
  

  
  alk_only <- alk_data %>%
    filter(year == 2025) %>%
    anti_join(
      tile_data %>% distinct(cod.FAO, semester, FMU),
      by = c("cod.FAO", "semester", "FMU")
    ) %>%
    mutate(year = as.character(year)) %>%
    mutate(
      all_lengths_list = purrr::map2(alk_min, alk_max, function(mn, mx) {
        step <- if (first(cod.FAO) %in% c("ANE", "PIL")) 0.5 else 1
        seq(mn, mx, by = step)
      })
    ) %>%
    tidyr::unnest(all_lengths_list) %>%
    rename(all_lengths = all_lengths_list) %>%
    # ← mismo join que en create_tile_data
    left_join(
      alk_by_length,
      by = c("cod.FAO", "semester", "FMU",
             "all_lengths" = "length")
    ) %>%
    mutate(
      tile_type = ifelse(
        !is.na(n_otoliths_length) & n_otoliths_length > 0,
        "✓  ALK available / No catch sampled",
        "⚠  Partial: Gap in Sizes"            # ← talla sin otolitos en este dominio
      ),
      alk_label = ifelse(!is.na(n_otoliths_length) & n_otoliths_length > 0,
                         as.character(n_otoliths_length),
                         NA_character_)    )%>% as.data.frame()
  

  tile_data_plot <- bind_rows(
    tile_data %>% mutate(year = as.character(year)),
    alk_only  %>% mutate(year = as.character(year))
  )
  

  
 p_tile<- ggplot(tile_data_plot, aes(x = all_lengths, y = FMU)) +
    geom_tile(
      aes(fill  = tile_type,
          width = ifelse(cod.FAO %in% c("ANE", "PIL"), 0.49, 0.99)),
      color     = "black",
      linewidth = 0.3
    ) +
    facet_grid(semester ~ cod.FAO, scales = "free") +
    scale_x_continuous(
      breaks = function(x) {
        x <- x[is.finite(x)]
        if (length(x) == 0) return(numeric(0))
        step <- if (diff(range(x)) <= 15) 1 else 2
        seq(floor(min(x)), ceiling(max(x)), by = step)
      }
    ) +
    scale_fill_manual(
      values = c(
        "✓  Length Coverage by ALK"          = "#3A6EA5",
        "✓  ALK available / No catch sampled" = "#90CAF9",   # ← azul clarito
        "⚠  Partial: Gap in Sizes"            = "#FFB300",
        "No ALK"                              = "#E53935"
      ),
      name = "Status"
    ) +
    labs(
      title    = "ALK Coverage vs Catch Lengths",
      subtitle = "Blue = Covered | Light blue = ALK only | Amber = Gap | Red = No ALK",
      x        = "Length (cm)",
      y        = "FMU"
    ) +
    geom_text(
      aes(label = alk_label),
      size     = 3,
      color    = "black",
      fontface = "bold",
      na.rm    = TRUE
    ) +
    geom_text(
      data        = otolith_labels %>%
        left_join(                              # ← x_pos por FMU
          tile_data_plot |>
            group_by(cod.FAO, semester, FMU) |>
            summarise(x_pos = 50, #max(all_lengths, na.rm = TRUE) + 1.5,
                      .groups = "drop"),
          by = c("cod.FAO", "semester", "FMU")
        ),
      aes(x = x_pos, y = FMU, label = label, color = label_color),
      size        = 3.5,
      fontface    = "bold",
      hjust       = 0,
      inherit.aes = FALSE
    ) +
    scale_color_identity() +
    theme_bw(12) +
    theme(
      plot.title       = element_text(face = "bold", size = 11),
      plot.subtitle    = element_text(size = 9, color = "gray40"),
      strip.text       = element_text(face = "bold", size = 9),
      strip.background = element_rect(fill = "gray90", color = NA),
      panel.grid.minor = element_blank(),
      legend.position  = "right"
    )
  
    table_grob <- gridExtra::tableGrob(
    table_df, rows = NULL,
    theme = gridExtra::ttheme_default(base_size =6)
  )
  
  cowplot::ggdraw(p_tile) +
    cowplot::draw_grob(table_grob, x = 0.85, y = 0.8, width = 0.13, height = 0.05)
  }
  
  
final_plot <- plot_alk_coverage(
  tile_data,
  table_df
)
  
  
###############################################################################
### STEP 5 — ALL-IN-ONE FUNCTION
###############################################################################
analyze_alk_simple <- function(ALK, Samp_ALK) {
  
  result    <- create_tile_data(ALK_FINAL, Samp_ALK)              # ← ahora devuelve lista
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
    group_by(domain_label, cod.FAO, semester, FMU) %>%  # ← quitado Stock
    dplyr::summarise(
      n_total    = n(),
      n_covered  = sum(tile_type == "✓  Length Coverage by ALK"),
      n_gaps     = sum(tile_type == "⚠  Partial: Gap in Sizes"),
      n_no_alk   = sum(tile_type == "No ALK"),
      pct_covered = round(n_covered / n_total * 100, 1),
      n_otoliths = first(n_otoliths),
      .groups    = "drop"
    ) %>%
    arrange(desc(n_gaps)) %>% as.data.frame()
  
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
results <- analyze_alk_simple(ALK, Samp)

# Ver el gráfico
print(results$final_plot)

# Ver resumen por dominio
results$summary

# Ver los huecos (tallas sin cobertura en la ALK)
results$gaps %>% as.data.frame()

# Ver los datos completos del tile
#results$tile_data

# Ver la tabla de otolitos/muestras
results$table_df
ggsave("alk_coverage.png", results$final_plot, width = 14, height = 8, dpi = 300)




