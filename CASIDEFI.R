# ------------------------------------------------------------------------------
# prepare_age_setup()
#
# This function prepares several diagnostic tables related to ALK coverage,
# age sampling, and length ranges in both ALK and DISTRIAGE. It produces:
#   • Age sampling diagnostics by domain
#   • ALK length ranges (min/max)
#   • Catch length ranges (min/max)
#
# The function includes input validation and harmonizes area names between
# ALK and DISTRIAGE.
# ------------------------------------------------------------------------------
library(ggplot2)
prepare_age_setup <- function(
    
    threshold_coverage = 0.50,
    min_otoliths_alk = 100,
    DISTRIAGE = DISTRIAGE,
    ALK = ALK,
    tableBV = tableBV
) {
  # --------------------------------------------------------------------------
  # 0. INPUT VALIDATION
  # --------------------------------------------------------------------------
  required_dist <- c("stock", "quarter", "area", "gear", "cod.FAO", "length", "total")
  required_alk  <- c("cod.FAO", "area", "quarter", "length", "otholits", "num_samples")
  
  missing_dist <- setdiff(required_dist, names(DISTRIAGE))
  missing_alk  <- setdiff(required_alk, names(ALK))
  
  if (length(missing_dist) > 0) {
    stop(paste("DISTRIAGE is missing required columns:", paste(missing_dist, collapse=", ")))
  }
  if (length(missing_alk) > 0) {
    stop(paste("ALK is missing required columns:", paste(missing_alk, collapse=", ")))
  }
  
  message("✓ Input validation passed")
  
  # --------------------------------------------------------------------------
  # 1. AGE DIAGNOSTIC TABLE
  # --------------------------------------------------------------------------
  age_diagnostic <- DISTRIAGE %>%
    group_by(quarter, area, gear, cod.FAO) %>%
    summarise(
      Total_Numbers = sum(as.numeric(total), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      ALK %>%
        group_by(area, quarter, cod.FAO) %>%
        summarise(
          Min_L = min(length, na.rm = TRUE),
          Max_L = max(length, na.rm = TRUE),
          N_Ages = max(otholits, na.rm = TRUE),
          num_samples = max(num_samples, na.rm = TRUE),
          .groups = "drop"
        ),
      by = c("area", "quarter", "cod.FAO")
    ) %>%
    mutate(
      Status = case_when(
        N_Ages >= min_otoliths_alk ~ "SAFE: above threshold",
        N_Ages <  min_otoliths_alk ~ "WEAK: below threshold",
        TRUE ~ "CRITICAL: no ALK"
      )
    ) %>%
    as.data.frame()
  
  # --------------------------------------------------------------------------
  # 2. ALK LENGTH RANGES
  # --------------------------------------------------------------------------
  alk_ranges <- ALK %>%
    group_by(cod.FAO, area, quarter) %>%
    summarise(
      alk_min_L = round(min(length, na.rm = TRUE)),
      alk_max_L = round(max(length, na.rm = TRUE)),
      n_otoliths = max(otholits, na.rm = TRUE),
      n_samples  = max(num_samples, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    as.data.frame()
  
  # --------------------------------------------------------------------------
  # 3. DISTRIBUTION LENGTH RANGES
  # --------------------------------------------------------------------------
  dist_ranges <- DISTRIAGE %>%
    mutate(
      cod.FAO = toupper(substr(stock, 1, 3)),
      q_dist = as.numeric(substr(domainBiology, 1, 1)),
      area_dist = area
    ) %>%
    group_by(stock, cod.FAO, gear, quarter, q_dist, area_dist) %>%
    summarise(
      dist_min_L = min(as.numeric(length), na.rm = TRUE),
      dist_max_L = max(as.numeric(length), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    as.data.frame()
  
  # --------------------------------------------------------------------------
  # 4. RETURN ALL TABLES
  # --------------------------------------------------------------------------
  return(list(
    tableBV      = tableBV,
    ALK          = ALK,
    age_diagnostic = age_diagnostic,
    alk_ranges     = alk_ranges,
    dist_ranges    = dist_ranges
  ))
}
setup_AGE <- prepare_age_setup(
 

  threshold_coverage = 0.50,   # minimum % coverage to consider SAFE
  min_otoliths_alk = 100,       # minimum otoliths required per domain
  DISTRIAGE = DISTRIAGE,       # pass your DISTRIAGE table
  ALK = ALK,                   # pass your ALK table
  tableBV = tableBV            # pass your biological variables table
)

ALK          <- setup_AGE$ALK
age_diag     <- setup_AGE$age_diagnostic
alk_ranges   <- setup_AGE$alk_ranges
dist_ranges  <- setup_AGE$dist_ranges


tabyl(DISTRIAGE, stock)
DISTRIAGE<-filter(DISTRIAGE, stock!= "pil.27.8abd")

threshold_ages<-100

census_catches_MAC_PIL_2024 <- read_csv("D:/Usuarios/jlcebrian/Nextcloud/RCEF/census_catches_MAC_PIL_2024.csv")






check_age_setup <- function(census_df, dist_df, ALK_df,
                            threshold_ages = 100) {
  
  # -----------------------------
  # 1. LANDINGS POR DOMINIO
  # -----------------------------
  
  landings_summary <- filter(DISTRIAGE , !area %in% c("27.8.b", "27.8.a"))%>%
   # filter(CatchCategory %in% c("Lan")) %>%
    mutate(
      cod.FAO = toupper( substring(stock, 1, 3)),
      group_metier = ifelse(
        metier == "PS_SPF_>0_0_0",
        substring(metier, 1, 2),
        substring(metier, 1, 3)
      )
    ) %>%
    group_by(stock ,cod.FAO,quarter, area,  group_metier) %>%
    summarise(
      Total_Numbers = sum(total, na.rm = TRUE),
      .groups = "drop"
    ) %>% as.data.frame()
  
  # -----------------------------
  # 2. RESUMEN DE MUESTREO (distributions)
  # -----------------------------
  # -----------------------------
  # 2. RESUMEN DE MUESTREO (distributions)
  # -----------------------------
  #sampling_summary <- DISTRIAGE %>%
  #   group_by(cod.FAO, area, quarter) %>%
  #   summarise(
  #    N_Measurements = sum(numMeasurements, na.rm = TRUE),
  #  N_Samples = sum(numSamples, na.rm = TRUE),
  #    Has_Age = any(bvType == "Age"),
  #   .groups = "drop"
  # )
  # -----------------------------
  # 3. DIAGNÓSTICO PRINCIPAL
  # -----------------------------
  setup_diagnostic <- landings_summary %>%
    left_join(
      ALK %>%
        group_by(cod.FAO,area, quarter) %>%
        summarise(
          N_Ages = sum(n),
          N_Samples = max(num_samples),
          Min_L = min(length, na.rm = TRUE),
          Max_L = max(length, na.rm = TRUE),
          .groups = "drop"
        ),
      by = c("area", "quarter", "cod.FAO"= "cod.FAO")
    ) %>%
    mutate(
      Has_Age = !is.na(N_Samples),
      Age_Status = case_when(
        is.na(N_Samples) ~ "WARNING: Lengths only \n(Needs ALK)",
        N_Ages >= threshold_ages ~ "SAFE: >threshold",
        N_Ages < threshold_ages ~ "WEAK (Need Borrowing)",
        TRUE ~ "check"
      )
    )%>% as.data.frame()
  
  
  # -----------------------------
  # 4. BUBBLE PLOT
  # -----------------------------
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
    )+
  #  geom_point(aes(size = Total_Numbers, col= Age_Status), alpha = 0.7) +
  #  geom_point(aes(size = Total_Numbers, fill = Age_Status), 
             #  shape = 21, color = "black", stroke = 0.8) +
    
    
    scale_size(range = c(4, 10)) +
    facet_grid(stock~area) +
    
    scale_fill_manual(
      values = c(
        "SAFE: >threshold"                      = "#2ca25f",
        "WEAK (Need Borrowing)"                 = "cornflowerblue",
        "WARNING: Lengths only \n(Needs ALK)"   = "tomato",
        "check"                                 = "#de2d26"
      ),
      name = "Age data status",
      guide = guide_legend(
        override.aes = list(size = 6)          # tamaño fijo en la leyenda
      )
    )+
    labs(
      title = "Age Allocation Setup",
    
      x = "Quarter", y = "Metier",   subtitle  = "Size= Number Total"
    )   +
    theme_bw(base_size = 9) +
    theme(
      plot.title      = element_text(face = "bold", hjust = 0.5),
      strip.text      = element_text(face = "bold", size = 9),
      strip.background= element_rect(fill = "gray90", color = NA),
      panel.grid.minor= element_blank(),
      legend.position = "right",
      legend.title    = element_text(size = 8),
      legend.text     = element_text(size = 8),
      axis.text.x     = element_text(size = 8),
      axis.text.y     = element_text(size = 8)
    )
  
  # -----------------------------
  # 5. HEATMAP
  # -----------------------------
  heatmap_plot <- ggplot(setup_diagnostic_plot,
                         aes(x = factor(quarter), y = group_metier, fill = Age_Status)) +
    geom_tile(color = "black") +
    geom_text(aes(label = round(Total_Numbers, 0)), size = 2.5) +
    facet_grid(stock~area, scales="fixed") +
    scale_fill_manual(
      values = c(
        "SAFE: >threshold"                      = "#00ACC1",  # verde
        "WEAK (Need Borrowing)"                 = "#FFB300",  # morado
        "WARNING: Lengths only \n(Needs ALK)"   = "#E53935",  # naranja
        "check"                                 = "#e7298a"   # magenta/rojo
      ),
      name = "Status Age"
    )      +
    labs(
      title   = "Age allocation setup (heatmap by stock & area)",
      x       = "Quarter",
      y       = "Metier",
      caption = "Label = Total numbers (rounded)"
    ) 
  heatmap_plot
  # -----------------------------
  # 6. DEVOLVER TODO
  # -----------------------------
  return(list(
    diagnostic = setup_diagnostic,
    bubble_plot = bubble_plot,
    heatmap_plot = heatmap_plot
  ))
}
age_setup <- check_age_setup(
  census_df = census,
  dist_df = distributions,
  ALK_df = ALK,
  threshold_ages = 100
)

age_setup$diagnostic %>% filter(group_metier=="PS" & area =='27.8.c.e') %>% as.data.frame()
age_setup$bubble_plot
age_setup$heatmap_plot



###############################################################################
### ALK Coverage - CLEAN & ROBUST VERSION
### Author: Pepe (refactored with English comments)
### Date: 2026-01-27
###############################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

###############################################################################
### STEP 1 — PREPARE ALK (lengths available in ALK)
###############################################################################

prepare_alk <- function(ALK) {
  
  ALK %>%
    mutate(
      length = as.integer(length),
      domainAge = paste0(quarter, "_", area)
    ) %>%
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
  
  DISTRIAGE %>%
    filter(!area %in% c("27.8.a", "27.8.b")) %>%
    mutate(length = as.integer(length)) %>%
    filter(!is.na(length), length < 50) %>%
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
  
  cat(sprintf("✓ ALK domains: %d\n", nrow(alk_data)))
  cat(sprintf("✓ DISTRIAGE domains: %d\n", nrow(dist_data)))
  
  cat("\n=== Expanding length ranges ===\n")
  
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
  
  cat(sprintf("✓ Generated %d tiles (length × domain)\n", nrow(tile_base)))
  
  cat("\n=== Detecting ALK coverage ===\n")

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
      
      tile_type = case_when(
        !has_alk ~ "No ALK",
        in_alk ~ "Covered",
        TRUE ~ "Gap"
      ),
      
      domain_label = sprintf("Q%s - %s", quarter, area),
      domain_label = reorder(domain_label, -as.numeric(interaction(quarter, area)))
    )
  
  cat("\n=== Summary ===\n")
  cat(sprintf("✓ Covered: %d tiles\n", sum(tile_data$tile_type == "Covered")))
  cat(sprintf("⚠ Gaps: %d tiles\n", sum(tile_data$tile_type == "Gap")))
  cat(sprintf("✗ No ALK: %d tiles\n\n", sum(tile_data$tile_type == "No ALK")))
  
  return(tile_data)
}
head (tile_data)
###############################################################################
### STEP 4 — PLOT ALK COVERAGE
###############################################################################

## ============================================================================
## PASO 4: CREAR GRÁFICO
## ============================================================================

plot_alk_coverage <- function(tile_data) {
  
  ggplot(tile_data, aes(x = all_lengths, y = area)) +
    
    # Tiles
    geom_tile(aes(fill = tile_type), 
              color = "black", linewidth = 0.4) +
    # Facets
    #facet_grid(quarter ~ cod.FAO, scales = "free_y") +
    
    
    # Improved color palette (professional & colorblind-friendly)
    scale_fill_manual(
      values = c(
        "✓ Covered" = "#00ACC1",   # Green (good coverage)
        "⚠ Gap"     = "#FFB300",   # Amber (internal gap)
        "No ALK"  = "#E53935"    # Dark red (no ALK)
      ),
      name = "Status"
    ) +
    
    
    
    # Labels
    labs(
      title = "ALK Coverage vs Catch Lengths",
      subtitle = "Green = Covered | Amber = Internal Gap | Red = No ALK",
      x = "Length (cm)",
      y = "Domain (Quarter - Area)"
    ) +
    
    # Theme
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      strip.text = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "gray90", color = NA),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )+
    
    
    facet_grid(cod.FAO+gear~ quarter, scales = "free_y") +
    geom_text(
      data = overlap_check,
      aes(
        x = dist_min_L - 1.5,
        y = area_dist,
        label = ifelse(is.finite(dist_min_L),
                       sprintf("←%d", round(dist_min_L)),
                       NA_character_)
      ),
      hjust = 1, size = 2.5, fontface = "bold", color = "gray30"
    )
  
  geom_text(
    data = overlap_check %>%
      mutate(domain_label = sprintf("Q%d - %s", q_dist, area_dist),
             domain_label = reorder(domain_label,
                                    -as.numeric(interaction(q_dist, area_dist)))),
    aes(x = dist_max_L + 1, y = domain_label,
        label = sprintf("%d→", dist_max_L)),
    hjust = 0, size = 2.5, fontface = "bold", color = "gray30"
  ) 
}


 
 
 
 distriage_imputada <-  filter(DISTRIAGE , !area %in% c('27.8.a', '27.8.b'))%>%
   mutate(
     length = case_when(
       cod.FAO == "MAC" ~ round(length, 0),   # MAC → entero
       cod.FAO == "PIL" ~ round(length, 1),   # PIL → 1 decimal (10.5, 27.5…)
       TRUE             ~ length              # resto de especies sin tocar
     )
   ) %>% filter  (length<50) %>% unique() %>% 
  
   
   # 1. Intentamos el cruce con la ALK específica (Area/Quarter)
   left_join(ALK, by = c("area", 
                                  "cod.FAO" ,
                                  "quarter" ,
                                  "length")) %>%
   
   mutate(source= ifelse(is.na(Age), "Interpolated", "Real")) %>% 
   
  
   # 2. Traemos la ALK global como respaldo
   left_join(ALK_filled)   %>%
   
   # 3. IMPUTACIÓN: Si prop es NA, usamos prop_global
   mutate(prop_final = coalesce(prop, prop_g),
          Age_final  = coalesce (Age, Age_back)) %>%
   select(- prop_g,-n_global,-Age_back) %>% unique() %>% as.data.frame()
 
 
 distriage_imputada<-distriage_imputada %>% 
   mutate(source_final= ifelse(is.na(Age) & data_source=="Real", source, data_source))
 # 4. Limpiamos: Si aún así es NA (talla nunca muestreada), 
 # habrá que decidir si descartar o asignar a la edad máxima
 
 
 
 filter(distriage_imputada, cod.FAO=="MAC", length== 44) %>% as.data.frame()
 filter(distriage_imputada, cod.FAO=="PIL", length==10.5) %>% as.data.frame()
 
 ALK_global <- ALK_agrupada %>%
   group_by(cod.FAO, length, Age) %>%
   summarise(n = sum(n), .groups = "drop") %>%
   group_by(cod.FAO, length) %>%
   mutate(prop_global = n / sum(n)) %>%
   select(cod.FAO, length, Age, prop_global)
 
 library(dplyr)
 
 imputar_distriage_con_alk <- function(DISTRIAGE, ALK_agrupada, ALK_filled) {
   
   distriage_imputada <-  filter(DISTRIAGE , !area %in% c('27.8.a', '27.8.b'))%>%
     mutate(
       length = case_when(
         cod.FAO == "MAC" ~ round(length, 0),   # MAC → entero
         cod.FAO == "PIL" ~ round(length, 1),   # PIL → 1 decimal (10.5, 27.5…)
         TRUE             ~ length              # resto de especies sin tocar
       )
     ) %>% filter  (length<50) %>% unique() %>% 
     # 1. Cruce con ALK específica (área / quarter / especie / talla / edad)
     left_join(
       ALK %>% filter(!area %in% c("27.8.a", "27.8.b")),
       by = c("area", "cod.FAO", "quarter", "length")
     ) %>%
     mutate(source= ifelse(is.na(Age), "Interpolated", "Real")) %>% 
     # 2. Traer ALK global como respaldo (por especie y talla/edad)
     left_join(
       ALK_filled
     ) %>%
     
     # 3. IMPUTACIÓN: Si prop es NA, usamos prop_global
     mutate(prop_final = coalesce(prop, prop_g),
            Age_final  = coalesce (Age, Age_back)) 
   
   
  #mutate(
     #data_source= ifelse(is.na(Age) & data_source=="Real", "Interpolated",
                       #  data_source))    %>%
      
   # 5. Calcular números por edad (nage)
   distriage_imputada<-distriage_imputada %>% 
     mutate(source_final= ifelse(is.na(Age) & data_source=="Real", source, data_source)) %>% 
     select(- prop_g,-n_global,-Age_back,-data_source,-source, -Age) %>% unique() %>%  as.data.frame()
   
   return(  distriage_imputada)
 }
 
distriage_imputada<- imputar_distriage_con_alk(DISTRIAGE, ALK)
 headtail(distriage_imputada,4)
 
 ## Ver gráfico de cobertura
 distriage_imputada %>%
   #filter(cod.FAO == "PIL") %>%
   group_by(length,cod.FAO, quarter, gear,,source_final) %>%
   summarise(total_prop = sum(prop_final), .groups = "drop") %>%
   ggplot(aes(x = length, y = total_prop, fill = source_final)) +
   geom_col() +
   facet_grid(quarter~cod.FAO+gear, scales="free") +
   labs(title = "Cobertura de ALK - PIL",
        x = "Talla (cm)", y = "Proporción total")
 
 
 
 imputar_distriage_con_alk <- function(DISTRIAGE, ALK, ALK_filled) {
   
   # -----------------------------------------------------------------------
   # 1. PRE-PROCESSING & LENGTH STANDARDIZATION
   # -----------------------------------------------------------------------
   # - Exclude specific areas if needed
   # - Standardize length resolution: Integer for Mackerel (MAC), 0.1 for Sardine (PIL)
   # - Filter extreme values to ensure data quality
   # -----------------------------------------------------------------------
   
   distriage_processing <- DISTRIAGE %>%
     filter(!area %in% c('27.8.a', '27.8.b')) %>%
     mutate(
       length = case_when(
         cod.FAO == "MAC" ~ round(length, 0),  
         cod.FAO == "PIL" ~ round(length, 1),  
         TRUE             ~ length             
       )
     ) %>% 
     filter(length < 50) %>% 
     unique() %>%
     
     # ---------------------------------------------------------------------
   # 2. JOIN WITH BIOLOGICAL DATA (ALK)
   # ---------------------------------------------------------------------
   # First join: Specific ALK (Area / Quarter / Species / Length / Age)
   left_join(
     ALK %>% filter(!area %in% c("27.8.a", "27.8.b")),
     by = c("area", "cod.FAO", "quarter", "length")
   ) %>%
     mutate(source = ifelse(is.na(Age), "Interpolated", "Real")) %>% 
     
     # Second join: Global ALK backup (to fill gaps in tails/internal classes)
     left_join(ALK_filled) %>%
     
     # ---------------------------------------------------------------------
   # 3. IMPUTATION LOGIC
   # ---------------------------------------------------------------------
   # Coalesce: Use local data first; if NA, use global/hierarchical backup
   mutate(
     prop_final = coalesce(prop, prop_g),
     Age_final  = coalesce(Age, Age_back)
   ) %>%
     
     # Traceability: Final source identification
     mutate(source_final = ifelse(is.na(Age) & data_source == "Real", source, data_source)) %>%
     
     # Data cleanup: remove intermediate join columns
     select(-prop_g, -n_global, -Age_back, -data_source, -source, -Age) %>% 
     unique() %>% 
     as.data.frame()
   
   # -----------------------------------------------------------------------
   # 4. FINAL FORMATTING FOR YVES' FUNCTIONS (RDBES STANDARDS)
   # -----------------------------------------------------------------------
   # Transforming the biological data into the final 'distribution_data' 
   # format required by grp_AoL_alloc_N.
   # -----------------------------------------------------------------------
   
   distriage_yves <- distriage_processing %>%
     transmute(
       # Stratum Identifiers (Primary Keys for the join with catch data)
       year = 2024,
       stock = stock,
       speciesCode = speciesCode,
       domainBiology = domainBiology,
       vesselFlagCountry = "ES",
       workingGroup = ifelse(stock == "pil.27.8c9a", "WGHANSA", "WGWIDE"),
       catchCategory = "Lan", 
       
       # Age and Length data
       distributionValue = length,          
       attibuteValue = as.character(Age_final), # Note: 'attibuteValue' (RDBES typo)
       
       # Probabilities / ALK Proportions
       value = prop_final, 
       
       # Technical Metadata
       distributionType = "Length",
       distributionUnit = "cm",
       variableType = "Number",
       variableUnit = "n",
       valueType = "Relative",
       attributeType = "Age",
       
       # Plus Group handling
       ageGroupPlus = ifelse(Age_final >= 15, TRUE, FALSE),
       
       # Traceability field for diagnostics
       data_source = source_final
     )
   
   return(distriage_yves)
 }
 
 # EXECUTION
 distriage_final <- imputar_distriage_con_alk(DISTRIAGE, ALK, ALK_filled)
 headtail(distriage_final)
 library(dplyr)
 head (DISTRIAGE)
 # Creamos la tabla final para Yves a partir de tu tabla imputada
 distriage_yves <- distriage_imputada %>%
   transmute(
     # 1. Identificadores de estrato (idénticos a la tabla de capturas)
     year = 2024, # O el campo year si lo tienes en SD
     stock = stock,
     speciesCode = speciesCode,
     domainBiology = domainBiology,
     vesselFlagCountry = "ES", # Ajustar si tienes más países
     workingGroup =ifelse(stock=="pil.27.8c9a", "WGHANSA", "WGWIDE"),
     catchCategory = "Lan", # Importante: debe coincidir con la captura (Lan/RegDis)
     
     # 2. Datos de Talla y Edad
     distributionValue = length,          # Tu columna 'length'
     attibuteValue = as.character(Age_final), # IMPORTANTE: Yves usa 'attibuteValue' (sin T)
     
     # 3. El valor numérico: LA PROPORCIÓN
     # Usamos prop_final porque es la que tiene los datos reales + imputados
     value = prop_final, 
     
     # 4. Metadatos técnicos que Yves busca en sus funciones
     distributionType = "Length",
     distributionUnit = "cm",
     variableType = "Number",
     variableUnit = "n",
     valueType = "Relative",
     attributeType = "Age",
     
     # 5. Plus Group (Lógico: TRUE/FALSE)
     # Si la edad es mayor o igual a 15 (según tu contexto anterior), es TRUE
     ageGroupPlus = ifelse(Age_final >= 15, TRUE, FALSE),
     
     # 6. Trazabilidad (Opcional, pero útil)
     data_source = source_final
   )
 
 # Verificación rápida
 head(distriage_yves, 4)
 asignar_plus_group_extremos <- function(DISTRIAGE, ALK_global, plus_age = 11) {
   
   gaps_extremos <-filter(DISTRIAGE, !area %in% c("27.8.a", "27.8.b")) %>%
     anti_join(
       ALK_global,
       by = c("cod.FAO", "length")
     ) %>%
    mutate(
       Age        = plus_age,
       prop_final = 1
     ) %>%
     mutate(nage = total * prop_global)
   
   return(gaps_extremos)
 }
 
 gaps_extremos <- asignar_plus_group_extremos(DISTRIAGE, ALK_global, plus_age = 11)
 
 out_imputado_con_extremos <- bind_rows(out_imputado, gaps_extremos)
 
 
 
 library(tidyr)
 library(purrr)
 library(zoo)
 
 imputar_alk_global <- function(ALK_global, DISTRIAGE, incrementos) {
   # incrementos: named numeric vector, e.g. c("PIL" = 0.5, "MAC" = 1.0)
   
   ALK_imputada_final <- ALK_global %>%
     group_split(cod.FAO) %>%
     map_dfr(function(df) {
       sp <- unique(df$cod.FAO)
       if (length(sp) != 1L) {
         stop("Cada grupo debe tener un único cod.FAO")
       }
       inc <- incrementos[[sp]]
       if (is.null(inc)) {
         stop(paste0("No se ha definido incremento para especie ", sp))
       }
       
       # Rango de tallas basado en DISTRIAGE para esa especie
       rango_tallas <- DISTRIAGE %>%
         filter(cod.FAO == sp) %>%
         pull(length) %>%
         range(na.rm = TRUE)
       
       df %>%
         # 1. Crear rejilla completa de tallas con incremento específico
         complete(length = seq(rango_tallas[1], rango_tallas[2], by = inc),
                  Age) %>%
         
         group_by(Age) %>%
         # 2. Interpolación lineal de prop_global por edad
         mutate(
           prop_global = na.approx(prop_global, na.rm = FALSE)
         ) %>%
         # 3. Extrapolación en bordes
         mutate(
           prop_global = na.locf(prop_global, na.rm = FALSE),
           prop_global = na.locf(prop_global, fromLast = TRUE, na.rm = FALSE)
         ) %>%
         ungroup() %>%
         
         # 4. Normalizar por talla (suma de prop_global = 1)
         group_by(cod.FAO, length) %>%
         mutate(
           prop_global = prop_global / sum(prop_global, na.rm = TRUE)
         ) %>%
         ungroup()
     })
   
   return(ALK_imputada_final)
 }
 
 
 incrementos <- c("PIL" = 0.5, "MAC" = 1.0)
 
 ALK_global_interp <- imputar_alk_global(
   ALK_global  = ALK_global,
   DISTRIAGE   = DISTRIAGE,
   incrementos = incrementos
 )
 
 out_imputado <- imputar_distriage_con_alk(DISTRIAGE, ALK_agrupada, ALK_global_interp)
 gaps_extremos <- asignar_plus_group_extremos(DISTRIAGE, ALK_global_interp, plus_age = 11)
 
 out_imputado_final <- bind_rows(out_imputado, gaps_extremos)
 
 