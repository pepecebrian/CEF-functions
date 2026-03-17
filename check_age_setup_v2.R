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
library(readr)
ALK_PIL_MAC <- read_csv("D:/Usuarios/jlcebrian/Nextcloud/RCEF/2026/ALK_PIL_MAC.csv")
setwd("C:/Users/Usuario/Nextcloud/RCEF/2026/Scripts Leire/PP")
ALK_PIL_MAC <- read_csv("C:/Users/Usuario/Nextcloud/RCEF/2026/ALK_PIL_MAC.csv")
headtail(ALK_PIL_MAC)
census_catches <- read_csv("C:/Users/Usuario/Nextcloud/RCEF/census_catches_MAC_PIL_2024.csv")
distributions <- read_csv("C:/Users/Usuario/Nextcloud/RCEF/distributions_MAC_PIL.csv")
ALK_PIL_MAC <- ALK_PIL_MAC %>% mutate(
  metier_group = ifelse(is.na(metier_group), "MIS", metier_group)
)
filter(ALK_PIL_MAC, quarter==2, metier_group=="MIS")
tabyl(ALK_PIL_MAC, cod.FAO,area)
head (ALK,2)
ALK<-ALK_PIL_MAC 

ALK%>% mutate(
  area = if_else(
    cod.FAO == "MAC",
    substring(area, 1, 6),
    area
  )
) %>%
  group_by(cod.FAO, area,metier_group, quarter) %>%
  summarise(
   # N_Samples = sum(unique(num_samples), na.rm = TRUE),
    otholits    = sum(n,   na.rm = TRUE),
   N_Ages   = sum(unique(otholits),   na.rm = TRUE),
    Min_L        = min(length, na.rm = TRUE),
    Max_L        = max(length, na.rm = TRUE),
    .groups = "drop"
  )
filter(ALK, area =="27.9.a" , cod.FAO=="MAC")
filter(ALK, quarter==2, metier_group=="MIS")


tabyl(setup_diagnostic, Age_Status)
head (alk_summary,2)
head (ALK,2)
head (census,2) %>% as.data.frame()
headtail (distributions,2)
headtail(census)
stocks<-"mac.27.nea"
stocks<-"pil.27.8c9a"
stocks<-c("pil.27.8c9a","mac.27.nea")
check_age_setup <- function(census_df,
                            dist_df,
                            ALK_df,
                            threshold_ages  = 50,
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
      cod.FAO = toupper(substring(stock, 1, 3)),
      
      metier_group = ifelse(
        metier6 == "PS_SPF_>0_0_0",
        substring(metier6, 1, 2),
        substring(metier6, 1, 3)
      ),
      
      area = ifelse(
        cod.FAO == "MAC",
        substring(area, 1, 6),
        area
      )
    ) %>%
    group_by(stock , cod.FAO, !!!grouping_syms, CatchCategory) %>%
    summarise(
      Total_Landings = sum(total , na.rm = TRUE),  # kg → tonnes
      .groups = "drop"
    )
  
  # ----------------------------------------------------------
  # STEP 2 – SAMPLING SUMMARY (length distributions)
  # Count the maximum number of length samples per domain.
  # Used later to distinguish "no sampling at all" from
  # "sampled but without ages".
  # ----------------------------------------------------------
  sampling_summary <- distributions %>%
    filter(variableType == "Number",  !grepl("27.8.b", domainBiology)) %>%
    distinct() %>% 
    mutate(
      quarter = as.numeric(substring(domainBiology, 1, 1)), 
      area = ifelse(
        stock== "mac.27.nea",
        substring(domainBiology, 3, 8),          # caso WGWIDE
        substring(domainBiology, 3, 10)          # resto (incluye WGHANSA, etc.)
      ),
      metier6 = sub("^[^_]*_[^_]*_", "", domainBiology),  # Remueve "número_área_" y toma el resto como metier
      metier_group= ifelse(grepl("^PS_SPF", metier6), 
                    substring(metier6, 1, 2), 
                    substring(metier6, 1, 3)),
      cod.FAO = toupper(substr(stock, 1, 3))  
    )  %>% filter( !area%in% c('27.8.b')) %>% 
    group_by(cod.FAO, !!!grouping_syms) %>%
    summarise(
      Total_Numbers= sum(total),
      N_Samples = max(numSamples, na.rm = TRUE),
      .groups   = "drop"
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
 
 
  alk_summary <- ALK%>% mutate(
    area = if_else(
      cod.FAO == "MAC",
      substring(area, 1, 6),
      area
    )
  ) %>%
    group_by(cod.FAO, area,metier_group, quarter) %>%
    summarise(
      # N_Samples = sum(unique(num_samples), na.rm = TRUE),
      otholits    = sum(n,   na.rm = TRUE),
      N_Ages   = sum(unique(otholits),   na.rm = TRUE),
      Min_L        = min(length, na.rm = TRUE),
      Max_L        = max(length, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(
      stock = ifelse(cod.FAO == "MAC", "mac.27.nea", "pil.27.8c9a")
    ) 
   
  
  setup_diagnostic <- landings_summary %>%

    full_join(alk_summary)%>%
    left_join(sampling_summary) %>%
    distinct() %>%
    mutate(
      Age_Status = case_when(
        Total_Landings == 0 ~ "No Landings",
        
        # --- EL NUEVO ESTADO QUE BUSCABAS ---
        is.na(N_Samples) & N_Ages > 0  ~ "Ages only \n(No Length Sampling)",
        
        # --- RESTO DE LA LÓGICA ---
        is.na(N_Samples) & is.na(N_Ages) & Total_Landings > 0 ~ "CRITICAL: No Sampling",
        
        is.na(N_Ages) & N_Samples >= threshold_samples ~ "WARNING: Lengths only (Needs ALK)",
      
        N_Ages >= threshold_ages & !is.na(N_Samples)    ~ "SAFE: Above threshold \nSufficient Age Data",
        N_Ages < threshold_ages  & !is.na(N_Samples)    ~ "WEAK: below threshold \n(Need Borrowing)",
        
        TRUE ~ "Other/Check")) %>% 
  
    as.data.frame()
  # --- 5. GLOBAL QUALITY METRIC ---
  # Calculate the percentage of total landings covered by each status
  quality_check <- setup_diagnostic %>%
    filter(!is.na(Total_Landings), Total_Landings > 0) %>%
    group_by(stock, Age_Status) %>%
    summarise(Landings_t = sum(Total_Landings), .groups = "drop_last") %>%
    mutate(Percentage = round(Landings_t / sum(Landings_t) * 100, 1))
  
  # --- 6. UNIFIED BUBBLE PLOT ---
  # Size = Catch volume | Color = Quality Status | Labels = Potential Donors
  # ----------------------------------------------------------
  # STEP 4 – BUBBLE PLOT
  # Each bubble represents one domain (quarter × metier × area
  # facet). Bubble SIZE encodes total landings; FILL colour
  # encodes Age_Status. Domains with no length sampling are
  # additionally marked with a cross (shape = 4).
  # ggrepel labels show otolith counts for SAFE/WEAK domains
  # and "No ALK" for WARNING domains.
  # ----------------------------------------------------------
  library(ggrepel)
  status_colors <- c(
    "SAFE: Above threshold \nSufficient Age Data" ="#B2DF8A",   # Darker green
   "CRITICAL: No Sampling" = "lightblue",#  "#33A02C",   # Lighter green
    "WARNING: Lengths only (Needs ALK)" =    "#FF7F00",     # Darker orange
   "Other/Check" =  "#1F78B4",      # Lighter orange
   "Ages only \n(No Length Sampling)" = "#FDBF6F",      # Darker blue
    "WEAK: below threshold \n(Need Borrowing)" = "cornflowerblue"          # Lighter blue
  )

  setup_diagnostic<-filter(setup_diagnostic, stock %in%stocks)  
  
  bubble_plot <- ggplot(
    filter(setup_diagnostic, stock %in%stocks),
    aes(x = factor(.data[[grouping_vars[1]]]),
        y= .data[[grouping_vars[2]]])
  ) +
    
    # Main bubbles: size = landings, fill = age status
     geom_point(
     #  data  = filter(setup_diagnostic, !is.na(N_Samples)),
       data = filter(setup_diagnostic, stock %in%stocks,
                     !is.na(N_Samples) | (!is.na(N_Ages)
                                                 & is.na(N_Samples))),
       aes(size = Total_Landings, fill = Age_Status),
       shape = 21, color = "black", stroke = 0.5,
       position = position_jitter(width = 0.1, height = 0)
    
      ) +
    geom_point(
      #  data  = filter(setup_diagnostic, !is.na(N_Samples)),
      data = filter(setup_diagnostic, stock %in%stocks,
                    is.na(N_Samples) & (!is.na(N_Ages)
                      & is.na(Total_Landings ))),
      aes(size = 0.5, fill = Age_Status),
      shape = 21, color = "black", stroke = 0.5,
      position = position_jitter(width = 0.1, height = 0)
      
    ) +
    
     #Cross overlay for domains with no length sampling
      geom_point(
     data  = filter(setup_diagnostic, stock %in%stocks,
                    is.na(N_Ages) & is.na(N_Samples)  ),
     aes(x    = factor(.data[[grouping_vars[1]]]),
        y= .data[[grouping_vars[2]]],
       shape = Age_Status),
    color = "red",  size = 1.5, stroke = 1
    ) +
    facet_grid(stock~area, scales= "free_y")   +
        scale_size_continuous(range = c(4, 12)) +
        scale_fill_manual(values = status_colors, name = "Status",
            guide = guide_legend(override.aes = list(size = 8))) +

    scale_shape_manual(
      values = c("CRITICAL: No Sampling" = 4,
                 "WARNING: Lengths only \n(Needs ALK)" = 21,
                 "Ages only \n(No Length Sampling)" = 14,
                 "Only Landings" = 21),
      name  = "Age status",
      guide = "none"
    ) +
  
   # scale_size_continuous(breaks = sort(unique(setup_diagnostic$Total_Landings)))+
    geom_text_repel(
      aes(
        label = case_when(
          Age_Status %in% c("Ages only \n(No Length Sampling)",
                            "WEAK: below threshold \n(Need Borrowing)") ~
            paste(N_Ages, "otoliths"),
          Age_Status == "WARNING: Lengths only \n(Needs ALK)" ~ "No ALK",
          TRUE ~ ""
        )
      )   ,
      size               = 3,
      fontface           = "bold",
      force              = 3,
      box.padding        = 0.6,
      point.padding      = 0.4,
      min.segment.length = 0,
      segment.size       = 0.8,
      segment.color      = "grey40",
      lineheight         = 0.8,
      max.overlaps       = Inf
    ) +
  #  theme_bw() +
    labs(
      title   = paste(unique(setup_diagnostic$stock),
                      collapse = " / "),
      subtitle = paste0(
        "Bubble size = Total Landings (t)  |  Label = N otoliths",
        "  (Safe threshold > ", threshold_ages, ")"
      ),
      caption  = "X = Landings but \nNo Length Samplings",
      x        = "Quarter",
      y        = "Métier Group"
    )  +
    theme(
      # Title elements
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11),
      
      # Axis formatting
      axis.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size =10),
      
      # Panel and grid formatting
      panel.grid.major = element_line(color = "grey85"),
      # panel.grid.minor = element_blank(),
      #panel.border = element_rect(color = "grey70"),
      
      # Facet formatting
      strip.text = element_text(size = 12, face = "bold"),
      strip.background = element_rect(fill = "grey95", color = "grey70"),
      
      # No legend needed with facets
      legend.position = "right"
    )
  
  bubble_plot
  
  # ----------------------------------------------------------
  # STEP 5 – HEATMAP
  # One tile per domain; FILL = Age_Status, TEXT = landings
  # (tonnes, rounded). Faceted by stock × area so each species
  # / area combination gets its own panel.
  # ----------------------------------------------------------
  setup_diagnostic<-filter(setup_diagnostic, stock %in%stocks)
  heatmap_plot <- ggplot(  setup_diagnostic,
    aes(x   = factor(.data[[grouping_vars[1]]]),
       y   = .data[[grouping_vars[2]]],
        fill = Age_Status)
  ) +
    geom_tile(color = "black", alpha = 0.8) +
    geom_text(
      aes(label = round(Total_Landings,0)),
      size = 2.4,color="black", fontface = "bold"
    ) +
    #facet_grid( .data[[grouping_vars[3]]]~stock ) +
   # facet_grid(stock~area, scales="free_y")+
    facet_grid(stock ~ area, scales = "free_y", space = "free_y")  +
    scale_fill_manual(values = status_colors, name = "Status",
                      guide = guide_legend(override.aes = list(size = 8)))  +
    labs(
      title   = paste(unique(setup_diagnostic$stock),
                      collapse = " / "),
      subtitle = "Age Diagnostic — Heatmap",
      x       = "Quarter",
      y       = "Métier",
      caption = "Label = Landings (t)"
    ) +
    
    scale_x_discrete(expand = expansion(0)) +             # ← elimina márgenes laterales
    scale_y_discrete(expand = expansion(0)) +

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
    
  heatmap_plot
  

  
dist_ready <- distributions %>%
    filter(variableType == "Number", !grepl("27.8.b", domainBiology)) %>%
    distinct() %>%
    distinct() %>% 
    mutate(
      quarter = as.numeric(substring(domainBiology, 1, 1)), 
      area = ifelse(
        stock== "mac.27.nea",
        substring(domainBiology, 3, 8),          # caso WGWIDE
        substring(domainBiology, 3, 10)          # resto (incluye WGHANSA, etc.)
      ),
      metier6 = sub("^[^_]*_[^_]*_", "", domainBiology),  # Remueve "número_área_" y toma el resto como metier
      metier_group= ifelse(grepl("^PS_SPF", metier6), 
                           substring(metier6, 1, 2), 
                           substring(metier6, 1, 3)),
      cod.FAO = toupper(substr(stock, 1, 3))  ,
      match_key = paste(stock, area, quarter, metier_group, sep = "_")
    ) %>%
    rename(length = bvValue) # Yves suele buscar 'Length'
  # 2. Limpieza y preparación de ALK (Edades)
  alk_ready <- ALK %>%

      mutate(
        stock = ifelse(cod.FAO == "MAC", "mac.27.nea", "pil.27.8c9a")
        ,
      match_key = paste(stock, area, quarter, metier_group, sep = "_")
    )
  
  yves_input_data <- dist_ready %>%
    left_join(
      alk_ready %>% select(match_key, length, Age, n, prop)  , 
      by = c("match_key", "length")
    ) %>%
    mutate(
      # Si una talla no tiene edad en ese estrato, Yves necesitará saberlo
      needs_borrowing = is.na(Age) )%>% as.data.frame()
     %>% transmute(
      # 1. Identificadores de estrato (idénticos a la tabla de capturas)
      year = 2024, # O el campo year si lo tienes en SD
      stock = stock,
      speciesCode = speciesCode,
      domainBiology = domainBiology,
      vesselFlagCountry = "ES", # Ajustar si tienes más países
      workingGroup = workingGroup,
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
      data_source = data_source 
    )
  
  # ============================================================
  # CONTROL DE CALIDAD PRE-YVES
  # ============================================================
  n_matches <- sum(!is.na(yves_input_data$Age))
  cat("--- PRE-YVES DIAGNOSTIC ---\n")
  cat("Registros de talla con edad encontrada:", n_matches, "\n")
  cat("Registros de talla sin edad (necesitan Borrowing):", sum(is.na(yves_input_data$Age)), "\n")
  
  
    
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
    quality_check = quality_check,
    bubble_plot  = bubble_plot,
    heatmap_plot = heatmap_plot
  ))
}

age_setup <- check_age_setup(
  census_df       = census,        # landings / catch table
  dist_df         = distributions, # length-distribution samples
  ALK_df          = ALK,   
 # quality_check= quality_check,# Age-Length Key (otolith readings)
  threshold_ages  = 100          # minimum otoliths to flag a domain as "SAFE"
)

age_setup$quality
age_setup$diagnostic

age_setup$diagnostic %>%
  filter(metier_group == "PS", area == "27.8.c")

# Ver los gráficos
age_setup$bubble_plot
age_setup$heatmap_plot
