

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
    left_join(
      ALK_filled) %>%
    
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
head (distriage_final)
## Ver gráfico de cobertura
distriage_final%>%
  #filter(cod.FAO == "PIL") %>%
  group_by(distributionValue, stock ,  domainBiology,data_source) %>%
  summarise(total_prop = sum(value), .groups = "drop") %>%
  ggplot(aes(x = distributionValue, y = total_prop, fill = data_source)) +
  geom_col() +
  facet_grid(.~stock, scales="free") +
  labs(title = "Cobertura de ALK - PIL",
       x = "Talla (cm)", y = "Proporción total")
