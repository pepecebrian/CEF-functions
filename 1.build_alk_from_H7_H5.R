
# ------------------------------------------------------------------------------
# build_alk_from_H7_H5()
#
# This function builds a combined ALK (Age-Length Key) from two RDBES datasets:
#   • H7 (IEO)
#   • H5 (AZTI)
#
# It performs:
#   1. Cleaning H7 using the real join structure
#   2. Cleaning H5 using the real join structure
#   3. Pivoting both datasets to wide format (Length + Age)
#   4. Standardizing lengths (0.5 cm for PIL/ANE, 1 cm for others)
#   5. Harmonizing areas (FMU)
#   6. Computing otolith and sample counts
#   7. Binding H7 + H5
#   8. Building the final ALK grouped by area × species × quarter × length × age
#
# Returns:
#   A list with:
#     • H7_wide
#     • H5_wide
#     • combined_wide
#     • ALK_grouped
# ------------------------------------------------------------------------------

build_alk_from_H7_H5 <- function(
    path_H7= 'H7_2024_ALL.zip',
    path_H5= 'H5_AZTI.zip',
    species = c("PIL", "MAC"),
    round_half_species = c("PIL", "ANE")
) {
  
  # --------------------------------------------------------------------------
  # 1. Load RDBES objects
  # --------------------------------------------------------------------------
  H7 <- createRDBESDataObject(input = path_H7)
  H5 <- createRDBESDataObject(input = path_H5)
  
  validateRDBESDataObject(H7, verbose = TRUE)
  validateRDBESDataObject(H5, verbose = TRUE)
  
  # Filter species in SA
  H7$SA <- H7$SA %>% filter(SAspeCodeFAO %in% species)
  H5$SA <- H5$SA %>% filter(SAspeCodeFAO %in% species)
  
  # --------------------------------------------------------------------------
  # 2. CLEAN H7 (using your real join structure)
  # --------------------------------------------------------------------------
  clean_H7 <- function(H7) {
    
    DE_H7 <- H7$DE %>%
      mutate(year = DEyear) %>%
      filter(grepl("PIL|MAC", DEstratumName)) %>%
      select(DEid, DEhierarchy, year, DEstratumName)
    
    SD_H7 <- H7$SD %>% select(SDid, DEid, SDinst)
    
    OS_H7 <- H7$OS %>%
      mutate(locode = OSlocode,
             date = lubridate::date(OSsamDate),
             quarter= quarter(date)) %>%
      select(OSid, SDid, locode, date,quarter) %>%
      distinct()
    
    SS_H7 <- H7$SS %>%
      select(SSid, SLid, OSid, SSspecListName) %>%
      distinct()
    
    LE_H7 <- H7$LE %>%
      mutate(date = lubridate::date(LEdate),
             quarter = quarter(date),
             year = year(date)) %>%
      select(year, quarter, LEid, VDid, SSid, date,
             vessel = LEencrVessCode,
             LEmetier5, LEmetier6, metier_group=LEgear,
             area = LEarea,
             locode = LElocode) %>%
      distinct()
    
    SA_H7 <- H7$SA %>%
      mutate(cod.FAO = SAspeCodeFAO) %>%
      select(SAid, SSid, SAspeCode, cod.FAO,
             CatchCategory = SAcatchCat,
             SAcommCat, SAcommCatScl)
    
    BV_H7 <- H7$BV %>%
      filter(BVvalUnitScale %in% c("Lengthmm", "Ageyear")) %>%
      select(BVid, SAid, BVfishId, BVvalueMeas, BVvalUnitScale)
    
    H7_clean <- DE_H7 %>%
      left_join(SD_H7, by = "DEid") %>%
      left_join(OS_H7, by = "SDid") %>%
      left_join(SS_H7, by = "OSid") %>%
      left_join(LE_H7, by = c("year", "locode", "date", "SSid", "quarter")) %>%
      left_join(SA_H7, by = "SSid") %>%
      left_join(BV_H7, by = "SAid") %>%
      select(where(~ !(all(is.na(.)) || (is.character(.) && all(. == "")))))
    
    return(H7_clean)
  }
  
  # --------------------------------------------------------------------------
  # 3. CLEAN H5 (using your real join structure)
  # --------------------------------------------------------------------------
  clean_H5 <- function(H5) {
    
    DE_H5 <- H5$DE %>%
      select(DEid, DEhierarchy, year = DEyear, DEstratumName) %>%
      filter(grepl("PIL|MAC", DEstratumName))
    
    SD_H5 <- H5$SD %>% select(SDid, DEid, SDinst)
    
    OS_H5 <- H5$OS %>%
      mutate(locode = OSlocode,
             date = lubridate::date(OSsamDate)) %>%
      select(OSid, SDid, locode, date) %>%
      distinct()
    
    LE_H5 <- H5$LE %>%
      mutate(date = lubridate::date(LEdate),
             quarter = quarter(date),
             year = year(date)) %>%
      select(LEid, VDid,  date, quarter, year,
             barco = LEencrVessCode,
             LEmetier5, LEmetier6, metier_group=LEgear,
             area = LEarea,
             locode = LElocode) %>%
      distinct()
    
    SS_H5 <- H5$SS %>%
      filter(SSspecListName == "AZTI - Market Bio Sampling") %>%
      select(SSid, LEid, SLid, SSspecListName)
    
    SA_H5 <- H5$SA %>%
      mutate(cod.FAO = SAspeCodeFAO) %>%
      select(SAid, SSid, SAspeCode, cod.FAO,
             CatchCategory = SAcatchCat,
             SAcommCat, SAcommCatScl)
    
    BV_H5 <- H5$BV %>%
      filter(BVvalUnitScale %in% c("Lengthmm", "Ageyear")) %>%
      select(BVid, SAid, BVfishId, BVvalueMeas, BVvalUnitScale)
    
    VD_H5 <- H5$VD %>% select(VDid, vessel = VDencrVessCode)
    
    H5_clean <- DE_H5 %>%
      left_join(SD_H5, by = "DEid") %>%
      left_join(OS_H5, by = "SDid") %>%
      left_join(LE_H5, by = c("year", "locode", "date"),
                relationship = "many-to-many") %>%
      left_join(SS_H5, by = "LEid") %>%
      left_join(SA_H5) %>%
      left_join(BV_H5, by = "SAid") %>%
      left_join(VD_H5, by = "VDid") %>%
      filter(area == "27.8.c", !is.na(SAcommCatScl)) %>%
      select(where(~ !(all(is.na(.)) || (is.character(.) && all(. == "")))))
    
    return(H5_clean)
  }
  
  # --------------------------------------------------------------------------
  # 4. Apply cleaners
  # --------------------------------------------------------------------------
  H7_clean <- clean_H7(H7)
  H5_clean <- clean_H5(H5)
  
  #--------------------------------------------------------------------------
    # 4.2. Harmonize FishingArea (FMU) for H7 and H5
    # --------------------------------------------------------------------------
  
  # --- H7: derive FMU from DEstratumName ---
  H7_clean <- H7_clean %>%
    mutate(
      FMU = substring(DEstratumName, 12, 24),
      area = FMU
    ) %>% select(-FMU) %>% as.data.frame()
  
  # --- H5: convert 27.8.c → 27.8.c.e ---
  H5_clean <- H5_clean %>%
    mutate(
      FMU = ifelse(area == "27.8.c", "27.8.c.e", area),
      area = FMU
    ) %>% select(-FMU) %>% as.data.frame()
  # --------------------------------------------------------------------------
  # 5. Pivot to wide (Length + Age)
  # --------------------------------------------------------------------------
  pivot_to_wide <- function(df) {
    df %>%
      filter(BVvalUnitScale %in% c("Lengthmm", "Ageyear")) %>%
      select(-BVid) %>%
  
      pivot_wider(
        names_from = BVvalUnitScale,
        values_from = BVvalueMeas
      ) %>%distinct() %>% 
      mutate(
        Lengthmm = as.numeric(Lengthmm),
        Age = as.numeric(Ageyear),
        length = if_else(
          cod.FAO %in% round_half_species,
          round((Lengthmm / 10) * 2) / 2,
          round(Lengthmm / 10)
        )
      ) %>%
      select(-Lengthmm, -Ageyear)
  }
  
  H7_wide <- pivot_to_wide(H7_clean) %>% as.data.frame()
  H5_wide <- pivot_to_wide(H5_clean)%>% as.data.frame()
  
  #
  
  
  # --------------------------------------------------------------------------
  # 6. Compute otoliths and samples
  # --------------------------------------------------------------------------
  add_counts <- function(df) {
    df %>%
      group_by(area, quarter,metier_group, cod.FAO) %>%
      mutate(
        num_otholits = n_distinct(BVfishId),
        num_samples = n_distinct(SAid)
      ) %>%
      filter(!is.na(Age)) %>%
      ungroup()
  }
  
  H7_wide <- add_counts(H7_wide) %>% as.data.frame()
  H5_wide <- add_counts(H5_wide)%>% as.data.frame()
  
  # --------------------------------------------------------------------------
  # 7. Bind H7 + H5
  # --------------------------------------------------------------------------
  combined <- plyr::rbind.fill(H7_wide, H5_wide)
  
  # --------------------------------------------------------------------------
  # 8. Build ALK grouped
  # --------------------------------------------------------------------------
  dominio_totales <- combined %>%
    filter(!is.na(Age), !is.na(length)) %>%
    group_by(area, cod.FAO, quarter,metier_group, SAcommCatScl) %>%
    summarise(
      num_samples_lab = max(num_samples, na.rm = TRUE),
      otholits_lab = max(num_otholits, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(area, cod.FAO, quarter,metier_group) %>%
    summarise(
      num_samples = sum(num_samples_lab, na.rm = TRUE),
      otholits = sum(otholits_lab, na.rm = TRUE),
      .groups = "drop"
    )
  
  ALK_grouped <- combined %>%
    filter(!is.na(Age), !is.na(length)) %>%
    group_by(area, cod.FAO, quarter,metier_group, length, Age) %>%
    summarise(n = n(), .groups = "drop") %>%
    left_join(dominio_totales, by = c("area", "cod.FAO", "quarter", "metier_group")) %>%
    group_by(area, cod.FAO, metier_group,quarter, length) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
  
  # --------------------------------------------------------------------------
  # 9. Return everything
  # --------------------------------------------------------------------------
  return(list(
    H7_wide = H7_wide,
    H5_wide = H5_wide,
    combined_wide = combined,
    ALK_grouped = ALK_grouped
  ))
}


result <- build_alk_from_H7_H5(
  path_H7 = "H7_2024_ALL.zip",
  path_H5 = "H5_AZTI.zip"
)

ALK_funcion <- result$ALK_grouped

getwd()
fwrite(ALK_funcion, "ALK_PIL_MAC.csv")
headtail(H7_wide)
H5_wide %>% filter(area=='27.8.c.e' , quarter==1 & cod.FAO=="MAC") %>% 
  group_by(area, quarter, cod.FAO, SAcommCatScl) %>% 
  summarise(otolitos= n_distinct(BVfishId),
            muestreos = n_distinct(SAid))
  summarise(otolitos= )