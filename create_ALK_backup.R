



library(dplyr)
library(tidyr)

## =========================================================================
## FUNCIÓN: Asignar edades usando regla biológica
## =========================================================================

create_ALK_backup <- function(ALK, 
                                inc_pil = 0.5, 
                                inc_default = 1,
                                length_min = 10,
                                length_max = 30) {
  
  cat("\n=== Asignación de edades ===\n\n")
  
  ## PASO 1: Resumir datos reales
  alk_real <- ALK%>%
    group_by(cod.FAO, length, Age) %>%
    summarise(n_global = sum(n), .groups = "drop")
  
  cat(sprintf("Datos reales: %d combinaciones (talla × edad)\n", nrow(alk_real)))
  
  ## PASO 2: Identificar tallas con y sin datos por especie
  coverage_by_length <- alk_real %>%
    group_by(cod.FAO, length) %>%
    summarise(
      has_data = TRUE,
      ages_present = list(sort(unique(Age))),
      min_age = min(Age),
      max_age = max(Age),
      total_n = sum(n_global),
      .groups = "drop"
    )
  
  ## PASO 3: Para cada especie, crear grid completo
  alk_complete <- alk_real %>%
    group_by(cod.FAO) %>%
    group_modify(~ {
      
      species <- .y$cod.FAO
      inc <- if (species == "PIL") inc_pil else inc_default
      
      ## Rango completo de tallas y edades
      all_lengths <- seq(length_min, length_max, by = inc)
      all_ages <- sort(unique(.x$Age))
      
      ## Grid completo
      full_grid <- expand_grid(
        length = all_lengths,
        Age = all_ages
      )
      
      ## Unir con datos reales
      result <- full_grid %>%
        left_join(.x, by = c("length", "Age"))
      
      ## Info de cobertura
      coverage <- coverage_by_length %>% filter(cod.FAO == species)
      
      result %>%
        left_join(
          coverage %>% select(length, has_data, min_age, max_age),
          by = "length"
        ) %>%
        mutate(has_data = replace_na(has_data, FALSE))
      
    }) %>%
    ungroup()
  
  cat(sprintf("Grid completo: %d combinaciones (talla × edad)\n\n", 
              nrow(alk_complete)))
  
  ## PASO 4: Asignar edades donde faltan usando lógica biológica
  cat("Asignando edades faltantes...\n")
  
  alk_filled <- alk_complete %>%
    group_by(cod.FAO) %>%
    mutate(
      ## Identificar rangos con datos reales
      min_length_with_data = min(length[has_data], na.rm = TRUE),
      max_length_with_data = max(length[has_data], na.rm = TRUE)
    ) %>%
    group_by(cod.FAO, length) %>%
    mutate(
      
      ## CASO 1: Hay datos reales → mantener
      prop_g = if (any(!is.na(n_global))) {
        n_global
      } 
      
      ## CASO 2: Talla MUY PEQUEÑA (menor que el mínimo con datos)
      else if (first(length) < first(min_length_with_data)) {
        case_when(
          Age == 0 ~ 100,  # 70% edad 0
          Age == 1 ~ 30,   # 30% edad 1
          TRUE ~ 0
        )
      }
      
      ## CASO 3: Talla MUY GRANDE (mayor que el máximo con datos)
      else if (first(length) > first(max_length_with_data)) {
        max_age_observed <- max(Age)
        case_when(
          Age >= (max_age_observed - 1) ~ 50,  # 50% para las 2 edades mayores
          Age == (max_age_observed - 2) ~ 30,  # 30% para edad -2
          TRUE ~ 0
        )
      }
      
      ## CASO 4: Talla INTERMEDIA sin datos → buscar talla cercana con datos
      else {
        ## Buscar la talla más cercana que SÍ tiene datos
        lengths_with_data <- unique(length[has_data])
        
        if (length(lengths_with_data) > 0) {
          nearest_length <- lengths_with_data[which.min(abs(lengths_with_data - first(length)))]
          
          ## Copiar distribución de esa talla
          ref_dist <- cur_data() %>%
            filter(length == nearest_length, !is.na(n_global))
          
          if (nrow(ref_dist) > 0) {
            match_age <- ref_dist %>% filter(Age == first(Age))
            if (nrow(match_age) > 0) match_age$n_global[1] else 0
          } else {
            0
          }
        } else {
          0
        }
      }
    ) %>%
    
    ## Normalizar proporción por talla
    mutate(
      prop_g = replace_na(prop_g, 0),
      prop_g = if (sum(prop_g) > 0) prop_g / sum(prop_g) else 0,
      
      ## Marcar origen del dato
      data_source = case_when(
        !is.na(n_global) ~ "Real",
        length < first(min_length_with_data) ~ "Extrapolated (small)",
        length > first(max_length_with_data) ~ "Extrapolated (large)",
        TRUE ~ "Interpolated"
      )
    ) %>%
    ungroup() %>%
    filter(prop_g>0) %>% 
    select(cod.FAO, length, Age, n_global, prop_g, data_source) %>%
    rename(Age_back = Age)
  
  ## PASO 5: Resumen
  summary_stats <- alk_filled %>%
    group_by(cod.FAO, data_source) %>%
    summarise(
      n_combinations = n(),
      n_lengths = n_distinct(length),
      .groups = "drop"
    )
  
  cat("\nResumen por origen de datos:\n")
  print(summary_stats)
  cat("\n")
  
  return(alk_filled)
}


## =========================================================================
## USO
## =========================================================================

ALK_filled <- create_ALK_backup (
  ALK = ALK,
  inc_pil = 0.5,
  inc_default = 1,
  length_min = 10,
  length_max = 50
)

## Verificar talla 10 (PIL)
ALK_filled %>%
  filter(cod.FAO == "PIL", length == 10.5) %>%
  arrange(desc(prop_g))

## Ver distribución
ALK_filled %>%
  filter(cod.FAO == "PIL", length == 10) %>%
  ggplot(aes(x = Age_back, y = prop_g, fill = data_source)) +
  geom_col() +
  labs(title = "Talla 10 cm - Distribución de edades",
       subtitle = "PIL: Asignación con lógica biológica")


## =========================================================================
## VERIFICAR COBERTURA
## =========================================================================

coverage_summary <- ALK_filled %>%
  group_by(cod.FAO, data_source) %>%
  summarise(
    n_lengths = n_distinct(length),
    length_range = sprintf("%g - %g", min(length), max(length)),
    .groups = "drop"
  )

print(coverage_summary)

## Ver gráfico de cobertura
ALK_filled%>%
  #filter(cod.FAO == "PIL") %>%
  group_by(length,cod.FAO, data_source) %>%
  summarise(total_prop = sum(prop_g), .groups = "drop") %>%
  ggplot(aes(x = length, y = total_prop, fill = data_source)) +
  geom_col() +
  facet_wrap(~cod.FAO, scales="free") +
  labs(title = "Cobertura de ALK - PIL",
       x = "Talla (cm)", y = "Proporción total")

ALK_filled %>%
  group_by(cod.FAO, length) %>%
  # 1) quedarnos con las 2 edades con mayor prop_g
  slice_max(prop_g, n = 2, with_ties = FALSE) %>%
  filter(prop_g>0 , cod.FAO=="PIL") %>% 
  # 2) renormalizar
  mutate(
    prop_g = prop_g / sum(prop_g, na.rm = TRUE)
  ) %>%
  ungroup()
