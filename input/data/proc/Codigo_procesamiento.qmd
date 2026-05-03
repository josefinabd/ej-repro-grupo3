# Código de procesamiento 

## Cargar librerías necesarias
library(tidyverse)
library(haven)
library(survey)
library(gt)

## Cargar datos
ENUT_DATA <- readRDS("input/data/original/250403-ii-enut-bdd-r-v2.RDS")

## PASO 2: Identificación de variables participación y tiempo ----

ds_vars_part <- ENUT_DATA %>%
  select(matches("^tc\\d*_p_ds$"), matches("^td\\d*_p_ds$"),
         matches("^tv\\d*_p_ds$")) %>%
  names()

fds_vars_part <- ENUT_DATA %>%
  select(matches("^tc\\d*_p_fds$"), matches("^td\\d*_p_fds$"),
         matches("^tv\\d*_p_fds$")) %>%
  names()

ds_vars_tiempo <- ENUT_DATA %>%
  select(matches("^tc\\d*_t(_n[1-4])?_ds$"),
         matches("^td\\d*_t_ds$"),
         matches("^tv\\d*_t_ds$")) %>%
  names()

fds_vars_tiempo <- ENUT_DATA %>%
  select(matches("^tc\\d*_t(_n[1-4])?_fds$"),
         matches("^td\\d*_t_fds$"),
         matches("^tv\\d*_t_fds$")) %>%
  names()


## PASO 3: Creación de base auxiliar ----

enut_auxiliar <- ENUT_DATA %>%
  select(id_persona, tiempo,
         all_of(ds_vars_part), all_of(fds_vars_part),
         all_of(ds_vars_tiempo), all_of(fds_vars_tiempo))

enut_tnr <- ENUT_DATA %>% select(id_persona)


## PASO 4: Derivar variable de participación ----

enut_auxiliar <- enut_auxiliar %>%
  mutate(
    p_tnr_ds =
      case_when(
        tiempo == 0
        ~ NA_real_,
        across(all_of(ds_vars_part), ~ . == 96) %>%
          rowSums(na.rm = TRUE) == length(ds_vars_part)
        ~ 96,
        rowSums(across(all_of(ds_vars_part), ~ . == 1),
                na.rm = TRUE) > 0
        ~ 1,
        rowSums(across(all_of(ds_vars_part), ~ . == 1),
                na.rm = TRUE) == 0
        ~ 0,
        TRUE ~ NA_real_),
    
    p_tnr_fds =
      case_when(
        tiempo == 0
        ~ NA_real_,
        across(all_of(fds_vars_part), ~ . == 96) %>%
          rowSums(na.rm = TRUE) == length(fds_vars_part)
        ~ 96,
        rowSums(across(all_of(fds_vars_part), ~ . == 1),
                na.rm = TRUE) > 0
        ~ 1,
        rowSums(across(all_of(fds_vars_part), ~ . == 1),
                na.rm = TRUE) == 0
        ~ 0,
        TRUE ~ NA_real_),
    
    p_tnr_dt =
      case_when(
        p_tnr_ds == 1 | p_tnr_fds == 1 ~ 1,
        p_tnr_ds == 96 | p_tnr_fds == 96 ~ 96,
        p_tnr_ds == 0 & p_tnr_fds == 0 ~ 0
      )
  )

rm(ds_vars_part, fds_vars_part)

## PASO 5: Derivación de variable tiempo ----

enut_auxiliar <- enut_auxiliar %>%
  mutate(
    across(
      all_of(ds_vars_tiempo),
      ~ replace(., . == 96 | . == 0, NA),
      .names = "temp_ds_{col}"),
    across(
      all_of(fds_vars_tiempo),
      ~ replace(., . == 96 | . == 0, NA),
      .names = "temp_fds_{col}"))

enut_auxiliar <- enut_auxiliar %>%
  mutate(
    t_tnr_ds = rowSums(across(starts_with("temp_ds_")), na.rm = TRUE),
    t_tnr_ds = if_else(t_tnr_ds == 0, NA_real_, t_tnr_ds),
    t_tnr_fds = rowSums(across(starts_with("temp_fds_")), na.rm = TRUE),
    t_tnr_fds = if_else(t_tnr_fds == 0, NA_real_, t_tnr_fds),
    t_tnr_dt =
      case_when(
        !is.na(t_tnr_ds) & !is.na(t_tnr_fds)
        ~ (t_tnr_ds * 5/7) + (t_tnr_fds * 2/7),
        !is.na(t_tnr_ds) & is.na(t_tnr_fds)
        ~ t_tnr_ds * 5/7,
        is.na(t_tnr_ds) & !is.na(t_tnr_fds)
        ~ t_tnr_fds * 2/7,
        TRUE ~ NA_real_
      )
  )

rm(ds_vars_tiempo, fds_vars_tiempo)


## PASO 6: Unir variables generadas a la base principal ----

enut_auxiliar <- enut_auxiliar %>%
  select(id_persona, matches("^[pt]_tnr_(ds|fds|dt)$"))

enut_tnr <- left_join(x = enut_tnr, y = enut_auxiliar,
                      by = "id_persona")
rm(enut_auxiliar)

## Eliminar columnas tnr previas si existen (evita duplicados al re-ejecutar)
ENUT_DATA <- ENUT_DATA %>% select(-matches("tnr_(ds|fds|dt)"))
ENUT_DATA <- ENUT_DATA %>% left_join(enut_tnr, by = "id_persona")
rm(enut_tnr)


## PASO 7: Diseño muestral complejo ----

enut1 <- ENUT_DATA %>%
  filter(!is.na(fe_cut))

disenio <- survey::svydesign(data = enut1, strata = ~varstrat,
                             ids = ~varunit, weights = ~fe_cut)


## PASO 8: Estimaciones poblacionales ----

part_tnr_pdn  <- survey::svyby(formula = ~p_tnr_dt,
                               by = ~sexo + pdn, design = disenio,
                               FUN = survey::svymean,
                               na.rm = TRUE, vartype = c("se", "cv"))

tiemp_tnr_pdn <- survey::svyby(formula = ~t_tnr_dt,
                               by = ~sexo + pdn, design = disenio,
                               FUN = survey::svymean,
                               na.rm = TRUE, vartype = c("se", "cv"))

n_part <- survey::svyby(formula = ~I(p_tnr_dt == 1),
                        by = ~sexo + pdn, design = disenio,
                        FUN = survey::svytotal, na.rm = TRUE)

part_total  <- survey::svyby(formula = ~p_tnr_dt,
                             by = ~pdn, design = disenio,
                             FUN = survey::svymean, na.rm = TRUE)
tiemp_total <- survey::svyby(formula = ~t_tnr_dt,
                             by = ~pdn, design = disenio,
                             FUN = survey::svymean, na.rm = TRUE)
n_nac       <- survey::svyby(formula = ~I(p_tnr_dt == 1),
                             by = ~pdn, design = disenio,
                             FUN = survey::svytotal, na.rm = TRUE)

part_sexo  <- survey::svyby(formula = ~p_tnr_dt,
                            by = ~sexo, design = disenio,
                            FUN = survey::svymean, na.rm = TRUE)
tiemp_sexo <- survey::svyby(formula = ~t_tnr_dt,
                            by = ~sexo, design = disenio,
                            FUN = survey::svymean, na.rm = TRUE)

part_gral  <- survey::svymean(~p_tnr_dt, design = disenio, na.rm = TRUE)
tiemp_gral <- survey::svymean(~t_tnr_dt, design = disenio, na.rm = TRUE)
n_gral_val <- coef(survey::svytotal(~I(p_tnr_dt == 1),
                                    design = disenio,
                                    na.rm = TRUE))["I(p_tnr_dt == 1)TRUE"]

print(part_tnr_pdn)
print(tiemp_tnr_pdn)



