## code to prepare `adnimerge` dataset goes here

library(tidyverse)

# read data
adnimerge <- read.csv('/Users/ni5875cu/Desktop/Biofinder/_random/ADNIMERGE.csv')
adnimerge <- adnimerge %>% dplyr::tibble()

## select variables of interest ##
# csf: ABETA_bl, PTAU_bl, TAU_bl
# pet: AV45_bl
# cognition: CDRSB, CDRSB_bl, ADAS13, ADAS13_bl, MMSE, MMSE_bl
# mri: Hippocampus_bl
# demo: AGE, PTGENDER, PTEDUCAT, APOE4
# subj: RID, VISCODE, Years_bl, DX_bl
adnimerge <- adnimerge %>%
  select(
    RID, VISCODE, Years_bl, DX_bl,
    AGE, PTGENDER, PTEDUCAT, APOE4,
    CDRSB, CDRSB_bl, ADAS13, ADAS13_bl, MMSE, MMSE_bl,
    ABETA_bl, PTAU_bl, TAU_bl,
    Hippocampus_bl,
    AV45_bl
  )

## process CSF data ##
adnimerge <- adnimerge %>%
  mutate(
    across(
      c(ABETA_bl, PTAU_bl, TAU_bl),
      ~stringr::str_replace_all(., c('<'='','>'='')) %>% as.numeric()
    )
  )

# filter
adnimerge <- adnimerge %>%
  filter(complete.cases(.)) %>%
  arrange(RID, Years_bl) %>%
  mutate(
    DX_bl = fct_recode(
      DX_bl,
      'CU' = 'CN',
      'CU' = 'SMC',
      'MCI' = 'EMCI',
      'MCI' = 'LMCI'
    )
  ) %>%
  mutate(
    PET_ABETA_STATUS_bl = as.integer(AV45_bl > 1.11),
    APOE4 = as.integer(APOE4 >= 1),
    PTGENDER = as.integer(PTGENDER == 'Male')
  ) %>%
  rename(
    PET_ABETA_bl = AV45_bl,
    CSF_ABETA_bl = ABETA_bl,
    CSF_PTAU_bl = PTAU_bl,
    CSF_TAU_bl = TAU_bl,
    GENDER = PTGENDER,
    EDUCATION = PTEDUCAT,
    YEARS_bl = Years_bl,
    MRI_HIPP_bl = Hippocampus_bl
  )

usethis::use_data(adnimerge, overwrite = TRUE)
