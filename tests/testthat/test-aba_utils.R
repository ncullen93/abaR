test_that("setting outcomes using tidy selection works as expected", {
  df <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
  my_outcomes <- list(
    'a' = 'CSF_ABETA_bl',
    't' = 'CSF_PTAU_bl',
    'n' = 'CSF_TAU_bl'
  )
  outcomes2 <- list('O1'='CSF_ABETA_bl',
                    'O2'=  'CSF_PTAU_bl',
                    'O3' = 'CSF_TAU_bl')
  outcomes3 <- c('CSF_ABETA_bl',
                 'CSF_PTAU_bl',
                 'CSF_TAU_bl')
  m <- df %>% aba_model() %>% set_outcomes(my_outcomes)
  expect_identical(m$outcomes, my_outcomes)

  m2 <- df %>% aba_model() %>% set_outcomes(outcomes3)
  expect_identical(
    m2$outcomes,
    outcomes2
  )

  m3 <- df %>% aba_model() %>% set_outcomes(CSF_ABETA_bl:CSF_TAU_bl)
  expect_equal(
    m3$outcomes,
    outcomes2
  )

  m4 <- df %>% aba_model() %>%
    set_outcomes(outcomes3, labels=names(my_outcomes))
  expect_equal(
    m4$outcomes,
    my_outcomes
  )

  m5 <- df %>% aba_model() %>% set_outcomes(CSF_ABETA_bl:CSF_TAU_bl,
                                            labels=names(my_outcomes))
  expect_equal(
    m5$outcomes,
    my_outcomes
  )
})


test_that('different ways to set models work', {

  df <- adnimerge %>% dplyr::filter(VISCODE == 'bl')

  # all variables and expressions
  expect_error(
    model <- df %>% aba_model() %>%
      set_groups(everyone()) %>%
      set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
      set_predictors(
        PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
        c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
      ) %>%
      set_covariates(AGE, GENDER, EDUCATION) %>%
      set_stats('glm') %>%
      fit(),
  NA)

  # all strings
  expect_error(
    model2 <- aba_model() %>%
      set_groups('everyone()') %>%
      set_outcomes('ConvertedToAlzheimers', 'CSF_ABETA_STATUS_bl') %>%
      set_predictors(
        'PLASMA_ABETA_bl', 'PLASMA_PTAU181_bl', 'PLASMA_NFL_bl',
        c('PLASMA_ABETA_bl', 'PLASMA_PTAU181_bl', 'PLASMA_NFL_bl')
      ) %>%
      set_stats('glm') %>%
      set_data(df) %>%
      fit(),
  NA)

  # all with labels supplied separately from values
  expect_error(
    model3 <- df %>% aba_model() %>%
      set_groups(everyone(), labels=c('All')) %>%
      set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl,
                   labels = c('AD','ABETA')) %>%
      set_predictors(
        PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
        c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl),
        labels = c('A','T','N','ATN')
      ) %>%
      set_stats('glm', labels='glm1') %>%
      fit(),
  NA)

  # all with lists/vectors input directly into setter functions
  expect_error(
    model4 <- df %>% aba_model() %>%
      set_groups(list('All'='everyone()',
                      'MCI'='DX_bl=="MCI"')) %>%
      set_outcomes(list('AD2'='ConvertedToAlzheimers','ABETA2'='CSF_ABETA_STATUS_bl')) %>%
      set_predictors(
        list('A'='PLASMA_ABETA_bl', 'T'= 'PLASMA_PTAU181_bl', 'N'= 'PLASMA_NFL_bl',
             'ATN'= c('PLASMA_ABETA_bl', 'PLASMA_PTAU181_bl', 'PLASMA_NFL_bl'))
      ) %>%
      set_covariates(c('AGE','GENDER','EDUCATION')) %>%
      set_stats(list('myglm1'= stat_glm(),
                     'myglm2' = stat_glm(std.beta=T))) %>%
      fit(),
  NA)


  # all with lists/vectors defined externally outside of setter functions

  groups <- list('All'='everyone()', 'MCI'='DX_bl=="MCI"')
  outcomes <- list('AD' = 'ConvertedToAlzheimers', 'ABETA' = 'CSF_ABETA_STATUS_bl')
  predictors <- list(
    'A' = 'PLASMA_ABETA_bl',
    'T' = 'PLASMA_PTAU181_bl',
    'N' = 'PLASMA_NFL_bl',
    'ATN' = c('PLASMA_ABETA_bl', 'PLASMA_PTAU181_bl', 'PLASMA_NFL_bl')
  )
  covariates <- c('AGE', 'GENDER', 'EDUCATION')
  stats <- list('glm' = stat_glm())

  expect_error(
    model5 <- df %>% aba_model() %>%
      set_groups(all_of(groups)) %>%
      set_outcomes(all_of(outcomes)) %>%
      set_predictors(all_of(predictors)) %>%
      set_covariates(all_of(covariates)) %>%
      set_stats(stats) %>%
      fit(),
  NA)

  # externally defined lists/vectors passed directly into "aba_model" function
  expect_error(
    model6 <- aba_model(
      data = df,
      groups = all_of(groups),
      outcomes = all_of(outcomes),
      predictors = all_of(predictors),
      covariates = all_of(covariates),
      stats = stats
    ) %>% fit(),
  NA)



})


test_that('set_groups works', {
  m <- aba_model() %>%
    set_data(data.frame(c(1,2,3))) %>%
    set_groups(everyone())
  expect_s3_class(m, 'abaModel')
})

test_that('set_groups from list works', {
  expect_error(
    m <- adnimerge %>% aba_model() %>%
      set_groups(
        list(DX_bl == 'MCI', AGE < 85)
      ),
    NA
  )

  expect_error(
    m <- adnimerge %>% aba_model() %>%
      set_groups(
        list(DX_bl == 'MCI', AGE < 85),
        list(DX_bl == 'CU', GENDER == "1")
      ),
    NA
  )
})


test_that('set_outcomes works', {
  m <- aba_model() %>%
    set_data(data.frame(x=c(1,2,3))) %>%
    set_outcomes(x)
  expect_s3_class(m, 'abaModel')
})

test_that('set_covariates works', {
  m <- aba_model() %>%
    set_data(data.frame(x=c(1,2,3))) %>%
    set_covariates(x)
  expect_s3_class(m, 'abaModel')
})

test_that('set_predictors works', {
  m <- aba_model() %>%
    set_data(data.frame(x=c(1,2,3))) %>%
    set_predictors(x)
  expect_s3_class(m, 'abaModel')
})


test_that("set_covariates with strings", {
  m <- aba_model()
  # string / no data -> should work
  expect_error(
    m2 <- m %>% set_covariates('a','b','c'),
    NA
  )
  # string / data -> should throw error if variable(s) doesnt exist
  #expect_error(
  #  m %>% set_data(adnimerge) %>% set_covariates('a','b','c')
  #)
  # should work if variable(s) all exist
  expect_error(
    m2 <- m %>% set_data(adnimerge) %>% set_covariates('AGE','GENDER','EDUCATION'),
    NA
  )
})


test_that("tidy eval throws error without data set", {
  # set_predictors()
  expect_error(
    aba_model() %>% set_predictors('x','y',z)
  )
  expect_error(
    aba_model() %>% set_predictors('x','y',c(x,y))
  )
  expect_error(
    aba_model() %>% set_predictors('x','y',contains('x'))
  )
  # set_groups()
  expect_error(
    aba_model() %>% set_groups(madeup_variable > 1)
  )
})

test_that("string eval works without data set", {
  # set_predictors()
  expect_error(
    aba_model() %>% set_predictors('x','y','z'),
    NA
  )
  expect_error(
    aba_model() %>% set_predictors('x','y',c('x','y')),
    NA
  )
  # set_groups()
  expect_error(
    aba_model() %>% set_groups('x > 1'),
    NA
  )
})




