# Hospital R Project — Step-by-step (for research-intern interview)
# Purpose: a complete, runnable R script that you can use to practice data cleaning,
# EDA, statistics, predictive modeling, survival analysis, reporting, and a Shiny
# dashboard — using a synthetic hospital admissions dataset.
# Run in RStudio. Run sections interactively. Keep answers short and clear.

#### 0. Prerequisites --------------------------------------------------------
# R (>=4.0) and these packages. The script installs missing packages.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, data.table, lubridate, janitor, broom,
  tableone, finalfit, survminer, survival, caret, glmnet,
  randomForest, pROC, factoextra, cluster, broom.mixed,
  knitr, rmarkdown, shiny, DT, gt, flextable, renv, pwr, mice
)

# Set random seed for reproducibility
set.seed(123)

#### 1. Project structure ---------------------------------------------------
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

#### 2. Generate a synthetic raw dataset (raw + messy) ----------------------
# This creates a realistic 'raw' CSV you can practice cleaning on.
n_adm <- 2500
n_pat <- 1500

admission_dates <- sample(seq.Date(from = Sys.Date() - 365*2,
                                   to = Sys.Date(), by = "day"),
                          size = n_adm, replace = TRUE)

df_raw <- tibble(
  admission_id = 1:n_adm,
  patient_id = sample(1:n_pat, n_adm, replace = TRUE),
  age = sample(18:95, n_adm, replace = TRUE),
  sex = sample(c("Male","Female","M","F","male","female","f"), n_adm, replace = TRUE,
               prob = c(0.45,0.45,0.02,0.02,0.03,0.02,0.01)),
  admission_date = admission_dates,
  discharge_date = admission_dates + sample(0:30, n_adm, replace = TRUE),
  diagnosis_code = sample(c("I10","E11","J18","N39","K29","R07","z00"), n_adm, replace = TRUE),
  comorbidity_score = rpois(n_adm, lambda = 1.8),
  heart_rate = round(rnorm(n_adm, mean = 82, sd = 15)),
  bp_systolic = round(rnorm(n_adm, 128, 18)),
  creatinine = round(rlnorm(n_adm, meanlog = log(0.9), sdlog = 0.3),2),
  wbc = round(rnorm(n_adm, mean = 8.0, sd = 3),1),
  icu_admission = rbinom(n_adm, 1, prob = 0.08)
)

# Introduce some outcomes deterministically + noise
# Longer stay, higher comorbidity increase death/readmission risk
los_days <- as.numeric(df_raw$discharge_date - df_raw$admission_date) + 1
logit_readmit <- -3 + 0.02*df_raw$age + 0.4*df_raw$comorbidity_score + 0.06*los_days + 0.9*df_raw$icu_admission
prob_readmit <- plogis(logit_readmit)
df_raw$readmitted_30 <- rbinom(n_adm, 1, prob_readmit)

logit_death <- -5 + 0.03*df_raw$age + 0.6*df_raw$comorbidity_score + 0.12*los_days + 1.2*df_raw$icu_admission
prob_death <- plogis(logit_death)
df_raw$in_hospital_death <- rbinom(n_adm, 1, prob_death)

# Add random missingness
set.seed(44)
na_idx <- sample(1:n_adm, size = round(0.06*n_adm))
df_raw$wbc[na_idx] <- NA
na_idx2 <- sample(1:n_adm, size = round(0.03*n_adm))
df_raw$creatinine[na_idx2] <- NA

# Mess up some diagnosis strings and add trailing spaces
df_raw$diagnosis_code <- ifelse(runif(n_adm) < 0.05,
                                paste0(df_raw$diagnosis_code, " "),
                                df_raw$diagnosis_code)

# Add a few duplicate rows to mimic messy export
dups <- df_raw[sample(1:nrow(df_raw), 15), ]
df_raw <- bind_rows(df_raw, dups)

# Write raw CSV for practice
write_csv(df_raw, file = "D:/MERCYBUNDI/OneDrive/Desktop/data science with r/R Projects (Practice)/Hospital R/Hospital_R_admissions_raw.csv")