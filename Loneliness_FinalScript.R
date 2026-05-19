# GBM Loneliness Analysis
#
# Inputs (place these files in INPUT_DIR; see Configuration below):
#   - GBM_Survey.csv      Raw survey data, N = 525
#   - NCI_Data.csv        Companion file with NCI_Center indicator per record
#
# Outputs (written to OUTPUT_DIR):
#   - Table1_descriptives_by_role.csv
#   - Table2_role_associations.csv      (2-group and 3-group; unadjusted and
#                                         fully adjusted models)
#   - Table3_primary_model.csv          (full primary multivariable model)
#   - Table4_sensitivity.csv            (5 pre-specified sensitivity analyses
#                                         plus a primary-model reference row;
#                                         maps to Table S1 in the manuscript)
#   - Figure1_loneliness_by_role.pdf / .png   (publication-ready, 2 panels)
#   - Figure2_forest_plot.pdf / .png          (publication-ready)
#   - final_analytic_data.csv
#
# Notes
#   * Figures 1 and 2 are generated end-to-end by this script (sections 9-10).
#     PDFs are written with the cairo device when available so that Unicode
#     glyphs (e.g. the "greater-than-or-equal" sign) render and fonts embed.
#   * Financial hardship is REVERSE-coded relative to the raw survey field
#     (see section 2). The script prints a raw-vs-constructed cross-tab so the
#     coding can be verified against the survey codebook before interpretation.
#   * The ~27% caregiver-of-older-adults norm reference line has been removed
#     from Figure 1 per author decision.


# ---------- Configuration ----------
# Folder containing the two input CSVs. Set to "." if they are in the working
# directory; this default keeps the script portable for the public repository.
INPUT_DIR  <- "/Users/jellen/dropbox/MedicalSchool/Projects/GBM_Survey_Analysis/"
OUTPUT_DIR <- "/Users/jellen/dropbox/Loneliness_GBM/Final/"         # Edit if outputs should go elsewhere

# ---------- Libraries ----------
required_pkgs <- c("dplyr", "tidyr", "readr", "broom",
                    "ggplot2", "scales", "patchwork")
for (p in required_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}
suppressMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(broom)
  library(ggplot2)
  library(scales)
  library(patchwork)
})

# Use the cairo PDF device when available (Unicode glyphs + embedded fonts);
# fall back to the base device otherwise.
pdf_device <- if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else "pdf"

# Reproducibility
set.seed(42)

# =============================================================================
# 1. DATA LOAD AND MERGE
# =============================================================================
cat("\n========================================================================\n")
cat("1. LOADING DATA\n")
cat("========================================================================\n")

raw <- read_csv(file.path(INPUT_DIR, "GBM_Survey.csv"),
                show_col_types = FALSE, locale = locale(encoding = "latin1"))
nci <- read_csv(file.path(INPUT_DIR, "NCI_Data.csv"),
                show_col_types = FALSE, locale = locale(encoding = "latin1"))

df <- raw %>% left_join(nci %>% select(record, NCI_Center), by = "record")
cat(sprintf("  Merged dataset: N = %d rows, %d columns\n", nrow(df), ncol(df)))

# =============================================================================
# 2. CONSTRUCT KEY VARIABLES
# =============================================================================
cat("\n========================================================================\n")
cat("2. CONSTRUCTING ANALYTIC VARIABLES\n")
cat("========================================================================\n")

# --- UCLA-3 score (sum of 3 items) and binary loneliness indicator ---
ucla_map <- c("Hardly ever" = 1, "Some of the time" = 2, "Often" = 3)
df <- df %>%
  mutate(
    Companionship_n   = ucla_map[Companionship],
    Left_Out_n        = ucla_map[Left_Out],
    Isolated_Others_n = ucla_map[Isolated_Others],
    UCLA_Score        = Companionship_n + Left_Out_n + Isolated_Others_n,
    Lonely            = as.integer(UCLA_Score >= 6)
  )

# --- Three-group role variable: Patient / Current Caregiver / Bereaved Caregiver ---
df <- df %>%
  mutate(
    Role = case_when(
      CaregiverPatient == "GBM Patient"  ~ "Patient",
      SurvivalStatus   == "Deceased"     ~ "Bereaved Caregiver",
      TRUE                                ~ "Current Caregiver"
    ),
    Role = factor(Role, levels = c("Patient", "Current Caregiver", "Bereaved Caregiver"))
  )

# --- 2-group binary caregiver variable (for reproducing original abstract) ---
df <- df %>%
  mutate(
    Caregiver_binary = factor(
      ifelse(CaregiverPatient == "GBM Patient", "Patient", "Caregiver"),
      levels = c("Patient", "Caregiver")
    )
  )

# --- Race (2-category + 3-category) ---
df <- df %>%
  mutate(
    Race_Category = case_when(
      Race == "White" ~ "White",
      is.na(Race)     ~ NA_character_,
      TRUE            ~ "Nonwhite"
    ),
    Race_Category = factor(Race_Category, levels = c("White", "Nonwhite")),
    Race_3cat = case_when(
      Race == "White"               ~ "White",
      Race == "Hispanic or Latino"  ~ "Hispanic",
      is.na(Race)                   ~ NA_character_,
      TRUE                          ~ "Other Nonwhite"
    ),
    Race_3cat = factor(Race_3cat, levels = c("White", "Hispanic", "Other Nonwhite"))
  )

# --- Urban/rural based on RUCA code ---
df <- df %>%
  mutate(
    Urban_Rural = ifelse(RUCACode <= 3, "Urban", "Rural"),
    Urban_Rural = factor(Urban_Rural, levels = c("Rural", "Urban"))
  )

# --- Financial hardship (REVERSE-coded: raw 0 = trouble, 1 = no trouble) ---
# Financial_Trouble = 1 denotes financial hardship. This depends entirely on the
# raw coding of Trouble_With_Expenses; a verification cross-tab is printed below.
df <- df %>%
  mutate(Financial_Trouble = as.integer(Trouble_With_Expenses == 0))

# --- Education (3-category) ---
edu_less    <- c("Did not complete high school", "High school or G.E.D.")
edu_some    <- c("Some college", "Associate's degree")
edu_college <- c("College graduate", "Post-graduate degree")
df <- df %>%
  mutate(
    Education_Group = case_when(
      Education %in% edu_less    ~ "Less than College",
      Education %in% edu_some    ~ "Some College",
      Education %in% edu_college ~ "College+",
      TRUE                       ~ NA_character_
    ),
    Education_Group = factor(Education_Group,
                             levels = c("College+", "Some College", "Less than College"))
  )

# --- Insurance type (exclude uninsured) ---
df <- df %>%
  mutate(
    Insurance = case_when(
      Uninsured == 1          ~ NA_character_,
      Private_Insurance == 1  ~ "Private",
      TRUE                    ~ "Public"
    ),
    Insurance = factor(Insurance, levels = c("Private", "Public"))
  )

# --- Time symptoms to diagnosis (binary) ---
quick <- c("Less than 1 week", "1 to 2 weeks", "3 to 4 weeks")
slow  <- c("Between 1 and 3 months", "Between 3 and 6 months",
           "Between 6 months and 1 year", "More than 1 year")
df <- df %>%
  mutate(
    Sx_to_Dx = case_when(
      Time_Symptoms_to_Diagnosis %in% quick ~ "Less than 1 month",
      Time_Symptoms_to_Diagnosis %in% slow  ~ "Over 1 month",
      TRUE                                  ~ NA_character_
    ),
    Sx_to_Dx = factor(Sx_to_Dx, levels = c("Less than 1 month", "Over 1 month"))
  )

# --- Years since diagnosis ---
df <- df %>% mutate(Yrs_Since_Dx = pmin(pmax(2024 - Diagnosis_Year, 0), 30))

# --- Distance from hospital (continuous miles) ---
df <- df %>% mutate(Distance_Miles = as.numeric(Miles_From_Hospital))

# --- Gender (exclude prefer-not-to-say) ---
df <- df %>%
  mutate(
    Gender_clean = ifelse(Gender == "Prefer not to say", NA_character_, Gender),
    Gender_clean = factor(Gender_clean, levels = c("Female", "Male"))
  )

# --- NCI center (Yes/No -> 1/0) ---
df <- df %>% mutate(NCI_Center = ifelse(NCI_Center == "Yes", 1L, 0L))

# --- Numeric coercion for binary 0/1 supportive-care variables ---
binary_vars <- c("Mobility", "Falls", "Socialwork", "NoContact",
                 "OncologyTeam", "HospiceTeam")
for (v in binary_vars) df[[v]] <- as.numeric(df[[v]])

# --- Data-integrity checks --------------------------------------------------
# Role must be defined for every record.
if (any(is.na(df$Role))) {
  stop("Role is NA for ", sum(is.na(df$Role)),
       " record(s); check CaregiverPatient / SurvivalStatus.")
}
# Flag any Education value that fell through the case_when() to NA, so that an
# unexpected response option does not silently drop respondents from models.
unmapped_edu <- setdiff(unique(na.omit(df$Education)),
                        c(edu_less, edu_some, edu_college))
if (length(unmapped_edu) > 0) {
  warning("Unmapped Education value(s) set to NA: ",
          paste(unmapped_edu, collapse = "; "))
}
# Verify the financial-hardship reverse-coding against the raw field.
cat("\n  Financial hardship coding check",
    "(raw Trouble_With_Expenses x constructed Financial_Trouble):\n")
print(table(Raw_Trouble_With_Expenses = df$Trouble_With_Expenses,
            Financial_Trouble        = df$Financial_Trouble, useNA = "ifany"))
cat("  Expectation: Financial_Trouble = 1 denotes hardship.",
    "Confirm against the survey codebook.\n")

cat(sprintf("\n  UCLA_Score available for %d / %d respondents\n",
            sum(!is.na(df$UCLA_Score)), nrow(df)))
cat("  Role distribution:\n"); print(table(df$Role, useNA = "ifany"))

# Save the analytic dataset
write_csv(df, file.path(OUTPUT_DIR, "final_analytic_data.csv"))

# =============================================================================
# 3. HELPER FUNCTIONS FOR MODELING AND FIGURES
# =============================================================================
fit_logit <- function(formula, data = df) {
  glm(as.formula(formula), data = data, family = binomial(link = "logit"))
}

fit_linear <- function(formula, data = df) {
  lm(as.formula(formula), data = data)
}

# Tidy logistic output -> exponentiated OR with 95% CI and p-value
tidy_logit <- function(m, term) {
  if (!(term %in% names(coef(m)))) return(list(est = NA, lo = NA, hi = NA, p = NA))
  ci <- suppressMessages(confint.default(m))   # Wald CIs
  list(est = exp(coef(m)[term]),
       lo  = exp(ci[term, 1]),
       hi  = exp(ci[term, 2]),
       p   = summary(m)$coefficients[term, "Pr(>|z|)"])
}

tidy_linear <- function(m, term) {
  if (!(term %in% names(coef(m)))) return(list(est = NA, lo = NA, hi = NA, p = NA))
  ci <- confint(m)
  list(est = coef(m)[term],
       lo  = ci[term, 1],
       hi  = ci[term, 2],
       p   = summary(m)$coefficients[term, "Pr(>|t|)"])
}

format_or <- function(x) {
  if (is.na(x$est)) return("--")
  sprintf("%.2f (%.2f-%.2f)", x$est, x$lo, x$hi)
}
format_beta <- function(x) {
  if (is.na(x$est)) return("--")
  sprintf("%.2f (%.2f to %.2f)", x$est, x$lo, x$hi)
}
format_p <- function(p) {
  if (is.na(p)) return("--")
  if (p < 0.001) "<0.001" else sprintf("%.4g", p)
}

# Shared publication theme for Figures 1 and 2
theme_pub <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(
      plot.title         = element_text(face = "bold", size = base + 1, hjust = 0),
      plot.subtitle      = element_text(size = base - 1, hjust = 0),
      plot.caption       = element_text(size = base - 2.5, hjust = 1,
                                        face = "italic"),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line          = element_line(color = "black", linewidth = 0.4),
      axis.ticks         = element_line(color = "black", linewidth = 0.4),
      axis.text          = element_text(color = "black", size = base - 1),
      axis.title         = element_text(size = base)
    )
}

# =============================================================================
# 4. TABLE 1: DESCRIPTIVES BY ROLE
# =============================================================================
cat("\n========================================================================\n")
cat("4. TABLE 1 - DESCRIPTIVE STATISTICS BY ROLE\n")
cat("========================================================================\n")

t1_rows <- list()

# Helper: continuous variable summary row
cont_row <- function(varname, label) {
  groups <- c("Patient", "Current Caregiver", "Bereaved Caregiver")
  vals <- lapply(groups, function(g) df[[varname]][df$Role == g])
  out <- sapply(vals, function(x) {
    x <- x[!is.na(x)]
    sprintf("%.1f (%.1f)", mean(x), sd(x))
  })
  p <- tryCatch({
    summary(aov(df[[varname]] ~ df$Role))[[1]][["Pr(>F)"]][1]
  }, error = function(e) NA)
  data.frame(Variable = label, Patient = out[1],
             `Current Caregiver` = out[2], `Bereaved Caregiver` = out[3],
             `p-value` = format_p(p), check.names = FALSE)
}

# Helper: categorical variable counts/percent
cat_row <- function(varname, label) {
  ct <- table(df$Role, df[[varname]], useNA = "no")
  totals <- rowSums(ct)
  lvls <- colnames(ct)
  rows <- list()
  p <- tryCatch({
    chisq.test(ct)$p.value
  }, error = function(e) NA)
  rows[[1]] <- data.frame(Variable = label, Patient = "",
                          `Current Caregiver` = "", `Bereaved Caregiver` = "",
                          `p-value` = format_p(p), check.names = FALSE)
  for (l in lvls) {
    counts <- ct[, l]
    pcts   <- 100 * counts / totals
    rows[[length(rows) + 1]] <- data.frame(
      Variable = paste0("  ", l),
      Patient = sprintf("%d (%.1f%%)", counts["Patient"],            pcts["Patient"]),
      `Current Caregiver` = sprintf("%d (%.1f%%)", counts["Current Caregiver"],  pcts["Current Caregiver"]),
      `Bereaved Caregiver` = sprintf("%d (%.1f%%)", counts["Bereaved Caregiver"], pcts["Bereaved Caregiver"]),
      `p-value` = "", check.names = FALSE)
  }
  do.call(rbind, rows)
}

t1_rows[[1]]  <- cont_row("Age",          "Age (years), mean (SD)")
t1_rows[[2]]  <- cont_row("Yrs_Since_Dx", "Years since diagnosis, mean (SD)")
t1_rows[[3]]  <- cont_row("UCLA_Score",   "UCLA-3 score, mean (SD)")
t1_rows[[4]]  <- cat_row("Gender_clean",     "Gender")
t1_rows[[5]]  <- cat_row("Race_Category",    "Race (2-category)")
t1_rows[[6]]  <- cat_row("Race_3cat",        "Race (3-category)")
t1_rows[[7]]  <- cat_row("Urban_Rural",      "Residence")
t1_rows[[8]]  <- cat_row("Education_Group",  "Education")
t1_rows[[9]]  <- cat_row("Insurance",        "Insurance")
t1_rows[[10]] <- cat_row("Financial_Trouble","Financial hardship")
t1_rows[[11]] <- cat_row("NCI_Center",       "NCI Center treatment")
t1_rows[[12]] <- cat_row("Sx_to_Dx",         "Time from symptoms to diagnosis")
t1_rows[[13]] <- cat_row("Mobility",         "Mobility issues")
t1_rows[[14]] <- cat_row("Falls",            "Falls")
t1_rows[[15]] <- cat_row("Socialwork",       "Social work contact")
t1_rows[[16]] <- cat_row("OncologyTeam",     "Oncology team contact")
t1_rows[[17]] <- cat_row("HospiceTeam",      "Hospice team contact")
t1_rows[[18]] <- cat_row("Lonely",           "Lonely (UCLA-3 >= 6)")

table1 <- do.call(rbind, t1_rows)
write_csv(table1, file.path(OUTPUT_DIR, "Table1_descriptives_by_role.csv"))
cat("\nTable 1 saved.\n")
print(table1, row.names = FALSE)

# =============================================================================
# 5. PRIMARY GROUP COMPARISON STATISTICS
# =============================================================================
cat("\n========================================================================\n")
cat("5. PRIMARY GROUP COMPARISONS (loneliness by role)\n")
cat("========================================================================\n")

cat("\nMean (SD) UCLA score by role:\n")
print(df %>% group_by(Role) %>%
        summarise(N = n(),
                  Mean = round(mean(UCLA_Score, na.rm = TRUE), 2),
                  SD   = round(sd(UCLA_Score,   na.rm = TRUE), 2),
                  Median = median(UCLA_Score, na.rm = TRUE)))

cat("\n% Lonely by role:\n")
print(df %>% group_by(Role) %>%
        summarise(N_lonely = sum(Lonely, na.rm = TRUE), N_total = n(),
                  Pct = round(100 * mean(Lonely, na.rm = TRUE), 1)))

# ANOVA on continuous UCLA-3 score
aov_res <- aov(UCLA_Score ~ Role, data = df)
cat("\nOne-way ANOVA on UCLA_Score by Role:\n"); print(summary(aov_res))

# Pairwise comparisons. Keys map each role pair to a short identifier reused by
# the Figure 1 panels.
pairs <- list(c("Patient", "Current Caregiver"),
              c("Patient", "Bereaved Caregiver"),
              c("Current Caregiver", "Bereaved Caregiver"))
pw_keys <- c("Patient vs Current Caregiver"            = "P_vs_C",
             "Patient vs Bereaved Caregiver"           = "P_vs_B",
             "Current Caregiver vs Bereaved Caregiver" = "C_vs_B")

# Pairwise Welch t-tests on the CONTINUOUS UCLA-3 score
pw_cont_p <- list()
cat("\nPairwise Welch t-tests (continuous UCLA-3 score):\n")
for (pr in pairs) {
  key <- paste(pr, collapse = " vs ")
  a <- df$UCLA_Score[df$Role == pr[1]]
  b <- df$UCLA_Score[df$Role == pr[2]]
  tt <- t.test(a, b, var.equal = FALSE)
  pw_cont_p[[ pw_keys[[key]] ]] <- as.numeric(tt$p.value)
  cat(sprintf("  %s: t = %.3f, p = %.4g\n", key, tt$statistic, tt$p.value))
}

# Pairwise chi-square tests on the BINARY loneliness indicator. These are the
# p-values shown on Figure 1A (a prevalence panel); no continuity correction is
# applied, matching the uncorrected overall 3-group test below.
pw_binary_p <- list()
cat("\nPairwise chi-square tests (binary loneliness, UCLA-3 >= 6):\n")
for (pr in pairs) {
  key <- paste(pr, collapse = " vs ")
  sub <- df[df$Role %in% pr, ]
  ct  <- table(factor(sub$Role, levels = pr), sub$Lonely)
  ht  <- suppressWarnings(chisq.test(ct, correct = FALSE))
  pw_binary_p[[ pw_keys[[key]] ]] <- as.numeric(ht$p.value)
  cat(sprintf("  %s: X^2 = %.3f, p = %.4g\n", key, ht$statistic, ht$p.value))
}

# Overall chi-square on binary loneliness (3-group)
chi_res <- chisq.test(table(df$Role, df$Lonely))
cat(sprintf("\nOverall chi-square (Lonely x Role): X^2 = %.3f, p = %.4g\n",
            chi_res$statistic, chi_res$p.value))

# =============================================================================
# 6. TABLE 2: NESTED MODELS - CAREGIVER VS PATIENT (2-group AND 3-group)
# =============================================================================
cat("\n========================================================================\n")
cat("6. TABLE 2 - ROLE ASSOCIATIONS ACROSS NESTED MODELS\n")
cat("========================================================================\n")

t2 <- list()

# ----- 2-group caregiver/patient (matches original abstract) -----
# Table 2 in the manuscript reports only unadjusted and fully adjusted models.
adj_sets <- list(
  list(name = "Unadjusted",                       adj = ""),
  list(name = "Fully adjusted",
       adj  = " + Age + Gender_clean + Urban_Rural + Race_Category + Financial_Trouble + Education_Group + Insurance")
)

for (a in adj_sets) {
  m_log <- fit_logit(paste0("Lonely ~ Caregiver_binary", a$adj))
  m_lin <- fit_linear(paste0("UCLA_Score ~ Caregiver_binary", a$adj))
  log_e <- tidy_logit(m_log,  "Caregiver_binaryCaregiver")
  lin_e <- tidy_linear(m_lin, "Caregiver_binaryCaregiver")
  t2[[length(t2) + 1]] <- data.frame(
    Comparison    = "Caregiver vs Patient (2-group)",
    Model         = a$name,
    N             = nobs(m_log),
    `OR (95% CI)`  = format_or(log_e),
    `p (logistic)` = format_p(log_e$p),
    `Beta (95% CI)` = format_beta(lin_e),
    `p (linear)`    = format_p(lin_e$p),
    check.names    = FALSE
  )
}

# ----- 3-group (Current/Bereaved vs Patient) -----
for (a in adj_sets) {
  m_log <- fit_logit(paste0("Lonely ~ Role", a$adj))
  m_lin <- fit_linear(paste0("UCLA_Score ~ Role", a$adj))
  for (gv in c("RoleCurrent Caregiver", "RoleBereaved Caregiver")) {
    log_e <- tidy_logit(m_log,  gv)
    lin_e <- tidy_linear(m_lin, gv)
    label <- ifelse(grepl("Current",  gv),
                    "Current Caregiver vs Patient",
                    "Bereaved Caregiver vs Patient")
    t2[[length(t2) + 1]] <- data.frame(
      Comparison    = label,
      Model         = a$name,
      N             = nobs(m_log),
      `OR (95% CI)`  = format_or(log_e),
      `p (logistic)` = format_p(log_e$p),
      `Beta (95% CI)` = format_beta(lin_e),
      `p (linear)`    = format_p(lin_e$p),
      check.names    = FALSE
    )
  }
}

table2 <- do.call(rbind, t2)
write_csv(table2, file.path(OUTPUT_DIR, "Table2_role_associations.csv"))
cat("\nTable 2 saved.\n")
print(table2, row.names = FALSE)

# =============================================================================
# 7. TABLE 3: FULL PRIMARY MODEL - ALL COEFFICIENTS
# =============================================================================
cat("\n========================================================================\n")
cat("7. TABLE 3 - PRIMARY MODEL FULL COEFFICIENTS\n")
cat("========================================================================\n")

full_log <- fit_logit(paste0(
  "Lonely ~ Role + Age + Gender_clean + Urban_Rural + Race_Category + ",
  "Financial_Trouble + Education_Group + Insurance"))
full_lin <- fit_linear(paste0(
  "UCLA_Score ~ Role + Age + Gender_clean + Urban_Rural + Race_Category + ",
  "Financial_Trouble + Education_Group + Insurance"))

predictors <- list(
  list(term = "RoleCurrent Caregiver",                 lbl = "Current caregiver (ref: patient)"),
  list(term = "RoleBereaved Caregiver",                lbl = "Bereaved caregiver (ref: patient)"),
  list(term = "Financial_Trouble",                     lbl = "Financial hardship (ref: no hardship)"),
  list(term = "Race_CategoryNonwhite",                 lbl = "Non-White (ref: White)"),
  list(term = "Age",                                    lbl = "Age (per year)"),
  list(term = "Gender_cleanMale",                       lbl = "Male (ref: female)"),
  list(term = "Urban_RuralUrban",                       lbl = "Urban (ref: rural)"),
  list(term = "Education_GroupSome College",            lbl = "Some college (ref: college+)"),
  list(term = "Education_GroupLess than College",       lbl = "< College (ref: college+)"),
  list(term = "InsurancePublic",                        lbl = "Public insurance (ref: private)")
)

t3 <- do.call(rbind, lapply(predictors, function(p) {
  log_e <- tidy_logit(full_log,  p$term)
  lin_e <- tidy_linear(full_lin, p$term)
  data.frame(Predictor = p$lbl,
             `OR (95% CI)`  = format_or(log_e),
             `p (logistic)` = format_p(log_e$p),
             `Beta (95% CI)` = format_beta(lin_e),
             `p (linear)`    = format_p(lin_e$p),
             check.names    = FALSE)
}))

write_csv(t3, file.path(OUTPUT_DIR, "Table3_primary_model.csv"))
cat(sprintf("\nN(logistic) = %d, N(linear) = %d, AIC(logistic) = %.1f, R^2(linear) = %.3f\n",
            nobs(full_log), nobs(full_lin), AIC(full_log), summary(full_lin)$r.squared))
print(t3, row.names = FALSE)

# =============================================================================
# 8. TABLE 4: SENSITIVITY ANALYSES
# =============================================================================
cat("\n========================================================================\n")
cat("8. TABLE 4 - SENSITIVITY ANALYSES\n")
cat("========================================================================\n")

primary_terms <- c("RoleCurrent Caregiver", "RoleBereaved Caregiver", "Financial_Trouble")

sens_row <- function(formula_logit, label, data = df) {
  m_log <- fit_logit(formula_logit, data = data)
  m_lin <- fit_linear(gsub("Lonely", "UCLA_Score", formula_logit), data = data)
  out <- data.frame(Sensitivity = label, N = nobs(m_log), check.names = FALSE)
  for (term in primary_terms) {
    short <- gsub("Role|_Trouble", "", term)
    short <- gsub("Financial",      "Financial",  short)
    short <- gsub("Current Caregiver","Current",  short)
    short <- gsub("Bereaved Caregiver","Bereaved",short)
    log_e <- tidy_logit(m_log,  term)
    lin_e <- tidy_linear(m_lin, term)
    out[[paste0(short, " OR")]]    <- format_or(log_e)
    out[[paste0(short, " p_log")]] <- format_p(log_e$p)
    out[[paste0(short, " beta")]]  <- format_beta(lin_e)
    out[[paste0(short, " p_lin")]] <- format_p(lin_e$p)
  }
  out
}

primary_form <- paste0(
  "Lonely ~ Role + Age + Gender_clean + Urban_Rural + Race_Category + ",
  "Financial_Trouble + Education_Group + Insurance")

t4 <- list()
t4[[1]] <- sens_row(primary_form, "Primary model (for reference)")
t4[[2]] <- sens_row(paste0("Lonely ~ Role + Age + Gender_clean + Urban_Rural + ",
                           "Race_Category + Education_Group"),
                    "S1: Drop financial + insurance (mediation test)")
t4[[3]] <- sens_row(paste0(primary_form, " + Yrs_Since_Dx"),
                    "S2: + Years since diagnosis")
t4[[4]] <- sens_row(paste0(primary_form, " + Distance_Miles + Mobility"),
                    "S3: + Distance + mobility")
t4[[5]] <- sens_row(paste0(
  "Lonely ~ Role + Age + Gender_clean + Urban_Rural + Race_3cat + ",
  "Financial_Trouble + Education_Group + Insurance"),
  "S4: 3-category race (White/Hispanic/Other)")
t4[[6]] <- sens_row(primary_form,
                    "S5: Recent diagnosis only (2022-2024)",
                    data = df %>% filter(Diagnosis_Year >= 2022))

table4 <- do.call(rbind, t4)
write_csv(table4, file.path(OUTPUT_DIR, "Table4_sensitivity.csv"))
cat("\nTable 4 saved.\n")
print(table4, row.names = FALSE)

# ----- Race subgroup ORs from the 3-category-race sensitivity model (S4) -----
# Supply the Hispanic and Other-Nonwhite aOR estimates referenced in the Race
# paragraph of the Results.
cat("\nRace subgroup ORs from the 3-category-race model (logistic):\n")
m_log_race3 <- fit_logit(paste0(
  "Lonely ~ Role + Age + Gender_clean + Urban_Rural + Race_3cat + ",
  "Financial_Trouble + Education_Group + Insurance"))
for (term in c("Race_3catHispanic", "Race_3catOther Nonwhite")) {
  e <- tidy_logit(m_log_race3, term)
  cat(sprintf("  %-26s OR = %s, p = %s\n",
              gsub("Race_3cat", "", term), format_or(e), format_p(e$p)))
}

# =============================================================================
# 9. FIGURE 1 - LONELINESS PREVALENCE AND MEAN UCLA-3 BY ROLE
# =============================================================================
cat("\n========================================================================\n")
cat("9. FIGURE 1\n")
cat("========================================================================\n")

# Wilson 95% CI for a proportion (returned in percentage points)
wilson_ci <- function(k, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  p <- k / n
  denom  <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  half   <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
  c(lo = (center - half) * 100, hi = (center + half) * 100)
}

f1_data <- df %>%
  group_by(Role) %>%
  summarise(N         = n(),
            N_lonely  = sum(Lonely, na.rm = TRUE),
            Mean_UCLA = mean(UCLA_Score, na.rm = TRUE),
            SE_UCLA   = sd(UCLA_Score, na.rm = TRUE) /
                        sqrt(sum(!is.na(UCLA_Score))),
            .groups   = "drop") %>%
  rowwise() %>%
  mutate(Pct    = 100 * N_lonely / N,
         Pct_lo = wilson_ci(N_lonely, N)["lo"],
         Pct_hi = wilson_ci(N_lonely, N)["hi"]) %>%
  ungroup()

role_colors <- c("Patient" = "#5B9BD5",
                 "Current Caregiver" = "#ED7D31",
                 "Bereaved Caregiver" = "#A5277A")
role_labels <- c("Patient"            = "GBM\nPatient",
                 "Current Caregiver"  = "Current\nCaregiver",
                 "Bereaved Caregiver" = "Bereaved\nCaregiver")

# Panel A shows loneliness PREVALENCE (a binary outcome); the pairwise bracket
# p-values are therefore the binary chi-square tests. To display the
# continuous-score Welch t-test p-values instead, set: panelA_src <- pw_cont_p
fmt_fig_p  <- function(p) ifelse(p < 0.001, "p < 0.001", sprintf("p = %.3f", p))
panelA_src <- pw_binary_p
panelA_p   <- vapply(panelA_src, fmt_fig_p, character(1))

# Significance-bracket helper (numeric x positions on the discrete role axis)
sig_bracket <- function(x1, x2, y, label, tick = 1.4, text_gap = 1.7) {
  list(
    annotate("segment", x = x1, xend = x2, y = y, yend = y, linewidth = 0.5),
    annotate("segment", x = x1, xend = x1, y = y, yend = y - tick, linewidth = 0.5),
    annotate("segment", x = x2, xend = x2, y = y, yend = y - tick, linewidth = 0.5),
    annotate("text", x = (x1 + x2) / 2, y = y + text_gap, label = label, size = 3.1)
  )
}

# ----- Panel A: prevalence -----
p1 <- ggplot(f1_data, aes(x = Role, y = Pct, fill = Role)) +
  geom_col(width = 0.62, color = "black", linewidth = 0.45) +
  geom_errorbar(aes(ymin = Pct_lo, ymax = Pct_hi),
                width = 0.14, linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%\n(n = %d)", Pct, N), y = Pct_hi + 4),
            size = 3.4, lineheight = 0.9) +
  sig_bracket(1, 2, 79, panelA_p["P_vs_C"]) +
  sig_bracket(2, 3, 83, panelA_p["C_vs_B"]) +
  sig_bracket(1, 3, 87, panelA_p["P_vs_B"]) +
  scale_fill_manual(values = role_colors, guide = "none") +
  scale_y_continuous(limits = c(0, 92), expand = c(0, 0),
                     breaks = seq(0, 80, 20)) +
  scale_x_discrete(labels = role_labels) +
  labs(title = "A. Loneliness prevalence by role", x = NULL,
       y = "% Lonely (UCLA-3 score \u2265 6)") +
  theme_pub()

# ----- Panel B: mean score -----
p2 <- ggplot(f1_data, aes(x = Role, y = Mean_UCLA, fill = Role)) +
  geom_col(width = 0.62, color = "black", linewidth = 0.45) +
  geom_errorbar(aes(ymin = Mean_UCLA - 1.96 * SE_UCLA,
                    ymax = Mean_UCLA + 1.96 * SE_UCLA),
                width = 0.14, linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", Mean_UCLA),
                y = Mean_UCLA + 1.96 * SE_UCLA + 0.16), size = 3.4) +
  geom_hline(yintercept = 6, linetype = "dotted",
             color = "#C0392B", linewidth = 0.6) +
  annotate("text", x = 0.62, y = 6.18, label = "Loneliness cutoff (\u2265 6)",
           size = 3, color = "#C0392B", hjust = 0, fontface = "italic") +
  scale_fill_manual(values = role_colors, guide = "none") +
  scale_y_continuous(limits = c(3, 7.5), expand = c(0, 0)) +
  scale_x_discrete(labels = role_labels) +
  labs(title = "B. Mean UCLA-3 loneliness score by role", x = NULL,
       y = "Mean UCLA-3 score (range 3-9)") +
  theme_pub()

fig1 <- p1 + p2
ggsave(file.path(OUTPUT_DIR, "Figure1_loneliness_by_role.pdf"),
       fig1, width = 11, height = 5, device = pdf_device)
ggsave(file.path(OUTPUT_DIR, "Figure1_loneliness_by_role.png"),
       fig1, width = 11, height = 5, dpi = 600)
cat("Figure 1 saved (PDF + PNG).\n")

cat("\n========================================================================\n")
cat("10. FIGURE 2\n")
cat("========================================================================\n")

fp_terms <- list(
  list(term = "RoleBereaved Caregiver",            label = "Bereaved caregiver\n(vs. patient)"),
  list(term = "RoleCurrent Caregiver",             label = "Current caregiver\n(vs. patient)"),
  list(term = "Financial_Trouble",                 label = "Financial hardship\n(vs. no hardship)"),
  list(term = "Race_CategoryNonwhite",             label = "Non-White\n(vs. White)"),
  list(term = "InsurancePublic",                   label = "Public insurance\n(vs. private)"),
  list(term = "Education_GroupSome College",       label = "Some college\n(vs. college+)"),
  list(term = "Education_GroupLess than College",  label = "< College\n(vs. college+)"),
  list(term = "Urban_RuralUrban",                  label = "Urban residence\n(vs. rural)"),
  list(term = "Gender_cleanMale",                  label = "Male\n(vs. female)"),
  list(term = "Age",                                label = "Age\n(per year)")
)

fp_data <- do.call(rbind, lapply(fp_terms, function(x) {
  e <- tidy_logit(full_log, x$term)
  data.frame(label = x$label, OR = e$est, lo = e$lo, hi = e$hi, p = e$p)
}))
fp_data$label <- factor(fp_data$label, levels = rev(fp_data$label))
fp_data$sig   <- fp_data$p < 0.05

# x position (on the OR scale) for the single right-hand annotation column
ann_x_or <- 10

p3 <- ggplot(fp_data, aes(x = OR, y = label)) +
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "gray45", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.22, linewidth = 0.85) +
  geom_point(size = 3.3, shape = 15) +
  geom_text(aes(x = ann_x_or,
                label = sprintf("%.2f (%.2f-%.2f)%s",
                                OR, lo, hi, ifelse(sig, " *", "")),
                fontface = ifelse(sig, "bold", "plain")),
            hjust = 0, size = 3.2, color = "black") +
  scale_x_log10(breaks = c(0.5, 1, 2, 5),
                labels = c("0.5", "1", "2", "5"),
                limits = c(0.4, 48)) +
  labs(
    x = expression("Adjusted Odds Ratio          " %<-%
                     "Less lonely     More lonely" %->% ""),
    y = NULL) +
  theme_pub() +
  theme(panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "black", linewidth = 0.4),
        axis.text.y = element_text(size = 9.5))

ggsave(file.path(OUTPUT_DIR, "Figure2_forest_plot.pdf"), p3,
       width = 8, height = 6.5, device = pdf_device)
ggsave(file.path(OUTPUT_DIR, "Figure2_forest_plot.png"), p3,
       width = 8, height = 6.5, dpi = 600)
cat("Figure 2 saved (PDF + PNG).\n")

# =============================================================================
# 11. KEY HEADLINE NUMBERS SUMMARY (sanity check)
# =============================================================================
cat("\n========================================================================\n")
cat("11. KEY HEADLINE NUMBERS\n")
cat("========================================================================\n")
cat(sprintf("Total N: %d\n", nrow(df)))
cat(sprintf("  Patients:            %d\n", sum(df$Role == "Patient")))
cat(sprintf("  Current Caregivers:  %d\n", sum(df$Role == "Current Caregiver")))
cat(sprintf("  Bereaved Caregivers: %d\n", sum(df$Role == "Bereaved Caregiver")))
cat(sprintf("Overall %% lonely: %.1f%% (%d/%d)\n",
            100 * mean(df$Lonely, na.rm = TRUE),
            sum(df$Lonely, na.rm = TRUE), nrow(df)))
cat(sprintf("  Patients lonely:     %.1f%% (%d/%d)\n",
            100 * mean(df$Lonely[df$Role == "Patient"],            na.rm = TRUE),
            sum(df$Lonely[df$Role == "Patient"],                  na.rm = TRUE),
            sum(df$Role == "Patient")))
cat(sprintf("  Current Caregivers:  %.1f%% (%d/%d)\n",
            100 * mean(df$Lonely[df$Role == "Current Caregiver"],  na.rm = TRUE),
            sum(df$Lonely[df$Role == "Current Caregiver"],         na.rm = TRUE),
            sum(df$Role == "Current Caregiver")))
cat(sprintf("  Bereaved Caregivers: %.1f%% (%d/%d)\n",
            100 * mean(df$Lonely[df$Role == "Bereaved Caregiver"], na.rm = TRUE),
            sum(df$Lonely[df$Role == "Bereaved Caregiver"],        na.rm = TRUE),
            sum(df$Role == "Bereaved Caregiver")))
cat("\nAll analyses complete. Outputs written to:\n")
cat(sprintf("  %s\n", normalizePath(OUTPUT_DIR)))
