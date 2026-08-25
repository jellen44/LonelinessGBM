# ---------------------------------------------------------------------------
# Loneliness in the US glioblastoma community: patients, current caregivers,
# and bereaved caregivers
#
# Cross-sectional analysis of the OurBrainBank GBM survey (Feb-Sep 2024).
# Produces every table and figure in the manuscript plus the analytic dataset.
#
# Data
#   GBM_Survey.csv   one row per respondent, N = 525
#   NCI_Data.csv     NCI-designated-center flag, joined on `record`
#
# Outputs (written to out_dir)
#   Table1_descriptives_by_role.csv
#   Table2_role_associations.csv     unadjusted + fully adjusted role effects
#   Table3_primary_model.csv         all coefficients, primary model
#   Table4_sensitivity.csv           S1-S5 plus primary as reference (= Table S1)
#   Figure1_loneliness_by_role.*     prevalence + mean score, 2 panels
#   Figure2_forest_plot.*            adjusted ORs, primary model
#   final_analytic_data.csv
#
# Outcome: UCLA 3-item loneliness scale, summed (range 3-9). Co-primary as
# both the continuous score and the standard >= 6 binary cutoff, so most
# results below are reported as an OR and a beta coefficient side by side.
# ---------------------------------------------------------------------------

in_dir  <- "/Users/jellen/dropbox/MedicalSchool/Projects/GBM_Survey_Analysis"
out_dir <- "~/dropbox/"

library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)


# ---- load ----

# latin1 because a handful of free-text fields have smart quotes
raw <- read_csv(file.path(in_dir, "GBM_Survey.csv"), show_col_types = FALSE,
                locale = locale(encoding = "latin1"))
nci <- read_csv(file.path(in_dir, "NCI_Data.csv"), show_col_types = FALSE,
                locale = locale(encoding = "latin1"))

# left join: every survey row is kept even if the NCI file has no match
df <- left_join(raw, select(nci, record, NCI_Center), by = "record")


# ---- variable construction + inclusion criteria ----
ucla_map <- c("Hardly ever" = 1, "Some of the time" = 2, "Often" = 3)

df <- df %>%
  mutate(
    UCLA_Score = ucla_map[Companionship] + ucla_map[Left_Out] + ucla_map[Isolated_Others],
    Lonely     = as.integer(UCLA_Score >= 6),
    
    # Order matters: patients are claimed first, so SurvivalStatus is only ever
    # read for caregivers (for whom it refers to the person they cared for).
    Role = case_when(
      CaregiverPatient == "GBM Patient" ~ "Patient",
      SurvivalStatus   == "Deceased"    ~ "Bereaved Caregiver",
      TRUE                              ~ "Current Caregiver"
    ) %>% factor(levels = c("Patient", "Current Caregiver", "Bereaved Caregiver")),
    
    # 2-group version, kept so the original abstract numbers can be reproduced
    Caregiver_binary = factor(
      if_else(CaregiverPatient == "GBM Patient", "Patient", "Caregiver"),
      levels = c("Patient", "Caregiver")
    ),
    
    # Sample is 88% White, so the primary models collapse to White/Nonwhite.
    # Race_3cat pulls Hispanic out separately for S4.
    Race_Category = case_when(
      Race == "White" ~ "White",
      is.na(Race)     ~ NA_character_,
      TRUE            ~ "Nonwhite"
    ) %>% factor(levels = c("White", "Nonwhite")),
    
    Race_3cat = case_when(
      Race == "White"              ~ "White",
      Race == "Hispanic or Latino" ~ "Hispanic",
      is.na(Race)                  ~ NA_character_,
      TRUE                         ~ "Other Nonwhite"
    ) %>% factor(levels = c("White", "Hispanic", "Other Nonwhite")),
    
    # RUCA 1-3 = metropolitan core / high commuting flow
    Urban_Rural = factor(if_else(RUCACode <= 3, "Urban", "Rural"),
                         levels = c("Rural", "Urban")),
    
    # raw field is 0 = had trouble paying, so flip it
    Financial_Trouble = as.integer(Trouble_With_Expenses == 0),
    
    # a few free-text diagnosis years are implausible; clamp to 0-30
    Yrs_Since_Dx   = pmin(pmax(2024 - Diagnosis_Year, 0), 30),
    Distance_Miles = as.numeric(Miles_From_Hospital),
    
    # n = 6 chose "prefer not to say"; too few to keep as a level
    Gender_clean = factor(na_if(Gender, "Prefer not to say"),
                          levels = c("Female", "Male")),
    
    NCI_Center = as.integer(NCI_Center == "Yes")
  )

edu_less    <- c("Did not complete high school", "High school or G.E.D.")
edu_some    <- c("Some college", "Associate's degree")
edu_college <- c("College graduate", "Post-graduate degree")

quick_dx <- c("Less than 1 week", "1 to 2 weeks", "3 to 4 weeks")
slow_dx  <- c("Between 1 and 3 months", "Between 3 and 6 months",
              "Between 6 months and 1 year", "More than 1 year")

df <- df %>%
  mutate(
    Education_Group = case_when(
      Education %in% edu_less    ~ "Less than College",
      Education %in% edu_some    ~ "Some College",
      Education %in% edu_college ~ "College+"
    ) %>% factor(levels = c("College+", "Some College", "Less than College")),
    
    # Uninsured are set to NA rather than folded into public, which drops them
    # from every adjusted model. Small group, but it moves the modelled n --
    # report the model n in the manuscript, not the full 525.
    Insurance = case_when(
      Uninsured == 1         ~ NA_character_,
      Private_Insurance == 1 ~ "Private",
      TRUE                   ~ "Public"
    ) %>% factor(levels = c("Private", "Public")),
    
    Sx_to_Dx = case_when(
      Time_Symptoms_to_Diagnosis %in% quick_dx ~ "Less than 1 month",
      Time_Symptoms_to_Diagnosis %in% slow_dx  ~ "Over 1 month"
    ) %>% factor(levels = c("Less than 1 month", "Over 1 month"))
  )

# supportive-care contact flags come in as character in some exports
df <- df %>% mutate(across(c(Mobility, Falls, Socialwork, NoContact,
                             OncologyTeam, HospiceTeam), as.numeric))

stopifnot(!any(is.na(df$Role)))

# Catch response options that fell through case_when() -- otherwise a new or
# renamed level silently becomes NA and quietly drops people from the models.
unmapped_edu <- setdiff(unique(na.omit(df$Education)),
                        c(edu_less, edu_some, edu_college))
if (length(unmapped_edu)) {
  warning("Education values not mapped: ", paste(unmapped_edu, collapse = "; "))
}

# eyeball the reverse-coding above against the codebook before trusting the ORs
print(table(df$Trouble_With_Expenses, df$Financial_Trouble, useNA = "ifany"))

table(df$Role)
sum(!is.na(df$UCLA_Score))

#Applying age exclusion criteria
df <- df %>% filter(Age >= 18)

# ---- helpers ----

# Both extractors return a plain list so the formatters below can stay dumb.
# Missing term -> NA row, which keeps sensitivity models that drop a covariate
# from erroring out mid-table.

or_ci <- function(m, term) {
  if (!term %in% names(coef(m))) return(list(est = NA, lo = NA, hi = NA, p = NA))
  ci <- confint.default(m)   # Wald; profile CIs shift these <0.05 at this n
  list(est = exp(coef(m)[term]), lo = exp(ci[term, 1]), hi = exp(ci[term, 2]),
       p = summary(m)$coefficients[term, "Pr(>|z|)"])
}

b_ci <- function(m, term) {
  if (!term %in% names(coef(m))) return(list(est = NA, lo = NA, hi = NA, p = NA))
  ci <- confint(m)
  list(est = coef(m)[term], lo = ci[term, 1], hi = ci[term, 2],
       p = summary(m)$coefficients[term, "Pr(>|t|)"])
}

fmt_or   <- function(x) if (is.na(x$est)) "--" else sprintf("%.2f (%.2f-%.2f)", x$est, x$lo, x$hi)
fmt_beta <- function(x) if (is.na(x$est)) "--" else sprintf("%.2f (%.2f to %.2f)", x$est, x$lo, x$hi)
fmt_p    <- function(p) if (is.na(p)) "--" else if (p < .001) "<0.001" else sprintf("%.4g", p)

# shared look for Figures 1 and 2
theme_pub <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(
      plot.title         = element_text(face = "bold", size = base + 1, hjust = 0),
      plot.caption       = element_text(size = base - 2.5, hjust = 1, face = "italic"),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line          = element_line(color = "black", linewidth = 0.4),
      axis.ticks         = element_line(color = "black", linewidth = 0.4),
      axis.text          = element_text(color = "black", size = base - 1),
      axis.title         = element_text(size = base)
    )
}


# ---- table 1 ----
# Descriptives by role. One-way ANOVA for continuous rows, chi-square for
# categorical; complete cases per variable, so denominators vary slightly.

roles <- levels(df$Role)

cont_row <- function(v, label) {
  cells <- sapply(roles, function(g) {
    x <- df[[v]][df$Role == g]
    sprintf("%.1f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
  })
  p <- summary(aov(df[[v]] ~ df$Role))[[1]][["Pr(>F)"]][1]
  data.frame(Variable = label, Patient = cells[1],
             `Current Caregiver` = cells[2], `Bereaved Caregiver` = cells[3],
             `p-value` = fmt_p(p), check.names = FALSE)
}

# header row carries the p-value, then one indented row per level
cat_row <- function(v, label) {
  ct <- table(df$Role, df[[v]])
  n  <- rowSums(ct)
  p  <- suppressWarnings(chisq.test(ct)$p.value)
  
  head_row <- data.frame(Variable = label, Patient = "", `Current Caregiver` = "",
                         `Bereaved Caregiver` = "", `p-value` = fmt_p(p),
                         check.names = FALSE)
  
  body <- lapply(colnames(ct), function(l) {
    k   <- ct[, l]
    pct <- 100 * k / n
    data.frame(
      Variable = paste0("  ", l),
      Patient              = sprintf("%d (%.1f%%)", k[1], pct[1]),
      `Current Caregiver`  = sprintf("%d (%.1f%%)", k[2], pct[2]),
      `Bereaved Caregiver` = sprintf("%d (%.1f%%)", k[3], pct[3]),
      `p-value` = "", check.names = FALSE)
  })
  
  bind_rows(head_row, bind_rows(body))
}

table1 <- bind_rows(
  cont_row("Age",          "Age (years), mean (SD)"),
  cont_row("Yrs_Since_Dx", "Years since diagnosis, mean (SD)"),
  cont_row("UCLA_Score",   "UCLA-3 score, mean (SD)"),
  cat_row("Gender_clean",      "Gender"),
  cat_row("Race_Category",     "Race (2-category)"),
  cat_row("Race_3cat",         "Race (3-category)"),
  cat_row("Urban_Rural",       "Residence"),
  cat_row("Education_Group",   "Education"),
  cat_row("Insurance",         "Insurance"),
  cat_row("Financial_Trouble", "Financial hardship"),
  cat_row("NCI_Center",        "NCI Center treatment"),
  cat_row("Sx_to_Dx",          "Time from symptoms to diagnosis"),
  cat_row("Mobility",          "Mobility issues"),
  cat_row("Falls",             "Falls"),
  cat_row("Socialwork",        "Social work contact"),
  cat_row("OncologyTeam",      "Oncology team contact"),
  cat_row("HospiceTeam",       "Hospice team contact"),
  cat_row("Lonely",            "Lonely (UCLA-3 >= 6)")
)

print(table1, row.names = FALSE)


# ---- unadjusted comparisons ----
# Headline prevalence numbers, the overall 3-group tests, and the pairwise
# tests that get plotted on Figure 1A.

df %>%
  group_by(Role) %>%
  summarise(N = n(),
            n_lonely = sum(Lonely, na.rm = TRUE),
            pct      = round(100 * mean(Lonely, na.rm = TRUE), 1),
            mean_ucla = round(mean(UCLA_Score, na.rm = TRUE), 2),
            sd_ucla   = round(sd(UCLA_Score, na.rm = TRUE), 2))

summary(aov(UCLA_Score ~ Role, data = df))
chisq.test(table(df$Role, df$Lonely))

role_pairs <- list(P_vs_C = c("Patient", "Current Caregiver"),
                   P_vs_B = c("Patient", "Bereaved Caregiver"),
                   C_vs_B = c("Current Caregiver", "Bereaved Caregiver"))

# Welch on the continuous score
p_cont <- sapply(role_pairs, function(pr) {
  t.test(df$UCLA_Score[df$Role == pr[1]], df$UCLA_Score[df$Role == pr[2]])$p.value
})

# chi-square on the binary outcome; these are what get plotted in Fig 1A.
# No continuity correction, to match the overall 3-group test.
p_binary <- sapply(role_pairs, function(pr) {
  sub <- filter(df, Role %in% pr)
  suppressWarnings(
    chisq.test(table(droplevels(sub$Role), sub$Lonely), correct = FALSE)$p.value
  )
})

# the two sets agree on which contrasts clear 0.05
round(rbind(p_cont, p_binary), 4)


# ---- table 2: role effect, unadjusted vs adjusted ----
# Same contrast run three ways (caregiver vs patient, then split into current
# and bereaved), crude and fully adjusted, on both outcomes.

covars <- ~ . + Age + Gender_clean + Urban_Rural + Race_Category +
  Financial_Trouble + Education_Group + Insurance

role_effect <- function(rhs_var, term, label) {
  f_crude <- as.formula(paste("Lonely ~", rhs_var))
  f_adj   <- update(f_crude, covars)
  
  lapply(list(Unadjusted = f_crude, `Fully adjusted` = f_adj), function(f) {
    ml <- glm(f, data = df, family = binomial)
    mm <- lm(update(f, UCLA_Score ~ .), data = df)   # same RHS, continuous LHS
    data.frame(
      Comparison = label,
      Model      = NA_character_,
      N          = nobs(ml),
      `OR (95% CI)`   = fmt_or(or_ci(ml, term)),
      `p (logistic)`  = fmt_p(or_ci(ml, term)$p),
      `Beta (95% CI)` = fmt_beta(b_ci(mm, term)),
      `p (linear)`    = fmt_p(b_ci(mm, term)$p),
      check.names = FALSE)
  }) %>% bind_rows(.id = "Model2") %>%
    mutate(Model = Model2) %>% select(-Model2)
}

table2 <- bind_rows(
  role_effect("Caregiver_binary", "Caregiver_binaryCaregiver",
              "Caregiver vs Patient (2-group)"),
  role_effect("Role", "RoleCurrent Caregiver",  "Current Caregiver vs Patient"),
  role_effect("Role", "RoleBereaved Caregiver", "Bereaved Caregiver vs Patient")
)

print(table2, row.names = FALSE)


# ---- table 3: full primary model ----
# Covariates were pre-specified: role, then demographics (age, gender, race,
# urban/rural) and structural factors (financial hardship, education,
# insurance). Not selected on p-values.

f_primary <- Lonely ~ Role + Age + Gender_clean + Urban_Rural + Race_Category +
  Financial_Trouble + Education_Group + Insurance

m_log <- glm(f_primary, data = df, family = binomial)
m_lin <- lm(update(f_primary, UCLA_Score ~ .), data = df)

terms3 <- c(
  "RoleCurrent Caregiver"           = "Current caregiver (ref: patient)",
  "RoleBereaved Caregiver"          = "Bereaved caregiver (ref: patient)",
  "Financial_Trouble"               = "Financial hardship (ref: no hardship)",
  "Race_CategoryNonwhite"           = "Non-White (ref: White)",
  "Age"                             = "Age (per year)",
  "Gender_cleanMale"                = "Male (ref: female)",
  "Urban_RuralUrban"                = "Urban (ref: rural)",
  "Education_GroupSome College"     = "Some college (ref: college+)",
  "Education_GroupLess than College" = "< College (ref: college+)",
  "InsurancePublic"                 = "Public insurance (ref: private)"
)

table3 <- bind_rows(lapply(names(terms3), function(tm) {
  data.frame(Predictor = terms3[[tm]],
             `OR (95% CI)`   = fmt_or(or_ci(m_log, tm)),
             `p (logistic)`  = fmt_p(or_ci(m_log, tm)$p),
             `Beta (95% CI)` = fmt_beta(b_ci(m_lin, tm)),
             `p (linear)`    = fmt_p(b_ci(m_lin, tm)$p),
             check.names = FALSE)
}))

print(table3, row.names = FALSE)

# model n differs from 525 because of the insurance and gender exclusions above
c(n_log = nobs(m_log), n_lin = nobs(m_lin),
  aic = round(AIC(m_log), 1), r2 = round(summary(m_lin)$r.squared, 3))


# ---- table 4: sensitivity analyses ----
# Only the three coefficients the paper actually leans on are carried across
# rows: the two role contrasts and financial hardship.
#
#   S1  drop financial + insurance -- does SES sit on the causal path from
#       role to loneliness, i.e. does the role effect inflate without them?
#   S2  add years since diagnosis  -- bereaved are further out by definition
#   S3  add distance + mobility    -- physical access as a competing explanation
#   S4  split Hispanic from other nonwhite
#   S5  restrict to 2022-2024 diagnoses -- recall and survivorship bias

key_terms <- c(Current  = "RoleCurrent Caregiver",
               Bereaved = "RoleBereaved Caregiver",
               Financial = "Financial_Trouble")

sens_row <- function(f, label, data = df) {
  ml <- glm(f, data = data, family = binomial)
  mm <- lm(update(f, UCLA_Score ~ .), data = data)
  
  out <- data.frame(Sensitivity = label, N = nobs(ml), check.names = FALSE)
  for (nm in names(key_terms)) {
    tm <- key_terms[[nm]]
    out[[paste(nm, "OR")]]    <- fmt_or(or_ci(ml, tm))
    out[[paste(nm, "p_log")]] <- fmt_p(or_ci(ml, tm)$p)
    out[[paste(nm, "beta")]]  <- fmt_beta(b_ci(mm, tm))
    out[[paste(nm, "p_lin")]] <- fmt_p(b_ci(mm, tm)$p)
  }
  out
}

table4 <- bind_rows(
  sens_row(f_primary, "Primary model (for reference)"),
  sens_row(update(f_primary, . ~ . - Financial_Trouble - Insurance),
           "S1: Drop financial + insurance (mediation test)"),
  sens_row(update(f_primary, . ~ . + Yrs_Since_Dx),
           "S2: + Years since diagnosis"),
  sens_row(update(f_primary, . ~ . + Distance_Miles + Mobility),
           "S3: + Distance + mobility"),
  sens_row(update(f_primary, . ~ . - Race_Category + Race_3cat),
           "S4: 3-category race (White/Hispanic/Other)"),
  sens_row(f_primary, "S5: Recent diagnosis only (2022-2024)",
           data = filter(df, Diagnosis_Year >= 2022))
)

print(table4, row.names = FALSE)

# Hispanic / other-nonwhite ORs quoted in the race paragraph of the Results
m_race3 <- glm(update(f_primary, . ~ . - Race_Category + Race_3cat),
               data = df, family = binomial)
sapply(c("Race_3catHispanic", "Race_3catOther Nonwhite"),
       function(tm) fmt_or(or_ci(m_race3, tm)))


# ---- figure 1 ----
# A: loneliness prevalence with Wilson intervals (better than Wald at the
# patient n of 121). B: mean UCLA-3 with 1.96*SE and the >= 6 cutoff drawn in.

wilson <- function(k, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  p <- k / n
  d <- 1 + z^2 / n
  ctr  <- (p + z^2 / (2 * n)) / d
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / d
  c(lo = (ctr - half) * 100, hi = (ctr + half) * 100)
}

f1 <- df %>%
  group_by(Role) %>%
  summarise(N = n(),
            k = sum(Lonely, na.rm = TRUE),
            mean_ucla = mean(UCLA_Score, na.rm = TRUE),
            se_ucla   = sd(UCLA_Score, na.rm = TRUE) / sqrt(sum(!is.na(UCLA_Score))),
            .groups = "drop") %>%
  rowwise() %>%
  mutate(pct = 100 * k / N,
         lo  = wilson(k, N)["lo"],
         hi  = wilson(k, N)["hi"]) %>%
  ungroup()

role_cols <- c("Patient" = "#5B9BD5", "Current Caregiver" = "#ED7D31",
               "Bereaved Caregiver" = "#A5277A")
role_labs <- c("Patient" = "GBM\nPatient", "Current Caregiver" = "Current\nCaregiver",
               "Bereaved Caregiver" = "Bereaved\nCaregiver")

star <- function(p) if (p < .001) "p < 0.001" else sprintf("p = %.3f", p)
lab_p <- sapply(p_binary, star)

# significance brackets; y positions are hand-tuned to the 0-92 panel A scale
bracket <- function(x1, x2, y, label, tick = 1.4, gap = 1.7) {
  list(
    annotate("segment", x = x1, xend = x2, y = y, yend = y, linewidth = 0.5),
    annotate("segment", x = x1, xend = x1, y = y, yend = y - tick, linewidth = 0.5),
    annotate("segment", x = x2, xend = x2, y = y, yend = y - tick, linewidth = 0.5),
    annotate("text", x = (x1 + x2) / 2, y = y + gap, label = label, size = 3.1)
  )
}

p1 <- ggplot(f1, aes(Role, pct, fill = Role)) +
  geom_col(width = 0.62, color = "black", linewidth = 0.45) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.14, linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%\n(n = %d)", pct, N), y = hi + 4),
            size = 3.4, lineheight = 0.9) +
  bracket(1, 2, 79, lab_p["P_vs_C"]) +
  bracket(2, 3, 83, lab_p["C_vs_B"]) +
  bracket(1, 3, 87, lab_p["P_vs_B"]) +
  scale_fill_manual(values = role_cols, guide = "none") +
  scale_y_continuous(limits = c(0, 92), expand = c(0, 0), breaks = seq(0, 80, 20)) +
  scale_x_discrete(labels = role_labs) +
  labs(title = "A. Loneliness prevalence by role", x = NULL,
       y = "% Lonely (UCLA-3 score \u2265 6)") +
  theme_pub()

p2 <- ggplot(f1, aes(Role, mean_ucla, fill = Role)) +
  geom_col(width = 0.62, color = "black", linewidth = 0.45) +
  geom_errorbar(aes(ymin = mean_ucla - 1.96 * se_ucla,
                    ymax = mean_ucla + 1.96 * se_ucla),
                width = 0.14, linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", mean_ucla),
                y = mean_ucla + 1.96 * se_ucla + 0.16), size = 3.4) +
  geom_hline(yintercept = 6, linetype = "dotted", color = "#C0392B", linewidth = 0.6) +
  annotate("text", x = 0.62, y = 6.18, label = "Loneliness cutoff (\u2265 6)",
           size = 3, color = "#C0392B", hjust = 0, fontface = "italic") +
  scale_fill_manual(values = role_cols, guide = "none") +
  # y starts at 3, the scale floor, not 0
  scale_y_continuous(limits = c(3, 7.5), expand = c(0, 0)) +
  scale_x_discrete(labels = role_labs) +
  labs(title = "B. Mean UCLA-3 loneliness score by role", x = NULL,
       y = "Mean UCLA-3 score (range 3-9)") +
  theme_pub()

fig1 <- p1 + p2

# cairo_pdf so the >= glyph renders and fonts embed for submission
ggsave(file.path(out_dir, "Figure1_loneliness_by_role.png"), fig1,
       width = 11, height = 5, dpi = 600)


# ---- figure 2: forest plot ----
# Adjusted ORs from the primary logistic model, ordered largest effect first.

fp_labs <- c(
  "RoleBereaved Caregiver"           = "Bereaved caregiver\n(vs. patient)",
  "RoleCurrent Caregiver"            = "Current caregiver\n(vs. patient)",
  "Financial_Trouble"                = "Financial hardship\n(vs. no hardship)",
  "Race_CategoryNonwhite"            = "Non-White\n(vs. White)",
  "InsurancePublic"                  = "Public insurance\n(vs. private)",
  "Education_GroupSome College"      = "Some college\n(vs. college+)",
  "Education_GroupLess than College" = "< College\n(vs. college+)",
  "Urban_RuralUrban"                 = "Urban residence\n(vs. rural)",
  "Gender_cleanMale"                 = "Male\n(vs. female)",
  "Age"                              = "Age\n(per year)"
)

fp <- bind_rows(lapply(names(fp_labs), function(tm) {
  e <- or_ci(m_log, tm)
  data.frame(label = fp_labs[[tm]], OR = e$est, lo = e$lo, hi = e$hi, p = e$p)
})) %>%
  mutate(label = factor(label, levels = rev(unname(fp_labs))),  # top row = first
         sig   = p < .05)

p3 <- ggplot(fp, aes(OR, label)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray45", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.22, linewidth = 0.85) +
  geom_point(size = 3.3, shape = 15) +
  # numeric column parked at OR = 10, past the widest CI; the axis runs to 48
  # to leave room for it. Adjust both together if a CI ever gets wider.
  geom_text(aes(x = 10,
                label = sprintf("%.2f (%.2f-%.2f)%s", OR, lo, hi, if_else(sig, " *", "")),
                fontface = if_else(sig, "bold", "plain")),
            hjust = 0, size = 3.2) +
  scale_x_log10(breaks = c(0.5, 1, 2, 5), labels = c("0.5", "1", "2", "5"),
                limits = c(0.4, 48)) +
  labs(x = "Adjusted odds ratio  (<- less lonely   |   more lonely ->)", y = NULL) +
  theme_pub() +
  theme(panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_text(size = 9.5))

ggsave(file.path(out_dir, "Figure2_forest_plot.png"), p3,
       width = 8, height = 6.5, dpi = 600, bg = "white")
