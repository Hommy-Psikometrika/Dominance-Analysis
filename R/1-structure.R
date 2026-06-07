
library(readxl)
library(psych)
library(GPArotation)
library(tidyverse)
library(officer)
library(flextable)


dt <- readxl::read_excel("./data/Data Motivasi Belajar.xlsx")

wording <- readxl::read_excel("./materials/Wording Skala 2.xlsx")

head(dt)

# ============================================================
# EXPLORATORY FACTOR ANALYSIS
# Ekstraksi: Maximum Likelihood | Rotasi: Varimax
# ============================================================


# ── 1. CEK ASUMSI ────────────────────────────────────────────

# KMO & Bartlett's Test
kmo_result <- KMO(dt)
print(kmo_result)

bartlett_result <- cortest.bartlett(dt)
print(bartlett_result)

# ── 2. PARALLEL ANALYSIS ─────────────────────────────────────

set.seed(123)
parallel <- fa.parallel(
  dt,
  fm       = "ml",
  fa       = "fa",       # fokus pada faktor (bukan PC)
  n.iter   = 1000,
  quant    = 0.95,
  main     = "Parallel Analysis Scree Plot"
)

n_factors <- parallel$nfact
cat("\n>>> Jumlah faktor yang disarankan parallel analysis:", n_factors, "\n")

# ── 3. EFA ───────────────────────────────────────────────────

# hasil analisis setelah dikaji 3 dimensi lebih pas
n_factors = 3

efa_result <- fa(
  dt,
  nfactors = n_factors,
  fm       = "ml",
  rotate   = "varimax",
  scores   = "regression",
  missing  = FALSE,
  impute   = "mean"
)

# ── 4. OUTPUT ────────────────────────────────────────────────

# Ringkasan fit model
cat("Chi-square  :", efa_result$STATISTIC, "\n")
cat("df          :", efa_result$dof, "\n")
cat("p-value     :", efa_result$PVAL, "\n")
cat("CFI         :", efa_result$CFI, "\n")
cat("RMSEA       :", efa_result$RMSEA[1], 
    "[", efa_result$RMSEA[2], "-", efa_result$RMSEA[3], "]\n")
cat("TLI         :", efa_result$TLI, "\n")
cat("BIC         :", efa_result$BIC, "\n")

# Factor loadings (hanya loading >= 0.30)
cat("\n=== FACTOR LOADINGS (|loading| >= 0.30) ===\n")
print(
  fa.sort(efa_result),
  digits = 3,
  cut    = 0.30,
  sort   = TRUE
)

# Ekstrak loading matrix
loading_matrix <- efa_result$loadings |> 
  unclass() |> 
  as.data.frame() |> 
  rownames_to_column("Item")

# Tambah kolom: faktor dominan & selisih dua loading tertinggi
loading_only <- loading_matrix |> select(ML1, ML2, ML3)

loading_display <- loading_only |>
  mutate(across(starts_with("ML"), ~ ifelse(abs(.) >= 0.5, round(., 3), NA)))

loading_display <- data.frame(items="txt",loading_display)
loading_display$items <- wording$Pernyataan

loading_sorted <- loading_display |>
  mutate(
    Dominant_F  = apply(across(starts_with("ML")), 1, function(x) {
      x_num <- as.numeric(x)
      paste0("ML", which.max(abs(x_num)))
    }),
    Max_loading = apply(across(starts_with("ML")), 1, function(x) {
      max(abs(as.numeric(x)), na.rm = TRUE)
    })
  ) |>
  arrange(Dominant_F, desc(Max_loading)) |>
  select(-Max_loading)

print(loading_sorted)

# tulis ke word
ft <- flextable(loading_sorted) |>
  colformat_num(na_str = "-", digits = 3) |>
  autofit()

doc <- read_docx() |> body_add_flextable(ft)
print(doc, target = "./results/EFA_Factor_Loadings.docx")
