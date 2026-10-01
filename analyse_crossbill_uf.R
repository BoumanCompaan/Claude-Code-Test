# Analyse van crossbill_uf (unmarkedFrame, package 'unmarked')
#
# Uitgangspunt: crossbill_uf bestaat al in de R-sessie (bv. opgebouwd met
# unmarkedMultFrame() rond de ingebouwde 'crossbill'-dataset: 267 telpunten,
# 9 jaren, herhaalde presence/absence-tellingen -> dynamisch occupancy-model).

library(unmarked)

if (!exists("crossbill_uf")) {
  stop("crossbill_uf niet gevonden in deze sessie. Maak het object eerst aan.")
}

cat("Klasse van crossbill_uf:", class(crossbill_uf), "\n")

# ---- 1. Basisinformatie ----
print(crossbill_uf)
summary(crossbill_uf)

site_covs <- siteCovs(crossbill_uf)
cat("\nSite-covariaten:\n")
print(names(site_covs))

is_dynamic <- inherits(crossbill_uf, "unmarkedMultFrame")
if (is_dynamic) {
  ysc <- yearlySiteCovs(crossbill_uf)
  cat("\nJaarlijkse (yearly) site-covariaten:\n")
  print(names(ysc))
  numPrimary <- crossbill_uf@numPrimary
  cat("\nAantal primaire perioden (jaren):", numPrimary, "\n")
}

# ---- 1b. Sites met ontbrekende covariaten verwijderen ----
# colext()/occu() laten sites met NA's in de gebruikte covariaten vallen, maar
# per model verschillend (afhankelijk van de formule). Dat maakt fitList()
# onmogelijk ("Data are not the same among models"). Daarom hier vooraf
# uniform filteren op complete cases, zodat alle modellen dezelfde sites gebruiken.
complete_rows <- complete.cases(site_covs)
if (is_dynamic) {
  ysc_complete <- complete.cases(ysc)
  site_ysc_complete <- tapply(ysc_complete, rep(seq_len(nrow(site_covs)), each = numPrimary), all)
  complete_rows <- complete_rows & site_ysc_complete
}
if (!all(complete_rows)) {
  cat("\nVerwijderen van", sum(!complete_rows), "van", length(complete_rows),
      "site(s) met ontbrekende covariaten.\n")
  crossbill_uf <- crossbill_uf[complete_rows, ]
  site_covs <- siteCovs(crossbill_uf)
  if (is_dynamic) ysc <- yearlySiteCovs(crossbill_uf)
}

# ---- 2. Detectiehistorie visualiseren ----
plot(crossbill_uf)

# ---- 3. Naive occupancy per periode ----
y <- getY(crossbill_uf)
if (is_dynamic) {
  nsurveys <- ncol(y) / numPrimary
  naive_occ <- sapply(seq_len(numPrimary), function(t) {
    cols <- ((t - 1) * nsurveys + 1):(t * nsurveys)
    mean(apply(y[, cols, drop = FALSE], 1, function(x) any(x == 1, na.rm = TRUE)))
  })
  cat("\nNaive occupancy per jaar:\n")
  print(round(naive_occ, 3))
} else {
  naive_occ <- mean(apply(y, 1, function(x) any(x == 1, na.rm = TRUE)))
  cat("\nNaive occupancy:", round(naive_occ, 3), "\n")
}

# ---- 4. Modellen fitten ----
# Covariaten worden dynamisch opgepikt zodat het script niet breekt als
# de kolomnamen afwijken van de standaard 'elev'/'forest' uit de unmarked-demo.
covnames <- names(site_covs)
psi_form <- if (length(covnames) > 0) as.formula(paste("~", paste(covnames, collapse = " + "))) else ~1

if (is_dynamic) {

  m_null <- colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1,
                    pformula = ~1, data = crossbill_uf)

  m_sitecov <- colext(psiformula = psi_form, gammaformula = ~1, epsilonformula = ~1,
                       pformula = ~1, data = crossbill_uf)

  m_yeareffect <- if ("year" %in% names(ysc)) {
    colext(psiformula = psi_form, gammaformula = ~year, epsilonformula = ~year,
           pformula = ~1, data = crossbill_uf)
  } else NULL

  cat("\n---- Nulmodel (alles constant) ----\n")
  print(summary(m_null))

  cat("\n---- Model met site-covariaten op initiele bezetting ----\n")
  print(summary(m_sitecov))

  models <- list(null = m_null, sitecov = m_sitecov)
  if (!is.null(m_yeareffect)) {
    cat("\n---- Model met jaareffect op kolonisatie/extinctie ----\n")
    print(summary(m_yeareffect))
    models$yeareffect <- m_yeareffect
  }

  # ---- 5. Modelvergelijking (AIC) ----
  fl <- fitList(fits = models)
  ms <- modSel(fl)
  cat("\n---- Modelvergelijking (AIC) ----\n")
  print(ms)

  best_name <- rownames(ms@Full)[1]
  best <- models[[best_name]]
  cat("\nBeste model:", best_name, "\n")
  print(summary(best))

  # ---- 6. Voorspelde bezetting per jaar ----
  sm <- smoothed(best)
  cat("\nGladde (smoothed) bezettingskans per jaar (rij 'occupied'):\n")
  print(round(sm, 3))

  jaren <- if (is.numeric(colnames(sm))) as.numeric(colnames(sm)) else seq_len(ncol(sm))
  plot(jaren, sm[2, ], type = "b", pch = 16, ylim = c(0, 1),
       xlab = "Jaar", ylab = "Voorspelde bezettingsgraad",
       main = "Bezettingstrend crossbill")

  # ---- 7. Goodness-of-fit (optioneel, kan traag zijn) ----
  # Vereist package 'AICcmodavg'. Ontkommentarieer om te draaien:
  # if (requireNamespace("AICcmodavg", quietly = TRUE)) {
  #   gof <- AICcmodavg::mb.gof.test(best, nsim = 100)
  #   print(gof)
  # }

} else {
  # Statisch (single-season) occupancy-model
  # occu() gebruikt de vorm ~detectie ~bezetting
  m_null <- occu(~1 ~1, data = crossbill_uf)
  psi_rhs <- paste(deparse(psi_form), collapse = "")
  m_sitecov <- occu(as.formula(paste("~1", psi_rhs)), data = crossbill_uf)

  cat("\n---- Nulmodel ----\n")
  print(summary(m_null))
  cat("\n---- Model met site-covariaten ----\n")
  print(summary(m_sitecov))

  fl <- fitList(null = m_null, sitecov = m_sitecov)
  ms <- modSel(fl)
  cat("\n---- Modelvergelijking (AIC) ----\n")
  print(ms)
}
