
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyterra) 

#jonction des tableaux------------------------------------------------------------------------------------
epi <- epi %>%
  left_join(spei_annual, by = c("Country Code", "year")) %>%
  left_join(spi_annual,  by = c("Country Code", "year"))

colSums(is.na(epi[c("spei", "spei_l1", "spi", "spi_l1")])) #aucun NA OK
cor(epi$spei, epi$spi) #égal à 0,95

#vérification corrélation intra-pays

within <- epi %>%
  group_by(`Country Code`) %>%
  mutate(spei_w = spei - mean(spei),
         spi_w  = spi  - mean(spi)) %>%
  ungroup()

cor(within$spei_w, within$spi_w)

# Par pays
within %>%
  group_by(`Country Code`) %>%
  summarise(cor_spei_spi = cor(spei, spi)) %>%
  arrange(cor_spei_spi)

#test de persistance-----------------------------------------------------------------------------------
library(fixest)

epi <- epi %>%
  mutate(log_mu = log(epi_index))
sum(is.na(epi$log_mu)) #sans NA OK

# Persistance : coefficient rho = 0,377 très significatif à 0,001
feols(log_mu ~ l(log_mu, 1) | `Country Code` + year,
      data = epi, panel.id = ~`Country Code` + year)

#CONTROLE DE LA QUEUE BASSE-----------------------------------------------------------------
#vérification RMSE
summary(epi$log_mu)
epi %>% arrange(log_mu) %>% select(`Country Code`, year, epi_index, log_mu) %>% head(8)
#diagnostique pour résoudre RMSE élevé
nm <- c("gdp_norm", "ext_norm", "infl_norm", "unemp_norm")

# Facteur le plus bas de chaque observation
epi$facteur_min <- nm[max.col(-as.matrix(epi[nm]), ties.method = "first")]

# Les 8 plus bas, avec leurs quatre facteurs
epi %>% arrange(log_mu) %>% slice(1:8) %>%
  select(`Country Code`, year, all_of(nm), epi_index, epi_index_clip)

# Combien d'observations ont au moins un facteur < 0,05 ?
epi$quasi_plancher <- apply(epi[nm], 1, min) < 0.05
sum(epi$quasi_plancher)
table(epi$quasi_plancher, epi$epi_index_clip)

summary(epi$log_mu)

library(dplyr)

nm <- c("gdp_norm", "ext_norm", "infl_norm", "unemp_norm")

# Les 11 observations, avec le facteur le plus bas et le SPEI de l'année
q11 <- epi %>%
  filter(quasi_plancher) %>%
  mutate(facteur_min = nm[max.col(-as.matrix(across(all_of(nm))), ties.method = "first")]) %>%
  select(`Country Code`, year, all_of(nm), epi_index, epi_index_clip,
         facteur_min, spei, spei_l1) %>%
  arrange(`Country Code`, year)

q11

# Répartition par pays et par facteur en cause
table(q11$`Country Code`)
table(q11$facteur_min)

# Ces années sont-elles plus sèches que les autres ?
epi %>%
  group_by(quasi_plancher) %>%
  summarise(n = n(), spei_moy = mean(spei), spei_sd = sd(spei),
            part_spei_neg = mean(spei < 0))

# Les 11 observations, avec le facteur le plus bas et le SPI de l'année
q11b <- epi %>%
  filter(quasi_plancher) %>%
  mutate(facteur_min = nm[max.col(-as.matrix(across(all_of(nm))), ties.method = "first")]) %>%
  select(`Country Code`, year, all_of(nm), epi_index, epi_index_clip,
         facteur_min, spi, spi_l1) %>%
  arrange(`Country Code`, year)

q11b

# Répartition par pays et par facteur en cause
table(q11b$`Country Code`)
table(q11b$facteur_min)

# Ces années sont-elles plus sèches que les autres ?
epi %>%
  group_by(quasi_plancher) %>%
  summarise(n = n(), spi_moy = mean(spi), spi_sd = sd(spi),
            part_spi_neg = mean(spi < 0))



#MODELE PRINCIPAL SPEI -------------------------------------------------------------------------------------------------------
library(fixest)
library(dplyr)

#erreurs standards clusterisées : déjà incluses avec les effets fixes pays et années

# Modèle principala avec log mu
m_all <- feols(log_mu ~ spei + spei_l1 | `Country Code` + year,
               data = epi, cluster = ~`Country Code`)
summary(m_all)

# Effet cumulé (t + t-1)
b <- coef(m_all)[c("spei", "spei_l1")]
V <- vcov(m_all)[c("spei", "spei_l1"), c("spei", "spei_l1")]
c(cumul = sum(b), se = sqrt(sum(V)))

# Première robustesse : sans les 11 observations au plancher
m_sans <- feols(log_mu ~ spei + spei_l1 | `Country Code` + year,
                data = filter(epi, !quasi_plancher),
                cluster = ~`Country Code`)

etable(m_all, m_sans)
# => RESULTATS NON SIGNIFICATIFS SUR L'INDICE AGREGE

library(fixest)
#DECOMPOSITION DU MODELE PRINCIPAL
epi <- epi %>%
  mutate(log_gdp   = log(gdp_norm),
         log_ext   = log(ext_norm),
         log_infl  = log(infl_norm),
         log_unemp = log(unemp_norm))

etable(
  feols(log_mu    ~ spei + spei_l1 | `Country Code` + year, data = epi, cluster = ~`Country Code`),
  feols(log_gdp   ~ spei + spei_l1 | `Country Code` + year, data = epi, cluster = ~`Country Code`),
  feols(log_ext   ~ spei + spei_l1 | `Country Code` + year, data = epi, cluster = ~`Country Code`),
  feols(log_infl  ~ spei + spei_l1 | `Country Code` + year, data = epi, cluster = ~`Country Code`),
  feols(log_unemp ~ spei + spei_l1 | `Country Code` + year, data = epi, cluster = ~`Country Code`)
)

#TESTS DE ROBUSTESSE DES EFFETS SIGNIFICATIFS

library(fixest)
library(dplyr)

f <- function(y, data = epi)
  feols(as.formula(paste(y, "~ spei + spei_l1 | `Country Code` + year")),
        data = data, cluster = ~`Country Code`)

# Croissance : log du score, variable brute, log sans le point au plancher
etable(f("log_gdp"), #ok
       f("gdp_growth"), #ok
       f("log_gdp", filter(epi, gdp_norm > 0.01))) #non significatif

# Solde extérieur : idem
etable(f("log_ext"),#ok
       f("ext_balance"),#ok
       f("log_ext", filter(epi, ext_norm > 0.01))) #non significatif

#Modèle principala avec epi_index
m_all_epi <- feols(
  epi_index ~ spei + spei_l1 | `Country Code` + year,
  data = epi,
  cluster = ~`Country Code`
)

summary(m_all_epi)

# Effet cumulé (t + t-1)
b <- coef(m_all_epi)[c("spei", "spei_l1")]

V <- vcov(m_all_epi)[c("spei", "spei_l1"),
                     c("spei", "spei_l1")]

c(
  cumul = sum(b),
  se    = sqrt(sum(V))
)
#obtenir t stat
effet_cumule <- sum(b)
se_cumule <- sqrt(sum(V))
t_cumule <- effet_cumule / se_cumule

c(
  effet = effet_cumule,
  se = se_cumule,
  t = t_cumule
)

# Première robustesse : sans les 11 observations au plancher
m_sans_epi <- feols(
  epi_index ~ spei + spei_l1 | `Country Code` + year,
  data = filter(epi, !quasi_plancher),
  cluster = ~`Country Code`
)

etable(m_all_epi, m_sans_epi)
# => RESULTATS NON SIGNIFICATIFS SUR INDICE AGREGE

#TEST DE SENSIBILITE (Leave-one-out analysis: effets de levier par pays)
pays <- unique(epi$`Country Code`)

##leave-one-out analysis: modèle global => aucun pays ne semble porter entièrement le résultat.
res <- lapply(pays, function(p){
  
  m <- feols(
    epi_index ~ spei + spei_l1 |
      `Country Code` + year,
    data = filter(epi, `Country Code` != p),
    cluster = ~`Country Code`
  )
  
  data.frame(
    pays_retire = p,
    beta_spei = coef(m)["spei"],
    beta_spei_l1 = coef(m)["spei_l1"]
  )
  
})

res <- do.call(rbind, res)
res

#leave-one-out analysis: gdp growth (significatif) => coefficient principal sur la croissance est remarquablement stable,le résultat n'est pas simplement dû à un pays atypique
res <- lapply(pays, function(p){
  
  m <- feols(
    gdp_growth ~ spei + spei_l1 |
      `Country Code` + year,
    data = filter(epi, `Country Code` != p),
    cluster = ~`Country Code`
  )
  
  ct <- coeftable(m)
  
  data.frame(
    pays_retire = p,
    coef = ct["spei","Estimate"],
    pval = ct["spei","Pr(>|t|)"]
  )
})

bind_rows(res)

#leave-one-out analysis: ext_balance avec spei_l1 (significatif) => effet plausible mais plus fragile: significatif à 5 % seulement sans le Niger ; très significatif sans la Guinée-Bissau ; non significatif sans Madagascar ; non significatif sans le Mali.
res <- lapply(pays, function(p){
  
  m <- feols(
    ext_balance ~ spei + spei_l1 |
      `Country Code` + year,
    data = filter(epi, `Country Code` != p),
    cluster = ~`Country Code`
  )
  
  ct <- coeftable(m)
  
  data.frame(
    pays_retire = p,
    coef_l1 = ct["spei_l1", "Estimate"],
    pval_l1 = ct["spei_l1", "Pr(>|t|)"]
  )
  
})

bind_rows(res)


#TEST DE ROBUSTESSE : wild cluster bootstrap
install.packages("fwildclusterboot")
library(fwildclusterboot)  # 1. CRAN

install.packages("fwildclusterboot", repos = "https://s3alfisc.r-universe.dev")   # 2. R-universe
# remotes::install_github("s3alfisc/fwildclusterboot")                  # 3. GitHub
install.packages(c("JuliaConnectoR", "collapse", "Formula", "dqrng", "gtools",
                   "Matrix.utils", "generics", "dreamerr"))


library(fixest)
library(fwildclusterboot)
packageVersion("fixest")
packageVersion("fwildclusterboot") #différente version de fixest et de fwildclusterboot => on countourne ce problème: réestimer le modèle avec lm()

epi$iso_f  <- factor(epi$iso)
epi$year_f <- factor(epi$year)

m_lm <- lm(gdp_growth ~ spei + spei_l1 + iso_f + year_f, data = epi)

# Contrôle : doit donner le même coefficient que feols #les coeffs sont identiques donc ok
coef(m_lm)["spei"]
coef(m_gdp)["spei"]

set.seed(123)
boottest(m_lm, param = "spei", clustid = "iso", B = 9999, type = "webb")

m_lm2 <- lm(log_gdp     ~ spei + spei_l1 + iso_f + year_f, data = epi)
m_lm3 <- lm(ext_balance ~ spei + spei_l1 + iso_f + year_f, data = epi)
m_lm4 <- lm(log_ext     ~ spei + spei_l1 + iso_f + year_f, data = epi)

set.seed(123); boottest(m_lm2, param = "spei",    clustid = "iso", B = 9999, type = "webb")
set.seed(123); boottest(m_lm3, param = "spei_l1", clustid = "iso", B = 9999, type = "webb")
set.seed(123); boottest(m_lm4, param = "spei_l1", clustid = "iso", B = 9999, type = "webb")



library(fixest)

verif <- function(m_lm, m_fe, nom) {
  data.frame(
    modele     = nom,
    spei_lm    = coef(m_lm)["spei"],
    spei_fe    = coef(m_fe)["spei"],
    spei_l1_lm = coef(m_lm)["spei_l1"],
    spei_l1_fe = coef(m_fe)["spei_l1"],
    ecart_max  = max(abs(coef(m_lm)[c("spei", "spei_l1")] -
                           coef(m_fe)[c("spei", "spei_l1")])),
    n_lm = nobs(m_lm), n_fe = nobs(m_fe),
    row.names = NULL
  )
}

rbind(
  verif(m_lm,  m_gdp,    "gdp_growth"),
  verif(m_lm2, m_loggdp, "log_gdp"),
  verif(m_lm3, m_ext,    "ext_balance"),
  verif(m_lm4, m_logext, "log_ext")
)

#lancement du vrai bootstrap" #modèle sur la croissance robuste (log ou brute)
library(fwildclusterboot)

set.seed(123); dqrng::dqset.seed(123)
b1 <- boottest(m_lm,  param = "spei",    clustid = "iso", B = 9999, type = "webb")  # gdp_growth
b2 <- boottest(m_lm2, param = "spei",    clustid = "iso", B = 9999, type = "webb")  # log_gdp
b3 <- boottest(m_lm3, param = "spei_l1", clustid = "iso", B = 9999, type = "webb")  # ext_balance
b4 <- boottest(m_lm4, param = "spei_l1", clustid = "iso", B = 9999, type = "webb")  # log_ext

lapply(list(gdp_brut = b1, log_gdp = b2, ext_brut = b3, log_ext = b4), summary)

# Erreurs standard classiques des versions brutes, pour comparer
etable(m_gdp, m_ext)


#TEST DE PLACEBO (seulement pour la croissance dont seule la relation est robuste et a survécu au bootstrap)
library(dplyr); library(fixest)

epi <- epi %>% arrange(iso, year) %>% group_by(iso) %>%
  mutate(spei_lead1 = lead(spei)) %>% ungroup()

# Placebo : le SPEI de l'année suivante ne doit pas expliquer la croissance
m_pl <- feols(gdp_growth ~ spei + spei_l1 + spei_lead1 | iso + year,
              data = epi, cluster = ~iso)
etable(m_pl)
#=> le SPEI de l’année suivante n’explique pas la croissance de l’année en cours.

# Effet cumulé t + t-1 sur la croissance, avec bootstrap
set.seed(123); dqrng::dqset.seed(123)
boottest(m_lm, param = c("spei", "spei_l1"), R = c(1, 1), r = 0,
         clustid = "iso", B = 9999, type = "webb")

library(fwildclusterboot)

epi$iso    <- epi$`Country Code`
epi$iso_f  <- factor(epi$iso)
epi$year_f <- factor(epi$year)

m_lm <- lm(gdp_growth ~ spei + spei_l1 + iso_f + year_f, data = epi)

# Contrôle : l'estimation doit valoir 1.137 + (-0.369) = 0.768
sum(coef(m_lm)[c("spei", "spei_l1")])

set.seed(123); dqrng::dqset.seed(123)
b_cum <- boottest(m_lm,
                  param  = c("spei", "spei_l1"),
                  R      = c(1, 1),     # combinaison linéaire : 1*spei + 1*spei_l1
                  r      = 0,           # hypothèse nulle : somme = 0
                  clustid = "iso", B = 9999, type = "webb")
summary(b_cum)
sum(coef(m_lm)[c("spei","spei_l1")])

# Solde extérieur sans les valeurs GNB 1998-2000 (autre source)
m_ext_noimp <- feols(ext_balance ~ spei + spei_l1 | iso + year,
                     data = filter(epi, !ext_balance_imp), cluster = ~iso)
etable(m_ext, m_ext_noimp)
#version sans imputations au bootstrap avec ext_balance
d_ni  <- filter(epi, !ext_balance_imp)
m_lm5 <- lm(ext_balance ~ spei + spei_l1 + iso_f + year_f, data = d_ni)

set.seed(123); dqrng::dqset.seed(123)
summary(boottest(m_lm5, param = "spei_l1", clustid = "iso", B = 9999, type = "webb"))

#RESUME DES TESTS DE ROBUSTESSE DE LA CROISSANCE
library(dplyr); library(fixest); library(fwildclusterboot)

epi$iso    <- epi$`Country Code`
epi$iso_f  <- factor(epi$iso)
epi$year_f <- factor(epi$year)

# Modèle principal (croissance)
m_gdp <- feols(gdp_growth ~ spei + spei_l1 | iso + year, data = epi, cluster = ~iso)

# B. Sans les trois épisodes politiques
ex <- !((epi$iso == "GNB" & epi$year == 1998) |
          (epi$iso == "MDG" & epi$year %in% c(2002, 2009)))
sum(!ex)   # doit valoir 3
m_B <- feols(gdp_growth ~ spei + spei_l1 | iso + year, data = epi[ex, ], cluster = ~iso)

# C. SPI à la place du SPEI
m_spi <- feols(gdp_growth ~ spi + spi_l1 | iso + year, data = epi, cluster = ~iso)

# D. Asymétrie : on sépare la partie sèche (<0) et la partie humide (>0)
epi <- epi %>%
  mutate(spei_neg    = pmin(spei, 0),    spei_pos    = pmax(spei, 0),
         spei_neg_l1 = pmin(spei_l1, 0), spei_pos_l1 = pmax(spei_l1, 0))

m_asym <- feols(gdp_growth ~ spei_neg + spei_pos + spei_neg_l1 + spei_pos_l1 | iso + year,
                data = epi, cluster = ~iso)

etable(m_gdp, m_B, m_spi, m_asym) #NON VALIDE POUR ASYM

m_sym <- feols(gdp_growth ~ spei + spei_neg + spei_l1 + spei_neg_l1 | iso + year,
               data = epi, cluster = ~iso)
etable(m_sym) # le modèle linéaire reste la spécification principale, et l’asymétrie est une limite à signaler: le modèle linéaire suppose qu’une unité de SPEI négatif coûte autant qu’une unité de SPEI positif rapporte.

#graphique régression partielle
library(fixest); library(ggplot2)

ry <- resid(feols(gdp_growth ~ spei_l1 | iso + year, data = epi))
rx <- resid(feols(spei       ~ spei_l1 | iso + year, data = epi))
pl <- data.frame(rx, ry, iso = epi$iso, year = epi$year)

coef(lm(ry ~ rx))["rx"]   # contrôle : doit valoir 1.137

g1 <- ggplot(pl, aes(rx, ry)) +
  geom_hline(yintercept = 0, colour = "grey80") +
  geom_vline(xintercept = 0, colour = "grey80") +
  geom_point(aes(colour = iso), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(x = "SPEI-12, résiduel (écart à la normale du pays et de l'année)",
       y = "Croissance du PIB, résiduelle (points de %)",
       colour = "Pays") +
  theme_minimal()
g1

#graphique des coefficients
est <- function(m, v, nom) {
  ct <- coeftable(m)[v, ]
  data.frame(spec = nom, b = ct["Estimate"],
             lo = ct["Estimate"] - qt(.975, 7) * ct["Std. Error"],
             hi = ct["Estimate"] + qt(.975, 7) * ct["Std. Error"])
}

rob <- rbind(
  est(m_gdp, "spei", "Principal"),
  est(m_B,   "spei", "Sans GNB 1998, MDG 2002 et 2009"),
  est(m_spi, "spi",  "SPI à la place du SPEI"),
  est(m_pl,  "spei", "Avec placebo (SPEI futur)")
)
rob$spec <- factor(rob$spec, levels = rev(rob$spec))

g2 <- ggplot(rob, aes(b, spec)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = lo, xmax = hi)) +
  labs(x = "Effet d'une unité de SPEI sur la croissance (points de %)", y = NULL) +
  theme_minimal()
g2

#ANALYSE CONTREFACTUELLE--------------------------------------------------------------------------
# QUESTION PRINCIPALE: les pays plus secs que les autres la même année ont-ils moins crû que les autres ?
#seuil SPEI: 0

library(dplyr); library(ggplot2)

b0 <- unname(coef(m_gdp)["spei"])      # 1.137
ci <- c(lo = 0.564, hi = 1.588)        # IC bootstrap 95 % (ligne gdp_brut de ton boottest)

cf <- epi %>%
  mutate(deficit  = pmax(-spei, 0),            # SPEI amené à 0 les années sèches
         perte_pp = b0 * deficit,              # points de croissance perdus
         perte_lo = ci["lo"] * deficit,
         perte_hi = ci["hi"] * deficit,
         gdp_cf   = gdp_growth + perte_pp)     # croissance sans sécheresse

# Contrôle : une année sèche coûte b0 * |SPEI|, une année humide 0
cf %>% filter(year %in% c(1998, 2015)) %>% select(iso, year, spei, gdp_growth, gdp_cf) %>% head()

# Résumé par pays
cf %>% group_by(iso) %>%
  summarise(part_annees_seches = mean(spei < 0),
            perte_moy_annees_seches = mean(perte_pp[spei < 0]),
            perte_moy_toutes_annees = mean(perte_pp),
            lo = mean(perte_lo), hi = mean(perte_hi))
#graphique
#graphe
ggplot(cf, aes(year)) +
  geom_ribbon(aes(ymin = gdp_growth, ymax = gdp_cf), fill = "tomato", alpha = 0.4) +
  geom_line(aes(y = gdp_growth), colour = "black") +
  geom_line(aes(y = gdp_cf), colour = "tomato", linetype = "dashed") +
  facet_wrap(~ iso, ncol = 2) +
  labs(x = NULL, y = "Croissance du PIB (%)") +
  theme_minimal()

library(dplyr); library(ggplot2) #diagramme

ggplot(cf, aes(year, perte_pp)) +
  geom_col(fill = "tomato", width = 0.8) +
  facet_wrap(~ iso, ncol = 2) +
  labs(x = NULL, y = "Croissance perdue à cause de la sécheresse (points de %)") +
  theme_minimal()


#SPEI seuil -1
library(dplyr); library(ggplot2); library(tidyr)

b0 <- unname(coef(m_gdp)["spei"])      # 1.137
ci <- c(lo = 0.564, hi = 1.588)        # IC bootstrap 95 % (bloc gdp_brut)

scenario <- function(df, seuil, nom) {
  df %>%
    mutate(scenario = nom,
           deficit  = pmax(seuil - spei, 0),     # part du SPEI sous le seuil
           perte_pp = b0 * deficit,
           perte_lo = ci["lo"] * deficit,
           perte_hi = ci["hi"] * deficit,
           gdp_cf   = gdp_growth + perte_pp)
}

cf_all <- bind_rows(scenario(epi,  0, "A : sans année sèche (seuil 0)"),
                    scenario(epi, -1, "B : sans sécheresse modérée à sévère (seuil -1)"))

# Résumé par pays et par scénario
resume <- cf_all %>%
  group_by(scenario, iso) %>%
  summarise(annees_touchees = sum(deficit > 0),
            perte_moy_an   = mean(perte_pp),
            perte_cumulee  = sum(perte_pp),
            lo = sum(perte_lo), hi = sum(perte_hi),
            pire_annee     = year[which.max(perte_pp)],
            pire_perte     = max(perte_pp), .groups = "drop")
print(resume, n = 20)

# Total sur le panel
cf_all %>% group_by(scenario) %>%
  summarise(perte_moy_pays_annee = mean(perte_pp),
            lo = mean(perte_lo), hi = mean(perte_hi),
            part_obs_touchees = mean(deficit > 0))

#graphique diagramme
ggplot(cf_all, aes(year, perte_pp, fill = scenario)) +
  geom_col(position = "identity", alpha = 0.7, width = 0.8) +
  facet_wrap(~ iso, ncol = 2) +
  scale_fill_manual(values = c("tomato", "steelblue")) +
  labs(x = NULL, y = "Croissance perdue (points de %)", fill = NULL) +
  theme_minimal() + theme(legend.position = "bottom")

#graphique heatmap : Carte de chaleur pays × année (la plus moderne)
library(dplyr); library(ggplot2)

hm <- cf_all %>%
  filter(grepl("^A", scenario)) %>%
  mutate(severe = spei < -1,
         iso = factor(iso, levels = rev(sort(unique(iso)))))

g_heat <- ggplot(hm, aes(year, iso, fill = perte_pp)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_point(data = filter(hm, severe), shape = 21, size = 2,
             fill = "white", colour = "grey15", stroke = 0.5) +
  scale_fill_viridis_c(option = "rocket", direction = -1, name = "Croissance\nperdue\n(points de %)") +
  scale_x_continuous(breaks = seq(1995, 2025, 5), expand = c(0, 0)) +
  labs(x = NULL, y = NULL,
       caption = "● : année avec SPEI < -1 (sécheresse modérée à sévère)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, colour = "grey30"))
g_heat

#graphique: Haltères (dumbbell) pour les pertes cumulées par pays
library(tidyr)

cum <- resume %>%
  mutate(sc = ifelse(grepl("^A", scenario), "A", "B")) %>%
  select(sc, iso, perte_cumulee) %>%
  pivot_wider(names_from = sc, values_from = perte_cumulee) %>%
  mutate(iso = reorder(iso, A))

g_dumb <- ggplot(cum, aes(y = iso)) +
  geom_segment(aes(x = B, xend = A, yend = iso), colour = "grey75", linewidth = 1.5) +
  geom_point(aes(x = A, colour = "A : seuil 0"),  size = 4) +
  geom_point(aes(x = B, colour = "B : seuil -1"), size = 4) +
  scale_colour_manual(values = c("A : seuil 0" = "#e4572e", "B : seuil -1" = "#17657d")) +
  labs(x = "Croissance perdue cumulée sur 1994-2024 (points de %)", y = NULL, colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), legend.position = "top")
g_dumb

g_dumb <- ggplot(cum, aes(y = iso)) +
  geom_segment(aes(x = B, xend = A, yend = iso), colour = "grey75", linewidth = 1.5) +
  geom_point(aes(x = A, colour = "A: threshold 0"), size = 4) +
  geom_point(aes(x = B, colour = "B: threshold -1"), size = 4) +
  scale_colour_manual(values = c("A: threshold 0" = "#e4572e", "B: threshold -1" = "#17657d")) +
  labs(x = "Cumulative growth loss over 1994-2024 (% points)", y = NULL, colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), legend.position = "top")
g_dumb

#part de SPEI commune
library(fixest); library(dplyr)

spei_w  <- epi$spei - ave(epi$spei, epi$iso)                 # écart à la moyenne du pays
spei_id <- resid(feols(spei ~ 1 | iso + year, data = epi))   # partie propre au pays (hors année commune)

# Part de la variation du SPEI qui est commune à l'année: 26,18%
1 - var(spei_id) / var(spei_w)

# Années où plusieurs pays sont secs en même temps: 2021
epi %>% group_by(year) %>%
  summarise(n_pays_sec = sum(spei < -1), spei_moy = mean(spei)) %>%
  filter(n_pays_sec >= 3)

#EFFETS fixes pays seulement => gdp significatif dans les deux modèles
m_noyear <- feols(gdp_growth ~ spei + spei_l1 | iso, data = epi, cluster = ~iso)
etable(m_gdp, m_noyear)

#grapghique comparaison effets années et effets pays seulement sans année****************************
library(dplyr); library(tidyr); library(ggplot2)


# Reprend d (construit dans le bloc précédent) : une ligne par pays et par modèle
off <- 0.22                                   # écart vertical entre les deux lignes
d <- d %>% mutate(y = as.numeric(iso) + ifelse(avec, off, -off))

av <- d %>% filter(avec)  %>% select(iso, A_av = A, B_av = B, y_av = y)
sa <- d %>% filter(!avec) %>% select(iso, A_sa = A, B_sa = B, y_sa = y)
w  <- inner_join(av, sa, by = "iso")

# 1. Quadrilatère : B_av -> A_av -> A_sa -> B_sa
poly <- bind_rows(lapply(seq_len(nrow(w)), function(i) with(w[i, ],
                                                            data.frame(iso, x = c(B_av, A_av, A_sa, B_sa), y = c(y_av, y_av, y_sa, y_sa)))))

# 2. Grille de petits carreaux à l'intérieur de chaque quadrilatère
dx <- diff(range(c(d$A, d$B))) / 110          # pas horizontal
dy <- 0.075                                   # pas vertical
tiles <- bind_rows(lapply(seq_len(nrow(w)), function(i) {
  r  <- w[i, ]
  ys <- seq(r$y_sa + dy / 2, r$y_av - dy / 2, by = dy)
  bind_rows(lapply(ys, function(yy) {
    t  <- (yy - r$y_sa) / (r$y_av - r$y_sa)   # position entre les deux lignes
    xl <- r$B_sa + t * (r$B_av - r$B_sa)      # bord gauche (bornes B)
    xr <- r$A_sa + t * (r$A_av - r$A_sa)      # bord droit  (bornes A)
    if (xr - xl < dx) return(NULL)
    data.frame(iso = r$iso, x = seq(xl + dx / 2, xr - dx / 2, by = dx), y = yy)
  }))
}))
#français
g_zone <- ggplot() +
  geom_polygon(data = poly, aes(x, y, group = iso),
               fill = "grey96", colour = "grey65", linewidth = 0.3) +
  geom_point(data = tiles, aes(x, y), shape = 15, size = 0.7, colour = "grey78") +
  geom_segment(data = d, aes(x = B, xend = A, y = y, yend = y),
               colour = "grey80", linewidth = 1.4) +
  geom_point(data = d, aes(x = A, y = y, colour = cA), size = 3.8) +
  geom_point(data = d, aes(x = B, y = y, colour = cB), size = 3.8) +
  scale_colour_manual(values = cols, breaks = names(cols)) +
  scale_y_continuous(breaks = seq_along(ord), labels = ord) +
  guides(colour = guide_legend(nrow = 2)) +
  labs(x = "Croissance perdue cumulée, 1994-2024 (points de %)", y = NULL, colour = NULL,
       caption = paste0("Zone grisée : plage couverte par les quatre estimations ",
                        "(deux coefficients × deux seuils).\n",
                        "EF année : effets fixes année. Coefficients : 1,14 (avec) et 0,78 (sans).")) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), legend.position = "top",
        plot.caption = element_text(hjust = 0, colour = "grey30"))
g_zone

ggsave("figures/contrefactuel_zone.pdf", g_zone, width = 7, height = 4.8)

#anglais
g_zone <- ggplot() +
  geom_polygon(data = poly, aes(x, y, group = iso),
               fill = "grey96", colour = "grey65", linewidth = 0.3) +
  geom_point(data = tiles, aes(x, y), shape = 15, size = 0.7, colour = "grey78") +
  geom_segment(data = d, aes(x = B, xend = A, y = y, yend = y),
               colour = "grey80", linewidth = 1.4) +
  geom_point(data = d, aes(x = A, y = y, colour = cA), size = 3.8) +
  geom_point(data = d, aes(x = B, y = y, colour = cB), size = 3.8) +
  scale_colour_manual(
    values = cols,
    breaks = names(cols),
    labels = c(
      "With year FE, threshold 0",
      "Without year FE, threshold 0",
      "With year FE, threshold -1",
      "Without year FE, threshold -1"
    )
  ) +
  scale_y_continuous(breaks = seq_along(ord), labels = ord) +
  guides(colour = guide_legend(nrow = 2)) +
  labs(
    x = "Cumulative growth loss, 1994–2024 (percentage points)",
    y = NULL,
    colour = NULL,
    caption = paste0(
      "Shaded area indicates the range spanned by the four estimates ",
      "(two coefficients and two drought thresholds).\n",
      "Year FE denotes year fixed effects. Estimated coefficients are ",
      "1.14 (with year FE) and 0.78 (without year FE)."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, colour = "grey30")
  )

g_zone

ggsave("figures/counterfactual_range.pdf", g_zone, width = 7, height = 4.8)