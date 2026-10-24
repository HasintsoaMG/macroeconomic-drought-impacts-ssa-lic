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

# Ces années sont-elles plus sèches que les autres ? Cela suggère que le phénomène que tu observes dans ton modèle n'est probablement pas dû à des sécheresses exceptionnellement sévères.
epi %>%
  group_by(quasi_plancher) %>%
  summarise(n = n(), spi_moy = mean(spi), spi_sd = sd(spi),
            part_spi_neg = mean(spi < 0))

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


#MODELE PRINCIPAL SPI -------------------------------------------------------------------------------------------------------
library(fixest)
library(dplyr)

#erreurs standards clusterisées : déjà incluses avec les effets fixes pays et années

# Modèle principal avec log mu #RESULTATS NON SIGNIFICATIFS SUR L'INDICE AGREGE
m_all_spi <- feols(log_mu ~ spi + spi_l1 | `Country Code` + year,
               data = epi, cluster = ~`Country Code`)
summary(m_all_spi)

# Effet cumulé (t + t-1)
b <- coef(m_all_spi)[c("spi", "spi_l1")]
V <- vcov(m_all_spi)[c("spi", "spi_l1"), c("spi", "spi_l1")]
c(cumul = sum(b), se = sqrt(sum(V)))

# Première robustesse : sans les 11 observations au plancher
m_sans_spi <- feols(log_mu ~ spi + spi_l1 | `Country Code` + year,
                data = filter(epi, !quasi_plancher),
                cluster = ~`Country Code`)

etable(m_all_spi, m_sans_spi)
# => RESULTATS NON SIGNIFICATIFS SUR L'INDICE AGREGE

library(fixest)
#DECOMPOSITION DU MODELE PRINCIPAL
epi <- epi %>%
  mutate(log_gdp   = log(gdp_norm),
         log_ext   = log(ext_norm),
         log_infl  = log(infl_norm),
         log_unemp = log(unemp_norm))

etable(
  feols(log_mu    ~ spi + spi_l1 | `Country Code` + year, data = epi, cluster = ~`Country Code`),
  feols(log_gdp   ~ spi + spi_l1 | `Country Code` + year, data = epi, cluster = ~`Country Code`),
  feols(log_ext   ~ spi + spi_l1 | `Country Code` + year, data = epi, cluster = ~`Country Code`),
  feols(log_infl  ~ spi + spi_l1 | `Country Code` + year, data = epi, cluster = ~`Country Code`),
  feols(log_unemp ~ spi + spi_l1 | `Country Code` + year, data = epi, cluster = ~`Country Code`)
)

#TESTS DE ROBUSTESSE DES EFFETS SIGNIFICATIFS---------------------------------------------------------------------

library(fixest)
library(dplyr)

f <- function(y, data = epi)
  feols(as.formula(paste(y, "~ spi + spi_l1 | `Country Code` + year")),
        data = data, cluster = ~`Country Code`)

# Croissance : log du score, variable brute, log sans le point au plancher
etable(f("log_gdp"),
       f("gdp_growth"),
       f("log_gdp", filter(epi, gdp_norm > 0.01)))

# Solde extérieur : idem
etable(f("log_ext"),
       f("ext_balance"),
       f("log_ext", filter(epi, ext_norm > 0.01)))

#Modèle principale avec epi_index
m_all_spiepi <- feols(
  epi_index ~ spi + spi_l1 | `Country Code` + year,
  data = epi,
  cluster = ~`Country Code`
)

summary(m_all_spiepi)

# Effet cumulé (t + t-1)*** non significatif
b <- coef(m_all_spiepi)[c("spi", "spi_l1")]

V <- vcov(m_all_spiepi)[c("spi", "spi_l1"),
                     c("spi", "spi_l1")]

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
p_cumule <- 2 * pnorm(abs(t_cumule), lower.tail = FALSE)

c(
  effet = effet_cumule,
  se = se_cumule,
  t = t_cumule,
  p = p_cumule
)

# Première robustesse : sans les 11 observations au plancher #significatif
m_sans_spiepi <- feols(
  epi_index ~ spi + spi_l1 | `Country Code` + year,
  data = filter(epi, !quasi_plancher),
  cluster = ~`Country Code`
)

etable(m_all_spiepi, m_sans_spiepi)
# => RESULTATS SIGNIFICATIFS SUR INDICE AGREGE

#TEST DE SENSIBILITE (Leave-one-out analysis: effets de levier par pays)----------------------------------------------------------------------------------------------
pays <- unique(epi$`Country Code`)

##leave-one-out analysis: modèle global => coeff sur spi et spi_l1 remarquablement stables, aucun pays ne porte entièrement le résultat
res <- lapply(pays, function(p){
  
  m <- feols(
    epi_index ~ spi + spi_l1 |
      `Country Code` + year,
    data = filter(epi, `Country Code` != p),
    cluster = ~`Country Code`
  )
  
  data.frame(
    pays_retire = p,
    beta_spi = coef(m)["spi"],
    beta_spi_l1 = coef(m)["spi_l1"]
  )
  
})

res <- do.call(rbind, res)
res

#leave-one-out analysis: gdp growth (significatif) => coefficient principal sur la croissance est remarquablement stable,le résultat n'est pas simplement dû à un pays atypique
res <- lapply(pays, function(p){
  
  m <- feols(
    gdp_growth ~ spi + spi_l1 |
      `Country Code` + year,
    data = filter(epi, `Country Code` != p),
    cluster = ~`Country Code`
  )
  
  ct <- coeftable(m)
  
  data.frame(
    pays_retire = p,
    coef = ct["spi","Estimate"],
    pval = ct["spi","Pr(>|t|)"]
  )
})

bind_rows(res)

#leave-one-out analysis: ext_balance avec spei_l1 (significatif) => significatif quelque soit le pays retiré  pas comme avec SPEI
res <- lapply(pays, function(p){
  
  m <- feols(
    ext_balance ~ spi + spi_l1 |
      `Country Code` + year,
    data = filter(epi, `Country Code` != p),
    cluster = ~`Country Code`
  )
  
  ct <- coeftable(m)
  
  data.frame(
    pays_retire = p,
    coef_l1 = ct["spi_l1", "Estimate"],
    pval_l1 = ct["spi_l1", "Pr(>|t|)"]
  )
  
})

bind_rows(res)


#TEST DE ROBUSTESSE : wild cluster bootstrap-------------------------------------------------------------------------------------
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

#LSDV signifie Least Squares Dummy Variables.
m_lm <- lm(gdp_growth ~ spi + spi_l1 + iso_f + year_f, data = epi)
m_gdp <- lm(gdp_growth ~ spi + spi_l1 + iso_f + year_f, data = epi)
#estimation FE  avec spi
m_gdp_spi <- feols(
  gdp_growth ~ spi + spi_l1 |
    `Country Code` + year,
  data = epi
)

m_loggdp_spi <- feols(
  log_gdp ~ spi + spi_l1 |
    `Country Code` + year,
  data = epi
)

m_ext_spi <- feols(
  ext_balance ~ spi_l1 + spi |
    `Country Code` + year,
  data = epi
)

m_logext_spi <- feols(
  log_ext ~ spi + spi_l1 |
    `Country Code` + year,
  data = epi
)

m_epi_index_spi <- feols(
  epi_index ~ spi + spi_l1 |
    `Country Code` + year,
  data = epi
)


# Contrôle : doit donner le même coefficient que feols #les coeffs sont identiques donc ok
coef(m_lm)["spi"]
coef(m_gdp)["spi"]

set.seed(123)
boottest(m_lm, param = "spi", clustid = "iso", B = 9999, type = "webb")

m_lm2 <- lm(log_gdp     ~ spi + spi_l1 + iso_f + year_f, data = epi)
m_lm3 <- lm(ext_balance ~ spi + spi_l1 + iso_f + year_f, data = epi)
m_lm4 <- lm(log_ext     ~ spi + spi_l1 + iso_f + year_f, data = epi)

set.seed(123); boottest(m_lm2, param = "spi",    clustid = "iso", B = 9999, type = "webb")
set.seed(123); boottest(m_lm3, param = "spi_l1", clustid = "iso", B = 9999, type = "webb")
set.seed(123); boottest(m_lm4, param = "spi_l1", clustid = "iso", B = 9999, type = "webb")

library(fixest)

verif <- function(m_lm, m_fe, nom) {
  data.frame(
    modele     = nom,
    spi_lm     = coef(m_lm)["spi"],
    spi_fe     = coef(m_fe)["spi"],
    spi_l1_lm  = coef(m_lm)["spi_l1"],
    spi_l1_fe  = coef(m_fe)["spi_l1"],
    ecart_max  = max(abs(
      coef(m_lm)[c("spi","spi_l1")] -
        coef(m_fe)[c("spi","spi_l1")]
    )),
    n_lm = nobs(m_lm),
    n_fe = nobs(m_fe),
    row.names = NULL
  )
}

rbind(
  verif(m_lm,  m_gdp_spi,    "gdp_growth"),
  verif(m_lm2, m_loggdp_spi, "log_gdp"),
  verif(m_lm3, m_ext_spi,    "ext_balance"),
  verif(m_lm4, m_logext_spi, "log_ext")
)
#LM fait le même travail que feols avec presque 0 écart donc possible de faire lm avec le bootstrap


#lancement du vrai bootstrap" #modèle robuste pour les composantes GDP et ext et leur retard
library(fwildclusterboot)

set.seed(123); dqrng::dqset.seed(123)
b1 <- boottest(m_lm,  param = "spi",    clustid = "iso", B = 9999, type = "webb")  # gdp_growth
b2 <- boottest(m_lm2, param = "spi",    clustid = "iso", B = 9999, type = "webb")  # log_gdp
b3 <- boottest(m_lm3, param = "spi_l1", clustid = "iso", B = 9999, type = "webb")  # ext_balance
b4 <- boottest(m_lm4, param = "spi_l1", clustid = "iso", B = 9999, type = "webb")  # log_ext

lapply(list(gdp_brut = b1, log_gdp = b2, ext_brut = b3, log_ext = b4), summary)

# Erreurs standard classiques des versions brutes, pour comparer
etable(m_gdp, m_ext)


#test bootstrap avec le modèle global
#estimation de la version LSDV DE m_all_spiepi
m_lm_epi <- lm(
  epi_index ~ spi + spi_l1 +
    factor(`Country Code`) +
    factor(year),
  data = epi
)
#bootstrap test
library(fwildclusterboot)

b_epi_spi <- boottest(
  m_lm_epi,
  param   = "spi",
  B       = 9999,
  clustid = "iso",
  type    = "webb"
)

b_epi_spi_l1 <- boottest(
  m_lm_epi,
  param   = "spi_l1",
  B       = 9999,
  clustid = "iso",
  type    = "webb"
)

summary(b_epi_spi) #l'effet contemporain reste significatif après wild cluster bootstrap
summary(b_epi_spi_l1) #l'effet n'est plus significatif

#=========================================
# WILD CLUSTER BOOTSTRAP (SPI) EFFETS CUMULES ET SANS IMPUTATION POUR EXT
#=========================================

library(fwildclusterboot)
library(dplyr)

#--------------------------------------------------
# Préparation
#--------------------------------------------------

epi$iso    <- epi$`Country Code`
epi$iso_f  <- factor(epi$iso)
epi$year_f <- factor(epi$year)

set.seed(123)
dqrng::dqset.seed(123)

#--------------------------------------------------
# 1. Croissance du PIB
#--------------------------------------------------

m_lm_gdp <- lm(
  gdp_growth ~ spi + spi_l1 + iso_f + year_f,
  data = epi
)

# Effet contemporain
b_gdp_spi <- boottest(
  m_lm_gdp,
  param = "spi",
  clustid = "iso",
  B = 9999,
  type = "webb"
)

# Effet retardé
b_gdp_spi_l1 <- boottest(
  m_lm_gdp,
  param = "spi_l1",
  clustid = "iso",
  B = 9999,
  type = "webb"
)

# Effet cumulé
b_gdp_cum <- boottest(
  m_lm_gdp,
  param = c("spi", "spi_l1"),
  R = c(1, 1),
  r = 0,
  clustid = "iso",
  B = 9999,
  type = "webb"
)

#--------------------------------------------------
# 2. Log PIB
#--------------------------------------------------

m_lm_loggdp <- lm(
  log_gdp ~ spi + spi_l1 + iso_f + year_f,
  data = epi
)

b_loggdp_spi <- boottest(
  m_lm_loggdp,
  param = "spi",
  clustid = "iso",
  B = 9999,
  type = "webb"
)

b_loggdp_cum <- boottest(
  m_lm_loggdp,
  param = c("spi", "spi_l1"),
  R = c(1, 1),
  r = 0,
  clustid = "iso",
  B = 9999,
  type = "webb"
)

#--------------------------------------------------
# 3. EPI global
#--------------------------------------------------

m_lm_epi <- lm(
  epi_index ~ spi + spi_l1 + iso_f + year_f,
  data = epi
)

b_epi_spi <- boottest(
  m_lm_epi,
  param = "spi",
  clustid = "iso",
  B = 9999,
  type = "webb"
)

b_epi_spi_l1 <- boottest(
  m_lm_epi,
  param = "spi_l1",
  clustid = "iso",
  B = 9999,
  type = "webb"
)

b_epi_cum <- boottest(
  m_lm_epi,
  param = c("spi", "spi_l1"),
  R = c(1, 1),
  r = 0,
  clustid = "iso",
  B = 9999,
  type = "webb"
)

#--------------------------------------------------
# 4. Solde extérieur brut
#--------------------------------------------------

m_lm_ext <- lm(
  ext_balance ~ spi + spi_l1 + iso_f + year_f,
  data = epi
)

b_ext_spi_l1 <- boottest(
  m_lm_ext,
  param = "spi_l1",
  clustid = "iso",
  B = 9999,
  type = "webb"
)

b_ext_cum <- boottest(
  m_lm_ext,
  param = c("spi", "spi_l1"),
  R = c(1, 1),
  r = 0,
  clustid = "iso",
  B = 9999,
  type = "webb"
)

#--------------------------------------------------
# 5. Log solde extérieur
#--------------------------------------------------

m_lm_logext <- lm(
  log_ext ~ spi + spi_l1 + iso_f + year_f,
  data = epi
)

b_logext_spi_l1 <- boottest(
  m_lm_logext,
  param = "spi_l1",
  clustid = "iso",
  B = 9999,
  type = "webb"
)

b_logext_cum <- boottest(
  m_lm_logext,
  param = c("spi", "spi_l1"),
  R = c(1, 1),
  r = 0,
  clustid = "iso",
  B = 9999,
  type = "webb"
)

#--------------------------------------------------
# 6. Robustesse sans imputations du compte extérieur
#--------------------------------------------------

d_ni <- filter(epi, !ext_balance_imp)

m_lm_ext_noimp <- lm(
  ext_balance ~ spi + spi_l1 + iso_f + year_f,
  data = d_ni
)

b_ext_noimp <- boottest(
  m_lm_ext_noimp,
  param = "spi_l1",
  clustid = "iso",
  B = 9999,
  type = "webb"
)

#--------------------------------------------------
# Affichage
#--------------------------------------------------

summary(b_gdp_spi) #ok
summary(b_gdp_cum) #pas sign

summary(b_loggdp_spi) #ok
summary(b_loggdp_cum) #pas sign

summary(b_epi_spi) #ok
summary(b_epi_spi_l1) #pas sign
summary(b_epi_cum) #pas sign

summary(b_ext_spi_l1) #ok
summary(b_ext_cum) #ok étonnant avec p-value de 0.038

summary(b_logext_spi_l1) #ok
summary(b_logext_cum) #pas sign

summary(b_ext_noimp) #ok p-value: 0.001

#TEST DE PLACEBO (pour epi_index avec spi, spi brut avec gdp et loggdp, spi_l1 avec ext et log ext)
#=========================
# TESTS DE PLACEBO
# SPI FUTUR (t+1)
#=========================

library(fixest)
library(dplyr)
library(broom)

# Création du placebo : SPI futur
epi <- epi %>%
  arrange(`Country Code`, year) %>%
  group_by(`Country Code`) %>%
  mutate(spi_f1 = lead(spi, 1)) %>%
  ungroup()

#-------------------------
# 1. EPI global
#-------------------------

m_placebo_epi <- feols(
  epi_index ~ spi_f1 + spi + spi_l1 |
    `Country Code` + year,
  data = epi,
  cluster = ~`Country Code`
)

#-------------------------
# 2. PIB croissance
#-------------------------

m_placebo_gdp <- feols(
  gdp_growth ~ spi_f1 + spi + spi_l1 |
    `Country Code` + year,
  data = epi,
  cluster = ~`Country Code`
)

#-------------------------
# 3. PIB logarithme
#-------------------------

m_placebo_loggdp <- feols(
  log_gdp ~ spi_f1 + spi + spi_l1 |
    `Country Code` + year,
  data = epi,
  cluster = ~`Country Code`
)

#-------------------------
# 4. Solde extérieur brut
#-------------------------

m_placebo_ext <- feols(
  ext_balance ~ spi_f1 + spi + spi_l1 |
    `Country Code` + year,
  data = epi,
  cluster = ~`Country Code`
)

#-------------------------
# 5. Solde extérieur log
#-------------------------

m_placebo_logext <- feols(
  log_ext ~ spi_f1 + spi + spi_l1 |
    `Country Code` + year,
  data = epi,
  cluster = ~`Country Code`
)

#-------------------------
# Tableau de synthèse
#-------------------------

res_placebo <- bind_rows(
  tidy(m_placebo_epi)     %>% filter(term == "spi_f1") %>% mutate(modele = "epi_index"),
  tidy(m_placebo_gdp)     %>% filter(term == "spi_f1") %>% mutate(modele = "gdp_growth"),
  tidy(m_placebo_loggdp)  %>% filter(term == "spi_f1") %>% mutate(modele = "log_gdp"),
  tidy(m_placebo_ext)     %>% filter(term == "spi_f1") %>% mutate(modele = "ext_balance"),
  tidy(m_placebo_logext)  %>% filter(term == "spi_f1") %>% mutate(modele = "log_ext")
) %>%
  select(modele, estimate, std.error, statistic, p.value)

print(res_placebo)
#========================================
# RESULTAT DES TEST DE PLACEBO: Cela signifie que le SPI futur n'explique aucune de tes variables économiques contemporaines
#=============================================

#=============================================================================================================
#TEST DE ROBUSTESSE SANS EPISODE POLITIQUE ET  ASYMETRIE
#RESUME DES TESTS DE ROBUSTESSE DE LA CROISSANCE
library(dplyr); library(fixest); library(fwildclusterboot)

epi$iso    <- epi$`Country Code`
epi$iso_f  <- factor(epi$iso)
epi$year_f <- factor(epi$year)
################################### CROISSANCE
# Modèle principal (croissance)
m_gdp <- feols(gdp_growth ~ spi + spi_l1 | iso + year, data = epi, cluster = ~iso)

# B. Sans les trois épisodes politiques # okok significatif 
ex <- !((epi$iso == "GNB" & epi$year == 1998) |
          (epi$iso == "MDG" & epi$year %in% c(2002, 2009)))
sum(!ex)   # doit valoir 3
m_B <- feols(gdp_growth ~ spi + spi_l1 | iso + year, data = epi[ex, ], cluster = ~iso)

# C. SPI à la place du SPEI OK pas nécessaire
m_spi <- feols(gdp_growth ~ spi + spi_l1 | iso + year, data = epi, cluster = ~iso)

# D. Asymétrie : on sépare la partie sèche (<0) et la partie humide (>0)
epi <- epi %>%
  mutate(spi_neg    = pmin(spi, 0),    spi_pos    = pmax(spi, 0),
         spi_neg_l1 = pmin(spi_l1, 0), spi_pos_l1 = pmax(spi_l1, 0))

m_asym <- feols(gdp_growth ~ spi_neg + spi_pos + spi_neg_l1 + spi_pos_l1 | iso + year,
                data = epi, cluster = ~iso)

etable(m_gdp, m_B, m_asym) #NON VALIDE POUR ASYM

m_sym <- feols(gdp_growth ~ spei + spei_neg + spei_l1 + spei_neg_l1 | iso + year,
               data = epi, cluster = ~iso)
etable(m_sym)
############################################### EPI
# B. Sans les trois épisodes politiques # okok significatif pour spi et spi_l1
ex <- !((epi$iso == "GNB" & epi$year == 1998) |
          (epi$iso == "MDG" & epi$year %in% c(2002, 2009)))
sum(!ex)   # doit valoir 3
m_B <- feols(epi_index ~ spi + spi_l1 | iso + year, data = epi[ex, ], cluster = ~iso)

# D. Asymétrie : on sépare la partie sèche (<0) et la partie humide (>0)
epi <- epi %>%
  mutate(spi_neg    = pmin(spi, 0),    spi_pos    = pmax(spi, 0),
         spi_neg_l1 = pmin(spi_l1, 0), spi_pos_l1 = pmax(spi_l1, 0))

m_asym <- feols(epi_index ~ spi_neg + spi_pos + spi_neg_l1 + spi_pos_l1 | iso + year,
                data = epi, cluster = ~iso)

etable(m_B, m_asym) #NON VALIDE POUR ASYM
############################################### EXT_BALANCE
# B. Sans les trois épisodes politiques # okok significatif pour spi et spi_l1
ex <- !((epi$iso == "GNB" & epi$year == 1998) |
          (epi$iso == "MDG" & epi$year %in% c(2002, 2009)))
sum(!ex)   # doit valoir 3
m_B <- feols(ext_balance ~ spi_l1 + spi | iso + year, data = epi[ex, ], cluster = ~iso)

# D. Asymétrie : on sépare la partie sèche (<0) et la partie humide (>0)
epi <- epi %>%
  mutate(spi_neg    = pmin(spi, 0),    spi_pos    = pmax(spi, 0),
         spi_neg_l1 = pmin(spi_l1, 0), spi_pos_l1 = pmax(spi_l1, 0))

m_asym <- feols(ext_balance ~ spi_neg + spi_pos + spi_neg_l1 + spi_pos_l1 | iso + year,
                data = epi, cluster = ~iso)

etable(m_B, m_asym) #NON VALIDE POUR ASYM


#--------------------------------------------- GRAPHIQUES MCO régression spi 
#graphique régression partielle
library(fixest); library(ggplot2)
#######  CROISSANCE
# ligne de régression
ry <- resid(feols(gdp_growth ~ spi_l1 | iso + year, data = epi))
rx <- resid(feols(spi       ~ spi_l1 | iso + year, data = epi))
pl <- data.frame(rx, ry, iso = epi$iso, year = epi$year)

coef(lm(ry ~ rx))["rx"]  #1.000955 

g1 <- ggplot(pl, aes(rx, ry)) +
  geom_hline(yintercept = 0, colour = "grey80") +
  geom_vline(xintercept = 0, colour = "grey80") +
  geom_point(aes(colour = iso), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(x = "SPI-12, résiduel (écart à la normale du pays et de l'année)",
       y = "Croissance du PIB, résiduelle (points de %)",
       colour = "Pays") +
  theme_minimal()
g1
#graphiques des coefficients
est <- function(m, v, nom) {
  ct <- coeftable(m)[v, ]
  data.frame(spec = nom, b = ct["Estimate"],
             lo = ct["Estimate"] - qt(.975, 7) * ct["Std. Error"],
             hi = ct["Estimate"] + qt(.975, 7) * ct["Std. Error"])
}

rob <- rbind(
  est(m_gdp, "spi", "Principal"),
  est(m_B,   "spi", "Sans GNB 1998, MDG 2002 et 2009"),
  est(m_placebo_gdp,  "spi", "Avec placebo (SPI futur)")
)
rob$spec <- factor(rob$spec, levels = rev(rob$spec))

g2 <- ggplot(rob, aes(b, spec)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = lo, xmax = hi)) +
  labs(x = "Effet d'une unité de SPI sur la croissance (points de %)", y = NULL) +
  theme_minimal()
g2

#######  EPI
ry <- resid(feols(epi_index ~ spi_l1 | iso + year, data = epi))
rx <- resid(feols(spi       ~ spi_l1 | iso + year, data = epi))
pl <- data.frame(rx, ry, iso = epi$iso, year = epi$year)

coef(lm(ry ~ rx))["rx"]  # 0.014274

g1 <- ggplot(pl, aes(rx, ry)) +
  geom_hline(yintercept = 0, colour = "grey80") +
  geom_vline(xintercept = 0, colour = "grey80") +
  geom_point(aes(colour = iso), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(x = "SPI-12, résiduel (écart à la normale du pays et de l'année)",
       y = "Indice de performance économique, résiduel (points de d'indice)",
       colour = "Pays") +
  theme_minimal()
g1

#graphiques des coefficients
est <- function(m, v, nom) {
  ct <- coeftable(m)[v, ]
  data.frame(spec = nom, b = ct["Estimate"],
             lo = ct["Estimate"] - qt(.975, 7) * ct["Std. Error"],
             hi = ct["Estimate"] + qt(.975, 7) * ct["Std. Error"])
}

rob <- rbind(
  est(m_epi_index_spi, "spi", "Principal"),
  est(m_B,   "spi", "Sans GNB 1998, MDG 2002 et 2009"),
  est(m_placebo_epi,  "spi", "Avec placebo (SPI futur)")
)
rob$spec <- factor(rob$spec, levels = rev(rob$spec))

g2 <- ggplot(rob, aes(b, spec)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = lo, xmax = hi)) +
  labs(x = "Effet d'une unité de SPI sur la performance économique (points d'indice)", y = NULL) +
  theme_minimal()
g2
#######  EXT BALANCE ET  SPI_L1
ry <- resid(feols(ext_balance ~ spi | iso + year, data = epi))
rx <- resid(feols(spi_l1      ~ spi | iso + year, data = epi))

pl <- data.frame(rx, ry, iso = epi$iso, year = epi$year)

coef(lm(ry ~ rx))["rx"] # - 1,0079

g1 <- ggplot(pl, aes(rx, ry)) +
  geom_hline(yintercept = 0, colour = "grey80") +
  geom_vline(xintercept = 0, colour = "grey80") +
  geom_point(aes(colour = iso), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(x = "SPI-12 retardé, résiduel (écart à la normale du pays et de l'année)",
       y = "Solde de compte  (% PIB), résiduel (points de %)",
       colour = "Pays") +
  theme_minimal()
g1
#graphiques des coefficients
est <- function(m, v, nom) {
  ct <- coeftable(m)[v, ]
  data.frame(spec = nom, b = ct["Estimate"],
             lo = ct["Estimate"] - qt(.975, 7) * ct["Std. Error"],
             hi = ct["Estimate"] + qt(.975, 7) * ct["Std. Error"])
}

rob <- rbind(
  est(m_ext_spi, "spi_l1", "Principal"),
  est(m_B,   "spi_l1", "Sans GNB 1998, MDG 2002 et 2009"),
  est(m_placebo_ext,  "spi_l1", "Avec placebo (SPI futur)")
)
rob$spec <- factor(rob$spec, levels = rev(rob$spec))

g2 <- ggplot(rob, aes(b, spec)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = lo, xmax = hi)) +
  labs(x = "Effet d'une unité de SPI de l'année précédente sur le solde de compte courant (% PIB)", y = NULL) +
  theme_minimal()
g2

#ANALYSE CONTREFACTUELLE--------------------------------------------------------------------------

#####################  CROISSANCE
# QUESTION PRINCIPALE: les pays plus secs que les autres la même année ont-ils moins crû que les autres ?

#seuil SPEI: 0

library(dplyr); library(ggplot2) 
#PREPARATION
b0 <- unname(coef(m_gdp)["spi"])      # 1.0009      
ci <- c(lo = 0.46, hi = 1.452)        # IC bootstrap 95 % (ligne gdp_brut de ton boottest)

cf <- epi %>%
  mutate(deficit  = pmax(-spi, 0),            # SPI amené à 0 les années sèches
         perte_pp = b0 * deficit,              # points de croissance perdus
         perte_lo = ci["lo"] * deficit,
         perte_hi = ci["hi"] * deficit,
         gdp_cf   = gdp_growth + perte_pp)     # croissance sans sécheresse

# Contrôle : une année sèche coûte b0 * |SPI|, une année humide 0
cf %>% filter(year %in% c(1998, 2015)) %>% select(iso, year, spi, gdp_growth, gdp_cf) %>% head()

# Résumé par pays
cf %>% group_by(iso) %>%
  summarise(part_annees_seches = mean(spi < 0),
            perte_moy_annees_seches = mean(perte_pp[spi < 0]),
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
  labs(x = NULL, y = "Croissance perdue à cause de la sécheresse (points de %) - SPI") +
  theme_minimal()


#SPEI seuil -1
library(dplyr); library(ggplot2); library(tidyr)

b0 <- unname(coef(m_gdp)["spi"])      # 1.0009
ci <- c(lo = 0.46, hi = 1.452)        # IC bootstrap 95 % (bloc gdp_brut)

scenario <- function(df, seuil, nom) {
  df %>%
    mutate(scenario = nom,
           deficit  = pmax(seuil - spi, 0),     # part du SPI sous le seuil
           perte_pp = b0 * deficit,
           perte_lo = ci["lo"] * deficit,
           perte_hi = ci["hi"] * deficit,
           gdp_cf   = gdp_growth + perte_pp)
}

cf_all <- bind_rows(scenario(epi,  0, "A : sécheresse (seuil 0)"),
                    scenario(epi, -1, "B : sécheresse modérée à sévère (seuil -1)"))

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
  mutate(severe = spi < -1,
         iso = factor(iso, levels = rev(sort(unique(iso)))))

g_heat <- ggplot(hm, aes(year, iso, fill = perte_pp)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_point(data = filter(hm, severe), shape = 21, size = 2,
             fill = "white", colour = "grey15", stroke = 0.5) +
  scale_fill_viridis_c(option = "rocket", direction = -1, name = "Croissance\nperdue\n(points de %)") +
  scale_x_continuous(breaks = seq(1995, 2025, 5), expand = c(0, 0)) +
  labs(x = NULL, y = NULL,
       caption = "● : année avec SPI < -1 (sécheresse modérée à sévère)") +
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

#part de SPI commune
library(fixest); library(dplyr)

spi_w  <- epi$spi - ave(epi$spi, epi$iso)                 # écart à la moyenne du pays
spi_id <- resid(feols(spi ~ 1 | iso + year, data = epi))   # partie propre au pays (hors année commune)

# Part de la variation du SPI qui est commune à l'année: 27,21%
1 - var(spi_id) / var(spi_w)

# Années où plusieurs pays sont secs en même temps:2021 et 2022
epi %>% group_by(year) %>%
  summarise(n_pays_sec = sum(spi < -1), spi_moy = mean(spi)) %>%
  filter(n_pays_sec >= 3)

#EFFETS fixes pays seulement
m_noyear <- feols(gdp_growth ~ spi + spi_l1 | iso, data = epi, cluster = ~iso)
etable(m_gdp, m_noyear)

#graphique comparaison effets années et effets pays seulement sans année****************************
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
                        "EF année : effets fixes année. Coefficients : 1,001 (avec) et 0,71 (sans).")) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), legend.position = "top",
        plot.caption = element_text(hjust = 0, colour = "grey30"))
g_zone

ggsave("04_figures/contrefactuel_zone_spi.pdf", g_zone, width = 7, height = 4.8)

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
      "1.001 (with year FE) and 0.71 (without year FE)."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, colour = "grey30")
  )

g_zone

ggsave("04_figures/counterfactual_range_spi.pdf", g_zone, width = 7, height = 4.8)



#####################  EPI
# QUESTION PRINCIPALE: la performance des pays plus secs que les autres la même année a-t-elle baissée par rapport aux autres autres ?

#seuil SPI: 0

library(dplyr); library(ggplot2) 
#PREPARATION
b0 <- unname(coef(m_epi_index_spi)["spi"])      # 0.014                
ci <- c(lo = 0.002, hi = 0.029)        # IC bootstrap 95 % (ligne gdp_brut de ton boottest)

cf <- epi %>%
  mutate(deficit  = pmax(-spi, 0),            # SPI amené à 0 les années sèches
         perte_pp = b0 * deficit,              # points de croissance perdus
         perte_lo = ci["lo"] * deficit,
         perte_hi = ci["hi"] * deficit,
         epi_index_cf   = epi_index + perte_pp)     # epi index sans sécheresse

# Contrôle : une année sèche coûte b0 * |SPI|, une année humide 0
cf %>% filter(year %in% c(1998, 2015)) %>% select(iso, year, spi, epi_index, epi_index_cf) %>% head()

# Résumé par pays
cf %>% group_by(iso) %>%
  summarise(part_annees_seches = mean(spi < 0),
            perte_moy_annees_seches = mean(perte_pp[spi < 0]),
            perte_moy_toutes_annees = mean(perte_pp),
            lo = mean(perte_lo), hi = mean(perte_hi))
#graphique
#graphe
ggplot(cf, aes(year)) +
  geom_ribbon(aes(ymin = epi_index, ymax = epi_index_cf), fill = "tomato", alpha = 0.4) +
  geom_line(aes(y = epi_index), colour = "black") +
  geom_line(aes(y = epi_index_cf), colour = "tomato", linetype = "dashed") +
  facet_wrap(~ iso, ncol = 2) +
  labs(x = NULL, y = "Indice de performance économique(points d'indice)") +
  theme_minimal()

library(dplyr); library(ggplot2) #diagramme

ggplot(cf, aes(year, perte_pp)) +
  geom_col(fill = "tomato", width = 0.8) +
  facet_wrap(~ iso, ncol = 2) +
  labs(x = NULL, y = "Perte de performance économique (points d'indice) - seuil SPI 0") +
  theme_minimal()


#SPEI seuil -1
library(dplyr); library(ggplot2); library(tidyr)

b0 <- unname(coef(m_epi_index_spi)["spi"])      # 0.014                
ci <- c(lo = 0.002, hi = 0.029)        # IC bootstrap 95 % (bloc gdp_brut)

scenario <- function(df, seuil, nom) {
  df %>%
    mutate(scenario = nom,
           deficit  = pmax(seuil - spi, 0),     # part du SPI sous le seuil
           perte_pp = b0 * deficit,
           perte_lo = ci["lo"] * deficit,
           perte_hi = ci["hi"] * deficit,
           epi_index_cf   = epi_index + perte_pp)
}

cf_all <- bind_rows(scenario(epi,  0, "A : sécheresse (seuil 0)"),
                    scenario(epi, -1, "B : sécheresse modérée à sévère (seuil -1)"))

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
  labs(x = NULL, y = "Perte de performance économique (points d'indice)", fill = NULL) +
  theme_minimal() + theme(legend.position = "bottom")



#graphique heatmap : Carte de chaleur pays × année (la plus moderne)
library(dplyr); library(ggplot2)

hm <- cf_all %>%
  filter(grepl("^A", scenario)) %>%
  mutate(severe = spi < -1,
         iso = factor(iso, levels = rev(sort(unique(iso)))))

g_heat <- ggplot(hm, aes(year, iso, fill = perte_pp)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_point(data = filter(hm, severe), shape = 21, size = 2,
             fill = "white", colour = "grey15", stroke = 0.5) +
  scale_fill_viridis_c(option = "rocket", direction = -1, name = "Perte\nde\nperformance\néconomique\n(points d'indice)") +
  scale_x_continuous(breaks = seq(1995, 2025, 5), expand = c(0, 0)) +
  labs(x = NULL, y = NULL,
       caption = "● : année avec SPI < -1 (sécheresse modérée à sévère)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, colour = "grey30"))
g_heat

#graphique: Haltères (dumbbell) pour les pertes cumulées dans la période par pays
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
  labs(x = "Perte de performance économique cumulée sur 1994-2024 (points d'indice)", y = NULL, colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), legend.position = "top")
g_dumb

g_dumb <- ggplot(cum, aes(y = iso)) +
  geom_segment(aes(x = B, xend = A, yend = iso), colour = "grey75", linewidth = 1.5) +
  geom_point(aes(x = A, colour = "A: threshold 0"), size = 4) +
  geom_point(aes(x = B, colour = "B: threshold -1"), size = 4) +
  scale_colour_manual(values = c("A: threshold 0" = "#e4572e", "B: threshold -1" = "#17657d")) +
  labs(x = "Cumulative economic performance loss over 1994-2024 (% points)", y = NULL, colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), legend.position = "top")
g_dumb

#part de SPI commune
library(fixest); library(dplyr)

#EFFETS fixes pays seulement
m_noyear <- feols(epi_index ~ spi + spi_l1 | iso, data = epi, cluster = ~iso) 
etable(m_epi_index_spi, m_noyear) #noyear non significatif 

# ======> donc modèle fragile pour epi et spi passons à ext balance

#####################  EXTERNAL BALANCE
# QUESTION PRINCIPALE: les pays qui ont été plus secs en t-1 ont-ils un meilleur solde de compte courant en t ?
#Quel aurait été le solde de compte courant (en % du PIB) de ce pays en t si l’année t-1 n’avait pas été sèche (SPI au seuil choisi, 0 ou -1) ?

#seuil SPEI: 0

library(dplyr); library(ggplot2)

# PRÉPARATION
b0 <- unname(coef(m_ext_spi)["spi_l1"])     # -1,008
ci <- c(lo = -1.369, hi = -0.44)         # IC bootstrap 95 %

cf <- epi %>%
  mutate(deficit = pmax(-spi_l1, 0),                # SPI_l1 amené à 0 les années sèches
         gain_pp = -b0 * deficit,                # hausse du solde associée à la sécheresse (>= 0)
         gain_lo = -ci["hi"] * deficit,          # bornes inversées avec le signe
         gain_hi = -ci["lo"] * deficit,
         ext_cf  = ext_balance - gain_pp)        # solde sans sécheresse (plus bas que l'observé)

# CONTRÔLE : année la plus sèche et la plus humide de chaque pays # contrôle de cohérence (sanity check)
# (sèche : gain = -b0 * |SPI| ; humide : gain = 0, donc ext_cf = ext_balance)
cf %>%
  group_by(iso) %>%
  filter(spi == min(spi_l1) | spi_l1 == max(spi)) %>%
  select(iso, year, spi_l1, ext_balance, ext_cf, gain_pp) %>%
  arrange(iso, spi_l1)

# RÉSUMÉ PAR PAYS
cf %>%
  group_by(iso) %>%
  summarise(part_annees_seches    = mean(spi_l1 < 0),
            gain_moy_annees_seches = mean(gain_pp[spi_l1 < 0]),
            gain_moy_toutes_annees = mean(gain_pp),
            lo = mean(gain_lo), hi = mean(gain_hi),
            .groups = "drop")

# GRAPHIQUE 1 : solde observé vs contrefactuel
ggplot(cf, aes(year)) +
  geom_ribbon(aes(ymin = pmin(ext_balance, ext_cf),
                  ymax = pmax(ext_balance, ext_cf)),
              fill = "tomato", alpha = 0.4) +
  geom_line(aes(y = ext_balance, colour = "Observé")) +
  geom_line(aes(y = ext_cf, colour = "Sans sécheresse (contrefactuel)"),
            linetype = "dashed") +
  scale_colour_manual(values = c("Observé" = "black",
                                 "Sans sécheresse (contrefactuel)" = "tomato")) +
  facet_wrap(~ iso, ncol = 2) +
  labs(x = NULL, y = "Solde de compte courant (en % du PIB)", colour = NULL) +
  theme_minimal() + theme(legend.position = "bottom")

# GRAPHIQUE 2 : gain de solde associé à la sécheresse
ggplot(cf, aes(year, gain_pp)) +
  geom_col(fill = "tomato", width = 0.8) +
  facet_wrap(~ iso, ncol = 2) +
  labs(x = NULL,
       y = "Hausse du solde de compte courant (points de % du PIB)- SPI 0",
       caption = "Écart entre le solde observé et le solde contrefactuel sans sécheresse (SPI ramené à 0).") +
  theme_minimal()

#SPI seuil 0 et seuil -1 (effet de spi_l1 sur le solde de compte courant)
library(dplyr); library(ggplot2); library(tidyr)

b0 <- unname(coef(m_ext_spi)["spi_l1"])    # -1,008
ci <- c(lo = -1.369, hi = -0.44)           # À REMPLACER par l'IC bootstrap de spi_l1 (voir remarque 3)

scenario <- function(df, seuil, nom) {
  df %>%
    mutate(scenario = nom,
           deficit  = pmax(seuil - spi_l1, 0),     # part du SPI(t-1) sous le seuil
           gain_pp  = -b0 * deficit,               # hausse du solde associée à la sécheresse (>= 0)
           gain_lo  = -ci["hi"] * deficit,         # bornes inversées avec le signe
           gain_hi  = -ci["lo"] * deficit,
           ext_cf   = ext_balance - gain_pp)       # solde sans sécheresse (plus bas que l'observé)
}

cf_all <- bind_rows(scenario(epi,  0, "A : sécheresse (seuil 0)"),
                    scenario(epi, -1, "B : sécheresse modérée à sévère (seuil -1)"))

# Résumé par pays et par scénario
resume <- cf_all %>%
  group_by(scenario, iso) %>%
  summarise(annees_touchees = sum(deficit > 0),
            gain_moy_an     = mean(gain_pp),
            gain_cumule     = sum(gain_pp),
            lo = sum(gain_lo), hi = sum(gain_hi),
            pire_annee      = year[which.max(gain_pp)],
            pire_gain       = max(gain_pp), .groups = "drop")
print(resume, n = 20)

# Total sur le panel
cf_all %>% group_by(scenario) %>%
  summarise(gain_moy_pays_annee = mean(gain_pp),
            lo = mean(gain_lo), hi = mean(gain_hi),
            part_obs_touchees = mean(deficit > 0))

# Graphique
ggplot(cf_all, aes(year, gain_pp, fill = scenario)) +
  geom_col(position = "identity", alpha = 0.7, width = 0.8) +
  facet_wrap(~ iso, ncol = 2) +
  scale_fill_manual(values = c("tomato", "steelblue")) +
  labs(x = NULL,
       y = "Hausse du solde de compte courant (points de % du PIB) - SPI 0",
       fill = NULL,
       caption = "Sécheresse de l'année précédente (SPI t-1). Le scénario B (bleu) est contenu dans le scénario A (rouge).") +
  theme_minimal() + theme(legend.position = "bottom")



#graphique heatmap : Carte de chaleur pays × année (la plus moderne)
library(dplyr); library(ggplot2)
library(dplyr); library(ggplot2)

hm <- cf_all %>%
  filter(grepl("^A", scenario)) %>%
  mutate(severe = spi_l1 < -1,
         iso = factor(iso, levels = rev(sort(unique(iso)))))

g_heat <- ggplot(hm, aes(year, iso, fill = gain_pp)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_point(data = filter(hm, severe), shape = 21, size = 2,
             fill = "white", colour = "grey15", stroke = 0.5) +
  scale_fill_viridis_c(option = "rocket", direction = -1,
                       name = "hausse du solde\nde compte courant\n(% PIB)") +
  scale_x_continuous(breaks = seq(1995, 2025, 5), expand = c(0, 0)) +
  labs(x = NULL, y = NULL,
       caption = "● : année avec SPI_l1 < -1 (sécheresse modérée à sévère)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, colour = "grey30"))

g_heat

#DUMBELL


library(dplyr); library(tidyr); library(ggplot2); library(fixest)

# 0. DONNÉES : on garde les lignes où spi_l1 existe (le retard crée un NA la 1re année)
e <- epi %>% filter(!is.na(spi_l1), !is.na(ext_balance))

# 1. PART DE LA VARIATION DE SPI_L1 COMMUNE À L'ANNÉE
spi_w  <- e$spi_l1 - ave(e$spi_l1, e$iso)                       # écart à la moyenne du pays
spi_id <- resid(feols(spi_l1 ~ 1 | iso + year, data = e))       # partie propre au pays
1 - var(spi_id) / var(spi_w)                                    # part commune à l'année

# Années où plusieurs pays sont secs en même temps (SPI t-1 < -1)
e %>% group_by(year) %>%
  summarise(n_pays_sec = sum(spi_l1 < -1), spi_moy = mean(spi_l1)) %>%
  filter(n_pays_sec >= 3)

# 2. MODÈLES : avec EF année (m_ext_spi) et EF pays seulement
m_ext_noyear <- feols(ext_balance ~ spi + spi_l1 | iso, data = epi, cluster = ~iso)
etable(m_ext_spi, m_ext_noyear)

b_av <- unname(coef(m_ext_spi)["spi_l1"])        # avec EF année  (-1,008)
b_sa <- unname(coef(m_ext_noyear)["spi_l1"])     # sans EF année #non significatif

# 3. GAIN CUMULÉ PAR PAYS, MODÈLE ET SEUIL
# gain = -b * max(seuil - spi_l1, 0) : hausse du solde associée à la sécheresse (>= 0)
cumul <- function(b, seuil) {
  e %>% group_by(iso) %>%
    summarise(v = sum(-b * pmax(seuil - spi_l1, 0)), .groups = "drop")
}
mk <- function(b, avec) {
  full_join(cumul(b, 0)  %>% rename(A = v),
            cumul(b, -1) %>% rename(B = v), by = "iso") %>%
    mutate(avec = avec)
}
d <- bind_rows(mk(b_av, TRUE), mk(b_sa, FALSE))

ord <- d %>% filter(avec) %>% arrange(A) %>% pull(iso)          # tri par A (avec EF année)
d <- d %>%
  mutate(iso = factor(iso, levels = ord),
         cA  = ifelse(avec, "Avec EF année, seuil 0",  "Sans EF année, seuil 0"),
         cB  = ifelse(avec, "Avec EF année, seuil -1", "Sans EF année, seuil -1"))

cols <- c("Avec EF année, seuil 0"   = "#e4572e",
          "Sans EF année, seuil 0"   = "#f2a07f",
          "Avec EF année, seuil -1"  = "#17657d",
          "Sans EF année, seuil -1"  = "#7fb5c5")

# 4. HALTÈRES : modèle avec EF année
cum <- d %>% filter(avec) %>% select(iso, A, B)

# french
g_dumb <- ggplot(cum, aes(y = iso)) +
  geom_segment(aes(x = B, xend = A, yend = iso), colour = "grey75", linewidth = 1.5) +
  geom_point(aes(x = A, colour = "A : seuil 0"),  size = 4) +
  geom_point(aes(x = B, colour = "B : seuil -1"), size = 4) +
  scale_colour_manual(values = c("A : seuil 0" = "#e4572e", "B : seuil -1" = "#17657d")) +
  labs(x = "Hausse cumulée du solde de compte courant associée à la sécheresse (points de % du PIB)",
       y = NULL, colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), legend.position = "top")
g_dumb

# English
g_dumb_en <- ggplot(cum, aes(y = iso)) +
  geom_segment(aes(x = B, xend = A, yend = iso), colour = "grey75", linewidth = 1.5) +
  geom_point(aes(x = A, colour = "A: threshold 0"),  size = 4) +
  geom_point(aes(x = B, colour = "B: threshold -1"), size = 4) +
  scale_colour_manual(values = c("A: threshold 0" = "#e4572e", "B: threshold -1" = "#17657d")) +
  labs(x = "Cumulative increase in the current account balance associated with drought, 1994-2024 (% of GDP, points)",
       y = NULL, colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), legend.position = "top")
g_dumb_en



