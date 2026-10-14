library(readxl)
library(dplyr)
library(tidyr)

#Chargement du taux de croissance du PIB en %
GDP_growth <- read_excel(
  "01_raw_data/magic square/GDP growth.xls",
  skip = 2
)
class(GDP_growth$`1960`)
sapply(GDP_growth[, 1:30], class)

GDP_long <- GDP_growth %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "year",
    values_to = "gdp_growth"
  ) %>%
  mutate(
    year = as.integer(year),
    gdp_growth = as.numeric(gdp_growth)
  )
glimpse(GDP_long)

#Chargement du solde de compte courant en % du PIB
ext_balance <- read_excel(
  "01_raw_data/magic square/Current account balance (% GDP).xls",
  skip = 2
)
class(ext_balance$`1960`)
sapply(ext_balance[, 1:30], class)

ext_long <- ext_balance %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "year",
    values_to = "ext_balance"
  ) %>%
  mutate(
    year = as.integer(year),
    ext_balance = as.numeric(ext_balance)
  )
glimpse(ext_long)

#Chargement du taux d'inflation (indice de prix à la consommation)
inflation <- read_excel(
  "01_raw_data/magic square/Inflation consumer prices (annual %).xls",
  skip = 2
)
class(inflation$`1960`)
sapply(inflation[, 1:30], class)

infl_long <- inflation %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "year",
    values_to = "inflation"
  ) %>%
  mutate(
    year = as.integer(year),
    inflation = as.numeric(inflation)
  )
glimpse(infl_long)

#Chargement du taux de chômage
unemployment <- read_excel(
  "01_raw_data/magic square/unemployment  labor force %.xls",
  skip = 2
)
class(unemployment$`1960`)
sapply(unemployment[, 1:30], class)

unemployment_long <- unemployment %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "year",
    values_to = "unemployment"
  ) %>%
  mutate(
    year = as.integer(year),
    unemployment = as.numeric(unemployment)
  )
glimpse(unemployment_long)

#garder seulement les 4 colonnes pour les 4 bases
GDP_long <- GDP_long %>%
  select(`Country Name`,
         `Country Code`,
         year,
         gdp_growth)

ext_long <- ext_long %>%
  select(`Country Name`,
         `Country Code`,
         year,
         ext_balance)

infl_long <- infl_long %>%
  select(`Country Name`,
         `Country Code`,
         year,
         inflation)

unemployment_long <- unemployment_long %>%
  select(`Country Name`,
         `Country Code`,
         year,
         unemployment)

#fusion des 4 bases pour créer la base economic performance index epi
epi <- GDP_long %>%
  left_join(
    ext_long,
    by = c("Country Name", "Country Code", "year")
  ) %>%
  left_join(
    infl_long,
    by = c("Country Name", "Country Code", "year")
  ) %>%
  left_join(
    unemployment_long,
    by = c("Country Name", "Country Code", "year")
  )

#Flitrer les données des 21 pays ssalic
ssa_lic <- c(
  "BDI", "BFA", "CAF", "COD", "ERI",
  "ETH", "GMB", "GIN", "GNB", "LBR",
  "MDG", "MLI", "MOZ", "MWI", "NER",
  "RWA", "SLE", "SOM", "SSD", "TCD",
  "UGA"
)
epi <- epi %>%
  filter(`Country Code` %in% ssa_lic)
n_distinct(epi$`Country Code`)
colSums(is.na(epi))#vérifie les NA
epi %>%
  summarise(
    debut_gdp = min(year[!is.na(gdp_growth)]),
    debut_ext = min(year[!is.na(ext_balance)]),
    debut_infl = min(year[!is.na(inflation)]),
    debut_unemp = min(year[!is.na(unemployment)])
  ) #vérifie les années dispo => 1991 tout dispo
epi <- epi %>%
  filter(year >= 1991)#garder les données d'après 1991
colSums(is.na(epi))#vérifie les NA
epi %>%
  group_by(`Country Code`) %>%
  summarise(
    ext_na = sum(is.na(ext_balance))
  ) %>%
  arrange(desc(ext_na))#quels pays sont concernés par les données manquantes? R: 10pays
#si garder les années après 1995
epi1995 <- epi %>%
  filter(year >= 1995)
colSums(is.na(epi1995))#vérifie les NA
epi1995 %>%
  group_by(`Country Code`) %>%
  summarise(
    ext_na = sum(is.na(ext_balance))
  ) %>%
  arrange(desc(ext_na))#quels pays sont concernés par les données manquantes sur la balance ex
#résultats: 10pays
#des pays concentrent-ils la majorité des NA?
epi %>%
  group_by(`Country Code`) %>%
  summarise(
    gdp_na   = sum(is.na(gdp_growth)),
    ext_na   = sum(is.na(ext_balance)),
    infl_na  = sum(is.na(inflation)),
    unemp_na = sum(is.na(unemployment))
  ) %>%
  arrange(desc(ext_na))
#quels pays ont des données complètes?
qualité <- epi %>%
  group_by(`Country Code`) %>%
  summarise(
    complete_obs = sum(
      !is.na(gdp_growth) &
        !is.na(ext_balance) &
        !is.na(inflation) &
        !is.na(unemployment)
    ),
    total_obs = n(),
    pct_complete = round(100 * complete_obs / total_obs, 1)
  ) %>%
  arrange(pct_complete)
print(qualité, n = Inf)
#résultat: 8 pays seulement ont des données dispo sur plus de 30 ans (qualité)
pays_retenus <- qualité %>%
  filter(complete_obs >= 31) %>%
  pull(`Country Code`)#filtrer ces pays
epi <- epi %>%
  filter(`Country Code` %in% pays_retenus)
unique(epi$`Country Code`)
n_distinct(epi$`Country Code`)
colSums(is.na(epi))#vérifie les NA
epi <- epi %>%
  filter(year >= 1991)
colSums(is.na(epi))#vérifie les NA
#qualité des données pour ces pays
qualité2 <- epi %>%
  group_by(`Country Code`) %>%
  summarise(
    complete_obs = sum(
      !is.na(gdp_growth) &
        !is.na(ext_balance) &
        !is.na(inflation) &
        !is.na(unemployment)
    ),
    total_obs = n(),
    pct_complete = round(100 * complete_obs / total_obs, 1)
  ) %>%
  arrange(pct_complete)
print(qualité2, n = Inf)
#résultat: certains pays n'ont pas encore des données sur 2025 (qualité 2)
#garder les données jusqu'en 2024
epi <- epi %>%
  filter(year <= 2024)
colSums(is.na(epi))#vérifie les NA
#qualité des données pour ces pays
qualité3 <- epi %>%
  group_by(`Country Code`) %>%
  summarise(
    complete_obs = sum(
      !is.na(gdp_growth) &
        !is.na(ext_balance) &
        !is.na(inflation) &
        !is.na(unemployment)
    ),
    total_obs = n(),
    pct_complete = round(100 * complete_obs / total_obs, 1)
  ) %>%
  arrange(pct_complete)
print(qualité3, n = Inf)
#résultat: uganda inflation na 1991-> 1993, et gnb: solde de compte courant (%PIB): 1998,1999, 2000

#compléter les données
#installation package banque mondiale WDI
install.packages("WDI")
library(WDI)
ug_cpi <- WDI(country = "UGA", indicator = c(cpi_infl = "FP.CPI.TOTL.ZG",
                                             defl_infl = "NY.GDP.DEFL.KD.ZG"),
              start = 1990, end = 1995)#données déflation ok mais ne seront pas utilisées

gw_ca <- WDI(country = "GNB", indicator = c(ca = "BN.CAB.XOKA.GD.ZS"),
             start = 1996, end = 2002)#pas de données
#Données sur ext_balance de gnb trouvées dans les bases de données de la FMI
#chargement des données
library(readr)
GNB_current_account_balance_GDP <- read_csv("01_raw_data/magic square/add/GNB current account balance %GDP.csv")
View(GNB_current_account_balance_GDP)
#remplacer les valeurs manquantes de GNB dans EPI
fix <- tibble::tribble(
  ~`Country Code`, ~year, ~ext_balance_new,
  "GNB", 1998, -3.556,
  "GNB", 1999, -2.192,
  "GNB", 2000,  2.725
)
epi <- epi %>%
  left_join(fix, by = c("Country Code", "year")) %>%
  mutate(ext_balance_imp = is.na(ext_balance) & !is.na(ext_balance_new),
         ext_balance     = coalesce(ext_balance, ext_balance_new)) %>%
  select(-ext_balance_new)
# Vérification
epi %>%
  filter(`Country Code` == "GNB", year %in% 1996:2001) %>%
  select(year, ext_balance, ext_balance_imp)
# Il ne doit plus rester de NA pour GNB
sum(is.na(epi$ext_balance[epi$`Country Code` == "GNB"])) #nombre de na=0
#j'enlève les lignes avec inflation pour Uganda =NA (1991,1992,1994)
epi <- epi %>%
  filter(year >= 1994)
colSums(is.na(epi))#vérifie les NA #maintenant aucune valeurs manquantes dans les données

#définition des bornes fixes pour l'analyse
#distribution du panel
distribution <- epi %>%
  summarise(across(c(gdp_growth, ext_balance, inflation, unemployment),
                   list(min = ~min(.x, na.rm = TRUE),
                        p05 = ~quantile(.x, .05, na.rm = TRUE),
                        p95 = ~quantile(.x, .95, na.rm = TRUE),
                        max = ~max(.x, na.rm = TRUE))))
mean(epi$gdp_growth < -11, na.rm = TRUE)#vérification de la part écrêtée: 0,8% 
mean(epi$inflation   > 40,  na.rm = TRUE)#part écrêtée si borne sup 40
mean(epi$inflation   > 60,  na.rm = TRUE)#part écrêtée si borne sup 60
mean(epi$inflation   > 50,  na.rm = TRUE)#part écrêtée si borne sup 60: 0,8% 
mean(epi$inflation   < -4,  na.rm = TRUE)#part écrêtée si borne inf -4: 0,8% 
#définition des bornes avec un critère de 0,8% OK
vars <- c("gdp_growth", "ext_balance", "inflation", "unemployment")
# Bornes qui écrêtent au plus 0,8 % des obs (0,4 % de chaque côté)
sapply(epi[vars], quantile, probs = c(0.004, 0.996), na.rm = TRUE)
#vérification
bornes <- list(            # à remplacer par tes valeurs arrondies
  gdp_growth   = c(lo = -13, hi = 17),
  ext_balance  = c(lo = -18.5, hi = 3),
  inflation    = c(lo = -9,   hi = 52),
  unemployment = c(lo = 0,   hi = 8)#au lieu de prendre 6,2 on agardé 8 car le chômage bouge très peu en SSALIC (économie informelle dominant) la variable ne doit pas peser autant que les autres
)
sapply(names(bornes), function(v) {
  mean(epi[[v]] < bornes[[v]]["lo"] | epi[[v]] > bornes[[v]]["hi"], na.rm = TRUE)
})
#quelles observations sortent-elles encore de ces bornes?
epi %>%
  filter(gdp_growth < -13 | ext_balance < -18.5 | inflation > 52 | unemployment > 8) %>%
  select(`Country Code`, year, gdp_growth, ext_balance, inflation, unemployment)
#plafonner le facteur à un petit plancher
norm_fixed <- function(x, lo, hi, invert = FALSE, eps = 0.01) {
  z <- (x - lo) / (hi - lo)
  if (invert) z <- 1 - z
  pmin(pmax(z, eps), 1)
}

#normalisation par pays des indicateurs : méthode bornes (Saavedra-Rivano et Teixeira 2017) et inversion du sens de inflation et unemployment
epi <- epi %>%
  mutate(
    gdp_norm   = norm_fixed(gdp_growth,    -13,   17),
    ext_norm   = norm_fixed(ext_balance, -18.5,    3),
    infl_norm  = norm_fixed(inflation,     -9,    52, invert = TRUE),
    unemp_norm = norm_fixed(unemployment,   0,   8, invert = TRUE),
    mu         = gdp_norm * ext_norm * infl_norm * unemp_norm,
    mu_clip    = gdp_growth < -13 | ext_balance < -18.5 |
      inflation > 52 | unemployment > 8
  )

#VERIFICATION
# 1. Les scores sont bien dans [0,01 ; 1] : OK
VER1_norm <- epi %>% summarise(across(ends_with("_norm"),
                         list(min = ~min(.x, na.rm = TRUE),
                              max = ~max(.x, na.rm = TRUE))))
# 2. Les observations marquées: OK
VER2_plafond <- epi %>% filter(mu_clip) %>%
  select(`Country Code`, year, gdp_norm, ext_norm, infl_norm, unemp_norm, mu)w
#renommer mu et mu_clip
epi <- epi %>%
  rename(
    epi      = mu,
    epi_clip = mu_clip
  )
epi <- epi %>%
  rename(
    epi_index      = epi,
    epi_index_clip = epi_clip
  )

#TESTS DE SENSIBILITES si je fais un autre choix raisonnable, mes conclusions changent-elles ?
calc_index <- function(data, b) {
  norm_fixed(data$gdp_growth,   b$gdp[1], b$gdp[2]) *
    norm_fixed(data$ext_balance,  b$ext[1], b$ext[2]) *
    norm_fixed(data$inflation,    b$inf[1], b$inf[2], invert = TRUE) *
    norm_fixed(data$unemployment, b$une[1], b$une[2], invert = TRUE)
}
variantes <- list(
  ref    = list(gdp = c(-13, 17), ext = c(-18.5, 3), inf = c(-9, 52),  une = c(0, 8)),
  une62  = list(gdp = c(-13, 17), ext = c(-18.5, 3), inf = c(-9, 52),  une = c(0, 6.2)),
  une12  = list(gdp = c(-13, 17), ext = c(-18.5, 3), inf = c(-9, 52),  une = c(0, 12)),
  large  = list(gdp = c(-25, 17), ext = c(-20, 4),   inf = c(-10, 85), une = c(0, 12)),
  etroit = list(gdp = c(-2, 11),  ext = c(-16, 0.5), inf = c(-2, 33),  une = c(0, 8))
)

idx_var <- as.data.frame(lapply(variantes, function(b) calc_index(epi, b)))
# Contrôle : la variante "ref" doit reproduire exactement epi_index
all.equal(idx_var$ref, epi$epi_index)
#CLASSEMENT GLOBAL SPEARMAN: le classement des observations est très stable
# Corrélation de rang (Spearman) de chaque variante avec la référence
sapply(idx_var[-1], function(x) cor(idx_var$ref, x, method = "spearman", use = "complete.obs"))
# Écart de rang moyen : de combien de places une observation bouge-t-elle ?
sapply(idx_var[-1], function(x) mean(abs(rank(idx_var$ref) - rank(x)), na.rm = TRUE))
#CONTROLE IONTRA_PAYS: révèle une sensibilité masquée par spearman PEARSON SUR LOG
epi_cmp <- bind_cols(epi %>% select(`Country Code`, year), idx_var)
within_cor <- epi_cmp %>%
  group_by(`Country Code`) %>%
  mutate(across(c(ref, une62, une12, large, etroit), ~ log(.x) - mean(log(.x), na.rm = TRUE))) %>%
  ungroup()
sapply(within_cor[c("une62", "une12", "large", "etroit")],
       function(x) cor(within_cor$ref, x, use = "complete.obs"))
#vérification sur l'écrêtage (VARIANTE étroit éloigné)
# Part d'observations au plancher dans la variante étroite
b <- variantes$etroit
mean(norm_fixed(epi$gdp_growth,  b$gdp[1], b$gdp[2])  <= 0.01, na.rm = TRUE)
mean(norm_fixed(epi$inflation,   b$inf[1], b$inf[2], invert = TRUE) <= 0.01, na.rm = TRUE)
mean(norm_fixed(epi$ext_balance, b$ext[1], b$ext[2])  <= 0.01, na.rm = TRUE)
# Corrélation intra-pays en excluant les observations écrêtées dans etroit
clip_etroit <- epi$gdp_growth < b$gdp[1] | epi$gdp_growth > b$gdp[2] |
  epi$ext_balance < b$ext[1] | epi$ext_balance > b$ext[2] |
  epi$inflation < b$inf[1] | epi$inflation > b$inf[2]
keep <- !clip_etroit
cor(within_cor$ref[keep], within_cor$etroit[keep], use = "complete.obs")
#recentrage après filtrage
epi_cmp2 <- bind_cols(epi %>% select(`Country Code`, year), idx_var) %>%
  filter(!clip_etroit & !epi$epi_index_clip) %>%
  group_by(`Country Code`) %>%
  mutate(across(c(ref, etroit), ~ log(.x) - mean(log(.x), na.rm = TRUE))) %>%
  ungroup()
cor(epi_cmp2$ref, epi_cmp2$etroit, use = "complete.obs")

#enregistrer les résultats des tests de sensibilité--------------------------------------------------------------------------
library(knitr)
library(kableExtra)

# 1. Description des variantes
desc <- tribble(
  ~variante, ~bornes, ~description,
  
  "une62",
  "Chômage sans écrêtage",
  linebreak(
    paste(
      c(
        "Croissance : [-13 ; 17]",
        "Compte courant : [-18.5 ; 3]",
        "Inflation : [-9 ; 52]",
        "Chômage : [0 ; 6.2]"
      ),
      collapse = "\n"
    )
  ),
  
  "une12",
  "Saavedra-Rivano et Teixeira (2017)",
  linebreak(
    paste(
      c(
        "Croissance : [-13 ; 17]",
        "Compte courant : [-18.5 ; 3]",
        "Inflation : [-9 ; 52]",
        "Chômage : [0 ; 12]"
      ),
      collapse = "\n"
    )
  ),

  
  "large",
  "Larges (sans écrêtage)",
  linebreak(
    paste(
      c( 
        "Croissance : [-25 ; 17]",
         "Compte courant : [-20 ; 4]",
         "Inflation : [-10 ; 85]",
         "Chômage : [0 ; 12]"
      ),
      collapse = "\n"
    )
  ),

  "etroit",
  "Étroites (P05-P95)",
  linebreak(
    paste(
      c( 
        "Croissance : [-2 ; 11]",
        "Compte courant : [-16 ; 0.5]",
        "Inflation : [-2 ; 33]",
        "Chômage : [0 ; 8]"
      ),
      collapse = "\n"
    )
  ),
)
 
vars_cmp <- c("une62", "une12", "large", "etroit")

# 2. Part d'observations hors bornes (au moins un facteur écrêté), en %
part_ecretee <- sapply(vars_cmp, function(v) {
  b <- variantes[[v]]
  mean(epi$gdp_growth   < b$gdp[1] | epi$gdp_growth   > b$gdp[2] |
         epi$ext_balance  < b$ext[1] | epi$ext_balance  > b$ext[2] |
         epi$inflation    < b$inf[1] | epi$inflation    > b$inf[2] |
         epi$unemployment < b$une[1] | epi$unemployment > b$une[2],
       na.rm = TRUE) * 100
})

# 3. Corrélations
spear <- sapply(idx_var[vars_cmp], function(x)
  cor(idx_var$ref, x, method = "spearman", use = "complete.obs"))
ecart <- sapply(idx_var[vars_cmp], function(x)
  mean(abs(rank(idx_var$ref) - rank(x)), na.rm = TRUE))
intra <- sapply(within_cor[vars_cmp], function(x)
  cor(within_cor$ref, x, use = "complete.obs"))

# 4. Tableau de synthèse
tab_sens <- desc %>%
  mutate(
    obs_ecretees_pct = round(part_ecretee[variante], 1),
    spearman_global  = round(spear[variante], 3),
    ecart_rang_moyen = round(ecart[variante], 1),
    pearson_intra    = round(intra[variante], 3),
    seuil_09         = ifelse(pearson_intra >= 0.9, "Respecté", "Non respecté") #seuil 0,9 coefficient de corrélation
  )

# 5. Version LaTeX (sans la colonne variante, descriptions à la ligne, tableau centré)
tab_tex <- tab_sens %>%
  dplyr::select(-variante) %>%
  rename(
    `Type de bornes` = bornes,
    Description = description,
    `Obs. écrêtées (\\%)` = obs_ecretees_pct,
    `Spearman (global)` = spearman_global,
    `Écart de rang moyen` = ecart_rang_moyen,
    `Corr. intra-pays` = pearson_intra,
    `Seuil 0,9` = seuil_09
  ) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    caption = "Tests de sensibilité de l'indice de performance économique aux bornes de normalisation",
    label = "sensibilite",
    align = "lccccc"
  ) %>%
  kable_styling(
    latex_options = "hold_position",
    font_size = 9
  ) %>%
  column_spec(1, width = "5cm") %>%   # Type de bornes
  column_spec(
    2,
    width = "6cm",
    latex_column_spec = ">{\\raggedright\\arraybackslash}p{6cm}"
  ) %>%   # Description
  row_spec(1:4,
           extra_latex_after = "\\addlinespace[0.6em]")%>%
  footnote(
    general = paste(
      "Ce tableau présente les résultats des tests de sensibilité réalisés afin d'évaluer la robustesse de l'indice de performance économique vis-à-vis des bornes retenues pour la normalisation de ces composantes. Les corrélations de Spearman mesurent la stabilité du classement des pays par rapport à l'indice de référence, tandis que la corrélation intra-pays évalue la conservation des variations temporelles de l'indice. Un seuil minimal de 0,9 a été retenu a priori pour juger de la robustesse des résultats. Concernant le taux de chômage, les bornes de référence (0 à 8) ont été adaptées aux spécificités des pays d'Afrique subsaharienne à faible revenu, caractérisés par une forte prédominance de l'emploi informel et une faible couverture statistique du marché du travail. La variante dite 'étroite' génère une proportion substantielle d'observations écrêtées, ce qui explique sa corrélation intra-pays plus faible."
    ),
    general_title = "Note : ",
    footnote_as_chunk = TRUE,
    escape = FALSE,
    threeparttable = TRUE
  )
# 6. Export .tex
con <- file("02_processed_data/annexe_tests_sensibilite_french.tex", encoding = "UTF-8")
writeLines(tab_tex, con)
close(con)

file.exists("02_processed_data/annexe_tests_sensibilite.tex")
cat(tab_tex)

#création du document d'aperçu
apercu <- c(
  "\\documentclass[11pt,a4paper]{article}",
  "\\usepackage[utf8]{inputenc}",
  "\\usepackage[T1]{fontenc}",
  "\\usepackage[english]{babel}",
  "\\usepackage{booktabs}",
  "\\usepackage{threeparttable}",
  "\\usepackage{float}",
  "\\usepackage[margin=2.5cm]{geometry}",
  "",
  "\\begin{document}",
  "",
  "\\section*{Annexe : tests de sensibilité}",
  "",
  "\\input{annexe_tests_sensibilite.tex}",
  "",
  "\\end{document}"
)
con <- file("02_processed_data/apercu.tex", encoding = "UTF-8")
writeLines(apercu, con)
close(con)
#compiler en pdf
tinytex::pdflatex("02_processed_data/apercu.tex")
