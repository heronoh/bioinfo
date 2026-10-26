# =============================================================================
# 05_municipios.R
#
# Desmatamento por município: PRODES (polígonos anuais, somados pelo
# município em que cada polígono cai) x DETER (alertas, que já trazem o
# código IBGE do município).
#
# Entradas:
#   dados_brutos/prodes_vetor/prodes_*.csv  (baixar_dados.sh prodes_mun)
#   dados/deter_alertas_mun.csv             (03_processar_downloads.R)
#   limites municipais 2022 (IPEA/geobr; baixar_dados.sh limites)
#
# Saídas (em dados/):
#   prodes_mun.csv        área desmatada PRODES (km²) por município e ano PRODES
#   deter_prodes_mun.csv  PRODES x DETER (desmatamento) por município e ano
#
# Requer o pacote sf. Rodar a partir da raiz do repositório:
#   Rscript analises/desmatamento_v2/R/05_municipios.R
# =============================================================================

suppressMessages({
  library(dplyr)
  library(sf)
})
source("analises/desmatamento_v2/R/limites.R")
sf_use_s2(FALSE)

brutos <- "analises/desmatamento_v2/dados_brutos"
saida  <- "analises/desmatamento_v2/dados"

# --- municípios da Amazônia Legal ----------------------------------------------------
mun <- municipios() %>%
  filter(abbrev_state %in% amazonia_legal) %>%
  transmute(mun_geocod = as.character(code_muni), municipio = name_muni,
            uf = abbrev_state)
mun$area_mun_km2 <- as.numeric(st_area(mun)) / 1e6
message("municípios da Amazônia Legal: ", nrow(mun))

# --- PRODES: cada polígono vai para o município que contém um ponto interno ----------
arqs <- list.files(file.path(brutos, "prodes_vetor"), "^prodes_\\d+\\.csv$", full.names = TRUE)
if (!length(arqs)) stop("rode antes: bash analises/desmatamento_v2/baixar/baixar_dados.sh prodes_mun",
                        call. = FALSE)

ler_pagina <- function(arq) {
  d <- read.csv(arq, colClasses = "character", fileEncoding = "UTF-8")
  names(d) <- tolower(names(d))
  col_geom <- names(d)[vapply(d, function(x) any(grepl("^\\s*(MULTI)?POLYGON", head(x, 20))), logical(1))][1]
  col_ano  <- intersect(c("year", "ano"), names(d))[1]
  col_area <- intersect(c("area_km", "areakm", "area_km2", "area"), names(d))[1]
  if (anyNA(c(col_geom, col_ano, col_area)))
    stop("colunas não reconhecidas em ", arq, ": ", paste(names(d), collapse = ", "), call. = FALSE)
  pol <- st_as_sf(d[, c(col_ano, col_area, col_geom)], wkt = col_geom, crs = NA)
  pol <- ajustar_crs(pol, arq)
  pontos <- st_point_on_surface(st_transform(st_make_valid(pol), albers_brasil))
  pontos %>%
    transmute(ano = as.integer(round(as.numeric(.data[[col_ano]]))),
              area_km2 = as.numeric(.data[[col_area]]))
}

# O WFS 2.0 pode devolver as coordenadas em ordem latitude/longitude (padrão
# do EPSG para SIRGAS 2000) ou numa projeção em metros. Detecta pela faixa
# de valores e corrige.
crs_do_servico <- local({
  cap <- file.path(brutos, "prodes_vetor", "capabilities.xml")
  if (!file.exists(cap)) return(NA)
  x <- paste(readLines(cap, warn = FALSE), collapse = " ")
  m <- regmatches(x, regexpr("<(wfs:)?DefaultCRS>[^<]+</(wfs:)?DefaultCRS>", x))
  cod <- suppressWarnings(as.integer(sub(".*EPSG:+([0-9]+).*", "\\1", m)))
  if (length(cod) && !is.na(cod)) cod else NA
})
avisado <- FALSE
ajustar_crs <- function(pol, arq) {
  cx <- st_bbox(pol)
  em_graus_lonlat <- cx["xmin"] >= -80 && cx["xmax"] <= -40 && cx["ymin"] >= -20 && cx["ymax"] <= 8
  em_graus_latlon <- cx["xmin"] >= -20 && cx["xmax"] <= 8 && cx["ymin"] >= -80 && cx["ymax"] <= -40
  if (em_graus_lonlat) {
    st_crs(pol) <- 4674; caso <- "graus, longitude/latitude"
  } else if (em_graus_latlon) {
    st_geometry(pol) <- st_sfc(lapply(st_geometry(pol), function(g) {
      g[] <- lapply(g, function(p) if (is.list(p)) lapply(p, function(m) m[, 2:1]) else p[, 2:1]); g
    }), crs = 4674)
    caso <- "graus em ordem latitude/longitude (invertida para longitude/latitude)"
  } else if (!is.na(crs_do_servico)) {
    st_crs(pol) <- crs_do_servico; caso <- paste0("projeção EPSG:", crs_do_servico, " informada pelo serviço")
  } else {
    stop("não reconheci o sistema de coordenadas em ", arq, " (caixa: ",
         paste(round(cx), collapse = ", "), ")", call. = FALSE)
  }
  if (!avisado) { message("coordenadas do PRODES: ", caso); avisado <<- TRUE }
  pol
}

pontos <- do.call(rbind, lapply(arqs, function(a) { message("lendo ", basename(a)); ler_pagina(a) }))
message("polígonos PRODES: ", nrow(pontos))

prodes_mun <- st_join(pontos, mun %>% select(mun_geocod), join = st_intersects) %>%
  st_drop_geometry() %>%
  filter(!is.na(mun_geocod), ano >= 2008) %>%
  group_by(mun_geocod, ano) %>%
  summarise(prodes_km2 = sum(area_km2, na.rm = TRUE), poligonos = n(), .groups = "drop") %>%
  left_join(st_drop_geometry(mun), by = "mun_geocod") %>%
  select(mun_geocod, municipio, uf, ano, prodes_km2, poligonos, area_mun_km2) %>%
  arrange(uf, municipio, ano)

write.csv(prodes_mun, file.path(saida, "prodes_mun.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# conferência: soma por estado e ano x taxa oficial (a taxa inclui correções
# para nuvens e fica um pouco acima da soma dos polígonos mapeados)
oficial <- read.csv("aulas/r/arquivos/desmatamento_prodes_amazonia_uf.csv")
conf <- prodes_mun %>% group_by(uf, ano) %>% summarise(poligonos_km2 = sum(prodes_km2), .groups = "drop") %>%
  inner_join(oficial %>% select(uf, ano, taxa_km2 = desmatamento_km2), by = c("uf", "ano")) %>%
  group_by(ano) %>% summarise(across(c(poligonos_km2, taxa_km2), sum)) %>%
  mutate(razao = round(poligonos_km2 / taxa_km2, 3))
message("PRODES por município: soma dos polígonos / taxa oficial, por ano:")
print(as.data.frame(conf))

# --- junta com o DETER ----------------------------------------------------------------
deter_mun <- read.csv(file.path(saida, "deter_alertas_mun.csv"), colClasses = c(mun_geocod = "character")) %>%
  filter(classe %in% c("DESMATAMENTO_CR", "DESMATAMENTO_VEG", "MINERACAO")) %>%
  group_by(mun_geocod, ano = ano_prodes) %>%
  summarise(deter_km2 = sum(area_km2), alertas = sum(alertas), .groups = "drop")

comparacao <- full_join(prodes_mun, deter_mun, by = c("mun_geocod", "ano")) %>%
  filter(ano >= 2017, ano <= max(prodes_mun$ano)) %>%
  select(-municipio, -uf, -area_mun_km2) %>%
  left_join(st_drop_geometry(mun), by = "mun_geocod") %>%
  mutate(across(c(prodes_km2, deter_km2, alertas, poligonos), ~ coalesce(.x, 0))) %>%
  filter(!is.na(uf)) %>%
  select(mun_geocod, municipio, uf, ano, prodes_km2, deter_km2, poligonos, alertas, area_mun_km2) %>%
  arrange(uf, municipio, ano)

write.csv(comparacao, file.path(saida, "deter_prodes_mun.csv"), row.names = FALSE, fileEncoding = "UTF-8")
message("DETER x PRODES por município: ", nrow(comparacao), " linhas (",
        n_distinct(comparacao$mun_geocod), " municípios, anos ",
        min(comparacao$ano), "–", max(comparacao$ano), ")")
