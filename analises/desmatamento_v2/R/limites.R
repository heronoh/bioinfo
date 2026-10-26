# =============================================================================
# limites.R
#
# Limites oficiais (municípios, estados, biomas, terras indígenas) nas versões
# preparadas pelo IPEA para o pacote geobr. Os arquivos são baixados direto
# pelo script de download (não precisa do pacote geobr):
#   bash analises/desmatamento_v2/baixar/baixar_dados.sh limites
#
# Tudo é devolvido na projeção cônica de Albers do IBGE para o Brasil, que
# preserva áreas (bom para km² e para cruzar pontos e polígonos).
# =============================================================================

suppressMessages(library(sf))

albers_brasil <- paste("+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22",
                       "+x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs")

pasta_limites <- "analises/desmatamento_v2/dados_brutos/limites/gpkg"

amazonia_legal <- c("AC", "AM", "AP", "MA", "MT", "PA", "RO", "RR", "TO")

ler_limite <- function(padrao) {
  arqs <- list.files(pasta_limites, padrao, full.names = TRUE)
  if (!length(arqs))
    stop("limites não encontrados (", padrao, "). Rode antes:\n",
         "  bash analises/desmatamento_v2/baixar/baixar_dados.sh limites", call. = FALSE)
  camadas <- lapply(arqs, function(a) st_transform(st_read(a, quiet = TRUE), albers_brasil))
  do.call(rbind, camadas)
}

municipios       <- function() ler_limite("^\\d{2}municipality_2022\\.gpkg$")
estados          <- function() ler_limite("^\\d{2}state_2020\\.gpkg$")
biomas           <- function() ler_limite("^biomes_2019\\.gpkg$")
terras_indigenas <- function() ler_limite("^indigenous_land_202103\\.gpkg$")
