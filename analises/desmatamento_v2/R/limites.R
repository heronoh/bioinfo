# =============================================================================
# limites.R
#
# Limites oficiais (municípios, estados, biomas, terras indígenas) via pacote
# geobr (IPEA), guardados em cache em dados_brutos/limites/ para não baixar de
# novo. Tudo é devolvido na projeção cônica de Albers do IBGE para o Brasil,
# que preserva áreas (bom para km² e para cruzar pontos e polígonos).
# =============================================================================

suppressMessages(library(sf))

albers_brasil <- paste("+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22",
                       "+x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs")

pasta_limites <- "analises/desmatamento_v2/dados_brutos/limites"

amazonia_legal <- c("AC", "AM", "AP", "MA", "MT", "PA", "RO", "RR", "TO")

limite <- function(nome, baixar) {
  dir.create(pasta_limites, showWarnings = FALSE, recursive = TRUE, mode = "0700")
  arquivo <- file.path(pasta_limites, paste0(nome, ".gpkg"))
  if (!file.exists(arquivo)) {
    if (!requireNamespace("geobr", quietly = TRUE))
      stop("instale o pacote geobr: install.packages('geobr')", call. = FALSE)
    message("baixando ", nome, " (geobr/IPEA)...")
    st_write(baixar(), arquivo, quiet = TRUE)
  }
  st_transform(st_read(arquivo, quiet = TRUE), albers_brasil)
}

municipios <- function() limite("municipios_2022", function()
  geobr::read_municipality(code_muni = "all", year = 2022, simplified = FALSE,
                           showProgress = FALSE))

estados <- function() limite("estados_2020", function()
  geobr::read_state(code_state = "all", year = 2020, simplified = FALSE,
                    showProgress = FALSE))

biomas <- function() limite("biomas_2019", function()
  geobr::read_biomes(year = 2019, simplified = FALSE, showProgress = FALSE))

terras_indigenas <- function() limite("terras_indigenas", function()
  geobr::read_indigenous_land(simplified = FALSE, showProgress = FALSE))
