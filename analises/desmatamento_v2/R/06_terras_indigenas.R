# =============================================================================
# 06_terras_indigenas.R
#
# Focos de calor dentro e fora de terras indígenas (TIs), por estado, bioma e
# ano. Contar TIs e focos por estado mistura tudo com o tamanho do estado;
# por isso calculamos também a DENSIDADE de focos (focos por 1.000 km²)
# dentro e fora das TIs, que é a comparação usada na literatura.
#
# Entradas:
#   dados_brutos/queimadas/**.csv  focos anuais do INPE, com latitude e longitude
#   limites de TIs (Funai, 2021), estados e biomas (IPEA/geobr; baixar_dados.sh limites)
#
# Saídas (em dados/):
#   ti_focos.csv       focos por UF, bioma, ano e dentro/fora de TI
#   ti_area_uf.csv     área do estado, área de TIs e nº de TIs por UF
#   ti_area_bioma.csv  o mesmo por bioma
#
# Requer o pacote sf. Rodar a partir da raiz do repositório:
#   Rscript analises/desmatamento_v2/R/06_terras_indigenas.R
# =============================================================================

suppressMessages({
  library(dplyr)
  library(sf)
})
source("analises/desmatamento_v2/R/limites.R")
sf_use_s2(FALSE)

brutos <- "analises/desmatamento_v2/dados_brutos"
saida  <- "analises/desmatamento_v2/dados"

# --- limites ---------------------------------------------------------------------------
ti  <- terras_indigenas() %>% st_make_valid()
uf  <- estados() %>% transmute(uf = abbrev_state) %>% st_make_valid()
bio <- biomas() %>% filter(!grepl("Costeiro", name_biome)) %>%
  transmute(bioma = name_biome) %>% st_make_valid()

col_id   <- intersect(c("code_terrai", "terrai_cod", "gid"), names(ti))[1]
col_fase <- intersect(c("fase_ti", "fase"), names(ti))[1]
ti$id_ti <- if (!is.na(col_id)) as.character(ti[[col_id]]) else as.character(seq_len(nrow(ti)))
message("terras indígenas: ", n_distinct(ti$id_ti),
        if (!is.na(col_fase)) paste0(" (fases: ", paste(names(table(ti[[col_fase]])), collapse = ", "), ")"))

# TIs dissolvidas (sem sobreposição) para medir área sem contar duas vezes
ti_uniao <- st_union(ti)

area_por <- function(regioes, chave) {
  area_total <- as.numeric(st_area(regioes)) / 1e6
  area_ti <- vapply(seq_len(nrow(regioes)), function(i) {
    x <- st_intersection(st_geometry(regioes)[i], ti_uniao)
    if (length(x)) sum(as.numeric(st_area(x))) / 1e6 else 0
  }, numeric(1))
  n_ti <- lengths(st_intersects(regioes, ti))
  tibble(!!chave := regioes[[chave]], area_km2 = area_total,
         area_ti_km2 = area_ti, n_ti = n_ti) %>%
    group_by(.data[[chave]]) %>%
    summarise(across(c(area_km2, area_ti_km2, n_ti), sum), .groups = "drop")
}

area_uf <- area_por(uf, "uf")
area_bioma <- area_por(bio, "bioma")
write.csv(area_uf, file.path(saida, "ti_area_uf.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(area_bioma, file.path(saida, "ti_area_bioma.csv"), row.names = FALSE, fileEncoding = "UTF-8")
message("área de TIs: ", round(sum(area_uf$area_ti_km2)), " km² (",
        round(100 * sum(area_uf$area_ti_km2) / sum(area_uf$area_km2), 1), "% do Brasil)")

# --- focos: dentro ou fora de TI ---------------------------------------------------------
arqs <- list.files(file.path(brutos, "queimadas"), "\\.csv$", recursive = TRUE,
                   full.names = TRUE, ignore.case = TRUE)
if (!length(arqs)) stop("nenhum CSV de focos em ", file.path(brutos, "queimadas"), call. = FALSE)

contar <- function(arq) {
  d <- read.csv(arq, colClasses = "character", fileEncoding = "UTF-8")
  names(d) <- tolower(names(d))
  col_data <- intersect(c("data_hora_gmt", "datahora", "data_pas", "data"), names(d))[1]
  stopifnot(all(c("lat", "lon") %in% names(d)), !is.na(col_data))
  p <- st_as_sf(tibble(lon = as.numeric(d$lon), lat = as.numeric(d$lat),
                       ano = as.integer(substr(d[[col_data]], 1, 4))) %>%
                  filter(!is.na(lon), !is.na(lat)),
                coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(albers_brasil)
  # estado e bioma pela posição (mesma base dos limites usados nas áreas)
  p$uf    <- uf$uf[vapply(st_intersects(p, uf), function(i) if (length(i)) i[1] else NA_integer_, integer(1))]
  p$bioma <- bio$bioma[vapply(st_intersects(p, bio), function(i) if (length(i)) i[1] else NA_integer_, integer(1))]
  p$dentro_ti <- lengths(st_intersects(p, ti)) > 0
  message(basename(arq), ": ", nrow(p), " focos, ", sum(p$dentro_ti), " em TIs")
  st_drop_geometry(p) %>% count(uf, bioma, ano, dentro_ti, name = "focos")
}

focos <- bind_rows(lapply(arqs, contar)) %>%
  filter(!is.na(uf)) %>%
  group_by(uf, bioma, ano, dentro_ti) %>%
  summarise(focos = sum(focos), .groups = "drop") %>%
  arrange(uf, bioma, ano, dentro_ti)

write.csv(focos, file.path(saida, "ti_focos.csv"), row.names = FALSE, fileEncoding = "UTF-8")
message("focos: ", sum(focos$focos), " (", round(100 * sum(focos$focos[focos$dentro_ti]) / sum(focos$focos), 1),
        "% dentro de TIs)")
