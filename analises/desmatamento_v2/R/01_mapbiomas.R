# =============================================================================
# 01_mapbiomas.R
#
# Organiza as estatísticas do MapBiomas (Coleção 11, biomas x estados) em
# tabelas "longas", uma linha por bioma x estado x ano x categoria.
#
# Entrada: a planilha MAPBIOMAS_BRAZIL-COL.11-BIOME_STATE.xlsx, baixada em
#   https://brasil.mapbiomas.org/estatisticas/  (salve em dados_brutos/)
#
# Saídas (em dados/):
#   mapbiomas_uso_uf.csv       área de cada uso da terra (ha)
#   mapbiomas_transicao_uf.csv conversão anual de vegetação nativa em cada uso
#                              e regeneração (uso -> vegetação nativa), em ha
#
# Rodar a partir da raiz do repositório:
#   Rscript analises/desmatamento_v2/R/01_mapbiomas.R caminho/para/planilha.xlsx
# =============================================================================

suppressMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
})

args <- commandArgs(trailingOnly = TRUE)
planilha <- if (length(args)) args[1] else
  "analises/desmatamento_v2/dados_brutos/MAPBIOMAS_BRAZIL-COL.11-BIOME_STATE.xlsx"
saida <- "analises/desmatamento_v2/dados"

siglas <- c("Acre" = "AC", "Alagoas" = "AL", "Amapá" = "AP", "Amazonas" = "AM",
            "Bahia" = "BA", "Ceará" = "CE", "Distrito Federal" = "DF",
            "Espírito Santo" = "ES", "Goiás" = "GO", "Maranhão" = "MA",
            "Mato Grosso" = "MT", "Mato Grosso do Sul" = "MS",
            "Minas Gerais" = "MG", "Pará" = "PA", "Paraíba" = "PB",
            "Paraná" = "PR", "Pernambuco" = "PE", "Piauí" = "PI",
            "Rio de Janeiro" = "RJ", "Rio Grande do Norte" = "RN",
            "Rio Grande do Sul" = "RS", "Rondônia" = "RO", "Roraima" = "RR",
            "Santa Catarina" = "SC", "São Paulo" = "SP", "Sergipe" = "SE",
            "Tocantins" = "TO")

# agrupa os códigos de classe do MapBiomas em usos da terra
uso_da_classe <- function(classe, nivel1) {
  case_when(
    startsWith(nivel1, "1.")   ~ "Vegetação nativa florestal",
    startsWith(nivel1, "2.")   ~ "Vegetação nativa campestre",
    classe == 15               ~ "Pastagem",
    classe == 39               ~ "Soja",
    classe %in% c(20, 40, 41, 62, 35, 46, 47, 48) ~ "Outras lavouras",
    classe == 9                ~ "Silvicultura",
    classe == 21               ~ "Mosaico de usos",
    classe == 30               ~ "Mineração",
    classe == 24               ~ "Área urbana",
    classe %in% c(33, 31)      ~ "Água",
    TRUE                       ~ "Outros"
  )
}

# --- cobertura e uso -----------------------------------------------------------
cobertura <- read_excel(planilha, sheet = "COVERAGE_11")

uso <- cobertura %>%
  filter(class != 0) %>%                                   # "não observado"
  mutate(uso = uso_da_classe(class, class_level_1)) %>%
  select(bioma = biome, estado = state, uso, starts_with("y")) %>%
  pivot_longer(starts_with("y"), names_to = "ano", values_to = "area_ha",
               names_prefix = "y", names_transform = as.integer) %>%
  group_by(bioma, estado, ano, uso) %>%
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
  mutate(uf = siglas[estado]) %>%
  select(bioma, uf, estado, ano, uso, area_ha) %>%
  arrange(bioma, uf, ano, uso)

stopifnot(!anyNA(uso$uf))
write.csv(uso, file.path(saida, "mapbiomas_uso_uf.csv"), row.names = FALSE,
          fileEncoding = "UTF-8")

# --- transições anuais ----------------------------------------------------------
# códigos de classe -> uso (a aba de transições não traz o nível 0)
classes <- cobertura %>%
  distinct(class, class_level_0, class_level_1) %>%
  mutate(uso       = uso_da_classe(class, class_level_1),
         natural   = class_level_0 == "Natural",
         antropico = class_level_0 == "Antropic")

transicao <- read_excel(planilha, sheet = "TRANSITION_11")

# só as transições de um ano para o seguinte (p1985_1986, ..., p2024_2025)
anuais <- grep("^p\\d{4}_\\d{4}$", names(transicao), value = TRUE)
anuais <- anuais[as.integer(substr(anuais, 7, 10)) - as.integer(substr(anuais, 2, 5)) == 1]

trans <- transicao %>%
  select(bioma = biome, estado = state, class_from, class_to, all_of(anuais)) %>%
  pivot_longer(all_of(anuais), names_to = "periodo", values_to = "area_ha") %>%
  mutate(ano = as.integer(substr(periodo, 7, 10))) %>%      # ano final do par
  left_join(classes %>% select(class_from = class, uso_de = uso,
                               natural_de = natural, antropico_de = antropico),
            by = "class_from") %>%
  left_join(classes %>% select(class_to = class, uso_para = uso,
                               natural_para = natural, antropico_para = antropico),
            by = "class_to") %>%
  mutate(tipo = case_when(
    natural_de & antropico_para ~ "Conversão de vegetação nativa",
    antropico_de & natural_para ~ "Regeneração",
    TRUE                        ~ NA_character_)) %>%
  filter(!is.na(tipo)) %>%
  # na conversão interessa o destino; na regeneração, a origem
  mutate(uso = ifelse(tipo == "Regeneração", uso_de, uso_para)) %>%
  group_by(bioma, estado, ano, tipo, uso) %>%
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
  mutate(uf = siglas[estado]) %>%
  select(bioma, uf, estado, ano, tipo, uso, area_ha) %>%
  arrange(bioma, uf, ano, tipo, uso)

stopifnot(!anyNA(trans$uf))
write.csv(trans, file.path(saida, "mapbiomas_transicao_uf.csv"), row.names = FALSE,
          fileEncoding = "UTF-8")

message("uso: ", nrow(uso), " linhas; transições: ", nrow(trans), " linhas")
