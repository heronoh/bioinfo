# =============================================================================
# 02_painel.R
#
# Junta todas as fontes num único painel "longo":
#   uma linha por  uf x bioma x ano x variável,
#   com as colunas  valor, unidade, fonte, situacao (consolidado/estimativa).
#
# Cada nova fonte (queimadas, fiscalização, orçamento, preço da terra...)
# entra aqui como mais um bloco com as mesmas colunas. Valores monetários
# entram em reais e são convertidos para dólares num passo próprio, quando
# a tabela de câmbio e deflator estiver disponível.
#
# Rodar a partir da raiz do repositório:
#   Rscript analises/desmatamento_v2/R/02_painel.R
# =============================================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
})

pasta <- "analises/desmatamento_v2/dados"
ler <- function(arquivo) read.csv(arquivo, fileEncoding = "UTF-8")

# --- PRODES (INPE), Amazônia Legal, km² por ano ---------------------------------
prodes <- ler("aulas/r/arquivos/desmatamento_prodes_amazonia_uf.csv") %>%
  transmute(uf, bioma = "Amazônia Legal", ano,
            variavel = "desmatamento_prodes", valor = desmatamento_km2,
            unidade = "km²", fonte = "PRODES/INPE",
            situacao = "consolidado")

# estimativa PRODES 2026 (ago/2025 a jul/2026), divulgada pelo INPE em 16/10/2026
prodes_2026 <- tibble::tribble(
  ~uf,  ~valor,
  "PA", 1130, "MT", 849, "AM", 525, "RR", 286, "AC", 252,
  "RO", 149,  "MA", 128, "AP", 8,   "TO", 8
) %>%
  transmute(uf, bioma = "Amazônia Legal", ano = 2026L,
            variavel = "desmatamento_prodes", valor,
            unidade = "km²", fonte = "PRODES/INPE (estimativa de 16/10/2026)",
            situacao = "estimativa")

# --- MapBiomas: uso da terra e transições, em hectares ------------------------
uso <- ler(file.path(pasta, "mapbiomas_uso_uf.csv")) %>%
  transmute(uf, bioma, ano,
            variavel = paste0("area_", uso), valor = area_ha,
            unidade = "ha", fonte = "MapBiomas Coleção 11",
            situacao = "consolidado")

transicao <- ler(file.path(pasta, "mapbiomas_transicao_uf.csv")) %>%
  transmute(uf, bioma, ano,
            variavel = ifelse(tipo == "Regeneração",
                              paste0("regeneracao_de_", uso),
                              paste0("conversao_para_", uso)),
            valor = area_ha, unidade = "ha/ano",
            fonte = "MapBiomas Coleção 11", situacao = "consolidado")

painel <- bind_rows(prodes, prodes_2026, uso, transicao) %>%
  arrange(uf, bioma, ano, variavel)

write.csv(painel, file.path(pasta, "painel_longo.csv"), row.names = FALSE,
          fileEncoding = "UTF-8")

# dicionário das variáveis
dicionario <- painel %>%
  group_by(variavel, unidade, fonte) %>%
  summarise(anos = paste(min(ano), max(ano), sep = "–"),
            n_linhas = n(), .groups = "drop")
write.csv(dicionario, file.path(pasta, "dicionario_variaveis.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

message("painel: ", nrow(painel), " linhas, ", n_distinct(painel$variavel), " variáveis")
