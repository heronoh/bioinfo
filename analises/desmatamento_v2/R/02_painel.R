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

# --- fontes baixadas por baixar/baixar_dados.sh (entram só se existirem) ----------
opcional <- function(arquivo, f) {
  caminho <- file.path(pasta, arquivo)
  if (file.exists(caminho)) f(ler(caminho)) else NULL
}

focos <- opcional("queimadas_focos_uf.csv", function(d) d %>%
  mutate(bioma = ifelse(is.na(bioma) | bioma == "", "Não informado", bioma)) %>%
  transmute(uf, bioma, ano, variavel = "focos_calor", valor = focos,
            unidade = "focos/ano", fonte = "INPE Programa Queimadas (satélite de referência)",
            situacao = ifelse(ano >= as.integer(format(Sys.Date(), "%Y")), "parcial", "consolidado")))

deter <- opcional("deter_alertas_uf.csv", function(d) d %>%
  group_by(uf, ano = ano_prodes, classe) %>%
  summarise(valor = sum(area_km2), .groups = "drop") %>%
  transmute(uf, bioma = "Amazônia Legal", ano,
            variavel = paste0("deter_", gsub("[^a-z0-9]+", "_", tolower(iconv(classe, to = "ASCII//TRANSLIT")))),
            valor, unidade = "km² (ano PRODES)", fonte = "DETER/INPE",
            situacao = "alerta (não consolidado)"))

autos <- opcional("ibama_autos_uf_ano.csv", function(d) d %>%
  group_by(uf, ano) %>%
  summarise(n = sum(n_autos), reais = sum(valor_reais), .groups = "drop") %>%
  pivot_longer(c(n, reais), names_to = "medida", values_to = "valor") %>%
  transmute(uf, bioma = "Todos", ano,
            variavel = ifelse(medida == "n", "ibama_autos", "ibama_multas_valor"),
            valor, unidade = ifelse(medida == "n", "autos/ano", "R$ nominais"),
            fonte = "Ibama (autos de infração, dados abertos)", situacao = "consolidado"))

embargos <- opcional("ibama_embargos_uf_ano.csv", function(d) d %>%
  pivot_longer(c(n_embargos, area_ha), names_to = "medida", values_to = "valor") %>%
  transmute(uf, bioma = "Todos", ano,
            variavel = ifelse(medida == "n_embargos", "ibama_embargos", "ibama_area_embargada"),
            valor, unidade = ifelse(medida == "n_embargos", "embargos/ano", "ha"),
            fonte = "Ibama (termos de embargo, dados abertos)", situacao = "consolidado"))

painel <- bind_rows(prodes, prodes_2026, uso, transicao, focos, deter, autos, embargos)

# --- valores em reais -> US$ de 2025 (IPCA + dólar médio de 2025) ------------------
cambio <- opcional("cambio_ipca_anual.csv", identity)
if (!is.null(cambio)) {
  usd <- painel %>%
    filter(unidade == "R$ nominais") %>%
    inner_join(cambio %>% select(ano, fator_usd_base), by = "ano") %>%
    mutate(variavel = paste0(variavel, "_usd"), valor = valor * fator_usd_base,
           unidade = "US$ de 2025", fonte = paste(fonte, "+ BCB (IPCA e câmbio)")) %>%
    select(-fator_usd_base)
  painel <- bind_rows(painel, usd)
}

painel <- painel %>% arrange(uf, bioma, ano, variavel)

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
