# =============================================================================
# 03_processar_downloads.R
#
# Transforma os arquivos baixados por baixar/baixar_dados.sh em tabelas
# resumidas (por UF, bioma e ano), prontas para o painel (02_painel.R).
# Cada fonte só é processada se os arquivos dela existirem.
#
# Saídas (em dados/):
#   cambio_ipca_anual.csv    dólar médio, IPCA anual e fatores de conversão
#                            para "US$ de 2025" (ver nota abaixo)
#   queimadas_focos_uf.csv   focos de calor (satélite de referência) por
#                            UF, bioma e ano
#   deter_alertas_mun.csv    o mesmo por município e ano PRODES
#   deter_alertas_mes.csv    o mesmo por UF, classe, ano e mês
#   deter_alertas_uf.csv     área (km²) e n de alertas DETER por UF, classe,
#                            ano civil e ano PRODES (ago–jul)
#
# Nota sobre dólares: valores em reais de cada ano são primeiro corrigidos
# pelo IPCA para reais de 2025 e depois divididos pelo dólar médio de 2025.
# Assim, anos diferentes ficam comparáveis em "US$ de 2025".
#
# Rodar a partir da raiz do repositório:
#   Rscript analises/desmatamento_v2/R/03_processar_downloads.R
# =============================================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
})

brutos <- "analises/desmatamento_v2/dados_brutos"
saida  <- "analises/desmatamento_v2/dados"
ANO_BASE <- 2025

ufs <- c("ACRE"="AC","ALAGOAS"="AL","AMAPÁ"="AP","AMAZONAS"="AM","BAHIA"="BA",
         "CEARÁ"="CE","DISTRITO FEDERAL"="DF","ESPÍRITO SANTO"="ES","GOIÁS"="GO",
         "MARANHÃO"="MA","MATO GROSSO"="MT","MATO GROSSO DO SUL"="MS",
         "MINAS GERAIS"="MG","PARÁ"="PA","PARAÍBA"="PB","PARANÁ"="PR",
         "PERNAMBUCO"="PE","PIAUÍ"="PI","RIO DE JANEIRO"="RJ",
         "RIO GRANDE DO NORTE"="RN","RIO GRANDE DO SUL"="RS","RONDÔNIA"="RO",
         "RORAIMA"="RR","SANTA CATARINA"="SC","SÃO PAULO"="SP","SERGIPE"="SE",
         "TOCANTINS"="TO")
sigla_uf <- function(x) {
  x <- toupper(trimws(x))
  ifelse(x %in% ufs, x, unname(ufs[x]))
}

numero_br <- function(x) as.numeric(gsub(",", ".", gsub("\\.(?=\\d{3}(,|$))", "", x, perl = TRUE)))

# --- câmbio e IPCA (Banco Central, SGS) ----------------------------------------------
ler_sgs <- function(serie) {
  arqs <- list.files(file.path(brutos, "bcb"), paste0("^sgs_", serie, "_.*\\.csv$"),
                     full.names = TRUE)
  if (!length(arqs)) return(NULL)
  bind_rows(lapply(arqs, function(a) {
    d <- read.csv2(a, colClasses = "character", fileEncoding = "UTF-8")
    if (!all(c("data", "valor") %in% names(d))) {
      warning("ignorando ", a, ": não é a tabela do SGS (início: ",
              substr(paste(readLines(a, n = 2, warn = FALSE), collapse = " "), 1, 120),
              ")", call. = FALSE)
      return(NULL)
    }
    d
  })) %>%
    transmute(data = as.Date(data, "%d/%m/%Y"), valor = numero_br(valor)) %>%
    filter(!is.na(data), !is.na(valor)) %>%
    distinct(data, .keep_all = TRUE) %>%
    arrange(data)
}

dolar <- ler_sgs(1)
ipca  <- ler_sgs(433)

if (!is.null(dolar) && !is.null(ipca)) {
  # número-índice mensal a partir das variações (%), média por ano
  indice <- ipca %>%
    mutate(indice = cumprod(1 + valor / 100), ano = as.integer(format(data, "%Y")))
  ipca_ano <- indice %>%
    group_by(ano) %>%
    summarise(indice_medio = mean(indice),
              ipca_anual   = 100 * (prod(1 + valor / 100) - 1),
              meses = n(), .groups = "drop")

  # conferência: IPCA de 2023 foi 4,62% e de 2024, 4,83% (IBGE)
  conf <- ipca_ano %>% filter(ano %in% 2023:2024) %>% pull(ipca_anual)
  if (length(conf) == 2 && any(abs(conf - c(4.62, 4.83)) > 0.05))
    stop("a série 433 não parece ser o IPCA mensal (2023/2024 = ",
         paste(round(conf, 2), collapse = "/"), ")", call. = FALSE)

  dolar_ano <- dolar %>%
    group_by(ano = as.integer(format(data, "%Y"))) %>%
    summarise(dolar_medio = mean(valor), dias = n(), .groups = "drop")

  base_ipca  <- ipca_ano$indice_medio[ipca_ano$ano == ANO_BASE]
  base_dolar <- dolar_ano$dolar_medio[dolar_ano$ano == ANO_BASE]
  faixa <- function(x) if (length(x)) paste(range(x), collapse = "–") else "nenhum"
  if (length(base_ipca) != 1 || length(base_dolar) != 1)
    stop("faltam dados de ", ANO_BASE, " (ano-base). Anos lidos: IPCA ",
         faixa(ipca_ano$ano), "; dólar ", faixa(dolar_ano$ano),
         ".\nRode de novo: bash analises/desmatamento_v2/baixar/baixar_dados.sh cambio --forcar",
         call. = FALSE)

  cambio <- full_join(ipca_ano, dolar_ano, by = "ano") %>%
    mutate(fator_reais_base = base_ipca / indice_medio,        # R$ do ano -> R$ de 2025
           fator_usd_base   = fator_reais_base / base_dolar,   # R$ do ano -> US$ de 2025
           ano_completo     = meses == 12,
           # antes do Plano Real (jul/1994) os valores estavam em outras moedas:
           # a conversão só vale a partir do primeiro ano inteiro em reais
           fator_reais_base = ifelse(ano >= 1995, fator_reais_base, NA),
           fator_usd_base   = ifelse(ano >= 1995, fator_usd_base, NA)) %>%
    select(ano, ipca_anual, dolar_medio, fator_reais_base, fator_usd_base, ano_completo) %>%
    arrange(ano)
  write.csv(cambio, file.path(saida, "cambio_ipca_anual.csv"), row.names = FALSE,
            fileEncoding = "UTF-8")
  message("câmbio/IPCA: ", nrow(cambio), " anos (base ", ANO_BASE,
          ": US$ 1 = R$ ", round(base_dolar, 2), ")")
}

# --- focos de calor (INPE) -------------------------------------------------------------
arqs_focos <- list.files(file.path(brutos, "queimadas"), "\\.csv$", recursive = TRUE,
                         full.names = TRUE, ignore.case = TRUE)
if (length(arqs_focos)) {
  focos <- bind_rows(lapply(arqs_focos, function(a) {
    d <- read.csv(a, colClasses = "character", fileEncoding = "UTF-8")
    names(d) <- tolower(names(d))
    col_data <- intersect(c("data_hora_gmt", "datahora", "data_pas", "data"), names(d))[1]
    stopifnot(!is.na(col_data), "estado" %in% names(d))
    tibble(uf    = sigla_uf(d$estado),
           bioma = if ("bioma" %in% names(d)) d$bioma else NA_character_,
           ano   = as.integer(substr(d[[col_data]], 1, 4)))
  })) %>%
    filter(!is.na(uf), !is.na(ano)) %>%
    count(uf, bioma, ano, name = "focos")
  write.csv(focos, file.path(saida, "queimadas_focos_uf.csv"), row.names = FALSE,
            fileEncoding = "UTF-8")
  message("focos: ", sum(focos$focos), " focos -> ", nrow(focos), " linhas")
}

# --- DETER Amazônia -----------------------------------------------------------------
arqs_deter <- list.files(file.path(brutos, "deter"), "^deter_\\d+\\.csv$", full.names = TRUE)
if (length(arqs_deter)) {
  deter <- bind_rows(lapply(arqs_deter, read.csv, colClasses = "character",
                            fileEncoding = "UTF-8"))
  names(deter) <- tolower(names(deter))
  col_area <- intersect(c("areamunkm", "areauckm", "areatotalkm", "area_km"), names(deter))[1]
  col_data <- intersect(c("view_date", "date", "data"), names(deter))[1]
  stopifnot(!is.na(col_area), !is.na(col_data), "uf" %in% names(deter))
  # O gid NÃO identifica o alerta: a camada junta tabelas com numeração
  # própria (ex.: "_hist") e o mesmo número aparece em alertas diferentes
  # (datas, municípios e classes distintos). Cada linha é um alerta; só são
  # removidas as linhas iguais em tudo, exceto nos identificadores.
  ids <- intersect(c("fid", "gid"), names(deter))
  antes <- nrow(deter)
  deter <- distinct(deter, across(-all_of(ids)), .keep_all = TRUE)
  message("DETER: ", antes, " linhas -> ", nrow(deter), " alertas (removidas ",
          antes - nrow(deter), " cópias com os mesmos dados)")

  col_mun <- intersect(c("mun_geocod", "geocode", "geocodibge"), names(deter))[1]
  deter <- deter %>%
    transmute(uf     = sigla_uf(uf),
              mun_geocod = if (!is.na(col_mun)) .data[[col_mun]] else NA_character_,
              municipio  = if ("municipality" %in% names(.)) municipality else NA_character_,
              classe = if ("classname" %in% names(.)) classname else "Todas",
              data   = as.Date(substr(.data[[col_data]], 1, 10)),
              area   = as.numeric(.data[[col_area]])) %>%
    filter(!is.na(uf), !is.na(data)) %>%
    mutate(ano        = as.integer(format(data, "%Y")),
           mes        = as.integer(format(data, "%m")),
           ano_prodes = ifelse(mes >= 8, ano + 1L, ano))   # ago/2024–jul/2025 = 2025

  resumo <- deter %>%
    group_by(uf, classe, ano, ano_prodes) %>%
    summarise(alertas = n(), area_km2 = sum(area, na.rm = TRUE), .groups = "drop")
  write.csv(resumo, file.path(saida, "deter_alertas_uf.csv"), row.names = FALSE,
            fileEncoding = "UTF-8")

  # por município (código IBGE de 7 dígitos) e ano PRODES
  deter %>%
    filter(!is.na(mun_geocod), mun_geocod != "") %>%
    group_by(mun_geocod, municipio, uf, classe, ano_prodes) %>%
    summarise(alertas = n(), area_km2 = sum(area, na.rm = TRUE), .groups = "drop") %>%
    write.csv(file.path(saida, "deter_alertas_mun.csv"), row.names = FALSE,
              fileEncoding = "UTF-8")

  # por mês (para ver mudanças de tendência dentro dos mandatos)
  deter %>%
    group_by(uf, classe, ano, mes) %>%
    summarise(alertas = n(), area_km2 = sum(area, na.rm = TRUE), .groups = "drop") %>%
    write.csv(file.path(saida, "deter_alertas_mes.csv"), row.names = FALSE,
              fileEncoding = "UTF-8")
  message("DETER: ", nrow(deter), " alertas (", min(deter$data), " a ", max(deter$data),
          ") -> ", nrow(resumo), " linhas")
}
