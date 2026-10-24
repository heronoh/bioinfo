# =============================================================================
# 04_ibama.R
#
# Resume os autos de infração e os termos de embargo do Ibama por UF e ano.
#
# PRIVACIDADE: os arquivos do Ibama trazem nomes e CPF/CNPJ de autuados.
# Este script lê SÓ as colunas de uma lista permitida (data, UF, valor,
# área, tipo de infração); as demais colunas nem chegam a ser carregadas
# na memória. Nada que identifique pessoas é gravado.
#
# Usa apenas R base (roda com Rscript --vanilla, sem pacotes extras).
# Chamado por baixar/baixar_dados.sh; também pode ser rodado à mão:
#   Rscript --vanilla analises/desmatamento_v2/R/04_ibama.R analises/desmatamento_v2/dados_brutos/ibama
#
# Saídas (em dados/):
#   ibama_autos_uf_ano.csv     n de autos e valor (R$ nominais) por UF, ano e tipo
#   ibama_embargos_uf_ano.csv  n de embargos e área embargada (ha) por UF e ano
# =============================================================================

args  <- commandArgs(trailingOnly = TRUE)
pasta <- if (length(args)) args[1] else "analises/desmatamento_v2/dados_brutos/ibama"
saida <- "analises/desmatamento_v2/dados"
dir.create(saida, showWarnings = FALSE, recursive = TRUE)

ufs <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA",
         "PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")

# colunas que NUNCA podem ser lidas
proibidas <- "NOME|CPF|CNPJ|DOCUMENTO|ENDERECO|LOGRADOURO|BAIRRO|CEP|TELEFONE|EMAIL|E_MAIL|RG\\b|PROPRIETARIO|INFRATOR|AUTUADO|EMBARGADO|RESPONSAVEL|LATITUDE|LONGITUDE|COORD|GEOM"

# --- leitura segura --------------------------------------------------------------
cabecalho <- function(arquivo) {
  linha <- readLines(arquivo, n = 1, warn = FALSE, encoding = "bytes")
  # o cabeçalho costuma ser ASCII: a codificação é decidida por uma amostra
  # do arquivo (até 2 MB, descartando a última linha, que pode estar cortada)
  amostra <- readBin(arquivo, "raw", n = 2e6)
  amostra <- amostra[amostra != as.raw(0)]
  linhas  <- strsplit(rawToChar(amostra), "\n", fixed = TRUE, useBytes = TRUE)[[1]]
  if (length(linhas) > 1) linhas <- head(linhas, -1)
  enc   <- if (all(validUTF8(linhas))) "UTF-8" else "latin1"
  sep   <- if (lengths(regmatches(linha, gregexpr(";", linha))) >=
               lengths(regmatches(linha, gregexpr(",", linha)))) ";" else ","
  linha <- sub("^\xef\xbb\xbf", "", linha, useBytes = TRUE)   # remove BOM
  nomes <- strsplit(iconv(linha, enc, "UTF-8"), sep, fixed = TRUE)[[1]]
  nomes <- toupper(trimws(gsub('"', "", nomes, fixed = TRUE)))
  list(enc = enc, sep = sep, nomes = nomes)
}

# primeira coluna que casa com algum dos padrões (na ordem dada)
achar <- function(nomes, padroes, obrigatoria = TRUE, o_que = "") {
  for (p in padroes) {
    hit <- grep(p, nomes, value = TRUE, perl = TRUE)
    hit <- hit[!grepl(proibidas, hit)]
    if (length(hit)) return(hit[1])
  }
  if (obrigatoria)
    stop("não encontrei a coluna de ", o_que, ". Colunas disponíveis:\n  ",
         paste(nomes[!grepl(proibidas, nomes)], collapse = ", "), call. = FALSE)
  NA_character_
}

# lê só as colunas pedidas; as outras são descartadas na leitura (colClasses NULL)
ler_colunas <- function(arquivo, colunas) {
  cab <- cabecalho(arquivo)
  colunas <- colunas[!is.na(colunas)]
  stopifnot(!any(grepl(proibidas, colunas)))           # trava de segurança
  classes <- ifelse(cab$nomes %in% colunas, "character", "NULL")
  d <- read.table(arquivo, header = TRUE, sep = cab$sep, quote = "\"",
                  colClasses = classes, col.names = cab$nomes,
                  encoding = cab$enc, comment.char = "", fill = TRUE,
                  na.strings = c("", "NA"), check.names = FALSE)
  stopifnot(!any(grepl(proibidas, names(d))))
  # lê os bytes como estão (marcados com a codificação) e converte para UTF-8;
  # assim funciona em qualquer locale do servidor
  d[] <- lapply(d, enc2utf8)
  d
}

numero_br <- function(x) {                         # "1.234,56" ou "1234.56"
  x <- trimws(x)
  br <- grepl(",", x)
  x[br] <- gsub(",", ".", gsub(".", "", x[br], fixed = TRUE), fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

ano_de <- function(x) {                            # aceita AAAA-MM-DD e DD/MM/AAAA
  a <- suppressWarnings(as.integer(sub("^(\\d{4})-.*", "\\1", x)))
  b <- suppressWarnings(as.integer(sub("^\\d{2}/\\d{2}/(\\d{4}).*", "\\1", x)))
  ifelse(is.na(a) | a < 1900, b, a)
}

# --- autos de infração --------------------------------------------------------------
csvs <- list.files(file.path(pasta, "autos"), "\\.csv$", recursive = TRUE,
                   full.names = TRUE, ignore.case = TRUE)
if (!length(csvs)) stop("nenhum CSV de autos de infração em ", pasta, call. = FALSE)

autos <- do.call(rbind, lapply(csvs, function(arq) {
  n <- cabecalho(arq)$nomes
  col <- c(
    data  = achar(n, c("^DAT_HORA_AUTO_INFRACAO$", "^DAT.*AUTO", "^DT.*AUTO", "^DAT.*LAVRATURA", "^DAT"), o_que = "data"),
    uf    = achar(n, c("^UF$", "^SIG_UF$", "^SG_UF$", "^UF_"), o_que = "UF"),
    valor = achar(n, c("^VAL_AUTO_INFRACAO$", "^VAL.*AUTO", "^VALOR"), o_que = "valor"),
    tipo  = achar(n, c("^TIPO_INFRACAO$", "^DES_TIPO_INFRACAO", "TIPO.*INFRA"), obrigatoria = FALSE),
    sit   = achar(n, c("^DES_STATUS_FORMULARIO$", "STATUS", "SITUACAO"), obrigatoria = FALSE))
  d <- ler_colunas(arq, col)
  data.frame(
    uf    = toupper(trimws(d[[col["uf"]]])),
    ano   = ano_de(d[[col["data"]]]),
    tipo  = if (is.na(col["tipo"])) "Todos" else trimws(d[[col["tipo"]]]),
    valor = numero_br(d[[col["valor"]]]),
    cancelado = if (is.na(col["sit"])) FALSE else grepl("cancel", d[[col["sit"]]], ignore.case = TRUE),
    stringsAsFactors = FALSE)
}))

autos <- autos[autos$uf %in% ufs & !is.na(autos$ano) & !autos$cancelado, ]
autos$tipo[is.na(autos$tipo) | autos$tipo == ""] <- "Não informado"
autos$um <- 1
autos$valor[is.na(autos$valor)] <- 0
res_autos <- aggregate(cbind(n_autos = um, valor_reais = valor) ~ uf + ano + tipo,
                       data = autos, FUN = sum)
res_autos <- res_autos[order(res_autos$uf, res_autos$ano, res_autos$tipo), ]
write.csv(res_autos, file.path(saida, "ibama_autos_uf_ano.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
message("autos de infração: ", nrow(autos), " autos -> ", nrow(res_autos), " linhas")

# --- termos de embargo -------------------------------------------------------------
arq <- file.path(pasta, "termo_de_embargo.csv")
n <- cabecalho(arq)$nomes
col <- c(
  data = achar(n, c("^DAT_EMBARGO$", "^DAT.*EMBARGO", "^DT.*EMBARGO", "^DAT"), o_que = "data"),
  uf   = achar(n, c("^SIG_UF$", "^UF$", "^SG_UF$", "^UF_"), o_que = "UF"),
  area = achar(n, c("^QTD_AREA_EMBARGADA$", "AREA.*EMBARG", "^QTD_AREA", "AREA"), obrigatoria = FALSE),
  sit  = achar(n, c("^SIT_DESEMBARGO$", "DESEMBARGO", "SITUACAO"), obrigatoria = FALSE))
d <- ler_colunas(arq, col)
emb <- data.frame(
  uf   = toupper(trimws(d[[col["uf"]]])),
  ano  = ano_de(d[[col["data"]]]),
  area = if (is.na(col["area"])) NA_real_ else numero_br(d[[col["area"]]]),
  stringsAsFactors = FALSE)
emb <- emb[emb$uf %in% ufs & !is.na(emb$ano), ]
emb$um <- 1
emb$area[is.na(emb$area)] <- 0
res_emb <- aggregate(cbind(n_embargos = um, area_ha = area) ~ uf + ano,
                     data = emb, FUN = sum)
res_emb <- res_emb[order(res_emb$uf, res_emb$ano), ]
write.csv(res_emb, file.path(saida, "ibama_embargos_uf_ano.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
message("embargos: ", nrow(emb), " termos -> ", nrow(res_emb), " linhas")
rm(d, autos, emb); invisible(gc())
