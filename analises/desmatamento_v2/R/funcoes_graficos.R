# =============================================================================
# funcoes_graficos.R
#
# Tabelas e funções compartilhadas pelos gráficos da v2 (as mesmas da aula v1,
# aulas/r/desmatamento.qmd): mandatos presidenciais, período da pandemia,
# faixas e linhas dos mandatos, cores e formatação de números.
# =============================================================================

library("ggplot2")
library("dplyr")

num <- scales::label_number(big.mark = ".", decimal.mark = ",")
porcentagem <- scales::label_percent(accuracy = 0.1, decimal.mark = ",")

mandatos <- tibble::tribble(
  ~presidente,                 ~rotulo,     ~inicio,      ~fim,
  "José Sarney",               "Sarney",    "1985-03-15", "1990-03-15",
  "Fernando Collor",           "Collor",    "1990-03-15", "1992-10-02",
  "Itamar Franco",             "Itamar",    "1992-10-02", "1995-01-01", # interino até a renúncia de Collor, em 29/12/1992
  "Fernando Henrique Cardoso", "FHC I",     "1995-01-01", "1999-01-01",
  "Fernando Henrique Cardoso", "FHC II",    "1999-01-01", "2003-01-01",
  "Luiz Inácio Lula da Silva", "Lula I",    "2003-01-01", "2007-01-01",
  "Luiz Inácio Lula da Silva", "Lula II",   "2007-01-01", "2011-01-01",
  "Dilma Rousseff",            "Dilma I",   "2011-01-01", "2015-01-01",
  "Dilma Rousseff",            "Dilma II",  "2015-01-01", "2016-05-12", # até o impeachment (golpe) em 31 de agosto de 2016
  "Michel Temer",              "Temer",     "2016-05-12", "2019-01-01", # interino até 31/08/2016
  "Jair Bolsonaro",            "Bolsonaro", "2019-01-01", "2023-01-01",
  "Luiz Inácio Lula da Silva", "Lula III",  "2023-01-01", "2027-01-05"
)


# converte uma data em ano decimal
ano_decimal <- function(data) {
  data <- as.Date(data)
  as.numeric(format(data, "%Y")) + (as.numeric(format(data, "%j")) - 1) / 365
}

mandatos <- mandatos %>%
  mutate(inicio_ano = ano_decimal(inicio),
         fim_ano    = ano_decimal(fim),
         cor_faixa  = ifelse(row_number() %% 2 == 0, "grey80", "grey93"))


cor_pandemia <- "#8B0000"   # vermelho escuro

pandemia <- tibble::tribble(
  ~evento,                ~rotulo,    ~inicio,      ~fim,
  "Pandemia de COVID-19", "COVID-19", "2020-03-11", "2022-05-22"
) %>%
  mutate(inicio_ano = ano_decimal(inicio),
         fim_ano    = ano_decimal(fim))


# recorta os mandatos ao período do gráfico
mandatos_periodo <- function(de, ate) {
  mandatos %>%
    mutate(inicio_ano = pmax(inicio_ano, de - 0.5),
           fim_ano    = pmin(fim_ano,    ate + 0.5),
           meio       = (inicio_ano + fim_ano) / 2) %>%   # posição do nome
    filter(inicio_ano < fim_ano)
}

# o mesmo para o período da pandemia
pandemia_periodo <- function(de, ate) {
  pandemia %>%
    mutate(inicio_ano = pmax(inicio_ano, de - 0.5),
           fim_ano    = pmin(fim_ano,    ate + 0.5),
           meio       = (inicio_ano + fim_ano) / 2) %>%
    filter(inicio_ano < fim_ano)
}

# faixas de fundo, alternando tons de cinza, e a pandemia em vermelho
# (adicionar ANTES dos dados)
mandatos_fundo <- function(de = 1988, ate = 2025) {
  m <- mandatos_periodo(de, ate)
  # uma camada para cada tom de cinza
  faixas <- lapply(c("grey80", "grey93"), function(cor) {
    geom_rect(data = m %>% filter(cor_faixa == cor),
              aes(xmin = inicio_ano, xmax = fim_ano, ymin = -Inf, ymax = Inf),
              fill = cor,
              alpha = 0.6,
              inherit.aes = FALSE)
  })
  covid <- geom_rect(data = pandemia_periodo(de, ate),
                     aes(xmin = inicio_ano, xmax = fim_ano, ymin = -Inf, ymax = Inf),
                     fill = cor_pandemia,
                     alpha = 0.18,
                     inherit.aes = FALSE)
  c(faixas, list(covid))
}

# linhas de troca de mandato + nomes dos presidentes (adicionar NO FINAL)
# dentro = FALSE: nomes acima do gráfico; dentro = TRUE: nomes verticais dentro de cada painel
# so_no_painel: em gráficos com painéis, escreve os nomes só em um deles,
#               por exemplo so_no_painel = list(bioma = "Amazônia")
mandatos_linhas <- function(de = 1988, ate = 2025, dentro = FALSE, tamanho = 3,
                            so_no_painel = NULL) {
  m <- mandatos_periodo(de, ate)
  m_nomes <- m
  for (coluna in names(so_no_painel)) m_nomes[[coluna]] <- so_no_painel[[coluna]]
  linhas <- geom_vline(data = m %>% filter(inicio_ano > de - 0.5),
                       aes(xintercept = inicio_ano),
                       linetype = "dashed", linewidth = 0.3, colour = "grey20",
                       inherit.aes = FALSE)
  # pandemia: linha cheia no início e pontilhada no fim, em vermelho escuro
  p <- pandemia_periodo(de, ate)
  p_nomes <- p
  for (coluna in names(so_no_painel)) p_nomes[[coluna]] <- so_no_painel[[coluna]]
  linhas_covid <- list(
    geom_vline(data = p, aes(xintercept = inicio_ano),
               linewidth = 0.7, colour = cor_pandemia, inherit.aes = FALSE),
    geom_vline(data = p, aes(xintercept = fim_ano),
               linetype = "dotted", linewidth = 0.7, colour = cor_pandemia,
               inherit.aes = FALSE))
  if (dentro) {
    nomes <- geom_text(data = m_nomes,
                       aes(x = meio, y = Inf, label = rotulo),
                       angle = 90, hjust = 1.1, size = tamanho, colour = "grey20",
                       inherit.aes = FALSE)
    # nos painéis, a pandemia é marcada só pela faixa e pelas linhas vermelhas
    c(list(linhas, nomes), linhas_covid)
  } else {
    # nomes em duas alturas alternadas, para não se sobreporem nos mandatos curtos
    m_nomes$altura <- ifelse(seq_len(nrow(m_nomes)) %% 2 == 1, -0.5, -1.9)
    nomes <- geom_text(data = m_nomes,
                       aes(x = meio, y = Inf, label = rotulo, vjust = altura),
                       size = tamanho, colour = "grey20",
                       inherit.aes = FALSE)
    # "COVID-19" numa terceira linha, acima dos presidentes
    nome_covid <- geom_text(data = p_nomes,
                            aes(x = meio, y = Inf, label = rotulo),
                            vjust = -3.3, size = tamanho, fontface = "bold",
                            colour = cor_pandemia, inherit.aes = FALSE)
    c(list(linhas, nomes), linhas_covid,
      list(nome_covid,
           coord_cartesian(clip = "off"),  # permite escrever fora da área do gráfico
           # os nomes ficam entre o subtítulo e o gráfico (todo gráfico com
           # mandatos_linhas() acima do painel precisa de subtítulo)
           theme(plot.subtitle = element_text(margin = margin(b = 46)),
                 plot.margin = margin(t = 6, r = 6, b = 6, l = 6))))
  }
}

# presidente em 1º de julho de cada ano
presidente_do_ano <- function(anos) {
  sapply(anos, function(a) {
    mandatos$rotulo[mandatos$inicio_ano <= a + 0.5 & mandatos$fim_ano > a + 0.5][1]
  })
}

# partido pelo qual cada presidente foi eleito (como titular ou como vice)
partido_mandato <- c("Sarney" = "PMDB", "Collor" = "PRN", "Itamar" = "PRN",
                     "FHC I" = "PSDB", "FHC II" = "PSDB",
                     "Lula I" = "PT", "Lula II" = "PT",
                     "Dilma I" = "PT", "Dilma II" = "PT",
                     "Temer" = "PMDB", "Bolsonaro" = "PSL", "Lula III" = "PT")

# cores dos mandatos (mesmo partido = tons da mesma paleta)
cores_mandatos <- c("Sarney"    = "#fec44f", "Collor"   = "#fa9fb5",
                    "Itamar"    = "#c51b8a",
                    "FHC I"     = "#9ecae1", "FHC II"   = "#3182bd",
                    "Lula I"    = "#c7e9c0", "Lula II"  = "#a1d99b",
                    "Dilma I"   = "#74c476", "Dilma II" = "#31a354",
                    "Temer"     = "#cc8a00", "Bolsonaro" = "#807dba",
                    "Lula III"  = "#006d2c")


# --- referências das fontes, para rodapés e legendas ---------------------------
# cada gráfico cita as fontes que usa: fontes("prodes", "mandatos", nota = "...")
referencias <- list(
  prodes = c("INPE. PRODES: taxas anuais de desmatamento por corte raso na Amazônia Legal, por estado (km²/ano).",
             "https://terrabrasilis.dpi.inpe.br/downloads/"),
  prodes2026 = c("INPE. Estimativa da taxa PRODES 2026 (ago/2025–jul/2026), divulgada em 16/10/2026.",
                 NA),
  deter = c("INPE. DETER Amazônia: alertas de desmatamento e degradação (camada deter-amz:deter_amz).",
            "https://terrabrasilis.dpi.inpe.br/geoserver/deter-amz/wfs"),
  mapbiomas = c("Projeto MapBiomas. Coleção 11 da série anual de mapas de cobertura e uso da terra do Brasil: estatísticas por bioma e estado (cobertura e transições).",
                "https://brasil.mapbiomas.org/estatisticas/"),
  queimadas = c("INPE. Programa Queimadas: focos de calor anuais detectados pelo satélite de referência.",
                "https://dataserver-coids.inpe.br/queimadas/queimadas/focos/csv/anual/"),
  ibama_autos = c("Ibama. Dados abertos: autos de infração (SIFISC).",
                  "https://stibamadadosabertosprd.blob.core.windows.net/dados-abertos/dados/SIFISC/auto_infracao/auto_infracao/auto_infracao_csv.zip"),
  ibama_embargos = c("Ibama. Dados abertos: termos de embargo.",
                     "https://stibamadadosabertosprd.blob.core.windows.net/dados-abertos/dados/TERMOS_DE_EMBARGO/TERMO_EMBARGO/termo_de_embargo.csv"),
  bcb = c("Banco Central do Brasil. SGS: série 1 (dólar comercial, venda, diário) e série 433 (IPCA, variação mensal). Valores corrigidos pelo IPCA para 2025 e convertidos pelo dólar médio de 2025.",
          "https://api.bcb.gov.br/dados/serie/bcdata.sgs.433/dados?formato=csv"),
  prodes_poligonos = c("INPE. PRODES Amazônia Legal: polígonos anuais de desmatamento (serviço WFS prodes-legal-amz), somados por município.",
                       "https://terrabrasilis.dpi.inpe.br/geoserver/prodes-legal-amz/wfs"),
  deter_mun = c("INPE. DETER Amazônia: área de cada alerta no município (campo areamunkm, código IBGE do município).",
                "https://terrabrasilis.dpi.inpe.br/geoserver/deter-amz/wfs"),
  limites = c("IBGE: malhas de municípios (2022), estados (2020) e biomas (2019), na versão preparada pelo IPEA para o pacote geobr.",
              "https://www.ipea.gov.br/geobr/data_gpkg/"),
  ti = c("Funai: terras indígenas (todas as fases), versão de março de 2021 preparada pelo IPEA para o pacote geobr.",
         "https://www.ipea.gov.br/geobr/data_gpkg/indigenous_land/202103/indigenous_land_202103.gpkg"),
  mandatos = c("Mandatos presidenciais: datas de posse e de término (Presidência da República). Pandemia de COVID-19: declaração da OMS (11/03/2020) até o fim da Emergência em Saúde Pública de Importância Nacional (Portaria GM/MS nº 913/2022, vigente em 22/05/2022).",
               NA)
)

# rodapé do gráfico (texto simples, com as URLs por extenso)
fontes <- function(..., nota = NULL, largura = 150) {
  chaves <- c(...)
  stopifnot(all(chaves %in% names(referencias)))
  linhas <- vapply(seq_along(chaves), function(i) {
    r <- referencias[[chaves[i]]]
    paste0("[", i, "] ", r[1], if (!is.na(r[2])) paste0(" ", r[2]) else "")
  }, character(1))
  linhas <- c(if (!is.null(nota)) paste("Nota:", nota), "Fontes:", linhas)
  paste(unlist(lapply(linhas, strwrap, width = largura, exdent = 4)), collapse = "\n")
}

# legenda da figura em markdown (links clicáveis no HTML e no PDF)
legenda <- function(texto, ...) {
  chaves <- c(...)
  refs <- vapply(chaves, function(k) {
    r <- referencias[[k]]
    if (is.na(r[2])) r[1] else paste0(r[1], " <", r[2], ">")
  }, character(1))
  paste0(texto, " Fontes: ", paste(refs, collapse = " "))
}
