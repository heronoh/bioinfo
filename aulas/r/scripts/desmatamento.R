# Desmatamento no Brasil
#
# Script da prática: https://heronoh.github.io/bioinfo/aulas/r/desmatamento.html
# Gerado automaticamente a partir da página da prática; para mudar o
# código, edite o .qmd correspondente.

# Prática ----

## Preparação ----

### Carregar pacotes ----

# para instalar os pacotes, caso ainda não tenha (só se instala uma vez)
install.packages("ggplot2")
install.packages("dplyr")

# execute para carregar os pacotes
library("ggplot2")
library("dplyr")

# função para escrever números no padrão brasileiro (1.234,5)
num <- scales::label_number(big.mark = ".", decimal.mark = ",")

## Obtendo os dados ----

### Definindo seu diretório de trabalho ----

getwd()

setwd("")
# utilize o tab para autocompletar com o cursor entre as aspas

### Obter arquivos ----

# taxas anuais de desmatamento na Amazônia Legal (PRODES/INPE)
download.file(
  "https://raw.githubusercontent.com/heronoh/bioinfo/main/aulas/r/arquivos/desmatamento_prodes_amazonia_uf.csv",
  destfile = "desmatamento_prodes_amazonia_uf.csv",
  mode = "wb",
  method = "libcurl"
  )

# área de vegetação nativa por bioma e estado (MapBiomas, Coleção 11)
download.file(
  "https://raw.githubusercontent.com/heronoh/bioinfo/main/aulas/r/arquivos/vegetacao_nativa_mapbiomas_uf.csv",
  destfile = "vegetacao_nativa_mapbiomas_uf.csv",
  mode = "wb",
  method = "libcurl"
  )

### Ler os arquivos .csv ----

#complete usando a tecla TAB até chegar aos arquivos que você baixou
desmat <- read.csv(file = "desmatamento_prodes_amazonia_uf.csv",
                   sep = ",",
                   fileEncoding = "UTF-8"
                   ) %>%
  tibble::tibble()

veg <- read.csv(file = "vegetacao_nativa_mapbiomas_uf.csv",
                sep = ",",
                fileEncoding = "UTF-8"
                ) %>%
  tibble::tibble()

## Conhecendo a tabela ----

# as primeiras linhas
head(desmat)

# estrutura: nome, tipo e exemplos de cada coluna
str(desmat)

# quantas linhas e colunas?
dim(desmat)

# quais estados?
unique(desmat$estado)

# qual o período coberto?
range(desmat$ano)

# quantas linhas por estado? (deve ser igual ao número de anos)
table(desmat$uf)

# resumo da coluna numérica
summary(desmat$desmatamento_km2)

## Contexto: os mandatos presidenciais ----

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

mandatos

# converte uma data em ano decimal
ano_decimal <- function(data) {
  data <- as.Date(data)
  as.numeric(format(data, "%Y")) + (as.numeric(format(data, "%j")) - 1) / 365
}

mandatos <- mandatos %>%
  mutate(inicio_ano = ano_decimal(inicio),
         fim_ano    = ano_decimal(fim),
         cor_faixa  = ifelse(row_number() %% 2 == 0, "grey80", "grey93"))

mandatos %>% select(rotulo, inicio_ano, fim_ano)

cor_pandemia <- "#8B0000"   # vermelho escuro

pandemia <- tibble::tribble(
  ~evento,                ~rotulo,    ~inicio,      ~fim,
  "Pandemia de COVID-19", "COVID-19", "2020-03-11", "2022-05-22"
) %>%
  mutate(inicio_ano = ano_decimal(inicio),
         fim_ano    = ano_decimal(fim))

pandemia

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
           theme(plot.title = element_text(margin = margin(b = 44)),
                 plot.margin = margin(t = 48, r = 6, b = 6, l = 6))))
  }
}

## A série histórica da Amazônia Legal ----

total_ano <- desmat %>%
  group_by(ano) %>%
  summarise(desmatamento_km2 = sum(desmatamento_km2))

total_ano

total_ano %>% slice_max(desmatamento_km2, n = 3)
total_ano %>% slice_min(desmatamento_km2, n = 3)

total_ano %>%
  ggplot(aes(x = ano, y = desmatamento_km2, fill = desmatamento_km2)) +
  mandatos_fundo() +
  geom_col() +
  scale_fill_viridis_c(option = "rocket", direction = -1, guide = "none") +
  scale_y_continuous(labels = num,
                     limits = c(0,30000),
                     expand = F) +
  scale_x_continuous(breaks = seq(1988, 2025, by = 1)) +
  labs(title = "Taxa anual de desmatamento na Amazônia Legal (PRODES)",
       x = "Ano PRODES",
       y = "Desmatamento (km²/ano)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  mandatos_linhas()

## Comparando os estados ----

### Um gráfico para cada estado ----

desmat %>%
  ggplot(aes(x = ano, y = desmatamento_km2)) +
  mandatos_fundo() +
  geom_area(fill = "#B5442E", alpha = 0.8) +
  facet_wrap(~ estado) +
  scale_y_continuous(labels = num) +
  labs(x = "Ano PRODES",
       y = "Desmatamento (km²/ano)") +
  theme_minimal() +
  mandatos_linhas(dentro = TRUE, tamanho = 2.2)

### Mapa de calor ----

desmat %>%
  ggplot(aes(x = ano,
             y = reorder(estado, desmatamento_km2, FUN = sum),
             fill = desmatamento_km2)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "rocket", direction = -1, labels = num) +
  scale_x_continuous(breaks = seq(1988, 2025, by = 1), expand = c(0, 0)) +
  labs(x = "Ano PRODES",
       y = NULL,
       fill = "km²/ano") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  mandatos_linhas()

### Participação de cada estado ----

desmat %>%
  ggplot(aes(x = ano,
             y = desmatamento_km2,
             fill = reorder(estado, desmatamento_km2, FUN = sum))) +
  geom_col(position = "fill", width = 1) +
  scale_fill_viridis_d(option = "turbo") +
  scale_y_continuous(labels = scales::label_percent(), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1988, 2025, by = 1), expand = c(0, 0)) +
  labs(x = "Ano PRODES",
       y = "Participação no desmatamento do ano",
       fill = "Estado") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  mandatos_linhas()

## Desmatamento acumulado ----

# como funciona a soma acumulada?
c(10, 20, 5, 15)
cumsum(c(10, 20, 5, 15))

desmat_acum <- desmat %>%
  arrange(estado, ano) %>%
  group_by(estado) %>%
  mutate(acumulado_km2 = cumsum(desmatamento_km2)) %>%
  ungroup()

# conferindo um estado
desmat_acum %>% filter(uf == "MT")

### Desmatamento acumulado, ano a ano ----

acumulado_ano <- total_ano %>%
  arrange(ano) %>%
  mutate(acumulado_km2 = cumsum(desmatamento_km2),
         variacao = case_when(
           is.na(lag(desmatamento_km2))             ~ "1988 (média de 1977 a 1988)",
           desmatamento_km2 > lag(desmatamento_km2) ~ "Taxa subiu",
           TRUE                                     ~ "Taxa caiu ou se manteve"))

acumulado_ano

acumulado_ano %>%
  ggplot(aes(x = ano, y = acumulado_km2,
             fill = variacao, alpha = desmatamento_km2)) +
  mandatos_fundo() +
  geom_col(width = 0.85) +
  scale_fill_manual(values = c("Taxa caiu ou se manteve"     = "#c8a165",
                               "Taxa subiu"                  = "#5b3412",
                               "1988 (média de 1977 a 1988)" = "grey60")) +
  scale_alpha_continuous(range = c(0.35, 1), labels = num) +
  scale_y_continuous(labels = num, expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1988, 2025, by = 1), expand = c(0, 0)) +
  labs(title = "Desmatamento acumulado na Amazônia Legal desde 1988",
       x = "Ano PRODES",
       y = "Área desmatada acumulada (km²)",
       fill = "Em relação ao\nano anterior",
       alpha = "Taxa anual\n(km²/ano)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  mandatos_linhas()

### Quanto cada estado já perdeu? ----

ranking <- desmat_acum %>%
  filter(ano == max(ano)) %>%
  arrange(desc(acumulado_km2))

ranking %>% select(estado, acumulado_km2)

# total da Amazônia Legal
sum(ranking$acumulado_km2) %>% num()

ranking %>%
  ggplot(aes(x = acumulado_km2,
             y = reorder(estado, acumulado_km2))) +
  geom_col(fill = "#B5442E") +
  geom_text(aes(label = num(acumulado_km2)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(labels = num, expand = expansion(mult = c(0, 0.15))) +
  labs(x = "Desmatamento acumulado desde 1988 (km²)",
       y = NULL) +
  theme_minimal()

## Vegetação nativa em todos os biomas (MapBiomas) ----

### Conhecendo a tabela ----

head(veg)

# período coberto
range(veg$ano)

# quantos estados em cada bioma?
veg %>%
  distinct(bioma, uf) %>%
  count(bioma)

### Quanto resta em cada bioma? ----

veg_bioma <- veg %>%
  group_by(bioma, ano) %>%
  summarise(vegetacao_nativa_ha = sum(vegetacao_nativa_ha),
            area_total_ha       = sum(area_total_ha)) %>%
  ungroup() %>%
  mutate(perc_nativa = 100 * vegetacao_nativa_ha / area_total_ha)

veg_bioma %>% filter(ano %in% c(1985, 2025))

cores_biomas <- c("Amazônia" = "#1f6f43", "Cerrado" = "#c9a227",
                  "Mata Atlântica" = "#5fa35f", "Caatinga" = "#d9805f",
                  "Pampa" = "#9ec46b", "Pantanal" = "#3f8fb5")

veg_bioma %>%
  ggplot(aes(x = ano,
             y = vegetacao_nativa_ha / 1e6,
             fill = bioma,
             col = bioma)) +
  mandatos_fundo(de = 1985) +
  geom_density(stat = "identity", position = "stack", alpha = 0.85) +
  scale_fill_manual(values = cores_biomas) +
  scale_color_manual(values = cores_biomas) +
  scale_x_continuous(breaks = seq(1985, 2025, by = 1), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Vegetação nativa no Brasil, por bioma (MapBiomas)",
       x = "Ano",
       y = "Vegetação nativa (milhões de ha)",
       fill = "Bioma",
       col = "Bioma") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  mandatos_linhas(de = 1985)

veg_bioma %>%
  ggplot(aes(x = ano, y = perc_nativa, col = bioma)) +
  mandatos_fundo(de = 1985) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = cores_biomas) +
  scale_x_continuous(breaks = seq(1985, 2025, by = 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(x = "Ano",
       y = "Área do bioma com vegetação nativa (%)",
       col = "Bioma") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  mandatos_linhas(de = 1985)

### Variação acumulada por estado ----

veg_var <- veg %>%
  arrange(bioma, uf, ano) %>%
  group_by(bioma, uf) %>%
  mutate(perda_acumulada_ha = first(vegetacao_nativa_ha) - vegetacao_nativa_ha,
         perc_restante      = 100 * vegetacao_nativa_ha / first(vegetacao_nativa_ha)) %>%
  ungroup()

# os 10 recortes com maior perda acumulada até 2025
veg_var %>%
  filter(ano == max(ano)) %>%
  slice_max(perda_acumulada_ha, n = 10) %>%
  select(bioma, estado, perda_acumulada_ha, perc_restante)

#### Gráfico de densidade da perda acumulada por estado ----

veg_var %>%
  ggplot(aes(x = ano,
             y = perda_acumulada_ha / 1e6,
             fill = uf,
             col = uf)) +
  mandatos_fundo(de = 1985) +
  geom_density(stat = "identity", position = "stack", alpha = 0.85) +
  facet_wrap(~ bioma, scales = "free_y", ncol = 2) +
  scale_fill_viridis_d(option = "turbo") +
  scale_color_viridis_d(option = "turbo") +
  scale_x_continuous(breaks = seq(1985, 2025, by = 10), expand = c(0, 0)) +
  labs(x = "Ano",
       y = "Perda acumulada desde 1985 (milhões de ha)",
       fill = "UF",
       col = "UF") +
  guides(fill = guide_legend(ncol = 2), col = guide_legend(ncol = 2)) +
  theme_minimal() +
  mandatos_linhas(de = 1985, dentro = TRUE, tamanho = 2.2)

#### Mapa de calor: quanto resta em cada estado ----

veg_var %>%
  ggplot(aes(x = ano,
             y = reorder(estado, perc_restante, FUN = min),
             fill = perc_restante)) +
  geom_tile() +
  facet_grid(bioma ~ ., scales = "free_y", space = "free_y") +
  scale_fill_distiller(palette = "RdYlGn", direction = 1, limits = c(50, 115),
                       oob = scales::squish) +
  scale_x_continuous(breaks = seq(1985, 2025, by = 1), expand = c(0, 0)) +
  labs(x = "Ano",
       y = NULL,
       fill = "% da vegetação\nnativa de 1985") +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0, hjust = 0),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  mandatos_linhas(de = 1985, tamanho = 2.5,
                  so_no_painel = list(bioma = "Amazônia"))

## Extra: gráfico animado ----

# instale apenas uma vez
install.packages(c("gganimate", "gifski", "transformr", "tidyr"))

### Preparando os dados ----

library("gganimate")

# partido pelo qual cada presidente foi eleito (como titular ou como vice)
partido_mandato <- c("Sarney" = "PMDB", "Collor" = "PRN", "Itamar" = "PRN",
                     "FHC I" = "PSDB", "FHC II" = "PSDB",
                     "Lula I" = "PT", "Lula II" = "PT",
                     "Dilma I" = "PT", "Dilma II" = "PT",
                     "Temer" = "PMDB", "Bolsonaro" = "PSL", "Lula III" = "PT")

# cores dos mandatos, em ordem cronológica: governos do mesmo partido
# aparecem em tons da mesma paleta (PT em verdes, PSDB em azuis, PMDB em
# amarelos, PRN em rosas, PSL em roxo)
cores_mandatos <- c("Sarney"    = "#fec44f", "Collor"   = "#fa9fb5",
                    "Itamar"    = "#c51b8a",
                    "FHC I"     = "#9ecae1", "FHC II"   = "#3182bd",
                    "Lula I"    = "#c7e9c0", "Lula II"  = "#a1d99b",
                    "Dilma I"   = "#74c476", "Dilma II" = "#31a354",
                    "Temer"     = "#cc8a00", "Bolsonaro" = "#807dba",
                    "Lula III"  = "#006d2c")

# rótulos da legenda: "Lula I (PT)", "FHC II (PSDB)"...
rotulos_mandatos <- paste0(names(cores_mandatos), " (",
                           partido_mandato[names(cores_mandatos)], ")")
names(rotulos_mandatos) <- names(cores_mandatos)

# barras com cantos arredondados: cada retângulo vira um polígono de 20 pontos,
# 5 em cada canto, formando um quarto de círculo. Como o eixo horizontal muda a
# cada ano (por causa do view_follow), o raio na horizontal é uma pequena fração
# da maior barra do ano. "grupo" identifica cada barra (ou bloco).
arredondar <- function(dados, inicio, fim, grupo, largura = 0.425, raio = 0.16) {
  maximo <- dados %>%
    group_by(ano) %>%
    summarise(M = max(abs({{ fim }}), 1))
  angulos <- tibble::tibble(
    ponto = 1:20,
    canto = rep(1:4, each = 5),
    t = c(seq(pi, 1.5 * pi, length.out = 5), seq(1.5 * pi, 2 * pi, length.out = 5),
          seq(0, 0.5 * pi, length.out = 5),  seq(0.5 * pi, pi, length.out = 5)))
  dados %>%
    mutate(grupo = {{ grupo }},
           h0 = pmin({{ inicio }}, {{ fim }}), h1 = pmax({{ inicio }}, {{ fim }}),
           v0 = posicao - largura,            v1 = posicao + largura) %>%
    left_join(maximo, by = "ano") %>%
    mutate(rh = pmin(0.01 * M, (h1 - h0) / 2),    # raio na horizontal
           rv = raio) %>%                         # raio na vertical
    tidyr::crossing(angulos) %>%
    mutate(cv = ifelse(canto %in% c(1, 4), v0 + rv, v1 - rv),   # centro do canto
           ch = ifelse(canto %in% c(1, 2), h0 + rh, h1 - rh),
           vertical   = cv + rv * cos(t),
           horizontal = ch + rh * sin(t)) %>%
    arrange(ano, grupo, ponto)
}

# presidente em 1º de julho de cada ano
presidente_do_ano <- function(anos) {
  sapply(anos, function(a) {
    meio_do_ano <- a + 0.5
    mandatos$rotulo[mandatos$inicio_ano <= meio_do_ano & mandatos$fim_ano > meio_do_ano][1]
  })
}

corrida <- veg %>%
  group_by(uf, estado, ano) %>%
  summarise(vegetacao_nativa_ha = sum(vegetacao_nativa_ha), .groups = "drop") %>%
  arrange(uf, ano) %>%
  group_by(uf) %>%
  mutate(perda_km2 = (first(vegetacao_nativa_ha) - vegetacao_nativa_ha) / 100) %>%
  group_by(ano) %>%
  mutate(posicao = rank(-perda_km2, ties.method = "first")) %>%
  ungroup() %>%
  filter(posicao <= 12)

# vetor com o nome do presidente, usando o ano como nome
presidentes <- setNames(presidente_do_ano(1985:2025), 1985:2025)

# perda de cada estado em cada ano, atribuída ao presidente em 1º de julho
perda_anual <- veg %>%
  group_by(uf, ano) %>%
  summarise(vegetacao_nativa_ha = sum(vegetacao_nativa_ha), .groups = "drop") %>%
  arrange(uf, ano) %>%
  group_by(uf) %>%
  mutate(perda_ano_km2 = (lag(vegetacao_nativa_ha) - vegetacao_nativa_ha) / 100,
         perda_ano_km2 = coalesce(perda_ano_km2, 0)) %>%   # 1985 não tem ano anterior
  ungroup() %>%
  mutate(mandato = presidentes[as.character(ano)])

# blocos das barras: para cada ano da animação (ano) e cada estado do top 12,
# soma a perda de cada mandato até aquele ano e empilha os mandatos em ordem
blocos <- corrida %>%
  select(ano, uf, posicao) %>%
  inner_join(perda_anual %>% select(uf, ano_perda = ano, mandato, perda_ano_km2),
             by = "uf", relationship = "many-to-many") %>%
  filter(ano_perda <= ano) %>%
  mutate(mandato = factor(mandato, levels = names(cores_mandatos))) %>%
  group_by(ano, uf, posicao, mandato) %>%
  summarise(perda_mandato = sum(perda_ano_km2), .groups = "drop") %>%
  # mandatos que ainda não começaram entram com perda zero, no fim da barra:
  # assim o gganimate já "conhece" o bloco e o faz crescer suavemente
  tidyr::complete(tidyr::nesting(ano, uf, posicao), mandato,
                  fill = list(perda_mandato = 0)) %>%
  arrange(ano, uf, mandato) %>%
  group_by(ano, uf) %>%
  mutate(fim    = cumsum(perda_mandato),     # onde o bloco termina
         inicio = fim - perda_mandato) %>%    # onde o bloco começa
  ungroup() %>%
  # mandato em que o estado GANHOU vegetação: bloco claro, recuando a barra
  mutate(transparencia = ifelse(perda_mandato < 0, 0.25, 0.95))

blocos %>% filter(ano == 2025, uf == "MT", perda_mandato != 0)

# fim visível de cada barra (onde vai o número): quando a barra recua,
# o bloco mais longo vai além do total, e o número fica depois dele
corrida <- corrida %>%
  left_join(blocos %>%
              group_by(ano, uf) %>%
              summarise(fim_barra = max(pmax(inicio, fim)), .groups = "drop"),
            by = c("ano", "uf"))

# anos da pandemia (também em 1º de julho), para o destaque em vermelho
anos <- 1985:2025
eventos <- ifelse(anos + 0.5 >= pandemia$inicio_ano & anos + 0.5 < pandemia$fim_ano,
                  "Pandemia de COVID-19", "")
eventos[anos == 2020] <- "Início da pandemia de COVID-19\ne das quarentenas estaduais"
names(eventos) <- anos

eventos[eventos != ""]

corrida

### Um mini-gráfico dentro do gráfico ----

# fração da vegetação nativa de 1985 já perdida no Brasil, ano a ano
perda_brasil <- veg %>%
  group_by(ano) %>%
  summarise(vegetacao_nativa_ha = sum(vegetacao_nativa_ha)) %>%
  mutate(fracao_perdida = 1 - vegetacao_nativa_ha / first(vegetacao_nativa_ha),
         restante_km2   = vegetacao_nativa_ha / 100,
         perdida_km2    = (first(vegetacao_nativa_ha) - vegetacao_nativa_ha) / 100,
         # perda de cada ano e se ela subiu ou caiu em relação ao ano anterior,
         # como no gráfico do desmatamento acumulado da Amazônia Legal
         perda_ano_km2  = (lag(vegetacao_nativa_ha) - vegetacao_nativa_ha) / 100,
         variacao = case_when(
           is.na(lag(perda_ano_km2))           ~ "sem ano anterior",
           perda_ano_km2 > lag(perda_ano_km2)  ~ "subiu",
           TRUE                                ~ "caiu ou se manteve"),
         intensidade = scales::rescale(perda_ano_km2, to = c(0.35, 1)),
         intensidade = coalesce(intensidade, 0.6))

perda_brasil %>% filter(ano %in% c(1985, 2005, 2025))

porcentagem <- scales::label_percent(accuracy = 0.1, decimal.mark = ",")

# posição padrão do mini-gráfico (argumentos de camadas_inset(), abaixo):
#   na vertical, da posição 7,75 (topo) à 12,45 (base, abaixo da 12ª barra);
#   na horizontal, de 55% a 97% da maior barra do ano

# calcula os retângulos do mini-gráfico para cada quadro (ano) da corrida
dados_inset <- function(corrida, topo_inset, base_inset, esq_inset, dir_inset) {
  maximo <- corrida %>%
    group_by(ano) %>%
    summarise(M = max(perda_km2, 1))         # maior barra do ano
  anos_inset <- perda_brasil$ano
  largura <- (dir_inset - esq_inset) / length(anos_inset)

  expand.grid(ano = maximo$ano, ano_inset = anos_inset) %>%
    left_join(maximo, by = "ano") %>%
    left_join(perda_brasil %>% select(ano_inset = ano, fracao_perdida,
                                      restante_km2, perdida_km2,
                                      variacao, intensidade),
              by = "ano_inset") %>%
    # anos que ainda não chegaram ficam com altura zero
    mutate(fracao    = ifelse(ano_inset <= ano, fracao_perdida, 0),
           esquerda  = M * (esq_inset + (ano_inset - min(anos_inset)) * largura),
           direita   = esquerda + M * largura,
           alto      = base_inset - fracao * (base_inset - topo_inset))
}

# camadas do ggplot2 com o mini-gráfico
# (os argumentos permitem mudar a posição do mini-gráfico em cada animação)
camadas_inset <- function(corrida,
                          topo_inset = 7.75, base_inset = 12.45,
                          esq_inset = 0.55, dir_inset = 0.97) {
  inset <- dados_inset(corrida, topo_inset, base_inset, esq_inset, dir_inset)
  caixa <- inset %>%
    filter(ano_inset == ano) %>%
    mutate(pct_restante = porcentagem(1 - fracao),
           km2_restante = paste(num(round(restante_km2)), "km²"),
           pct_perdida  = porcentagem(fracao),
           km2_perdida  = paste(num(round(perdida_km2)), "km²"),
           # como fatores, o gganimate troca os textos de um ano para o outro
           # (como texto simples, ele tentaria "misturá-los" como se fossem cores)
           across(c(pct_restante, km2_restante, pct_perdida, km2_perdida), factor))

  # trocas de governo: uma linha por quadro (ano) e mandato
  largura <- (dir_inset - esq_inset) / length(perda_brasil$ano)
  transicoes <- expand.grid(ano = caixa$ano, i = seq_len(nrow(mandatos))) %>%
    left_join(caixa %>% select(ano, M), by = "ano") %>%
    mutate(rotulo     = mandatos$rotulo[i],
           inicio     = pmax(mandatos$inicio_ano[i], min(perda_brasil$ano)),
           horizontal = M * (esq_inset + (inicio - min(perda_brasil$ano)) * largura),
           # a linha aparece quando o mandato começa e fica até o fim
           linha = ifelse(inicio <= ano + 0.5 & inicio > min(perda_brasil$ano) + 0.5, 0.8, 0),
           # o nome aparece no início do mandato e some depois de 2 anos
           nome  = ifelse(presidentes[as.character(ano)] == rotulo &
                          ano + 0.5 < inicio + 2, 1, 0),
           # perto do fim do mini-gráfico, o nome fica à esquerda da linha
           lado  = ifelse(inicio > 2013, 1.08, -0.08))
  list(
    # fundo verde: a vegetação nativa de 1985 inteira (100%)
    geom_rect(data = caixa,
              aes(xmin = topo_inset, xmax = base_inset,
                  ymin = M * esq_inset, ymax = M * dir_inset),
              fill = "#d4ebd0", colour = NA,
              inherit.aes = FALSE),
    # área perdida, acumulando ano a ano: uma barra por ano, marrom claro quando
    # a perda anual caiu, marrom escuro quando subiu, com intensidade
    # proporcional à perda daquele ano (uma camada para cada cor)
    # fundo branco sob as barras, para o verde não aparecer através delas
    geom_rect(data = inset,
              aes(xmin = alto, xmax = base_inset, ymin = esquerda, ymax = direita,
                  group = ano_inset),
              fill = "white", inherit.aes = FALSE),
    geom_rect(data = inset %>% filter(variacao == "caiu ou se manteve"),
              aes(xmin = alto, xmax = base_inset, ymin = esquerda, ymax = direita,
                  alpha = intensidade, group = ano_inset),
              fill = "#c8a165", inherit.aes = FALSE),
    geom_rect(data = inset %>% filter(variacao == "subiu"),
              aes(xmin = alto, xmax = base_inset, ymin = esquerda, ymax = direita,
                  alpha = intensidade, group = ano_inset),
              fill = "#5b3412", inherit.aes = FALSE),
    geom_rect(data = inset %>% filter(variacao == "sem ano anterior"),
              aes(xmin = alto, xmax = base_inset, ymin = esquerda, ymax = direita,
                  alpha = intensidade, group = ano_inset),
              fill = "grey60", inherit.aes = FALSE),
    # títulos e rótulos do mini-gráfico
    geom_text(data = caixa,
              aes(x = topo_inset - 0.85, y = M * esq_inset,
                  label = "Vegetação nativa do Brasil (1985 = 100%)"),
              hjust = 0, size = 4, fontface = "bold", inherit.aes = FALSE),
    geom_text(data = caixa,
              aes(x = topo_inset - 0.35, y = M * esq_inset,
                  label = "perda anual: clara quando caiu, escura quando subiu"),
              hjust = 0, size = 3.2, colour = "#5b3412", inherit.aes = FALSE),
    # área restante e perdida, em três colunas alinhadas: nome (à esquerda),
    # porcentagem e km² (alinhados à direita), em posições proporcionais à
    # largura do mini-gráfico
    geom_text(data = caixa,
              aes(x = topo_inset + 0.35, y = M * (esq_inset + 0.02 * (dir_inset - esq_inset)),
                  label = "Área restante:"),
              hjust = 0, size = 3.6, fontface = "bold", colour = "grey15",
              inherit.aes = FALSE),
    geom_text(data = caixa,
              aes(x = topo_inset + 0.85, y = M * (esq_inset + 0.02 * (dir_inset - esq_inset)),
                  label = "Área perdida:"),
              hjust = 0, size = 3.6, fontface = "bold", colour = "grey15",
              inherit.aes = FALSE),
    geom_text(data = caixa,
              aes(x = topo_inset + 0.35, y = M * (esq_inset + 0.52 * (dir_inset - esq_inset)),
                  label = pct_restante),
              hjust = 1, size = 3.6, colour = "grey15", inherit.aes = FALSE),
    geom_text(data = caixa,
              aes(x = topo_inset + 0.85, y = M * (esq_inset + 0.52 * (dir_inset - esq_inset)),
                  label = pct_perdida),
              hjust = 1, size = 3.6, colour = "grey15", inherit.aes = FALSE),
    geom_text(data = caixa,
              aes(x = topo_inset + 0.35, y = M * (esq_inset + 0.96 * (dir_inset - esq_inset)),
                  label = km2_restante),
              hjust = 1, size = 3.6, colour = "grey15", inherit.aes = FALSE),
    geom_text(data = caixa,
              aes(x = topo_inset + 0.85, y = M * (esq_inset + 0.96 * (dir_inset - esq_inset)),
                  label = km2_perdida),
              hjust = 1, size = 3.6, colour = "grey15", inherit.aes = FALSE),
    geom_text(data = caixa,
              aes(x = base_inset + 0.3, y = M * esq_inset, label = "1985"),
              hjust = 0, size = 3.3, colour = "grey30", inherit.aes = FALSE),
    geom_text(data = caixa,
              aes(x = base_inset + 0.3, y = M * dir_inset, label = "2025"),
              hjust = 1, size = 3.3, colour = "grey30", inherit.aes = FALSE),
    # linha em cada troca de governo, que aparece quando a animação chega lá
    # (só na metade de baixo, para não passar por cima dos números)
    geom_segment(data = transicoes,
                 aes(x = (topo_inset + base_inset) / 2, xend = base_inset,
                     y = horizontal, yend = horizontal,
                     alpha = linha, group = rotulo),
                 colour = "grey20", linewidth = 0.4, inherit.aes = FALSE),
    # nome do presidente, que aparece junto com a linha e some depois
    geom_text(data = transicoes,
              aes(x = topo_inset + (base_inset - topo_inset) * 0.62,
                  y = horizontal, label = rotulo,
                  alpha = nome, hjust = lado, group = rotulo),
              size = 3.8, fontface = "bold", colour = "grey10",
              inherit.aes = FALSE),
    scale_alpha_identity()
  )
}

### Construindo a animação ----

animacao <- corrida %>%
  ggplot(aes(x = posicao, group = uf)) +
  # barras em blocos arredondados, um por mandato
  geom_polygon(data = arredondar(blocos, inicio, fim, paste(uf, mandato)),
               aes(x = vertical, y = horizontal,
                   fill = mandato, alpha = transparencia, group = grupo),
               inherit.aes = FALSE) +
  # nome do estado, à esquerda da barra
  geom_text(aes(y = 0, label = paste0(estado, " ")),
            hjust = 1, size = 4.5) +
  # valor (perda total), no fim da barra
  geom_text(aes(y = fim_barra, label = paste0(" ", num(round(perda_km2)), " km²")),
            hjust = 0, size = 4.5) +
  # mini-gráfico da vegetação nativa do Brasil
  camadas_inset(corrida) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  scale_y_continuous(labels = num) +
  scale_fill_manual(values = cores_mandatos, labels = rotulos_mandatos,
                    drop = FALSE) +
  guides(fill = guide_legend(nrow = 3)) +
  labs(title = "Desmatamento no Brasil · {closest_state}",
       subtitle = "Presidente: {presidentes[closest_state]}",
       tag = "{eventos[closest_state]}",   # destaque da pandemia
       y = "Perda acumulada de vegetação nativa desde 1985 (km²)",
       x = NULL,
       fill = "Perda em cada\nmandato (partido)",
       caption = "Dados: MapBiomas, Coleção 11 · heronoh.github.io/bioinfo") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(size = 26, face = "bold"),
        plot.subtitle = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        # o "tag" vai no canto superior direito, em vermelho escuro
        plot.tag = element_text(colour = cor_pandemia, face = "bold",
                                size = 17, hjust = 1, lineheight = 0.9),
        plot.tag.position = c(0.97, 0.925),
        plot.margin = margin(15, 120, 15, 160)) +
  transition_states(ano, transition_length = 4, state_length = 1,
                    wrap = FALSE) +   # termina em 2025, sem voltar a 1985
  view_follow(fixed_x = TRUE) +
  ease_aes("cubic-in-out")

### Gerando e salvando o GIF ----

gif <- animate(animacao,
               nframes = 800,       # número total de quadros
               fps = 20,            # quadros por segundo (800 / 20 = 40 segundos)
               end_pause = 120,     # segura o último quadro por 6 segundos
               width = 1280, height = 720, res = 90,   # resolução HD (1280 × 720)
               renderer = gifski_renderer())

gif   # mostra a animação no painel Viewer do RStudio

anim_save("desmatamento_animado.gif", gif)

### Versão 2: cores pelo partido do governador (2007 a 2025) ----

# partido pelo qual o governador de cada estado foi eleito, em cada eleição
governadores <- tibble::tribble(
  ~uf, ~e2006,  ~e2010,  ~e2014,  ~e2018,  ~e2022,
  "AC", "PT",    "PT",    "PT",    "PP",    "PP",
  "AL", "PSDB",  "PSDB",  "PMDB",  "MDB",   "MDB",
  "AP", "PDT",   "PSB",   "PDT",   "PDT",   "SD",
  "AM", "PMDB",  "PMN",   "PROS",  "PSC",   "UNIÃO",
  "BA", "PT",    "PT",    "PT",    "PT",    "PT",
  "CE", "PSB",   "PSB",   "PT",    "PT",    "PT",
  "DF", "PFL",   "PT",    "PSB",   "MDB",   "MDB",
  "ES", "PMDB",  "PSB",   "PMDB",  "PSB",   "PSB",
  "GO", "PP",    "PSDB",  "PSDB",  "DEM",   "UNIÃO",
  "MA", "PDT",   "PMDB",  "PCdoB", "PCdoB", "PSB",
  "MT", "PPS",   "PMDB",  "PDT",   "DEM",   "UNIÃO",
  "MS", "PMDB",  "PMDB",  "PSDB",  "PSDB",  "PSDB",
  "MG", "PSDB",  "PSDB",  "PT",    "NOVO",  "NOVO",
  "PA", "PT",    "PSDB",  "PSDB",  "MDB",   "MDB",
  "PB", "PSDB",  "PSB",   "PSB",   "PSB",   "PSB",
  "PR", "PMDB",  "PSDB",  "PSDB",  "PSD",   "PSD",
  "PE", "PSB",   "PSB",   "PSB",   "PSB",   "PSDB",
  "PI", "PT",    "PSB",   "PT",    "PT",    "PT",
  "RJ", "PMDB",  "PMDB",  "PMDB",  "PSC",   "PL",
  "RN", "PSB",   "DEM",   "PSD",   "PT",    "PT",
  "RS", "PSDB",  "PT",    "PMDB",  "PSDB",  "PSDB",
  "RO", "PPS",   "PMDB",  "PMDB",  "PSL",   "UNIÃO",
  "RR", "PSDB",  "PSDB",  "PP",    "PSL",   "PP",
  "SC", "PMDB",  "DEM",   "PSD",   "PSL",   "PL",
  "SP", "PSDB",  "PSDB",  "PSDB",  "PSDB",  "REP",
  "SE", "PT",    "PT",    "PMDB",  "PSD",   "PSD",
  "TO", "PMDB",  "PSDB",  "PMDB",  "PHS",   "REP"
)

# formato longo: uma linha por estado e eleição
governadores_longo <- governadores %>%
  tidyr::pivot_longer(-uf,
                      names_to = "eleicao", names_prefix = "e",
                      names_transform = as.integer,
                      values_to = "partido") %>%
  mutate(partido = recode(partido, "PMDB" = "MDB", "PFL" = "DEM"))

governadores_longo

# cores dos partidos (os 10 que mais aparecem na corrida) e "Outros"
cores_partidos <- c("PT" = "#CC3311", "PSDB" = "#0077BB", "MDB" = "#117733",
                    "PSB" = "#EE7733", "UNIÃO" = "#332288", "DEM" = "#7570B3",
                    "PDT" = "#AA4499", "PP" = "#DDCC77", "PCdoB" = "#882255",
                    "NOVO" = "#44AA99", "Outros" = "#BBBBBB")

corrida_partidos <- veg %>%
  filter(ano >= 2007) %>%
  group_by(uf, estado, ano) %>%
  summarise(vegetacao_nativa_ha = sum(vegetacao_nativa_ha), .groups = "drop") %>%
  arrange(uf, ano) %>%
  group_by(uf) %>%
  mutate(perda_km2 = (first(vegetacao_nativa_ha) - vegetacao_nativa_ha) / 100) %>%
  group_by(ano) %>%
  mutate(posicao = rank(-perda_km2, ties.method = "first")) %>%
  ungroup() %>%
  filter(posicao <= 12) %>%
  mutate(eleicao = 2006 + 4 * ((ano - 2007) %/% 4)) %>%
  left_join(governadores_longo, by = c("uf", "eleicao")) %>%
  mutate(cor_partido = ifelse(partido %in% names(cores_partidos), partido, "Outros"),
         cor_partido = factor(cor_partido, levels = names(cores_partidos)))

corrida_partidos %>% count(partido, sort = TRUE)

animacao_partidos <- corrida_partidos %>%
  ggplot(aes(x = posicao, group = uf, fill = cor_partido)) +
  geom_polygon(data = arredondar(corrida_partidos, 0, perda_km2, uf),
               aes(x = vertical, y = horizontal, group = grupo),
               alpha = 0.9) +
  geom_text(aes(y = 0, label = paste0(estado, " ")),
            hjust = 1, size = 4.5) +
  geom_text(aes(y = perda_km2,
                label = paste0(" ", num(round(perda_km2)), " km² · ", partido)),
            hjust = 0, size = 4.5) +
  # rótulos mais longos: mini-gráfico um pouco mais à direita e mais baixo
  camadas_inset(corrida_partidos, topo_inset = 8.7, esq_inset = 0.63) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  scale_y_continuous(labels = num) +
  scale_fill_manual(values = cores_partidos, drop = FALSE) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(title = "Desmatamento no Brasil · {closest_state}",
       subtitle = "Presidente: {presidentes[closest_state]}",
       tag = "{eventos[closest_state]}",
       y = "Perda acumulada de vegetação nativa desde 2007 (km²)",
       x = NULL,
       fill = "Partido do\ngovernador",
       caption = "Dados: MapBiomas, Coleção 11; partido pelo qual o governador foi eleito (2006 a 2022) · heronoh.github.io/bioinfo") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(size = 26, face = "bold"),
        plot.subtitle = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.caption = element_text(size = 10),
        plot.tag = element_text(colour = cor_pandemia, face = "bold",
                                size = 17, hjust = 1, lineheight = 0.9),
        plot.tag.position = c(0.97, 0.935),
        plot.margin = margin(15, 150, 15, 160)) +
  transition_states(ano, transition_length = 4, state_length = 1,
                    wrap = FALSE) +
  view_follow(fixed_x = TRUE) +
  ease_aes("cubic-in-out")

gif_partidos <- animate(animacao_partidos,
                        nframes = 600, fps = 20, end_pause = 120,
                        width = 1280, height = 720, res = 90,
                        renderer = gifski_renderer())

gif_partidos

anim_save("desmatamento_animado_partidos.gif", gif_partidos)

### Versão 3: mapa de calor animado da taxa por estado (PRODES) ----

anos_prodes <- sort(unique(desmat$ano))

# estados em ordem de desmatamento total (o maior fica em cima)
ordem_estados <- desmat %>%
  group_by(estado) %>%
  summarise(total = sum(desmatamento_km2)) %>%
  arrange(total) %>%
  pull(estado)

# a tabela completa, repetida para cada quadro
mapa_quadros <- tidyr::crossing(quadro = anos_prodes, desmat) %>%
  mutate(visivel = ifelse(ano <= quadro, 1, 0),
         estado  = factor(estado, levels = ordem_estados))

# linhas e nomes dos presidentes, que aparecem quando o mandato começa
n_estados <- length(ordem_estados)
presidentes_quadros <- tidyr::crossing(quadro = anos_prodes,
                                       mandatos_periodo(1988, 2025)) %>%
  mutate(visivel = ifelse(inicio_ano <= quadro + 0.5, 1, 0),
         linha   = ifelse(inicio_ano > 1987.5, visivel, 0),
         # nomes em duas alturas alternadas, acima do mapa
         altura  = n_estados + 0.5 + ifelse(as.integer(factor(inicio_ano)) %% 2 == 1,
                                            0.45, 1.05))

# o mesmo para a pandemia
pandemia_quadros <- tidyr::crossing(quadro = anos_prodes,
                                    pandemia_periodo(1988, 2025)) %>%
  mutate(inicio_visivel = ifelse(inicio_ano <= quadro + 0.5, 1, 0),
         fim_visivel    = ifelse(fim_ano    <= quadro + 0.5, 1, 0))

# taxa total da Amazônia Legal em cada ano, para o subtítulo
taxas <- setNames(num(total_ano$desmatamento_km2), total_ano$ano)

animacao_mapa <- ggplot() +
  geom_tile(data = mapa_quadros,
            aes(x = ano, y = estado, fill = desmatamento_km2, alpha = visivel),
            colour = "white") +
  # trocas de presidente
  geom_segment(data = presidentes_quadros,
               aes(x = inicio_ano, xend = inicio_ano,
                   y = 0.5, yend = n_estados + 0.5, alpha = linha),
               linetype = "dashed", linewidth = 0.4, colour = "grey20") +
  geom_text(data = presidentes_quadros,
            aes(x = meio, y = altura, label = rotulo, alpha = visivel),
            size = 4, colour = "grey20") +
  # pandemia: linha cheia no início e pontilhada no fim
  geom_segment(data = pandemia_quadros,
               aes(x = inicio_ano, xend = inicio_ano,
                   y = 0.5, yend = n_estados + 0.5, alpha = inicio_visivel),
               linewidth = 0.9, colour = cor_pandemia) +
  geom_segment(data = pandemia_quadros,
               aes(x = fim_ano, xend = fim_ano,
                   y = 0.5, yend = n_estados + 0.5, alpha = fim_visivel),
               linetype = "dotted", linewidth = 0.9, colour = cor_pandemia) +
  geom_text(data = pandemia_quadros,
            aes(x = meio, y = n_estados + 2.1, label = rotulo, alpha = inicio_visivel),
            size = 4, fontface = "bold", colour = cor_pandemia) +
  scale_fill_viridis_c(option = "rocket", direction = -1, labels = num) +
  scale_alpha_identity() +
  scale_x_continuous(breaks = seq(1988, 2025, by = 1), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "Taxa anual de desmatamento na Amazônia Legal · {closest_state}",
       subtitle = "Presidente: {presidentes[closest_state]} · taxa do ano: {taxas[closest_state]} km²",
       tag = "{eventos[closest_state]}",
       x = "Ano PRODES",
       y = NULL,
       fill = "km²/ano",
       caption = "Dados: PRODES/INPE (TerraBrasilis) · heronoh.github.io/bioinfo") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(size = 22, face = "bold"),
        plot.subtitle = element_text(size = 16, margin = margin(b = 50)),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        panel.grid = element_blank(),
        plot.tag = element_text(colour = cor_pandemia, face = "bold",
                                size = 15, hjust = 1, lineheight = 0.9),
        plot.tag.position = c(0.97, 0.93),
        plot.margin = margin(15, 15, 10, 10)) +
  transition_states(quadro, transition_length = 2, state_length = 1,
                    wrap = FALSE) +
  ease_aes("linear")

gif_mapa <- animate(animacao_mapa,
                    nframes = 600, fps = 20, end_pause = 100,
                    width = 1280, height = 720, res = 90,
                    renderer = gifski_renderer())

gif_mapa

anim_save("desmatamento_mapa_animado.gif", gif_mapa)

