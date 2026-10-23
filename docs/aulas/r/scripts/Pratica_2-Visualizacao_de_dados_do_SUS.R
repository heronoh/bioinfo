# Visualização de dados do SUS
#
# Script da prática: https://heronoh.github.io/bioinfo/aulas/r/Pratica_2-Visualizacao_de_dados_do_SUS.html
# Gerado automaticamente a partir da página da prática; para mudar o
# código, edite o .qmd correspondente.

# Prática ----

## Preparação ----

### Carregar pacotes ----

# para instalar o pacote, caso ainda não tenha (só se instala uma vez)
install.packages("ggplot2")
install.packages("dplyr")

# execute para carregar o pacote, caso já instalado
library("ggplot2")
library("dplyr")

## Obtendo os dados ----

### Definindo seu diretório de trabalho ----

getwd()

setwd("")
# utilize o tab para autocompletar com o cursor entre as aspas

### Obter arquivos ----

download.file(
  "https://raw.githubusercontent.com/heronoh/bioinfo/main/aulas/r/arquivos/dados_SUS_pneumo.csv",
  destfile = "dados_SUS_pneumo.csv",
  mode = "wb",
  method = "libcurl"
  )

#complete usando a tecla TAB até chegar ao arquivo que você baixou
dados_SUS <- read.csv(file = "",
                      sep = ","
                      ) %>% 
  tibble::tibble()

## Investigando os dados ----

# ver a tabela de dados
dados_SUS

# qual o tamanho da tabela
dim(dados_SUS)

# qual o nome das colunas
colnames(dados_SUS)

# quais os valores de uma coluna
dados_SUS$Idade_anos 
dados_SUS$Sexo 

# quantas observações temos de cada tipo? 
dados_SUS$Idade_anos %>% table()
dados_SUS$Sexo %>% table()

# visualizar quantas observações temos de cada tipo
dados_SUS$Idade_anos %>% table() %>% plot()
dados_SUS$Sexo %>% table() %>% barplot()

#quais as 10 primeiras observações dessa tabela
dados_SUS[1:10,]

#quais asegunda coluna
dados_SUS[,2]

#qual a média de idades?
dados_SUS$Idade_anos %>% mean(na.rm = T)

### Gráficos exploratórios ----

#### Gráfico escolaridade por Sexo ----

# grágico escolaridade por Sexo
 dados_SUS %>%
  mutate(Idade_anos = as.numeric(Idade_anos) ) %>% 
  filter(Sexo %in% c("Masculino", "Feminino")) %>% 
  ggplot(aes(x = CID_significado,
             group = Escolaridade,
             col = Escolaridade,
             fill  = Escolaridade)) +
  geom_bar(stat = "count",position = "dodge") +
  facet_grid(cols = vars(Sexo))

#### Gráfico de Sexo e escolaridade ----

 dados_SUS %>%
  mutate(Idade_anos = as.numeric(Idade_anos) ) %>% 
  filter(Sexo %in% c("Masculino", "Feminino")) %>% 
  ggplot(aes(x = CID_significado,
             group = Sexo,
             col = Sexo,
             fill  = Sexo)) +
  geom_bar(stat = "count",position = "dodge") +
  facet_grid(cols = vars(Escolaridade))

#### Gráfico aumento de mortes por grupo de escolaridade ----

 dados_SUS %>%
  filter(Sexo %in% c("Masculino", "Feminino")) %>% 
  ggplot(aes(x = Ano_obito,
             group = Sexo,
             col = Sexo,
             fill  = Sexo)) +
  geom_density(stat = "count",position = "stack") +
  facet_grid(cols = vars(Escolaridade)) +
   theme(axis.text.x = element_text(angle = 90))

#### Gráfico aumento de mortes total ----

 dados_SUS %>%
  filter(Sexo %in% c("Masculino", "Feminino")) %>% 
  ggplot(aes(x = Ano_obito,
             group = Sexo,
             col = Sexo,
             fill  = Sexo)) +
  geom_density(stat = "count",position = "stack") +
   theme(axis.text.x = element_text(angle = 90))

#### Gráfico aumento de mortes por ano e estado civil ----

 dados_SUS %>%
  filter(Sexo %in% c("Masculino", "Feminino")) %>% 
  ggplot(aes(x = Ano_obito,
             group = Estado_civil,
             col = Estado_civil,
             fill  = Estado_civil)) +
  geom_density(stat = "count",position = "stack") +
   theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(cols = vars(Sexo)) 

#### Gráfico de média de idades por grupo de escolaridade ----

 dados_SUS %>%
  filter(Sexo %in% c("Masculino", "Feminino")) %>% 
  ggplot(aes(y = Idade_anos,
             x = Sexo,
             group = Sexo,
             fill  = Sexo)) +
  geom_boxplot() +
   # geom_jitter() +
   theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(cols = vars(Escolaridade)) 

