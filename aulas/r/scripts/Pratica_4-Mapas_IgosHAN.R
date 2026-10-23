# Construção de mapas temáticos com o R
#
# Script da prática: https://heronoh.github.io/bioinfo/aulas/r/Pratica_4-Mapas_IgosHAN.html
# Gerado automaticamente a partir da página da prática; para mudar o
# código, edite o .qmd correspondente.

# Prática ----

## Carregar pacotes necessários ----

{
  library(sf)
  library(terra)
  library(ggspatial)
  library(ggplot2)
  library(dplyr)
}

## Definindo seu diretório de trabalho ----

getwd()

setwd("")
# utilize o tab para autocompletar com o cursor entre as aspas

## Dados para está prática ----

### Baixar e descompactar shapes ----

# abixar aruivos
download.file("https://github.com/heronoh/bioinfo/blob/main/aulas/r/arquivos/maps.tar.gz",
              destfile = "maps.tar.gz", mode = "wb")

#descompactar arquivos ----
untar("maps.tar.gz", exdir = ".")

### Ler arquivos ----

# MG
minas <- st_read("ide_1101_mg_unidade_federacao_pol.shp")

plot(minas)

# Parques
# uc_minas <- st_read("/PASTA/SUA_UC.shp")
uc_minas <- st_read("ide_2010_mg_unidades_conservacao_estaduais_pol.shp")

plot(uc_minas)

uc_minas$categoria %>% unique()

# é possível filtrar por qualquer valor desejado, exemplos:

uc_interesse <- uc_minas %>% 
  dplyr::filter(categoria == "PAR")
  # dplyr::filter(bioma == "Mata Atlantica")
  # dplyr::filter(nome == "Parque Estadual da Serra do Cipo")

minas_wgs <- sf::st_transform(x = minas,
                              crs = 4326)

uc_minas_wgs <- sf::st_transform(x = uc_minas,
                                 crs = 4326)

uc_int_wgs <- sf::st_transform(x = uc_interesse,
                               crs = 4326)

## Plotar o mapa final ----

ggplot()+
  geom_sf(data = uc_int_wgs,
          aes(fill = bioma))+
  scale_fill_viridis_d(option = "turbo")+
  geom_sf(data = minas_wgs,
          fill = "transparent")+
  theme_minimal()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white",
                                       color = NA),
        panel.background = element_rect(fill = "white",
                                        color = NA),
        panel.border = element_blank())

## Trabalhando com Rasters ----

### Visualizando rasters ----

raster_temp <- terra::rast("wc2.1_30s_bio_1_cortado.tif")

raster_temp_df <- as.data.frame(x = raster_temp,
                                xy = TRUE)

# renomear a coluna de valores do raster
names(raster_temp_df)[3] <- "value"

### Plotar o mapa temático final ----

# ler shp
bacia <- st_read("bacia_doce.shp")

rios <- st_read("rios_intersec.shp")

# definir projeção
bacia_wgs <- st_set_crs(x = bacia,
                        value = 4326)

# reprojetar para coordenada certa
rios_wgs <- st_transform(x = rios,
                         crs = 4326)

# Carregar dados a partir de um .csv
ocur_peixes <- read.csv("banco_final_tricho.csv")

# transformar csv em shp
ocur_peixe_shp <- st_as_sf(ocur_peixes,
                           coords = c("long",
                                      "lat"), crs = 4326)

# cortar dados apenas para a área de interesse
peixes_inter <- st_intersection(ocur_peixe_shp, bacia_wgs)

# plotar o mapa
ggplot()+
  geom_raster(data = raster_temp_df,
              aes(x = x,
                  y = y,
                  fill = value))+
  scale_fill_gradient(low = "blue", high = "pink", name = "Temperatura (°C)")+
  geom_sf(data = bacia_wgs,
          color = "black",
          fill = "transparent") +
  geom_sf(data = rios_wgs,
          color = "cyan") +
  geom_sf(data = peixes_inter,
          color = "grey40") +
  annotation_north_arrow(location = "tr",
                         which_north = "true",
                         # pad_x = unit(8, "cm"),
                         # pad_y = unit(5, "cm"),
                         style = north_arrow_nautical()) + # elemento obrigatorio
  annotation_scale(location = "br",
                   # pad_x = ,
                   # pad_y = ,
                   width_hint = 0.3) +
  theme_void() +
  labs(title = "Temperatura Média Anual para o ano de 2020")

