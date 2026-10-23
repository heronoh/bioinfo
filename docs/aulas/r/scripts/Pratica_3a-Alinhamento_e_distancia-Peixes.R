# DNA de peixes: alinhamento e distância
#
# Script da prática: https://heronoh.github.io/bioinfo/aulas/r/Pratica_3a-Alinhamento_e_distancia-Peixes.html
# Gerado automaticamente a partir da página da prática; para mudar o
# código, edite o .qmd correspondente.

# Prática ----

## Preparativos ----

### Carregando pacotes ----

#### CRAN - Instalando pacotes ----

#instale apenas caso você ainda não tenha o pacote ----
#    DICA: só é preciso instalar cada pacote uma única vez

# lista de pacotes CRAN que você quer garantir instalados
cran_pkgs <- c("pheatmap",
               "stringr",
               "BiocManager",
               "ggseqlogo",
               "tidyverse",
               "RColorBrewer")

# instala apenas os que não estão presentes
for (pkg in cran_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

#### Bioconductor - Instalando pacotes ----

#instale apenas caso você ainda não tenha o pacote ----
#    DICA: só é preciso instalar cada pacote uma única vez

# lista de pacotes Bioconductor
bio_pkgs <- c("Biostrings", "DECIPHER")

# instala apenas os que faltam
for (pkg in bio_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    BiocManager::install(pkg, ask = FALSE, update = FALSE)
  }
}

#### Carregando pacotes ----

# carregando pacotes ----
{
library("tidyverse")
library("Biostrings")
library("stringr")
library("dplyr")
library("DECIPHER")
library("tibble")
library("RColorBrewer")
library("pheatmap")
library("ggseqlogo")
}

### Definindo seu diretório de trabalho ----

getwd()

setwd("")
# utilize o tab para autocompletar com o cursor entre as aspas

### Obter arquivos ----

download.file(
  "https://raw.githubusercontent.com/heronoh/bioinfo/main/aulas/r/arquivos/LGC12Sdb_peixes_Doce.fasta",
  destfile = "LGC12Sdb_peixes_Doce.fasta",
  mode = "wb",            # seguro no Windows/macOS/Linux
  method = "libcurl"      # segue redirecionamentos HTTPS direitinho
)

### carregar o .fasta como objeto no R ----

fasta_seqs <-  Biostrings::readDNAStringSet(filepath = "LGC12Sdb_peixes_Doce.fasta")

fasta_seqs

## 3 - alinhamentos ----

### a - preparo ----
# orientar todas sequências na mesma direção (sobrescrevendo o objeto fasta_seqs)
#              (se alguma estiver no complemento reverso?)

fasta_seqs <- DECIPHER::OrientNucleotides(fasta_seqs)

DECIPHER::BrowseSeqs(fasta_seqs)

### b - alinhar sequências ----

fasta_seqs_alg <- DECIPHER::AlignSeqs(myXStringSet = fasta_seqs, 
                                      refinements = 100,
                                      iterations = 100,
                                      verbose = TRUE)

fasta_seqs_alg

#veja seu alinhamento 
DECIPHER::BrowseSeqs(fasta_seqs_alg)

## 4 - Core alignment ----

# devemos utilizar apenas a parte comum a todos alinhamentos, jogar o resto fora!

#veja o objeto biostrings
fasta_seqs_alg 

### Matriz de distâncias ----

## 5 - Distâncias ----

### a - calcular as distâncias ----

fasta_seqs_dist <- DECIPHER::DistanceMatrix(myXStringSet = fasta_seqs_alg,
                                            includeTerminalGaps = FALSE,
                                            correction = "JC69",   # Jukes-Cantor (nas versões antigas do DECIPHER: "Jukes-Cantor")
                                            processors = 20)

# esta é a matriz de distâncias genéticas entre essas sequências
# (aqui, só um pedaço: as 5 primeiras linhas e colunas)
round(fasta_seqs_dist[1:5, 1:5], 3)

View(fasta_seqs_dist)

### Visualizando a matriz de distâncias ----

# vamos ver como fica a matriz

pheatmap::pheatmap(fasta_seqs_dist,
                   fontsize = 10,
                   color = RColorBrewer::brewer.pal(n = 9,name = "Greens"))

### Logo de sequências ----

## 6 -  plotar proporção de bases para uma subsequência (ggseq logo)

### a - selecionar apenas uma pequena região parcialmente conservada

fasta_sub_char <- Biostrings::subseq(fasta_seqs_alg,180,220) %>% as.character()

seqs_logo_plot <- ggplot2::ggplot() +
  ggseqlogo::geom_logo(data = fasta_sub_char,
                       method = "probability") +
  ggplot2::annotate(geom = "rect",
                    xmin = 12.5,xmax = 33.5,
                    ymin = -0.1,ymax = 1.1,
                    fill="#3cff00", alpha = 0.25)

seqs_logo_plot

ggplot2::ggsave(filename = "seqs_logo.png",
                device = "png",
                width = 30, height = 10,
                units = "cm")

# Referências ----
# https://www.bioconductor.org/help/course-materials/2016/BioC2016/ConcurrentWorkshops2/Wright/BigBioSeqData.pdf
# https://www.bioconductor.org/packages/release/bioc/vignettes/DECIPHER/inst/doc/DECIPHERing.pdf
# http://www2.decipher.codes/RLessons/RLesson14.html
# https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12687
# http://www2.decipher.codes/OligoDesign.html
# http://www2.decipher.codes/Bioinformatics/BigBioSeqData/BigBioSeqData2.html
# https://cran.r-project.org/web/packages/ggdendro/vignettes/ggdendro.html

