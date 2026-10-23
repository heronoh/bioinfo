# A **linguagem R** e o **RStudio**
#
# Script da prática: https://heronoh.github.io/bioinfo/aulas/r/Pratica_1-o_R_e_o_RStudio.html
# Gerado automaticamente a partir da página da prática; para mudar o
# código, edite o .qmd correspondente.

# Prática ----

## Preparando um documento de código ----

##                 Introdução à introdução ao **R** e ao **RStudio**          ##
##                                                                            ##
##                             Prof. Heron Hilário                            ##
##                                                                            ##
#_______________________________________________________________________________

# criando objetos ----
# neste código vamos criar objetos e tabelas

## criar objeto objA ----
# este comando cria um objeto objA

objA <- 3

# este comando executa o objeto objA, mostrando o que ele armazena
objA

## criar objeto objB ----
objB <- 35

objB

### Utilizando objetos em operações matemáticas ----

# operações matemáticas com os objetos ---
## multiplicar objetos ----
objA*objB

## criar o objeto objD com o resultado
objD <- objA*objB

#verificar o que está armazenado no objeto
objD

## recriar o objA ----

# a função conjunto c() possibilita armazenar diferentes elementos num mesmo objeto
objA <- c(238, 55, 10, 3, 28, -1, 0.33)

objA

## multiplicar objetos ----
objA*objB

## criar o objeto objD ----
objD <- objA*objB

objD

### Funções ----

## a média do objeto objD ---- objD 
mean(objD)

## testando funções ----
length(objD)
max(objD)
min(objD)
sum(objD)

######-> repita todos os passos anteriores com diferentes valores armazenados no objA

### Tabelas ----

# trabalhando com tabelas ----

# vamos criar uma tabela com a função data.frame()
tabela_exp <- data.frame(antes = c(130, 100, 90, 150, 130, 120, 86, 122, 99, 78, 82),
                        depois = c( 56,  88, 32,  28,  40,  20, 30,  22, 35, 43, 56),
                      controle = c(  0,   5,  2,   1,   5,   1,  7,   5,  3,  3, 21))

tabela_exp$antes

tabela_exp$depois

tabela_exp$depois[2]
tabela_exp[2,2]
tabela_exp[c(2,3),2]

### Gráficos simples ----

# gráficos ----
## o mais simples ----

# veja o poder do R, de uma tabela a um gráfico em uma linha!

boxplot(tabela_exp)

### Gráficos customizáveis ----

#### Instalando pacotes ----

# fazer um grafico customizável com o ggplot ---- 
install.packages("ggplot2") # este só precisa ser executado uma vez!

library(ggplot2) #este você executa sempre que for usar

#### Usando pacotes ----

# transformando a tabela para facilicar a entrada na função de gráfico

tabela_exp
stack(tabela_exp)

#gráfico simples ----

ggplot(stack(tabela_exp),
       aes(x = ind,
           y = values,
           fill = ind,
           )) +
         geom_boxplot() 

#adicionando camaad de pontos ----

ggplot(stack(tabela_exp),
       aes(x = ind,
           y = values,
           fill = ind,
           )) +
         geom_boxplot() +
  geom_jitter()

# customizando as cores ----
ggplot(stack(tabela_exp),
       aes(x = ind,
           y = values,
           fill = ind,
           # col = ind
           )) +
         geom_boxplot() +
  geom_jitter()+
  scale_fill_manual(values = c("#ab3310","#2000b0","#2aa0b0"))+
  # scale_colour_manual(values = c("#ab3310","#2000b0","#2aa0b0"))+
  theme_bw()

# salvando o gráfico em um objeto ----
meu_1_boxplot <- ggplot(stack(tabela_exp),
       aes(x = ind,
           y = values,
           fill = ind,
           # col = ind
           )) +
         geom_boxplot() +
  geom_jitter()+
  scale_fill_manual(values = c("#ab3310","#2000b0","#2aa0b0"))+
  # scale_colour_manual(values = c("#ab3310","#2000b0","#2aa0b0"))+
  theme_bw()

meu_1_boxplot

#### Outras dicas e atalhos ----

##                 Introdução à introdução ao **R** e ao **RStudio**          ##
##                                                                            ##
##                             Prof. Heron Hilário                            ##
####                               março/2024                               ####
##                                                                            ##
#_______________________________________________________________________________

## 0 - **RStudio**! ----
#        0a - Console/Terminal

#        0b - Source (arquivos abertos, variáveis, tabelas)

#        0c - Global environment/...

#        0d - Files/Plots/Help/...

# Tools -> Global Options (tudo pode ser configurável)

## 1 - a **linguagem **R**  ----
#          <-              atribuir o que está na esquerda ao que está na direita   
 
#       function()         executar a função "function" no que está dentro do ()

#        %>% ou |>         pipe!  este é o pipe (|) do **R**   (dplyr)

#      ?function()         abrir o manual/documentação da função  (ou, aperte F1
#                          com o cursor na função)

## 0f - atalhos!

#             ctrl  +    ENTER               Executar linha com o cursor ou pró-
#                                            ximo bloco de código
#             ctrl  +    s                   salvar
#             tab                            autocompleta coisas existentes
#             ctrl  +    d                   deletar a linha com o cursor/
#                                            selecionada
#             ctrl  +  shift +  c            comentar/descomentar a linha com o 
#                                            cursor/selecionada
#             ctrl  +  shift +  d            duplicar a linha com o cursor/
#                                            selecionada
#             ctrl  +  setas(l/r)            caminhar pelas palavras
#             ctrl  +  shift + setas(l/r)    caminhar pelas palavras
#             ctrl  +  alt   + setas(u/d)    caminhar pelas palavras
#             ctrl  +  z                     desfazer
#             ,   ou  \   ou  +              continua na próxima linha

