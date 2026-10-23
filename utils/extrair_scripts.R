#!/usr/bin/env Rscript
# =============================================================================
# extrair_scripts.R
#
# Gera, para cada prática em aulas/r/, um script .R com o código que o aluno
# deve executar, salvo em aulas/r/scripts/<nome_da_pratica>.R.
#
# É executado automaticamente antes de cada renderização (pre-render no
# _quarto.yml), a partir da raiz do projeto. Também pode ser rodado à mão:
#   Rscript utils/extrair_scripts.R
#
# Quais práticas: todo .qmd de aulas/r/ que tenha o botão de download, ou seja,
# um link para "scripts/<nome_da_pratica>.R".
#
# O que entra no script:
#   - chunks ```{r} visíveis (inclusive os com eval = FALSE);
#   - blocos de exibição ```r / ``` r;
#   - os títulos das seções, como marcadores do RStudio ("## Título ----").
# O que fica de fora:
#   - chunks com include = FALSE ou echo = FALSE (setup, hooks, etc.);
#   - chunks com script = FALSE (no cabeçalho ou como #| script: false);
#   - blocos de exibição marcados com a classe .nao-script: ```{.r .nao-script}
#   - blocos de outras linguagens (text, html, bash...) e comentários HTML.
# =============================================================================

dir_aulas   <- "aulas/r"
dir_scripts <- file.path(dir_aulas, "scripts")
site_aulas  <- "https://heronoh.github.io/bioinfo/aulas/r"

# limpa marcações de markdown de um título
limpar_titulo <- function(x) {
  x <- gsub("\\{[^}]*\\}\\s*$", "", x)          # atributos {#id .classe}
  x <- gsub("\\[([^]]*)\\]\\([^)]*\\)", "\\1", x) # links
  x <- gsub("[*_`]", "", x)                      # negrito, itálico, código
  trimws(x)
}

# TRUE se a opção estiver desligada no cabeçalho (ex.: include=FALSE, echo = F)
opcao_falsa <- function(cabecalho, opcao) {
  grepl(paste0("\\b", opcao, "\\s*=\\s*(FALSE|F)\\b"), cabecalho)
}

# TRUE se a opção estiver desligada como #| opcao: false
opcao_falsa_pipe <- function(linhas, opcao) {
  any(grepl(paste0("^#\\|\\s*", opcao, ":\\s*(false|FALSE|no)\\s*$"), linhas))
}

extrair <- function(arq_qmd) {
  linhas <- readLines(arq_qmd, warn = FALSE, encoding = "UTF-8")
  nome   <- tools::file_path_sans_ext(basename(arq_qmd))

  # título da prática, do cabeçalho YAML
  titulo <- grep("^title:", linhas, value = TRUE)[1]
  titulo <- if (is.na(titulo)) nome else
    trimws(gsub("^title:\\s*['\"]?|['\"]?\\s*$", "", titulo))

  saida <- c(
    paste0("# ", titulo),
    "#",
    paste0("# Script da pr\u00e1tica: ", site_aulas, "/", nome, ".html"),
    "# Gerado automaticamente a partir da p\u00e1gina da pr\u00e1tica; para mudar o",
    "# c\u00f3digo, edite o .qmd correspondente.",
    ""
  )

  i <- 1
  n <- length(linhas)

  # pula o cabeçalho YAML
  if (n > 0 && grepl("^---\\s*$", linhas[1])) {
    i <- 2
    while (i <= n && !grepl("^---\\s*$", linhas[i])) i <- i + 1
    i <- i + 1
  }

  titulos_pendentes <- character(0)
  em_comentario <- FALSE

  while (i <= n) {
    l <- linhas[i]

    # comentários HTML <!-- ... --> (podem ter várias linhas)
    if (!em_comentario && grepl("<!--", l)) {
      if (!grepl("-->", sub(".*<!--", "", l))) em_comentario <- TRUE
      i <- i + 1
      next
    }
    if (em_comentario) {
      if (grepl("-->", l)) em_comentario <- FALSE
      i <- i + 1
      next
    }

    # títulos de seção (fora de blocos de código)
    if (grepl("^#{1,6}\\s+\\S", l)) {
      nivel <- nchar(sub("^(#+).*", "\\1", l))
      texto <- limpar_titulo(sub("^#+\\s+", "", l))
      # mantém só os títulos de nível superior ao novo
      titulos_pendentes <- titulos_pendentes[
        as.integer(sub("\\|.*", "", titulos_pendentes)) < nivel]
      titulos_pendentes <- c(titulos_pendentes, paste0(nivel, "|", texto))
      i <- i + 1
      next
    }

    # início de bloco de código
    if (grepl("^\\s*```", l)) {
      crases <- sub("^\\s*(`+).*", "\\1", l)
      info   <- trimws(sub("^\\s*`+", "", l))

      # acha o fim do bloco
      j <- i + 1
      while (j <= n && !grepl(paste0("^\\s*", crases, "\\s*$"), linhas[j])) j <- j + 1
      corpo <- if (j > i + 1) linhas[(i + 1):(j - 1)] else character(0)

      eh_chunk    <- grepl("^\\{[rR]([ ,}]|$)", info)
      eh_exibicao <- grepl("^[rR]$", info) ||
                     (grepl("^\\{\\s*\\.[rR]\\b", info) && !grepl("\\.nao-script\\b", info))

      incluir <- FALSE
      if (eh_chunk) {
        incluir <- !(opcao_falsa(info, "include") || opcao_falsa(info, "echo") ||
                     opcao_falsa(info, "script") ||
                     opcao_falsa_pipe(corpo, "include") || opcao_falsa_pipe(corpo, "echo") ||
                     opcao_falsa_pipe(corpo, "script"))
        corpo <- corpo[!grepl("^#\\|", corpo)]   # remove as opções #|
      } else if (eh_exibicao) {
        incluir <- TRUE
      }

      # tira linhas em branco do começo e do fim
      while (length(corpo) && !nzchar(trimws(corpo[1]))) corpo <- corpo[-1]
      while (length(corpo) && !nzchar(trimws(corpo[length(corpo)]))) corpo <- corpo[-length(corpo)]

      if (incluir && length(corpo)) {
        if (length(titulos_pendentes)) {
          for (t in titulos_pendentes) {
            nivel <- as.integer(sub("\\|.*", "", t))
            texto <- sub("^[0-9]+\\|", "", t)
            saida <- c(saida, "", paste0(strrep("#", nivel), " ", texto, " ----"), "")
          }
          titulos_pendentes <- character(0)
        }
        saida <- c(saida, corpo, "")
      }

      i <- j + 1
      next
    }

    i <- i + 1
  }

  # sem linhas em branco repetidas
  vazias <- !nzchar(trimws(saida))
  saida  <- saida[!(vazias & c(FALSE, vazias[-length(vazias)]))]

  destino <- file.path(dir_scripts, paste0(nome, ".R"))
  writeLines(saida, destino, useBytes = TRUE)
  destino
}

# --- execução -----------------------------------------------------------------

dir.create(dir_scripts, showWarnings = FALSE, recursive = TRUE)

qmds <- list.files(dir_aulas, pattern = "\\.qmd$", full.names = TRUE)
qmds <- qmds[!nzchar(Sys.readlink(qmds))]   # ignora links simbólicos

for (q in qmds) {
  nome <- tools::file_path_sans_ext(basename(q))
  conteudo <- paste(readLines(q, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (!grepl(paste0("scripts/", nome, ".R"), conteudo, fixed = TRUE)) next
  destino <- extrair(q)
  message("script gerado: ", destino)
}
