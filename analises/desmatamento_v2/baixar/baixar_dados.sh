#!/usr/bin/env bash
# =============================================================================
# baixar_dados.sh  —  download seguro das fontes públicas da análise v2
#
# Uso (a partir da raiz do repositório, como usuário comum, NUNCA como root):
#   bash analises/desmatamento_v2/baixar/baixar_dados.sh [fontes...] [opções]
#
# Fontes:   cambio  ibama  queimadas  deter      (padrão: todas)
# Opções:   --manter-brutos   não apaga os arquivos brutos do Ibama depois de
#                             processados (eles contêm nomes e CPF/CNPJ)
#           --listar          só mostra o que seria baixado, sem baixar
#
# Medidas de segurança:
#   - recusa rodar como root; usa umask 077 (arquivos visíveis só para o dono)
#   - só HTTPS (inclusive nos redirecionamentos), TLS 1.2+, certificado verificado
#   - só baixa de domínios de uma lista fixa (conferida antes e depois de
#     redirecionamentos)
#   - limites de tamanho, de tempo e de espaço livre em disco
#   - confere o tipo do arquivo (ZIP de verdade, CSV que não é página HTML)
#   - ZIPs: recusa caminhos absolutos, "..", links simbólicos e "zip bombs"
#   - grava em arquivo temporário e só move ao final (nada fica pela metade)
#   - registra URL, tamanho e SHA-256 de cada arquivo em dados_brutos/MANIFESTO.tsv
#   - nunca executa nada que foi baixado; nenhuma senha ou chave é usada
#   - dados pessoais do Ibama (nomes, CPF/CNPJ) são descartados no
#     processamento, e os arquivos brutos são apagados em seguida
#   - um único processo por vez (trava com flock)
# =============================================================================

set -Eeuo pipefail
IFS=$'\n\t'
umask 077

# --- configuração ------------------------------------------------------------
BASE="analises/desmatamento_v2"
BRUTOS="$BASE/dados_brutos"
MANIFESTO="$BRUTOS/MANIFESTO.tsv"
AGENTE="bioinfo-desmatamento/1.0 (uso academico; github.com/heronoh/bioinfo)"

DOMINIOS_PERMITIDOS=(
  "dataserver-coids.inpe.br"                     # INPE Programa Queimadas
  "terrabrasilis.dpi.inpe.br"                    # INPE TerraBrasilis (DETER)
  "stibamadadosabertosprd.blob.core.windows.net" # Ibama dados abertos
  "api.bcb.gov.br"                               # Banco Central (SGS)
)

TAMANHO_MAX_ARQUIVO=$((3 * 1024 * 1024 * 1024))   # 3 GB por download
TAMANHO_MAX_DESCOMPACTADO=$((8 * 1024 * 1024 * 1024)) # 8 GB por ZIP
ESPACO_MIN_LIVRE_GB=20
PAUSA_ENTRE_PEDIDOS=2                               # segundos

# padrões dos arquivos anuais de focos (satélite de referência) no INPE.
# Se o INPE mudar os nomes, ajuste aqui (o script mostra a listagem).
QUEIMADAS_DIR="https://dataserver-coids.inpe.br/queimadas/queimadas/focos/csv/anual/"
QUEIMADAS_PADRAO_SUBDIR='^Brasil_sat_ref/$'
QUEIMADAS_PADRAO_ARQUIVO='^focos_br_ref_[0-9]{4}\.(zip|csv)$'

DETER_WFS="https://terrabrasilis.dpi.inpe.br/geoserver/deter-amz/wfs"
DETER_CAMADAS_CANDIDATAS=("deter_public" "deter_amz")
DETER_POR_PAGINA=10000

IBAMA_AUTOS="https://stibamadadosabertosprd.blob.core.windows.net/dados-abertos/dados/SIFISC/auto_infracao/auto_infracao/auto_infracao_csv.zip"
IBAMA_EMBARGOS="https://stibamadadosabertosprd.blob.core.windows.net/dados-abertos/dados/TERMOS_DE_EMBARGO/TERMO_EMBARGO/termo_de_embargo.csv"

BCB_SERIE_DOLAR=1     # Taxa de câmbio - Livre - Dólar americano (venda) - diário
BCB_SERIE_IPCA=433    # IPCA, variação mensal (%) — validado abaixo contra 2023
ANO_INICIAL=1985

# --- utilidades ----------------------------------------------------------------
log()  { printf '[%s] %s\n' "$(date '+%F %T')" "$*" >&2; }
erro() { log "ERRO: $*"; exit 1; }

host_da_url() {
  # aceita apenas https://host[/...]; devolve o host em minúsculas
  local url="$1"
  [[ "$url" =~ ^https://([A-Za-z0-9.-]+)(/|\?|$) ]] || return 1
  printf '%s' "${BASH_REMATCH[1],,}"
}

dominio_permitido() {
  local host d
  host="$(host_da_url "$1")" || return 1
  for d in "${DOMINIOS_PERMITIDOS[@]}"; do
    [[ "$host" == "$d" ]] && return 0
  done
  return 1
}

checar_espaco() {
  local livre_kb
  livre_kb="$(df -Pk "$BRUTOS" | awk 'NR==2 {print $4}')"
  (( livre_kb / 1024 / 1024 >= ESPACO_MIN_LIVRE_GB )) ||
    erro "menos de ${ESPACO_MIN_LIVRE_GB} GB livres em $BRUTOS"
}

registrar() {
  # registra o arquivo no manifesto: data, url, arquivo, bytes, sha256
  local url="$1" arquivo="$2"
  printf '%s\t%s\t%s\t%s\t%s\n' "$(date -u '+%FT%TZ')" "$url" "$arquivo" \
    "$(stat -c %s "$arquivo")" "$(sha256sum "$arquivo" | cut -d' ' -f1)" >> "$MANIFESTO"
}

# baixar URL DESTINO TIPO   (TIPO: zip | csv | xml | texto)
baixar() {
  local url="$1" destino="$2" tipo="$3" tmp efetiva

  dominio_permitido "$url" || erro "domínio fora da lista permitida: $url"
  if [[ "${LISTAR:-0}" == 1 ]]; then log "baixaria: $url -> $destino"; return 0; fi
  checar_espaco

  tmp="$(mktemp "$BRUTOS/.parcial.XXXXXX")"
  TMP_ATUAL="$tmp"          # apagado pelo trap EXIT se algo der errado

  efetiva="$(curl --proto '=https' --proto-redir '=https' --tlsv1.2 \
      --fail --silent --show-error --location --max-redirs 3 \
      --connect-timeout 20 --max-time 3600 \
      --retry 3 --retry-delay 10 --retry-connrefused \
      --max-filesize "$TAMANHO_MAX_ARQUIVO" \
      --user-agent "$AGENTE" \
      --output "$tmp" --write-out '%{url_effective}' -- "$url")" ||
    erro "falha ao baixar $url"

  # o destino final (depois de redirecionamentos) também precisa ser permitido
  dominio_permitido "$efetiva" || erro "redirecionado para domínio não permitido: $efetiva"

  [[ -s "$tmp" ]] || erro "arquivo vazio: $url"
  case "$tipo" in
    zip)
      [[ "$(head -c 4 "$tmp" | od -An -tx1 | tr -d ' \n')" == "504b0304" ]] ||
        erro "não é um ZIP válido: $url" ;;
    csv|texto)
      # um CSV não pode começar como página HTML (página de erro, captcha...)
      if head -c 512 "$tmp" | grep -qiE '<(!doctype|html|head|body)'; then
        erro "recebi uma página HTML em vez de dados: $url"
      fi ;;
    xml)
      head -c 512 "$tmp" | grep -q '<' || erro "resposta não é XML: $url" ;;
  esac

  mv -f -- "$tmp" "$destino"
  TMP_ATUAL=""
  chmod 600 -- "$destino"
  registrar "$url" "$destino"
  log "ok: $destino ($(du -h "$destino" | cut -f1))"
  sleep "$PAUSA_ENTRE_PEDIDOS"
}

# descompactar_seguro ARQUIVO.zip PASTA
descompactar_seguro() {
  local zip="$1" pasta="$2" entrada total
  command -v unzip >/dev/null || erro "instale o 'unzip'"

  # recusa caminhos absolutos, '..' e caracteres de controle nos nomes
  while IFS= read -r entrada; do
    [[ "$entrada" == /* || "$entrada" == *..* || "$entrada" =~ [[:cntrl:]] ]] &&
      erro "entrada suspeita no ZIP ($zip): $entrada"
  done < <(unzip -Z1 -- "$zip")

  # recusa links simbólicos dentro do ZIP
  if zipinfo -- "$zip" | awk 'NR>2 {print substr($1,1,1)}' | grep -q '^l'; then
    erro "o ZIP contém links simbólicos: $zip"
  fi

  # recusa "zip bombs" (tamanho descompactado acima do limite)
  total="$(unzip -l -- "$zip" | awk 'END {print $1}')"
  [[ "$total" =~ ^[0-9]+$ ]] || erro "não consegui ler o tamanho de $zip"
  (( total <= TAMANHO_MAX_DESCOMPACTADO )) ||
    erro "conteúdo descompactado grande demais ($total bytes): $zip"

  mkdir -p -- "$pasta"
  # o tamanho declarado no ZIP pode mentir: limita também o tamanho de cada
  # arquivo gravado (ulimit -f, em blocos de 1 KB) e nunca sobrescreve (-n)
  ( ulimit -f $(( TAMANHO_MAX_DESCOMPACTADO / 1024 )); unzip -q -n -- "$zip" -d "$pasta" ) ||
    erro "falha ao descompactar $zip"
}

# lista os links de uma página de diretório, só com nomes "comportados"
# (roda em subshell: um erro aqui não derruba o script, só esvazia a lista)
listar_diretorio() (
  local url="$1" tmp
  tmp="$(mktemp "$BRUTOS/.indice.XXXXXX")"
  LISTAR=0 baixar "$url" "$tmp" texto >/dev/null 2>&1 || { rm -f "$tmp"; return 1; }
  grep -oE 'href="[A-Za-z0-9_.%-]+/?"' "$tmp" | sed -E 's/^href="//; s/"$//' |
    grep -vE '^\.\.?/?$' | sort -u || true
  rm -f -- "$tmp"
)

# --- fontes ------------------------------------------------------------------------
fonte_cambio() {
  log "Banco Central: dólar (série $BCB_SERIE_DOLAR) e IPCA (série $BCB_SERIE_IPCA)"
  local pasta="$BRUTOS/bcb" serie ini fim ano_fim
  mkdir -p -- "$pasta"
  ano_fim="$(date +%Y)"
  for serie in "$BCB_SERIE_DOLAR" "$BCB_SERIE_IPCA"; do
    # a API limita cada consulta a 10 anos
    for (( ini = ANO_INICIAL; ini <= ano_fim; ini += 10 )); do
      fim=$(( ini + 9 > ano_fim ? ano_fim : ini + 9 ))
      baixar "https://api.bcb.gov.br/dados/serie/bcdata.sgs.${serie}/dados?formato=csv&dataInicial=01/01/${ini}&dataFinal=31/12/${fim}" \
             "$pasta/sgs_${serie}_${ini}_${fim}.csv" csv
    done
  done
}

fonte_ibama() {
  log "Ibama: autos de infração e termos de embargo"
  local pasta="$BRUTOS/ibama"
  mkdir -p -- "$pasta"
  baixar "$IBAMA_AUTOS"    "$pasta/auto_infracao_csv.zip" zip
  baixar "$IBAMA_EMBARGOS" "$pasta/termo_de_embargo.csv"  csv
  [[ "${LISTAR:-0}" == 1 ]] && return 0
  descompactar_seguro "$pasta/auto_infracao_csv.zip" "$pasta/autos"

  # processa já, descartando dados pessoais, e apaga os brutos
  Rscript --vanilla "$BASE/R/04_ibama.R" "$pasta" || erro "falha ao processar os dados do Ibama"
  if [[ "${MANTER_BRUTOS:-0}" != 1 ]]; then
    rm -rf -- "$pasta"
    log "arquivos brutos do Ibama apagados (continham nomes e CPF/CNPJ)"
  fi
}

fonte_queimadas() {
  log "INPE Queimadas: focos anuais (satélite de referência)"
  local pasta="$BRUTOS/queimadas" subdir arquivo achou=0
  mkdir -p -- "$pasta"
  while IFS= read -r subdir; do
    [[ "$subdir" =~ $QUEIMADAS_PADRAO_SUBDIR ]] || continue
    while IFS= read -r arquivo; do
      [[ "$arquivo" =~ $QUEIMADAS_PADRAO_ARQUIVO ]] || continue
      achou=1
      [[ -s "$pasta/$arquivo" ]] && { log "já existe: $arquivo"; continue; }
      if [[ "$arquivo" == *.zip ]]; then
        baixar "${QUEIMADAS_DIR}${subdir}${arquivo}" "$pasta/$arquivo" zip
        [[ "${LISTAR:-0}" == 1 ]] || descompactar_seguro "$pasta/$arquivo" "$pasta/csv"
      else
        baixar "${QUEIMADAS_DIR}${subdir}${arquivo}" "$pasta/$arquivo" csv
      fi
    done < <(listar_diretorio "${QUEIMADAS_DIR}${subdir}")
  done < <(listar_diretorio "$QUEIMADAS_DIR")

  if (( achou == 0 )); then
    log "Nenhum arquivo bateu com os padrões. Conteúdo do diretório:"
    listar_diretorio "$QUEIMADAS_DIR" >&2 || true
    erro "ajuste QUEIMADAS_PADRAO_SUBDIR / QUEIMADAS_PADRAO_ARQUIVO no início do script"
  fi
}

fonte_deter() {
  log "INPE DETER Amazônia (WFS do TerraBrasilis)"
  local pasta="$BRUTOS/deter" camada="" c total inicio campos geom
  mkdir -p -- "$pasta"

  baixar "${DETER_WFS}?service=WFS&version=2.0.0&request=GetCapabilities" \
         "$pasta/capabilities.xml" xml
  [[ "${LISTAR:-0}" == 1 ]] && return 0
  for c in "${DETER_CAMADAS_CANDIDATAS[@]}"; do
    if grep -qE "<(wfs:)?Name>(deter-amz:)?${c}</(wfs:)?Name>" "$pasta/capabilities.xml"; then
      camada="$c"; break
    fi
  done
  [[ -n "$camada" ]] || erro "nenhuma camada DETER pública encontrada (veja $pasta/capabilities.xml)"
  log "camada: $camada"

  # campos da camada, sem a geometria (deixa o arquivo muito menor)
  baixar "${DETER_WFS}?service=WFS&version=2.0.0&request=DescribeFeatureType&typeName=${camada}" \
         "$pasta/campos.xsd" xml
  geom="$(grep -oE 'name="[A-Za-z0-9_]+"[^>]*type="gml:[A-Za-z]+PropertyType"' "$pasta/campos.xsd" |
          sed -E 's/^name="([^"]+)".*/\1/' | head -1 || true)"
  campos="$(grep -oE '<xsd:element [^>]*name="[A-Za-z0-9_]+"' "$pasta/campos.xsd" |
            sed -E 's/.*name="([^"]+)"/\1/' | grep -vx "${geom:-__nenhum__}" | paste -sd, -)"
  [[ -n "$campos" ]] || erro "não consegui ler os campos da camada (veja $pasta/campos.xsd)"
  log "campos: $campos (sem a geometria '${geom:-?}')"
  # ordenação estável para a paginação: gid, se existir; senão o primeiro campo
  local ordem="${campos%%,*}"
  [[ ",$campos," == *",gid,"* ]] && ordem="gid"

  baixar "${DETER_WFS}?service=WFS&version=2.0.0&request=GetFeature&typeName=${camada}&resultType=hits" \
         "$pasta/total.xml" xml
  total="$(grep -oE 'numberMatched="[0-9]+"' "$pasta/total.xml" | grep -oE '[0-9]+' | head -1)"
  [[ "$total" =~ ^[0-9]+$ ]] || erro "não consegui ler o total de alertas"
  log "total de alertas: $total"

  for (( inicio = 0; inicio < total; inicio += DETER_POR_PAGINA )); do
    baixar "${DETER_WFS}?service=WFS&version=2.0.0&request=GetFeature&typeName=${camada}&propertyName=${campos}&outputFormat=csv&count=${DETER_POR_PAGINA}&startIndex=${inicio}&sortBy=${ordem}" \
           "$pasta/deter_$(printf '%07d' "$inicio").csv" csv
  done
}

# --- principal -----------------------------------------------------------------------
TMP_ATUAL=""
trap 'rm -f -- "${TMP_ATUAL:-}"' EXIT
[[ "$(id -u)" -ne 0 ]] || erro "não rode como root; use seu usuário comum"
[[ -d "$BASE" ]] || erro "rode a partir da raiz do repositório bioinfo"
for prog in curl sha256sum unzip zipinfo flock Rscript; do
  command -v "$prog" >/dev/null || erro "programa necessário não encontrado: $prog"
done

mkdir -p -- "$BRUTOS"
chmod 700 -- "$BRUTOS"
[[ -f "$MANIFESTO" ]] || printf 'data_utc\turl\tarquivo\tbytes\tsha256\n' > "$MANIFESTO"

exec 9> "$BRUTOS/.trava"
flock -n 9 || erro "outro download já está em andamento"

FONTES=()
for arg in "$@"; do
  case "$arg" in
    --manter-brutos) MANTER_BRUTOS=1 ;;
    --listar)        LISTAR=1 ;;
    cambio|ibama|queimadas|deter) FONTES+=("$arg") ;;
    *) erro "argumento desconhecido: $arg" ;;
  esac
done
(( ${#FONTES[@]} )) || FONTES=(cambio ibama queimadas deter)

for f in "${FONTES[@]}"; do
  "fonte_$f"
done
log "concluído. Manifesto: $MANIFESTO"
