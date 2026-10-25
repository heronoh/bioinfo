# Download dos dados da análise v2

Rode **na raiz do repositório**, com seu usuário comum (o script recusa root):

```bash
bash analises/desmatamento_v2/baixar/baixar_dados.sh --listar          # só mostra o que faria
bash analises/desmatamento_v2/baixar/baixar_dados.sh cambio queimadas  # fontes escolhidas
bash analises/desmatamento_v2/baixar/baixar_dados.sh                   # todas
Rscript analises/desmatamento_v2/R/03_processar_downloads.R            # resume queimadas, DETER, câmbio
Rscript analises/desmatamento_v2/R/02_painel.R                         # junta tudo no painel
```

| fonte      | de onde                         | o que vira (em `dados/`)                          |
|------------|---------------------------------|---------------------------------------------------|
| `cambio`   | Banco Central, SGS 1 e 433      | `cambio_ipca_anual.csv` (US$ de 2025)             |
| `ibama`    | Ibama, dados abertos            | `ibama_autos_uf_ano.csv`, `ibama_embargos_uf_ano.csv` |
| `queimadas`| INPE, focos anuais (sat. ref.)  | `queimadas_focos_uf.csv`                          |
| `deter`    | INPE TerraBrasilis (WFS)        | `deter_alertas_uf.csv`, `_mun.csv`, `_mes.csv`    |
| `prodes_mun` (só quando pedida) | INPE TerraBrasilis, polígonos PRODES (WFS) | `prodes_mun.csv`, `deter_prodes_mun.csv` (via `R/05_municipios.R`) |

Terras indígenas x focos: `Rscript analises/desmatamento_v2/R/06_terras_indigenas.R` (usa os focos já
baixados e os limites da Funai, estados e biomas pelo pacote `geobr`, do IPEA; precisa dos pacotes `sf` e `geobr`).

O que já foi baixado não é baixado de novo: anos encerrados nunca se repetem, e o que muda
(ano corrente, Ibama, DETER) só é renovado depois de 7 dias (`VALIDADE_DIAS=30 bash ...`
muda o prazo; `--forcar` baixa tudo de novo).

Requisitos: `curl`, `unzip`, `zipinfo`, `flock`, `sha256sum`, R com `dplyr` e `tidyr`.
Espaço: reserve ~20 GB livres (o script confere antes de cada download).

## Segurança

- Só HTTPS (também nos redirecionamentos), TLS ≥ 1.2, certificado verificado; só 4 domínios permitidos.
- Limites de tamanho, tempo e espaço; tipo do arquivo conferido (HTML de erro é recusado).
- ZIPs: recusa caminhos absolutos, `..`, links simbólicos e "zip bombs"; nunca sobrescreve.
- Arquivos com permissão só do dono (`umask 077`, pasta `700`); nada baixado é executado.
- `dados_brutos/` está no `.gitignore` e o `MANIFESTO.tsv` registra URL, tamanho e SHA-256.
- **Dados pessoais do Ibama** (nomes, CPF/CNPJ): o R lê só uma lista permitida de colunas,
  e os arquivos brutos são apagados logo após o processamento (use `--manter-brutos` para manter).
- Sugestão: rode num usuário sem `sudo`, por exemplo `sudo -u dados bash ...`, e agende com
  `cron` desse usuário, não do root.
