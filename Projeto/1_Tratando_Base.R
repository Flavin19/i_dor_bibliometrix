# -----------------------------------------------------------------------------#
# Carregando pacotes, arquivos e funcoes auxiliares

source("utils/utils_tratamento.R")

# -----------------------------------------------------------------------------#
# Bases

arq <- list.files('Bases',full.names = T)
arq <- arq[str_detect(arq,'.bib')]

dados <- lapply(arq, function(x){
  convert2df(file = x, dbsource = "scopus", format = "bibtex")})

# ---------------------------------------------------------------------------- #
# Tratando a base

dados <- rbindlist(dados,fill = T)

colsmain = c('AU','TI','SO','JI','DT','DE','ID','AB','C1','RP','CR','DI',
             'TC','PY','DB','SR','SR_FULL','AU_UN')

dados <- dados |> select(all_of(colsmain)) |> filter(AU != 'NA NA')

gc()

## REMOVENDO DUPLICATAS

dados <-  rem_dupli(dados)

## PADRONIZANDO NOMES DE INSTITUICOES

dados$AU_UN <- name_insti(dados$AU_UN)

dist_matrix <- distancias(dados$AU_UN)

for (i in 1:nrow(dist_matrix)) {
  
  substituir <- dist_matrix$row[i]
  por <- dist_matrix$col[i]
  
  dados$AU_UN <- str_replace(dados$AU_UN, substituir, por)
}

## PADRONIZANDO PALAVRAS CHAVES

dados$ID <- palavras_chave(dados$ID)

dados$DE <- palavras_chave(dados$DE)

save(dados,file = 'Bases/base.RData')

dados |> select(!CR) |> write.xlsx('Bases/base_completa.xlsx')
