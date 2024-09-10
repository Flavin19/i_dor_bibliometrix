# Análise Bibliométrica

Bem-vindo ao projeto de Análise Bibliométrica! Este repositório contém ferramentas e métodos para realizar análises bibliométricas no R, com o objetivo de explorar e visualizar padrões e tendências na literatura científica, de uma forma bem automatizada.

## Sobre

A análise bibliométrica é uma técnica usada para medir e avaliar a produção acadêmica e o impacto de publicações científicas. Este projeto oferece um conjunto de scripts e ferramentas para coletar, processar e visualizar dados bibliométricos a partir de bases de dados acadêmicas extraídos do [**SCOPUS**](https://www.scopus.com).

## Funcionalidade

* Processamento de Dados: Métodos para limpar e preparar os dados bibliométricos;
* Visualização: Ferramentas para criar gráficos e diagramas, como redes de co-citação, análises de autores e tendências temporais;
* Análise Estatística: Cálculo de métricas como fator de impacto, índice h, e análise de citações.

## Instalação

Para começar a usar este projeto, é necessário instalar alguns pacotes que são utilizados nas análises.

```{r}
pacotes <- c("tidyverse", "bib2df", "janitor", "rscopus", "biblionetwork","RColorBrewer",
"tidygraph", "ggraph", "ggnewscale",'stringi',"data.table",'openxlsx','ggwordcloud',
 "bibliometrix", "ggpubr","broom","viridis","treemapify","ggrepel",'igraph',
'ggh4x','stringdist')

githubpacotes <- c("thomasp85/scico","agoutsmedt/networkflow","ParkerICI/vite",
                   'hrbrmstr/pluralize')
gitlib <- c("scico","networkflow","vite",'pluralize')

for(i in pacotes){
  if (!i %in% installed.packages()) {
    install.packages(i, dependencies = T)
  }
}

for(i in githubpacotes){
  if (!gsub(".*/", "", i) %in% installed.packages()) {
    devtools::install_github(i)
  }
}

for(i in pacotes){
  library(i, character.only = TRUE)
}

for(i in gitlib){
  library(i, character.only = TRUE)
}

```
