# Análise Bibliométrica (v 1.0.0 / 09.09.24)

Bem-vindo ao projeto de Análise Bibliométrica! Este repositório contém ferramentas e métodos para realizar análises bibliométricas no R, com o objetivo de explorar e visualizar padrões e tendências na literatura científica, de uma forma bem automatizada, facilitando ainda mais o uso do pacote 'Bibliometrix'.

## Sobre

A análise bibliométrica é uma técnica usada para medir e avaliar a produção acadêmica e o impacto de publicações científicas. Este projeto oferece um conjunto de scripts e ferramentas para coletar, processar e visualizar dados bibliométricos a partir de bases de dados acadêmicas extraídos do [**SCOPUS**](https://www.scopus.com) no formato 'BIB'.

## Funcionalidade

* Processamento de Dados: Métodos para limpar e preparar os dados bibliométricos;
* Visualização: Ferramentas para criar gráficos e diagramas, como redes de co-citação, análises de autores e tendências temporais;
* Análise Estatística: Cálculo de métricas como fator de impacto, índice h, e análise de citações.

## Instalação

Para começar a usar este projeto, é necessário instalar alguns pacotes que são utilizados nas análises.

```{r}

# ------------------------- Pacotes do Cran

pacotes <- c("tidyverse", "bib2df", "janitor", "rscopus", "biblionetwork","RColorBrewer",
"tidygraph", "ggraph", "ggnewscale",'stringi',"data.table",'openxlsx','ggwordcloud',
 "bibliometrix", "ggpubr","broom","viridis","treemapify","ggrepel",'igraph',
'ggh4x','stringdist')

# ------------------------- Pacotes do Github

githubpacotes <- c("thomasp85/scico","agoutsmedt/networkflow","ParkerICI/vite",
                   'hrbrmstr/pluralize')

# ------------------------- Verficando se estão instalados

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

```
## Uso

Para melhor funcionamento e organização do projeto é altamente recomendável seguir as seguintes etapas:

1. Instalar os pacotes necessários com o script acima;
2. Realizar o download da pasta 'Projeto' com todos itens e sub-pastas neles;
3. Colocar os arquivos com os dados para análise dentro da pasta 'Projeto/Bases';
4. Executar os scripts seguindo a ordenação do nome:
   1. 'Projetos/1_Tratando_Base.R';
   2. 'Projetos/2_Analises_Gerais.R';
   3. 'Projetos/3_Analises_de_Rede.R'.

Na pasta 'Projetos', contém uma pasta 'Projetos/Relatorios', onde serão salvos todas as figuras ('Projetos/Relatorios/Figuras') e tabelas ('Projetos/Relatorios/Tabelas') geradas durante a execução do script.
