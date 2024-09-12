source("utils/utils_analises_gerais.R")

# -----------------------------------------------------------------------------#
# Bases

load('Bases/base.RData')

# ---------------------------------------------------------------------------- #
# Produzindo resultados para analise

results <- biblioAnalysis(dados, sep = ";")

resultsobj = summary(results)

# ---------------------------------------------------------------------------- #
# Main Findings (Overview)

mainfindings(resultsobj)

# ---------------------------------------------------------------------------- #
# Annual Scientific Production

dados |> count(PY) |>
ggplot(aes(x = as.character(PY), y= n, group = 1))+
  geom_area(fill = "grey50",alpha = 0.15)+
  geom_line(linewidth = 0.5)+
theme_f(expandy = c(0,5,0,10), namex = 'Years', namey = 'Frequency',
        sizep =  2)+
  theme(axis.text.x = element_text(angle = 90))

ggsave("Relatorios/Figuras/Annual Scientific Production.png" ,
       width = 22, height = 12, units = "cm")

# ---------------------------------------------------------------------------- #
# Average Citation Per Year

anosf = seq(min(dados$PY),max(dados$PY),1)
anosf <- anosf[!anosf %in% unique(dados$PY)]

Anos = dados |> rename('Anos' = 'PY', 'n'='TC') |>
  bind_rows(tibble(Anos = anosf,n= 0)) |>
  group_by(Anos) |> reframe(n = mean(n,na.rm = T)) |> 
  mutate(Anos2 = year(Sys.Date()) - Anos,n2 = n/Anos2)

Anos |> 
  ggplot(aes(x = as.character(Anos), y= n, group = 1))+
  geom_area(fill = "grey50",alpha = 0.15)+
  geom_line(linewidth = 0.4)+
  theme_f(expandy = c(0,1,0,10), namex = 'Years', namey = 'Frequency',
          sizep =  0, breaksx = seq(min(dados$PY),max(dados$PY),3))+
  theme(axis.text.x = element_text(angle = 90))

ggsave("Relatorios/Figuras/Average Total Citations per Year.png",
       width = 22, height = 12, units = "cm")

Anos |> 
  ggplot(aes(x = as.character(Anos), y= n2, group = 1))+
  geom_area(fill = "grey50",alpha = 0.15)+
  geom_line(linewidth = 0.4)+
  theme_f(expandy = c(0,0.02,0,1), namex = 'Years', namey = 'Frequency',
          sizep =  0, breaksx = seq(min(dados$PY),max(dados$PY),3))+
  theme(axis.text.x = element_text(angle = 90))

ggsave("Relatorios/Figuras/Average Article Citations per Year.png",
       width = 22, height = 12, units = "cm")

# ---------------------------------------------------------------------------- #
# Three-Field Plot

#  Author -> KeyWord -> Journal
threeFieldsPlot(dados,fields = c('DE',"AU","SO"), n = c(10, 10, 10))

# Author -> Ref -> Keyword
threeFieldsPlot(dados,fields = c("AU", "CR", "DE"), n = c(10, 10, 10))

# -----------------------------------------------------------------------------#
# Most Productive Sources

dados |> count(SO) |> 
  top_n(10, wt = n) |> arrange(-n) |>
  mutate(SO = ifelse(str_length(SO) > 20,
  str_replace_all(SO,c('AND'='AND\n',':'=':\n')),SO)) |>
  ggplot(aes(x = reorder(SO,n), y = n))+
  geom_bar(stat= 'identity', fill = "grey50",colour = "black",alpha = 0.15,
           linewidth = 0.4)+
  geom_text(aes(label = n), hjust= 1.25)+
  coord_flip()+
  theme_f(expandy = c(0,0,0,7), namey = "Frequency", namex = 'Sources')+
  theme(line = element_blank())

ggsave("Relatorios/Figuras/Most Relevant Sources.png", width = 22, height = 12, units = "cm")

# -----------------------------------------------------------------------------#
# Most Local Cited Sources (from Reference Lists)

SO_CITED1 = metaTagExtraction(dados,"CR_SO")
TAB1<- tableTag(SO_CITED1,"CR_SO")

data.frame(TAB1) |> top_n(10,Freq) |> head(10) |> rename("SO"="Tab") |>
  mutate(SO = str_replace_all(SO,c('AND'='AND\n',':'=':\n')),SO) |> 
  ggplot(aes(x = reorder(SO,Freq), y = Freq))+
  geom_bar(stat= 'identity', fill = "grey50",colour = "black",alpha = 0.15,
  linewidth = 0.4)+
  geom_text(aes(label = Freq), hjust= 1.25)+
  coord_flip()+
  theme_f(namex = "Sources", namey = "Frequency", expandy = c(0,0,0,20))+
  theme(line = element_blank())

ggsave("Relatorios/Figuras/Most Local Cited Sources.png", width = 22, height = 12, units = "cm")

# -----------------------------------------------------------------------------#
# Source Local Impact

hindex<-  Hindex(dados, field = "source", sep = ";",year = 100)
 
hindex$H |> top_n(10,h_index) |> arrange(h_index) |> tail(10) |>
  mutate(Element = str_replace_all(Element,c('AND'='AND\n',':'=':\n')),Element) |> 
  ggplot(aes(x = reorder(Element,h_index), y = h_index))+
  geom_bar(stat= 'identity', fill = "grey50",colour = "black",alpha = 0.15) +
  geom_text(aes(label = h_index), hjust= 1.25)+
  coord_flip()+
  theme_f(namey = "H index", namex = "Sources", expandy = c(0,0,0,1.2))+
  theme(line = element_blank())

ggsave("Relatorios/Figuras/Sources' Local Impact by H index.png", 
       width = 22, height = 12, units = "cm")

# -----------------------------------------------------------------------------#
# Source Dynamics

sourcestop <- dados |> count(SO) |> top_n(5,n) |> select(SO) |> as_vector() |> 
head(5)

dados |> filter(SO %in% sourcestop) |>
  mutate(SO = str_replace_all(SO,c('AND '='AND\n',': '=':\n')),SO) |>
  mutate(across(everything(), ~ as.factor(.))) |> 
  count(SO,PY, .drop = F) |> mutate(across(PY, ~ parse_number(as.character(.)))) |> 
  arrange(SO,PY) |> group_by(SO) |> 
  mutate(CUMN = cumsum(n)) |> ungroup() |> mutate(PY= str_sub(PY,start = 3),ordem = 1:n()) |>
  ggplot(aes(x = reorder(PY,ordem), y = CUMN, colour = SO, group = SO))+
  geom_line(linewidth = 1.1)+
  theme_f(namey = "Frequency", namex = "Years")+
  guides(colour = guide_legend(nrow = 2))+
  scale_colour_manual(values = brewer.pal(5,"Dark2"))

ggsave("Relatorios/Figuras/Sources' Production over Time.png", 
       width = 22, height = 12, units = "cm")

# -----------------------------------------------------------------------------#
# Most Productive Authors

results$Authors |> data.frame() |>
  top_n(10,Freq) |> head(10) |> rename("SO"="AU") |>  
  ggplot(aes(x = reorder(SO,Freq), y = Freq))+
  geom_bar(stat= 'identity', fill = "grey50",colour = "black",alpha = 0.15,
           linewidth = 0.4)+
  geom_text(aes(label = Freq), hjust= 1.25)+
  coord_flip()+
  theme_f(namey = "Frequency", namex = "Authors", expandy = c(0,0,0,1))+
  theme(line = element_blank())

ggsave("Relatorios/Figuras/Most Productive Authors.png", width = 22, height = 12, units = "cm")

# -----------------------------------------------------------------------------#
# Most Local Cited Authors

# Para bases do scopus a funcao localCitations nao pode ser mais usada
# sugestao do proprio autor

# -----------------------------------------------------------------------------#
# Authors' Production over Time

topAU <- authorProdOverTime(dados, k = 10, graph = F)

topAU$graph+
  theme(plot.title = element_blank())
  
ggsave("Relatorios/Figuras/Authors Production over Time.png", width = 22, height = 12, units = "cm")

# -----------------------------------------------------------------------------#
# Most Productive Authors Density per Year

topAU <- authorProdOverTime(dados, k = 10, graph = F)

auttop <- results$Authors |> data.frame() |> 
  top_n(20,Freq) |> head(20) |> select(AU)

topAU$dfAU |> filter(Author %in% unique(auttop$AU)) |> select(Author,year,freq) |> 
  uncount(freq) |> group_by(Author) |> mutate(id = cur_group_id()) |> 
  filter(id < 10) |>
  ggplot(aes(x = as.character(year)))+
  geom_density(aes(y = after_stat(count),group =1), fill ='grey50',alpha = 0.15)+
  facet_wrap(vars(Author))+
  ylab("Frequency")+
  xlab("Year")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))

ggsave("Relatorios/Figuras/Most Productive Authors Density Year.png", width = 22, height = 12, units = "cm")

# -----------------------------------------------------------------------------#
# Author Local Impact

hindex<- Hindex(dados, field = "author", sep = ";",year = 300)

hindex$H |> top_n(10,h_index) |> arrange(h_index) |> tail(10) |>
  ggplot(aes(x = reorder(Element,h_index), y = h_index))+
  geom_bar(stat= 'identity', fill = "grey50",colour = "black",alpha = 0.15) +
  geom_text(aes(label = h_index), hjust= 1.25)+
  coord_flip()+
  theme_f(namey = "H index", namex = "Authors", expandy = c(0,0,0,0.6))+
  theme(line = element_blank())

ggsave("Relatorios/Figuras/Author Local Impact by H index.png",
       width = 22, height = 12, units = "cm")

# -----------------------------------------------------------------------------#
# Most Relevant Affiliations

results$Affiliations |> as_tibble() |>
  group_by(AFF) |> reframe(n = sum(n)) |> top_n(10,n) |> rename("SO"="AFF") |>
  arrange(n) |> mutate(ordem = c(2:n(),1)) |> 
  ggplot(aes(x = reorder(SO,ordem), y = n))+
  geom_bar(stat= 'identity', fill = "grey50",colour = "black",alpha = 0.15,
           linewidth = 0.4)+
  geom_text(aes(label = n), hjust= 1.25)+
  coord_flip()+
  theme_f(namey = "Frequency", namex = "Affiliations", expandy = c(0,0,0,3))+
  theme(line = element_blank())

ggsave("Relatorios/Figuras/Most Relevant Affiliations.png", width = 22, height = 12, units = "cm")

# -----------------------------------------------------------------------------#
# Affiliations' Production over Time

affiltop <- results$Affiliations |> as_tibble() |> 
  filter(!AFF %in% c('NON REPORTED','NOTREPORTED')) |> top_n(6,n) |>
  select(AFF) |> as_vector()

AF_CITED = metaTagExtraction(dados, aff.disamb = F)

AF_CITED |> select(AU_UN,PY) |> separate_rows(AU_UN,sep = ";") |> 
  filter(AU_UN %in% affiltop) |>
  mutate(across(everything(), ~ as.factor(.))) |> 
  count(AU_UN,PY, .drop = F) |> mutate(across(PY, ~ parse_number(as.character(.)))) |> 
  arrange(AU_UN,PY) |> group_by(AU_UN) |> 
  mutate(CUMN = cumsum(n)) |> 
  ggplot(aes(x = PY, y = CUMN, colour = AU_UN, group = AU_UN))+
  geom_line(linewidth = 1.1)+
  xlab("Years")+
  ylab("Frequency")+
  guides(colour = guide_legend(nrow = 2))+
  scale_colour_manual(values = brewer.pal(9,"Paired"))+
  theme_bw()+
  theme(legend.position = "top", legend.title = element_blank(), 
        legend.text = element_text(size = 8), legend.margin = margin(-5,0,-10,0))

ggsave("Relatorios/Figuras/Affiliations Production over Time.png", 
       width = 22, height = 12, units = "cm")

# -----------------------------------------------------------------------------#
# Most Global Cited Documents

results$MostCitedPapers |> top_n(10,TC) |> rename("SO"="Paper         ") |>  
  mutate(SO = stri_sub(SO,to = stri_locate_last_fixed(SO,",")[,1] - 1)) |>
  mutate(SO = str_replace_all(SO,c(', UNDERST CONSCIOUS'=''))) |> 
  ggplot(aes(x = reorder(SO,TC), y = TC))+
  geom_bar(stat= 'identity', fill = "grey50",colour = "black",alpha = 0.15,
           linewidth = 0.4)+
  geom_text(aes(label = TC), hjust= 1.25)+
  coord_flip()+
  theme_f(namey = "Frequency", namex = "Pappers", expandy = c(0,0,0,10))+
  theme(line = element_blank())

ggsave("Relatorios/Figuras/Most Global Cited Documents.png", 
       width = 22, height = 12, units = "cm")

dados |>  filter(DI %in% top_n(results$MostCitedPapers,10,TC)$DOI) |> 
  select(AU,TI) |> as_tibble() |> 
  write.xlsx("Relatorios/Tabelas/Most Global Cited Documents.xlsx")

# -----------------------------------------------------------------------------#
# Most Local Cited Documents

# LC11 = localCitations(dados) ## PARA O SCOPUS NAO FUNCIONA MAIS

# -----------------------------------------------------------------------------#
# Most Local Cited Author

LCR <- citations(dados,'author')$Cited |> as_tibble()

LCR |> filter(!is.na(CR), CR != 'ANONYMOUS') |>
  top_n(10,n) |> head(10) |>
  ggplot(aes(x = reorder(CR,n), y = n))+
  geom_bar(stat= 'identity', fill = "grey50",colour = "black",alpha = 0.15,
           linewidth = 0.4)+
  geom_text(aes(label = n), hjust= 1.25)+
  theme_f(namey = "Frequency", namex = "Pappers", expandy = c(0,0,0,20))+
  coord_flip()+ theme(line = element_blank())

ggsave("Relatorios/Figuras/Most Local Cited References.png", 
       width = 22, height = 12, units = "cm")

LCR |> filter(!is.na(CR), CR != 'ANONYMOUS') |> top_n(10,n) |> head(10) |> 
  write.xlsx("Relatorios/Tabelas/Most Local Cited References.xlsx")

# ---------------------------------------------------------------------------- #
# Most Frequent Words (TreeMap)

words1 <- KeywordGrowth(dados,top = 50)
tops <- 20

words1 |> select(!Year) |> tail(1) |> pivot_longer(everything()) |>
  group_by(name) |> reframe(value = sum(value)) |>
  top_n(20,value) |> head(tops) |>
  mutate(propvalue = value/sum(value), propvalue = formattable::percent(propvalue,1)) |> 
  ggplot(aes(area = value, fill = name,
                 label = paste0(" ",name,"\n ",value,"\n ",propvalue))) +
  geom_treemap(colour = "black",start = "topleft",size = 0.5) +
  geom_treemap_text(colour = rep(c("black","white"),tops)[1:tops],
                    size = 6, start = "topleft") +
  scale_fill_manual(values = c(brewer.pal(12,"Paired"),brewer.pal(12,"Paired"),
                               brewer.pal(12,"Paired"))) +
  theme(legend.position = "none")

ggsave("Relatorios/Figuras/FrequentsWordsTreeMap.png", 
       width = 22, height = 12, units = "cm")

# ---------------------------------------------------------------------------- #
# Most Frequent Words (Bar)

words1 |> pivot_longer(!Year) |> group_by(name) |> 
  reframe(value = sum(value)) |> 
  top_n(20,value) |> head(20) |> arrange(-value) |> 
  mutate(paleta) |>
  ggplot(aes(y = value, x= reorder(name,-value))) +
  geom_bar(stat ='identity',fill = paleta) +
  theme_f(namey = 'Word Occurrence',sizep = -1)+
  theme(legend.position = "none",line = element_blank(),
  axis.text.x = element_text(angle = 45,colour = 'black',hjust = 1,size=8),
        axis.title.x = element_blank())

ggsave("Relatorios/Figuras/FrequentsWordsBar.png", 
       width = 18, height = 14, units = "cm")

# ---------------------------------------------------------------------------- #
# Word Dynamics

words1[,1:11] |> pivot_longer(!Year) |> 
  distinct(name,Year, .keep_all = T) |> 
  group_by(name) |> top_n(10,sum(value)) |>
  ggplot(aes(x = Year, y = value, colour = name, group = name))+
  geom_line(linewidth = 1.1)+
  xlab("Years")+
  ylab("Frequency")+
  guides(colour = guide_legend(nrow = 2))+
  scale_colour_manual(values = brewer.pal(10,"Paired"))+
  theme_bw()+
  theme(legend.position = "top", legend.title = element_blank(), 
        legend.text = element_text(size = 8), legend.margin = margin(-5,0,-10,0))

ggsave("Relatorios/Figuras/Word Dynamics.png", 
       width = 22, height = 12, units = "cm")

# ---------------------------------------------------------------------------- #
# Word Percent Annual

palavras<-  words1[,1:11] |> pivot_longer(!Year) |> 
 mutate(Year = DescTools::RoundTo(Year,10,floor),
Year = paste0(Year,'-',Year + 9)) |> group_by(Year,name) |> 
  reframe(value = sum(value)) |> filter(value > 0) |>
  arrange(Year,-value) |>  group_by(Year) |>
  mutate(value = value/sum(value)) |> 
  slice_head(n= 10)

palavras |> 
  ggplot(aes(label = name, size = value)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 4) +
  facet_wrap(vars(Year)) +
  theme_classic()

palavras |> arrange(Year,-value) |>  group_by(Year) |>
  mutate(value = round(value/sum(value)*100,2)) |> slice_head(n= 10) |>
  mutate(Top = 1:n()) |> ungroup() |> 
  mutate(name = str_c(name,' (',value,'%)')) |> select(!value) |> 
  pivot_wider(names_from = Year,values_from = name) |>
  write.xlsx('Relatorios/Tabelas/Top_Keywors_Periodo.xlsx')

# ---------------------------------------------------------------------------- #
# KeyWord / University x Not Reported

notreported = KeywordGrowth(dados[str_detect(results$FirstAffiliation,'NOTREPORTED'),],top = 20)
universidade = KeywordGrowth(dados[!str_detect(results$FirstAffiliation,'NOTREPORTED'),],top = 20)

universidade <- universidade |> select(where(function(x) {sum(x)>0})) |> 
  pivot_longer(!Year) |> mutate(Classe= 'Universidade') |> 
  select(!"Year")

notreported<- notreported |> select(where(function(x) {sum(x)>0})) |>
  pivot_longer(!Year) |> mutate(Classe= 'Sem Afiliação') |> 
  select(!"Year")

keyword_aff <- bind_rows(universidade,notreported)

keyword_aff |> group_by(Classe,name) |> reframe(value = sum(value)) |> 
  group_by(Classe) |> mutate(value = value/sum(value)) |> 
  arrange(-value) |> slice_head(n  = 15) |>
  ggplot(aes(label = name, size = value)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 8) +
  facet_wrap(vars(Classe)) +
  theme_classic()

keyword_aff |> group_by(Classe,name) |> reframe(value = sum(value)) |> 
  arrange(Classe,-value) |>  group_by(Classe) |>
  mutate(value = round(value/sum(value)*100,2)) |> slice_head(n= 10) |>
  mutate(Top = 1:n()) |> ungroup() |> 
  mutate(name = str_c(name,' (',value,'%)')) |> select(!value) |> 
  pivot_wider(names_from = Classe,values_from = name) |>
write.xlsx('Relatorios/Tabelas/Top_Keywors_University_NotReported.xlsx')

# ---------------------------------------------------------------------------- #
# Trend Topics

ttref <- fieldByYear(dados ,graph = F)$df_graph |> ungroup()
filt <- ttref$freq |> sort() |> tail(20) |>  head(1)

ttref <- fieldByYear(dados ,graph = F,min.freq = filt)

ttref$graph +
  theme(plot.title = element_blank())

ggsave("Relatorios/Figuras/Trend Topics.png", width = 22, height = 12, units = "cm")

# ---------------------------------------------------------------------------- #
# Collaboration WorldMap

M1 = metaTagExtraction(dados,"AU_CO")
  
  net=biblioNetwork(M1,analysis="collaboration",network="countries")
  CO=data.frame(Tab=rownames(net),Freq=diag(as.matrix(net)),stringsAsFactors = FALSE)
  bsk.network=igraph::graph_from_adjacency_matrix(net,mode="undirected")
  COedges=as.data.frame(igraph::ends(bsk.network,igraph::E(bsk.network),names=TRUE),stringsAsFactors = FALSE)
  
  map.world <- map_data("world")
  map.world$region=toupper(map.world$region)
  map.world$region=gsub("UK","UNITED KINGDOM",map.world$region)
  map.world$region=gsub("SOUTH KOREA","KOREA",map.world$region)
  country.prod <- dplyr::left_join( map.world, CO, by = c('region' = 'Tab'))
  
  breaks=as.numeric(round(quantile(CO$Freq,c(0.2,0.4,0.6,0.8,1))))
  names(breaks)=breaks
  breaks=log(breaks)
  data("countries",envir=environment())
  names(countries)[1]="Tab"
  COedges=dplyr::inner_join(COedges,countries, by=c('V1'='Tab'))
  COedges=dplyr::inner_join(COedges,countries, by=c('V2'='Tab'))
  COedges=COedges[COedges$V1!=COedges$V2,]
  COedges=count.duplicates(COedges)
  tab=COedges
  COedges=COedges[COedges$count>=2,]
  
  ggplot(country.prod, aes( x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = Freq),colour = "black",size = 0.1) +
    geom_curve(data=COedges, aes(x = Longitude.x , y = Latitude.x,
    xend = Longitude.y, yend = Latitude.y, group=continent.x),
    size = 0.5, color = "firebrick3", curvature = 0.33, alpha = 0.5) +
    scale_size_continuous(guide = 'none', range = c(0.25, 2))+
    scale_fill_gradientn(trans = "log10",colours = blues9[-1],name = "Frequency")+
    labs(title = NULL, x = "Latitude", y = "Longitude") +
    theme(text = element_text(color = '#333333'), plot.title = element_text(size = 28),
          plot.subtitle = element_text(size = 14), axis.ticks = element_blank(),
          axis.text = element_blank(), panel.grid = element_blank(),
          panel.background = element_rect(fill = '#FFFFFF'),  
          plot.background = element_rect(fill = '#FFFFFF'),
          legend.position = "right"
    )

ggsave("Relatorios/Figuras/Collaboration WorldMap.png", width = 22, height = 12, units = "cm")

# ---------------------------------------------------------------------------- #
# MCA

CS <- conceptualStructure(dados, field="ID", method="MCA",
      minDegree=30, clust=5, stemming=FALSE, labelsize=10, documents=20)

CS$graph_terms

ggsave("Relatorios/Figuras/Conceptual Structure Map MCA.png", width = 22,
       height = 12, units = "cm",bg='white')

CS$graph_documents_Contrib

ggsave("Relatorios/Figuras/Factorial map docs contributes MCA.png", width = 22,
       height = 12, units = "cm",bg='white')

CS$graph_documents_TC

ggsave("Relatorios/Figuras/Factorial map most cited MCA.png", width = 22,
       height = 12, units = "cm",bg='white')

# ---------------------------------------------------------------------------- #
# CA

CS <- conceptualStructure(dados, field="ID", method="CA",
      minDegree=30, clust=5, stemming=FALSE, labelsize=10, documents=20)

CS$graph_terms

ggsave("Relatorios/Figuras/Conceptual Structure Map CA.png", width = 22,
       height = 12, units = "cm")

CS$graph_documents_Contrib

ggsave("Relatorios/Figuras/Factorial map docs contributes CA.png", width = 22,
       height = 12, units = "cm")

CS$graph_documents_TC

ggsave("Relatorios/Figuras/Factorial map most cited CA.png", width = 22,
       height = 12, units = "cm")

# ---------------------------------------------------------------------------- #
# Direct citation link 

# histResults <- histNetwork(dados, min.citations = 20, sep = ";")
## NAO SE UTILIZA MAIS EM BASES DO SCOPUS

# ---------------------------------------------------------------------------- #
# Bradford Law

brad = bradford(dados)

brad$graph

ggsave("Relatorios/Figuras/BradFord Law.png", width = 22,
       height = 12, units = "cm", bg = 'white')

# ---------------------------------------------------------------------------- #
# Spectroscopy

spec = rpys(dados)

spec$spectroscopy + 
  theme(plot.title = element_blank())

ggsave("Relatorios/Figuras/spectroscopy.png", width = 22,
       height = 12, units = "cm", bg = 'white')

# ---------------------------------------------------------------------------- #
# Jornais com alto impacto

jornais = c('NEUROQUANTOLOGY','PHYSICS ESSAYS') ## Jornais com baixo impacto

SO_CITED = metaTagExtraction(dados,"CR_SO")
TAB <- tableTag(SO_CITED,"CR_SO")

data.frame(TAB) |> filter(!Tab %in% jornais) |> 
  top_n(10,Freq) |> head(10) |> rename("SO"="Tab") |>
  mutate(SO = str_replace_all(SO,c('AND'='AND\n',':'=':\n')),SO) |> 
  ggplot(aes(x = reorder(SO,Freq), y = Freq))+
  geom_bar(stat= 'identity', fill = "grey50",colour = "black",alpha = 0.15,
           linewidth = 0.4)+
  geom_text(aes(label = Freq), hjust= 1.25)+
  coord_flip()+
  theme_f(namex = "Sources", namey = "Frequency", expandy = c(0,0,0,20))+
  theme(line = element_blank())

ggsave("Relatorios/Figuras/Most Local Cited Sources by High Journals.png", width = 22, height = 12, units = "cm")

# ---------------------------------------------------------------------------- #
# Jornais com baixo impacto

SO_CITED = metaTagExtraction(dados,"CR_SO")
TAB <- tableTag(SO_CITED,"CR_SO")

data.frame(TAB) |> filter(Tab %in% jornais) |> 
  top_n(10,Freq) |> head(10) |> rename("SO"="Tab") |>
  mutate(SO = str_replace_all(SO,c('AND'='AND\n',':'=':\n')),SO) |> 
  ggplot(aes(x = reorder(SO,Freq), y = Freq))+
  geom_bar(stat= 'identity', fill = "darkblue",colour = "black",alpha = 0.15,
           linewidth = 0.4)+
  geom_text(aes(label = Freq), hjust= 1.25)+
  coord_flip()+
  theme_f(namex = "Sources", namey = "Frequency", expandy = c(0,0,0,20))

ggsave("Relatorios/Figuras/Most Local Cited Sources by Low Journals.png", width = 22, height = 12, units = "cm")
