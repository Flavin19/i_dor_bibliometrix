# -----------------------------------------------------------------------------#
# Carregando pacotes, arquivos e funcoes auxiliares

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

Anos = dados$PY
Anos = data.frame(Anos = Anos) |> filter(!is.na(Anos)) |> 
  count(Anos) |>  
  bind_rows(data.frame(Anos = min(dados$PY):year(Sys.Date()),n = 0)) |>
  group_by(Anos) |> reframe(n = sum(n)) |>
  mutate(ordem = 1:n(), Anos = str_sub(Anos, start= 3))
  
Anos |> 
ggplot(aes(x = reorder(Anos,ordem), y= n, group = 1))+
  geom_area(fill = "grey50",alpha = 0.15)+
  geom_line(linewidth = 0.5)+
theme_f(expandy = c(0,5,0,10), namex = 'Years', namey = 'Frequency',
        sizep =  2, breaksx = Anos$Anos[seq(1,length(Anos$Anos),2)])

ggsave("Relatorios/Figuras/Annual Scientific Production.png" ,
       width = 22, height = 12, units = "cm")

# ---------------------------------------------------------------------------- #
# Average Citation Per Year

anosf = seq(min(dados$PY),max(dados$PY),1)
anosf<- anosf[!anosf %in% unique(dados$PY)]

Anos = dados |> rename('Anos' = 'PY', 'n'='TC') |>
  bind_rows(tibble(Anos = anosf,n= 0)) |>
  group_by(Anos) |> reframe(n = mean(n,na.rm = T)) |> 
  mutate(Anos2 = year(Sys.Date()) - Anos,n2 = n/Anos2)

Anos |> 
  ggplot(aes(x = as.character(Anos), y= n, group = 1))+
  geom_area(fill = "darkblue",alpha = 0.08)+
  geom_line(linewidth = 0.4,linetype = 1)+
  #  geom_text(aes(label = n), vjust = - 0.8, hjust = 0.8) +
  theme_f(expandy = c(0,1,0,10), namex = 'Years', namey = 'Frequency',
          sizep =  0, breaksx = seq(min(dados$PY),max(dados$PY),3))+
  theme(axis.text.x = element_text(angle = 90))

ggsave("Relatorios/Figuras/Average Total Citations per Year.png",
       width = 22, height = 12, units = "cm")

Anos |> 
  ggplot(aes(x = as.character(Anos), y= n2, group = 1))+
  geom_area(fill = "darkblue",alpha = 0.08)+
  geom_line(linewidth = 0.4,linetype = 1)+
  #  geom_text(aes(label = n), vjust = - 0.8, hjust = 0.8) +
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
  geom_bar(stat= 'identity', fill = "darkblue",colour = "black",alpha = 0.15,
           linewidth = 0.4)+
  geom_text(aes(label = n), hjust= 1.25)+
  coord_flip()+
  theme_f(expandy = c(0,0,0,7), namey = "Frequency", namex = 'Sources')

ggsave("Relatorios/Figuras/Most Relevant Sources.png", width = 22, height = 12, units = "cm")

# -----------------------------------------------------------------------------#
# Most Local Cited Sources (from Reference Lists)

SO_CITED1 = metaTagExtraction(dados[dados$DB == 'ISI',],"CR_SO")
SO_CITED2 = metaTagExtraction(dados[dados$DB != 'ISI',],"CR_SO")

TAB1<- tableTag(SO_CITED1,"CR_SO")
TAB2 <- tableTag(SO_CITED2,"CR_SO")

data.frame(TAB1) |> bind_rows(data.frame(TAB2)) |> 
  group_by(Tab) |> mutate(Freq = sum(Freq)) |>
  top_n(10,Freq) |> head(10) |> rename("SO"="Tab") |>
  mutate(SO = str_replace_all(SO,c('AND'='AND\n',':'=':\n')),SO) |> 
  ggplot(aes(x = reorder(SO,Freq), y = Freq))+
  geom_bar(stat= 'identity', fill = "darkblue",colour = "black",alpha = 0.15,
  linewidth = 0.4)+
  geom_text(aes(label = Freq), hjust= 1.25)+
  coord_flip()+
  theme_f(namex = "Sources", namey = "Frequency", expandy = c(0,0,0,20))

ggsave("Relatorios/Figuras/Most Local Cited Sources.png", width = 22, height = 12, units = "cm")

# -----------------------------------------------------------------------------#
# Source Local Impact

hindex<-  Hindex(dados, field = "source", sep = ";",year = 100)
 
hindex$H |> top_n(10,h_index) |> arrange(h_index) |> tail(10) |>
  mutate(Element = str_replace_all(Element,c('AND'='AND\n',':'=':\n')),Element) |> 
  ggplot(aes(x = reorder(Element,h_index), y = h_index))+
  geom_bar(stat= 'identity', fill = "darkblue",colour = "black",alpha = 0.15) +
  geom_text(aes(label = h_index), hjust= 1.25)+
  coord_flip()+
  theme_f(namey = "H index", namex = "Sources", expandy = c(0,0,0,1.2))

ggsave("Relatorios/Figuras/Sources' Local Impact by H index.png", 
       width = 22, height = 12, units = "cm")

# -----------------------------------------------------------------------------#
# Source Dynamics

sourcestop <- dados |> count(SO) |> top_n(5,n) |> select(SO) |> as_vector() |> 
head(5)

dados |> filter(SO %in% sourcestop) |> mutate(across(everything(), ~ as.factor(.))) |> 
  count(SO,PY, .drop = F) |> mutate(across(PY, ~ parse_number(as.character(.)))) |> 
  arrange(SO,PY) |> group_by(SO) |> 
  mutate(CUMN = cumsum(n)) |> ungroup() |> mutate(PY= str_sub(PY,start = 3),ordem = 1:n()) |>
  mutate(SO = str_replace_all(SO,c('AND '='AND\n',':'=':\n')),SO) |> 
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
  geom_bar(stat= 'identity', fill = "darkblue",colour = "black",alpha = 0.15,
           linewidth = 0.4)+
  geom_text(aes(label = Freq), hjust= 1.25)+
  coord_flip()+
  theme_f(namey = "Frequency", namex = "Authors", expandy = c(0,0,0,1))

ggsave("Relatorios/Figuras/Most Productive Authors.png", width = 22, height = 12, units = "cm")

# -----------------------------------------------------------------------------#
# Most Local Cited Authors

LC1AU = dados |> as_tibble() |> select(AU,PY) |> 
  mutate(AU = gsub("\\;.*", "", AU)) |> 
  mutate(Citado = TRUE)

LC1CR = dados |> as_tibble() |> select(CR,DB) |> 
mutate(Artigo = 1:n(), CR = 
case_when(DB == 'SCOPUS' ~ 
gsub("[)]", ",",gsub("[(]", "",gsub('\\.','',
gsub("1234", ",",gsub("\\, ", " ", gsub("\\.,", "1234",
  gsub("\\.,.*?\\(", "., (", CR))))))),
.default =  CR)) |> 
  filter(!is.na(CR)) |> separate_longer_delim(CR,delim = ';') |>
  mutate(AU = trimws(gsub("[ ]{2,}", "", (gsub("\\.", " ", unlist(lapply(strsplit(CR, 
  ",", fixed = TRUE), "[", 1)))))),
  PY = as.numeric(trimws(unlist(lapply(strsplit(CR, ",", fixed = TRUE), 
                       "[", 2))))) |> 
  filter(!is.na(PY)) |> mutate(AU = str_replace_all(AU,fixed(c("*"='')))) |> 
  distinct(AU,PY,Artigo) |> select(AU,PY)

LC = LC1CR |> left_join(LC1AU) |> filter(!is.na(Citado))

LC |> count(AU) |> arrange(-n) |> head(10) |>
  rename("Freq"="n",'Author'='AU') |>  
  ggplot(aes(x = reorder(Author,Freq), y = Freq))+
  geom_bar(stat= 'identity', fill = "darkblue",colour = "black",alpha = 0.15,
           linewidth = 0.4)+
  geom_text(aes(label = Freq), hjust= 1.25)+
  coord_flip()+
  theme_f(namey = "Frequency", namex = "Authors", expandy = c(0,0,0,1.1))

ggsave("Relatorios/Figuras/Most Local Cited Authors.png", width = 22, height = 12, units = "cm")

# -----------------------------------------------------------------------------#
# Authors' Production over Time

topAU1 <- authorProdOverTime(dados[dados$DB == 'ISI',], k = 10, graph = F)
topAU2 <- authorProdOverTime(dados[dados$DB != 'ISI',], k = 10, graph = F)
topAU <- bind_rows(topAU1$dfAU,topAU2$dfAU)

topAU |> group_by(Author,year) |> 
  reframe(freq = sum(freq), TC = sum(TC), TCpY = sum(TCpY)) |> ungroup() |>
  filter(Author %in% (LC |> count(AU) |> arrange(-n) |> head(10))$AU) |> 
  ggplot(aes(x = Author, y = year)) +
  geom_point(aes(alpha = TCpY, size = freq), 
  color = "dodgerblue4") + 
  coord_flip()+
  scale_size(range = c(2, 6)) + 
  scale_alpha(range = c(0.3, 1)) + 
  guides(size = guide_legend(order = 1, 
  "N.Articles"), alpha = guide_legend(order = 2, "TC per Year")) + 
  theme(legend.position = "right", text = element_text(color = "#444444"), 
  panel.background = element_rect(fill = "#FFFFFF"),
  axis.text.x = element_text(face = "bold", angle = 90), 
  axis.text.y = element_text(face = "bold"), 
  axis.line.x = element_line(color = "grey50", 
  size = 0.5), panel.grid.major.x = element_blank()) + 
  geom_line(aes(x =Author, y = year, group = Author), size = 1, color = "firebrick4", 
  alpha = 0.3) + 
  scale_y_continuous(name = "Year", breaks = seq(min(topAU$year),
  max(topAU$year), by = 2))

ggsave("Relatorios/Figuras/Authors Production over Time.png", width = 22, height = 12, units = "cm")

# -----------------------------------------------------------------------------#
# Most Productive Authors Density per Year

auttop <- results$Authors |> data.frame() |> 
  top_n(20,Freq) |> head(20) |> select(AU)

topAU |> filter(Author %in% unique(auttop$AU)) |> select(Author,year,freq) |> 
  uncount(freq) |> group_by(Author) |> mutate(id = cur_group_id()) |> 
  filter(id < 10) |>
  ggplot(aes(x = as.character(year)))+
  geom_density(aes(y = after_stat(count),group =1), fill ='darkblue',alpha = 0.25)+
  facet_wrap(vars(Author))+
  ylab("Frequency")+
  xlab("Year")+
  theme(axis.text.x = element_text(angle = 90))

ggsave("Relatorios/Figuras/Most Productive Authors Density Year.png", width = 22, height = 12, units = "cm")

# -----------------------------------------------------------------------------#
# Author Local Impact

hindex<- Hindex(dados, field = "author", sep = ";",year = 300)

hindex$H |> top_n(10,h_index) |> arrange(h_index) |> tail(10) |>
  ggplot(aes(x = reorder(Element,h_index), y = h_index))+
  geom_bar(stat= 'identity', fill = "darkblue",colour = "black",alpha = 0.15) +
  geom_text(aes(label = h_index), hjust= 1.25)+
  theme_bw()+
  coord_flip()+
  xlab("Authors")+
  scale_y_continuous(expand = c(0,0,0,0.6),name = "H index")

ggsave("Relatorios/Figuras/Author Local Impact by H index.png",
       width = 22, height = 12, units = "cm")

# -----------------------------------------------------------------------------#
# Most Relevant Affiliations

results$Affiliations |> as_tibble() |>
  group_by(AFF) |> reframe(n = sum(n)) |> top_n(10,n) |> rename("SO"="AFF") |>
  arrange(n) |> mutate(ordem = c(1:n())) |> 
  ggplot(aes(x = reorder(SO,ordem), y = n))+
  geom_bar(stat= 'identity', fill = "darkblue",colour = "black",alpha = 0.15,
           linewidth = 0.4)+
  geom_text(aes(label = n), hjust= 1.25)+
  theme_bw()+
  coord_flip()+
  xlab("Affiliations")+
  scale_y_continuous(expand = c(0,0,0,3),name = "Frequency")

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
  geom_bar(stat= 'identity', fill = "darkblue",colour = "black",alpha = 0.15,
           linewidth = 0.4)+
  geom_text(aes(label = TC), hjust= 1.25)+
  theme_bw()+
  coord_flip()+
  ylab("Frequency")+
  xlab("Pappers")+
  scale_y_continuous(expand = c(0,0,0,10))

ggsave("Relatorios/Figuras/Most Global Cited Documents.png", 
       width = 22, height = 12, units = "cm")

dados |>  filter(DI %in% top_n(results$MostCitedPapers,10,TC)$DOI) |> 
  select(AU,TI) |> as_tibble() |> 
  write.xlsx("Relatorios/Tabelas/Most Global Cited Documents.xlsx")

# -----------------------------------------------------------------------------#
# Most Local Cited Documents

LC11 = localCitations(dados[dados$DB == 'ISI',])
LC12 = localCitations(dados[dados$DB != 'ISI',] |> mutate(Page.start = 0 ,
                                                          Page.end = 0, PP = '1'))

LC = bind_rows(LC11$Papers,LC12$Papers) |> group_by(Paper) |>
  mutate(LCS = sum(LCS)) |> ungroup() |>  distinct(Paper, .keep_all = T)

LC |> 
top_n(10,LCS) |> rename("SO"="Paper") |>  
  mutate(SO = stri_sub(SO,to = stri_locate_last_fixed(SO,",")[,1] - 1)) |>
  ggplot(aes(x = reorder(SO,LCS), y = LCS))+
  geom_bar(stat= 'identity', fill = "darkblue",colour = "black",alpha = 0.15,
           linewidth = 0.4)+
  geom_text(aes(label = LCS), hjust= 1.25)+
  theme_bw()+
  coord_flip()+
  ylab("Frequency")+
  xlab("Pappers")+
  scale_y_continuous(expand = c(0,0,0,1))

ggsave("Relatorios/Figuras/Most Local Cited Documents.png", 
       width = 22, height = 12, units = "cm")

dados |>  filter(DI %in% top_n(LC,10,LCS)$DOI) |> select(AU,TI) |> 
  write.xlsx("Relatorios/Tabelas/Most Local Documents.xlsx")
  
# -----------------------------------------------------------------------------#
# Most Local Cited Author Reference

LCR1 <- citations(dados[dados$DB == 'ISI',],'author')
LCR2 <- citations(dados[dados$DB != 'ISI',],'author')

LCR <- bind_rows(LCR1$Cited|> data.table(),LCR2$Cited|> data.table())

LCR |> group_by(CR) |> reframe(N = sum(N)) |> 
  filter(!is.na(CR), CR != 'ANONYMOUS') |> top_n(15,N) |> head(10) |>
  ggplot(aes(x = reorder(CR,N), y = N))+
  geom_bar(stat= 'identity', fill = "darkblue",colour = "black",alpha = 0.15,
           linewidth = 0.4)+
  geom_text(aes(label = N), hjust= 1.25)+
  theme_f(namey = "Frequency", namex = "Pappers", expandy = c(0,0,0,2))+
  coord_flip()

ggsave("Relatorios/Figuras/Most Local Cited References.png", 
       width = 22, height = 12, units = "cm")

LCR |> group_by(CR) |> reframe(N = sum(N)) |> 
  filter(!is.na(CR), CR != 'ANONYMOUS') |> top_n(15,N) |> head(10) |> 
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

ggsave("Relatorios/Figuras/TreeMap.png", 
       width = 22, height = 12, units = "cm")

# ---------------------------------------------------------------------------- #
# Word Dynamics

words2 <- KeywordGrowth(dados,top = 50)

palavras <- words2 |> pivot_longer(!Year) |> 
  group_by(name) |> reframe(value = sum(value)) |> 
   top_n(10,value) |> head(10)

words2 |> pivot_longer(!Year) |> filter(name %in% palavras$name) |>
  group_by(name,Year) |> mutate(value = sum(value)) |>
  ungroup() |> 
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
library(ggwordcloud)

palavras = words1 |> pivot_longer(!Year) |> mutate(Year = DescTools::RoundTo(Year,10,floor),
Year = paste0(Year,'-',Year + 9)) |> group_by(Year,name) |> 
  reframe(value = sum(value)) |> filter(value > 0,!name %in% c('CONSCIOUSNESS','HUMAN'))

palavras |> arrange(Year,-value) |>  group_by(Year) |>
  mutate(value = value/sum(value)) |> 
  slice_head(n= 10) |> 
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

universidade<- universidade |> pivot_longer(!Year) |> mutate(Classe= 'Universidade') |> 
  select(!"Year")

notreported<- notreported |> pivot_longer(!Year) |> mutate(Classe= 'Sem Afiliação') |> 
  select(!"Year")

keyword_aff <- bind_rows(universidade,notreported) |> 
  filter(value > 0,!name %in% c('CONSCIOUSNESS','HUMAN'))

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

tt <- ttref |> mutate(item = as.character(item)) |>
  group_by(item,year_q1,year_med,year_q3) |> mutate(freq = sum(freq)) |>
  ungroup() |> distinct() |> top_n(25,freq)

  yrange <- range(unlist(tt[, which(regexpr("year", names(df)) > -1)]))
  x <- c(0 + 0.5, 0.05 + length(levels(tt$item)) * 0.125) + 1
  y <- c(yrange[2] - 0.02 - diff(yrange) * 0.125, yrange[2] - 0.02)
  
  tt |> 
  ggplot(aes(x = item, y = year_med))+
    geom_point(aes(size = freq), alpha = 0.6, color = "dodgerblue4")+
    scale_size(range = c(2, 6)) + 
    scale_y_continuous(breaks = seq(min(tt$year_q1), max(tt$year_q3), 
    by = 2)) +
    guides(size = guide_legend(order = 1,"Term frequency"),
    alpha = guide_legend(order = 2, "Term frequency")) + 
    theme(legend.position = "right", text = element_text(color = "#444444"), 
          panel.background = element_rect(fill = "#FFFFFF"), 
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "grey95"), 
          axis.line.x = element_line(color = "black", size = 0.5)) +
  labs(x = "Term", y = "Year") + 
  geom_segment(aes(x = item, y = year_q1, xend = item, yend = year_q3), size = 1, 
                 color = "royalblue4", alpha = 0.3) +
    coord_flip()

ggsave("Relatorios/Figuras/Trend Topics.png", width = 22, height = 12, units = "cm")

# ---------------------------------------------------------------------------- #
# Collaboration Network

value <- biblioNetwork(dados, analysis = "collaboration", short = T, n= 60,
                       network = "authors", sep = ";")

dimn <- dim(value)[1] - 1

dimsel <- NULL
for(i in 1:(dimn+1)) {
if((sum(value[,i] == 0)) != (dim(value)[1] -1)){
  dimsel <- c(dimsel,i)
}
}

png("Relatorios/Figuras/Collaboration Network.png",width = 900, height = 400, units = "px")

networkPlot(value[dimsel,dimsel], normalize="association", Title = NULL, type = "fruchterman",
  size.cex=TRUE, size=9 , remove.multiple=T, edgesize = 5,
  labelsize=3.2,label.cex=T, curved= T,
  label.n= 50,edges.min=1,label.color = F,alpha=0.7,
  remove.isolates = "yes", cluster= "Walktrap",
  community.repulsion = 0.01/2)

dev.off()

# ---------------------------------------------------------------------------- #
# Collaboration WorldMap

  M11= metaTagExtraction(dados[dados$DB == 'ISI',],"AU_CO")
  M12= metaTagExtraction(dados[dados$DB != 'ISI',],"AU_CO")
  
  M1= bind_rows(M11,M12)
  
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
    scale_size_continuous(guide = FALSE, range = c(0.25, 2))+
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
       height = 12, units = "cm")

CS$graph_documents_Contrib

ggsave("Relatorios/Figuras/Factorial map docs contributes MCA.png", width = 22,
       height = 12, units = "cm")

CS$graph_documents_TC

ggsave("Relatorios/Figuras/Factorial map most cited MCA.png", width = 22,
       height = 12, units = "cm")

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
# Bradford Law

brad = bradford(dados)

brad$graph

ggsave("Relatorios/Figuras/BradFord Law.png", width = 22,
       height = 12, units = "cm", bg = 'white')

# ---------------------------------------------------------------------------- #
# Spectroscopy

spec = rpys(dados)

ggsave("Relatorios/Figuras/spectroscopy.png", width = 22,
       height = 12, units = "cm", bg = 'white')

# ---------------------------------------------------------------------------- #
# Jornais com alto impacto

jornais = c('NEUROQUANTOLOGY','PHYSICS ESSAYS') ## Jornais com baixo impacto

dados_filtro = dados |> filter(!SO %in% jornais)

SO_CITED1 = metaTagExtraction(dados_filtro[dados_filtro$DB == 'ISI',],"CR_SO")
SO_CITED2 = metaTagExtraction(dados_filtro[dados_filtro$DB != 'ISI',],"CR_SO")

TAB1<- tableTag(SO_CITED1,"CR_SO")
TAB2 <- tableTag(SO_CITED2,"CR_SO")

data.frame(TAB1) |> bind_rows(data.frame(TAB2)) |> 
  group_by(Tab) |> mutate(Freq = sum(Freq)) |>
  top_n(10,Freq) |> head(10) |> rename("SO"="Tab") |>
  mutate(SO = str_replace_all(SO,c('AND'='AND\n',':'=':\n')),SO) |> 
  ggplot(aes(x = reorder(SO,Freq), y = Freq))+
  geom_bar(stat= 'identity', fill = "darkblue",colour = "black",alpha = 0.15,
           linewidth = 0.4)+
  geom_text(aes(label = Freq), hjust= 1.25)+
  coord_flip()+
  theme_f(namex = "Sources", namey = "Frequency", expandy = c(0,0,0,20))

ggsave("Relatorios/Figuras/Most Local Cited Sources by High Journals.png", width = 22, height = 12, units = "cm")

# ---------------------------------------------------------------------------- #
# Jornais com baixo impacto

jornais = c('NEUROQUANTOLOGY','PHYSICS ESSAYS') ## Jornais com baixo impacto

dados_filtro = dados |> filter(SO %in% jornais)

SO_CITED1 = metaTagExtraction(dados_filtro[dados_filtro$DB == 'ISI',],"CR_SO")
SO_CITED2 = metaTagExtraction(dados_filtro[dados_filtro$DB != 'ISI',],"CR_SO")

TAB1<- tableTag(SO_CITED1,"CR_SO")
TAB2 <- tableTag(SO_CITED2,"CR_SO")

data.frame(TAB1) |> bind_rows(data.frame(TAB2)) |> 
  group_by(Tab) |> mutate(Freq = sum(Freq)) |>
  top_n(10,Freq) |> head(10) |> rename("SO"="Tab") |>
  mutate(SO = str_replace_all(SO,c('AND'='AND\n',':'=':\n')),SO) |> 
  ggplot(aes(x = reorder(SO,Freq), y = Freq))+
  geom_bar(stat= 'identity', fill = "darkblue",colour = "black",alpha = 0.15,
           linewidth = 0.4)+
  geom_text(aes(label = Freq), hjust= 1.25)+
  coord_flip()+
  theme_f(namex = "Sources", namey = "Frequency", expandy = c(0,0,0,20))

ggsave("Relatorios/Figuras/Most Local Cited Sources by Low Journals.png", width = 22, height = 12, units = "cm")

