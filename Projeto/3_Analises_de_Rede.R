# -----------------------------------------------------------------------------#
# Carregando pacotes, arquivos e funcoes auxiliares

source("utils/utils_analise_de_rede.R")

# -----------------------------------------------------------------------------#
# Bases

load('Bases/base.RData')

# ---------------------------------------------------------------------------- #
# Co-citation Network

ref <- refciting(dados)

conet <- ggconetwork(ref,filterf = 5)

ggconetworkplot(conet)

ggsave("Relatorios/Figuras/cocitacao_geral.png", width = 22, height = 16, units = "cm")

conet$cluster |>
  write.xlsx("Relatorios/Tabelas/cocitacao_geral_cluster.xlsx")

for(i in 1:nrow(conet$clusters)){
  
  ggconetworkplot(conet,filtro = i)
  
  ggsave(paste0("Relatorios/Figuras/cocitacao_cluster_",i,".png"),
         width = 22, height = 16, units = "cm")
}

# CORTE ANOS

cortes <- c(min(dados$PY)-1,2000,2005,2010,2015,max(dados$PY))

for(i in 1:(length(cortes)-1)){
  
  corte1 = cortes[i]
  corte2 = cortes[i+1]
  
  ref_ano <- refciting(dados |> filter(PY > corte1,PY <= corte2))
  
  conet_ano <- ggconetwork(ref_ano,filterf = 3,weight_threshold = 2)
  
  ggconetworkplot(conet_ano)
  
  ggsave(paste0("Relatorios/Figuras/cocitacao_corte_",corte1,"_",corte2,".png"),
         width = 22, height = 16, units = "cm")

}

# ---------------------------------------------------------------------------- #
# Co-citation Network por tema

conet$community_labels$Community_name
temas <- c('Teste1','Teste2',"") ## coloque aqui os temas ou os clusters seguindo a ordem do vetor acima

conet_tema <- conet

conet_tema$community_labels<- conet_tema$community_labels |> 
  mutate(Community_name = temas)

ggconetworkplot(conet_tema)

ggsave("Relatorios/Figuras/cocitacao_tema.png",
                            width = 22, height = 16, units = "cm")

# ---------------------------------------------------------------------------- #
# Co-occurrence Network by SCOPUS

direct_citation <- dados |> mutate(citing_id = paste0("A",1:n())) |> select(citing_id,ID) |>
  separate_rows(ID,sep = ";") |> mutate(ID = str_squish(ID)) |>
  group_by(ID) |> filter(n()>3) |> ungroup() |> filter(ID != "")

references <- direct_citation |> distinct(ID) |> mutate(new_id_ref= 1:n())

direct_citation<- direct_citation |> left_join(references) |> select(new_id_ref,!new_id_ref)

ref <- list(REFERENCIAS = direct_citation, REFERENCIASUNICOS = references)

ggconetworkword(ref,filterf = 15, weight_threshold = 4,top_n = 3,
                top_n_per_com = 5, weightf = 0.025)

ggsave("Relatorios/Figuras/Co_occurrence_Network_by_SCOPUS.png",
       width = 22, height = 16, units = "cm")

# ---------------------------------------------------------------------------- #
# Co-occurrence Network by Author

direct_citation <- dados |> mutate(citing_id = paste0("A",1:n())) |> select(citing_id,DE) |>
  rename("ID"="DE") |> 
  separate_rows(ID,sep = ";") |> mutate(ID = str_squish(ID)) |>
  filter(!ID %in% c("ARTICLE","PRIORITY JOURNAL","MALE","FEMALE")) |>
  group_by(ID) |> filter(n()>0) |> ungroup() |> filter(ID != "")

references <- direct_citation |> distinct(ID) |> mutate(new_id_ref= 1:n())

direct_citation<- direct_citation |> left_join(references) |> select(new_id_ref,!new_id_ref)

ref <- list(REFERENCIAS = direct_citation, REFERENCIASUNICOS = references)

ggconetworkword(ref)

ggsave("Relatorios/Figuras/Co_occurrence_Network_by_Author.png",
       width = 22, height = 16, units = "cm")

# ---------------------------------------------------------------------------- #
# Thematic Map

tmapplot <- thematicMap(dados, n = 1000)

tmapplot$map

ggsave("Relatorios/Figuras/Thematic_Map.png", width = 22, height = 16, units = "cm")

# ---------------------------------------------------------------------------- #
# Thematic Evolution

tmev <- thematicEvolution(dados,years = c(2000,2005,2010,2015))

plotThematicEvolution(Nodes = tmev$Nodes,Edges = tmev$Edges)

# ---------------------------------------------------------------------------- #
# Matriz de Pesos

matrizpesos <- biblioNetwork(dados, n= 50)

matrizpesos |> as.matrix() |>  as_tibble() |> 
  mutate(Authors = row.names(matrizpesos)) |> select(Authors,!Authors) |> 
  write.xlsx("Relatorios/Tabelas/Matriz_Pesos_Apenas_Scopus.xlsx")
