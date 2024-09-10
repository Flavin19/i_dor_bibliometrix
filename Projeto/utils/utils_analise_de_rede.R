### 

# --------------------------- PACOTES NECESSARIOS -----------------------------#

pacotes <- c("tidyverse", "bib2df", "janitor", "rscopus", "biblionetwork","RColorBrewer",
"tidygraph", "ggraph", "ggnewscale",'stringi',"data.table",'openxlsx','ggwordcloud',
 "bibliometrix", "ggpubr","broom","viridis","treemapify","ggrepel",'igraph',
'ggh4x','stringdist')

githubpacotes <- c("thomasp85/scico","agoutsmedt/networkflow","ParkerICI/vite",
                   'hrbrmstr/pluralize')

for(i in c(pacotes,gsub(".*/", "",githubpacotes))){
  library(i, character.only = TRUE)
}

rm(pacotes,i,githubpacotes,gitlib)

# -----------------------------------------------------------------------------#

#### TIRANDO A LOGO

logo_dir <- paste0(find.package("bibliometrix"),'/data/Rdata.rds')
logo_data <- readRDS(logo_dir)
logo_data <- Filter(function(x){x!='logo'},logo_data)
saveRDS(logo_data,logo_dir)
rm(logo_data,logo_dir)

logo <- NA

matching_ref <- function(data, id_ref, ..., col_name){
  match <- data %>% 
    group_by(...) %>% 
    mutate(new_id = min({{id_ref}})) %>% 
    drop_na(...) %>% 
    ungroup() %>% 
    select({{id_ref}}, new_id) %>% 
    rename_with(~ paste0(col_name, "_new_id"), .cols = new_id)
  
  data <- data %>% 
    left_join(match)
}

refciting <- function(M){
  
  references_extract <- M |> rename('references'='CR') |>
  mutate(citing_id = paste0('A',1:n())) |> select(citing_id, references)  |>
  filter(!is.na(references)) |> separate_longer_delim(references, delim = "; ") |> 
  mutate(id_ref = 1:n())

  extract_authors <- ".*[:upper:][:alpha:]+( Jr(.)?)?, ([A-Z]\\.[ -]?)?([A-Z]\\.[ -]?)?([A-Z]\\.)?[A-Z]\\."
  extract_year_brackets <- "(?<=\\()\\d{4}(?=\\))"
  extract_pages <- "(?<= (p)?p\\. )([A-Z])?\\d+(-([A-Z])?\\d+)?"
  extract_volume_and_number <- "(?<=( |^)?)\\d+ \\(\\d+(-\\d+)?\\)"
 
  cleaning_references <- references_extract |>
  mutate(authors = stri_extract_first_regex(references, paste0(extract_authors, "(?=, )"))) |> 
  filter(!is.na(authors)) |> 
  mutate(
  remaining_ref = stri_replace_first_regex(references, paste0(extract_authors, ", "),""), # cleaning from authors
  is_article = !stri_detect_regex(remaining_ref, "^\\([:digit:]{4}"), 
  year = stri_extract_first_regex(references, extract_year_brackets ) |> as.integer())
  
  cleaning_articles <- cleaning_references |> filter(is_article == TRUE) |> 
    mutate(title = stri_extract_first_regex(remaining_ref, ".*(?=\\(\\d{4})"), # pre date extraction
           journal_to_clean = stri_extract_first_regex(remaining_ref, "(?<=\\d{4}\\)).*"), # post date extraction
           journal_to_clean = stri_replace_first_regex(journal_to_clean, "^,",'') %>% str_trim("both"), # cleaning a bit the journal info column
           pages = stri_extract_first_regex(journal_to_clean, extract_pages), # extracting pages
           volume_and_number = stri_extract_first_regex(journal_to_clean, extract_volume_and_number), # extracting standard volument and number: X (X)
           journal_to_clean = stri_replace_first_regex(journal_to_clean, " (p)?p\\. ([A-Z])?\\d+(-([A-Z])?\\d+)?",''), # clean from extracted pages
           journal_to_clean = stri_replace_first_regex(journal_to_clean, "( |^)?\\d+ \\(\\d+(-\\d+)?\\)",''), # clean from extracted volume and number
           volume_and_number = ifelse(is.na(volume_and_number), stri_extract_first_regex(journal_to_clean, "(?<= )([A-Z])?\\d+(-\\d+)?"), volume_and_number), # extract remaining numbers
           journal_to_clean = stri_replace_first_regex(journal_to_clean, " ([A-Z])?\\d+(-\\d+)?",''), # clean from remaining numbers
           journal = stri_replace_all_regex(journal_to_clean, "^[:punct:]+( )?[:punct:]+( )?|(?<=,( )?)[:punct:]+( )?([:punct:])?|[:punct:]( )?[:punct:]+( )?$",''), # extract journal info by removing inappropriate punctuations
           first_page = stri_extract_first_regex(pages, "\\d+"),
           volume = stri_extract_first_regex(volume_and_number, "\\d+"),
           issue = stri_extract_first_regex(volume_and_number, "(?<=\\()\\d+(?=\\))"),
           publisher = ifelse(is.na(first_page) & is.na(volume) & is.na(issue) & ! str_detect(journal, "(W|w)orking (P|p)?aper"), journal, NA),
           book_title = ifelse(str_detect(journal, " (E|e)d(s)?\\.| (E|e)dite(d|urs)? "), journal, NA), # Incollection article: Title of the book here
           book_title = stri_extract_first_regex(book_title, "[A-z ]+(?=,)"), # keeping only the title of the book
           publisher = ifelse(!is.na(book_title), NA, publisher), # if we have an incollection article, that's not a book, so no publisher
           journal = ifelse(!is.na(book_title) | ! is.na(publisher), NA, journal), # removing journal as what we have is a book
           publisher = ifelse(is.na(publisher) & str_detect(journal, "(W|w)orking (P|p)?aper"), journal, publisher), # adding working paper publisher information in publisher column
           journal = ifelse(str_detect(journal, "(W|w)orking (P|p)?aper"), "Working Paper", journal))
  
  cleaned_articles <- cleaning_articles %>% 
    select(citing_id, id_ref, authors, year, title, journal, volume, issue, pages, first_page, book_title, publisher, references)
  
  cleaning_non_articles <- cleaning_references %>% 
    filter(is_article == FALSE) %>% 
    mutate(remaining_ref = stri_replace_first_regex(remaining_ref, "\\(\\d{4}\\)(,)? ",''),
           title = stri_extract_first_regex(remaining_ref, ".*(?=, ,)"),
           pages = stri_extract_first_regex(remaining_ref, "(?<= (p)?p\\. )([A-Z])?\\d+(-([A-Z])?\\d+)?"), # extracting pages
           volume_and_number = stri_extract_first_regex(remaining_ref, "(?<=( |^)?)\\d+ \\(\\d+(-\\d+)?\\)"), # extracting standard volument and number: X (X)
           remaining_ref = stri_replace_first_regex(remaining_ref, " (p)?p\\. ([A-Z])?\\d+(-([A-Z])?\\d+)?",''), # clean from extracted pages
           remaining_ref = stri_replace_all_regex(remaining_ref, ".*, ,",''), # clean dates and already extracted titles
           remaining_ref = stri_replace_first_regex(remaining_ref, "( |^)?\\d+ \\(\\d+(-\\d+)?\\)",''), # clean from extracted volume and number
           volume_and_number = ifelse(is.na(volume_and_number), stri_extract_first_regex(remaining_ref, "(?<= )([A-Z])?\\d+(-\\d+)?"), volume_and_number), # extract remaining numbers
           remaining_ref = stri_replace_first_regex(remaining_ref, " ([A-Z])?\\d+(-\\d+)?",''), # clean from remaining numbers
           journal = ifelse(str_detect(remaining_ref, "(W|w)orking (P|p)aper"), "Working Paper", NA),
           journal = ifelse(str_detect(remaining_ref, "(M|m)anuscript"), "Manuscript", journal),
           journal = ifelse(str_detect(remaining_ref, "(M|m)imeo"), "Mimeo", journal),
           publisher = ifelse(is.na(journal), remaining_ref, NA) %>% str_trim("both"),
           first_page = stri_extract_first_regex(pages, "\\d+"),
           volume = stri_extract_first_regex(volume_and_number, "\\d+"),
           issue = stri_extract_first_regex(volume_and_number, "(?<=\\()\\d+(?=\\))"),
           book_title = NA) # to be symetric with "cleaned_articles"
  
  cleaned_non_articles <- cleaning_non_articles %>% 
    select(citing_id, id_ref, authors, year, title, journal, volume, issue, pages, first_page, book_title, publisher, references)
  
  cleaned_ref <- rbind(cleaned_articles, cleaned_non_articles)
  
  cleaned_ref <- cleaned_ref %>% 
    mutate(authors = stri_replace_first_regex(authors, " Jr\\.",''), # standardising authors name to favour matching later
           authors = stri_replace_first_regex(authors, "^\\(\\d{4}\\)(\\.)?( )?",''),
           authors = stri_replace_first_regex(authors, "^, ",''),
           authors = ifelse(is.na(authors), stri_extract_first_regex(references, ".*[:upper:]\\.(?= \\d{4})"), authors), # specific case
           journal = stri_replace_first_regex(journal, "[:punct:]$",''), # remove unnecessary punctuations at the end
           doi = stri_extract_first_regex(references, "(?<=DOI(:)? ).*|(?<=\\/doi\\.org\\/).*"),
           pii = stri_extract_first_regex(doi, "(?<=PII ).*"),
           doi = stri_replace_first_regex(doi, ",.*",''), # cleaning doi
           pii = stri_replace_first_regex(pii, ",.*",''), # cleaning pii
    )
  
  cleaned_ref <- cleaned_ref %>%
    mutate(first_author = stri_extract_first_regex(authors, "^[[:alpha:]+[']?[ -]?]+, ([A-Z]\\.[ -]?)?([A-Z]\\.[ -]?)?([A-Z]\\.)?[A-Z]\\.(?=(,|$))"),
           first_author_surname = stri_extract_first_regex(first_author, ".*(?=,)"),
           across(.cols = c("authors", "first_author", "journal", "title"), ~toupper(.))) 
  
  identifying_ref <- cleaned_ref |> 
    matching_ref(id_ref, first_author_surname, year, title, col_name = "fayt") |> 
    matching_ref(id_ref, journal, volume, issue, first_page, col_name = "jvip")  |>  
    matching_ref(id_ref, authors, year, volume, first_page, col_name = "ayvp") |> 
    matching_ref(id_ref, first_author_surname, year, volume, first_page, col_name = "fayvp") |>
    matching_ref(id_ref, title, year, first_page, col_name = "typ") |> 
    matching_ref(id_ref, pii, col_name = "pii") |> 
    matching_ref(id_ref, doi, col_name = "doi") 
  
  direct_citation <- identifying_ref %>%  
    mutate(new_id_ref = select(., ends_with("new_id")) |> reduce(pmin, na.rm = TRUE),
           new_id_ref = ifelse(is.na(new_id_ref), id_ref, new_id_ref)) |>  
    relocate(new_id_ref, .after = citing_id) |> 
    select(-id_ref & ! ends_with("new_id"))
  
  important_info <- c("authors", "year", "title", "journal", "volume", "issue",
                      "pages", "book_title", "publisher")
  
  references <- direct_citation %>%
    mutate(nb_na = rowSums(!is.na(select(., all_of(important_info))))) |> 
    group_by(new_id_ref) |> 
    slice_max(order_by = nb_na, n = 1, with_ties = FALSE) |>  
    select(-citing_id) |> unique()

    list(REFERENCIAS = direct_citation, REFERENCIASUNICOS = references)

}

ggconetwork <- function(ref,filterf = 5, top_n = 15,
                        top_n_per_com =2, weightf = 0.05){

  direct_citation <- ref$REFERENCIAS
  references <- ref$REFERENCIASUNICOS

  citations <- direct_citation |>
    add_count(new_id_ref) |>
    select(new_id_ref, n) |>
    unique()

  references_filtered <- references |> ungroup() |>
    left_join(citations) |>
    filter(n >= filterf)

  references_filtered <- references_filtered |> select(new_id_ref,!new_id_ref)

  for(i in 1:20){
    
    edges <- biblionetwork::biblio_cocitation(filter(direct_citation, new_id_ref %in% references_filtered$new_id_ref),
                                              "citing_id", "new_id_ref",weight_threshold = i)
    if(had_warning(tbl_main_component(nodes = references_filtered, edges = edges, directed = FALSE))){
      
      weight_threshold = i - 1
      
      break
      
    }
    
    if(weight_threshold < 1){
      
      weight_threshold = 1
      
    }
  }
  
  
  edges <- biblionetwork::biblio_cocitation(filter(direct_citation, new_id_ref %in% references_filtered$new_id_ref),
                                            "citing_id", "new_id_ref", weight_threshold = weight_threshold)

  graph <- tbl_main_component(nodes = references_filtered, edges = edges, directed = FALSE)

  graph <- add_clusters(graph,clustering_method = c("leiden"),
                        objective_function = c("modularity")) # identifying clusters of nodes

  nb_communities <- graph |>
    activate(nodes) |>
    as_tibble() |>
    select(cluster_leiden) |>
    unique() |>
    nrow()

  pal <- unique(c(brewer.pal(12,"Set3"),brewer.pal(8,"Set2"),brewer.pal(9,"Set1"),
           brewer.pal(12,"Paired")))
  set.seed(1)
  palette <- sample(pal,nb_communities, replace = F)

  graph <- community_colors(graph, palette, community_column = "cluster_leiden")

  graph <- graph |>
    activate(nodes) |>
    mutate(size = n,# will be used for size of nodes
           label = paste0(first_author_surname, "-", year))

  graph <- community_names(graph,
                           ordering_column = "size",
                           naming = "label",
                           community_column = "Com_ID")

  graph <- vite::complete_forceatlas2(graph,
                                      first.iter = 10000)

  top_nodes  <- top_nodes(graph,
                          ordering_column = "size",
                          top_n = top_n,
                          top_n_per_com = top_n_per_com,
                          biggest_community = F,
                          community_threshold = 0.02)

  community_labels <- community_labels(graph,
                                       community_name_column = "Community_name",
                                       community_size_column = "size",
                                       biggest_community = F,
                                       community_threshold = 0.02)

  graph <- graph |>
    activate(edges) |>
    filter(weight > weightf)

  ids <- as.list(graph)$nodes$Community_name |> unique() |>
    str_sub(start = 4)

  tis<- direct_citation |> filter(paste0(first_author_surname,"-",year)%in%ids) |>
    distinct(paste0(first_author_surname,"-",year), .keep_all = T) |>
    select(first_author_surname,year,title)

return(list(graph = graph,clusters = tis,
            top_nodes=top_nodes,community_labels = community_labels))

}

ggconetworkplot <- function(conet,filtro= 0){

  if(filtro == 0){
    conet$graph |>
    ggraph("manual", x = x, y = y) +
      geom_edge_arc0(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
      scale_edge_width_continuous(range = c(0.1,2)) +
      scale_edge_colour_identity() +
      geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.7, show.legend = FALSE) +
      scale_size_continuous(range = c(0.3,16)) +
      scale_fill_identity() +
      ggnewscale::new_scale("size") +
      #ggrepel::geom_text_repel(data = conet$top_nodes, aes(x=x, y=y, label = Label), size = 2, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
      ggrepel::geom_label_repel(data = conet$community_labels, aes(x=x, y=y, label = Community_name, fill = color), size = 2, fontface="bold", alpha = 0.9, point.padding=NA, show.legend = FALSE) +
      scale_size_continuous(range = c(0.5,5)) +
      theme_void()+
      theme(panel.background  = element_rect(colour = "white",fill = "white"))

  }else{
  filtroc = conet$clusters
  id <- as.list(conet$graph)$node$color[as.list(conet$graph)$node$year == filtroc$year[filtro] &
  as.list(conet$graph)$node$first_author_surname == filtroc$first_author_surname[filtro]]

  conet$graph |>
    activate(nodes) |>
    filter(color %in% id) |>
    ggraph("manual", x = x, y = y) +
    geom_edge_arc0(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
    scale_edge_width_continuous(range = c(0.1,2)) +
    scale_edge_colour_identity() +
    geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.7, show.legend = FALSE) +
    scale_size_continuous(range = c(0.3,16)) +
    scale_fill_identity() +
    ggnewscale::new_scale("size") +
   # ggrepel::geom_text_repel(data = conet$top_nodes|> filter(color == id), aes(x=x, y=y, label = Label), size = 2, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
    ggrepel::geom_label_repel(data = conet$community_labels |> filter(color == id), aes(x=x, y=y, label = Community_name, fill = color), size = 2, fontface="bold", alpha = 0.9, point.padding=NA, show.legend = FALSE) +
    scale_size_continuous(range = c(0.5,5)) +
    theme_void()+
    theme(plot.background  = element_rect(colour = "white",fill = "white"))

  }
}

ggconetworkword <- function(ref,filterf = 5, weight_threshold = 3,top_n = 15,
                        top_n_per_com =2, weightf = 0.05){

  direct_citation <- ref$REFERENCIAS
  references <- ref$REFERENCIASUNICOS

  citations <- direct_citation |>
    add_count(new_id_ref) |>
    select(new_id_ref, n) |>
    unique()

  references_filtered <- references |> ungroup() |>
    left_join(citations) |>
    filter(n >= filterf)

  references_filtered<- references_filtered |> select(new_id_ref,!new_id_ref)

  edges <- biblionetwork::biblio_cocitation(filter(direct_citation, new_id_ref %in% references_filtered$new_id_ref),
                                            "citing_id", "new_id_ref", weight_threshold = weight_threshold)

  graph <- tbl_main_component(nodes = references_filtered, edges = edges, directed = FALSE)

  graph <- add_clusters(graph,clustering_method = c("leiden"),
                        objective_function = c("modularity")) # identifying clusters of nodes

  nb_communities <- graph |>
    activate(nodes) |>
    as_tibble() |>
    select(cluster_leiden) |>
    unique() |>
    nrow()

  pal <- c(brewer.pal(12,"Set3"),brewer.pal(8,"Set2"))
  set.seed(1)
  palette <- sample(pal,nb_communities, replace = F)

  graph <- community_colors(graph, palette, community_column = "cluster_leiden")

  graph <- graph |>
    activate(nodes) |>
    mutate(size = n,# will be used for size of nodes
           label = ID)

  graph <- community_names(graph,
                           ordering_column = "size",
                           naming = "label",
                           community_column = "Com_ID")

  graph <- vite::complete_forceatlas2(graph,
                                      first.iter = 10000)

  top_nodes  <- top_nodes(graph,
                          ordering_column = "size",
                          top_n = top_n,
                          top_n_per_com = top_n_per_com,
                          biggest_community = F,
                          community_threshold = 0.02)

  community_labels <- community_labels(graph,
                                       community_name_column = "Community_name",
                                       community_size_column = "size",
                                       biggest_community = F,
                                       community_threshold = 0.02)

  graph <- graph |>
    activate(edges) |>
    filter(weight > weightf)

  ggraph(graph, "manual", x = x, y = y) +
    geom_edge_arc0(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
    scale_edge_width_continuous(range = c(0.1,2)) +
    scale_edge_colour_identity() +
    geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.7, show.legend = FALSE) +
    scale_size_continuous(range = c(0.3,16)) +
    scale_fill_identity() +
    ggnewscale::new_scale("size") +
    ggrepel::geom_text_repel(data = top_nodes, aes(x=x, y=y, label = Label), size = 2, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
    ggrepel::geom_label_repel(data = community_labels, aes(x=x, y=y, label = Community_name, fill = color), size = 2, fontface="bold", alpha = 0.9, point.padding=NA, show.legend = FALSE) +
    scale_size_continuous(range = c(0.5,5)) +
    theme_void()+
    theme(panel.background  = element_rect(colour = "white",fill = "white"))

}

# Função para verificar se um aviso foi gerado
had_warning <- function(expr) {
  # Inicializa uma lista para armazenar avisos
  warnings_list <- list()
  
  # Executa a expressão e captura avisos
  tryCatch(
    {
      withCallingHandlers(
        expr,
        warning = function(w) {
          warnings_list <<- append(warnings_list, list(w))
          invokeRestart("muffleWarning")  # Suprime o aviso para não interromper a execução
        }
      )
    },
    warning = function(w) {
      warnings_list <<- append(warnings_list, list(w))
      invokeRestart("muffleWarning")
    }
  )
  
  # Retorna TRUE se houver avisos, FALSE caso contrário
  length(warnings_list) > 0
}


