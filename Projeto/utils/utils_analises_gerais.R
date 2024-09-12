### 

# --------------------------- PACOTES NECESSARIOS -----------------------------#

pacotes <- c("tidyverse", "bib2df", "janitor", "rscopus", "biblionetwork","RColorBrewer",
"tidygraph", "ggraph", "ggnewscale",'stringi',"data.table",'openxlsx','ggwordcloud',
 "bibliometrix", "ggpubr","broom","viridis","treemapify","ggrepel",'igraph',
'ggh4x','stringdist','maps')

githubpacotes <- c("thomasp85/scico","agoutsmedt/networkflow","ParkerICI/vite",
                   'hrbrmstr/pluralize')

for(i in c(pacotes,gsub(".*/", "",githubpacotes))){
  library(i, character.only = TRUE)
}

rm(pacotes,i,githubpacotes)

# -----------------------------------------------------------------------------#
# 
#### PALETA

paleta <- c('#DB5027ff','#BC563Bff','#9C5E4Fff','#7C6464ff','#5A6A77ff','#3A7387ff',
            '#1C789Fff','#187DA1ff','#1A8899ff','#229294ff','#2C9A91ff','#35A28Dff',
            '#3EAC87ff','#4CB282ff','#68B37Aff','#85B372ff','#A2B26Bff','#C1B065ff',
            '#D3B168ff','#FBB256ff')

#### TIRANDO A LOGO

logo_dir <- paste0(find.package("bibliometrix"),'/data/Rdata.rds')
logo_data <- readRDS(logo_dir)
logo_data <- Filter(function(x){x!='logo'},logo_data)
saveRDS(logo_data,logo_dir)
rm(logo_data,logo_dir)

logo <- NA

#### MAIN FINDINGS

mainfindings <- function(x){
  
  wb <- createWorkbook()
  
  for(i in 2:length(names(x))) {
    
    addWorksheet(wb, names(x)[i])
    writeData(wb, sheet = names(x)[i], x[[names(x)[i]]])
    
    saveWorkbook(wb, "Relatorios/Tabelas/Main_Findings.xlsx", overwrite = TRUE)
    
  }
  
}

####

theme_f <- function(expandy = waiver(),expandx= waiver(),namex= waiver(),
namey = waiver(),sizep = 0,breaksx = waiver(),labely= waiver(),
breaksy= waiver(), limitx = NULL){
  
list(theme_bw(),theme(axis.text = element_text(size = 12),
                      panel.grid.minor.y = element_blank(),
legend.position = "top", legend.title = element_blank(),
axis.title = element_text(size = 12),legend.text = element_text(size = 8),
legend.margin = margin(-5,0,-10,0)),
scale_x_discrete(expand = expandx,name = namex,breaks = breaksx,
                 limits = limitx),
scale_y_continuous(expand = expandy, n.breaks = 7,name = namey,
labels = labely, breaks = breaksy),
geom_point(size = sizep))

}

count.duplicates <- function(DF){
  x <- do.call('paste', c(DF, sep = '\r'))
  ox <- order(x)
  rl <- rle(x[ox])
  cbind(DF[ox[cumsum(rl$lengths)],,drop=FALSE],count = rl$lengths)

}
