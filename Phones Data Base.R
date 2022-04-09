##CREACI?N DE LA BASE DE DATOS DE LOS M?VILES SAMSUNG##

## Limpiar la consola ##
rm(list = ls())


#cargar paquetes necesarios #
suppressPackageStartupMessages({
  library(rvest)
  library(dplyr)
  library(magrittr)
  library(rlist)
  library(stringr)
  library(robotstxt)
  library(selectr)
  library(xml2)
  library(forcats)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  library(lubridate)
  library(tibble)
  library(tm)
})


##Obtenci?n de p?ginas ra?ces para cada una de las marcas##
Url_root <- "https://www.gsmarena.com/search.php3?"
paths_allowed(paths = Url_root) #Permisos#

page_root <- read_html(Url_root)
object_root <- "a"   #clase de las p?ginas por selector gadget#

###Creaci?n de un vector con cada p?gina web de cada modelo de m?vil para su
#posterior lectura  ###

Brands_code <-  page_root                            %>%
           html_nodes(object_root)              %>%
           html_attrs()                         %>%
           as.character()                       %>%
           str_remove_all(".php")               %>%
           .[22:57]  %>%      #Vector con p?ginas ra?ces de cada marca#
           str_replace("(\\d*)$","f-\\1")            
  
#Vector con n?mero de p?ginas y con la direcci?n de las mismas #
v <- c(sort(rep(1:16,36)))
url <- c(paste0("https://www.gsmarena.com/", Brands_code,"-0-r1-p",v,".php"))


#Verificaci?n de permisos de wrapping##
if (mean(paths_allowed(paths = c(url)))==1) {print("TRUE")} else {print("FALSE")} 

     ##Extracci?n de datos en bruto##

parsed_pages <- replicate(list(), n = length(url))
for (k in seq_along(url)) parsed_pages[[k]] <- try(read_html(url[k]),
                                                   silent = TRUE)

#P?ginas fallidas y con informaci?n importante##
fail <- warnings() 
unextracted_url <- fail              %>%
                 names()             %>%
           as.character()            %>%
       str_replace( "^.+\\((.+)\\)", "\\1")

unextracted_pages <- replicate(list(), n = length(unextracted_url))
for (i in seq_along(unextracted_url)) unextracted_pages[[i]] <- try(read_html(unextracted_url[i]),
                                                   silent = TRUE)             

parsed_pages <- append(parsed_pages, unextracted_pages)

parsed_pages <- list.clean(parsed_pages,function (parsed_pages) 
  length(parsed_pages) == "1")

basic <- function(x) {
  fil <- str_extract(xml_text(x, title),"^.+\\r") %>%
    str_replace("\\r", "")
  write_html(x, file = fil)
}
map(parsed_pages, function(x) basic(x))

ccs_movil <- "span"                   ##Clase del objeto texto##
ccs_brand <- ".article-info-name"

Brands <- parsed_pages %>%
          lapply(html_nodes, ccs_brand) %>%
          lapply(html_text)            %>%
          unlist()                    %>%
          str_replace("^(.+) phones$","\\1" )
  
Phones <-parsed_pages %>% 
  lapply(html_nodes, ccs_movil) %>%
  lapply(html_text)             
  
names(Phones) <- Brands

#Limpiar datos #

garbage_words <- Phones %>%  #Para eliminar palabras que no son m?viles#
                 unlist() %>%
                 as.data.frame()  #19767 es la cantidad de caracteres en Phones#
                 
colnames(garbage_words) <- "Obs"                  #compatibilidad#
garbage_words[,2] <- c(1:nrow(garbage_words))        

garbage_words <- garbage_words %>%    #Para eliminar palabras que no son m?viles#
                 group_by(Obs) %>%
                 count()       %>%
                 as.data.frame() %>%
                 filter(n >10)     %>%
                 .$Obs            %>%
                 as.character()

Phones <- unlist(Phones)                
Phones <- Phones[!Phones %in% garbage_words]
## A?ADIR PROX VEZ names(Phones) <- str_replace(names(Phones), "([\\D]+)\\d", "\\1") ##

saveRDS(Phones, file = "Phones")


