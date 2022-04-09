## Limpiar la consola ##
rm(list = ls())

## Cargar paquetes ##
suppressPackageStartupMessages({

  library(jsonlite)
  library(dplyr)
  library(lubridate)
  library(rlist)
  library(stringr)
  library(readr)
  library(purrr)
  library(tidytext)
  library(tidyr)
  library(tokenizers)
  library(tm)
  library(tidyverse)
  library(viridis)
  library(havanas)
})


                       ##Ruta de trabajo##


setwd("~/swirl_temp/Havana´s Forros")


        # Obtener datos de los chats  y de la base de datos de m?viles #


chats <- fromJSON("2022-02-06.json")

phones <- readRDS("Phones")
names(phones) <- str_replace(names(phones), "([\\D]+)\\d*", "\\1") %>%
                          tolower()     #Arreglar las marcas#

brnd <- c(names(phones),"iphone)") %>% .[!duplicated(.)] # todas las marcas #

cuba <- c("samsung","apple","huawei", "sony", "lg", "motorola", "xiaomi",
                "google", "honor","oneplus", "alcatel","zte", "blu", "iphone",
                 "realme", "oppo") # marcas de cuba #


#Seleccionar ?nicamente las marcas existentes en Cuba y excluir otras
                        #para evitar ruido#

phones <- phones[names(phones) %in% cuba]            %>%
          str_replace("(.)", paste(names(.),"\\1"))   %>%
          tolower()              %>%  #Unir la marca con los modelos de m?vil#
          str_subset("pop|watch|tab|ipad", negate = TRUE) %>%
          removeWords( c("xiaomi redmi note$"))


#Incluir modelos con nombres diferentes#

phones <- c(phones, "samsung galaxy note 10 plus", "xiaomi note 5a",
                    "samsung galaxy note 10", "samsung galaxy j7 neo",
                    "samsung galaxy note 10 lite", "xiaomi redmi note 5",
                    "samsung galaxy note 10 5g", "xiaomi redmi 6 pro",
                    "samsung galaxy note 10 plus 5g") %>%
                     str_replace_all( "(\\s*\\+)", " plus")
                                                # ^ últimos detalles#




                   ## Limpieza de datos ##


chats <- as_tibble(chats$chats$list)
names(chats$messages)   <- chats$name

#chats con personas que no son clientes#

chats <- chats %>%
              filter(id != 777000 & name != "REI" & id != 780423313 &
                     id != 864946788 & id != 2139306291  & id !=868398716
                      & id != 2033203298 )

ind   <-  map_lgl(chats$messages, function(x) x[1,6]== "clear_history")

chats <-  chats[!ind,]


#Eliminar mensajes

ind <- which(map_lgl(chats$messages, function(x) x[1,"from_id"] != "user1341294918"))

chats <- chats[ind,]  #elimina directamente cualquier chat donde el vendedor haya sido el primero en escribir
                       ## (no relacionado con venta de forros) ##



              #Operaciones sobre cada DF de cada cliente ##


chats <- chats$messages %>%
         map(filter, from_id != "user1341294918") %>%              #eliminaci?n de todos los mensajes del vendedor#
         map(select,c("from", "date", "text", "from_id")) %>%     #seleccionar la informaci?n sobre la que vamos a trabajar#
         map(mutate, "longitud" = map(text, length))  %>%
         map(filter, longitud == 1)  %>%                          #eliminaci?n de listas dentro de la lista de mensajes##
         transpose() %>%                                         #pasamos los mensajes a una tabla nueva que los contiene a todos#
         as_tibble() %>%
         select(-longitud)

chats$text <- map(chats$text, unlist, use.names = FALSE)

chats <- unnest(chats, cols = c(from, date, text, from_id))  #desenlistamos los mensajes que se encuentran en vectores y le otorgamos una fila a cada uno#

#Funci?n para homogeneizar el texto#

chats$text <- map(chats$text, clean)

chats <- filter(chats, nchar(chats$text) > 1)


#funci?n primitiva para eliminar palabras no existentes en nuestro database#


chats$text <- chats$text %>%
            map(function(x) detector(x, phones))

chats <- filter(chats, text != "NA")


#Eliminaci?n esta vez de grupos de palabras que entorpecen el an?lisis, las a?adimos a un data base primero##

fphone <- chats %>%
              select(text) %>%
              map(function(x) tokenize_ngrams(x, n_min = 2,
                                              n = 10, simplify = TRUE)) %>%
              as_tibble() %>%
              unnest(cols = c(text)) %>%
              group_by(text) %>%
              count(text, sort = TRUE) %>%
              mutate(usef = text) %>%
              filter(usef != "NA")

fphone$usef <- fphone$usef %>%
               map(function(x) str_replace_all(x, "\\b(\\w+)\\b",
                                                  "\\\\b\\1\\\\b.*")) %>%
               map(function(x) str_replace_all(x, "\\s", "")) %>%
               map_lgl(function(x) any(str_detect(phones,x)))

fphone <- fphone[1:30, c(1,3)]
fphone <- fphone$text[!fphone$usef]

chats$text <- map(chats$text, function(x) removeWords(x, fphone))  %>%
              map( function(x) str_trim(x))

chats <-   filter(chats, nchar(text) > 1)



fphone <- chats                                %>%
           group_by(text)                      %>%
           count(text, sort = TRUE)            %>%
           .[1:15,1]                           %>%
            filter(count_words(text) == 1)     %>%
           mutate(d= map(text, function(x)
                        str_detect(x, "\\d"))) %>%
           filter(d != TRUE & nchar(text) < 6) %>%
           select(text)                        %>%
           simplify_all()                      %>%
           unlist()

chats <- filter(chats, !text %in% fphone)


#funci?n que se encuentra dentro de la funci?n detect2, sirve para extraer el m?vil m?s probable de un ngram#

brand <- c("apple", "samsung", "xiaomi")
subm <- c("samsung galaxy", "redmi.+", "\\bp[0-9]{1,2}\\b","iphone")
mod <- c("\\bs[0-9]{1,2}\\b","\\bxr\\b", "\\ba[1-9]{2}",
         "xs max", "\\bj[0-9]{1,2}\\b" )
nobrand <- c("poco", "amigo", "honor note", "premium", "[0-9]{4,}")
neutral <- c("motorola", "edge", "\\bmi\\b","zte",
             "\\bblu\\b", "lg", "black")




## función para ver el accuracy de detect2 ##

chats <- chats %>%
            mutate(model = map(text,  detect2),
            prob = map(text, probs))

 chats$date <- map(chats$date, ymd_hms)

chats <- chats              %>%
            unnest(c(model, prob))  %>%
            filter(prob > 30)    %>%
            group_by(from_id)     %>%
            group_split()


chats <- map_df(chats, phone_by_user)

modelos <- readRDS(file = "~/swirl_temp/Havana´s Forros/Modelos Procesados/modelos.rds")

modelos$model <- modelos$model %>%
                 map_chr(function(x) x[[1]])

modelos$prob <- modelos$prob %>%
                 map_dbl(function(x) x[[1]])

if (identical(chats, modelos) == TRUE) {
  chats = chats
} else {
  chats = union(chats, modelos)
  }

chats  <- chats            %>%
      group_by(from_id) %>%
      group_split()     %>%
      map_df(function(x) distinct(x, model, .keep_all = T)) %>%
      filter(prob > 30)



saveRDS(chats, file = "~/swirl_temp/Havana´s Forros/Modelos Procesados/modelos.rds")



##Análisis de Encargos ##

encargos <- fromJSON("Encargos/encargos-11-21.json")
encargos <- encargos[[4]][,c(1,13,3,8,14)] %>% filter(text != "")

encargos$text <- encargos$text %>% map(unlist) %>%
                   map(clean) %>%
                   map(function (x) str_remove_all(x, "💰|☑️|✅|⤵️|📮"))

encargos$from <- map_chr(encargos$text, function(x) x[[1]][1])       %>%
                 map_chr(str_replace, "^(\\S+\\s\\S+)\\s.*$", "\\1") %>%
                 map_chr(str_trim) %>% str_remove("plus")

encargos <- unnest(encargos, text)

encargos$text <- map_chr(encargos$text, detector)

na.index <- which(is.na(encargos$text)== FALSE)
encargos <- encargos[na.index,]


encargos <- encargos %>%
             mutate(cw=count_words(text)) %>%
            filter(cw>1)  %>% select(-cw)


encargos <- encargos %>%
  mutate(model=  map(text, function(x) encargosPD(x, n= 5)),
         prob = map(text, function(x) encargos_probs(x, n= 5)))

encargos$date <- map(encargos$date, ymd_hms)

encargos <- unnest(encargos, cols = c("model", "prob"))  %>%
            group_by(id)   %>%
             group_split()


encargos <- map_df(encargos, function(x) filter_tiny(x))
na.index <- which(is.na(encargos$text)== FALSE)
encargos <- encargos[na.index,] %>% select(-id) %>%
            filter(prob > 0.31)


modelos <- readRDS(file = "~/swirl_temp/Havana´s Forros/Modelos Procesados/encargos.rds")

if (identical(encargos, modelos) == TRUE) {
  encargos = encargos
} else {
  encargos = union(encargos, modelos)
}

saveRDS(encargos, file = "~/swirl_temp/Havana´s Forros/Modelos Procesados/encargos.rds")



##Inventario##

stockcuba <- fromJSON("Stock Cuba/stock.json") %>%
                .[[4]] %>% .[,c(10,3,8,11)] %>%
                filter(text != "")

stockcuba$text <- stockcuba$text %>% map(unlist)
stockcuba <- unnest(stockcuba, text)

stockcuba$text <- map_chr(stockcuba$text, detector)

na.index <- which(is.na(stockcuba$text)== FALSE)
stockcuba <- stockcuba[na.index,]

stockcuba <- stockcuba %>%
  mutate(cw=count_words(text)) %>%
  filter(cw>1)  %>% select(-cw)

stockcuba <- stockcuba %>%
  mutate(model=  map_chr(text, function(x) encargosPD(x)),
         prob = map(text, function(x) encargos_probs(x))) %>%
         filter(date != "2021-04-26T23:30:45" & prob > 30)

stockcuba$date <- map(stockcuba$date, ymd_hms)


##RESULTADOS##
chats$text <- as.character(chats$text)

now <- as.numeric(now())/(60*60*24*7)

interest <- union(chats,encargos) %>%
  group_by(model) %>%
  group_split()   %>%
  map(function(x) mutate(x, dif = now-as.numeric(date)/(60*60*24*7))) %>%
  map(function(x) mutate(x, pond = ifelse(dif < 4, 1,
                                   ifelse(dif < 8, 0.95,
                                   ifelse(dif < 12, 0.85,
                                   ifelse(dif < 16, 0.75,
                                   ifelse(dif < 26, 0.70,
                                   ifelse(dif < 52, 0.65,0.6)))))))) %>%
  map_df(function(x) c(x[1,5], n = sum(x[,8]))) %>%
  select(c(model, n))


stock <- stockcuba %>%
  group_by(model) %>%
  group_split() %>%
  map(function(x) mutate(x, dif = now-as.numeric(date)/(60*60*24*7))) %>%
  map(function(x) mutate(x, pond = ifelse(dif < 1.5, 0.5,
                                          ifelse(dif < 2.5, 0.75,
                                          ifelse(dif < 4,   0.88, 1))))) %>%
  map_df(function(x) c(x[1,5], n = -sum(x[,8]))) %>%
  select(c(model,n))


interest <- union(interest, stock) %>%
           group_by(model)       %>%
           summarise(n = sum(n)) %>%
           arrange(desc(n)) %>%
           mutate(brand = str_extract(model, "^\\s?\\b[a-z]+\\b"))

popular <- filter(interest, n > 3)

ggplot(popular, aes(y = reorder(model,n),x = n, fill= n))+
  geom_bar(stat = "identity" ) +
  theme(axis.text.x = element_text( hjust = 1)) +
  labs(x= "Interés general", y= "Modelo de teléfono" ) +
  scale_color_viridis_c(option = "A", aesthetics = c("colour", "fill"))




