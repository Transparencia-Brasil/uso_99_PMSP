#Aqui eu estou consultando as distâncias das corridas que não têm odometro registrado.

library(ggmap)  
library(readxl)
library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)
library(dplyr)
library(deflateBR)
library(stringi)
library(ggmap)
library(stringi)

setwd("C:/Users/coliv/Documents/Taxis")

dados_app <- read_excel("Dados_Aplicativo_Transporte.xlsx")

#ID é um valor único, não existem duas corridas com o mesmo ID

analise_taxis <- dados_app %>%
  clean_names() %>%
  mutate(odometro_km = gsub(",", ".", odometro_km),
         odometro_km = as.numeric(odometro_km),
         id_da_corrida = as.character(id_da_corrida),
         cidade_origem = gsub("Sao", "São", cidade_origem),
         origem = endereco_de_origem,
         origem = gsub("Sao", "São", origem),
         origem = ifelse(!grepl("São Paulo - SP", origem) & cidade_origem == "São Paulo", 
                         paste0(origem,", São Paulo - SP, Brasil"), origem),
         origem = ifelse(id_da_corrida == "87961660234245",
                         "Lago Sul, Brasília - DF, 71608-900", origem),
         origem = ifelse(id_da_corrida == "87961673470138", 
                         "s/n, SPO - Asa Sul, DF, 70610-900", origem),
         origem = ifelse(id_da_corrida == "151151398", paste0(endereco_de_origem, ", São Paulo - SP, Brasil"), origem),
         destino = endereco_de_destino,
         destino = gsub("Sao Paulo", "São Paulo", destino),
         destino = ifelse(!grepl("São Paulo - SP", destino) & cidade_origem == "São Paulo", 
                          paste0(destino,", São Paulo - SP, Brasil"), destino),
         omitido = ifelse(grepl("omitido", origem), 1, 0),
         omitido = ifelse(grepl("omitido", destino), 1, omitido),
         hora_final = as.POSIXct(strptime(hora_final, "%Y-%m-%d %H:%M:%S")),
         hora_origem =  as.POSIXct(strptime(hora_origem, "%Y-%m-%d %H:%M:%S")),
         tempo_viagem = as.numeric(hora_final - hora_origem),
         tempo_viagem_min = round(tempo_viagem/60, 0),
         tempo_viagem_hora = round(tempo_viagem_min/60, 2),
         data_origem = as.Date(data_origem, format="%Y-%m-%d"),
         tarifa_deflate = round(deflate(tarifa, data_origem, "03/2019", index = "ipca" ),2)) #demora um pouco



key <- ""
register_google(key = key)

df <- data.frame(from = NA,
                 to = NA,
                 m = NA,
                 km = NA,
                 miles = NA,
                 seconds = NA,
                 minutes = NA,
                 hours= NA,
                 mode = NA)

#primeiro objeto para verificar quais são as corridas sem distância. 
sem_dist <- analise_taxis %>%
  filter(odometro_km == 0,
         tarifa > 1) %>%
  select(id_da_corrida, tarifa_deflate, cidade_origem, endereco_de_origem, endereco_de_destino)
  
#Loop para consulta das distâncias na API do Google Maps
#Obs: várias linhas dão errado, porque a API não consegue encontrar o endereço. Eu tive que ficar 
#     supervisionando porque esse loop não prevê erro.

for(i in 1:3916){
  print(i)
  
  from <- as.character(sem_dist[i,4])
  to <- as.character(sem_dist[i,5])
  
  x <- mapdist(from, to, "driving", "simple")
  
  df <- df %>% 
    bind_rows(x)
  
  Sys.sleep(0.5)
}

#salvando objeto final da consulta
save(df, file="consulta_odometro_zero_taxis.Rdata")

#Fazendo um DF mais limpo, sem duplicatas
df2 <- df %>%
  select(1:9) %>%
  distinct(from, to, m, km, miles, seconds, minutes, hours, mode) %>%
  select(1,2,4)


# Eu cometi um erro: deveria ter imprimido o id da corrida no DF final da consulta. Como já tinha sido feito,
# Optei por cruzar com exatamente os nomes dos endereços de origem e destino.
# esse aqui é o objeto final que serve para substituir os ids sem distância

sem_dist_2 <- sem_dist %>%
  left_join(df2, by=c("endereco_de_origem" = "from",
                      "endereco_de_destino" = "to")) %>%
  distinct(id_da_corrida, tarifa_deflate, .keep_all = TRUE) %>%
  #consultei alguns endereços na mão porque a API entregou amis de 100km:
  mutate(km = ifelse(km > 105.566, 0, km)) %>%
  select(id_da_corrida, km)

analise_taxis_final <- analise_taxis %>%
  left_join(sem_dist_2, by=c("id_da_corrida")) %>%
  mutate(odometro_km = ifelse(!is.na(km), km, odometro_km),
         odometro_km = ifelse(!is.na(km) & tempo_viagem_min < 3, 0, odometro_km)) #corridas que foram canceladas

setwd("C:/Users/coliv/Documents/Taxis")
save(analise_taxis_final, file="analise_taxis_final.Rdata")

analise_taxis_final2 <- analise_taxis_final %>%
  mutate(endereco_de_origem_sa = gsub("\\?", "", endereco_de_origem),
         endereco_de_destino_sa = gsub("\\?", "", endereco_de_destino),
         endereco_de_origem_sa = tolower(stri_trans_general(endereco_de_origem_sa, "Latin-ASCII")),
         endereco_de_destino_sa = tolower(stri_trans_general(endereco_de_destino_sa, "Latin-ASCII")))

analise_taxis_final <- analise_taxis_final2


analise_taxis_final2 <- analise_taxis_final  %>%
  mutate(hora_origem = stri_sub(hora_origem,11),
         hora_origem = paste0(data_origem, hora_origem),
         hora_final = stri_sub(hora_final,11),
         hora_final = paste0(data_final, hora_final),
         hora_final = as.POSIXct(strptime(hora_final, "%Y-%m-%d %H:%M:%S")),
         hora_origem =  as.POSIXct(strptime(hora_origem, "%Y-%m-%d %H:%M:%S")),
         tempo_viagem = as.numeric(hora_final - hora_origem),
         tempo_viagem = ifelse(tempo_viagem < 0 , tempo_viagem*-1, tempo_viagem),
         tempo_viagem_min = round(tempo_viagem/60, 0),
         tempo_viagem_hora = round(tempo_viagem_min/60, 2),
         data_origem = as.Date(data_origem, format="%Y-%m-%d"))

analise_taxis_final <- analise_taxis_final2

setwd("C:/Users/coliv/Documents/Taxis")
save(analise_taxis_final, file="analise_taxis_final.Rdata")

analise_taxis_final <- analise_taxis_final %>%
  mutate(categoria = gsub("Regular Taxi", "Taxi comum", categoria)) %>%
  select(-c(cancelada))

setwd("C:/Users/coliv/Documents/Taxis")
save(analise_taxis_final, file="analise_taxis_final.Rdata")

x <-  analise_taxis_final %>%
  mutate(endereco_de_origem_sa = gsub("r.", "rua", endereco_de_origem_sa),
         endereco_de_origem_sa = gsub("av.", "avenida", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("viaduto do cha, 15", endereco_de_origem_sa), "viaduto do cha, 15", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("upa vila santa catarina - rua cidade de bagda", endereco_de_origem_sa), "upa vila santa catarina - rua cidade de bagda", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("serbom armazens gerais frigorificos", endereco_de_origem_sa), "serbom armazens gerais frigorificos ltda, avenida das nações unidas", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua xavier de almeida, 210", endereco_de_origem_sa), "rua xavier de almeida, 210", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua venceslau bras 50", endereco_de_origem_sa), "rua venceslau bras 50", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua vanderlei, 1371", endereco_de_origem_sa), "rua vanderlei, 1371", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua turiassu, 130", endereco_de_origem_sa), "rua turiassu, 130", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua tuiuti, 515", endereco_de_origem_sa), "rua tuiuti, 515", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua siria, 42", endereco_de_origem_sa), "rua siria, 42", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua silva bueno, 821", endereco_de_origem_sa), "rua silva bueno, 821", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua santo amaro, 439", endereco_de_origem_sa), "rua santo amaro, 439", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua santo amaro, 216", endereco_de_origem_sa), "rua santo amaro, 216", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua santa isabel, 181", endereco_de_origem_sa), "rua santa isabel, 181", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua santa eulalia, 86", endereco_de_origem_sa), "rua santa eulalia, 86", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua rosa de morais, 91", endereco_de_origem_sa), "rua rosa de morais, 91", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua pedro de toledo, 983", endereco_de_origem_sa), "rua pedro de toledo, 983", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua pedro de toledo, 1082", endereco_de_origem_sa), "rua pedro de toledo, 1082", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua pedro de toledo, 1071", endereco_de_origem_sa), "rua pedro de toledo, 1071", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua pedro avelino, 22", endereco_de_origem_sa), "rua pedro avelino, 22", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua paulistania, 520", endereco_de_origem_sa), "rua paulistania, 520", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua paranacity, 122", endereco_de_origem_sa), "rua paranacity, 122", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua paranacity, 117", endereco_de_origem_sa), "rua paranacity, 117", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua paineira do campo, 902", endereco_de_origem_sa), "rua paineira do campo, 902", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua padre marchetti, 557", endereco_de_origem_sa), "rua padre marchetti, 557", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua ouvidor portugal, 230", endereco_de_origem_sa), "rua ouvidor portugal, 230", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua olivia guedes penteado, 267", endereco_de_origem_sa), "rua olivia guedes penteado, 267", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua oliveira catrambi 952", endereco_de_origem_sa), "rua oliveira catrambi 952", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua oliveira catrambi 956", endereco_de_origem_sa), "rua oliveira catrambi 956", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua nossa senhora das dores, 350", endereco_de_origem_sa), "rua nossa senhora das dores, 350", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua nazir miguel 849", endereco_de_origem_sa), "rua nazir miguel 849", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua mourato coelho, 799", endereco_de_origem_sa), "rua mourato coelho, 799", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua mauro bonafe pauletti, 199", endereco_de_origem_sa), "rua mauro bonafe pauletti, 199", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua monte alegre 45", endereco_de_origem_sa), "rua monte alegre 45", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua mauro bonafe pauletti, 160", endereco_de_origem_sa), "rua mauro bonafe pauletti, 160", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua mauricio araujo martins, 342", endereco_de_origem_sa), "rua mauricio araujo martins, 342", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua marina crespi, 91", endereco_de_origem_sa), "rua marina crespi, 91", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua maria fett, 467", endereco_de_origem_sa), "rua maria fett, 467", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua maria amalia lopes azevedo, 3650", endereco_de_origem_sa), "rua maria amalia lopes azevedo, 3650", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua manuel moscoso, 15", endereco_de_origem_sa), "rua manuel moscoso, 15", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua manuel correia, 238", endereco_de_origem_sa), "rua manuel correia, 238", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua machado bitencourt, 220", endereco_de_origem_sa), "rua machado bitencourt, 220", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua luis carneiro, 193", endereco_de_origem_sa), "rua luis carneiro, 193", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua lucio de arruda leme, 211", endereco_de_origem_sa), "rua lucio de arruda leme, 211", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua lucas de leyde, 157", endereco_de_origem_sa), "rua lucas de leyde, 157", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua loefgren, 2109", endereco_de_origem_sa), "rua loefgren, 2109", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua loefgren, 1742", endereco_de_origem_sa), "rua loefgren, 1742", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua lino coutinho, 841", endereco_de_origem_sa), "rua lino coutinho, 841", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua lino coutinho, 444", endereco_de_origem_sa), "rua lino coutinho, 444", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua libero badaro, 425", endereco_de_origem_sa), "rua libero badaro, 425", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua libero badaro, 293", endereco_de_origem_sa), "rua libero badaro, 293", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua libero badaro, 190", endereco_de_origem_sa), "rua libero badaro, 190", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua libero badaro, 158", endereco_de_origem_sa), "rua libero badaro, 158", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua libero badaro, 119", endereco_de_origem_sa), "rua libero badaro, 119", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua leo ribeiro de moraes, 66", endereco_de_origem_sa), "rua leo ribeiro de moraes, 66", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua leandro dupret, 525", endereco_de_origem_sa), "rua leandro dupret, 525", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua juventus, 562", endereco_de_origem_sa), "rua juventus, 562", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua julio felipe guedes, 200", endereco_de_origem_sa), "rua julio felipe guedes, 200", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua juca mendes, 200", endereco_de_origem_sa), "rua juca mendes, 200", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua juca mendes, 182", endereco_de_origem_sa), "rua juca mendes, 182", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua juca mendes, 179", endereco_de_origem_sa), "rua juca mendes, 179", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua jose vicente cavalheiro, 109", endereco_de_origem_sa), "rua jose vicente cavalheiro, 109", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua jose de magalhaes, 500", endereco_de_origem_sa), "rua jose de magalhaes, 500", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua jose bauman, 69", endereco_de_origem_sa), "rua jose bauman, 69", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua joao teixeira de barros 227", endereco_de_origem_sa), "rua joao teixeira de barros 227", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua joao baptistussi, 55", endereco_de_origem_sa), "rua joao baptistussi, 55", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua joao baptistussi 0", endereco_de_origem_sa), "rua joao baptistussi 0", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua guaicurus, 1000", endereco_de_origem_sa), "rua guaicurus, 1000", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua giovanni di balduccio, 250", endereco_de_origem_sa), "rua giovanni di balduccio, 250", endereco_de_origem_sa),
         endereco_de_origem_sa = gsub("gen. jardim", "general jardim", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua general jardim, 36", endereco_de_origem_sa), "rua general jardim, 36", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua general chagas santos, 500", endereco_de_origem_sa), "rua general chagas santos, 500", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua dr. abelardo vergueiro cesar, 370", endereco_de_origem_sa), "rua dr. abelardo vergueiro cesar, 370", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua doutor jaci barbosa, 280", endereco_de_origem_sa), "rua doutor jaci barbosa, 280", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua coronel oscar porto, 472", endereco_de_origem_sa), "rua coronel oscar porto, 472", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua colonia nova, 110", endereco_de_origem_sa), "rua colonia nova, 110", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua cassiano dos santos 43", endereco_de_origem_sa), "rua cassiano dos santos 43", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua caraipe das aguas, 23", endereco_de_origem_sa), "rua caraipe das aguas, 23", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua candapui, 492", endereco_de_origem_sa), "rua candapui, 492", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua boa vista, 280", endereco_de_origem_sa), "rua boa vista, 280", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua balsa, 331", endereco_de_origem_sa), "rua balsa, 331", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua azem abdalla azem, 564", endereco_de_origem_sa), "rua azem abdalla azem, 564", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua azem abdalla azem 564", endereco_de_origem_sa), "rua azem abdalla azem, 564", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua augusto carlos bauman, 851", endereco_de_origem_sa), "rua augusto carlos bauman, 851", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua atucuri, 699", endereco_de_origem_sa), "rua atucuri, 699", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua apucarana, 215", endereco_de_origem_sa), "rua apucarana, 215", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua antonio marcondes, 159", endereco_de_origem_sa), "rua antonio marcondes, 159", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua antonio carlos de oliveira cesar, 97", endereco_de_origem_sa), "rua antonio carlos de oliveira cesar, 97", endereco_de_origem_sa),
         endereco_de_origem_sa = ifelse(grepl("rua afonso celso, 23", endereco_de_origem_sa), "rua afonso celso, 23", endereco_de_origem_sa),
         endereco_de_destino_sa = gsub("r.", "rua", endereco_de_destino_sa),
         endereco_de_destino_sa = gsub("av.", "avenida", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("viaduto do cha, 15", endereco_de_destino_sa), "viaduto do cha, 15", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("upa vila santa catarina - rua cidade de bagda", endereco_de_destino_sa), "upa vila santa catarina - rua cidade de bagda", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("serbom armazens gerais frigorificos", endereco_de_destino_sa), "serbom armazens gerais frigorificos ltda, avenida das nações unidas", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua xavier de almeida, 210", endereco_de_destino_sa), "rua xavier de almeida, 210", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua venceslau bras 50", endereco_de_destino_sa), "rua venceslau bras 50", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua vanderlei, 1371", endereco_de_destino_sa), "rua vanderlei, 1371", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua turiassu, 130", endereco_de_destino_sa), "rua turiassu, 130", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua tuiuti, 515", endereco_de_destino_sa), "rua tuiuti, 515", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua siria, 42", endereco_de_destino_sa), "rua siria, 42", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua silva bueno, 821", endereco_de_destino_sa), "rua silva bueno, 821", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua santo amaro, 439", endereco_de_destino_sa), "rua santo amaro, 439", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua santo amaro, 216", endereco_de_destino_sa), "rua santo amaro, 216", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua santa isabel, 181", endereco_de_destino_sa), "rua santa isabel, 181", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua santa eulalia, 86", endereco_de_destino_sa), "rua santa eulalia, 86", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua rosa de morais, 91", endereco_de_destino_sa), "rua rosa de morais, 91", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua pedro de toledo, 983", endereco_de_destino_sa), "rua pedro de toledo, 983", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua pedro de toledo, 1082", endereco_de_destino_sa), "rua pedro de toledo, 1082", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua pedro de toledo, 1071", endereco_de_destino_sa), "rua pedro de toledo, 1071", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua pedro avelino, 22", endereco_de_destino_sa), "rua pedro avelino, 22", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua paulistania, 520", endereco_de_destino_sa), "rua paulistania, 520", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua paranacity, 122", endereco_de_destino_sa), "rua paranacity, 122", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua paranacity, 117", endereco_de_destino_sa), "rua paranacity, 117", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua paineira do campo, 902", endereco_de_destino_sa), "rua paineira do campo, 902", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua padre marchetti, 557", endereco_de_destino_sa), "rua padre marchetti, 557", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua ouvidor portugal, 230", endereco_de_destino_sa), "rua ouvidor portugal, 230", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua olivia guedes penteado, 267", endereco_de_destino_sa), "rua olivia guedes penteado, 267", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua oliveira catrambi 952", endereco_de_destino_sa), "rua oliveira catrambi 952", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua oliveira catrambi 956", endereco_de_destino_sa), "rua oliveira catrambi 956", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua nossa senhora das dores, 350", endereco_de_destino_sa), "rua nossa senhora das dores, 350", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua nazir miguel 849", endereco_de_destino_sa), "rua nazir miguel 849", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua mourato coelho, 799", endereco_de_destino_sa), "rua mourato coelho, 799", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua mauro bonafe pauletti, 199", endereco_de_destino_sa), "rua mauro bonafe pauletti, 199", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua monte alegre 45", endereco_de_destino_sa), "rua monte alegre 45", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua mauro bonafe pauletti, 160", endereco_de_destino_sa), "rua mauro bonafe pauletti, 160", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua mauricio araujo martins, 342", endereco_de_destino_sa), "rua mauricio araujo martins, 342", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua marina crespi, 91", endereco_de_destino_sa), "rua marina crespi, 91", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua maria fett, 467", endereco_de_destino_sa), "rua maria fett, 467", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua maria amalia lopes azevedo, 3650", endereco_de_destino_sa), "rua maria amalia lopes azevedo, 3650", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua manuel moscoso, 15", endereco_de_destino_sa), "rua manuel moscoso, 15", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua manuel correia, 238", endereco_de_destino_sa), "rua manuel correia, 238", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua machado bitencourt, 220", endereco_de_destino_sa), "rua machado bitencourt, 220", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua luis carneiro, 193", endereco_de_destino_sa), "rua luis carneiro, 193", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua lucio de arruda leme, 211", endereco_de_destino_sa), "rua lucio de arruda leme, 211", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua lucas de leyde, 157", endereco_de_destino_sa), "rua lucas de leyde, 157", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua loefgren, 2109", endereco_de_destino_sa), "rua loefgren, 2109", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua loefgren, 1742", endereco_de_destino_sa), "rua loefgren, 1742", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua lino coutinho, 841", endereco_de_destino_sa), "rua lino coutinho, 841", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua lino coutinho, 444", endereco_de_destino_sa), "rua lino coutinho, 444", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua libero badaro, 425", endereco_de_destino_sa), "rua libero badaro, 425", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua libero badaro, 293", endereco_de_destino_sa), "rua libero badaro, 293", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua libero badaro, 190", endereco_de_destino_sa), "rua libero badaro, 190", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua libero badaro, 158", endereco_de_destino_sa), "rua libero badaro, 158", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua libero badaro, 119", endereco_de_destino_sa), "rua libero badaro, 119", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua leo ribeiro de moraes, 66", endereco_de_destino_sa), "rua leo ribeiro de moraes, 66", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua leandro dupret, 525", endereco_de_destino_sa), "rua leandro dupret, 525", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua juventus, 562", endereco_de_destino_sa), "rua juventus, 562", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua julio felipe guedes, 200", endereco_de_destino_sa), "rua julio felipe guedes, 200", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua juca mendes, 200", endereco_de_destino_sa), "rua juca mendes, 200", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua juca mendes, 182", endereco_de_destino_sa), "rua juca mendes, 182", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua juca mendes, 179", endereco_de_destino_sa), "rua juca mendes, 179", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua jose vicente cavalheiro, 109", endereco_de_destino_sa), "rua jose vicente cavalheiro, 109", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua jose de magalhaes, 500", endereco_de_destino_sa), "rua jose de magalhaes, 500", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua jose bauman, 69", endereco_de_destino_sa), "rua jose bauman, 69", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua joao teixeira de barros 227", endereco_de_destino_sa), "rua joao teixeira de barros 227", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua joao baptistussi, 55", endereco_de_destino_sa), "rua joao baptistussi, 55", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua joao baptistussi 0", endereco_de_destino_sa), "rua joao baptistussi 0", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua guaicurus, 1000", endereco_de_destino_sa), "rua guaicurus, 1000", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua giovanni di balduccio, 250", endereco_de_destino_sa), "rua giovanni di balduccio, 250", endereco_de_destino_sa),
         endereco_de_destino_sa = gsub("gen. jardim", "general jardim", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua general jardim, 36", endereco_de_destino_sa), "rua general jardim, 36", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua general chagas santos, 500", endereco_de_destino_sa), "rua general chagas santos, 500", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua dr. abelardo vergueiro cesar, 370", endereco_de_destino_sa), "rua dr. abelardo vergueiro cesar, 370", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua doutor jaci barbosa, 280", endereco_de_destino_sa), "rua doutor jaci barbosa, 280", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua coronel oscar porto, 472", endereco_de_destino_sa), "rua coronel oscar porto, 472", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua colonia nova, 110", endereco_de_destino_sa), "rua colonia nova, 110", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua cassiano dos santos 43", endereco_de_destino_sa), "rua cassiano dos santos 43", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua caraipe das aguas, 23", endereco_de_destino_sa), "rua caraipe das aguas, 23", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua candapui, 492", endereco_de_destino_sa), "rua candapui, 492", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua boa vista, 280", endereco_de_destino_sa), "rua boa vista, 280", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua balsa, 331", endereco_de_destino_sa), "rua balsa, 331", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua azem abdalla azem, 564", endereco_de_destino_sa), "rua azem abdalla azem, 564", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua azem abdalla azem 564", endereco_de_destino_sa), "rua azem abdalla azem, 564", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua augusto carlos bauman, 851", endereco_de_destino_sa), "rua augusto carlos bauman, 851", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua atucuri, 699", endereco_de_destino_sa), "rua atucuri, 699", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua apucarana, 215", endereco_de_destino_sa), "rua apucarana, 215", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua antonio marcondes, 159", endereco_de_destino_sa), "rua antonio marcondes, 159", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua antonio carlos de oliveira cesar, 97", endereco_de_destino_sa), "rua antonio carlos de oliveira cesar, 97", endereco_de_destino_sa),
         endereco_de_destino_sa = ifelse(grepl("rua afonso celso, 23", endereco_de_destino_sa), "rua afonso celso, 23", endereco_de_destino_sa))

analise_taxis_final <- x

setwd("C:/Users/coliv/Documents/Taxis")
save(analise_taxis_final, file="analise_taxis_final.Rdata")

x <- analise_taxis_final %>%
  mutate(endereco_de_origem_sa = gsub("\\?", "", endereco_de_origem),
         endereco_de_destino_sa = gsub("\\?", "", endereco_de_destino),
         endereco_de_origem_sa = tolower(stri_trans_general(endereco_de_origem_sa, "Latin-ASCII")),
         endereco_de_destino_sa = tolower(stri_trans_general(endereco_de_destino_sa, "Latin-ASCII")),
         endereco_de_destino_sa = gsub("\\-.*","",endereco_de_destino_sa),
         endereco_de_origem_sa = gsub("\\-.*","",endereco_de_origem_sa),
         endereco_de_origem_sa = gsub("^r\\.", "rua", endereco_de_origem_sa),
         endereco_de_destino_sa = gsub("^r\\.", "rua", endereco_de_destino_sa),
         endereco_de_origem_sa = gsub("^av\\.", "avenida", endereco_de_origem_sa),
         endereco_de_destino_sa = gsub("^av\\.", "avenida", endereco_de_destino_sa),
         endereco_de_origem_sa = gsub("dr\\.", "doutor", endereco_de_origem_sa),
         endereco_de_destino_sa = gsub("dr\\.", "doutor", endereco_de_destino_sa),
         endereco_de_origem_sa = gsub("sen\\.", "senador", endereco_de_origem_sa),
         endereco_de_destino_sa = gsub("sen\\.", "senador", endereco_de_destino_sa),
         endereco_de_origem_sa = gsub("gen\\.", "general", endereco_de_origem_sa),
         endereco_de_destino_sa = gsub("gen\\.", "general", endereco_de_destino_sa),
         endereco_de_origem_sa = gsub("^([^,]+,[^,]+),.*$", "\\1", endereco_de_origem_sa),
         endereco_de_destino_sa = gsub("^([^,]+,[^,]+),.*$", "\\1", endereco_de_destino_sa),
         endereco_de_origem_sa = gsub("sra\\.", "senhora", endereco_de_origem_sa),
         endereco_de_destino_sa = gsub("sra\\.", "senhora", endereco_de_destino_sa),
         endereco_de_origem_sa = gsub("sr\\.", "senhor", endereco_de_origem_sa),
         endereco_de_destino_sa = gsub("sr\\.", "senhor", endereco_de_destino_sa),
         endereco_de_origem_sa = gsub(",", "", endereco_de_origem_sa),
         endereco_de_destino_sa = gsub(",", "", endereco_de_destino_sa),
         endereco_de_origem_sa = gsub(" ", "_", endereco_de_origem_sa),
         endereco_de_destino_sa = gsub(" ", "_", endereco_de_destino_sa),
         endereco_de_origem_sa = gsub("_$", "", endereco_de_origem_sa),
         endereco_de_destino_sa = gsub("_$", "", endereco_de_destino_sa),
         endereco_de_origem_sa = ifelse(grepl("omitido_conforme", endereco_de_origem_sa), "omitido", endereco_de_origem_sa),
         endereco_de_destino_sa = ifelse(grepl("omitido_conforme", endereco_de_destino_sa), "omitido", endereco_de_destino_sa))

analise_taxis_final <- x

setwd("C:/Users/coliv/Documents/Taxis")
save(analise_taxis_final, file="analise_taxis_final.Rdata")

### Download relação dos endereços

library(googlesheets)
gs_ls() 

#Importando:
destinos_sheet <- gs_title("endereços_taxis")

#Atribuindo o df a um objeto:
destinos <- gs_read(destinos_sheet)


destinos <- destinos %>%
  mutate(destino_relacionado_ao_endereco = tolower(destino_relacionado_ao_endereco)) %>%
  distinct(destino_relacionado_ao_endereco, endereco)

setwd("C:/Users/coliv/Documents/Taxis")
save(destinos, file="destinos.Rdata")


test <- dados_app %>%
  clean_names() %>%
  mutate(odometro_km = gsub(",", ".", odometro_km),
         odometro_km = as.numeric(odometro_km),
         id_da_corrida = as.character(id_da_corrida)) %>%
  select(id_da_corrida, odometro_km)

analise_taxis_antiga <- analise_taxis_final

analise_taxis_final2 <- analise_taxis_final %>%
  select(-c(km, odometro_km)) %>%
  left_join(test)

analise_taxis_final <- analise_taxis_final2
save(analise_taxis_antiga, file="analise_taxis_antiga.Rdata")
save(analise_taxis_final, file="analise_taxis_final.Rdata")
