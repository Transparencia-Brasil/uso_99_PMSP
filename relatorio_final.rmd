---
title: "RelatÃ³rio TransparÃªncia Brasil - Taxis em SÃ£o Paulo"
author: "Jessica Voigt"
date: "27 de maio de 2019"
output:
  word_document: default
  html_document: default
---

# Uso de taxis em São Paulo

## Resumo Executivo
dahdsuad

## Introdução 
No ano de 2017, a prefeitura municipal de São Paulo passou a utilizar serviços fornecidos por aplicativo de transporte individual para a locomoção de servidores, substituindo a frota de carros da administração municipal. A empresa 99 Taxis venceu a licitação (botar nota de rodapé com contrato e aditivos) e desde então opera como provedora deste serviço, tendo seu contrato sido extendido até julho de 2019.
Este relatório tem como objetivo avaliar como tem sido, desde então, o uso de carros pelos servidores da prefeitura de São Paulo e identificar possíveis abusos cometidos por servidores. As conclusões são ...

## Padrão das corridas em São Paulo

```{r, echo = F, eval = TRUE, warning=FALSE}
library(readxl)
library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)
library(dplyr)
library(deflateBR)
library(stringi)
library(ggmap)
library(knitr)
library(stringr)
library(knitr)
library(xlsx)
library(readxl)
library(stringi)


theme_set(theme_minimal())

setwd("C:/Users/coliv/Documents/Taxis")

load("analise_taxis_final.Rdata")

```

A análise dos dados das corridas feitas por servidores da Prefeitura Municipal de São Paulo mostra que desde 2017 o uso dessa modalidade tem crescido. Entre agosto e dezembro de 2018 foram registradas mais de 116 mil viagens, três vezes a mais do mesmo período do ano passado, quando a prefeitura passou a adotar o serviço de transporte particular por aplicativo. Ao todo, foram registradas 245 mil viagens em 2018, número que deverá aumentar em 2019.


```{r, echo = F, eval = TRUE, warning=FALSE}

# #Caclulando  de agosto a dezembro de 2017 e de 2018. Para ves os números, rode as linhas abaixo:
# 
# analise_taxis_final %>%
#   mutate(data_origem = as.character(data_origem),
#          mes_origem = stri_sub(data_origem,1, -4)) %>%
#   group_by(mes_origem) %>%
#   summarise(viagens_mil = n()/1000) %>%
#   ungroup() %>%
#   filter(mes_origem != "2019-03") %>%
#   summarise(corridas_08_12_2017 = sum(viagens_mil[mes_origem %in% c("2017-08", "2017-09", "2017-10", "2017-11", "2017-12")]),
#             corridas_08_12_2018 = sum(viagens_mil[mes_origem %in% c("2018-08", "2018-09", "2018-10", "2018-11", "2018-12")]),
#             perc_maior = corridas_08_12_2018/corridas_08_12_2017)


# #viagens em 2018:
# 
# analise_taxis_final  %>%
#   filter(data_origem > "2017-12-31",
#          data_origem < "2019-01-01") %>%
#   nrow()

#Gráfico:

analise_taxis_final %>%
  mutate(data_origem = as.character(data_origem),
         mes_origem = stri_sub(data_origem,1, -4)) %>%
  group_by(mes_origem) %>%
  summarise(viagens_mil = n()/1000) %>%
  ungroup() %>%
  filter(mes_origem != "2019-03") %>%
  ggplot(aes(x=mes_origem, y=viagens_mil, group = 1)) + 
  geom_line(colour="#da7144") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank()) +
   labs(title= "Corridas realizadas", 
        subtitle="Viagens realizadas pela prefeitura de São Paulo usando aplicativo de transporte individual", 
       caption="Fonte: Prefeitura Municipal de São Paulo via Lei de Acesso à Informação. Elaborado pela Transparência Brasil", 
       y="viagens (mil)",
       x = "mês")
  
```

De forma geral, os servidores usam a os aplicativos de transporte individual de maneira resposável: 95% das corridas realizadas no período foram feitas com a modalidade 99 POP, percorrendo em média 11,4 km e custando em média cerca de R\$ 29,00. No entanto 5% das corridas foram realizadas em outras modalidades (99 TAXI, 99TOP e Taxi comum) e, para essas corridas, o valor médio do km rodado foi muito superior ao estabelecido pelo contrato entre a 99 Taxis e a prefeitura de São Paulo, que estabelece o valor do kilometro rodado em R\$2,52 [1] [2] [3] . 

[1] Foram desconsideradas nesse cálculo corridas que não apresentam quilometragem registrada. Trataremos dessas viagens em uma sessão posterior. 
[2] O custo do kilometro rodado definido pelo contrato [009/SMG/2017](https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/TERMODECONTRATO99(1).pdf) era de R\$2,46 , sendo mais tarde modificado por [aditivo](https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/Scanned-image_10-04-2019-092634.pdf) para R\$2,52
[3] O custo médio da corrida e o valor médio por km rodado foram calculados a partir dos valores correntes, de modo que não houvesse superestimação do custo médio por km rodado. 

```{r, echo = F, eval = TRUE, warning=FALSE}

analise_taxis_final %>%
  filter(odometro_km != 0) %>%
  group_by(categoria) %>%
  summarise(`Total corridas` = n(),
            `Total km rodados (mil)` = round(sum(odometro_km, na.rm=TRUE)/1000 ,2 ),
            `Percentual uso da categoria` =  paste0(round((`Total corridas`)/(312600+7856+5798+2973),2)*100, "%"),
            `Média km rodados` = round(mean(odometro_km, na.rm=TRUE),1),
            `Total gasto (mil)` = round(sum(tarifa_deflate, na.rm=TRUE)/1000, 2),
            `Custo médio da corrida` = round(mean(tarifa, na.rm=TRUE),2),
            `Valor médio por km rodado` = round(`Custo médio da corrida`/`Média km rodados`,2)) %>%
  arrange(desc(`Total corridas`)) %>%
  kable()

```

### Viagens que custaram o dobro da média

A Transparência Brasil verificou que em 16.835 (5,1%) viagens o valor do quilometro custou pelo menos o dobro do valor médio por quilometro dentro de cada modalidade (tabela 1). 

Para averiguar esse dado, dividimos o valor da corrida [nota - valor corrente, para as corridas que constam quilometragem] pelo total de quilometros rodados, e selecionamos aquelas corridas cujo valor médio do km era pelo menos duas vezes maior que o valor médio do km para aquela categoria como verificado na tabela 1. 

Para as categorias 99TOP e 99Taxi, o valor do quilometro para essas viagens custa em média 4 vezes o valor médio da modalidade, para a categoria 99POP esse vaor é 5 vezes maior e para o taxi comum o custo médio do quilometro chega aos inacreditáveis R$ 41,00. Cerca de 12% das corridas feitas com taxi e cuja quilometragem foi registrada possuem o valor do quilómetro acima do dobro da média. 

Não há como saber o porquê dessas corridas terem custado tão acima da média da modalidade. A relação dessas viagens está no anexo X.


```{r}
# Corridas que custaram mais de mil reais:

med_mod <- data.frame(categoria = c("99POP","99TOP","Taxi comum","99TAXI"),
                      valor_medio = c(2.53, 4.06, 5.62, 3.55),
                      dob_valor_medio = c(5.06,  8.12, 11.24,  7.10))


superfaturadas <- analise_taxis_final %>%
  filter(odometro_km != 0,
         tempo_viagem_hora < 24) %>%
  left_join(med_mod) %>%
  mutate(valor_km = round(tarifa/odometro_km,2)) %>%
  filter(valor_km > dob_valor_medio)

#Ficou salvo no documento final como anexo 1
# Anexo 1: Viagens cujo preço do quilometro corresponde a pelo menos o dobro do valor médio do quilometro para a modalidade: 
anexo1 <- superfaturadas %>%
  select(id_da_corrida, tarifa, hora_origem, hora_final, categoria, plataforma_de_chamada_web_app,
         empresa, centro_de_custo, endereco_de_origem, endereco_de_destino, justificativa,
         odometro_km, valor_km, valor_medio) %>%
  rename(valor_medio_km_para_modalidade = valor_medio)

# setwd("C:/Users/coliv/Documents/Taxis/anexos formatados")
# 
#Preparando o CSV

# name_anexo1 <- c("Id Corrida",	"Custo (valor corrente)",
#                  "Data e hora de origem",	"Data e hora final",
#                  "Categoria",	"Plataforma de chamada",
#                  "Empresa",
#                  "Centro de custo",
#                  "Endereço de origem",
#                  "Endereço de destino",
#                  "Justificativa",	"Odometro KM",	
#                  "Valor do KM",	"Valor médio do KM para a modalidade")
# 
# name_anexo1 <- snakecase::to_any_case(name_anexo1)
# name_anexo1 <- stri_trans_general(name_anexo1, "Latin-ASCII")
# names(anexo1) <- name_anexo1
# 
# write.csv(anexo1, file="Anexo 1 Viagens cujo preco do quilometro corresponde a pelo menos o dobro do valor medio do quilometro para a modalidade.csv", row.names = FALSE, fileEncoding = "UTF-8")



# write.xlsx(as.data.frame(anexo1),
#            file="anexo1.xlsx", sheetName="anexo1",
#            col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)



t <- analise_taxis_final %>%
  filter(odometro_km != 0) %>%
  group_by(categoria) %>%
  summarise(corridas_com_km = n())

superfaturadas %>%
  group_by(categoria) %>%
  summarise(corridas = n(),
            valor_medio_km = round(mean(valor_km),2)) %>%
  left_join(med_mod) %>%
  left_join(t) %>%
  mutate(perc_corridas_com_km = round(corridas/corridas_com_km, 2)*100) %>%
  select(-c(dob_valor_medio, corridas_com_km)) %>%
  kable()
```

## Tempo das corridas

Os dados apontam uma grande variação na duração das corridas. Mesmo considerando que a maioria das corridas demoram entre 10 minutos e uma hora (70,4%), há registro de corridas que demoram entre uma e duas horas (11,1%), entre duas e quatro horas (4,1%), mais de cinco horas (0,5%), e ainda 85 corridas demoraram mais de 12 horas. Há também corridas que não chegam a 3 minutos (1,7%) e que provavelmente poderiam ter sido feitas a pé.

```{r, echo = F, eval = TRUE, warning=FALSE}

# Tempo de duração das corridas:
#Tabela 4
analise_taxis_final %>%
  mutate(`Tempo viagem` = ifelse(tempo_viagem_min < 4 , "até 3 minutos",
                                 ifelse(tempo_viagem_min > 3 & tempo_viagem_min <11, "entre 3 e 10 minutos",
                                        ifelse(tempo_viagem_min > 10 & tempo_viagem_min <31, "entre 10 e 30 minutos",
                                               ifelse(tempo_viagem_min > 30 & tempo_viagem_min <61, "entre 30 minutos e 1 hora",
                                                      ifelse(tempo_viagem_hora > 0.99 & tempo_viagem_hora < 2.01, "entre uma e duas horas",
                                                             ifelse(tempo_viagem_hora > 2 & tempo_viagem_hora < 5.01, "entre duas e quatro horas",
                                                                    ifelse(tempo_viagem_hora > 5 & tempo_viagem_hora < 12.01, "entre cinco e doze horas",
                                                                           ifelse(tempo_viagem_hora > 12 , "mais de doze  horas", "ERRO"))))))))) %>%
  group_by(`Tempo viagem`) %>%
  summarise( `Corridas` = n(),
             `Total gasto` = paste0("R$ ",round(sum(tarifa_deflate), 0))) %>%
  mutate(ord = ifelse(`Tempo viagem` == "até 3 minutos", 1,
                      ifelse(`Tempo viagem` == "entre 3 e 10 minutos", 2,
                             ifelse(`Tempo viagem` == "entre 10 e 30 minutos", 3,
                                    ifelse(`Tempo viagem` == "entre 30 minutos e 1 hora", 4,
                                           ifelse(`Tempo viagem` == "entre uma e duas horas", 5,
                                                  ifelse(`Tempo viagem` == "entre duas e quatro horas", 6,
                                                         ifelse(`Tempo viagem` == "entre cinco e doze horas", 7,
                                                                ifelse(`Tempo viagem` == "mais de doze horas", 8, "ERRO")))))))),
         `Percentual das corridas` = paste0(round(`Corridas`/329227,3)*100, "%")) %>%
  arrange(ord) %>%
  select(-c(ord)) %>%
  kable()
```

### Corridas com mais de doze horas

Os dados indicam que as corridas que levaram mais de 12 horas dizem respeito a carros solicitados via aplicativo que ficou à disposição dos servidores durante todo o dia. Ao todo, 8 órgãos da Prefeitura Municipal de São Paulo registraram corridas com doze ou mais horas e 4 órgãos registraram corridas com mais de um dia de duração[nota].

[nota] Registramos outras quatro corridas com mais de 24 horas de duração. No entanto, como o valor pago para essas quatro corridas havia sido muito baixo, optamos por desconsiderá-las nessa análise. Os ids das corridas desconsideradas são "122785126", "160810029", "159789088" e "87961254633069" 

As Prefeitura Regional Ermelino Matarazzo registrou uma uma corrida com um dia. As prefeituras regionais do Itaim Paulista e a Diretoria Regional de Educação de Guianases registraram cada uma uma corrida que durou dois dias. A Prefeitura Regional do Itaim Paulista registrou uma corrida que durou 3 dias. Por fim, a Coordenadoria Regional de Saúde Sudeste possui uma corrida que durou oito dias. Essa viagem sozinha custou R$6588,13. As justificativas dessas corridas não ajudam a entender o porquê foi necessário ter o mesmo carro à disposição por mais de um dia. 


```{r, echo = F, eval = TRUE, warning=FALSE}

# # # Quem percorreu as corridas mais longas?
# analise_taxis_final %>%
#   mutate(`Tempo viagem` = ifelse(tempo_viagem_min == 0, "menos de 1 minuto",
#                                  ifelse(tempo_viagem_min > 0 & tempo_viagem_min <11, "até 10 minutos",
#                                         ifelse(tempo_viagem_min > 10 & tempo_viagem_min <31, "entre 10 e 30 minutos",
#                                                ifelse(tempo_viagem_min > 30 & tempo_viagem_min <61, "entre 30 minutos e 1 hora",
#                                                       ifelse(tempo_viagem_hora > 0.99 & tempo_viagem_hora < 2.01, "entre uma e duas horas",
#                                                              ifelse(tempo_viagem_hora > 2 & tempo_viagem_hora < 5.01, "entre duas e quatro horas",
#                                                                     ifelse(tempo_viagem_hora > 5 & tempo_viagem_hora < 12.01, "entre cinco e doze horas",
#                                                                            ifelse(tempo_viagem_hora > 12 , "mais de doze  horas", "ERRO"))))))))) %>%
#   filter(tempo_viagem_hora > 24) %>%
#   group_by(`Tempo viagem`, empresa) %>%
#   summarise(corridas = n()) %>%
#   ungroup() %>%
#   arrange(desc(corridas))

# 
# #Quantos dias o carro ficou à disposição da prefeitura?
# analise_taxis_final %>%
#   filter(tempo_viagem_hora > 24) %>%
#   mutate(data_origem = as.Date(data_origem, format="%Y-%m-%d"),
#          data_final = as.Date(data_final, format="%Y-%m-%d"),
#          dias_viagem = data_final - data_origem) %>%
#   group_by(empresa, dias_viagem) %>%
#   summarise(qtde_corridas = n()) %>%
#   arrange(desc(dias_viagem))


# #Corridas com mais de doze horas:
# analise_taxis_final %>%
#   filter(tempo_viagem_hora > 12) %>%
#   mutate(data_origem = as.Date(data_origem, format="%Y-%m-%d"),
#          data_final = as.Date(data_final, format="%Y-%m-%d"),
#          dias_viagem = data_final - data_origem) %>%
#   filter(dias_viagem < 1) %>%
#   group_by(empresa) %>%
#   summarise(qtde_corridas = n())

#Quais eram as justificativas para corridas com mais de um dia:
analise_taxis_final %>%
  mutate(data_origem = as.Date(data_origem, format="%Y-%m-%d"),
         data_final = as.Date(data_final, format="%Y-%m-%d"),
         dias_viagem = data_final - data_origem) %>%
  filter(tempo_viagem_hora > 24,
         !id_da_corrida %in% c("122785126", "160810029", "159789088", "87961254633069" )) %>%
  select(id_da_corrida, empresa, data_origem, data_final, dias_viagem, justificativa , tarifa, tarifa_deflate) %>%
  mutate(justificativa = gsub("\\+", " ", justificativa),
         justificativa = tolower(justificativa),
         dias_viagem = gsub("days", "dias", dias_viagem),
         dias_viagem = paste0(dias_viagem, " dias"),
         tarifa = paste0("R$ ", tarifa),
         tarifa_deflate = paste0("R$ ", tarifa_deflate)) %>%
  mutate(data_origem = format(data_origem, "%d/%m/%Y"),
         data_final = format(data_final, "%d/%m/%Y")) %>%
  arrange(desc(dias_viagem)) %>%
  rename(`ID da corrida`= id_da_corrida,
         `Órgão` = empresa,
         `Duração viagem` = dias_viagem,
         `Justificativa` = justificativa,
         `Valor da corrida (corrente)` = tarifa,
         `Valor da corrida ajustado`= tarifa_deflate,
         `Data início`= data_origem,
         `Data final` = data_final) %>%
  kable()
  

```

### Corridas com menos de 3 minutos

Por outro lado, encontramos 5.481 corridas que duraram menos de três minutos, um percurso que provavelmente uma pessoa poderia percorrer a pé em pouco mais de quinze minutos. Essas corridas são muito mais comuns do que corridas com mais de doze horas, correspondendo a 1,7% das viagens registradas em todo o período. Essas viagens foram realizadas por 87 dos 93 órgãos da prefeitura e custaram R\$47,6 mil aos cofres públicos. Destes, R\$19,7 mil foram gastos com corridas que duraram menos de um minuto. 

Os órgãos que mais gastaram com viagens com menos de três minutos foram novamente a Coordenadoria Regional de Saúde Sudeste, que gastou R\$ 8,7 mil e a Secretaria Municipal da Pessoa com Deficiência - SMPED, que gastou R\$ 7,9 mil com essas corridas. A lista completa dos órgãos que registraram corridas com menos de 3 minutos está no anexo 2.


```{r, echo = F, eval = TRUE, warning=FALSE}

# #Quantidade de corridas com menos de 3 minutos
# analise_taxis_final %>%
#   filter(tempo_viagem_min < 4 ) %>%
#   nrow()

# Numero de órgãos que têm corridas com menos de 3 minutos
# analise_taxis_final %>%
#   filter(tempo_viagem_min < 4 ) %>%
#   group_by(empresa) %>%
#   summarise(corridas = n()) %>%
#   nrow()

# #  Quanto custaram corridas com menos de 3 minutos
# analise_taxis_final %>%
#   filter(tempo_viagem_min < 4 ) %>%
#   summarise(sum(tarifa_deflate)) 

#Quem foram os órgãos com corridas com menos de 3 minutos?
#Ficou salvo no documento final como anexo 4
# Anexo 4 Órgãos que registraram corridas com menos de 3 minutos  (antigo anexo 2)
anexo4 <- analise_taxis_final %>%
  mutate(num = ifelse(tempo_viagem_min < 4, 1, 0)) %>%
  filter(tarifa_deflate > 1) %>%
  group_by(empresa) %>%
  summarise(`Corridas até 3 minutos` = sum(num),
            `Total de corridas` = n(),
            `Percentual de corridas de até 3 minutos em relação ao total de corridas do órgão` = paste0(round(`Corridas até 3 minutos`/`Total de corridas`, 2)*100, "%"),
            `Custo total corridas com menos de 3 minutos` = sum(tarifa_deflate[num == 1])) %>%
  arrange(desc(`Custo total corridas com menos de 3 minutos`)) %>%
  filter(`Corridas até 3 minutos` > 0)

setwd("C:/Users/coliv/Documents/Taxis")

# write.xlsx(as.data.frame(anexo4), 
#            file="anexo4.xlsx", sheetName="anexo4",
#            col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)
# name_anexo4 <- c("Empresa",
#                  "Corridas até 3 minutos",
#                  "Total de corridas",
#                  "Percentual de corridas de até 3 minutos em relação ao total de corridas do órgão",
#                  "Custo total corridas com menos de 3 minutos")
# 
# name_anexo4 <- snakecase::to_any_case(name_anexo4)
# name_anexo4 <- stri_trans_general(name_anexo4, "Latin-ASCII")
# names(anexo4) <- name_anexo4
# 
# write.csv(anexo4, file="Anexo 4 Orgaos que registraram corridas com menos de 3 minutos.csv", row.names = FALSE, fileEncoding = "UTF-8")


```


## Corridas sem quilometragem registrada

Ao todo, 3.921 (1,2%) corridas realizadas por 84 órgãos da prefeitura não possuem quilometragem registrada. Essas corridas custaram as cofres públicos R\$216,1 mil. Não sabemos o motivo de não se ter sido registrada nenhuma quilometragem para essas corridas[4], já que os dados obtidos indicam que todas as corridas foram requisitadas pelo app ou pela plataforma web da 99.

As corridas sem quilometragem ocorrem principalmente na modalidade "Taxi comum". Enquanto para as outras modalidades os gastos com corridas sem quilometragem não chegam a 0,5% do gasto total da modalidade, com o taxi comum esses gastos representam 57,3% . Cada corrida sem quilometragem feita com taxi comum custa, em média, R\$ 127,15 . 

---


[4] A princípio, pensou-se que essas corridas poderiam ser corridas canceladas pelo usuário. De acordo com o artigo 5.5 dos Termos de Uso do aplicativo 99 , o passageiro que cancelar uma viagem está sujeito a pagar uma taxa de cancelamento para ressarcimento do motorista. No entanto, foi encontrada a fórmula do cálculo da taxa de cancelamento nem nos termos de uso e nem no site da emrpesa. Os dados, por sua vez, não permitem distinguir uma corrida cancelada de uma corrida que foi realizada e cuja quilometragem não foi registrada. Diante disso e do fato que o poder público pagou por essas corridas, optamos por considerar todas as corridas válidas. 

```{r, echo = F, eval = TRUE, warning=FALSE}

# #quantidade de corridas:
# analise_taxis_final %>%
#   filter(odometro_km == 0) %>%
#   nrow()

# #quantos órgãos registraram corridas sem quilometragem
# analise_taxis_final %>%
#   filter(odometro_km == 0) %>%
#   distinct(empresa) %>%
#   nrow()

# #Quanto curstaram as corridas sem quilometragem:
# analise_taxis_final %>%
#   summarise(valor_gasto = sum(tarifa_deflate[odometro_km == 0]),
#             total_gasto = sum(tarifa_deflate),
#             perc = paste0(round(valor_gasto/total_gasto, 4)*100), "%")

# Em qual modalidade elas são mais frequentes?
#Tabela 6
analise_taxis_final %>%
  mutate(num = 1) %>%
  group_by(categoria) %>%
  summarise(corridas = sum(num[odometro_km == 0]),
            gastos_sem_km = sum(tarifa_deflate[odometro_km == 0]),
            gastos_totais = sum(tarifa_deflate),
            perc_gasto_modalidade = paste0(round(gastos_sem_km/gastos_totais,4)*100, "%"),
            custo_medio_corrida_sem_km = paste0("R$ ", round(mean(tarifa_deflate[odometro_km == 0]), 2))) %>%
  ungroup() %>%
  mutate(gastos_sem_km = paste0("R$ ", gastos_sem_km)) %>%
  rename(`Modalidade`= categoria,
         `Corridas sem quilometragem` = corridas,
         `Custo total corridas sem quilometragem` = gastos_sem_km,
         `Percentual do custo corridas sem quilometragem em relação ao total de corridas da modalidade` = perc_gasto_modalidade,
         `Custo médio corridas sem quilometragem` = custo_medio_corrida_sem_km) %>%
  select(-c(gastos_totais)) %>%
  kable()

#Dados da SMPED
smped <- analise_taxis_final %>%
  filter(empresa == "Secretaria Municipal da Pessoa com Deficiência - SMPED" ) %>%
  select(id_da_corrida, justificativa, tempo_viagem_min, tarifa, categoria, odometro_km, tarifa_deflate,
         hora_origem, hora_final, endereco_de_origem, endereco_de_destino, tempo_viagem_hora)

smped %>%
  mutate(km_registrada = ifelse(odometro_km == 0 , "não", "sim")) %>%
  group_by(km_registrada) %>%
  summarise(corridas = n(),
            perc_corridas = n()/5344,
            gastos = sum(tarifa_deflate))

smped %>%
  mutate(justificativa = tolower(justificativa),
         cid = ifelse(justificativa == "cid", 1, 0)) %>%
  group_by(cid) %>%
  summarise(corridas = n(),
            custo = sum(tarifa_deflate),
            valor_medio_viagem = mean(tarifa_deflate))

smped %>%
  mutate(falha = ifelse(grepl("falha", justificativa), 1,0)) %>%
  group_by(falha) %>%
  summarise(corridas = n(),
            valor_medio = mean(tarifa))
         
falha <- smped %>%
  filter(grepl("falha", justificativa))

#Viagens "lançadas"

lancadas <- analise_taxis_final %>%
  mutate(justificativa = tolower(justificativa),
         justificativa = stri_trans_general(justificativa, "Latin-ASCII")) %>%
  left_join(med_mod) %>%
  filter(odometro_km == 0 & categoria == "Taxi comum") %>%
  select(empresa,justificativa, odometro_km, tarifa)


```

A Secretaria Municipal da Pessoa com Deficiência - SMPED é o órgão que mais possui corridas sem quilometragem registrada. São 1.404 corridas que custaram R$191,5 mil. Destas corridas, 1.378 foram feitas com o uso de taxi comum. A tarifa média das corridas da SMPED sem quilometragem registrada é de R\$ 132,00. 

Uma justificativa comum para o uso dessa modalidade é o deslocamento do Secretário, que é tretaplégico. No entanto, cabe mencionar que a prestação de contas é rasa. Não se sabe a razão para solicitar um taxi, que possui tarifa mais cara, e ainda não registrar a quilometragem percorrida com o transporte. 

A Coordenadoria Regional de Saúde Sudeste ficou em segundo lugar em número de corridas sem quilometragem, gastando "apenas" R\$1,9 mil com 267 corridas sem quilometragem, todas utilizando 99POP. Não existe um período específico em que essas corridas são mais frequentes, as justificativas se referem a execução das atividades da secretaria e as corridas custam, em média, R$7,00 .

A relação da quantidade de corridas sem quilometragem está disponível no Anexo 3.



```{r, echo = F, eval = TRUE, warning=FALSE}

#Vendo a distribuição das corridas sem quilometragem por órgão e modalidade.

anexo5 <- analise_taxis_final %>%
  mutate(num = 1) %>%
  group_by(empresa) %>%
  summarise(corridas_sem_km = sum(num[odometro_km == 0]),
            gasto_total_corrida_Sem_km = paste0("R$ ", round(sum(tarifa_deflate[odometro_km == 0]),0)),
      
            `99POP` = sum(num[odometro_km == 0 & categoria == "99POP"]),
            perc_99pop = paste0(round(`99POP`/sum(num),2), "%"),
            gasto_99_pop = paste0("R$ ", round(sum(tarifa_deflate[odometro_km == 0 & categoria == "99POP"]),0)),
            
            `99TOP` = sum(num[odometro_km == 0 & categoria == "99TOP"]),
            perc_99top = paste0(round(`99TOP`/sum(num),2), "%"),
            gasto_99_top = paste0("R$ ", round(sum(tarifa_deflate[odometro_km == 0 & categoria == "99TOP"]),0)),
            
            `99TAXI` = sum(num[odometro_km == 0 & categoria == "99TAXI"]),
            perc_99taxi = paste0(round(`99TAXI`/sum(num),2), "%"),
            gasto_99_taxi = paste0("R$ ", round(sum(tarifa_deflate[odometro_km == 0 & categoria == "99TAXI"]),0)),

            `Taxi comum` = sum(num[odometro_km == 0 & categoria == "Taxi comum"]),
            perc_taxi_comum = paste0(round(`Taxi comum`/sum(num),2), "%"),
            gasto_taxi_comum = paste0("R$ ", round(sum(tarifa_deflate[odometro_km == 0 & categoria == "Taxi comum"]),0))) %>%
  filter(corridas_sem_km > 0) %>%
  arrange(desc(gasto_total_corrida_Sem_km))


 # setwd("C:/Users/coliv/Documents/Taxis/anexos formatados")
 
 # write.xlsx(as.data.frame(anexo5),
 #            file="anexo5.xlsx", sheetName="anexo5",
 #            col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)
# name_anexo5 <- c("Empresa",
#                  "Qtde corridas sem quilometragem",
#                  "Custo total (valores abril 2019)",
#                  "Qtde corridas 99POP",
#                  "Percentual corridas 99POP",
#                  "Custo 99POP (valores abril 2019)",
#                  "Qtde corridas 99TOP",
#                  "Percentual corridas 99TOP",
#                  "Custo 99TOP (valores abril 2019)",
#                  "Qtde corridas 99TAXI",
#                  "Percentual corridas 99TAXI",
#                  "Custo 99TAXI (valores abril 2019)",
#                  "Qtde corridas Taxi Comum",
#                  "Percentual corridas Taxi Comum",
#                  "Custo Taxi Comum (valores abril 2019)")
# 
# name_anexo5 <- snakecase::to_any_case(name_anexo5)
# name_anexo5 <- stri_trans_general(name_anexo5, "Latin-ASCII")
# names(anexo5) <- name_anexo5
# 
# write.csv(anexo5, file="Anexo 5 Corridas sem quilometragem registrada por orgao.csv", row.names = FALSE, fileEncoding = "UTF-8")

#Quando ocorreram as viagens da SMPD sem km?
# analise_taxis_final %>%
#   filter(odometro_km == 0,
#          empresa == "Secretaria Municipal da Pessoa com Deficiência - SMPED",
#          categoria == "Taxi comum") %>%
#   mutate(data_origem = as.character(data_origem),
#          mes_origem = stri_sub(data_origem,1, -4)) %>%
#   group_by(mes_origem) %>%
#   summarise(viagens = n()) %>%
#   ggplot(aes(x=mes_origem, y=viagens, group = 1)) +
#   geom_line(colour="#da7144") +
#   theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
#         panel.grid.minor = element_blank())

#Entre 100 e 1000
# x %>% filter(gasto_total_corrida_Sem_km > 100 & gasto_total_corrida_Sem_km < 1000) %>% nrow()

#justificativas:
# y <- analise_taxis_final %>%
#   filter(odometro_km == 0,
#          empresa == "Secretaria Municipal da Pessoa com Deficiência - SMPED") %>%
#   select(id_da_corrida, tarifa_deflate, categoria, justificativa)

#justificativas:
# y <- analise_taxis_final %>%
#   filter(odometro_km == 0,
#          empresa == "Coordenadoria Regional de Saúde Sudeste") %>%
#   select(id_da_corrida, tarifa_deflate, categoria, justificativa)

```

# Explorando os endereços

A Transparência Brasil fez um levantamento das rotas mais comuns com o intuito de descobrir se servidores estavam utilizando de maneira indevida o  serviço de tranporte particular, para se deslocar de ou para um endereço privado.

Para realizar este levantamento, verificamos a ocorrência de viagens com mesma rota (endereço de origem e destino) e consideramos aquelas que se repetiram mais de vinte vezes. Então, verificamos o que havia em cada endereço com o auxílio da ferramental Google Maps (mapas e satélites). 

Identificamos como "endereço privado" os logradouros que não correspondem ou não estaão próximos a um prédio público ou empresarial. Quando o logradouro não tinha número e a rua era extensa o suficiente de forma que não fosse possível afirmar que se tratava de um endereço comercial ou privado, identificamos o logradouro como NA. Quando o logradouro corresponde a um endereço público ou está a poucos metros de um prédio público, identificamos com o(s) serviço(s) correspondente(s) (ex: "Rua Líbero Badaró"). Por fim, quando o logradouro dizia respeito a um endereço comercial verificável voltado à pessoas jurídicas, identificamos aquele endereço com o nome da empresa correspondente.[5]

[5] Isso significa dizer que uma padaria ou um consultório médico não serão identificados como estabelecimentos comerciais, e aquele logradouro vai estar associado ou a um endereço privado ou a um serviço público próximo. 

### Viagens omitidas

De todas as viagens realizadas, cerca de 0,7% (2.388) possuem rotas (endereços de origem e destino) completamente omitidas porque ao menos um dos endereços corresponde a um domicílio [6], seja do servidor ou de um cidadão para quem foi prestado algum tipo de atendimento. A relação da quantidade de viagens omitidas por órgão está no anexo 4.

[6] estão omitidas de acordo com o inciso III do artigo 4o. do decreto municipal 53.623/2012.

Os destaques ficam para o Instituto de Previdência Municipal de São Paulo (IPREM) com 42,5% das suas viagens omitidas e para a Secretaria Municipal de Gestão (SMG), com cerca de 17% das viagens omitidas. A justificativa para essas viagens na maioria das vezes não é suficiente para entender a necessidade do transporte individual privado, mencionando apenas "Ida ao trabalho" ou "Retorno à residência". É necessária uma investigação mais aprofundada sobre essas corridas para entender se elas tinham alguma motivação razoável ou não. 



```{r, echo = F, eval = TRUE, warning=FALSE}

#viagens totalmente omitidas  #2388
# No documento final está como anexo Anexo 2 
anexo2 <- analise_taxis_final %>%
  mutate(viagem_omitida = ifelse(endereco_de_origem_sa == "omitido" | endereco_de_destino_sa == "omitido", 1, 0),
         num = 1) %>%
  group_by(empresa) %>%
  summarise(viagens_omitidas = sum(num[viagem_omitida == 1]),
            viagens_totais = sum(num),
            perc = paste0(round((viagens_omitidas/viagens_totais)*100, 2), "%"))  %>%
  filter(viagens_omitidas > 0)

# name_anexo2 <- c("Empresa",	"Qtde viagens omitidas",
#                  "Qtde total de viagens",	"Percentual viagens omitidas")
# 
# name_anexo2 <- snakecase::to_any_case(name_anexo2)
# name_anexo2 <- stri_trans_general(name_anexo2, "Latin-ASCII")
# names(anexo2) <- name_anexo2
# 
# write.csv(anexo2, file="Anexo 2 Viagens com rotas omitidas por orgao.csv", row.names = FALSE, fileEncoding = "UTF-8")
# setwd("C:/Users/coliv/Documents/Taxis/anexos formatados")
# 
# write.xlsx(as.data.frame(anexo2),
#              file="anexo2.xlsx", sheetName="anexo2",
#              col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

```

### Viagens informadas

Dentre as viagens informadas, foram verificadas 147 rotas (contabilizando 6.571 viagens) que ocorreram mais de vinte vezes e tinham como origem ou destino um endereço privado. Buscamos verificar se servidores utilizam o serviço de transporte individual como transporte privado entre a residência e o trabalho. Em 5 órgãos da Prefeitura de São Paulo, foram verificadas rotas que se repetem mais de cem vezes. A relação dessas viagens está na tabela abaixo [7]:

[7] Em respeito à privacidade dos servidores e tendo em vista o direito de defesa dos órgãos, optamos por divulgar apenas as ruas relacionadas à essas corridas. 


```{r, echo = F, eval = TRUE, warning=FALSE}

# Importando os destinos verificados
load("C:/Users/coliv/Documents/Taxis/destinos.Rdata")

possiveis_destinos <- analise_taxis_final %>%
  group_by(empresa, endereco_de_origem_sa, endereco_de_destino_sa ) %>%
  summarise(`Corridas` = n(),
            `KM média por corrida` = round(mean(odometro_km),0),
            primeira_viagem = min(data_origem),
            ultima_viagem = max(data_origem),
            valor_medio_corrida = round(mean(tarifa),2),
            valor_total = sum(tarifa_deflate)) %>%
  arrange(desc(`Corridas`)) %>%
  filter(`Corridas` > 20) %>%
  ungroup() %>%
  mutate(viagem_omitida = ifelse(endereco_de_origem_sa == "omitido" & endereco_de_destino_sa == "omitido", 1, 0)) %>%
  left_join(destinos, by=c("endereco_de_origem_sa" = "endereco")) %>%
  rename(provavel_origem = destino_relacionado_ao_endereco) %>%
  left_join(destinos, by=c("endereco_de_destino_sa" = "endereco")) %>%
  rename(provavel_destino = destino_relacionado_ao_endereco) %>%
  select(empresa, endereco_de_origem_sa, provavel_origem, endereco_de_destino_sa, provavel_destino,
         `Corridas`, `KM média por corrida`,valor_medio_corrida, valor_total, viagem_omitida, primeira_viagem, ultima_viagem)

#Tabela das rotas mais comuns

x <- possiveis_destinos %>%
  filter(`Corridas` > 100,
         viagem_omitida == 0,
         provavel_origem == "endereço privado" | provavel_destino == "endereço privado") %>%
  mutate(endereco_de_origem_sa = gsub("_", " ", endereco_de_origem_sa),
         endereco_de_destino_sa = gsub("_", " ", endereco_de_destino_sa)) %>%
  select(-c(viagem_omitida)) 

x %>%
  kable()

# No documento final ficou como Anexo 3: Rotas envolvendo endereços privados que ocorreram mais de vinte vezes
anexo3 <- possiveis_destinos %>%
  filter(viagem_omitida == 0,
         provavel_origem == "endereço privado" | provavel_destino == "endereço privado") %>%
  mutate(endereco_de_origem_sa = gsub("_", " ", endereco_de_origem_sa),
         endereco_de_destino_sa = gsub("_", " ", endereco_de_destino_sa)) %>%
  select(-c(viagem_omitida))

name_anexo3 <- c("Empresa",
                 "Endereço de origem",
                 "Provável origem",
                 "Endereço de destino",
                 "Provável destino",
                 "Qtd corridas para essa rota",
                 "Km rodados em média por corrida",
                 "Valor médio corrida",
                 "Valor total corridas",
                 "Data primeira viagem",
                 "Data última viagem")

name_anexo3 <- snakecase::to_any_case(name_anexo3)
name_anexo3 <- stri_trans_general(name_anexo3, "Latin-ASCII")
names(anexo3) <- name_anexo3
# 
# write.csv(anexo3, file="Anexo 3 Rotas envolvendo enderecos privados que ocorreram mais de vinte vezes.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.xlsx(as.data.frame(anexo3), file="Anexo 3 com valores.xlsx")

#Quantos órgãos possuem rotas com mais de 100 viagens?

# possiveis_destinos %>%
#   filter(`Corridas` > 100,
#          viagem_omitida == 0,
#          provavel_origem == "endereço privado" | provavel_destino == "endereço privado") %>%
#   group_by(empresa) %>%
#   summarise(corridas = n()) %>%
#   nrow()

#Quantas viagens tinham ao menos um endereço privado?
# 
# possiveis_destinos %>%
#   filter(viagem_omitida == 0,
#          provavel_origem == "endereço privado" | provavel_destino == "endereço privado") %>%
#   summarise(soma_viagens = sum(`Corridas`))

# x <- possiveis_destinos %>%
#   filter(provavel_origem == "endereço privado" | provavel_destino == "endereço privado") 
# 
# # Quais são as empresas com maior número de corridas envolvendo ao menos um endereço privado
# x %>%
#   group_by(empresa) %>%
#   summarise(corridas = sum(corridas)) %>%
#   arrange(desc(corridas))
# 
# y <- x %>%
#   filter(empresa == "Prefeitura do Município de São Paulo - Secretaria do Governo Municipal") %>%
#   arrange(desc(corridas)) 







```

A Secretaria do Governo Municipal é o órgão que possui o maior número de viagens envolvendo ao menos um endereço classificado como privado pela Transparência Brasil: 1872 corridas. O órgão possui 5 rotas que se repetem mais de 100 vezes e uma única viagem se repetiu em 206 dos 255 dias úteis de 2018. Há também o registro de 131 viagens que saem da rua Mourato Coelho, em Pinheiros, até o centro da cidade, e mais 118 viagens que fazem o caminho inverso.

A Secretaria Municipal da Fazenda registrou 6 rotas que ocorreram mais de 100 vezes, com destaque para 176 viagens que ocorreram entre a Rua Maria Fett, na Vila Graciosa zona leste de São Paulo, até o centro da cidade e outras 172 viagens que fizeram o caminho inverso. A Secretaria Municipal de Relações Internacionais, Secretaria Municipal do Verde e do Meio Ambiente e a Prefeitura Regional Itaim Paulista possuem 1 rota cada que ocorreu mais de cem vezes. 

A relação de todas as rotas que se repetiram mais de vinte vezes e que incluem algum endereço considerado por nós como privado está no anexo 5.

## Conclusões


```{r}
#Anexo 6
anexo6 <- analise_taxis_final %>%
   filter(tempo_viagem_min < 4,
         hora_final > hora_origem, 
         odometro_km < 1.4,
         odometro_km > 0) %>%
  select(-c(omitido, tempo_viagem, tempo_viagem_hora, endereco_de_destino_sa, endereco_de_origem_sa,
            data_origem, data_final, hora_da_solicitacao, origem, destino))

names(anexo6) <- c("Id da corrida", "Tarifa (valor corrente)", "Hora início", "Hora final", "Cidade de origem",
              "Categoria", "Plataforma de chamada", "Empresa", "Centro de custo", "Endereço de origem",
              "Endereço de destino", "Justificativa", "Tempo de viagem (minutos)", "Tarifa (valor constante)",
              "Odômetro (km)")

#Anexo 6

# setwd("C:/Users/coliv/Documents/Taxis")
# 
# library(xlsx)
# library(googledrive)
# 
# drive_find(n_max=10)
# 
# write.xlsx(as.data.frame(anexo6), 
#            file="anexo6.xlsx", sheetName="anexo6",
#            col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)
# 
# drive_upload(
#   "anexo6.xlsx",
#   path="~/TB/2019/Gnews - taxis",
#   name = "Anexo 6",
#   type = "spreadsheet")
# 
# anexo7 <- analise_taxis_final %>%
#   mutate(data_origem = as.Date(data_origem, format="%Y-%m-%d"),
#          data_final = as.Date(data_final, format="%Y-%m-%d"),
#          dias_viagem = data_final - data_origem) %>%
#   filter(tempo_viagem_hora > 24,
#          !id_da_corrida %in% c("122785126", "160810029", "159789088", "87961254633069" )) %>%
#   select(id_da_corrida, empresa, data_origem, data_final,endereco_de_origem,
#          endereco_de_destino, dias_viagem, justificativa , tarifa, tarifa_deflate) %>%
#   mutate(justificativa = gsub("\\+", " ", justificativa),
#          justificativa = tolower(justificativa),
#          dias_viagem = gsub("days", "dias", dias_viagem),
#          dias_viagem = paste0(dias_viagem, " dias"),
#          tarifa = paste0("R$ ", tarifa),
#          tarifa_deflate = paste0("R$ ", tarifa_deflate)) %>%
#   mutate(data_origem = format(data_origem, "%d/%m/%Y"),
#          data_final = format(data_final, "%d/%m/%Y")) %>%
#   arrange(desc(dias_viagem)) %>%
#   rename(`ID da corrida`= id_da_corrida,
#          `Órgão` = empresa,
#          `Duração viagem` = dias_viagem,
#          `Justificativa` = justificativa,
#          `Valor da corrida (corrente)` = tarifa,
#          `Valor da corrida ajustado`= tarifa_deflate,
#          `Data início`= data_origem,
#          `Data final` = data_final)
# 
# setwd("C:/Users/coliv/Documents/Taxis")
# 
# write.xlsx(as.data.frame(anexo7), 
#            file="anexo7.xlsx", sheetName="anexo7",
#            col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)
# 
# drive_upload(
#   "anexo7.xlsx",
#   path="~/TB/2019/Gnews - taxis",
#   name = "Anexo 7: Informações corridas mais de um dia",
#   type = "spreadsheet")

```

