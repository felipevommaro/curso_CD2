# Instale os pacotes caso não os tenha:
# install.packages(c("ggplot2", "ggthemes", "sf", "ggiraph", "dplyr", "patchwork", "htmltools"))
# install.packages("geobr") # Essencial para mapas do Brasil

library(ggplot2)
library(ggthemes)
library(sf)
library(ggiraph)
library(dplyr)
library(patchwork) # Necessário para o inset_element() e plot_layout()
library(geobr)
library(htmltools)
library(arrow)
######### RODR

linkage_sim_sinasc <- read_parquet("dadosR/base_processada.parquet")


#### arquivos espaciais
bairros_2022.sf <- read_sf('dadosR/bases_territoriais_2022.gpkg', layer = 'bairros')
ap_2022.sf <- read_sf('dadosR/bases_territoriais_2022.gpkg', layer = 'ap')



# 1. OBTER O MAPA DOS BAIRROS DO RIO DE JANEIRO
# Vamos baixar os bairros do estado do RJ e filtrar apenas o município do Rio
# atlas <- read_neighborhood(year = 2010, showProgress = FALSE) |>
#   filter(name_muni == "Rio De Janeiro")

quebras_ids <- c(0.4, 0.5, 0.6, 0.7, 0.8, 1)

#### bairros e IDS 
IDS <- read_sf("ids_2010_bairros.geojson") 

IDS <- IDS %>%
select(codbairro, bairro, ids)





# 3. PREPARAÇÃO DOS DADOS (Agrupamento e cálculos)
# --- 3. PREPARAÇÃO DOS DADOS ---

# 1. Definindo as suas quebras

quebras_ids <- c(0.4, 0.5, 0.6, 0.7, 0.8, 1)

# 2. Preparando os dados para o gráfico de barras
pop_ids <- IDS |>
  st_drop_geometry() |>
  mutate(
    # Usando a variável 'ids' DIRETAMENTE, sem o get()
    group_ids = findInterval(ids, quebras_ids, left.open = FALSE),
    group_ids = factor(group_ids)
  ) |>
  group_by(group_ids) |>
  # Usa n() para contar o número de polígonos em cada faixa. 
  # Se você tiver uma coluna de população, troque n() por: sum(nome_da_coluna, na.rm=TRUE)
  summarise(score = n()) |> 
  ungroup() |>
  mutate(share = score / sum(score) * 100) |>
  na.omit() |>
  mutate(
    # Ajuste para posicionar o texto da porcentagem (dentro ou fora da barra)
    y_text = if_else(as.numeric(group_ids) %in% c(1, max(as.numeric(group_ids))), share + 3, share - 3),
    label = paste0(round(share, 1), "%"),
    data_id = as.character(group_ids) 
  )

# 3. Adicionando o grupo de volta ao mapa original
IDS <- IDS |>
  mutate(group_ids = findInterval(ids, quebras_ids, left.open = FALSE))


# --- 4. CRIANDO O MAPA INTERATIVO (pmap) ---
pmap <- ggplot(IDS) +
  geom_sf_interactive(
    aes(
      fill = ids, # Usando a variável 'ids' diretamente aqui também
      data_id = group_ids,
      tooltip = paste("IDS:", round(ids, 3)) 
    ),
    lwd = 0.05,
    color = "white"
  ) +
  scale_fill_fermenter(
    name = "IDS",
    breaks = quebras_ids,
    direction = 1,
    palette = "YlGnBu" 
  ) +
  labs(
    title = "Mapa IDS",
    subtitle = "Índice de Desenvolvimento Social por bairro",
    caption = "Fonte: Dados Próprios"
  ) +
  theme_map() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

htmltools::save_html(interactive_plot, "mapa_IDS_interativo.html")





# # 4. CRIANDO O MAPA (pmap)
# pmap <- ggplot(atlas) +
#   geom_sf_interactive(aes(fill = HDI, data_id = group_hdi, tooltip = paste("Bairro:", name_neighborhood, "<br>HDI:", round(HDI, 3))), 
#                       lwd = 0.05, color = "white") +
#   scale_fill_fermenter(
#     name = "",
#     breaks = seq(0.65, 0.95, 0.05),
#     direction = 1,
#     palette = "YlGnBu"
#   ) +
#   labs(
#     title = "IDH nos Bairros do Rio de Janeiro",
#     subtitle = "Município do Rio de Janeiro (Dados Simulados)",
#     caption = "Geometria: geobr | Dados: Simulados"
#   ) +
#   theme_map() +
#   theme(
#     legend.position = "none",
#     plot.title = element_text(size = 16, hjust = 0.5),
#     plot.subtitle = element_text(hjust = 0.5)
#   )
# 
# # 5. CRIANDO O GRÁFICO DE BARRAS (pcol)
# x_labels <- c(
#   "0.650 or less", "0.650 to 0.699", "0.700 to 0.749", "0.750 to 0.799",
#   "0.800 to 0.849", "0.850 to 0.899", "0.900 to 0.949", "0.950 or more"
# )
# 
# pcol <- ggplot(pop_hdi, aes(group_hdi, share, fill = group_hdi)) +
#   geom_col_interactive(aes(data_id = data_id, tooltip = paste("Share:", label))) +
#   geom_hline(yintercept = 0) +
#   geom_text_interactive(
#     aes(y = y_text, label = label, color = group_hdi, data_id = data_id),
#     size = 2.5
#   ) +
#   coord_flip() +
#   scale_x_discrete(labels = x_labels) +
#   scale_fill_brewer(palette = "YlGnBu") +
#   scale_color_manual(values = c(rep("black", 5), rep("white", 2), "black")) +
#   guides(fill = "none", color = "none") +
#   labs(title = "", x = NULL, y = NULL) +
#   theme_void() +
#   theme(
#     panel.grid = element_blank(),
#     plot.title = element_text(size = 8),
#     axis.text.y = element_text(size = 6),
#     axis.text.x = element_blank(),
#     aspect.ratio = 1.5
#   )
# 
# # 6. JUNTANDO OS GRÁFICOS (Patchwork)
# # Inserindo o gráfico de barras dentro do mapa, no canto inferior direito
# p_hdi_atlas <- pmap + inset_element(pcol, left = 0.5, bottom = 0, right = 1, top = 0.5)
# 
# # 7. GERANDO A INTERATIVIDADE HTML (ggiraph)
# interactive_plot <- girafe(
#   ggobj = p_hdi_atlas,
#   options = list(
#     opts_hover(css = "fill:orange; stroke:black; stroke-width:0.5px;"),
#     opts_hover_inv(css = "opacity:0.3;"),
#     opts_selection(type = "single", only_shiny = FALSE)
#   )
# )
# 
# # Renderizar no RStudio Viewer
# interactive_plot
# 
# # Salvar como HTML
# htmltools::save_html(interactive_plot, "mapa_rio_interativo.html")



#################### gt summary do linkage

library(bannerCommenter)
library(foreign,quietly = TRUE)
library(descr, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(ggpubr,quietly = TRUE)
library(kableExtra,quietly = TRUE)
library(readxl, quietly = TRUE)
library(viridis, quietly = TRUE)
library(plotly, quietly = TRUE)
library(aweek)
library(gtsummary)
library(broom)
library(corrplot)
library(RColorBrewer)
library(MuMIn)
library(DT)
library(flextable)
library(officer)


tema_val_menor <- theme_light() +
  theme(axis.text.x = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 1, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .3, vjust = .5, face = "plain"),
        legend.text=element_text(size=10),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

theme_gtsummary_compact()

cols_tb <- c("#71c7ec","#005073","#107dac","#189ad3","#064273","#76b6c4","#7fcdff","#1da2d8","#1ebbd7","#bbeeff","#03396c")

banner("Script: Linkage mortalidade infantil - anos 2022 e 2023", "Autor(es): Valeria Saraceni","Data de atualização: 10/04/2025","Descrição: Descritivo do linkage SIM - SINASC para mortalidade infantil", emph = FALSE,center = FALSE)


## Bases de cada ano -----
# Wagner 2022
load("Z:/Agravos/nascimentos/01_bases/sim_sinasc_consolidada_fim2.RData")

# Preparando a base do Wagner
link <- sim_sinasc_consolidada_fim3

names(link)
count(link)

# # passando para uppercase
link <- link %>% 
  rename_all(toupper)

# juntar variaveis faltantes
load("Z:/Agravos/nascimentos/01_bases/dn22_link.RData") 

link <-  link %>% 
  left_join(dn22, by = c("NUMERODN" = "NUMERODN"))
names(link)

link2022 <- link 

# preparando data de nascimento como data
link2022$DTNASC <- ymd(link2022$DTNASC)
link2022$DTNASC <- format(link2022$DTNASC, format = "%Y-%m-%d")
link2022$anonasc <- year(link2022$DTNASC)
link2022$mesnasc <- month(link2022$DTNASC)

# preparando data de obito como data
link2022$DTOBITO <- ymd(link2022$DTOBITO)
link2022$DTOBITO <- format(link2022$DTOBITO, format = "%Y-%m-%d")
link2022$anoOBITO <- year(link2022$DTOBITO)
link2022$mesOBITO <- month(link2022$DTOBITO)

# calculando idade no óbito
link2022$idade <- floor(as.numeric(difftime(link2022$DTOBITO, link2022$DTNASC, units = "days")))
summary(link2022$idade)

# corrigindo um erro de data de nascimento
link2022$idade[link2022$idade < 0] <- 0

# primeiro separar os residentes, NV de 2022, óbitos até 364 dias de vida
link2022 <- filter(link2022,link2022$CODMUNRES=="330455")
link2022 <- link2022 %>% 
  filter(anonasc == 2022)

# marcando os obitos
link2022 <- link2022 %>% 
  mutate(obinfantil = case_when((!is.na(DTOBITO) ~ 1), TRUE ~ 0))

# desmarcando os obitos ocorridos após 364 dias de vida
link2022 <- link2022 %>% 
  mutate(obinfantil = ifelse(idade > 364 & obinfantil != 0, 0, obinfantil))

link2022$ob_inf_sn <- link2022$obinfantil
link2022$ob_inf_sn <- factor(link2022$ob_inf_sn, labels = c("Não","Sim"))

# Causabas vazia <- NA
link2022$CAUSABAS[link2022$CAUSABAS==""] <- NA

count(link2022)

# Wagner 2023
load("Z:/Agravos/nascimentos/01_bases/sim_sinasc_consolidada_fim2.RData")

# Preparando a base do Wagner 
link1 <- sim_sinasc_consolidada_fim3
names(link1)
count(link1)

# # passando para uppercase
link1 <- link1 %>% 
  rename_all(toupper)

names(link1)
count(link1)

# juntar variaveis faltantes
load("Z:/Agravos/nascimentos/01_bases/dn23_link.RData") 

link1 <-  link1 %>% 
  left_join(dn23, by = c("NUMERODN" = "NUMERODN"))
names(link1)

link2023 <- link1 

# preparando data de nascimento como data
link2023$DTNASC <- ymd(link2023$DTNASC)
link2023$DTNASC <- format(link2023$DTNASC, format = "%Y-%m-%d")
link2023$anonasc <- year(link2023$DTNASC)
link2023$mesnasc <- month(link2023$DTNASC)

# preparando data de obito como data
link2023$DTOBITO <- ymd(link2023$DTOBITO)
link2023$DTOBITO <- format(link2023$DTOBITO, format = "%Y-%m-%d")
link2023$anoOBITO <- year(link2023$DTOBITO)
link2023$mesOBITO <- month(link2023$DTOBITO)

# dropando um erro de linkage data de nascimento > data de obito
link2023 <- link2023 %>% 
  filter(NUMERODN != "73211443")
link2023 <- link2023 %>% 
  filter(NUMERODN != "74594403")
link2023 <- link2023 %>% 
  filter(NUMERODN != "76108051")

# calculando idade no óbito em dias
link2023$idade <- floor(as.numeric(difftime(link2023$DTOBITO, link2023$DTNASC, units = "days")))
summary(link2023$idade)

# primeiro separar os residentes, NV de 2023, óbitos até 364 dias de vida
link2023 <- filter(link2023,link2023$CODMUNRES=="330455")
link2023 <- link2023 %>% 
  filter(anonasc == 2023)

# marcando os obitos ----
link2023 <- link2023 %>% 
  mutate(obinfantil = case_when((!is.na(DTOBITO) ~ 1), TRUE ~ 0))

# desmarcando os obitos ocorridos após 364 dias de vida
link2023 <- link2023 %>% 
  mutate(obinfantil = ifelse(idade > 364 & obinfantil != 0, 0, obinfantil))
link2023$ob_inf_sn <- link2023$obinfantil
link2023$ob_inf_sn <- factor(link2023$ob_inf_sn, labels = c("Não","Sim"))

count(link2023)

# juntando as duas bases 2022 e 2023 -----
link2022 <- rbind(link2022, link2023)

## acertando variaveis -----
link2022$CODESTAB <- ifelse(is.na(link2022$CODESTAB),'',link2022$CODESTAB)
link2022$ESTCIVMAE <- ifelse(is.na(link2022$ESTCIVMAE),'',link2022$ESTCIVMAE)
link2022$ESTCIVMAE[link2022$ESTCIVMAE==""] <- 9
link2022$ESCMAE <- ifelse(is.na(link2022$ESCMAE),'9',link2022$ESCMAE)
link2022$ESCMAE[link2022$ESCMAE=='0'] <- '9'
link2022$ESCMAE[link2022$ESCMAE=='1'] <- "2"
link2022$GESTACAO <- ifelse(is.na(link2022$GESTACAO),'',link2022$GESTACAO)
link2022$GESTACAO[link2022$GESTACAO==''] <- "9"
link2022$GESTACAO[link2022$GESTACAO=='1'] <- "2"
link2022$GESTACAO[link2022$GESTACAO=='6'] <- "5"
link2022$GRAVIDEZ <- ifelse(is.na(link2022$GRAVIDEZ),'9',link2022$GRAVIDEZ)
link2022$PARTO <- ifelse(is.na(link2022$PARTO),'9',link2022$PARTO)
link2022$PARTO[link2022$PARTO==''] <- "9"
link2022$RACACORMAE <- ifelse(is.na(link2022$RACACORMAE),'9',link2022$RACACORMAE)
link2022$APGAR1 <- ifelse(is.na(link2022$APGAR1),'',link2022$APGAR1)
link2022$APGAR5 <- ifelse(is.na(link2022$APGAR5),'',link2022$APGAR5)
link2022$MESPRENAT <- ifelse(is.na(link2022$MESPRENAT),'',link2022$MESPRENAT)
link2022$MESPRENAT[link2022$MESPRENAT==99] <- '' 
link2022$CONSPRENAT[link2022$CONSPRENAT==99] <- ''

# transformando o peso em numerico
link2022$PESO <- as.numeric(as.character(link2022$PESO))
link2022$IDADEMAE <- as.numeric(as.character(link2022$IDADEMAE))

# transformando mesprenat em numerico
link2022$MESPRENAT <- as.numeric(as.character(link2022$MESPRENAT))
link2022$CONSPRENAT <- as.numeric(as.character(link2022$CONSPRENAT)) 
link2022$SEMAGESTAC <- as.numeric(as.character(link2022$SEMAGESTAC)) 

# inicio precoce do prenatal
link2022 <- link2022 %>% 
  mutate(ini_pre = case_when((!is.na(MESPRENAT) & (MESPRENAT < 4) ~ 0), TRUE ~ 1))
link2022$ini_pre_sn <- link2022$ini_pre

# Prematuridade - criando variavel
link2022$prematuro <- link2022$GESTACAO
link2022$prematuro[link2022$prematuro=="2"] <- "1"
link2022$prematuro[link2022$prematuro=="3"] <- "1"
link2022$prematuro[link2022$prematuro=="4"] <- "1"
link2022$prematuro[link2022$prematuro=="5"] <- "0"
link2022$prematuro[link2022$prematuro=="6"] <- "0"
link2022$prematuro[link2022$prematuro=="9"] <- "0"

# prematuro < 32 semanas
link2022$prem32 <- link2022$GESTACAO
link2022$prem32[link2022$prem32=="2"] <- "1"
link2022$prem32[link2022$prem32=="3"] <- "1"
link2022$prem32[link2022$prem32=="4"] <- "0"
link2022$prem32[link2022$prem32=="5"] <- "0"
link2022$prem32[link2022$prem32=="6"] <- "0"
link2022$prem32[link2022$prem32=="9"] <- "0"

link2022$prem32_sn <- link2022$prem32

# asfixia ao nascer ----
link2022$APGAR5 <- as.numeric(link2022$APGAR5)
link2022 <- link2022 %>% 
  mutate(asfixia = case_when((!is.na(APGAR5) & (APGAR5 < 7) ~ 1), TRUE ~ 0))
link2022$asfixia_sn <- link2022$asfixia

# colocando labels em fatores
link2022$ESTCIVMAE <- factor(link2022$ESTCIVMAE,labels = c("Solteira","Casada","Viúva","Separada","União estável","Ignorado"))
link2022$ESCMAE <- factor(link2022$ESCMAE, labels = c("Até 3 anos","4 a 7 anos","8 a 11 anos","12 anos e mais","Ignorada"))
link2022$SEXO <- factor(link2022$SEXO, labels = c("Feminino","Ignorado","Masculino"))
link2022$PARTO <- factor(link2022$PARTO, labels = c("Vaginal","Cesáreo","Ignorado"))
link2022$GESTACAO <- factor(link2022$GESTACAO, labels = c("< 28 semanas","28-31 semanas","32-36 semanas",">= 37 semanas","Ignorada"))
link2022$GRAVIDEZ <- factor(link2022$GRAVIDEZ, labels = c("Única", "Dupla", "Tripla ou +", "Ignorado"))
link2022$RACACORMAE <- factor(link2022$RACACORMAE, labels = c("Branca","Preta","Amarela","Parda","Indigena","Ignorada"))
link2022$CONSULTAS <- factor(link2022$CONSULTAS, labels = c("Nenhuma","1-3","4-6",">= 7","Ignorado"))
link2022$prematuro <- factor(link2022$prematuro, labels = c("Não","Sim"))
link2022$prem32_sn <- factor(link2022$prem32_sn,labels = c("Não","Sim"))
link2022$asfixia_sn <- factor(link2022$asfixia_sn,labels = c("Não","Sim"))
link2022$ini_pre_sn <- factor(link2022$ini_pre_sn,labels = c("Sim","Não"))

# faixa etaria materna
link2022$fetarmae <- cut(link2022$IDADEMAE,breaks = c(0,19,34,Inf),right = TRUE)
link2022$fetarmae <- factor(link2022$fetarmae,labels = c("<20 anos","20-34 anos",">=35 anos"))

# divisao adolescentes
link2022$faixa_adol <- cut(link2022$IDADEMAE,breaks = c(0,14,19,34,Inf),right = TRUE)
link2022$faixa_adol <- factor(link2022$faixa_adol,labels = c("10-14 anos","15-19 anos","20-34 anos",">=35 anos"))

# mae adolescente < 20
link2022 <- link2022 %>% 
  mutate(mae_adol = case_when(((IDADEMAE < 20) ~ 1), TRUE ~ 0))
link2022$mae_adol_sn <- link2022$mae_adol
link2022$mae_adol_sn <- factor(link2022$mae_adol_sn,labels = c("Não","Sim"))

# raca cor branca e não branca
link2022 <- link2022 %>% 
  mutate(branca_sn = case_when(((RACACORMAE != "Branca") ~ 1), TRUE ~ 0))
link2022$branca_sn <- factor(link2022$branca_sn,labels = c("Branca","Outra"))

# peso geral
link2022$peso_g <- cut(link2022$PESO,breaks = c(0,1499,2499,Inf),right = TRUE)
link2022$peso_g <- factor(link2022$peso_g, labels = c("< 1.500","1.500-2.499",">= 2.500"))

# peso ao nascer - baixo peso
link2022$baixopeso <- cut(link2022$PESO,breaks = c(0,2499,Inf),right = TRUE)
link2022$baixopeso <- factor(link2022$baixopeso,labels = c("Sim","Não"))

# peso ao nascer - muito baixo peso
link2022$muitobaixopeso <- cut(link2022$PESO,breaks = c(0,1499,Inf),right = TRUE)
link2022$muitobaixopeso <- factor(link2022$muitobaixopeso,labels = c("Sim","Não"))

# Ameaça à vida (Silva et al.) -----
# Peso < 1500, IG < 32 sem, Apgar 5 < 7
link2022 <- link2022 %>% 
  mutate(ameaca = case_when(((muitobaixopeso=="Sim" | prem32=="Sim" | asfixia=="Sim") ~ 1),
                            TRUE  ~ 0))

link2022$ameaca_sn <- link2022$ameaca
link2022$ameaca_sn <- factor(link2022$ameaca_sn, labels = c("Não","Sim"))


## faixa etaria infantil -----
link2022$Componente <- cut(link2022$idade,breaks = c(0,7,28,365),right=FALSE)
link2022$Componente <- factor(link2022$Componente, labels = c("Neonatal precoce","Neonatal tardio","Pos-neonatal"))



## carregando a tabela de AP do SINASC -----
ap <- read.csv2("Z:/Agravos/nascimentos/03_csv/tab_bai_ap.csv", header = TRUE, sep = ";",encoding = "UTF-8")

# trocando CODBAIRES para numerico
link2022$CODBAIRES <- as.numeric(link2022$CODBAIRES)

# juntando a tabela de AP
link2022 <- link2022 %>% 
  left_join(ap,by="CODBAIRES")


## Carregando a tabela de CID ----
link2022$cid3d <- substring(link2022$CAUSABAS,1,3)

### CID-10 ----
cid10 <- read.csv2("Z:/Agravos/mortalidade/03_csv/cid10.csv", header = TRUE, sep = ";",encoding = "UTF-8")
# juntando a tabela de CID
link2022 <- link2022 %>% 
  left_join(cid10,by="CAUSABAS")

### CID 3D----
cid103d <- read.csv2("Z:/Agravos/mortalidade/03_csv/cid103d.csv", header = TRUE, sep = ";",encoding = "UTF-8")
# juntando
link2022 <- link2022 %>% 
  left_join(cid103d,by="cid3d")

link2022$cap_cid <- substring(link2022$CAUSABAS,1,1) # separando capitulo CID pela letra



# Trabalhando com grupo da CID -----

library(janitor)
cid_grupo <-  read.csv("Z:/Agravos/mortalidade/03_csv/CID-10-GRUPOS-NEW.CSV",header=TRUE, sep = ";",encoding = "UTF-8")
cid_grupo <- cid_grupo %>% 
  janitor::remove_empty()

acha_cid <- function(cid3d) {
  if(is.na(cid3d) || cid3d == "" || cid3d == " ") return("Ignorado")
  resposta <- cid3d >= cid_grupo$CATINIC & cid3d <= cid_grupo$CATFIM
  as.character(cid_grupo[resposta,4])
}

link2022$grupo <- link2022$cid3d %>% map_chr(acha_cid)

## Evitabilidade -----
### 4 digitos ou CAUSABAS ------
causa_evit_causabas <-  read.csv2("Z:/Agravos/mortalidade/03_csv/evit_infantil_lana.csv",header=TRUE, sep = ";",encoding = "latin1")

link2 <- link2022 %>% 
  left_join(causa_evit_causabas,by=("CAUSABAS"))


link3 <- link2 %>% 
  mutate(acao = ifelse(obinfantil == 0 , NA, acao))

link3 <- link3 %>% 
  mutate(acao = ifelse(obinfantil == 1  & is.na(acao), 8, acao))

link3$acao <- factor(link3$acao, labels = c("Imunização","Pré-natal","Parto","Feto e recém-nascido","Diagnóstico e tratamento","Promoção","Causas mal definidas","Demais causas"))

# criando uma variavel ordenada para ap poder ter level
link3$ap1 <- link3$ap
link3$ap1[link3$ap1=="1.0"] <- "0"
link3$ap1[link3$ap1=="2.1"] <- "1"
link3$ap1[link3$ap1=="2.2"] <- "2"
link3$ap1[link3$ap1=="3.1"] <- "3"
link3$ap1[link3$ap1=="3.2"] <- "4"
link3$ap1[link3$ap1=="3.3"] <- "5"
link3$ap1[link3$ap1=="4.0"] <- "6"
link3$ap1[link3$ap1=="5.1"] <- "7"
link3$ap1[link3$ap1=="5.2"] <- "8"
link3$ap1[link3$ap1=="5.3"] <- "9"

link3$ap1 <- factor(link3$ap1,labels = c("1.0","2.1","2.2","3.1","3.2","3.3","4.0","5.1","5.2","5.3"))


# Tabelas ----
### NV vs. obitos ----
theme_gtsummary_language("pt", decimal.mark = ",", big.mark = ".")

link4 <- link3 %>% 
  select(ob_inf_sn,fetarmae,RACACORMAE,ESCMAE,ESTCIVMAE,ini_pre_sn,CONSULTAS,GESTACAO,GRAVIDEZ,PARTO,peso_g,
         asfixia_sn,ameaca_sn,ap)

tabela_nv_obitos <- link4 %>% 
  tbl_summary(by = ob_inf_sn,digits = all_categorical() ~ c(0,1),
              label = list(fetarmae ~ "Faixa etária",
                           ESCMAE ~ "Escolaridade",
                           RACACORMAE ~ "Raça/Cor da mãe",
                           ESTCIVMAE ~ "Estado civil",
                           ini_pre_sn ~ "Início precoce do pré-natal",
                           CONSULTAS ~ "Nº de consultas",
                           PARTO ~ "Tipo de parto",
                           GRAVIDEZ ~ "Tipo de gravidez",
                           GESTACAO ~ "Idade gestacional",
                           peso_g ~"Peso ao nascer (g)",
                           asfixia_sn ~ "Asfixia ao nascer",
                           ameaca_sn ~ "Ameaça à vida ao nascer",
                           ap ~ "AP de residência"),
              missing_text = "Ignorada") %>%
  modify_header(label ~ "**Características**") %>%
  bold_labels() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Óbitos infantis entre NV de 2022 e 2023**") %>%
  add_p(pvalue_fun = function(x) style_pvalue(x,digits=3))

tabela_nv_obitos

# tabela_nv_obitos %>%
#   as_flex_table() %>% 
#   save_as_docx(path = "Z:/agravos/nascimentos/05_figuras/tabela_nv_obitos.docx")

# se quiser abrir no word
write.table(tabela_nv_obitos, "Z:/agravos/nascimentos/05_figuras/tabela_nv_obitos_2023.csv", row.names= F, sep = "|",fileEncoding = "UTF-8")

# para abrir a tabela salva anteriormente
tab_nv_obitos <- read_delim("Z:/agravos/nascimentos/05_figuras/tabela_nv_obitos_2023.csv", delim = "|", trim_ws = TRUE)

# 2. Criar uma tabela formatada
tab_flex <- flextable(tab_nv_obitos)
# (Opcional) Melhorar visualmente
tab_flex <- tab_flex %>%
  autofit() %>%
  theme_vanilla() %>%
  set_caption("Tabela de Resultados - GTsummary") 
# 3. Exportar para Word
save_as_docx(tab_flex, path = "Z:/agravos/nascimentos/05_figuras/tab_nv_obitos2023.docx")