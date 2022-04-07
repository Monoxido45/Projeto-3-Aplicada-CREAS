# construindo alguns mapas que podem ser interessantes
library(tidyverse)
library(leaflet)
library(sf)
library(maps)
library(brazilmaps)

# creas
dados_creas <- "analise/dados_gerais_CREAS.rds" |>
  readRDS()
# dados do IBGE para cruzar
ibge <- "data-raw/RELATORIO_DTB_BRASIL_MUNICIPIO.ods" |>
readODS::read_ods()

dados_creas_mapa <- dados_creas |> 
  filter(tipo_creas != "Regional") |>
  mutate(IBGE7 = as.numeric(IBGE7)) |>
  inner_join(ibge |>
  select("Código Município Completo",
   "Região Geográfica Intermediária",)|>
    rename(MesoRegion = "Região Geográfica Intermediária",
    IBGE7 = "Código Município Completo"),
  by = "IBGE7")

bin_func <- function(var){
  case_when(var == "0" ~ 0,
            TRUE ~ 1)
}

# numero de creas por UF
state_map <- get_brmap(geo = "State",class = "sf")

state_map_sf <- state_map |>
  mutate(State = as.character(State)) |>
  st_as_sf() |>
  st_transform(4326)

# pegando as estatisticas
state_map_sf <- state_map_sf |>
  mutate(nome = stringi::stri_trans_general(nome, "Latin-ASCII")) |>
  left_join(dados_creas |>
              count(UF) |>
              rename(nome = UF) |>
              mutate(nome = toupper(nome),
                     nome = stringi::stri_trans_general(nome, "Latin-ASCII")) ,
            by = "nome")

# juntando os mapas de acordo com a media de servicos
ggplot(state_map_sf) +
  geom_sf(aes(geometry = geometry, fill = n))+
  geom_sf_label(aes(label = n),
                min.segment.length = 0,
                colour = "dodgerblue3",
                segment.colour = "blue")+
  scale_fill_distiller(palette = "Blues", direction = 1) +
  labs(fill = "Número de CREAS") +
  theme_void()
ggsave("num_UF.pdf",
       width = 10, height = 6,
       path = "analise/figuras")


# por regiao
region_map <- get_brmap(geo = "Region",class = "sf")
# um grafico inicial
plot_brmap(region_map)

region_map_sf <- region_map |>
  mutate(Region = as.character(Region)) |>
  st_as_sf() |>
  st_transform(4326)

region_map_sf <- region_map_sf |>
  mutate(desc_rg= stringi::stri_trans_general(desc_rg, "Latin-ASCII"),
         desc_rg = str_replace(desc_rg, "-", " ")) |>
  left_join(dados_creas |>
              mutate(across(violencia_fisica:priv_lib, bin_func)) |>
              rowwise() |>
              mutate(quantidade_servicos = 
                       sum(c_across(violencia_fisica:priv_lib))) |>  
              mutate(Regiao = str_remove(Regiao, "Região ")) |>
              group_by(Regiao) |>
              summarise(servicos_medio = mean(quantidade_servicos)) |>
              rename(desc_rg = Regiao) |>
              mutate(desc_rg = toupper(desc_rg),
                     desc_rg = stringi::stri_trans_general(desc_rg, "Latin-ASCII")) ,
            by = "desc_rg")


p1 <- ggplot(region_map_sf) +
  geom_sf(aes(geometry = geometry, fill = servicos_medio))+
  scale_fill_distiller(palette = "Blues", direction = 1, limits = c(8, 14)) + 
  labs(fill = "Média de serviços") +
  theme_void()


# pelos estados
state_map <- get_brmap(geo = "State",class = "sf")
# um grafico inicial
plot_brmap(state_map)

state_map_sf <- state_map |>
  mutate(State = as.character(State)) |>
  st_as_sf() |>
  st_transform(4326)

# pegando as estatisticas
state_map_sf <- state_map_sf |>
  mutate(nome = stringi::stri_trans_general(nome, "Latin-ASCII")) |>
  left_join(dados_creas |>
              mutate(across(violencia_fisica:priv_lib, bin_func)) |>
              rowwise() |>
              mutate(quantidade_servicos = 
                       sum(c_across(violencia_fisica:priv_lib))) |>  group_by(UF) |>
              summarise(servicos_medio = mean(quantidade_servicos)) |>
              rename(nome = UF) |>
              mutate(nome = toupper(nome),
                     nome = stringi::stri_trans_general(nome, "Latin-ASCII")) ,
            by = "nome")

# juntando os mapas de acordo com a media de servicos
p2 <- ggplot(state_map_sf) +
  geom_sf(aes(geometry = geometry, fill = servicos_medio))+
  scale_fill_distiller(palette = "Blues", direction = 1, limits = c(8, 14)) + 
  labs(fill = "Média de serviços") +
  theme_void()

# por alguns mesoregiao
mesoregiao_map <- get_brmap(geo = "MesoRegion",class = "sf")

mesoregiao_map_sf <- mesoregiao_map |>
  mutate(State = as.character(State)) |>
  st_as_sf() |>
  st_transform(4326)


mesoregiao_map_sf <- mesoregiao_map_sf |>
  left_join(dados_creas_mapa |>
              mutate(across(violencia_fisica:priv_lib, bin_func)) |>
              rowwise() |>
              mutate(quantidade_servicos = 
                       sum(c_across(violencia_fisica:priv_lib))) |> 
              group_by(MesoRegion) |>
              summarise(servicos_medio = mean(quantidade_servicos)),
            by = "MesoRegion")


p3 <- ggplot(mesoregiao_map_sf) +
  geom_sf(aes(geometry = geometry, fill = servicos_medio))+
  scale_fill_distiller(palette = "Blues", direction = 1, limits = c(8, 14)) + 
  labs(fill = "Média de serviços") +
  theme_void()


ggpubr::ggarrange(p1, p2, p3, common.legend = T, nrow = 1)
ggsave("mapas_servicos.pdf",
       width = 10, height = 6,
       path = "analise/figuras")

# para ano de criacao
region_map_sf <- region_map |>
  mutate(Region = as.character(Region)) |>
  st_as_sf() |>
  st_transform(4326)

region_map_sf <- region_map_sf |>
  mutate(desc_rg= stringi::stri_trans_general(desc_rg, "Latin-ASCII"),
         desc_rg = str_replace(desc_rg, "-", " ")) |>
  left_join(dados_creas |>
              mutate(ano = data_creas |> lubridate::dmy() |>
                       lubridate::year()) |> 
              mutate(Regiao = str_remove(Regiao, "Região ")) |>
              group_by(Regiao) |>
              summarise(ano_medio = mean(ano)) |>
              rename(desc_rg = Regiao) |>
              mutate(desc_rg = toupper(desc_rg),
                     desc_rg = stringi::stri_trans_general(desc_rg, "Latin-ASCII")) ,
            by = "desc_rg")

p1 <- ggplot(region_map_sf) +
  geom_sf(aes(geometry = geometry, fill = ano_medio))+
  scale_fill_distiller(palette = "Oranges", direction = 1, limits = c(2003, 2015),
                       breaks = scales::pretty_breaks(3)) + 
  labs(fill = "Ano de fundação médio") +
  theme_void()

state_map_sf <- state_map |>
  mutate(State = as.character(State)) |>
  st_as_sf() |>
  st_transform(4326)

state_map_sf <- state_map_sf |>
  mutate(nome = stringi::stri_trans_general(nome, "Latin-ASCII")) |>
  left_join(dados_creas |>
              mutate(ano = data_creas |> lubridate::dmy() |>
                       lubridate::year()) |>
              group_by(UF) |>
              summarise(ano_medio = mean(ano)) |>
              rename(nome = UF) |>
              mutate(nome = toupper(nome),
                     nome = stringi::stri_trans_general(nome, "Latin-ASCII")) ,
            by = "nome")

p2 <- ggplot(state_map_sf) +
  geom_sf(aes(geometry = geometry, fill = ano_medio))+
  scale_fill_distiller(palette = "Oranges", direction = 1, limits = c(2003, 2015),
                       breaks = scales::pretty_breaks(3)) + 
  labs(fill = "Ano de fundação médio") +
  theme_void()


mesoregiao_map <- get_brmap(geo = "MesoRegion",class = "sf")

mesoregiao_map_sf <- mesoregiao_map |>
  mutate(State = as.character(State)) |>
  st_as_sf() |>
  st_transform(4326)


mesoregiao_map_sf <- mesoregiao_map_sf |>
  left_join(dados_creas_mapa |>
              mutate(ano = data_creas |> lubridate::dmy() |>
                       lubridate::year())|>
              rowwise() |>
              group_by(MesoRegion) |>
              summarise(ano_medio = mean(ano)),
            by = "MesoRegion")


p3 <- ggplot(mesoregiao_map_sf) +
  geom_sf(aes(geometry = geometry, fill = ano_medio))+
  scale_fill_distiller(palette = "Oranges", direction = 1, limits = c(2003, 2015),
                       breaks = scales::pretty_breaks(3)) + 
  labs(fill = "Ano de fundação médio") +
  theme_void()

ggpubr::ggarrange(p1, p2, p3, nrow = 1, common.legend = T)
ggsave("mapas_ano_fund.pdf",
       width = 10, height = 6,
       path = "analise/figuras")
