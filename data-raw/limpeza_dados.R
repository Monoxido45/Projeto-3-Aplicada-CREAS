# script para dar upload dos dados do drive, limpar os dados e transforma-los em 
# arquivos rds
library(tidyverse)
library(googledrive)

# Importando os csvs das duas bases diretamente do drive ------------------
# autorizacao
drive_auth()
drive_find(n_max = 50)
# baixando os dados
drive_download("Censo_SUAS_2019_RH_CREAS_divulgação.csv",
                           overwrite = T, 
                           path = "data-raw/Censo_SUAS_2019_RH_CREAS_divulgação.csv")
drive_download("Censo_SUAS_2019_CREAS_Dados_Gerais_divulgacao.csv",
                        overwrite = T,
                      path = "data-raw/Censo_SUAS_2019_CREAS_Dados_Gerais_divulgacao.csv")

# limpando cada base:
# base das obs CREAS
dados_nao_limpos <- "data-raw/Censo_SUAS_2019_CREAS_Dados_Gerais_divulgacao.csv" |>
  read.csv(sep = ";")

