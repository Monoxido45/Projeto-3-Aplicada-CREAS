# script para dar upload dos dados do drive, limpar os dados e transforma-los em 
# arquivos rds
library(tidyverse)
library(googledrive)

# Importando os csvs das duas bases diretamente do drive ------------------
# autorizacao
drive_auth()
drive_find(n_max = 50)
# baixando os dados
drive_download("Censo_SUAS_2019_RH_CREAS_divulgação.xlsx",
                           overwrite = T, 
                           path = "data-raw/Censo_SUAS_2019_RH_CREAS_divulgação.xlsx")
drive_download("Censo_SUAS_2019_CREAS_Dados_Gerais_divulgacao.csv",
                        overwrite = T,
                      path = "data-raw/Censo_SUAS_2019_CREAS_Dados_Gerais_divulgacao.csv")

# limpando a base geral com oq queremos:
dados_nao_limpos_geral <- "data-raw/Censo_SUAS_2019_CREAS_Dados_Gerais_divulgacao.csv" |>
  read.csv(sep = ";")

# primeiro selecionando apenas as variaveis de interesse
dados_nao_limpos_geral <- dados_nao_limpos_geral |>
  select(c(IBGE:Q_completo), 
         q0_9, 
         q0_10,
         q0_15,
         q1,
         Latitude,
         Longitude,
         q2_1,
         q2_2,
         q3,
         c(q6_1:q6_10),
         c(q7_1:q7_4),
         q8_0,
         c(q9_1:q9_12),
         q10_1,
         q10_2,
         c(q11_1:q11_22),
         q12_1_0,
         q12_2_0,
         q12_3_0,
         q12_4_0,
         q12_5_0,
         q12_6_0,
         q12_7_0,
         q12_8_0,
         q12_9_0,
         q12_10_0,
         q12_11_0,
         q12_12_0,
         q12_13_0,
         q12_14_0,
         q12_15_0,
         q12_16_0,
         q12_17_0,
         q13_1_0,
         q13_2_0,
         q13_3_0,
         q14_1,
         q14_2,
         q15,
         q27,
         q30,
         q31,
         q33,
         q42,
         q44_0,
         q48_0,
         q49_0,
         q50_0,
         q55_0,
         q57)
    


# limpando, juntando variaveis, mudando tudo:
# primeiramente tirando algumas variaveis nao uteis
dados_nao_limpos_rh <- dados_nao_limpos_rh |>
  select(-c(d_58_8bin1, d_58_8bin2, d_58_8bin3,
            d_58_9bin1, d_58_9bin2, d_58_9bin3, d_58_9bin4, d_58_9bin5,
            d_58_10bin1, d_58_10bin2, d_58_10bin3, d_58_10bin4)) |>
  select(- c(d_58_14_1bin_Gestão:d_58_16_3bin_Outros))




