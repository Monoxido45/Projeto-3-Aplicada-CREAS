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
limpa_base <- function(dados_nao_limpos_geral){
# funcoes auxiliares para categorizacoes e unificacoes
# para a variavel de violencia
muda_q <- function(qs){
  bools <- c(qs[1] == "Sim", 
             qs[2] == "Sim", 
             qs[3] == "Sim", 
             qs[4] == "Sim")
  return(as.character(sum(bools)))
}
# para as salas
muda_salas <- function(q6){
  case_when(q6 == 0 ~ "0",
            q6 == 1 ~ "1",
            q6 >= 2 ~ "2")
}


# primeiro selecionando apenas as variaveis de interesse
# mudando alguma variaveis pegando variaveis de violencia fisica
dados_nao_limpos_geral <- dados_nao_limpos_geral |>
  select(c(IBGE:Q_completo), 
         q0_15,
         q1,
         q2_1,
         q2_2,
         q3,
         c(q6_1:q6_10),
         c(q7_1:q7_4),
         q8_0,
         c(q9_1:q9_12),
         q10_1,
         q10_2,
         c(q12_1_1:q12_17_0),
         q42,
         q57) |>
  rowwise() |>
  mutate(
    data_creas = q0_15,
    tipo_creas = q1,
    funcionamento_dias = q2_1,
    funcionamento_horas = q2_2,
    imovel_comp = q3,
    salas_5_pessoas = muda_salas(q6_1),
    salas_6_14_pessoas = muda_salas(q6_2),
    salas_15_29_pessoas = muda_salas(q6_3),
    salas_30_pessoas = muda_salas(q6_4),
    salas_coord = muda_salas(q6_5),
    banheiros = case_when(q6_6 == "Não" ~ "0",
                          q6_6 == "Sim" ~ "1",
                          q6_6 == "2" ~ "2",
                          TRUE ~ "3"),
    # renomeando
    recepcao = q6_7,
    cozinha = q6_8,
    almoxarifado = q6_9,
    espaco_externo = q6_10,
    # acesso a rampas e estabelecimentos
    rampas_calcada = recode_factor(q7_1, 
                                   "Sim, mas não estão de acordo com a Norma da ABNT" = "2",
                                   "Sim, de acordo com a Norma da ABNT" = "1",
                                   "Não possui" = "0"),
    rotas_espacos = recode_factor(q7_2, 
                                  "Sim, mas não estão de acordo com a Norma da ABNT" = "2",
                                  "Sim, de acordo com a Norma da ABNT" = "1",
                                  "Não possui" = "0"),
    rota_banheiro = recode_factor(q7_3, 
                                  "Sim, mas não estão de acordo com a Norma da ABNT" = "2",
                                  "Sim, de acordo com a Norma da ABNT" = "1",
                                  "Não possui" = "0"),
    banheiro_adaptado = recode_factor(q7_4, 
                                      "Sim, mas não estão de acordo com a Norma da ABNT" = "2",
                                      "Sim, de acordo com a Norma da ABNT" = "1",
                                      "Não possui" = "0"),
    outras_adaptacoes = q8_0,
    # condensando as questoes dos materiais
    telefone = case_when(q9_1 == "Sim" | q9_2 == "Sim" ~ "Sim",
                         TRUE ~ "Não"),
    # impressora
    impressora = q9_3,
    # tv, equip de som e dvd
    tv_equip = case_when(q9_4 == "Sim" | q9_5 == "Sim" | q9_6 == "Sim" ~ "Sim",
                         TRUE ~ "Não"),
    # datashow
    datashow = q9_7,
    # veiculo
    veiculo = case_when(q9_8 == "Sim" | q9_9 == "Sim" ~ "Sim",
                        TRUE ~ "Não"),
    # materiais de recreacao e consulta
    materiais_recreacao = case_when(
      q9_10 == "Sim" | q9_11 == "Sim" | q9_12 == "Sim" ~ "Sim",
      TRUE ~ "Não"),
    # 0 = sem computador, 1 = com computador, mas sem internet
    # 2 = 1 a 5 comp com internet
    comp_qtd = case_when(q10_1 == 0 | q10_2 == 0 ~ "1",
                         q10_2 > 0 & q10_2 <= 5 ~ "2",
                         q10_2 > 5 & q10_2 <= 10 ~ "3",
                         q10_2 > 10  ~ "4"),
    # numero indicando quantas faixas atende
    violencia_fisica = muda_q(c_across(q12_1_1:q12_1_4)),
    violencia_psico = muda_q(c_across(q12_2_1:q12_2_4)),
    violencia_sexual = muda_q(c_across(q12_3_1:q12_3_4)),
    exploracao_sexual = muda_q(c_across(q12_4_1:q12_4_4)),
    abandono = muda_q(c_across(q12_5_1:q12_5_4)),
    violencia_patri = muda_q(c_across(q12_6_1:q12_6_4)),
    trafico_pessoas = muda_q(c_across(q12_7_1:q12_7_4)),
    trab_infantil = muda_q(c_across(q12_8_1:q12_8_4)),
    sit_rua = muda_q(c_across(q12_9_1:q12_9_4)),
    disc_sexual = muda_q(c_across(q12_10_1:q12_10_4)),
    disc_racial = muda_q(c_across(q12_11_1:q12_11_4)),
    violencia_def = muda_q(c_across(q12_12_1:q12_12_4)),
    acolhimento = muda_q(c_across(q12_13_1:q12_13_4)),
    priv_lib = muda_q(c_across(q12_16_1:q12_16_4)),
    eg_sist_pr = muda_q(c_across(q12_17_1:q12_17_4)),
    med_soc_ad = muda_q(c_across(q12_14_1:q12_14_4)),
    med_soc_ad_eg = muda_q(c_across(q12_15_1:q12_15_4)),
    .keep = "unused"
  ) |>
  select(-c(q12_1_0:q12_17_0))

# cruzando infos para obter o numero de funcionarios em diferentes areas
dados_rh <- "data-raw/Censo_SUAS_2019_RH_CREAS_divulgação.xlsx" |>
  readxl::read_xlsx()

dados_nao_limpos_geral <- dados_nao_limpos_geral |>
  left_join(dados_rh |> select(NU_IDENTIFICADOR, d_58_8bin1_sum:d_58_8bin3_sum,
                               Porte_pop2010) |>
              distinct(NU_IDENTIFICADOR, .keep_all = TRUE),
            by = "NU_IDENTIFICADOR") |>
  mutate(func_fund = case_when(is.na(d_58_8bin1_sum) ~ "Sem info",
                               d_58_8bin1_sum == 0 ~ "0",
                               d_58_8bin1_sum == 1 ~ "1",
                               d_58_8bin1_sum > 1 ~ "2"),
         func_med = case_when(is.na(d_58_8bin2_sum) ~ "Sem info",
                              d_58_8bin2_sum == 0 ~ "0",
                              d_58_8bin2_sum == 1 ~ "1",
                              d_58_8bin2_sum == 2 ~ "2",
                              d_58_8bin2_sum > 2 ~ "3"),
         func_alto = case_when(is.na(d_58_8bin3_sum) ~ "Sem info",
                               d_58_8bin3_sum < 3 ~ "1",
                               d_58_8bin3_sum >= 3 & d_58_8bin3_sum < 6 ~ "2",
                               TRUE ~ "3"))
# salvando esses dados
dados_nao_limpos_geral |> 
  mutate(Porte_pop2010 = case_when(is.na(Porte_pop2010.y) ~ Porte_pop2010.x,
                                    Porte_pop2010.x == "" ~ Porte_pop2010.y,
                                    TRUE ~ Porte_pop2010.x),
         .keep = "unused") |>
write_rds("analise/dados_gerais_CREAS.rds")
}

dados_nao_limpos_geral <- "data-raw/Censo_SUAS_2019_CREAS_Dados_Gerais_divulgacao.csv" |>
  read.csv(sep = ";")

limpa_base(dados_nao_limpos_geral)
