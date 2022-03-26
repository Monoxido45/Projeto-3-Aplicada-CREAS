# fazendo apenas a analise de correspondencia
library(tidyverse)

dados_creas <- "analise/dados_gerais_CREAS.rds" |>
  readRDS()

# selecionando apenas variaveis de interesse
dados_mca <-  dados_creas |> mutate(ano = data_creas |> lubridate::dmy() |>
                        lubridate::year(),
                      ano = case_when(ano < 2010 ~ "<2010",
                                      ano >= 2010 ~ ">=2010"))|> 
  select(Regiao,
         imovel_comp:comp_qtd,
         -datashow,
         -cozinha,
         -recepcao,
         -impressora,
         -veiculo) |>
  rename(ic = imovel_comp,
         s_5 = salas_5_pessoas,
         s_6_14 = salas_6_14_pessoas,
         s_15_29 = salas_15_29_pessoas,
         s_30 = salas_30_pessoas,
         s_c = salas_coord,
         ban = banheiros,
         # rec = recepcao,
         # coz = cozinha,
         alm = almoxarifado,
         ramp_c = rampas_calcada,
         rota_esp = rotas_espacos,
         rota_ban = rota_banheiro,
         ban_adap = banheiro_adaptado,
         out_adap = outras_adaptacoes,
         tel = telefone,
         # imp = impressora,
         tv = tv_equip,
         # v = veiculo,
         mat_r = materiais_recreacao,
         comp = comp_qtd)

library(factoextra)
library(FactoMineR)
res_mca <- MCA(dados_mca, ncp = 10, quali.sup = 1, graph = FALSE)

# tema para os planos
tema_mca <- theme(text = element_text(size = 14, 
                                      family ="serif"),
                  plot.title = element_text(hjust = 0.5))

# screeplot
fviz_screeplot(res_mca, addlabels = TRUE, linecolor = "steelblue",
               geom = "line",
               xlab = "Componentes", 
               ylab = "Porcentagem da variância total explicada",
               title = "") +
  theme_bw() +
  tema_mca

# 4 primeiros componentes
# analisando a qualidade de representacao das variaveis em cada um do comp
fviz_cos2(res_mca, choice = "var", axes = c(1, 2), top = 25)+
  theme_bw() +
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,
                                    vjust = 1,
                                    hjust = 1)) +
  geom_hline(colour = "red", yintercept = 0.2, linetype = "dashed") +
  labs(x = "Variáveis",
       y = "Qualidade de representação (cos2)",
       title = "Dimensão (1,2)")

fviz_cos2(res_mca, choice = "var", axes = c(1, 3), top = 25)+
  theme_bw() +
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1)) +
  geom_hline(colour = "red", yintercept = 0.2, linetype = "dashed") +
  labs(x = "Variáveis",
       y = "Qualidade de representação (cos2)",
       title = "Dimensão (1,3)")

fviz_cos2(res_mca, choice = "var", axes = c(1, 4), top = 25)+
  theme_bw() +
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1)) +
  geom_hline(colour = "red", yintercept = 0.2, linetype = "dashed") +
  labs(x = "Variáveis",
       y = "Qualidade de representação (cos2)",
       title = "Dimensão (1,2)")

fviz_cos2(res_mca, choice = "var", axes = c(2, 4), top = 25)+
  theme_bw() +
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1)) +
  geom_hline(colour = "red", yintercept = 0.2, linetype = "dashed") +
  labs(x = "Variáveis",
       y = "Qualidade de representação (cos2)",
       title = "Dimensão (1,2)")

fviz_cos2(res_mca, choice = "var", axes = c(1, 3), top = 25)
fviz_cos2(res_mca, choice = "var", axes = c(1, 2), top = 25)
fviz_cos2(res_mca, choice = "var", axes = c(4), top = 25)


fviz_mca_var(res_mca, 
             choice = "mca.cor",
             repel = TRUE,
             ggtheme = theme_minimal())

fviz_mca_var(res_mca, 
             select.var = list(cos2 = 25),
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_bw()) +
  tema_mca



fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "Regiao", # color by groups 
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 
