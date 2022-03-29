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
         ano,
         imovel_comp:comp_qtd,
         -datashow,
         -cozinha,
         -recepcao,
         -impressora,
         -veiculo,
         -comp_qtd,
         -outras_adaptacoes) |>
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
         # out_adap = outras_adaptacoes,
         tel = telefone,
         # imp = impressora,
         tv = tv_equip,
         # v = veiculo,
         mat_r = materiais_recreacao
         # comp = comp_qtd
         )

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

ggsave("screeplot.pdf",
       width = 6, height = 4,
       path = "analise/figuras")

# 4 primeiros componentes parecem interessantes
# numero de variaveis
res_mca$var$cos2 |> nrow()

library(RColorBrewer)
cols <- brewer.pal(11, "BuGn")
pdf(
  "analise/figuras/qualidades_representacoes.pdf",
  width = 10,  height = 8)
corrplot::corrplot(res_mca$var$cos2, is.corr= FALSE, method = "color",
                   tl.srt = 45, diag = T, tl.col = "black", 
                   cl.ratio = 0.35, tl.cex = 0.85,
                   bg = "gray98",
                   addCoef.col = "black", 
                   addgrid.col = "gray", col= cols, 
                   number.cex= 0.25)
dev.off()

# analisando a qualidade de representacao das variaveis em cada um do comp
p1 <- fviz_cos2(res_mca, choice = "var", axes = c(1), top = 25)+
  theme_bw() +
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,
                                    vjust = 1,
                                    hjust = 1)) +
  geom_hline(yintercept = 0.15, linetype = "dashed",colour = "red") +
  labs(x = "",
       y = "Qualidade de representação",
       title = "Primeiro fator")

p2 <- fviz_cos2(res_mca, choice = "var", axes = c(2), top = 25)+
  theme_bw() +
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1)) +
  geom_hline(yintercept = 0.15, linetype = "dashed",colour = "red") +
  labs(x = "",
       y = "Qualidade de representação",
       title = "Segundo fator")

p3 <- fviz_cos2(res_mca, choice = "var", axes = c(3), top = 25)+
  theme_bw() +
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1)) +
  geom_hline(yintercept = 0.15, linetype = "dashed",colour = "red") +
  labs(x = "",
       y = "Qualidade de representação",
       title = "Terceiro fator")

p4 <- fviz_cos2(res_mca, choice = "var", axes = c(4), top = 25)+
  theme_bw() +
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1)) +
  geom_hline(yintercept = 0.15, linetype = "dashed",colour = "red") +
  labs(x = "",
       y = "Qualidade de representação",
       title = "Quarto fator")

ggpubr::ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
ggsave("qualit_rep.pdf",
       width = 10, height = 6,
       path = "analise/figuras")

# correlação quadrática para os três primeiros planos
p1 <- fviz_mca_var(res_mca, 
             choice = "mca.cor",
             repel = TRUE,
             axes = c(1, 2),
             ggtheme = theme_minimal()) +
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Primeiro plano")

p2 <- fviz_mca_var(res_mca, 
             choice = "mca.cor",
             repel = TRUE,
             axes = c(1, 3),
             ggtheme = theme_minimal()) +
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Segundo plano")

p3 <- fviz_mca_var(res_mca, 
             choice = "mca.cor",
             repel = TRUE,
             axes = c(2, 3),
             ggtheme = theme_minimal()) +
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Terceiro plano")

ggpubr::ggarrange(p1, p2, p3, nrow = 2, ncol = 2)

# planos fatoriais separados
fviz_mca_var(res_mca, 
             select.var = list(cos2 = 25),
             repel = TRUE,
             ggtheme = theme_bw()) +
  labs(title = "Primeiro plano fatorial") +
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))
ggsave("prim_plano.pdf",
       width = 8, height = 6,
       path = "analise/figuras")

fviz_mca_var(res_mca, 
                   select.var = list(cos2 = 25),
                   axes = c(1, 3),
                   repel = TRUE,
                   ggtheme = theme_bw()) +
  labs(title = "Segundo plano fatorial") +
  tema_mca
ggsave("segundo_plano.pdf",
       width = 8, height = 6,
       path = "analise/figuras")

fviz_mca_var(res_mca, 
                   select.var = list(cos2 = 25),
                   axes = c(2, 3),
                   repel = TRUE,
                   ggtheme = theme_bw()) +
  labs(title = "Terceiro plano fatorial") +
  tema_mca
ggsave("terceiro_plano.pdf",
       width = 8, height = 6,
       path = "analise/figuras")

# analisando cada creas individualmente
# salvando os planos juntos e separado
p1 <- fviz_mca_ind(res_mca,
             col.ind = "cos2",
             label = "none",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(1, 2),
             ggtheme = theme_minimal())+
  labs(title = "Primeiro plano fatorial") +
  tema_mca
p1
ggsave("ind_primeiro_plano.pdf",
       width = 8, height = 6,
       path = "analise/figuras")

p2 <- fviz_mca_ind(res_mca,
                   col.ind = "cos2",
                   label = "none",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   axes = c(1, 3),
                   ggtheme = theme_minimal())+
  labs(title = "Segundo plano fatorial") +
  tema_mca
p2
ggsave("ind_segundo_plano.pdf",
       width = 8, height = 6,
       path = "analise/figuras")

ggpubr::ggarrange(p1, p2, common.legend = TRUE)
ggsave("ind_primeiro_segundo_plano.pdf",
       width = 8, height = 6,
       path = "analise/figuras")

# analisando pela regiao de forma separada para nao ficar muito poluido
col <- RColorBrewer::brewer.pal(5, "Set2")
# apenas o primeiro plano
# obtendo o indice de cada
indice_regiao <- dados_mca |>
  rowid_to_column() |>
  select(rowid, Regiao)


p1 <- fviz_mca_ind(res_mca,
                   col.ind = col[1],
                   select.ind = list(name = indice_regiao |> 
                     filter(Regiao == "Região Norte") |>
                     pull(rowid) |> as.character()),
                   label = "none",
                   axes = c(1, 2),
                   ggtheme = theme_minimal())+
  labs(title = "Região Norte") +
  theme(text = element_text(size = 9, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))


p2 <- fviz_mca_ind(res_mca,
                   col.ind = col[2],
                   select.ind = list(name = indice_regiao |> 
                                       filter(Regiao == "Região Nordeste") |>
                                       pull(rowid) |> as.character()),
                   label = "none",
                   axes = c(1, 2),
                   ggtheme = theme_minimal())+
  labs(title = "Região Nordeste") +
  theme(text = element_text(size = 9, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))
  
p3 <- fviz_mca_ind(res_mca,
                   col.ind = col[3],
                   select.ind = list(name = indice_regiao |> 
                                       filter(Regiao == "Região Sudeste") |>
                                       pull(rowid) |> as.character()),
                   label = "none",
                   axes = c(1, 2),
                   ggtheme = theme_minimal())+
  labs(title = "Região Sudeste") +
  theme(text = element_text(size = 9, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))


p4 <- fviz_mca_ind(res_mca,
                   col.ind = col[4],
                   select.ind = list(name = indice_regiao |> 
                                       filter(Regiao == "Região Centro Oeste") |>
                                       pull(rowid) |> as.character()),
                   label = "none",
                   axes = c(1, 2),
                   ggtheme = theme_minimal())+
  labs(title = "Região Centro Oeste") +
  theme(text = element_text(size = 9, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))


p5 <- fviz_mca_ind(res_mca,
                   col.ind = col[5],
                   select.ind = list(name = indice_regiao |> 
                                       filter(Regiao == "Região Sul") |>
                                       pull(rowid) |> as.character()),
                   label = "none",
                   axes = c(1, 2),
                   ggtheme = theme_minimal())+
  labs(title = "Região Sul") +
  theme(text = element_text(size = 9, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))


ggpubr::ggarrange(p1, p2, p3, p4, p5, nrow = 3, ncol = 2)
ggsave("primeiro_plano_regioes.pdf",
       width = 12, height = 6,
       path = "analise/figuras")

