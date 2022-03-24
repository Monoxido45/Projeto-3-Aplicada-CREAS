# todas as analises descritivas feitas
library(tidyverse)

dados_creas <- "analise/dados_gerais_CREAS.rds" |>
  readRDS()

# analises univariadas das diversas variaveis cateogricas
# analises univariadas das variaveis nao usadas  ----------------------------------------------------

# por regiao
dados_creas |> count(Regiao)

# por UF
dados_creas |> count(UF)

# do sudeste
dados_creas |> filter(Regiao == "Região Sudeste") |> count(UF)

# porte pop 2010
dados_creas |> count(Porte_pop2010)

# questionario completo
dados_creas |> count(Q_completo)

# data do creas
dados_creas |>
  mutate(ano = data_creas |> lubridate::dmy() |>
           lubridate::year()) |>
  ggplot(aes(x = ano, y = ..density..)) +
  geom_histogram(fill = "white", colour = "black", alpha = 0.65) +
  geom_density(colour = "dodgerblue3", alpha = 0.5) +
  theme_minimal() +
  theme(text = element_text(family = "serif", size = 14),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(9)) +
  labs(y = "Densidade",
       x = "Ano de criação")


# analises univariadas para as horas e estrutura --------------------------
# tipo de creas
  dados_creas |>
  count(tipo_creas) |>
  ungroup() |>
  mutate(p = n/sum(n)) |>
  ggplot(aes(x = tipo_creas, y = p, fill = tipo_creas,
             label = scales::percent(p, accuracy = 0.1, decimal.mark = ",")))+
  geom_col()+
  scale_y_continuous(breaks = scales::pretty_breaks(10)) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3.2) +
  theme_minimal() +
  theme(text = element_text(family = "serif", size = 14),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Tipo de CREAS",
       y = "Proporção") +
  scale_fill_brewer(palette = "Set1", guide = "none")

dados_creas |>
  pivot_longer(funcionamento_dias:funcionamento_horas, names_to = "funcionamento",
               values_to = "horas") |>
  group_by(funcionamento) |>
  count(horas) |>
  mutate(p = n/sum(n)) |>
  ggplot(aes(x = horas, y = p, label = scales::percent(p, accuracy = 0.1, decimal.mark = ","))) +
  geom_col(fill = "dodgerblue3") +
  scale_y_continuous(breaks = scales::pretty_breaks(10)) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3.2) +
  theme_bw() +
  theme(text = element_text(family = "serif", size = 14),
        plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Horas",
       y = "Proporção") +
  facet_wrap(~funcionamento, scale = "free_x")
  

# estrutura
# imovel compartilhado
dados_creas |>
  count(imovel_comp) |>
  ungroup() |>
  mutate(p = n/sum(n)) |>
  ggplot(aes(x = imovel_comp, y = p, fill = imovel_comp, 
             label = scales::percent(p, accuracy = 0.1, decimal.mark = ",")))+
  geom_col()+
  scale_y_continuous(breaks = scales::pretty_breaks(10)) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3.2) +
  theme_minimal() +
  theme(text = element_text(family = "serif", size = 14),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Imóvel compartilhado",
         y = "Proporção") +
  scale_fill_brewer(palette = "Set1", guide = "none")

# numero de banheiros, salas, etc
dados_creas |>
  pivot_longer(salas_5_pessoas:banheiros, names_to = "salas",
               values_to = "numero") |>
  group_by(salas) |>
  count(numero) |>
  mutate(p = n/sum(n)) |>
  ggplot(aes(x = numero, y = p, 
             label = scales::percent(p, accuracy = 0.1, decimal.mark = ","), 
             fill = numero)) +
  geom_col() +
  scale_y_continuous(breaks = scales::pretty_breaks(10)) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3.2) +
  theme_bw() +
  theme(text = element_text(family = "serif", size = 14),
        plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Categoria",
       y = "Proporção") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  facet_wrap(~salas, scale = "free_x")

# se tem recepcao, cozinha, espaço externo
dados_creas |>
  pivot_longer(recepcao:espaco_externo, names_to = "espacos",
               values_to = "numero") |>
  group_by(espacos) |>
  count(numero) |>
  mutate(p = n/sum(n)) |>
  ggplot(aes(x = numero, y = p, 
             label = scales::percent(p, accuracy = 0.1, decimal.mark = ","), 
             fill = numero)) +
  geom_col() +
  scale_y_continuous(breaks = scales::pretty_breaks(10)) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3.2) +
  theme_bw() +
  theme(text = element_text(family = "serif", size = 14),
        plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Categoria",
       y = "Proporção") +
  scale_fill_brewer(palette = "Set1", guide = "none")  +
  facet_wrap(~espacos, scale = "free_x")


# rampas e adaptacoes
dados_creas |>
  pivot_longer(rampas_calcada:outras_adaptacoes, names_to = "adaptacoes",
               values_to = "numero") |>
  group_by(adaptacoes) |>
  count(numero) |>
  mutate(p = n/sum(n)) |>
  ggplot(aes(x = numero, y = p, 
             label = scales::percent(p, accuracy = 0.1, decimal.mark = ","), 
             fill = numero)) +
  geom_col() +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3.2) +
  theme_bw() +
  theme(text = element_text(family = "serif", size = 14),
        plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Presença",
       y = "Proporção") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  facet_wrap(~adaptacoes, scale = "free_x")


# equipamentos
dados_creas |>
  pivot_longer(telefone:materiais_recreacao, names_to = "materiais",
               values_to = "numero") |>
  group_by(materiais) |>
  count(numero) |>
  mutate(p = n/sum(n)) |>
  ggplot(aes(x = numero, y = p, 
             label = scales::percent(p, accuracy = 0.1, decimal.mark = ","), 
             fill = numero)) +
  geom_col() +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3.2) +
  theme_bw() +
  theme(text = element_text(family = "serif", size = 14),
        plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Presença de equipamentos",
       y = "Proporção") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  facet_wrap(~materiais, scale = "free_x")
  
# quantidade de computadores com internet
dados_creas |>
  count(comp_qtd) |>
  ungroup() |>
  mutate(p = n/sum(n)) |>
  ggplot(aes(x = comp_qtd, y = p, 
             label = scales::percent(p, accuracy = 0.1, decimal.mark = ","), 
             fill = comp_qtd)) +
  geom_col() +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3.2) +
  theme_minimal() +
  theme(text = element_text(family = "serif", size = 14),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Categorias de acesso a internet",
       y = "Proporção") +
  scale_fill_brewer(palette = "Set1", guide = "none")


# servicos
dados_creas |>
  pivot_longer(violencia_fisica:priv_lib, names_to = "servicos",
               values_to = "numero") |>
  group_by(servicos) |>
  count(numero) |>
  mutate(p = n/sum(n)) |>
  ggplot(aes(x = numero, y = p, 
             label = scales::percent(p, accuracy = 0.1, decimal.mark = ","), 
             fill = numero)) +
  geom_col() +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3.2) +
  theme_bw() +
  theme(text = element_text(family = "serif", size = 14),
        plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Número de categorias atendidas",
       y = "Proporção") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  facet_wrap(~servicos, scale = "free_x")


# perfil dos funcionarios em geral
dados_creas |>
  pivot_longer(func_fund:func_alto, names_to = "func",
               values_to = "numero") |>
  group_by(func) |>
  count(numero) |>
  mutate(p = n/sum(n)) |>
  ggplot(aes(x = numero, y = p, 
             label = scales::percent(p, accuracy = 0.1, decimal.mark = ","), 
             fill = numero)) +
  geom_col() +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3.2) +
  theme_bw() +
  theme(text = element_text(family = "serif", size = 14),
        plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Faixas de números de funcionários",
       y = "Proporção") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  facet_wrap(~func, scale = "free_x")




