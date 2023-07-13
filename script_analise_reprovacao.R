#CARREGANDO PACOTES NECESÁRIOS PARA AS ANÁLISES
library(ggplot2)
library(dplyr)
library(readr)
library(readxl)
library(openxlsx)
library(gridExtra)

##CARREGANDO BASE DE DADOS DOS CURSOS DA UFRN
cursos_de_graduacao <-
  read_delim(
    "D:/Tecnologia da Informação (TI)/P3 2023.1/Probabilidade e Inferência/Projeto ANÁLISE UFRN/cursos-de-graduacao.csv",
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )

##SELECIONANDO AS VARIAVEIS DE INTERESSE
cursos_de_graduacao <- cursos_de_graduacao %>%
  select(id_curso, nome,grau_academico,modalidade_educacao,area_conhecimento,municipio)


##CARREGANDO DADOS DAS MATRICUALS DE 2022.1
matriculas_2022_1 <-
  read_delim(
    "D:/Tecnologia da Informação (TI)/P3 2023.1/Probabilidade e Inferência/Projeto ANÁLISE UFRN/matriculas-2022.1.csv",
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )

##SELECIONANDO AS VARIAVEIS DE INTERESSE
matriculas_2022_1 <- matriculas_2022_1 %>%
  select(id_turma, id_curso, discente, media_final, descricao)

##SELECIONANDO OS VALORES UNICOS
matriculas_2022_1 <- unique(matriculas_2022_1)

##CARREGANDO DADOS DAS MATRICUALS DE 2022.2
matriculas_2022_2 <-
  read_delim(
    "D:/Tecnologia da Informação (TI)/P3 2023.1/Probabilidade e Inferência/Projeto ANÁLISE UFRN/matriculas-2022.2.csv",
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )

##SELECIONANDO AS VARIAVEIS DE INTERESSE
matriculas_2022_2 <- matriculas_2022_2 %>%
  select(id_turma, id_curso, discente, media_final, descricao)

##SELECIONANDO OS VALORES UNICOS
matriculas_2022_2 <- unique(matriculas_2022_2)



##UNINDO OS DADOS DE MATRICULAS EM 2022.1 E 2022.2
matriculas_2022 <- rbind(matriculas_2022_1,matriculas_2022_2)

##CONTABILIZANDO QUANTOS VALORES "NA" CADA COLUNA POSSUI
colunas_contagem <- colSums(is.na(matriculas_2022))

###UNINDO OS DADOS DAS INFORMAÇÕES SOBRE OS CURSOS E AS MATRICULAS DE 2022.1
df_filtrado <- merge(matriculas_2022, cursos_de_graduacao, by = "id_curso")

## Definir os estratos desejados
estratos <- c("APROVADO", "REPROVADO", "APROVADO POR NOTA")

# Filtrar o dataframe pelos estratos
df_filtrado <- df_filtrado %>% filter(descricao %in% estratos)

##CONTABILIZANDO QUANTOS VALORES "NA" CADA COLUNA POSSUI
colunas_contagem <- colSums(is.na(df_filtrado))

##REMOVENDO AS COLUNAS COM "NA"
df_filtrado <- df_filtrado[complete.cases(df_filtrado$media_final), ]

##SELECIONANDO OS VALORES UNICOS
df_filtrado <- unique(df_filtrado)

##CONTABILIZANDO QUANTOS VALORES "NA" CADA COLUNA POSSUI
colunas_contagem <- colSums(is.na(df_filtrado))

# Substituir a vírgula por um ponto na coluna "media_final", para reconhecer como numeric ao invés de string
df_filtrado$media_final <- gsub(",", ".", df_filtrado$media_final)
df_filtrado$media_final <- as.numeric(df_filtrado$media_final)
#write.xlsx(df_filtrado, "D:/df_filtrado.xlsx")

##ADICIONANDO UM NOVA COLUNA "descricao_nova" que corresponde a simulação da alteração da média de 5 para 6
df_filtrado <- df_filtrado %>%
  mutate(descricao_nova = case_when(
    media_final < 6 ~ "REPROVADO",
    media_final >= 6 & media_final < 7 ~ "APROVADO POR NOTA",
    TRUE ~ "APROVADO"
  ))

# Contabilizando o número de cada categoria nas colunas "descricao" e "descricao_nova"
count_descricao <- table(df_filtrado$descricao)
count_descricao_nova <- table(df_filtrado$descricao_nova)

# Resultados da contabilização
print(count_descricao)
print(count_descricao_nova)



# GERANDO O HISTOGRAMA DO DADO GERAL DE 2022 PARA A MÉDIA ANTIGA
hist_media_antiga <- df_filtrado %>%
  select(media_final,descricao)

# Atribuindo uma cor a cada intervalo de nota para o plot
hist_media_antiga <- hist_media_antiga %>%
  mutate(cor = case_when(
    media_final < 5 ~ "vermelho",
    media_final >= 5 & media_final < 7 ~ "amarelo",
    media_final >= 7 ~ "verde"
  )) %>%
  mutate(media_final_floor = floor(media_final))

#Calculando a contagem de cada intervalo de nota
hist_counts <- hist_media_antiga %>%
  group_by(media_final_floor, cor) %>%
  summarise(count = n())

##Gerando o histograma da distribuição das notas
ggplot(hist_counts, aes(x = media_final_floor, y = count, fill = cor)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = count), vjust = -0.5, size = 3) +
  geom_vline(xintercept = c(4.5, 6.5), linetype = "dashed", color = "red", size = 1) +
  scale_x_continuous(breaks = seq(min(hist_media_antiga$media_final), max(hist_media_antiga$media_final), 1)) +
  scale_y_continuous(breaks = seq(0, max(hist_counts$count), by = 5000)) +
  scale_fill_manual(values = c("vermelho" = "red", "amarelo" = "yellow", "verde" = "green"),
                    labels = c("Reprovado", "Aprovado por Nota", "Aprovado")) +
  xlab("Média Final") +
  ylab("Status de Aprovação") +
  ggtitle("Aprovações pela regra antiga da Média Final") +
  theme_hc() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = NULL)

# GERANDO O HISTOGRAMA DO DADO GERAL DE 2022 PARA A MÉDIA NOVA
hist_media_nova <- df_filtrado %>%
  select(media_final,descricao_nova)


# Atribuindo uma cor a cada intervalo de nota para o plot
hist_media_nova <- hist_media_nova %>%
  mutate(cor = case_when(
    media_final < 6 ~ "vermelho",
    media_final >= 6 & media_final < 7 ~ "amarelo",
    media_final >= 7 ~ "verde"
  )) %>%
  mutate(media_final_floor = floor(media_final))

# Calculando a contagem de cada intervalo de nota
hist_counts <- hist_media_nova %>%
  group_by(media_final_floor, cor) %>%
  summarise(count = n())

##Gerando o histograma da distribuição das notas
ggplot(hist_counts, aes(x = media_final_floor, y = count, fill = cor)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = count), vjust = -0.5, size = 3) +
  geom_vline(xintercept = c(5.5, 6.5), linetype = "dashed", color = "red", size = 1) +
  scale_x_continuous(breaks = seq(min(hist_media_nova$media_final), max(hist_media_nova$media_final), 1)) +
  scale_y_continuous(breaks = seq(0, max(hist_counts$count), by = 5000)) +
  scale_fill_manual(values = c("vermelho" = "red", "amarelo" = "yellow", "verde" = "green"),
                    labels = c("Reprovado", "Aprovado por Nota", "Aprovado")) +
  xlab("Média Final") +
  ylab("Status de Aprovação") +
  ggtitle("Aprovações pela regra nova da Média Final") +
  theme_hc() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = NULL)


###################### ------> IMPLEMENTANDO FUNÇÃO PARA GERAR OS GRÁFICOS DE BARRA PARA CADA CURSO

# Função para gerar e salvar o gráfico
gerar_grafico <- function(nome) {
  # Filtrando os dados para o nome atual
  hist_media_nova <- df_filtrado %>%
    filter(nome == !!nome) %>%
    select(media_final, descricao_nova)
  
  # Criando a variável de cor
  hist_media_nova <- hist_media_nova %>%
    mutate(cor = case_when(
      media_final < 6 ~ "vermelho",
      media_final >= 6 & media_final < 7 ~ "amarelo",
      media_final >= 7 ~ "verde"
    )) %>%
    mutate(media_final_floor = floor(media_final))
  
  # Calculando a contagem de cada intervalo de nota
  hist_counts <- hist_media_nova %>%
    group_by(media_final_floor, cor) %>%
    summarise(count = n())
  
  # Gerando os gráfico
  p <- ggplot(hist_counts, aes(x = media_final_floor, y = count, fill = cor)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = count), vjust = -0.5, size = 3) +
    geom_vline(xintercept = c(4.5, 5.5), linetype = "dashed", color = "red", size = 1) +
    scale_x_continuous(breaks = seq(min(hist_media_nova$media_final), max(hist_media_nova$media_final), 1)) +
    scale_y_continuous(breaks = seq(0, max(hist_counts$count), by = 5000)) +
    scale_fill_manual(values = c("vermelho" = "red", "amarelo" = "yellow", "verde" = "green"),
                      labels = c("Reprovado", "Aprovado por Nota", "Aprovado")) +
    xlab("Média Final") +
    ylab("Status de Aprovação") +
    ggtitle(paste("Aprovações pela regra nova da Média Final -", nome)) +
    theme_hc() +
    theme(plot.title = element_text(hjust = 0.5, size = 9)) +
    labs(fill = NULL)
  
  # Salvando os gráficos localmente
  ggsave(paste0("D:/Tecnologia da Informação (TI)/P3 2023.1/Probabilidade e Inferência/Projeto ANÁLISE UFRN/Histogramas_explorar/", nome, ".jpeg"), plot = p)
}


# Aplicando  função a cada item na coluna "nome"
nomes <- unique(df_filtrado$nome)

for (nome in nomes) {
  gerar_grafico(nome)
}


##########################################################################
### ANALISANDO OS STATUS DE APROVAÇÃO POR CURSO PELA MÉDIA ANTIGA/NOVA

##MANIPULANDO OS DADOS PARA FAZER O PLOT DOS DOIS GRÁFICOS DE FORMA UNCIA

#SELECIONANDO DO DATAFRAME PRINCIPAL AS VARIAVEIS DE INTERESSE PARA MÉDIA NOVA/ANTIGA
status_curso_media_antiga <- df_filtrado %>%
  select(descricao,nome,area_conhecimento)

status_curso_media_nova <- df_filtrado %>%
  select(descricao_nova,nome,area_conhecimento)

merged_status_curso <- status_curso_media_nova

#ADICIONANDO O SUFIXO "_nova" PARA DIFERENCIAR DO DF DAS MÉDIAS ANTIGAS
merged_status_curso$nome <- paste0(merged_status_curso$nome, "_nova")

merged_status_curso <- merged_status_curso %>%
  rename(descricao = descricao_nova)

#UNINDO OS DF'S DOS 2 CRITÉRIOS DE MÉDIAS
merged_status_curso <- rbind(merged_status_curso, status_curso_media_antiga)

#CONTABILIZANDO O STATUS DE APROVAÇÃO PARA CADA CURSO E ÁREA DE CONHECIMENTO
merged_status_curso <- merged_status_curso %>%
  group_by(nome,descricao, area_conhecimento) %>%
  summarise(contagem = n())

merged_status_curso <- unique(merged_status_curso)
#write.xlsx(merged_status_curso, "D:/merged_status_curso.xlsx")

###FAZENDO O PLOT EM CONJUNTO
###################### ------> IMPLEMENTANDO FUNÇÃO PARA GERAR OS GRÁFICOS DE PORPORÇÃO POR STATUS DE APROVAÇÃO PARA CADA CURSO

gerar_grafico <- function(df, titulo, filename) {
  p <- ggplot(df) +
    aes(y = nome, x = contagem, fill = descricao) +
    geom_col(position = "fill") +
    geom_text(aes(label = contagem), position = position_fill(vjust = 0.5)) +
    scale_fill_manual(values = c("REPROVADO" = "red", "APROVADO POR NOTA" = "yellow", "APROVADO" = "green"), 
                      name = "Status de aprovação") +
    scale_y_discrete(limits = rev(unique(df$nome))) +
    theme_bw() +
    labs(title = titulo,
         subtitle = "Eixo y ordenado em ordem alfabética decrescente",
         x = "Contagem de Status",
         y = "Curso") +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(face = "italic"),
          axis.title.x = element_text(size = 20, face = "bold"),
          axis.title.y = element_text(size = 20, face = "bold"),
          axis.text.y = element_text(size = 10, face = "bold"),
          axis.text.x = element_text(size = 20, face = "bold"))
  
  # Salvando o gráfico
  ggsave(filename, plot = p, path = "D:/Tecnologia da Informação (TI)/P3 2023.1/Probabilidade e Inferência/Projeto ANÁLISE UFRN/Reprovações_area_curso", 
         width = 1920/96, height = 1080/96, dpi = 96, device = "jpeg")
}

# Gerando e salvando os gráficos para cada área de conhecimento
for (area in unique(merged_status_curso$area_conhecimento)) {
  df_area <- filter(merged_status_curso, area_conhecimento == area)
  filename <- paste0(area, ".jpeg") # cria um nome de arquivo com a área de conhecimento
  gerar_grafico(df_area, paste("Análise das aprovações e reprovações na área: ", area), filename)
}



################################################
#Contabilizando para a influencia da mudança da média para o grau academico
#MEDIA ANTIGA
status_grau_academico_antiga <- df_filtrado %>%
  select(grau_academico,descricao)%>%
  group_by(grau_academico,descricao) %>%
  summarise(contagem = n())
  
#MÉDIA NOVA
#Contabilizando para a influencia da mudança da média para o grau academico
status_grau_academico_nova <- df_filtrado %>%
  select(grau_academico,descricao_nova)%>%
  group_by(grau_academico,descricao_nova) %>%
  summarise(contagem = n())

status_grau_academico_nova <- status_grau_academico_nova %>%
  rename(descricao = descricao_nova)

status_grau_academico_nova$grau_academico <- paste0(status_grau_academico_nova$grau_academico, "_nova")


status_grau_academico <- rbind(status_grau_academico_antiga,status_grau_academico_nova)


#############
# Criando o gráfico
ggplot(status_grau_academico) +
  aes(y = fct_rev(grau_academico), x = contagem, fill = descricao) +
  geom_col(position = "fill") +
  geom_text(aes(label = contagem), position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("REPROVADO" = "red", "APROVADO POR NOTA" = "yellow", "APROVADO" = "green"), 
                    name = "Status de aprovação") +
  #scale_y_discrete(limits = rev(unique(status_grau_academico$grau_academico))) +
  theme_bw() +
  labs(title = "Análise das aprovações e reprovações por grau academico",
       x = "Contagem de Status",
       y = "Grau Academico") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"))


###AVALIANDO A MUDANÇA EM % DO GRAU ACADEMICO
status_grau_academico_prop <- cbind(status_grau_academico_antiga,status_grau_academico_nova)
status_grau_academico_prop$prop <- (((status_grau_academico_prop$contagem...6 - status_grau_academico_prop$contagem...3))/ status_grau_academico_prop$contagem...3) * 100 
status_grau_academico_prop$prop <- round(status_grau_academico_prop$prop, 1)

##VISUALIZANDO O GRÁFICO DA PROPORÇÃO PROVOCADA PELA MUDANÇA NO GRAU ACADEMICO
ggplot(status_grau_academico_prop) +
  aes(x = fct_rev(grau_academico...4), y = prop, fill = ifelse(prop < 0, "Aprovado por Nota", "Reprovado")) +
  geom_col() +
  geom_text(aes(label = prop), vjust = 0.5, size = 5) +
  scale_fill_manual(values = c("Aprovado por Nota" = "yellow", "Reprovado" = "red"), name = "Status de Aprovação") +
  coord_flip() +
  theme_bw() +
  labs(title = "Análise das aprovações e reprovações por Grau Acadêmico",
       x = "Grau Acadêmico",
       y = "Variação do status de aprovação em %") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"))


################################################

#Contabilizando para a influencia da mudança da média para a modalidade de educação
#MEDIA ANTIGA
status_modalidade_educacao_antiga <- df_filtrado %>%
  select(modalidade_educacao,descricao)%>%
  group_by(modalidade_educacao,descricao) %>%
  summarise(contagem = n())

#MÉDIA NOVA
#Contabilizando para a influencia da mudança da média para o grau academico
status_modalidade_educacao_nova <- df_filtrado %>%
  select(modalidade_educacao,descricao_nova)%>%
  group_by(modalidade_educacao,descricao_nova) %>%
  summarise(contagem = n())

status_modalidade_educacao_nova <- status_modalidade_educacao_nova %>%
  rename(descricao = descricao_nova)

status_modalidade_educacao_nova$modalidade_educacao <- paste0(status_modalidade_educacao_nova$modalidade_educacao, "_nova")


status_modalidade_educacao <- rbind(status_modalidade_educacao_antiga,status_modalidade_educacao_nova)


#############
# Criando o gráfico
ggplot(status_modalidade_educacao) +
  aes(y = fct_rev(modalidade_educacao), x = contagem, fill = descricao) +
  geom_col(position = "fill") +
  geom_text(aes(label = contagem), position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("REPROVADO" = "red", "APROVADO POR NOTA" = "yellow", "APROVADO" = "green"), 
                    name = "Status de aprovação") +
  #scale_y_discrete(limits = rev(unique(status_modalidade_educacao$modalidade_educacao))) +
  theme_bw() +
  labs(title = "Análise das aprovações e reprovações por modalidade de educação",
       x = "Contagem de Status",
       y = "Modalidade de Educação") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"))

###AVALIANDO A MUDANÇA EM % DO STATUS DE ARPOVAÇÃO
status_modalidade_educacao_prop <- cbind(status_modalidade_educacao_antiga,status_modalidade_educacao_nova)
status_modalidade_educacao_prop$prop <- (((status_modalidade_educacao_prop$contagem...6 - status_modalidade_educacao_prop$contagem...3))/ status_modalidade_educacao_prop$contagem...3) * 100 
status_modalidade_educacao_prop$prop <- round(status_modalidade_educacao_prop$prop, 1)

##VISUALIZANDO O GRÁFICO DA PROPORÇÃO PROVOCADA PELA MUDANÇA NA MODALIDADE DE ENSINO
ggplot(status_modalidade_educacao_prop) +
  aes(x = fct_rev(modalidade_educacao...4), y = prop, fill = ifelse(prop < 0, "Aprovado por Nota", "Reprovado")) +
  geom_col() +
  geom_text(aes(label = prop), vjust = 0.5, size = 5) +
  scale_fill_manual(values = c("Aprovado por Nota" = "yellow", "Reprovado" = "red"), name = "Status de Aprovação") +
  coord_flip() +
  theme_bw() +
  labs(title = "Análise das aprovações e reprovações por Modalidade de Ensino",
       x = "Modalidade de Educação",
       y = "Variação do status de aprovação em %") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"))


################################################

#Contabilizando para a influencia da mudança da média para o municipio
#MEDIA ANTIGA
status_municipio_antiga <- df_filtrado %>%
  select(municipio,descricao)%>%
  group_by(municipio,descricao) %>%
  summarise(contagem = n())

#MÉDIA NOVA
#Contabilizando para a influencia da mudança da média para o municipio
status_municipio_nova <- df_filtrado %>%
  select(municipio,descricao_nova)%>%
  group_by(municipio,descricao_nova) %>%
  summarise(contagem = n())

status_municipio_nova <- status_municipio_nova %>%
  rename(descricao = descricao_nova)

status_municipio_nova$municipio <- paste0(status_municipio_nova$municipio, "_nova")


#############
# Criando o gráfico
ggplot(status_municipio) +
  aes(y = fct_rev(municipio), x = contagem, fill = descricao) +
  geom_col(position = "fill") +
  geom_text(aes(label = contagem), position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("REPROVADO" = "red", "APROVADO POR NOTA" = "yellow", "APROVADO" = "green"), 
                    name = "Status de aprovação") +
  #scale_y_discrete(limits = rev(unique(status_municipio$municipio))) +
  theme_bw() +
  labs(title = "Análise das aprovações e reprovações por Município",
       x = "Contagem de Status",
       y = "Município") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"))


###AVALIANDO A MUDANÇA EM % DO STATUS DE ARPOVAÇÃO
status_municipio_prop <- cbind(status_municipio_antiga,status_municipio_nova)
status_municipio_prop$prop <- (((status_municipio_prop$contagem...6 - status_municipio_prop$contagem...3))/ status_municipio_prop$contagem...3) * 100 
status_municipio_prop$prop <- round(status_municipio_prop$prop, 1)

##VISUALIZANDO O GRÁFICO DA PROPORÇÃO PROVOCADA PELA MUDANÇA NO STATUS DE APROVAÇÃO
ggplot(status_municipio_prop) +
  aes(x = fct_rev(municipio...4), y = prop, fill = ifelse(prop < 0, "Aprovado por Nota", "Reprovado")) +
  geom_col() +
  geom_text(aes(label = prop), vjust = 0.5, size = 5) +
  scale_fill_manual(values = c("Aprovado por Nota" = "yellow", "Reprovado" = "red"), name = "Status de Aprovação") +
  coord_flip() +
  theme_bw() +
  labs(title = "Análise das aprovações e reprovações por Município",
       x = "Município",
       y = "Variação do status de aprovação em %") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"))



teste <- df_filtrado %>%
  select(discente,media_final, nome,descricao, descricao_nova)
teste <- unique(teste)
#write.xlsx(teste, "D:/teste_estatisticas.xlsx")
