# 1. Pacotes
# carregando pacote de leitura, manipulação, exportação e visualizacao
library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(ggplot2)
library(survey)

# 2. importe de dados
dados <- readRDS("dados_domicilios.rds")

# inspecionando dados
glimpse(dados) # Visualização do tipo de dados e as primeiras observações de cada coluna


# 3. Limpeza e recodificações

# criando variavel com classificacao de inseguranca alimenta
dados$V6199_a<-factor(dados$V6199, levels = 1:4,
                      labels =c("Segurança", "Insegurança leve", "Insegurança moderada","Insegurança grave"))

# transforma nome das colunas e separa variaveis de interesse
dados <- dados %>%
  rename(
    CONDICAO_NA_UC = V0306,
    IDADE_EM_ANOS = V0403,
    SEXO = V0404,
    COR_RACA = V0405,
    RENDIM_REMUN_12M = V0407,
    LE_ESCREVE = V0414,
    COMPOSICAO_FAMILIAR = C6,
    URBANO_RURAL = TIPO_SITUACAO_REG,
    FORMA_ABAST_AGUA = V0207,
    ESCOADOURO_DEJECOES = V0212,
    DESTINO_LIXO = V0213,
    ENERGIA_ELETRICA_REDE_GERAL = V02141,
    ENERGIA_ELETRICA_OUTRA_ORIGEM = V02142,
    FREQ_ENERGIA_ELETRICA_REDE_GERAL_DISPONIVEL = V0215,
    SITUACAO_SEGURANCA_ALIMENTAR = V6199,
    TIPO_SEGURANCA_ALIMENTAR = V6199_a
  ) %>%
  select(
    COD_UPA,
    ESTRATO_POF,
    PESO_FINAL,
    CONDICAO_NA_UC,
    IDADE_EM_ANOS,
    SEXO,
    COR_RACA,
    RENDIM_REMUN_12M,
    LE_ESCREVE,
    ANOS_ESTUDO,
    NIVEL_INSTRUCAO,
    RENDA_DISP_PC,
    RENDA_MONET_PC,
    COMPOSICAO_FAMILIAR,
    UF,
    GRANDE_REGIAO,
    URBANO_RURAL,
    FORMA_ABAST_AGUA,
    ESCOADOURO_DEJECOES,
    DESTINO_LIXO,
    ENERGIA_ELETRICA_REDE_GERAL,
    ENERGIA_ELETRICA_OUTRA_ORIGEM,
    FREQ_ENERGIA_ELETRICA_REDE_GERAL_DISPONIVEL,
    SITUACAO_SEGURANCA_ALIMENTAR,
    TIPO_SEGURANCA_ALIMENTAR
  )

# trata NA e outros códigos
dados <- dados %>%
  mutate(
    
    # Variáveis sociodemográficas
    IDADE_EM_ANOS = ifelse(IDADE_EM_ANOS %in% c(999) | IDADE_EM_ANOS < 0, NA, IDADE_EM_ANOS),
    
    # Educação
    ANOS_ESTUDO = ifelse(ANOS_ESTUDO %in% c(99, 999)| ANOS_ESTUDO < 0, NA, ANOS_ESTUDO),
    
    # Renda
    RENDA_DISP_PC = ifelse(RENDA_DISP_PC < 0, NA, RENDA_DISP_PC),
    RENDA_MONET_PC = ifelse(RENDA_MONET_PC < 0, NA, RENDA_MONET_PC),
)


# Criação de faixas etárias de acordo com ciclos de vida (infância, adolescência, idade ativa, envelhecimento)
dados$FAIXA_IDADE = case_when(
  is.na(dados$IDADE_EM_ANOS) ~ NA_character_,
  dados$IDADE_EM_ANOS <= 9  ~ "0–9 anos",
  dados$IDADE_EM_ANOS <= 19 ~ "10–19 anos",
  dados$IDADE_EM_ANOS <= 39 ~ "20–39 anos",
  dados$IDADE_EM_ANOS <= 59 ~ "40–59 anos",
  dados$IDADE_EM_ANOS >= 60 ~ "60 anos ou mais"
)

# Criação de faixas de renda
dados <- dados %>%
  mutate(
    FAIXA_RENDA_PC = case_when(
      is.na(RENDA_DISP_PC) ~ NA_character_,
      RENDA_DISP_PC <= 500 ~ "Até R$ 500",
      RENDA_DISP_PC <= 1000 ~ "R$ 501 a R$ 1.000",
      RENDA_DISP_PC <= 2000 ~ "R$ 1.001 a R$ 2.000",
      RENDA_DISP_PC <= 5000 ~ "R$ 2.001 a R$ 5.000",
      RENDA_DISP_PC > 5000 ~ "Acima de R$ 5.000"
    )
  )

# Criação de faixas de anos de estudo
dados <- dados %>%
  mutate(
    FAIXA_ANOS_ESTUDO = case_when(
      is.na(ANOS_ESTUDO) ~ NA_character_,
      ANOS_ESTUDO == 0 ~ "Sem instrução",
      ANOS_ESTUDO <= 4 ~ "1–4 anos",
      ANOS_ESTUDO <= 8 ~ "5–8 anos",
      ANOS_ESTUDO <= 11 ~ "9–11 anos",
      ANOS_ESTUDO >= 12 ~ "12 anos ou mais"
    )
  )


# Criação Descrição de Cor ou Raça
dados <- dados %>%
  mutate(
    COR_RACA_DESC = case_when(
      COR_RACA == 1 ~ "Branca",
      COR_RACA == 2 ~ "Preta",
      COR_RACA == 3 ~ "Amarela",
      COR_RACA == 4 ~ "Parda",
      COR_RACA == 5 ~ "Indígena",
      COR_RACA == 9 ~ "Sem declaração",
      is.na(COR_RACA) ~ NA_character_
    )
  )


# Criação Descrição de Sexo
dados <- dados %>%
  mutate(
    SEXO_DESC = case_when(
      SEXO == 1 ~ "Homem",
      SEXO == 2 ~ "Mulher",
      SEXO == 9 ~ "Sem declaração",
      TRUE ~ NA_character_
    )
  )

# Criação Descrição nível de instrucao
dados <- dados %>%
  mutate(
    NIVEL_INSTRUCAO_DESC = case_when(
      NIVEL_INSTRUCAO == 1 ~ "Sem instrução",
      NIVEL_INSTRUCAO == 2 ~ "Ensino Fundamental Incompleto",
      NIVEL_INSTRUCAO == 3 ~ "Ensino Fundamental Completo",
      NIVEL_INSTRUCAO == 4 ~ "Ensino Médio Incompleto",
      NIVEL_INSTRUCAO == 5 ~ "Ensino Médio Completo",
      NIVEL_INSTRUCAO == 6 ~ "Ensino Superior Incompleto",
      NIVEL_INSTRUCAO == 7 ~ "Ensino Superior Completo",
      TRUE ~ NA_character_
    )
  )

# Criação Descrição da composicao familiar
dados <- dados %>%
  mutate(
    COMPOSICAO_FAMILIAR_DESC = case_when(
      COMPOSICAO_FAMILIAR == 1 ~ "Um adulto sem criança",
      COMPOSICAO_FAMILIAR == 2 ~ "Um adulto com ao menos uma criança",
      COMPOSICAO_FAMILIAR == 3 ~ "Mais de um adulto sem criança",
      COMPOSICAO_FAMILIAR == 4 ~ "Mais de um adulto com ao menos uma criança",
      COMPOSICAO_FAMILIAR == 5 ~ "Um ou mais idosos com ou sem crianças",
      COMPOSICAO_FAMILIAR == 6 ~ "Um ou mais idosos, com ao menos um adulto, com ou sem crianças",
      TRUE ~ NA_character_
    )
  )



# Criação Descrição das UF
dados <- dados %>%
  mutate(
    UF_DESC = case_when(
      UF == 11 ~ "Rondônia",
      UF == 12 ~ "Acre",
      UF == 13 ~ "Amazonas",
      UF == 14 ~ "Roraima",
      UF == 15 ~ "Pará",
      UF == 16 ~ "Amapá",
      UF == 17 ~ "Tocantins",
      UF == 21 ~ "Maranhão",
      UF == 22 ~ "Piauí",
      UF == 23 ~ "Ceará",
      UF == 24 ~ "Rio Grande do Norte",
      UF == 25 ~ "Paraíba",
      UF == 26 ~ "Pernambuco",
      UF == 27 ~ "Alagoas",
      UF == 28 ~ "Sergipe",
      UF == 29 ~ "Bahia",
      UF == 31 ~ "Minas Gerais",
      UF == 32 ~ "Espírito Santo",
      UF == 33 ~ "Rio de Janeiro",
      UF == 35 ~ "São Paulo",
      UF == 41 ~ "Paraná",
      UF == 42 ~ "Santa Catarina",
      UF == 43 ~ "Rio Grande do Sul",
      UF == 50 ~ "Mato Grosso do Sul",
      UF == 51 ~ "Mato Grosso",
      UF == 52 ~ "Goiás",
      UF == 53 ~ "Distrito Federal",
      TRUE ~ NA_character_
    )
  )

# Criação Descrição Urbano e Rural
dados <- dados %>%
  mutate(
   URBANO_RURAL_DESC = case_when(
      URBANO_RURAL == 1 ~ "Urbano",
      URBANO_RURAL == 2 ~ "Rural",
      TRUE ~ NA_character_
    )
  )

# Criação Descrição das Regioes
dados <- dados %>%
  mutate(
    GRANDE_REGIAO_DESC = case_when(
      GRANDE_REGIAO == 1 ~ "Norte",
      GRANDE_REGIAO == 2 ~ "Nordeste",
      GRANDE_REGIAO == 3 ~ "Sudeste",
      GRANDE_REGIAO == 4 ~ "Sul",
      GRANDE_REGIAO == 5 ~ "Centro-Oeste",
      TRUE ~ NA_character_
    )
  )


# 4. Criar delineamento amostral complexo
delineamento<- svydesign(id = ~COD_UPA,strata = ~ESTRATO_POF,weights = ~PESO_FINAL ,
                         data = dados ,
                         nest = TRUE)


# 5. Análises descritivas ponderadas

##  Análises descritivas univariada distribuicao percentual
svymean(~TIPO_SEGURANCA_ALIMENTAR, delineamento, na.rm = TRUE) * 100
svymean(~FAIXA_RENDA_PC, delineamento, na.rm = TRUE) * 100
svymean(~FAIXA_IDADE, delineamento, na.rm = TRUE) * 100
svymean(~SEXO_DESC, delineamento, na.rm = TRUE) * 100
svymean(~COR_RACA_DESC, delineamento, na.rm = TRUE) * 100
svymean(~NIVEL_INSTRUCAO_DESC, delineamento, na.rm = TRUE) * 100
svymean(~COMPOSICAO_FAMILIAR_DESC, delineamento, na.rm = TRUE) * 100
svymean(~UF_DESC, delineamento, na.rm = TRUE) * 100
svymean(~GRANDE_REGIAO_DESC, delineamento, na.rm = TRUE) * 100
svymean(~URBANO_RURAL_DESC, delineamento, na.rm = TRUE) * 100



## Análises descritivas bivariada: TIPO_SEGURANCA_ALIMENTAR e outras variaveis

# Funcao para atribuir variavel
calc_prop <- function(var) {
  svytable(
    as.formula(paste("~ TIPO_SEGURANCA_ALIMENTAR +", var)),
    design = delineamento
  ) |>
    prop.table(margin = 2) |>
    as.data.frame()
}


# Funcao para construir tabela e teste qui-quadrado
bivariada <- function(var) {
  
  tab <- svytable(
    as.formula(paste("~ TIPO_SEGURANCA_ALIMENTAR +", var)),
    design = delineamento
  )
  
  teste <- svychisq(
    as.formula(paste("~ TIPO_SEGURANCA_ALIMENTAR +", var)),
    design = delineamento
  )
  
  list(
    frequencias = tab,
    qui_quadrado = teste
  )
}

# padronizacao de cores dos graficos
cores_seg_alim <- c(
  "Segurança"              = "#2E7D32",  # verde
  "Insegurança leve"       = "#F9A825",  # amarelo
  "Insegurança moderada"   = "#EF6C00",  # laranja
  "Insegurança grave"      = "#C62828"   # vermelho
)


# cria tema padronizado dos graficos
tema_artigo <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )


# label padronizado do grafico
labs(fill = "Situação de Segurança Alimentar")


##### analise da inseguranca alimentar por faixa de renda
bivariada("FAIXA_RENDA_PC")


# Cria tabela com participacao % de SEGURANCA ALIMENTAR por FAIXA RENDA Per Capita
perfil_plot <- svytable(
  ~TIPO_SEGURANCA_ALIMENTAR + FAIXA_RENDA_PC,
  delineamento
) |>
  prop.table(margin = 2) |>
  as.data.frame()


# Mostra tabela abela com participacao % de SEGURANCA ALIMENTAR por FAIXA RENDA Per Capita
print(perfil_plot)


# cria grafico de barras empilhadas (100%) Perfil por renda
ggplot(perfil_plot,
       aes(x = FAIXA_RENDA_PC,
           y = Freq * 100,
           fill = TIPO_SEGURANCA_ALIMENTAR)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = cores_seg_alim) +
  labs(
    x = "Perfil por renda per capita",
    y = "Percentual (%)",
    title = "Distribuição da Segurança Alimentar segundo o perfil da renda"
  ) +
  tema_artigo + 
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1)
  )



##### analise da inseguranca alimentar por faixa de idade
bivariada("FAIXA_IDADE")

# Cria tabela com participacao % de SEGURANCA ALIMENTAR por faixa de idade
perfil_plot <- svytable(
  ~TIPO_SEGURANCA_ALIMENTAR + FAIXA_IDADE,
  delineamento
) |>
  prop.table(margin = 2) |>
  as.data.frame()


# Mostra tabela abela com participacao % de SEGURANCA ALIMENTAR por faixa de idade
print(perfil_plot)


# cria grafico de barras empilhadas (100%) Perfil por faixa de idade
ggplot(perfil_plot,
       aes(x = FAIXA_IDADE,
           y = Freq * 100,
           fill = TIPO_SEGURANCA_ALIMENTAR)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = cores_seg_alim) +
  labs(
    x = "Perfil por faixa de idade",
    y = "Percentual (%)",
    title = "Distribuição da Segurança Alimentar segundo a faixa de idade"
  ) +
  tema_artigo + 
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1)
  )



##### analise da inseguranca alimentar por faixa de idade
bivariada("SEXO_DESC")

# Cria tabela com participacao % de SEGURANCA ALIMENTAR por faixa de idade
perfil_plot <- svytable(
  ~TIPO_SEGURANCA_ALIMENTAR + SEXO_DESC,
  delineamento
) |>
  prop.table(margin = 2) |>
  as.data.frame()


# Mostra tabela abela com participacao % de SEGURANCA ALIMENTAR por faixa de idade
print(perfil_plot)


# cria grafico de barras empilhadas (100%) Perfil por faixa de idade
ggplot(perfil_plot,
       aes(x = SEXO_DESC,
           y = Freq * 100,
           fill = TIPO_SEGURANCA_ALIMENTAR)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = cores_seg_alim) +
  labs(
    x = "Perfil por Sexo",
    y = "Percentual (%)",
    title = "Distribuição da Segurança Alimentar segundo o Sexo"
  ) +
  tema_artigo + 
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1)
  )



##### analise da inseguranca alimentar por Cor e Raça
bivariada("COR_RACA_DESC")

# Cria tabela com participacao % de SEGURANCA ALIMENTAR por Cor e Raça
perfil_plot <- svytable(
  ~TIPO_SEGURANCA_ALIMENTAR + COR_RACA_DESC,
  delineamento
) |>
  prop.table(margin = 2) |>
  as.data.frame()


# Mostra tabela abela com participacao % de SEGURANCA ALIMENTAR por Cor e Raça
print(perfil_plot)


# cria grafico de barras empilhadas (100%) Perfil por Cor e Raça
ggplot(perfil_plot,
       aes(x = COR_RACA_DESC,
           y = Freq * 100,
           fill = TIPO_SEGURANCA_ALIMENTAR)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = cores_seg_alim) +
  labs(
    x = "Perfil por Cor e Raça",
    y = "Percentual (%)",
    title = "Distribuição da Segurança Alimentar segundo Cor e Raça"
  ) +
  tema_artigo + 
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1)
  )



##### analise da inseguranca alimentar por nivel de instrucao
bivariada("NIVEL_INSTRUCAO_DESC")

# Cria tabela com participacao % de SEGURANCA ALIMENTAR por nivel de instrucao
perfil_plot <- svytable(
  ~TIPO_SEGURANCA_ALIMENTAR + NIVEL_INSTRUCAO_DESC,
  delineamento
) |>
  prop.table(margin = 2) |>
  as.data.frame()


# Mostra tabela abela com participacao % de SEGURANCA ALIMENTAR por nivel de instrucao
print(perfil_plot)


# cria grafico de barras empilhadas (100%) Perfil por nivel de instrucao
ggplot(perfil_plot,
       aes(x = NIVEL_INSTRUCAO_DESC,
           y = Freq * 100,
           fill = TIPO_SEGURANCA_ALIMENTAR)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = cores_seg_alim) +
  labs(
    x = "Perfil por nivel de instrucao",
    y = "Percentual (%)",
    title = "Distribuição da Segurança Alimentar segundo o nivel de instrucao"
  ) +
  tema_artigo + 
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1)
  )


##### analise da inseguranca alimentar pela composicao familiar
bivariada("COMPOSICAO_FAMILIAR_DESC")

# Cria tabela com participacao % de SEGURANCA ALIMENTAR pela composicao familiar
perfil_plot <- svytable(
  ~TIPO_SEGURANCA_ALIMENTAR + COMPOSICAO_FAMILIAR_DESC,
  delineamento
) |>
  prop.table(margin = 2) |>
  as.data.frame()


# Mostra tabela abela com participacao % de SEGURANCA ALIMENTAR pela composicao familiar
print(perfil_plot)


# cria grafico de barras empilhadas (100%) Perfil pela composicao familiar
ggplot(perfil_plot,
       aes(x = COMPOSICAO_FAMILIAR_DESC,
           y = Freq * 100,
           fill = TIPO_SEGURANCA_ALIMENTAR)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = cores_seg_alim) +
  labs(
    x = "Perfil pela composicao familiar",
    y = "Percentual (%)",
    title = "Distribuição da Segurança Alimentar segundo a composicao familiar"
  ) +
  tema_artigo + 
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1)
  )



##### analise da inseguranca alimentar pela UF
bivariada("UF_DESC")

# Cria tabela com participacao % de SEGURANCA ALIMENTAR pela UF
perfil_plot <- svytable(
  ~TIPO_SEGURANCA_ALIMENTAR + UF_DESC,
  delineamento
) |>
  prop.table(margin = 2) |>
  as.data.frame()


# Mostra tabela abela com participacao % de SEGURANCA ALIMENTAR pela UF
print(perfil_plot)


# cria grafico de barras empilhadas (100%) Perfil pela UF
ggplot(perfil_plot,
       aes(x = UF_DESC,
           y = Freq * 100,
           fill = TIPO_SEGURANCA_ALIMENTAR)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = cores_seg_alim) +
  labs(
    x = "Perfil pela UF",
    y = "Percentual (%)",
    title = "Distribuição da Segurança Alimentar segundo a UF"
  ) +
  tema_artigo + 
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1)
  )



##### analise da inseguranca alimentar por Regiao
bivariada("GRANDE_REGIAO_DESC")

# Cria tabela com participacao % de SEGURANCA ALIMENTAR por Regiao
perfil_plot <- svytable(
  ~TIPO_SEGURANCA_ALIMENTAR + GRANDE_REGIAO_DESC,
  delineamento
) |>
  prop.table(margin = 2) |>
  as.data.frame()


# Mostra tabela abela com participacao % de SEGURANCA ALIMENTAR por Regiao
print(perfil_plot)


# cria grafico de barras empilhadas (100%) Perfil por Regiao
ggplot(perfil_plot,
       aes(x = GRANDE_REGIAO_DESC,
           y = Freq * 100,
           fill = TIPO_SEGURANCA_ALIMENTAR)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = cores_seg_alim) +
  labs(
    x = "Perfil por Regiao",
    y = "Percentual (%)",
    title = "Distribuição da Segurança Alimentar segundo a Regiao"
  ) +
  tema_artigo + 
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1)
  )



##### analise da inseguranca alimentar pela situação regional
bivariada("URBANO_RURAL_DESC")

# Cria tabela com participacao % de SEGURANCA ALIMENTAR pela situação regional
perfil_plot <- svytable(
  ~TIPO_SEGURANCA_ALIMENTAR + URBANO_RURAL_DESC,
  delineamento
) |>
  prop.table(margin = 2) |>
  as.data.frame()


# Mostra tabela abela com participacao % de SEGURANCA ALIMENTAR pela situação regional
print(perfil_plot)


# cria grafico de barras empilhadas (100%) Perfil pela situação regional
ggplot(perfil_plot,
       aes(x = URBANO_RURAL_DESC,
           y = Freq * 100,
           fill = TIPO_SEGURANCA_ALIMENTAR)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = cores_seg_alim) +
  labs(
    x = "Perfil pela situação regional",
    y = "Percentual (%)",
    title = "Distribuição da Segurança Alimentar segundo a situação regional"
  ) +
  tema_artigo + 
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1)
  )







