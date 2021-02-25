library(tidyverse)
library(openxlsx)
library(scales)
library(ggthemes)

# Ler base de dados
df <- read.xlsx("data/execucao_orcamentaria.xlsx")

# Tirar notação científica
options(scipen = 999)          
`%notin%` <- Negate(`%in%`)

# Limpar base de dados

# 2009 - 2012
df_2010_2011 <- filter(df, Ano %in% c(2010, 2011)) #%>%
  filter(`Ação` %in% c("8893 - Apoio à Organização e Gestão do Sistema Único de Assistência Social - SUAS",
                       "8937 - Serviço de Vigilância Social no Território",
                       "8249 - Funcionamento dos Conselhos de Assistência Social") |
           Programa %in% c("1335 - Transferência de Renda com Condicionalidades - Bolsa Família",
                         "1384 - Proteção Social Básica",
                         "1385 - Proteção Social Especial"))  %>%
  mutate(area = case_when(Programa == "1335 - Transferência de Renda com Condicionalidades - Bolsa Família" ~ "PBF",
                          `Ação` %in% c("0573 - Benefício de Prestação Continuada da Assistência Social à Pessoa Idosa",
                                        "0575 - Benefício de Prestação Continuada da Assistência Social à Pessoa com Deficiência",
                                        "2583 - Serviço de Processamento de Dados do Benefício de Prestação Continuada e da Renda Mensal Vitalícia",
                                        "2589 - Avaliação e Operacionalização do Benefício de Prestação Continuada da Assistência Social e Manutenção da Renda Mensal Vitalícia",
                                        "0561 - Renda Mensal Vitalícia por Idade",
                                        "0565 - Renda Mensal Vitalícia por Invalidez") ~ "BPC",
                          TRUE ~ "SUAS")) %>%
  group_by(Ano, area) %>%
  summarise(Liquidado = sum(Liquidado))

## 2012 a 2015
df_2012_2015 <- filter(df, Ano %in% c(2012:2015)) %>% 
  filter(Programa %in% c("2019 - Bolsa Família", "2037 - Fortalecimento do Sistema Único de Assistência Social (SUAS)")) %>%
  mutate(area = case_when(`Ação` %in% c("0573 - Benefício de Prestação Continuada da Assistência Social à Pessoa Idosa",
                                        "0575 - Benefício de Prestação Continuada da Assistência Social à Pessoa com Deficiência",
                                        "2583 - Serviço de Processamento de Dados do Benefício de Prestação Continuada (BPC) e da Renda Mensal Vitalícia (RMV)",
                                        "2589 - Avaliação e Operacionalização do Benefício de Prestação Continuada da Assistência Social (BPC) e Manutenção da Renda Mensal Vitalícia (RMV)") ~ "BPC",
                          str_detect(`Ação`, "2583") ~ "BPC",
                          str_detect(Objetivo, "0371") ~ 'BPC',
                          Programa == "2019 - Bolsa Família" ~ "PBF",
                          TRUE ~ "SUAS")) %>%
  group_by(Ano, area) %>%
  summarise(Liquidado = sum(Liquidado))

## 2016 a 2019
df_2016_2019 <- filter(df, Ano %in% c(2016:2019)) %>%
  filter(Programa %in% c("2019 - Inclusão social por meio do Bolsa Família, do Cadastro Único e da articulação de políticas sociais",
                         "2037 - Consolidação do Sistema Único de Assistência Social (SUAS)")) %>%
  mutate(area = case_when(`Ação` %in% c("0573 - Benefício de Prestação Continuada da Assistência Social à Pessoa Idosa",
                                        "0575 - Benefício de Prestação Continuada da Assistência Social à Pessoa com Deficiência",
                                        "2583 - Serviço de Processamento de Dados do Benefício de Prestação Continuada (BPC) e da Renda Mensal Vitalícia (RMV)",
                                        "2589 - Avaliação e Operacionalização do Benefício de Prestação Continuada da Assistência Social (BPC) e Manutenção da Renda Mensal Vitalícia (RMV)") ~ "BPC",
                          str_detect(`Ação`, "2583") ~ "BPC",
                          str_detect(Objetivo, "0371") ~ 'BPC',
                          Programa == "2019 - Inclusão social por meio do Bolsa Família, do Cadastro Único e da articulação de políticas sociais" ~ "PBF",
                          TRUE ~ "SUAS")) %>%
  group_by(Ano, area) %>%
  summarise(Liquidado = sum(Liquidado))

## 2020
df_2020 <- filter(df, Ano == 2020) %>%
  filter(Programa %in% c("5028 - Inclusão Social por meio do Bolsa Família e da Articulação de Políticas Públicas",
                         "5031 - Proteção Social no âmbito do Sistema Único de Assistência Social (SUAS)")) %>%
  filter(!str_detect(`Ação`, "Auxílio Emergencial")) %>%
  mutate(area = case_when(Programa == "5028 - Inclusão Social por meio do Bolsa Família e da Articulação de Políticas Públicas" ~ "PBF",
                          `Ação` %in% c("00H5 - Benefícios de Prestação Continuada (BPC) à Pessoa Idosa e da Renda Mensal Vitalícia (RMV) por Idade",
                                        "00IN - Benefícios de Prestação Continuada (BPC) à Pessoa com Deficiência e da Renda Mensal Vitalícia (RMV) por Invalidez",
                                        "2583 - Processamento de Dados do Benefício de Prestação Continuada (BPC) e da Renda Mensal Vitalícia (RMV)",
                                        "2589 - Avaliação e Operacionalização do Benefício de Prestação Continuada da Assistência Social (BPC) e Manutenção da Renda Mensal Vitalícia (RMV)") ~ "BPC",
                                        TRUE ~ "SUAS")) %>%
  group_by(Ano, area) %>%
  summarise(Liquidado = sum(Liquidado))


df_2010_2020 <- bind_rows(df_2010_2011, df_2012_2015, df_2016_2019, df_2020)

df_2010_2020 %>%
  group_by(area) %>%
  ggplot(aes(x=Ano, y=Liquidado, color=area)) +
  scale_x_continuous(breaks = seq(2010, 2020, by=1)) +
  scale_y_continuous(labels = unit_format(unit = "Bi", scale = 1e-9)) +
  geom_line() +
  geom_point() +
  theme_fivethirtyeight() +
  labs(title = "Dados do orçamento liquidado das áreas do BPC, PBF e SUAS\n2010 a 2019",
       color = "Área")

write.xlsx(df_2010_2020, "resultado.xlsx", asTable = FALSE)
