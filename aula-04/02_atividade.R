library(tidyverse)
library(lubridate)

## Nesta atividade você deve utilizar o resultado do exercício 01 da Atividade da aula 03 (remuneração em dólares convertida para reais)
## Utilize o código daquele exercício como ponto de partida para esta atividade. 
## Sempre utilize o caminho relativo, não o caminho absoluto, pois não funcionará na correção do exercício.

### IMPORTANTE ###
## Se você utilizar alguma função própria ou do material de aula, o código da(s) função(ões) deve estar neste arquivo da atividade.


### 1 ####
## 
## Correlação de ano de ingresso por cargo
## - Determine o coeficiente de correlação entre o tempo em anos desde a DATA_INGRESSO_ORGAO e o tempo em anos desde a DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO
##   para todos os cargos que possuem no mínimo 200 servidores.
## - Crie uma coluna que determina se a correlação é positiva ou negativa, e outra coluna que define a força da correlação de acordo com 
##   o material visto em aula sobre interpretação do coeficiente.
## - O resultado desta atividade deve ser um Data Frame com as variáveis de Cargo, Coeficiente de Correlação, Direção da Correlação e Força da Correlação
## 
### # ####

salarios <- read_csv("aula-03/data/201802_dados_salarios_servidores.csv.gz")

lista_cargos <- salarios %>% 
  select(DESCRICAO_CARGO) %>% 
  count(DESCRICAO_CARGO) %>%
  filter(n >= 200) %>%
  pull(DESCRICAO_CARGO)

ano_atual <- format(Sys.Date(), "%Y") %>% 
              as.numeric()

salarios <- salarios %>% mutate(TOTAL_SALARIO = REMUNERACAO_REAIS + ( REMUNERACAO_DOLARES * 3.2421),
                                ano_1 = ano_atual - year(DATA_INGRESSO_ORGAO),
                                ano_2 = ano_atual - year(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO),
                                correlacao = cor(ano_2,ano_1) ) %>%
  filter( DESCRICAO_CARGO %in% c(lista_cargos))

print(salarios)

DF_1 <- salarios %>%
  group_by(DESCRICAO_CARGO) %>%
  filter(DESCRICAO_CARGO %in% lista_cargos) %>%
  summarise(coefCorr = cor(x = year(DATA_INGRESSO_ORGAO),
                           y = year(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO)),
            dirCorr = if(coefCorr < 0) {print("Negativa")} else {
                      if(coefCorr > 0) {print("Positiva")
            }},
            forcaCorr = if(abs(coefCorr) < 0.3) {print("Desprezível")} else {
                        if(abs(coefCorr) < 0.5) {print("Fraca")} else {
                        if(abs(coefCorr) < 0.7) {print("Moderada")} else {
                        if(abs(coefCorr) < 0.9) {print("Forte")} else {
                        if(abs(coefCorr) > 0.9) {print("Muito Forte")
            }}}}}) %>%
  ungroup()

### 2 ###
##
## - A partir do dataset do exercício anterior, selecione os 10 cargos de correlação mais forte (seja positiva ou negativa) e os 
##   10 cargos de correlação mais fraca (de novo, independente de ser positiva ou negativa)
## - Para estes 20 cargos, determine a Moda do órgão de lotação (ORGSUP_LOTACAO) e de exercício (ORGSUP_EXERCICIO)
## - Reponda se existe diferença entre as modas e se existe relação entre a Força da Correlação e a diferença entre as modas 
##   (caso haja diferença)
##
### # ###

dezMaioresCorr <- DF_1 %>%
  mutate(coefAbs = abs(coefCorr)) %>%
  arrange(desc(coefAbs)) %>%
  head(10) %>%
  pull(DESCRICAO_CARGO)

dezMenoresCorr <- DF_1 %>%
  mutate(coefAbs = abs(coefCorr)) %>%
  arrange(desc(coefAbs)) %>%
  tail(10) %>%
  pull(DESCRICAO_CARGO)

Cargos_DF_2 <- c(dezMaioresCorr, dezMenoresCorr)

DF_2 <- salarios %>%
  group_by(DESCRICAO_CARGO) %>%
  filter(DESCRICAO_CARGO %in% Cargos_DF_2) %>%
  summarise(modaexercicio = names(sort(table(ORGSUP_EXERCICIO), decreasing = TRUE))[1],
            modalotacao = names(sort(table(ORGSUP_LOTACAO), decreasing = TRUE))[1],
            semelhantes = modalotacao == modaexercicio,
            coefCorr = cor(x = year(DATA_INGRESSO_ORGAO),
                                            y = year(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO)),
            forcaCorr = if(abs(coefCorr) < 0.3) {print("Desprezível")} else {
                        if(abs(coefCorr) < 0.5) {print("Fraca")} else {
                        if(abs(coefCorr) < 0.7) {print("Moderada")} else {
                        if(abs(coefCorr) < 0.9) {print("Forte")} else {
                        if(abs(coefCorr) > 0.9) {print("Muito Forte")}}}}}) %>%
  ungroup()

# COMENTARIO: Se as modas forem diferentes a correlação é desprezível, mas se forem semelhantes dizemos que há uma correlação forte.