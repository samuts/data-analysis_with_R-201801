# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse
library(tidyverse)

# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 
DF_TED <- read_csv("aula-05/data/ted_main.csv.gz")

# Visualize o resumo dos dados do dataframe. 
# Verifique os mínimos, máximos, médias e medianas das variáveis numéricas.
# As variáveis duration, film_date e published_date estão no tipo de dados apropriado?
summary(DF_TED)
DF_TED %>% select(duration, film_date, published_date)

# Converta as seguintes variáveis utilizando o pacote Lubridate:
#     * duration, para duração (em segundos). Experimente utilizar as funções as.duration e duration. Mantenha aquela que considerar mais apropriada.
#     * film_date, para data, com a função as_datetime.
#     * published_date, para data, com a função as_datetime..
DF_TED <- DF_TED %>% mutate(duration = as.duration(duration),
                            film_date = as_datetime(film_date),
                            published_date = as_datetime(published_date))

# Converta as seguintes variáveis character para variáveis categóricas com a função factor.
#     * event
#     * speaker_occupation
DF_TED <- DF_TED %>% mutate(event = as.factor(event),
                            speaker_occupation = as.factor(speaker_occupation))

# Retire do dataframe a variável name
DF_TED <- DF_TED %>% select(- name)

# Visualize novamente o resumo dos dados do dataframe. 
# Verifique os mínimos, máximos, médias e medianas das variáveis numéricas. 
# Verifique as contagens das variáveis categóricas
summary(DF_TED)

# Verifique quais registros possuem a menor quantidade de línguas. 
# Corrija para que possuam no mínimo 1 idioma.
DF_TED <- DF_TED %>% mutate(languages = if_else(languages == 0, 1L, languages))

# Verifique os 15 registros com menor data de filmagem. 
DF_TED %>% arrange(desc(film_date)) %>%
           tail(15) %>%
           View()

# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo
DF_ANO_FILMAGEM <- DF_TED %>% mutate(AnoFilmagem = year(film_date)) %>%
                              group_by(AnoFilmagem) %>%
                              count(AnoFilmagem) %>%
                              ungroup()

# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.
QUANTIS <- quantile(DF_ANO_FILMAGEM$n, probs = seq(from = 0.1, 
                                                   to = 1, 
                                                   by = 0.1
                                                )
           )

DF_ANOS_MAIORES <- DF_ANO_FILMAGEM %>%
  filter(n > 33) %>%
  pull(AnoFilmagem)

DF_TED <- DF_TED %>%
  filter(year(film_date) %in% DF_ANOS_MAIORES)

# Verifique novamente o resumo dos dados do dataframe
summary(DF_TED)

# Verifique os 10 registros com maior duração.
DF_TED %>% arrange(desc(duration)) %>%
           head(10) %>%
           View()

# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas
mean(DF_TED$duration) + (sd(DF_TED$duration) * 3)
DF_TED %>% filter(duration > as.duration((mean(duration) + (sd(duration) * 3)))) %>%
           View()

# Calcule os 4 quartis e o IQR da duração das apresentações.
# Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil
QUARTIL <- quantile(as.numeric(DF_TED$duration),
         probs = seq(from = 0, to = 1, by = 0.25))

IQR <- IQR(DF_TED$duration)

DF_TED %>% filter(as.numeric(duration) > ((IQR * 1.5) + Quartil[4])) %>%
           View()

# Visualize os 10 quantis da quantidade de visualizações
quantile(DF_TED$views, probs = seq(from = 0.1, to = 1, by = 0.1))

# Compare as seguintes estatísticas descritivas da quantidade de visualizações:
#   * Média e Mediana. Qual é maior?
#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?
mean(DF_TED$views) > median(DF_TED$views) #Media maior
median(abs(DF_TED$views - median(DF_TED$views))) > sd(DF_TED$views) #Desvio Padrão maior
IQR(DF_TED$views) / median(abs(DF_TED$views - median(DF_TED$views))) #2.192295

# RESPOSA: Não estão

# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de línguas dos seguintes grupos:
#     * 10% de vídeos com maior número de visualizações
#     * 10% de vídeos com menor número de visualizações
GRUPO_1 <- DF_TED %>% arrange(desc(views)) %>%
           head(abs(nrow(DF_TED)) / 10) 

mean(GRUPO_1$languages)
sd(GRUPO_1$languages)
median(GRUPO_1$languages)
IQR(GRUPO_1$languages)

GRUPO_2 <- DF_TED %>% arrange(desc(views)) %>%
                      tail(abs(nrow(DF_TED)) / 10)

mean(GRUPO_2$languages)
sd(GRUPO_2$languages)
median(GRUPO_2$languages)
IQR(GRUPO_2$languages)

# Determine a quantidade de apresentações por evento cujo nome inicie com TED. 
# Utilize a função str_detect para este filtro
DF_TED %>% mutate(TED = str_detect(event, "^TED")) %>%
           filter(TED == TRUE) %>%
           count(event) %>%
           View()

# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos vídeos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentações resultante do filtro, por evento
#   * o ano do evento (utilizar o menor ano da data de publicação)
#   * a quantidade média de línguas das apresentações
#   * o desvio padrão da quantidade de línguas
#   * o coeficiente de variação da quantidade de línguas
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES
DF_TED %>%  group_by(event) %>%
            filter(str_detect(event, "^TED") == TRUE) %>%
            filter(views > median(views)) %>%
            summarise(Quantidade_Apresentações = n(),
                      Ano_Evento = min(year(published_date)),
                      Média_Linguas = mean(languages),
                      Desvio_Padrão_Linguas = sd(languages),
                      Coeficiente_Variação = Desvio_Padrão_Linguas / Média_Linguas) %>%
            ungroup() %>%
            filter(Quantidade_Apresentações > 10) %>%
            View()

# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de línguas
#     * Quantidade de visualizações e Duração
#     * Quantidade de visualizações e Quantidade de Comentários
#     * Quantidade de Comentários e Quantidade de línguas
cor(x = DF_TED$views, y = DF_TED$languages)
cor(x = DF_TED$views, y = DF_TED$duration)
cor(x = DF_TED$views, y = DF_TED$comments)
cor(x = DF_TED$comments, y = DF_TED$languages) 

# Descarte os vídeos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas
DF_TED_2 <- DF_TED %>%  filter(duration < as.duration((mean(duration) + (sd(duration) * 3)))) 

cor(x = DF_TED_2$views, y = DF_TED_2$languages)
cor(x = DF_TED_2$views, y = DF_TED_2$duration)
cor(x = DF_TED_2$views, y = DF_TED_2$comments)
cor(x = DF_TED_2$comments, y = DF_TED_2$languages)

# Utilizando o data frame original, crie um dataframe com a mediana da duração dos vídeos por ano de filmagem. 
# Calcule a correlação entre o ano e a mediana da duração
# e interprete o resultado
DF_TED_3 <- read_csv("aula-05/data/ted_main.csv.gz")

DF_TED_4 <- DF_TED_3 %>% group_by(AnoFilmagem = year(as_datetime(film_date))) %>%
             summarise(MedianaDuração = median(as.duration(duration))) %>%
             ungroup() -> DiferenteDF

cor(x = DF_TED_4$AnoFilmagem, y = DF_TED_4$MedianaDuração)