# Descrição dos dados: https://tech.instacart.com/3-million-instacart-orders-open-sourced-d40d29ead6f2
# Estamos trabalhando com somente uma amostra do total de pedidos. O dataset abaixo não possui 3 milhões de pedidos ;)
library( tidyverse )

departments <- read_csv("project/departments.csv")                   # Cadastro de Departamentos
aisles <- read_csv("project/aisles.csv")                             # Cadastro de "Corredores"
products <- read_csv("project/products.csv")                         # Cadastro de Produtos

insta_orders <- read_csv( "project/orders_instacart.csv" )           # Amostra de pedidos de usuários
insta_products <- read_csv( "project/order_products_instacart.csv" ) # Produtos que compõe os pedidos


#1 # Quantos dos produtos do cadastro nunca foram comprados?
anti_join(products, insta_products, by = "product_id" ) %>% count()

#2 # Crie um dataframe com os dados combinados de produtos, corredores e departamentos. 
df_PCD <- products %>%
            inner_join(aisles, by = "aisle_id") %>%
            inner_join(departments, by = "department_id")

#3 # Quais as 10 combinações corredor + departamento que possuem mais produtos cadastrados? Use o dataframe da atividade #2.
df_PCD %>%
    select(aisle_id, aisle, department_id, department) %>%
    group_by(aisle_id, aisle, department_id, department) %>%
    count(sort = TRUE) %>%
    head(10) -> top_10_combinacoes_ids

#4 # Qual o percentual de pedidos que possuem algum produto dos pares 'corredor + departamento' da atividade anterior?
products %>%
    inner_join(top_10_combinacoes_ids, by="aisle_id","department_id") %>%
    select(product_id) %>%
    distinct(product_id) %>%
    inner_join(insta_products, by="product_id") %>%
    select(order_id) %>%
    distinct(order_id) -> orders_com_top_10_combinacoes

insta_products %>%
    select(order_id) %>%
    distinct(order_id) -> orders_todas

quantidade_total_orders <- orders_todas %>%
                                count()
quantidade_top_10_combinacoes <- orders_com_top_10_combinacoes %>%
                                count()

print((quantidade_top_10_combinacoes / quantidade_total_orders) * 100, digits = 4)

#5 # Crie um novo dataframe de produtos em pedidos retirando aqueles produtos que não estão categorizados (usar resultado das atividades 3 e 4)
df_PNC <- insta_products %>%
    left_join(products, by="product_id") %>%
    left_join(aisles, by="aisle_id") %>%
    left_join(departments, by="department_id") %>%
    filter(department != "missing" | aisle != "missing")

#6 # Crie um dataframe que combine todos os dataframes através das suas chaves de ligação. Para produtos de pedidos, use o dataframe da atividade 4
   df_Orders <- insta_orders %>%
                left_join(df_PNC, by="order_id")
   # Transforme as variáveis user_id, department e aisle em factor
   df_Orders$user_id = as.factor(df_Orders$user_id)
   df_Orders$department = as.factor(df_Orders$department)
   df_Orders$aisle = as.factor(df_Orders$aisle)
   
   # Transforme a variável order_hour_of_day em um factor ordenado (ordered)
   df_Orders$order_hour_of_day = factor(df_Orders$order_hour_of_day, ordered=T)

   # Este dataframe deverá ser utilizado em todas as atividades seguintes
   

#7 # Identifique os 5 horários com maior quantidade de usuários que fizeram pedidos
   var_hora_do_dia <- df_Orders  %>%
       select(user_id, order_id, order_hour_of_day) %>%
       group_by(order_hour_of_day) %>%
       distinct(user_id, order_id, order_hour_of_day)
   
   top_horas <- var_hora_do_dia %>%
       group_by(order_hour_of_day) %>%
       count(sum = n()) %>%
       arrange(desc(sum)) %>%
       head(5)

#8 # Quais os 15 produtos mais vendidos nestes 5 horários? Identifique os produtos e a quantidade total nestes horários (total geral, não por hora)
   var_top15_produtos <- df_Orders %>%
       filter(order_hour_of_day %in% as.factor(top_horas$order_hour_of_day)) %>%
       group_by(product_id, product_name) %>%
       count(n = n()) %>%
       arrange(desc(n)) %>%
       head(15)

#9 # Calcule a média de vendas por hora destes 15 produtos ao longo do dia,
   
   # e faça um gráfico de linhas mostrando a venda média por hora destes produtos.
   library(ggplot2)
   var_top_horas2 <- df_Orders %>% filter(product_id %in% as.factor(var_top15_produtos$product_id))  %>%
       group_by(order_hour_of_day, product_name) %>%
       summarise(Mean = mean(order_id))
   
   # Utilize o nome do produto para legenda da cor da linha.
   ggplot(var_top_horas2, aes(x=order_hour_of_day, y=Mean)) +
       geom_line(aes(group=product_name, color=product_name))
   
   # Você consegue identificar algum produto com padrão de venda diferente dos demais? 
   # >>>>>>>>>>>>>    Sim, tem 2 produtos na hora 03.

#10 # Calcule as seguintes estatísticas descritivas sobre a quantidade de pedidos por dia, para cada hora do dia. O resultado final deve ser exibido para cada hora do dia:
    # Média, Desvio Padrão, Mediana, Mínimo e Máximo
   var_estatisticas_orders <- df_Orders %>%
       group_by(order_hour_of_day) %>%
       summarise(mean = mean(order_id), 
                 sd = sd(order_id), 
                 max = max(order_id), 
                 min = min(order_id), 
                 order_count = n()) %>%
       ungroup()
   
   ggplot(var_estatisticas_orders, aes(x=order_hour_of_day, y=order_count)) +
       geom_bar(stat="identity")
   
    # Considerando os valores calculados, você acredita que a distribuição por hora é gaussiana? 
    # >>>>>>> Sim, demonstra gaussiana.

#11 # Faça um gráfico da média de quantidade de produtos por hora, com 1 desvio padrão para cima e para baixo em forma de gráfico de banda
   var_estatisticas_products <- df_Orders %>%
       group_by(order_hour_of_day) %>%
       count(product_id) %>%
       mutate(low = mean(n) - 2 * sd(n), 
              hi = mean(n) + 2 * sd(n), 
              meanProdQuant = mean(n),
              sumProdQuant = sum(n),
              sdProdQuant = sd(n), 
              product_count = n())
   
   ggplot(var_estatisticas_products, aes(x=order_hour_of_day, y=meanProdQuant, ymin=low, ymax=hi, group=1)) +
       geom_line() + 
       geom_ribbon(fill = "lightgray", alpha = 0.5) + 
       geom_jitter(alpha = .2, height = 0, width = 0.3) +
       theme_bw()
   

#12 # Visualize um boxplot da quantidade de pedidos por hora nos 7 dias da semana. O resultado deve ter order_dow como eixo x.
   var_ggplot <- df_Orders %>%
       group_by(order_dow) %>%
       count(order_id) -> t
   
   ggplot(var_ggplot, aes(x=order_dow, group=order_dow)) +
       geom_boxplot(aes(y=n)) +
       scale_x_continuous( breaks = 0:6 ) +
       scale_y_continuous(labels = scales::format_format(big.mark = ".", decimal.mark=",", scientific = FALSE)) +
       labs( x = "Dia da Semana"
             , y = "Quantidade de Pedidos") +
       theme_bw()

#13 # Identifique, por usuário, o tempo médio entre pedidos
   var_tempo_medio <- df_Orders %>%
       group_by(user_id) %>%
       summarise(meanTime = mean(days_since_prior_order))

#14 # Faça um gráfico de barras com a quantidade de usuários em cada tempo médio calculado
   ggplot(var_tempo_medio, aes(x=meanTime)) +
       geom_bar( fill="blue", color = "blue", alpha=0.6 ) +
       labs( x = "Tempo Médio"
             , y = "Quantidade de usuários" )

#15 # Faça um gráfico de barras com a quantidade de usuários em cada número de dias desde o pedido anterior. Há alguma similaridade entre os gráficos das atividades 14 e 15? 
   ggplot(df_Orders, aes(x=days_since_prior_order)) +
       geom_bar( fill="blue", color = "blue", alpha=0.6 ) +
       labs( x = "Tempo Médio"
             , y = "Quantidade de usuários" )
   
   # Sim, são similares.
   

#16 # Repita o gráfico da atividade 14 mantendo somente os usuários com no mínimo 5 pedidos. O padrão se mantém?
   var_usuarios_com_min_5_pedidos <- df_Orders %>%
       group_by(user_id)%>%
       count(order_id)%>%
       filter(n>4)
   
   var_ggplot <- df_Orders %>%
       inner_join(var_usuarios_com_min_5_pedidos, by = "user_id")%>%
       group_by(user_id)%>%
       summarise(meanTime = mean(days_since_prior_order))
   
   ggplot(var_ggplot,  aes( x = meanTime)) +
       geom_bar( fill="blue", color = "blue", alpha=0.6 ) +
       labs( x = "Tempo Médio"
             , y = "Quantidade de usuários" )

#17 # O vetor abaixo lista todos os IDs de bananas maduras em seu estado natural.
    # Utilizando este vetor, identifique se existem pedidos com mais de um tipo de banana no mesmo pedido.
   var_ids_bananas <- c(24852, 13176, 39276, 37067, 29259)
   
   var_bananas_ids_orders_ids <- df_Orders %>%
       filter(product_id %in% as_vector(var_ids_bananas)) %>%
       group_by(order_id) %>%
       distinct(product_id)
   
   var_conta_orders <- var_bananas_ids_orders_ids %>%
       count() %>%
       filter(n>1) %>%
       ungroup() %>%
       count()
   
   print(paste(paste("Possuem ", var_conta_orders), " orders que compraram um ou mais de um tipo de banana"))

#18 # Se existirem, pedidos resultantes da atividade 17, conte quantas vezes cada tipo de banana aparece nestes pedidos com mais de um tipo de banana.
    # Após exibir os tipos de banana, crie um novo vetor de id de bananas contendo somente os 3 produtos de maior contagem de ocorrências
   var_top3_ids_bananas <- var_bananas_ids_orders_ids %>%
      group_by(product_id) %>%
      count() %>%
      arrange(desc(n)) %>%
      head(3)
   
   var_top3_ids_bananas2 = as_vector(var_top3_ids_bananas$product_id)

#19 # Com base no vetor criado na atividade 18, conte quantos pedidos de, em média, são feitos por hora em cada dia da semana. 
   var_pedidos_por_hora_por_dia_semana <- df_Orders %>%
      filter(product_id %in% var_top3_ids_bananas2) %>%
      group_by(order_dow, order_hour_of_day) %>%
      count(product_id) %>%
      summarise(meanCount = mean(n))

#20 # Faça um gráfico dos pedidos de banana da atividade 19. O gráfico deve ter o dia da semana no eixo X, a hora do dia no eixo Y, 
    # e pontos na intersecção dos eixos, onde o tamanho do ponto é determinado pela quantidade média de pedidos de banana 
    # nesta combinação de dia da semana com hora
   ggplot(var_pedidos_por_hora_por_dia_semana, aes(x=order_dow, y=order_hour_of_day, size=meanCount)) +
      geom_point()

#21 # Faça um histograma da quantidade média calculada na atividade 19, facetado por dia da semana
   ggplot(var_pedidos_por_hora_por_dia_semana, aes(x=order_hour_of_day, y=meanCount)) +
      geom_histogram(stat="Identidade") +
      facet_wrap(~order_dow, ncol=3)

#22 # Teste se há diferença nas vendas por hora entre os dias 3 e 4 usando o teste de wilcoxon e utilizando a simulação da aula de testes
   var_dias <- var_pedidos_por_hora_por_dia_semana %>%
      filter(order_dow %in% c(3,4))
   
   wilcox.test(meanCount ~ order_dow, 
               data = var_dias, 
               alternative = "two.sided",
               subset = order_dow %in% c(3, 4), 
               conf.int = TRUE)
   
   pairwise.wilcox.test(var_dias$meanCount, 
                        var_dias$order_dow, 
                        p.adjust.method = "BH")
   
   ggplot(var_dias, aes(x=order_hour_of_day, 
                             y=meanCount, 
                             group=as.factor(order_dow),
                             color=as.factor(order_dow))) +
      labs( x = "Horas ao longo do dia", 
            y = "Media", 
            color="Dia da semana" ) +
      geom_line()
