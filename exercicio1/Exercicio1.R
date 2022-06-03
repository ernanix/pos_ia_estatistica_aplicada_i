
#Estatística Aplicada I 
#Primeira Lista de Exercícios
#Com a base de dados “realestateiaa” obter os seguintes resultados com o 
#auxílio do “R”

load("C:/Users/escneto/Documents/Estudos/Pos_IA_UFPR/Estatistica_Aplicada/exercicio1/realestateiaa.RData")
view (realestateiaa)

#a) Elaborar o histograma e o boxplot das variáveis “parea e tarea”. 

install.packages("RcmdrMisc")
library (RcmdrMisc)

X11(width = 10, height = 12)
with(realestateiaa, Hist(parea, scale="frequency", breaks="sturges", 
                    col="darkgray"))

X11(width = 10, height = 12)
with(realestateiaa, Hist(tarea, scale="frequency", breaks="sturges", 
                         col="darkgray"))

X11(width = 10, height = 12)
Boxplot( ~ parea, data=realestateiaa, id=list(method="y"))

X11(width = 10, height = 12)
Boxplot( ~ tarea, data=realestateiaa, id=list(method="y"))

#b) Elaborar a tabela de distribuição de frequências da variável “price” 
#(preço dos imóveis); 

install.packages("fdth")

library (fdth)

table <- fdt (realestateiaa$price)

print (table)

#c) Para a variável “price” calcular os seguintes indicadores: média; mediana; 
#moda; variância; desvio padrão; CV–Coeficiente de Variação; Quartis; 
#distância interquartílica; percentis. 

#Média
mean(realestateiaa$price)
#Mediana
median (realestateiaa$price)
#Moda
table(realestateiaa$price)
subset(table(realestateiaa$price), 
       table(realestateiaa$price) == max(table(realestateiaa$price)))
#Variância
var(realestateiaa$price)
#Desvio Padrao
sd (realestateiaa$price)
#Coeficiente de Variação
(sd(realestateiaa$price) / mean(realestateiaa$price)) * 100
#Quartis
quantile(realestateiaa$price, probs = 0.25)
quantile(realestateiaa$price, probs = 0.50)
quantile(realestateiaa$price, probs = 0.75)
#Distância Interquartílica
IQR(realestateiaa$price)
#Percentis
quantile(realestateiaa$price, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) 

#d) Estimar o intervalo de confiança para a média da variável “price” com 95% de 
#confiança 
install.packages("BSDA")
library(BSDA)

z.test(realestateiaa$price, y = NULL, alternative = "two.sided", mu = 0, sigma.x = sd(realestateiaa$price),
       sigma.y = NULL, conf.level = 0.95)


#e) Fazer o teste de diferença entre médias para as variáveis “parea” e “tarea”. 

z.test(realestateiaa$parea, realestateiaa$tarea, alternative = "two.sided", mu = 0, sigma.x = sd(realestateiaa$parea),
       sigma.y = sd(realestateiaa$tarea), conf.level = 0.95)

#f) Fazer o teste de diferença entre variâncias para as variáveis “parea” 
#e “tarea”. 

#g) Fazer o Teste de Wilcoxon-Mann-Whitney para amostras independentes para as 
#variáveis “parea” e “tarea”. 


#h) Fazer 2 testes de normalidade (a sua escolha) para a variável “price”.
