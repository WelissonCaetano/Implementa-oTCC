#Script da implementação física do trabalho

#Biblioteca para pode chamar uma função join
library(plyr)

#Função para normalizar as metrica com valores entre 0 e 1
Normaliza = function (dados) {
  maior = max(dados[,2])
  menor = min(dados[,2])
  dados[,2] = (dados[,2] - menor) / (maior - menor)
  return (dados)
}

# Carregamento da base de dados e conversão para o outros 2 tipo da dataframe
base <- as.data.frame(read.csv("C:/temp/cf-sentiment-basic.csv"))
baseI <- as.data.frame(subset(base,judgment == 4))
baseC <- as.data.frame(subset(base,judgment != 4))

#Agregação dos raters de acordo com as caracteristicas: "Inconclusivos = I can't tell", Conclusivos e Todos
Todos <-aggregate(base$rater,by = list(rater = base$rater),length)
Inconclusivos <-aggregate(baseI$rater, by = list(rater = baseI$rater),length)
Conclusivos <-aggregate(baseC$rater,by = list(rater = baseC$rater),length)

#Merge feito para Conclusivos e Resultado para a métrica Pwy.conclusivos
Conclusivos.Pwy <- merge(Todos,Conclusivos, by="rater")
Conclusivos.Pwy$Tryw <- Conclusivos.Pwy$x.y/Conclusivos.Pwy$x.x #Para cada trabalhador, divide a quantidade de respostas conclusivas dele pela quantidade total de respostas dele
Conclusivos.Pwy$pwy<-Conclusivos.Pwy$Tryw/(sum(Conclusivos$x)/length(base$judgment))
Pwy.conclusivos <- Conclusivos.Pwy

#Merge feito para Inconclusivos e Resultado para a métrica Pwi.inconclusivos
Pwy.inconclusivos <-merge(Todos,Inconclusivos,by ="rater")
Pwy.inconclusivos$Tryw <- Pwy.inconclusivos[,3]/Pwy.inconclusivos[,2]
Pwy.inconclusivos$pwi <- Pwy.inconclusivos$Tryw/(sum(Conclusivos$x)/length(base$judgment))

#Calculo de porcentagem feito para Todos e Resultado para a métrica Pwy.todos
Pwy.todos <-aggregate(base$rater, by = list(rater = base$rater),length)
Denominador <- Pwy.todos$
  Denominador <- Pwy.todos$x * 100
Pwy.todos$Percentual <- Pwy.todos$Denominador / sum(Pwy.todos$x)

# cria um subset com a metrica e os rater de Pwy.conclusivos
MetricaPwyConclusivos <- subset(Pwy.conclusivos, select = c("rater","pwy"))
min(MetricaPwyConclusivos$pwy)
max(MetricaPwyConclusivos$pwy)
MetricaPwyConclusivosNormalizada <- Normaliza(MetricaPwyConclusivos)
min(MetricaPwyConclusivosNormalizada$pwy)
max(MetricaPwyConclusivosNormalizada$pwy)

# cria um subset com a metrica e os rater de Pwy.Inconclusivos
MetricaPwyInconclusivos <- subset(Pwy.inconclusivos, select = c("rater","pwi"))
min(MetricaPwyInconclusivos$pwi)
max(MetricaPwyInconclusivos$pwi)
MetricaPwyInconclusivosNormalizada <- Normaliza(MetricaPwyInconclusivos)
min(MetricaPwyInconclusivosNormalizada$pwi)
max(MetricaPwyInconclusivosNormalizada$pwi)

# cria um subset com a metrica e os rater de Pwy.Todos
MetricaPwycTodos <- subset(Pwy.todos, select = c("rater","Percentual"))
min(MetricaPwycTodos$Percentual)
max(MetricaPwycTodos$Percentual)
MetricaPwycTodosNormalizada <- Normaliza(MetricaPwycTodos)
min(MetricaPwycTodosNormalizada$Percentual)
max(MetricaPwycTodosNormalizada$Percentual)

# cria um dataframe que reuni as colunas em uma so tabela de nome 'ColunaDeMétricas'.
ColunaDeMétricas <- join(MetricaPwycTodosNormalizada, MetricaPwyConclusivosNormalizada, by="rater", type="left")
ColunaDeMétricas <- join(ColunaDeMétricas, MetricaPwyInconclusivosNormalizada, by="rater", type="left")
nrow(ColunaDeMétricas)

#Atribui 0 as colunas que possui linha com valor 'NA'
ColunaDeMétricas[is.na(ColunaDeMétricas)] <- 0

#renomeia as colunas da tabela com os nomes das métricas
names(ColunaDeMétricas) = c("raters","Pwc", "Pwi", "Pw")

#leitura da quarta metrica
distancias <- as.data.frame(read.csv2("C:/temp/distanciasNormalizada.csv"))
distancias$X <- NULL

#Renomeia a coluna trabalhadores par raters
names(distancias) = c("raters", "distancia")
min(distancias$distancia)
max(distancias$distancia)
distanciasNormalizada <- Normaliza(distancias)
min(distanciasNormalizada$distancia)
max(distanciasNormalizada$distancia)

#Realiao o join da tabela de metrica com a nova metrica que foi derivada
ColunaDeMétricasCompleta <- join(ColunaDeMétricas,distanciasNormalizada, by="raters", type="left")

#renomeia as colunas da tabela com os nomes das métricas
names(ColunaDeMétricasCompleta) = c("raters","Pwc", "Pwi", "Pw", "DEw")



#Dataframe com puramente dados numericos sem a lista de raters
Metricas <- data.frame(ColunaDeMétricasCompleta[,2:5])


boxplot(Metricas, las=1)

set.seed(7)

#Verificando o número ideal de clusters dado os dados com o Método Elbow
wss <- (nrow(Metricas)-1)*sum(apply(Metricas,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(Metricas,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Numero de Clusters",
     ylab="Soma dos quadrados dentro dos grupos ",
     main="",
     pch=20, cex=2)

#Execute K-Means com o número ótimo de clusters identificados a partir do método Elbow

Resultado = kmeans(Metricas, 5, nstart=100)
Resultado$centers

#
plot(Metricas, col =(Resultado$cluster +1) , main="K-Means resultados com 5 
     clusters", pch=20, cex=2)

a <- seq(1:5)

ndf <- as.data.frame(Resultado$centers)

ndf$Centroids <- a

MetricasVertival <- as.data.frame(rbind(cbind("Centroid"=ndf$Centroids,"Valor"=ndf$Pwc,"Metrica"=rep("Pwc",5)),
                                         cbind("Centroid"=ndf$Centroids,"Valor"=ndf$Pwi,"Metrica"=rep("Pwi",5)),
                                         cbind("Centroid"=ndf$Centroids,"Valor"=ndf$Pw,"Metrica"=rep("Pw",5)),
                                         cbind("Centroid"=ndf$Centroids,"Valor"=ndf$DEw,"Metrica"=rep("DEw",5))))
