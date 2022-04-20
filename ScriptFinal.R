#####################################UNIVARIADA###############################################

MediaIdades<-mean(Grupo2$Idade,na.rm=TRUE)
MedianaIdades<-median(Grupo2$Idade,na.rm=TRUE)
Idade<-na.omit(Grupo2$Idade)
boxplot(Idade,na.rm=TRUE,
        main="Diagrama de extremos e quartis para a idade",
        cex.main=1,
        ylab="Diferença de idades",cex.lab=0.8,
        horizontal=TRUE)
max(Idade)
install.packages("ggplot2")
library(ggplot2)

table<-table(na.omit(Grupo2$Idade))
data<-as.data.frame(table)
colnames(data)<-c("Idade","Número_Elementos")

ggplot(data, aes(x =Idade, y =Número_Elementos, fill = Idade)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Número_Elementos), vjust = 0)


######################################SEXO######################################################
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

result <- getmode (Grupo2$Sexo)

absol<-table(Grupo2$Sexo)
relat<-round(prop.table(table(Grupo2$Sexo)),3)
TabSexo<-cbind(absol,relat*100)
colnames(TabSexo)<-c("Número de Elementos","Percentagem")
pie(relat, labels = paste0(relat*100, "%"),cex=1.2,
    main = "Gráfico circular para a variável 'Sexo'", cex.main=1,
    col = c("blue","deeppink"),)
legend("bottomright", c("Masculino","Feminino"),
       fill = c("blue","deeppink"),
       cex=0.8)

Sexo<-(na.omit(Grupo2$Sexo))
colnames(table)<-c("Masculino","Feminino")
table<-table(Sexo)

barplot(prop.table(table(Sexo))*100,
        main="Gráfico barras para a variável'Sexo'",
        cex.main=1,
        xlab="Sexo do aluno",
        ylab="Percentagem",
        ylim=c(0,60))

######################################CURSO######################################################
result <- getmode (Grupo2$Curso)

absol<-table(Grupo2$Curso)
relat<-round(prop.table(table(Grupo2$Curso)),3)
TabCurso<-cbind(absol,relat*100)
colnames(TabCurso)<-c("CTeSP TLQB","L. Bioinformática","L. Biotecnologia")
pie(relat, labels = paste0(relat*100, "%"),cex=1.2,
    main = "Gráfico circular para a variável 'Sexo'", cex.main=1,
    col = c("green","blue","yellow"),)
legend("bottomright", c("CTeSP TLQB","L. Bioinformática","L. Biotecnologia"),
       fill = c("green","blue","yellow"),
       cex=0.8)

Curso<-(na.omit(Grupo2$Curso))
table<-table(Curso)

barplot(prop.table(table(Curso))*100,
        main="Gráfico barras para a variável'Sexo'",
        cex.main=1,
        xlab="Sexo do aluno",
        ylab="Percentagem",
        ylim=c(0,80))
######################################AnoCurricular######################################################
result <- getmode (Grupo2$AnoCurricular)
median(na.omit(Grupo2$AnoCurricular))

ano<-na.omit(Grupo2$AnoCurricular)

boxplot(Grupo2$AnoCurricular,na.rm=TRUE,
        main="Diagrama de extremos e quartis para o AnoCurricular",
        cex.main=1,
        ylab="AnoCurricular",cex.lab=0.8)

barplot(prop.table(table(Grupo2$AnoCurricular))*100,
        main="Gráfico barras para a variável 'AnoCurricular'",
        cex.main=1,
        xlab="Ano Curricular",
        ylab="Percentagem",
        ylim=c(0,60))
######################################OPÇÃO######################################################
result <- getmode (Grupo2$Curso)

absol<-table(Grupo2$opcao1)
relat<-round(prop.table(table(Grupo2$opcao1)),3)
TabCurso<-cbind(absol,relat*100)
colnames(TabCurso)<-c("Sim","Não")
pie(relat, labels = paste0(relat*100, "%"),cex=1.2,
    main = "Gráfico circular para a variável 'Opção'", cex.main=1,
    col = c("green","blue"),)
legend("bottomright", c("Sim","Não"),
       fill = c("green","blue"),
       cex=0.8)

Curso<-(na.omit(Grupo2$Curso))
table<-table(Curso)

barplot(prop.table(table(Curso))*100,
        main="Gráfico barras para a variável'Sexo'",
        cex.main=1,
        xlab="Sexo do aluno",
        ylab="Percentagem",
        ylim=c(0,80))
######################################ESCOLHA######################################################
result <- getmode (Grupo2$Escolhi)

absol<-table(Grupo2$Escolhi)
relat<-round(prop.table(table(Grupo2$Escolhi)),3)
TabEscolhi<-cbind(absol,relat*100)
colnames(TabEscolhi)<-c("Sim","Não")
pie(relat, labels = paste0(relat*100, "%"),cex=1.2,
    main = "Gráfico circular para a variável 'Sexo'", cex.main=1,
    col = c("green","red"),)
legend("bottomright", c("Sim","Não"),
       fill = c("green","red"),
       cex=0.8)

Grupo2$Escolhi[Grupo2$Escolhi == "2"] <- "Não"

Escolhi<-(na.omit(Grupo2$Escolhi))
table<-table(Escolhi)

barplot(prop.table(table(Escolhi  ))*100,
        main="Gráfico barras para a variável'Sexo'",
        cex.main=1,
        xlab="Respostas",
        ylab="Percentagem",
        ylim=c(0,100))
######################################TEMPODESLOC######################################################
MediaTempDesl<-mean(Grupo2$TempoDesloca,na.rm=TRUE)
MedianaTempDesl<-median(Grupo2$TempoDesloca,na.rm=TRUE)

max(Grupo2$TempoDesloca)

boxplot(Grupo2$TempoDesloca,na.rm=TRUE,
        main="Diagrama de extremos e quartis para a Variavel 'Tempo de Deslocação'",
        cex.main=1,
        ylab="Número de Horas",cex.lab=1.2,
        horizontal = TRUE)
n<-length(na.omit(Grupo2$TempoDesloca))
Cn<-trunc(1+1.44*log(n))+1
At<-max(na.omit(Grupo2$TempoDesloca))-min(na.omit(Grupo2$TempoDesloca))
AC<-round(At/Cn,2)
classes<-round(seq(min(na.omit(Grupo2$TempoDesloca)), min(na.omit(Grupo2$TempoDesloca))+Cn*AC,AC))
HistHorasRedes<-hist(Grupo2$TempoDesloca, breaks = classes,
                     main="Idades dos alunos que responderam ao questionario",
                     xlab="Número de tempo que os alunos demoram a ir e voltar em minutos",
                     ylab = "Número de Elementos",
                     freq=TRUE,
                     col = "blue",
                     labels = TRUE,
                     ylim = c(0,50),
                     axes = FALSE
)
axis(1,classes)
axis(2,n=seq)
######################################HORASESTUDO######################################################
MediaHorasEstudo<-mean(Grupo2$HorasEstudo,na.rm=TRUE)
MedianaHorasEstudo<-median(Grupo2$HorasEstudo,na.rm=TRUE)
max(Grupo2$HorasEstudo,na.rm=TRUE)
min(Grupo2$HorasEstudo,na.rm=TRUE)

boxplot(Grupo2$HorasEstudo,na.rm=TRUE,
        main="Diagrama de extremos e quartis para a variavel 'Horas de Estudo'",
        cex.main=1,
        ylim = c(0,70),
        ylab="Número de Horas de Estudo",cex.lab=0.8,
        horizontal = TRUE)

n<-length(na.omit(Grupo2$HorasEstudo))
Cn<-trunc(1+1.44*log(n))+1
At<-max(na.omit(Grupo2$HorasEstudo))-min(na.omit(Grupo2$HorasEstudo))
AC<-round(At/Cn,2)
classes<-round(seq(min(na.omit(Grupo2$HorasEstudo)), min(na.omit(Grupo2$HorasEstudo))+Cn*AC,AC))

HistHorasEstudo<-hist(Grupo2$HorasEstudo, breaks = classes,
                      main="Número de horas de estudo",
                      xlab="Horas de estudo",
                      col="blue",
                      ylab = "Percentagem por Intervalo",
                      ylim = c(0,100),
                      freq=TRUE,
                      axes =FALSE,
                      labels=TRUE
)
axis(1,classes)
axis(2,n=seq)
######################################HORASREDES######################################################
MediaHorasEstudo<-mean(Grupo2$HorasRedes,na.rm=TRUE)
MedianaHorasEstudo<-median(Grupo2$HorasRedes,na.rm=TRUE)
max(Grupo2$HorasRedes,na.rm=TRUE)
min(Grupo2$HorasRedes,na.rm=TRUE)

boxplot(Grupo2$HorasRedes,na.rm=TRUE,
        main="Diagrama de extremos e quartis para a variavel 'Horas de Redes'",
        cex.main=1,
        ylab="Número de Horas de Redes",cex.lab=0.8,
        horizontal = TRUE)

n<-length(na.omit(Grupo2$HorasRedes))
Cn<-trunc(1+1.44*log(n))+1
At<-max(na.omit(Grupo2$HorasRedes))-min(na.omit(Grupo2$HorasRedes))
AC<-round(At/Cn,2)
classes<-round(seq(min(na.omit(Grupo2$HorasRedes)), min(na.omit(Grupo2$HorasRedes))+Cn*AC,AC))

HistHorasEstudo<-hist(Grupo2$HorasRedes, breaks = classes,
                      main="Número de horas em redes",
                      xlab="Horas em redes (horas)",
                      col="blue",
                      ylab = "Percentagem por Intervalo",
                      ylim = c(0,125),
                      freq=TRUE,
                      axes =FALSE,
                      labels=TRUE
)
axis(1,classes)
axis(2,n=seq)
######################################HORASTV######################################################
MediaHorasEstudo<-mean(Grupo2$HorasTV,na.rm=TRUE)
MedianaHorasEstudo<-median(Grupo2$HorasTV,na.rm=TRUE)
max(Grupo2$HorasTV,na.rm=TRUE)
min(Grupo2$HorasTV,na.rm=TRUE)

boxplot(Grupo2$HorasTV,na.rm=TRUE,
        main="Diagrama de extremos e quartis para a variavel 'Horas TV'",
        cex.main=1,
        ylab="Número de Horas de Estudo",cex.lab=0.8,
        horizontal = TRUE)

n<-length(na.omit(Grupo2$HorasTV))
Cn<-trunc(1+1.44*log(n))+1
At<-max(na.omit(Grupo2$HorasTV))-min(na.omit(Grupo2$HorasTV))
AC<-round(At/Cn,2)
classes<-round(seq(min(na.omit(Grupo2$HorasTV)), min(na.omit(Grupo2$HorasTV))+Cn*AC,AC))

HistHorasTV<-hist(na.omit(Grupo2$HorasTV), breaks = classes,
                  main="Número de horas em redes",
                  xlab="Horas em redes (horas)",
                  col="blue",
                  ylab = "Percentagem por Intervalo",
                  ylim = c(0,80),
                  freq=TRUE,
                  axes =FALSE,
                  labels = TRUE,
)
axis(1,classes)
axis(2,n=seq)
######################################HORASSONO######################################################
MediaHorasSono<-mean(Grupo2$HorasSono,na.rm=TRUE)
MedianaHorasSono<-median(Grupo2$HorasTV,na.rm=TRUE)
max(Grupo2$HorasSono,na.rm=TRUE)
min(Grupo2$HorasSono,na.rm=TRUE)

boxplot(Grupo2$HorasSono,na.rm=TRUE,
        main="Diagrama de extremos e quartis para a variavel 'Horas TV'",
        cex.main=1,
        ylab="Número de Horas de Estudo",cex.lab=0.8,
        horizontal = TRUE)

n<-length(na.omit(Grupo2$HorasSono))
Cn<-trunc(1+1.44*log(n))+1
At<-max(na.omit(Grupo2$HorasSono))-min(na.omit(Grupo2$HorasSono))
AC<-round(At/Cn,2)
classes<-round(seq(min(na.omit(Grupo2$HorasSono)), min(na.omit(Grupo2$HorasSono))+Cn*AC,AC))

HistHorasSono<-hist(na.omit(Grupo2$HorasSono), breaks = classes,
                    main="Número de horas de Sono",
                    xlab="Horas de Sono(horas)",
                    col="blue",
                    ylab = "Percentagem por Intervalo",
                    ylim = c(0,80),
                    freq=TRUE,
                    axes =FALSE,
                    labels = TRUE,
)
axis(1,classes)
axis(2,n=seq)
######################################PMENTOR######################################################
result <- getmode (Grupo2$PMentoria)

absol<-table(Grupo2$PMentoria)
relat<-round(prop.table(table(Grupo2$PMentoria)),3)
TabPMentoria<-cbind(absol,relat*100)
pie(relat, labels = paste0(relat*100, "%"),cex=1.2,
    main = "Gráfico circular para a variável 'Mentoria'", cex.main=1,
    col = c("green","red"),)
legend("bottomright", c("Sim","Não"),
       fill = c("green","red"),
       cex=0.8)

barplot(prop.table(table(Grupo2$PMentoria))*100,
        main="Gráfico barras para a variável 'Mentoriar'",
        cex.main=1,
        xlab="Respostas",
        ylab="Percentagem",
        ylim=c(0,60))
##########################################BIVARIADA###################################################
######################################################################################################
#######################################TABELA DE CONTINGENCIA######################################
P6<-(Grupo2$Burnout_P6)
Sexo
P6<-factor(P6,label=c("Nunca","Quase nunca", "Algumas vezes","Regularmente","Muitas vezes","Quase sempre","Sempre"),levels=c(0,1,2,3,4,5,6))
table<-table(Grupo2$Sexo,P6)
install.packages("descr")
library(descr)
CrossTable(Grupo2$Sexo,Grupo2$Burnout_P6,prop.c = FALSE,prop.chisq = FALSE,prop.t = FALSE)

#####################################TABELA COM FREQUENCIAS ESPERADAS#############################
Grupo2$Sexo<-factor(Grupo2$Sexo, labels = c("Feminino", "Masculino"), levels = c(1, 2))
#####################################TABELA COM FREQUENCIAS ESPERADAS#############################
Grupo2$Escolhi<-factor(Grupo2$Escolhi, labels = c("Sim","Não"), levels = c(1,2))

Grupo2$opcao1<-factor(Grupo2$opcao1, labels = c("Sim","Não"), levels = c(1,2))
Grupo2$Burnout_P6<-factor(Grupo2$Burnout_P6,label=c("Nunca","Quase nunca", "Algumas vezes","Regularmente","Muitas vezes","Quase sempre","Sempre"),levels=c(0,1,2,3,4,5,6))

tabela<-table(Grupo2$Burnout_P6,Grupo2$Sexo)

Pearson<-cor.test(Grupo2$HorasTV,Grupo2$HorasSono, method = "pearson")

Spearman<-cor.test(Grupo2$HorasSono,Grupo2$Burnout_P3, method = "spearman")


tabela2<-table(Grupo2$opcao1,Grupo2$Sexo)

install.packages("assocstats")
library(assocstats)
cramerV(tabela2)

##########################

chisq.test(tabela2) #teste do qui-quadrado

tab<-table(Grupo2$Escolhi,Grupo2$Sexo)
tab2<-table(Grupo2$Escolhi,Grupo2$Curso)
binom.test(52,96,0.5) #teste binomial

fisher.test(tab,alternative = "two.sided", conf.int = TRUE, conf.level = 0.95) #teste de fisher com 5% de significancia

mean(na.omit(Grupo2$HorasRedes))
library(nortest)
lillie.test(Grupo2$HorasRedes) #teste de lillie
wilcox.test(Grupo2$HorasRedes, correct = FALSE, mu=10) #teste de wilcox


HE<-na.omit(Grupo2$HorasEstudo)
HR<-na.omit(Grupo2$HorasRedes)
HT<-na.omit(Grupo2$HorasTV)
TD<-na.omit(Grupo2$TempoDesloca)

mod<-lm(Grupo2$HorasEstudo~Grupo2$HorasRedes+Grupo2$, Grupo2) #regressão linear
summary(rstandard(mod)) #resumo dos residuos
shapiro.test(mod$residuals) #teste de shapiro(normalidade)
par(mfrow=c(2,2))
plot(mod) #grafico
library(car)
library(lmtest)
library(psych)
durbinWatsonTest(mod) #teste de durbinwatson
bptest(mod) #teste de Bartlett
vif(mod) #verificação de multicolinariedade
pairs.panels(mod)
summary(mod) #resumo da variavel mod
summary(rstandard(mod))
head(mod)
pairs(df5[,1:3], pch = 19)
head(df5)
df5<-data.frame(NHE,NHR,NTD)
NHS<-Grupo2$HorasSono
NHE<-Grupo2$HorasEstudo
NHR<-Grupo2$HorasRedes
NHTV<-Grupo2$HorasTV
NTD<-Grupo2$TempoDesloca
PB3<-Grupo2$Burnout_P3

durbinWatsonTest(df5)
bptest(df5)
vif(df5)
pairs.panels(df5)
summary(df5)
summary(rstandard(df5))
head(df5)


z<-plot(NHE~NHR+NTD)

mod<-lm(NHE~NHR+NTD, Grupo2)
summary(mod)
plot(mod)

install.packages("scatterplot3d")
library(scatterplot3d)

shapiro.test(mod$residuals) #Normalidade ao residuos
summary(rstandard(mod)) #Verificação de outliers
bptest(mod) #homogeneidade
durbinWatsonTest(mod) #independencia entre variaveis
vif(mod) #multicolinearlidade (como é inferior a 10 não existe)

graph<-scatterplot3d(Grupo2$HorasEstudo~Grupo2$HorasRedes+Grupo2$TempoDesloca,
                     pch=16, angle=50, color = "red", box = FALSE,
                     xlab="Horas de Redes", ylab="Tempo de Deslocamento", zlab="Horas de Estudo")
graph$plane3d(mod, col="black", draw_polygon = TRUE)

s3d <- scatterplot3d(mod, type = "h", color = "blue",
                     angle=55, pch = 16)

library(scatterplot3d)

iris<-data.frame(Grupo2$HorasEstudo,Grupo2$HorasRedes,Grupo2$TempoDesloca)
head(iris)
scatterplot3d(iris[,1:3])


######################
Burnout<-na.omit(Grupo2[,14:28])
dfb<-data.frame(Grupo2[,14:28])
st_dados<-scale(Burnout)
attach(Burnout)
matcor<-cor(Burnout,use = "pairwise", method = "spearman")
print(matcor, digits = 2)
library(psych)
lowerCor(Burnout)
library(corrplot)
corrplot(matcor, method = "circle")
KMO(Burnout)
cortest.bartlett(matcor, n=nrow(Burnout))
fit<-princomp(st_dados, cor = TRUE)
fit
summary(fit)

fit1<-principal(st_dados, nfactors = 5, n.obs = nrow(Burnout), rotate = "none", scores = TRUE)
fit1

fit1$loadings

fit1_varimax<-principal(st_dados, nfactors = 5, n.obs = nrow(Burnout),rotate = "varimax",scores = TRUE)
fit1_varimax

install.packages("umx")
library(umx)

f1<-data.matrix(Burnout)
head(f1)
reliability(cov(f1))

fa.diagram(fit1_varimax)
