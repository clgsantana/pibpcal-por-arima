# Estimação do PIB Alagoano e comparação em nível regional e nacional utilizando
#o método ARIMA.
#
# 1. Importando os dados de PIB per capita, de 1989 a 2019, à preços de 2010, em 
# mil reais. Os primeiros dados a serem importados versam sobre Alagoas. Utili-
# zamos o pacote readxl para faciitar:
install.packages("readxl")
library(readxl)
pibpc <- read_excel("local_armazenado_no_computador/pibpc.xlsx", 
      col_types = c("skip", "numeric", "skip", 
                    "skip"))


# Após inserir os dados, determinaremos os mesmos como séries temporais a serem
# lidas como tal pelo programa através do comando ts, com início em 1989 e peri-
# odicicade anual (freq=1):
pibpc <- ts(pibpc,
            start=1989,
            freq=1)

# Teste Dickey-Fuller para determinar a estacionariedade da série temporal do
# estado de Alagoas:
install.packages("urca")
library(urca)
summary(ur.df(pibpc, lag = 1))

# Para trabalharmos os dados de maneira efetiva, transformaremos os dados em lo-
# garítmo, objetivando a estabilização das respectivas variâncias:
log.pibpc <- log(pibpc)
par(mfrow = c(1,1))
plot(pibpc,
     type = "l",
     col = "blue",
     main = "PIB per capita - Alagoas",
     xlab = "Ano",
     ylab = " ")
par(new = TRUE)
plot(log.pibpc,
     axes = F,
     ann = F,
     col = "red",
     lty = 2)
legend('topleft',
       c('PIB', 'LN(PIB - AL)'),
       col=c('blue', 'red'), lty=1:2,
       bty='n')

# Ou, para visualização lado a lado:
par(mfrow = c(1,2))
plot(pibpc,
     type = "o",
     col = "blue",
     main = "PIB per capita - Alagoas",
     xlab = "Ano",
     ylab = " ")
plot(log.pibpc,
     type = "o",
     col = "red",
     main = "LN(PIB per capita - Alagoas)",
     xlab = "Ano",
     ylab = " ")

# Faremos a primeira diferença da série, com o intuito de torná-la estacionária:
yal <- diff(log.pibpc, lag = 1)
par(mfrow = c(1,1))
plot(yal,
     type = "o",
     col = "blue",
     main = "LN(PIB - AL) em primeira diferença",
     xlab = "Ano",
     ylab = " ")

# Realizaremos a função de atocorrelação e a função de autocorrelação parcial 
# através do comando acf e pacf:
par(mfrow = c(1,2))
acf(yal, lag.max = 30, main = "Autocorrelação - AL")
pacf(yal, lag.max = 30, main = "Autocorrelação parcial - AL")

# Estimando o Arima com base na FAC e FACP com o comando arima, e pondo o teste 
# de Ljung-Box em prática para observar a dependência entre os dados através do
# comando Box.test:
install.packages("stats")
library(stats)
install.packages("forecast")
library(forecast)
install.packages("lmtest")
library(lmtest)
auto.arima(yal)
modalestimado <- arima(yal, order = c(0,0,1))
summary(modalestimado)
coeftest(arima(yal, order=c(0,0,1)))
Box.test(yal, type = "Ljung-Box", lag = 1)

# Faremos então a análise dos resíduos, iniciando pela FAC e FACP residual:
par(mfrow=c(1,2))
acf(resid(modalestimado), main="Autocorrelação residual - AL")
pacf(resid(modalestimado), main="Autocorrelação parcial residual - AL")

# Após, faremos a transformação destes para logarítmo para observação:
log.residal <- resid(modalestimado)
par(mfrow = c(1,1))
qqnorm(log.residal,
       ylab = "",
       xlab = "Resíduos",
       main = "Gráfico dos resíduos")
qqline(log.residal)

# Notando a presença de outliers, é necessário retirá-los para plotar o gráfico 
# da distribuição dos resíduos:
boxplot.stats (log.residal, coef = 1.5, do.conf = TRUE, do.out = TRUE)
log.residal <- log.residal[!log.residal %in% boxplot.stats(log.residal)$out]
par(mfrow = c(1,1))
qqnorm(log.residal,
       ylab = "",
       xlab = "Resíduos",
       main = "Gráfico dos resíduos sem outliers")
qqline(log.residal)

# Realizando o teste de normalidade dos resíduos de Anderson Darling com o auxí-
# lio do pacote nortest:
install.packages("nortest")
library(nortest)
testeresidal <- ad.test(log.residal)
testeresidal

# Finalmente, realizando a previsão do PIB per capita Alagoano para os próximos
# 31 anos em gráfico, e para os próximos 101 anos em tabela com o auxílio do pa-
# cote forecast:
install.packages("TSstudio")
library(TSstudio)
preval <- forecast(log.pibpc, 
                   h = 31)
plot_forecast(preval,
              title = "LN(PIB per capita previsto - AL)",
              Xtitle = "Ano",
              Ytitle = " ")
preval2 <- forecast(pibpc,
                    h = 101)
preval2

# Notando o erro cometido para plotar o gráfico da previsão, o comando correto
# seria o seguinte:
par(mfrow = c(1,1))
plot(forecast(modalestimado),
     main = "Previsão de ARIMA(0,0,1) - AL")
summary(forecast(modalestimado))

# 2. Agora, vamos trabalhar os dados de Pernambuco e São Paulo, seguindo os mes-
# mos procedimentos econométricos:
pibpcpe <- read_excel("local_armazenado_no_computador/pibpc.xlsx", 
                    col_types = c("skip", "skip", "numeric", 
                                  "skip"))
pibpcsp <- read_excel("local_armazenado_no_computador/pibpc.xlsx", 
                      col_types = c("skip", "skip", "skip", 
                                    "numeric"))

# Determinaremos os dados como séries temporais:
pibpcpe <- ts(pibpcpe,
              start = 1989,
              freq = 1)

pibpcsp <- ts(pibpcsp,
              start = 1989,
              freq = 1)

# Teste Dickey-Fuller
summary(ur.df(pibpcpe))
summary(ur.df(pibpcsp))

# Transformamos em logarítmo para estabilizar as variâncias:
log.pibpcpe <- log(pibpcpe)
par(mfrow = c(1,1))
plot(pibpc,
     type = "l",
     col = "blue",
     main = "PIB per capita - Pernambuco",
     xlab = "Ano",
     ylab = " ")
par(new = TRUE)
plot(log.pibpc,
     axes = F,
     ann = F,
     col = "red",
     lty = 2)
legend('topleft',
       c('PIB', 'LN(PIB - PE)'),
       col=c('blue', 'red'), lty=1:2,
       bty='n')

log.pibpcsp <- log(pibpcsp)
par(mfrow = c(1,1))
plot(pibpc,
     type = "l",
     col = "blue",
     main = "PIB per capita - São Paulo",
     xlab = "Ano",
     ylab = " ")
par(new = TRUE)
plot(log.pibpc,
     axes = F,
     ann = F,
     col = "red",
     lty = 2)
legend('topleft',
       c('PIB', 'LN(PIB - SP)'),
       col=c('blue', 'red'), lty=1:2,
       bty='n')

# Ou lado a lado:
par(mfrow = c(1,2))
plot(pibpcpe,
     col="blue",
     main="PIB per capita - Pernambuco",
     xlab="Ano",
     ylab=" ")
plot(log.pibpcpe,
     col="red",
     main="LN(PIB per capita - Pernambuco)",
     xlab="Ano",
     ylab=" ")

log.pibpcsp <- log(pibpcsp)
par(mfrow = c(1,2))
plot(pibpcsp,
     col="blue",
     main="PIB per capita - São Paulo",
     xlab="Ano",
     ylab=" ")
plot(log.pibpcsp,
     col="red",
     main="LN(PIB per capita - São Paulo)",
     xlab="Ano",
     ylab=" ")

# Faremos a primeira diferença de ambos:
ype <- diff(log.pibpcpe, lag = 1)
par(mfrow = c(1,1))
plot(ype,
     type = "o",
     col = "blue",
     main = "LN(PIB - PE) em primeira diferença",
     xlab = "Ano",
     ylab = " ")

ysp <- diff(log.pibpcsp, lag = 1)
par(mfrow = c(1,1))
plot(ysp,
     type = "o",
     col = "blue",
     main = "LN(PIB - SP) em primeira diferença",
     xlab = "Ano",
     ylab = " ")

# Analisar os correlogramas de FAC e FACP:
par(mfrow = c(1,2))
acf(log.pibpcpe, lag.max = 30, main = "Autocorrelação - LN(PIB - PE)")
pacf(log.pibpcpe, lag.max = 30, main = "Autocorrelação parcial - LN(PIB - PE)")

par(mfrow = c(1,2))
acf(log.pibpcsp, lag.max = 30, main = "Autocorrelação - LN(PIB - SP)")
pacf(log.pibpcsp, lag.max = 30, main = "Autocorrelação parcial - LN(PIB - SP)")


# Estimando o Arima e  teste de Ljung-Box (usando o comando auto.arima para de-
# terminar os melhores parâmetros diante da problemática encontrada com os tes-
# tes de Dickey-Fuller):
auto.arima(ype)
modpeestimado <- arima(ype, order = c(1,0,0))
summary(modpeestimado)
coeftest(arima(ype, order = c(1,0,0)))
Box.test(ype, type = "Ljung-Box", lag = 1)

auto.arima(ysp)
modspestimado <- arima(ysp, order = c(0,0,0))
summary(modspestimado)
coeftest(arima(ysp, order = c(0,0,0)))
Box.test(ysp, type = "Ljung-Box", lag = 1)

# Análise dos resíduos:
par(mfrow=c(1,2))
acf(resid(modpeestimado), main="Autocorrelação residual - PE")
pacf(resid(modpeestimado), main="Autocorrelação parcial residual - PE")

acf(resid(modspestimado), main="Autocorrelação residual - SP")
pacf(resid(modspestimado), main="Autocorrelação parcial residual - SP")

# Após, faremos a transformação destes para logarítmo:
log.residpe <- resid(modpeestimado)
qqnorm(log.residpe,
       ylab = " ",
       xlab = "Resíduos - PE",
       main = "Gráfico dos resíduos - PE")
qqline(log.residpe)

log.residsp <- resid(modspestimado)
qqnorm(log.residsp,
       ylab = " ",
       xlab = "Resíduos - SP",
       main = "Gráfico dos resíduos - SP")
qqline(log.residsp)

# Retirando os outliers:
boxplot.stats (log.residpe, coef = 1.5, do.conf = TRUE, do.out = TRUE)
log.residpe <- log.residpe[!log.residpe %in% boxplot.stats(log.residpe)$out]
qqnorm(log.residpe,
       ylab = " ",
       xlab = "Resíduos - PE",
       main = "Gráfico dos resíduos sem outliers - PE")
qqline(log.residpe)

boxplot.stats (log.residsp, coef = 1.5, do.conf = TRUE, do.out = TRUE)
log.residsp <- log.residsp[!log.residsp %in% boxplot.stats(log.residsp)$out]
qqnorm(log.residsp,
       ylab = " ",
       xlab = "Resíduos - SP",
       main = "Gráfico dos resíduos sem outliers - SP")
qqline(log.residsp)

# Teste de normalidade dos resíduos:
testeresidpe <- ad.test(log.residpe)
testeresidpe

testeresidsp <- ad.test(log.residsp)
testeresidsp

# E então, realizando a previsão do PIB per capita de Pernambuco e São Paulo pa-
# ra os próximos 31 anos em gráfico, e para os próximos 101 anos em tabela:
prevpe <- forecast(log.pibpcpe,
                   h = 31)
plot_forecast(prevpe,
              title = "PIB per capita previsto - PE",
              Xtitle = "Ano",
              Ytitle = " ")
prevpe2 <- forecast(pibpcpe,
                    h = 101)
prevpe2

prevsp <- forecast(log.pibpcsp, 
                   h = 31)
plot_forecast(prevsp,
              title = "PIB per capita previsto - SP",
              Xtitle = "Ano",
              Ytitle = " ")
prevsp2 <- forecast(pibpcsp,
                    h = 101)
prevsp2

# Notando o erro cometido para plotar o gráfico da previsão, o comando correto
# seria o seguinte:
plot(forecast(modpeestimado),
     main = "Previsão de ARIMA(1,0,0) - PE")
summary(forecast(modpeestimado))

plot(forecast(modspestimado),
     main = "Previsão de ARIMA(0,0,0) - SP")
summary(forecast(modspestimado))
