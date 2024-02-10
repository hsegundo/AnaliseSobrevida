# Análise de sobrevida dos pacientes com LMA no HUWC (2020-2022)
library(survival)
library(ggfortify)

dados <- read.csv("/home/hsegundo/Documentos/dadoscompletos.csv")

# Análise de sobrevida global
km <- survfit(Surv(SOBREVIDA,OBITO) ~1, data=dados)
summary(km, times = c(60,365,730))  #SG em 60 dias, 1 ano e 2 anos
quantile(km, probs = 0.5, conf.int = TRUE)
autoplot(km)

# Análise por Risco
km_risco <- survfit(Surv(SOBREVIDA,OBITO) ~Estratificação.de.risco, data=dados)
summary(km_risco, times = c(60,365,730))
quantile(km_risco, probs = 0.5, conf.int = TRUE)
autoplot(km_risco)
survdiff(Surv(SOBREVIDA,OBITO)~Estratificação.de.risco, data = dados) #p da comparação entre as curvas

# Análise por Idade

