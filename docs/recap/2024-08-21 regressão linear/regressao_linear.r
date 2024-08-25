
# Pacotes -----------------------------------------------------------------
library(performance)

# Dados inventados --------------------------------------------------------
# Seed para gerar os mesmos números sempre
set.seed(123) 
# Fator de doses
doses <- c(0, 50, 100, 150, 200)
# Fator de cultivar
cultivares <- factor(c("Cultivar1", "Cultivar2", "Cultivar3"))
# Médias
media_produtividade <- matrix(c(50, 55, 60, 65, 70, 
                                45, 50, 55, 60, 65,
                                40, 45, 50, 55, 60),
                              nrow = length(cultivares), byrow = TRUE)
# Nùmero de repetições
n_reps <- 10
# Definindo um data.frame vazio para os dados
dados <- data.frame()
# Loop para fazer o banco de dados final
for (i in 1:length(cultivares)) {
  for (j in 1:length(doses)) {
    produtividade <- rnorm(n_reps, mean = media_produtividade[i, j], sd = 5)
    temp_df <- data.frame(
      cultivar = rep(cultivares[i], n_reps),
      dose = rep(doses[j], n_reps),
      produtividade = produtividade
    )
    dados <- rbind(dados, temp_df)
  }
}


# Com seus dados pode começar por aqui ---------------------------------

# Modelo de regressão -----------------------------------------------------
# A pergunta será: As variáveis de dose e cultivar são preditoras 
# da produtividade?

# Checando qual a nível de referência para cultivar
levels(dados$cultivar)

# Criando o modelo
mod <- lm(produtividade ~ dose + cultivar, data = dados)
# verificando os coeficientes do modelo
summary(mod)

# Qualidade do modelo -----------------------------------------------------
# Normalidade dos resíduos
# qqplot dos resíduos
plot(mod, which = 2)
# simetria dos valores 
plot(mod, which = 1)
# Distância de cook
plot(mod, which = 4)
# Multicolinearidade
check_collinearity(mod)
# Homocedasdicidade
check_heteroscedasticity(mod)

# Checando o modelo com uma função apenas ---------------------------------
check_model(mod)

# Para interpretação ------------------------------------------------------

# Para ter todas as comparações deve alterar a referência a cada vez
dados$cultivar <-  relevel(dados$cultivar, ref = "Cultivar1")
mod <- lm(produtividade ~ dose + cultivar, data = dados)
summary(mod)
# ref 2
dados$cultivar <-  relevel(dados$cultivar, ref = "Cultivar2")
mod <- lm(produtividade ~ dose + cultivar, data = dados)
summary(mod)
# ref 3
dados$cultivar <-  relevel(dados$cultivar, ref = "Cultivar3")
mod <- lm(produtividade ~ dose + cultivar, data = dados)
summary(mod)


