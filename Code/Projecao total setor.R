vetor1 <- c(35015245, 41735804, 47217912, 49207136, 56078157)
vetor2 <- c(5794788, 8084627, 10195497, 12147481, 17124292)
vetor3 <- c( 5794788, 8084627, 10195497, 12147481, 17124292)
vetor4 <- c( 2003670, 2765553, 2987683, 3237045, 3823603)
vetor5 <- c( 2063222, 3289449, 4687771, 6274675, 9292843)

library(ggplot2)

dados <- data.frame(
  ano = c(2020, 2021, 2022, 2023, 2024),
  Qnt_antendimentos = vetor1+vetor2+vetor3+vetor4+vetor5)#altera a serie temporal aqui

dados$tempo <- dados$ano - min(dados$ano) + 1
options(scipen = 999)

modelo_lin <- lm(Qnt_antendimentos ~ tempo, data = dados)
modelo_log <- lm(Qnt_antendimentos ~ log(tempo), data = dados)
modelo_exp <- lm(log(Qnt_antendimentos) ~ tempo, data = dados)

#Projeção (2025 e 2026)
futuro <- data.frame(ano = c(2025, 2026))#altera os anos para projeção aqui
futuro$tempo <- futuro$ano - min(dados$ano) + 1

futuro$Linear <- predict(modelo_lin, futuro)
futuro$Logaritmica <- predict(modelo_log, futuro)
futuro$Exponencial <- exp(predict(modelo_exp, futuro))

r2_lin <- summary(modelo_lin)$r.squared
r2_log <- summary(modelo_log)$r.squared
r2_exp <- summary(modelo_exp)$r.squared

plot_lin <-ggplot(dados, aes(x = ano, y = Qnt_antendimentos)) +
  geom_point(size = 3) +
  geom_line(aes(y = predict(modelo_lin, dados)), color = "darkred", size = 1) +
  geom_point(data = futuro, aes(x = ano, y = Linear), color = "red", shape = 18, size = 4) +
  labs(title = "Quantidade de sessões do setor", subtitle = paste("Fonte: Agência Nacional de Saúde Suplementar"))+labs(x = "Ano", y = "Qnt. sessões") +
  theme_minimal() +
  theme(text = element_text(color = "darkred"), axis.text = element_text(color = "black"))

plot_log <-ggplot(dados, aes(x = ano, y = Qnt_antendimentos)) +
  geom_point(size = 3) +
  geom_line(aes(y = predict(modelo_log, dados)), color = "darkred", size = 1) +
  geom_point(data = futuro, aes(x = ano, y = Logaritmica), color = "red", shape = 18, size = 4) +
  labs(title = "Quantidade de sessões do setor", subtitle = paste("Fonte: Agência Nacional de Saúde Suplementar"))+labs(x = "Ano", y = "Qnt. sessões") +
  theme_minimal() +
  theme(text = element_text(color = "darkred"), axis.text = element_text(color = "black"))

plot_exp <- ggplot(dados, aes(x = ano, y = Qnt_antendimentos)) +
  geom_point(size = 3) +
  geom_line(aes(y = exp(predict(modelo_exp, dados))), color = "darkred", size = 1) +
  geom_point(data = futuro, aes(x = ano, y = Exponencial), color = "red", shape = 18, size = 4) +
  labs(title = "Quantidade de sessões do setor", subtitle = paste("Fonte: Agência Nacional de Saúde Suplementar"))+labs(x = "Ano", y = "Qnt. sessões") +
  theme_minimal() +
  theme(text = element_text(color = "darkred"), axis.text = element_text(color = "black"))


print(plot_lin)
print(plot_log)
print(plot_exp)
