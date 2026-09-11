if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyr)) install.packages("tidyr")
library(ggplot2)
setwd("C:/Users/Windows/Desktop/GitHub/Saude suplementar")
dados <- data.frame(
  ano = c(2020, 2021, 2022, 2023, 2024),
  Qnt_antendimentos = cfaturamento_novo <- c(35015245, 41735804, 47217912, 49207136, 56078157))

dados$tempo <- dados$ano - min(dados$ano) + 1
options(scipen = 999)

modelo_lin <- lm(Qnt_antendimentos ~ tempo, data = dados)
modelo_log <- lm(Qnt_antendimentos ~ log(tempo), data = dados)
modelo_exp <- lm(log(Qnt_antendimentos) ~ tempo, data = dados)


futuro <- data.frame(ano = c(2025, 2026))
futuro$tempo <- futuro$ano - min(dados$ano) + 1

futuro$Linear <- predict(modelo_lin, futuro)
futuro$Logaritmica <- predict(modelo_log, futuro)
futuro$Exponencial <- exp(predict(modelo_exp, futuro)) # exp() para reverter o log

summary(modelo_lin)
summary(modelo_log)
summary(modelo_exp)

r2_lin <- summary(modelo_lin)$r.squared
r2_log <- summary(modelo_log)$r.squared
r2_exp <- summary(modelo_exp)$r.squared


plot_lin <-ggplot(dados, aes(x = ano, y = Qnt_antendimentos)) +
  geom_point(size = 3) +
  geom_line(aes(y = predict(modelo_lin, dados)), color = "darkred", size = 1) +
  geom_point(data = futuro, aes(x = ano, y = Linear), color = "red", shape = 18, size = 4) +
  labs(title = "Quantidade de sessões de fisioterapia", subtitle = paste("Fonte: Agência Nacional de Saúde Suplementar"))+labs(x = "Ano", y = "Qnt. sessões") +
  theme_minimal() +
  theme(text = element_text(color = "darkred"), axis.text = element_text(color = "black"))

plot_log <-ggplot(dados, aes(x = ano, y = Qnt_antendimentos)) +
  geom_point(size = 3) +
  geom_line(aes(y = predict(modelo_log, dados)), color = "darkred", size = 1) +
  geom_point(data = futuro, aes(x = ano, y = Logaritmica), color = "red", shape = 18, size = 4) +
  labs(title = "Quantidade de sessões de fisioterapia", subtitle = paste("Fonte: Agência Nacional de Saúde Suplementar"))+labs(x = "Ano", y = "Qnt. sessões") +
  theme_minimal() +
  theme(text = element_text(color = "darkred"), axis.text = element_text(color = "black"))


plot_exp <- ggplot(dados, aes(x = ano, y = Qnt_antendimentos)) +
  geom_point(size = 3) +
  geom_line(aes(y = exp(predict(modelo_exp, dados))), color = "darkred", size = 1) +
  geom_point(data = futuro, aes(x = ano, y = Exponencial), color = "red", shape = 18, size = 4) +
  labs(title = "Quantidade de sessões de fisioterapia", subtitle = paste("Fonte: Agência Nacional de Saúde Suplementar"))+labs(x = "Ano", y = "Qnt. sessões") +
  theme_minimal() +
  theme(text = element_text(color = "darkred"), axis.text = element_text(color = "black"))

print(plot_lin)
print(plot_log)
print(plot_exp)
print(futuro)
print(R2)
R2 <- data.frame(linear = r2_lin,
                 log = r2_log,
                 exp = r2_exp)

