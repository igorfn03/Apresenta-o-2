# Carregando e instalando os pacotes necessários
require(openxlsx)            # Leitura de base de dados
install.packages("openxlsx")
require(dplyr)               # Manipulação de base de dados
require(gtsummary)  
install.packages("gtsummary")# Tabelas automáticas
require(gt)       ]
install.packages("gt")# Tabelas automáticas
require(rstatix)  
install.packages("rstatix")# Coeficiente de Cramer
require(ggplot2)             # Gráficos
require(qqplotr)             # Gráficos qqplot
require(DescTools)           # Teste de Levene

# Lendo a base de dados
Dados = read.xlsx("DadosFatoresRisco.xlsx")

# Selecionando as variáveis
DadosQuali = Dados %>% select(Diabetico, IMC, Atividade_Fisica, Saude_Geral,
                              Faixa_Idade)

# Tabela resumo
tbl_summary(data = DadosQuali,
            by=Diabetico,
            percent = "row") |> 
  add_p()

# Investigando valores esperados
chisq.test(Dados$IMC,Dados$Diabetico)$expected
chisq.test(Dados$Atividade_Fisica,Dados$Diabetico)$expected
chisq.test(Dados$Saude_Geral,Dados$Diabetico)$expected
chisq.test(Dados$Faixa_Idade,Dados$Diabetico)$expected

# Investigando os resíduos padronizados
chisq.test(Dados$Atividade_Fisica,Dados$Diabetico)$stdres
chisq.test(Dados$Saude_Geral,Dados$Diabetico)$stdres
chisq.test(Dados$Faixa_Idade,Dados$Diabetico)$stdres

# Calculando o coeficiente de crâmer
A=table(Dados$IMC,Dados$Diabetico)
cramer_v(A)
B=table(Dados$Atividade_Fisica,Dados$Diabetico)
cramer_v(B)
C=table(Dados$Faixa_Idade,Dados$Diabetico)
cramer_v(C)

# Fazendo a tabela final
