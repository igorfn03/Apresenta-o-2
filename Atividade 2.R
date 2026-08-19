# Carregando e instalando os pacotes necessários
require(openxlsx)            # Leitura de base de dados
install.packages("openxlsx")
require(dplyr)               # Manipulação de base de dados
require(gtsummary)  
install.packages("gtsummary")# Tabelas automáticas
require(gt)
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
chisq.test(Dados$IMC,Dados$Diabetico)$stdres
chisq.test(Dados$Atividade_Fisica,Dados$Diabetico)$stdres
chisq.test(Dados$Saude_Geral,Dados$Diabetico)$stdres
chisq.test(Dados$Faixa_Idade,Dados$Diabetico)$stdres

# Calculando o coeficiente de crâmer
A=table(Dados$IMC,Dados$Diabetico)
cramer_v(A)
B=table(Dados$Atividade_Fisica,Dados$Diabetico)
cramer_v(B)
C=table(Dados$Saude_Geral,Dados$Diabetico)
cramer_v(C)
D=table(Dados$Faixa_Idade,Dados$Diabetico)
cramer_v(D)

# Fazendo a tabela final
# Função que calcula o coeficiente de Cramer
cramer_fun <- function(data, variable, by, ...) {
  tab <- table(data[[variable]], data[[by]])
  v <- cramer_v(tab)
  tibble::tibble(`**Cramér**` = round(v, 3))
}

# Código da tabela
tbl_summary(data = DadosQuali,
            by = Diabetico,
            percent = "row",
            label = list(
              IMC ~ "IMC<sup>",
              Atividade_Fisica ~ "Atividade Física<sup>",
              Saude_Geral ~ "Saúde Geral<sup>",
              Faixa_Idade ~"Faixa de Idade<sup>"
            )
)%>%
  add_p(pvalue_fun = label_style_pvalue(digits = 3)) %>%
  bold_p(t = 0.05) %>%
  add_stat(fns = everything() ~ cramer_fun)%>%
  modify_spanning_header(all_stat_cols() ~ "**Diabético**") %>%
  modify_header(label ~ "**Variáveis**") %>%
  bold_labels() %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>{n} ({style_percent(p)}%)")%>%
  modify_bold(columns = stat_1,rows = (variable == "Atividade_Fisica" &
      label %in% c("Não", "Sim"))) %>%
  modify_bold(columns = stat_2,rows = (variable == "Atividade_Fisica" &
        label %in% c("Não", "Sim"))) %>%
  modify_bold(columns = stat_1,rows = (variable == "Saude_Geral" &
        label %in% c(
          "Boa",
          "Excelente/Muito boa",
          "Razoável/Ruim"))) %>%
  modify_bold(columns = stat_2,rows = (variable == "Saude_Geral" &
        label %in% c(
          "Boa",
          "Excelente/Muito boa",
          "Razoável/Ruim"))) %>%
  modify_bold(columns = stat_1,rows = (variable == "Faixa_Idade" &label == "18 a 44 anos")) %>%
  modify_bold(columns = stat_2,rows = (variable == "Faixa_Idade" &
      label == "18 a 44 anos")) %>%
  modify_footnote(everything() ~ NA)%>%
  as_gt() %>%                   
  fmt_markdown(columns = label) %>%
  tab_options(
    table.font.size = "20px",    
    heading.title.font.size = "26px",
    column_labels.font.size = "22px"
  )
