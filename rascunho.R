# Bibliotecas necessárias ------------------------------------------------------
# ------------------------------------------------------------------------------
install.packages('openxlsx')
install.packages('dplyr')
install.packages('gtsummary')
install.packages('gt')
install.packages('rstatix')
# ------------------------------------------------------------------------------

# Tratamento dos dados ---------------------------------------------------------
# ------------------------------------------------------------------------------
theme_gtsummary_language("pt", big.mark = ".", decimal.mark = ",")

Dados = read.xlsx("DadosAviao.xlsx")

DadosQuali = Dados %>% select(Checkin.service, Genero, Cliente, Idade,
                              Tipo_Viagem, Classe, Distancia,
                              Atraso_Partida, Atraso_Chegada)

DadosQuali = DadosQuali %>%
  mutate(Idade = recode(Idade,
                        "30–50 anos" = "Entre 30 e 50",
                        "<30 anos" = "Menor que 30 anos",
                        ">50 anos" = "Maior que 50 anos"))

DadosQuali = DadosQuali %>%
  mutate(Distancia = recode(Distancia,
                        " 500–1500 km" = "Entre 500 e 1500 Km",
                        " <500 km" = "Menor que 500 Km",
                        ">1500 km" = "Maior que 1500 Km"))
# ------------------------------------------------------------------------------

# Primeiras tabelas ------------------------------------------------------------
# ------------------------------------------------------------------------------
tbl_summary(data = DadosQuali)

tbl_summary(data = DadosQuali,
            by = Checkin.service)

tbl_summary(data = DadosQuali,
            by = Checkin.service,
            percent = "row")
# ------------------------------------------------------------------------------

# Verificação de matrizes de valores esperados ---------------------------------
# ------------------------------------------------------------------------------
chisq.test(Dados$Genero,Dados$Checkin.service)$expected
chisq.test(Dados$Cliente,Dados$Checkin.service)$expected
chisq.test(Dados$Idade,Dados$Checkin.service)$expected
chisq.test(Dados$Tipo_Viagem,Dados$Checkin.service)$expected
chisq.test(Dados$Classe,Dados$Checkin.service)$expected # 1
chisq.test(Dados$Distancia,Dados$Checkin.service)$expected
chisq.test(Dados$Atraso_Partida,Dados$Checkin.service)$expected
chisq.test(Dados$Atraso_Chegada,Dados$Checkin.service)$expected
# ------------------------------------------------------------------------------

# Tabela de associação por coluna contendo valor p dos testes ------------------
# ------------------------------------------------------------------------------
tbl_summary(data = DadosQuali,
            by = Checkin.service,
            percent = "row")%>%
  add_p()
# ------------------------------------------------------------------------------

# Fazendo análise de resíduos --------------------------------------------------
# ------------------------------------------------------------------------------
chisq.test(Dados$Classe,Dados$Checkin.service)$stdres # 2
chisq.test(Dados$Tipo_Viagem,Dados$Checkin.service)$stdres # 1
chisq.test(Dados$Atraso_Partida,Dados$Checkin.service)$stdres
# ------------------------------------------------------------------------------

# Função que calcula o coeficiente de Cramer -----------------------------------
# ------------------------------------------------------------------------------
cramer_fun <- function(data, variable, by, ...) {
  tab <- table(data[[variable]], data[[by]])
  v <- cramer_v(tab)
  tibble::tibble(`**Cramér**` = round(v, 3))
}
# ------------------------------------------------------------------------------

# Código da tabela final -------------------------------------------------------
# ------------------------------------------------------------------------------
tbl_summary(data = DadosQuali,
            by = Checkin.service,
            percent = "row",
            label = list(
              Genero ~ "Gênero<sup>Q</sup>",
              Cliente ~ "Tipo de Cliente<sup>Q</sup>",
              Idade ~ "Idade<sup>Q</sup>",
              Tipo_Viagem ~"Tipo de Viagem<sup>Q</sup>",
              Classe ~ "Classe<sup>F</sup>",
              Distancia ~ "Distância<sup>Q</sup>",
              Atraso_Partida ~"Atraso na Partida<sup>Q</sup>",
              Atraso_Chegada ~"Atraso na Chegada<sup>Q</sup>"
            )
)%>%
  add_p(pvalue_fun = label_style_pvalue(digits = 3)) %>%
  bold_p(t = 0.05) %>%
  add_stat(fns = everything() ~ cramer_fun)%>%
  modify_spanning_header(all_stat_cols() ~ "**Satisfação com o Serviço de Checkin**") %>%
  modify_header(label ~ "**Variáveis**") %>%
  bold_labels() %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>{n} ({style_percent(p)}%)")%>%
  modify_bold(
    columns = stat_1,  # primeira coluna
    rows = (variable == "Tipo_Viagem" & label == "Viagem a Negocios"| label=="Viagem Pessoal") | 
      (variable == "Classe" & label == "Economica Plus") | 
      (variable == "Atraso_Partida" & label == "Com atraso")
  )%>%
  modify_bold(
    columns = stat_1,
    rows = (variable == "Atraso_Partida" & label == "Sem atraso")
  ) %>%
  modify_bold(
    columns = stat_3,  # terceira coluna
    rows = (variable == "Tipo_Viagem" & label == "Viagem a Negocios"| label=="Viagem Pessoal") | 
      (variable == "Atraso_Partida" & label == "Com atraso")
    )%>%
  modify_bold(
    columns = stat_3,
    rows = (variable == "Atraso_Partida" & label == "Sem atraso")
  ) %>%
  modify_footnote(everything() ~ NA)%>%
  as_gt() %>%                   
  fmt_markdown(columns = label) %>%
  tab_options(
    table.font.size = "20px",    
    heading.title.font.size = "26px",
    column_labels.font.size = "22px"
  )
# ------------------------------------------------------------------------------