library(readxl)
library(data.table)
library(knitr)
library(ggplot2)

#df <- read.csv("cnc.csv")
#df <- as.data.table(df)
#df[,data_autuacao := as.Date(data_autuacao, "%d/%m/%y")]
#df[,data_nascimento := as.Date(data_nascimento, "%d/%m/%y")]
#df[,data_concilacao := as.Date(data_concilacao, "%d/%m/%y")]
#save(df,file = "cnc.RData")
load("cnc.RData")
cnc <- as.data.table(cnc)
str(cnc)

# Converte para tipo Date
cnc[,data_concilacao := substr(data_concilacao,1, 10)]
cnc[,data_concilacao := as.Date(data_concilacao, "%Y-%m-%d")]
cnc[,data_autuacao := as.Date(data_autuacao, "%d/%m/%y")]
cnc[,data_nascimento := as.Date(data_nascimento, "%d/%m/%y")]

# Corrige nome da coluna
setnames(cnc, "replace", "vara")
names(cnc)
nrow(cnc) # 1.534.078 assuntos
ncol(cnc)

# Nº Processo
length(unique(cnc$processo))
# 98.056 Processos

#Agrupa por Processo
processos <- cnc[, 
                 .(data_autuacao      = max(data_autuacao),
                   valor_causa        = max(valor_causa),
                   vara               = first(vara),
                   id_usuario_magistrado = first(id_usuario_magistrado),
                   assunto            = list(assunto),
                   assunto_principal  = first(assunto_principal),
                   id_advogado        = first(id_advogado),
                   id_parte           = first(id_parte),
                   tipo_pessoa        = first(tipo_pessoa),
                   polo               = first(polo),
                   data_nascimento    = first(data_nascimento),
                   sexo               = first(sexo),
                   id_escolaridade    = first(id_escolaridade),
                   cidade             = first(cidade),
                   uf                 = first(uf),
                   cep                = first(cep),
                   cod_municipio_ibge = first(cod_municipio_ibge),
                   data_concilacao    = first(data_concilacao)), 
                 by = processo]

freq = table(cnc$assunto)
prop = round(prop.table(freq)*100,2) # Construindo as proporções
kable(cbind(freq,prop),col.names = c("Freq","Prop")) #Criando tabela de frequências e proporções


# Análise Vara
sum(is.na(cnc$vara))

freq = table(cnc$vara) # Construindo a tabela de frequencias 
prop = round(prop.table(freq)*100,2) # Construindo as proporções
kable(cbind(freq,prop),col.names = c("Freq","Prop")) #Criando tabela de frequências e proporções

xtab <- xtabs(~cnc$vara)
varas <- names(head(xtab[order(xtab, decreasing = TRUE)], 10)) # Selecionando as varas com maior número
cnc[, vara_analise := vara] # Crianda outro campo
cnc[!vara_analise %in% varas, vara_analise := "Outros"] # Renomeando para "Outros" 
cnc$vara_analise <- factor(cnc$vara_analise, levels = c(varas, "Outros"))  # Definindo a ordem dos fatores do mais frequente para o menos frequente

#rm(xtab, municipios) # Removendo variáveis que não serão mais utilizadas

ggplot(cnc, aes(x=vara_analise)) + 
  geom_bar() + 
  labs(x="Vara", y="Frequência", title="CNC") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust=1.0))


# Análise Data Autuação
sum(is.na(cnc$data_autuacao))
summary(cnc$data_autuacao)

sum(is.na(cnc$data_concilacao))
sum(!is.na(cnc$data_concilacao))


