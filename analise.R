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
prop = round(prop.table(freq)*100,2) # Construindo as propor??es
kable(cbind(freq,prop),col.names = c("Freq","Prop")) #Criando tabela de frequ?ncias e propor??es


# An?lise Vara
sum(is.na(cnc$vara))

freq = table(cnc$vara) # Construindo a tabela de frequencias 
prop = round(prop.table(freq)*100,2) # Construindo as propor??es
kable(cbind(freq,prop),col.names = c("Freq","Prop")) #Criando tabela de frequ?ncias e propor??es

xtab <- xtabs(~cnc$vara)
varas <- names(head(xtab[order(xtab, decreasing = TRUE)], 10)) # Selecionando as varas com maior n?mero
cnc[, vara_analise := vara] # Crianda outro campo
cnc[!vara_analise %in% varas, vara_analise := "Outros"] # Renomeando para "Outros" 
cnc$vara_analise <- factor(cnc$vara_analise, levels = c(varas, "Outros"))  # Definindo a ordem dos fatores do mais frequente para o menos frequente

#rm(xtab, municipios) # Removendo vari?veis que n?o ser?o mais utilizadas

ggplot(cnc, aes(x=vara_analise)) + 
  geom_bar() + 
  labs(x="Vara", y="Frequ?ncia", title="CNC") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust=1.0))


# An?lise Data Autua??o
sum(is.na(cnc$data_autuacao))
summary(cnc$data_autuacao)

sum(is.na(cnc$data_concilacao))
sum(!is.na(cnc$data_concilacao))

cnc[!is.na(data_concilacao)]

# Leitura COVID
obitos <- read_excel('COVID-19_03-08-2020.xlsx', sheet =  '?bitos')
obitos <- as.data.table(obitos)
str(obitos)
names(obitos)

#Agrupando
df[, .N, by = "ds_assunto"]

head(df)
sum(!is.na(df$cep))

#Construindo tabela de frequ?ncia
freq = table(df$ds_assunto)

prop = round(prop.table(table(df$ds_assunto))*100,2)
kable(cbind(freq,prop),col.names = c("Freq","Prop"))

ggplot(df[!is.na(data_autuacao)], aes(x=data_autuacao)) + 
  geom_histogram(bins=40) + 
  labs(x="Data", y="Frequ?ncia", title="COVID-19")

# Diagrama de dispers?o
plot(df$data_concilacao)

freq_idade <- table(obitos$idade) 
moda = names(freq_idade)[freq_idade == max(freq_idade)]
moda

median(obitos$idade)
mean(obitos$idade)

# Boxplot
ggplot(df[!is.na(data_autuacao)], aes(y=data_autuacao)) + 
  geom_boxplot() + 
  labs(x="", y="Idade", title="COVID-19")

