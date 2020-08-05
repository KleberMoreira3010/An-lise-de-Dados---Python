#CO_CATEGAD	Código da categoria administrativa da IES	

#1 = Pública Federal
#2 = Pública Estadual
#3 = Pública Municipal
#4 = Privada com fins lucrativos
#5 = Privada sem fins lucrativos
#7 = Especial

#area_curso
#Código da área de enquadramento do curso no Enade
#&CO_GRUPO==1401&
#"21 = Arquitetura e Urbanismo
#72 = Tecnologia em Análiseise e Desenvolvimento de Sistemas
#76 = Tecnologia em Gestão da Produçãoo Industrial
#79 = Tecnologia em Redes de Computadores
#701 = Matemática (Bacharelado)
#702 = Matemática (Licenciatura)
#903 = Letras-Portuguesa (Bacharelado)
#904 = Letras-Portuguesa (Licenciatura)
#905 = Letras-Portuguesa e Inglês (Licenciatura)
#906 = Letras-Portuguesa e Espanhol (Licenciatura)
#1401 = Física (Bacharelado)
#1402 = Física (Licenciatura)
#1501 = Química (Bacharelado)
#1502 = Química (Licenciatura)
#1601 = Ciências Biológicas (Bacharelado)
#1602 = Ciências Biológicas (Licenciatura)
#2001 = Pedagogia (Licenciatura)
#2401 = História (Bacharelado)
#2402 = História (Licenciatura)
#2501 = Artes Visuais (Licenciatura)
#3001 = Geografia (Bacharelado)
#3002 = Geografia (Licenciatura)
#3201 = Filosofia (Bacharelado)
#3202 = Filosofia (Licenciatura)
#3502 = Educaçãoo Física (Licenciatura)


#CO_REGIAO_CURSO	Código da região de funcionamento do curso	
#1 = Norte
#2 = Nordeste
#3 = Sudeste
#4 = Sul
#5 = Centro-Oeste

#NU_IDADE	Idade do inscrito em 26/11/2017	min = 10  max = 95

#TP_SEXO	Tipo de sexo	M = Masculino #F = Feminino  


#CO_TURNO_GRADUACAO	Código do turno de graduação	
#1 = Matutino
#2 = Vespertino
#3 = Integral
#4 = Noturno

#NT_GER	Nota bruta da prova - Media ponderada da formação geral (25%) e componente específico (75%). 
#(valor de 0 a 100)	

#QE_I01	Qual o seu estado civil?	

#A = Solteiro(a).
#B = Casado(a).
#C = Separado(a) judicialmente/divorciado(a).
#D = Viúvo(a).
#E = Outro.

#QE_I02	Qual a sua cor ou raça?	

#A = Branca.
#B = Preta.
#C = Amarela.
#D = Parda.
#E = Indígena.
#F = Não quero declarar.

#QE_I08	
#Qual a renda total de sua família, incluindo seus rendimentos?	

#A = Até 1,5 salários mínimo (atÃ© R$ 1.405,50).
#B = De 1,5 a 3 salários mínimos (R$ 1.405,51 a R$ 2.811,00).
#C = De 3 a 4,5 salários mínimos (R$ 2.811,01 a R$ 4.216,50).
#D = De 4,5 a 6 salários mínimos (R$ 4.216,51 a R$ 5.622,00).
#E = De 6 a 10 salários mínimos (R$ 5. 622,01 a R$ 9.370,00).
#F = De 10 a 30 salários mínimos (R$ 9.370,01 a R$ 28.110,00).
#G = Acima de 30 salários mínimos (mais de R$ 28.110,00).

#QE_I21	Alguém em sua família concluiu um curso superior?

#A = Sim.
#B = Não.

#QE_I23 Quantas horas por semana, aproximadamente, você dedicou aos estudos, excetuando as horas de aula?

#A = Nenhuma, apenas assisto as aulas.
#B = De uma a três.
#C = De quatro a sete.
#D = De oito a doze.
#E = Mais de doze.

vetor_pacotes=c("readr","ggplot2","plotly","e1071",
                "dplyr","Hmisc","esquisse","gridExtra")
install.packages(vetor_pacotes)
library(readr)
library(ggplot2)
library(plotly)
library(e1071)
require(dplyr)
require(Hmisc)
require(esquisse)
#require(devtools)
require(gridExtra)

# BLOCO CARREGANDO O BANCO E FAZENDO SUBCONJUNTOS(SUBSETS)

#Direcionando a pasta no diretório, para o R
setwd("C:/Users/klb_m/OneDrive/Documentos/R/Meus scripts")
setwd("C:\\Users\\teste\\Desktop\\Videos Aulas de Estatística e R")

getwd()

#Carregando o banco de dados

#base do R (Mais flexível e menor performance de velocidade)
microdados_enade <- read.table("MICRODADOS_ENADE_2017.txt",
                               header = TRUE, 
                               sep=";", 
                               dec = ",", 
                               colClasses=c(NT_OBJ_FG="numeric"))

#library readr (Menos flexível e maior perfomance de velocidade)
enade2017 = read.csv2("MICRODADOS_ENADE_2017.txt") 

#Visualiando o banco de dados
View(microdados_enade)

#Verificando as dimensões
dim(enade2017)

#Verificando a quantidade de linhas
dim(enade2017)[1]

#Verificando a quantidade de colunas
dim(enade2017)[2]

dim(microdados_enade)

#Selecionando as variáveis desejadas
microdados_enade_filtrados= enade2017 %>% dplyr::select(CO_GRUPO,CO_REGIAO_CURSO,NU_IDADE,
                                                 TP_SEXO,CO_TURNO_GRADUACAO,NT_GER,QE_I01,QE_I02,
                                                 QE_I08,QE_I21,QE_I23,
                                                 NT_OBJ_FG, 
                                                 NT_OBJ_CE
  )      

#enade2017 %>% head() %>% dim() %>% funcao()

dim(head(enade2017)) 





#Vendo o nome das colunas do Data Frame
names(microdados_enade_filtrados)

#Verificando as dimensões
dim(microdados_enade_filtrados)

#dicionário:
#QE_I01	Qual o seu estado civil?	
#CO_GRUPO Curso(Matemática, estatística, psicologia..)
#CO_REGIAO_CURSO (Norte, nordeste, sul..)
#TP_SEXO (Masculino/Feminino)

 
#Filtrando os dados só para os profissionais de análise e desenvolvimento de sistemas (t.i)
microdados_ti= microdados_enade_filtrados %>% filter(CO_GRUPO==72) 

#Verificando as dimensões
dim(microdados_ti)

#Certificando que o filtro funcionou
table(microdados_ti$CO_GRUPO)

#BLOCO TRANSFORMANDO AS VARIÁVEIS NO BASE E NO DPLYR

#Criando categorias no dplyr para que facilite a nossa Análise Descritiva

#Ajuda a entender CASE_WHEN e IFELSE
professor=c("THIAGO MARQUES","ADRIANA SILVA","OUTRO","OUTRO2")
ifelse(professor=="THIAGO MARQUES" | professor=="ADRIANA SILVA" ,"AULA LEGAL","AULA CHATA")
case_when(professor=="THIAGO MARQUES" ~ "AULA LEGAL",
          professor=="ADRIANA SILVA" ~ "AULA LEGAL",
          TRUE ~"CHATA")

microdados_ti$estado_civil=ifelse(microdados_ti$QE_I01=="A","Solteiro(a)",
                                  ifelse(microdados_ti$QE_I01=="B","Casado(a)",
                                         ifelse(microdados_ti$QE_I01=="C","Separado(a)",
                                                ifelse(microdados_ti$QE_I01=="D","Viúvo(a)","Outro"
                                                ))))

microdados_ti = microdados_ti %>% mutate(estado_civil2 = case_when( QE_I01 == "A" ~ "Solteiro(a)",
                                                                    QE_I01 == "B" ~ "Casado(a)",
                                                                    QE_I01 == "C" ~ "Separado(a)",
                                                                    QE_I01 == "D" ~ "Viúvo(a)",
                                                                    QE_I01 == "E" ~ "Outro"
                                                                    )) 

microdados_ti = microdados_ti %>% mutate(regiao = case_when( CO_REGIAO_CURSO == 1 ~ "Norte",
                                                             CO_REGIAO_CURSO == 2 ~ "Nordeste",
                                                             CO_REGIAO_CURSO == 3 ~ "Sudeste",
                                                             CO_REGIAO_CURSO == 4 ~ "Sul",
                                                             CO_REGIAO_CURSO == 5 ~ "Centro-Oeste"
                                                               )) 

#sexo
microdados_ti = microdados_ti %>% mutate(sexo = case_when( TP_SEXO == "M" ~ "Masculino",
                                                           TP_SEXO == "F" ~ "Feminino"
                                                           )) 

microdados_ti = microdados_ti %>% mutate(hestudos = case_when( QE_I23 == "A" ~ "Nenhuma, apenas assisto Às aulas",
                                                               QE_I23 == "B" ~ "De uma a três",
                                                               QE_I23 == "C" ~ "De quatro a sete",
                                                               QE_I23 == "D" ~ "De oito a doze",
                                                               QE_I23 == "E" ~ "Mais de doze"
                                                               )) 

#removendo a variável estado civil 2
microdados_ti = subset(microdados_ti, select = -c(estado_civil2))

#verificando
names(microdados_ti)

#Verificando a classe das variáveis
class(microdados_ti$estado_civil)
class(microdados_ti$regiao)
class(microdados_ti$sexo)
class(microdados_ti$hestudos)
class(microdados_ti$NT_OBJ_CE)

#BLOCO DA ANÁLISE DESCRITIVA DAS VARIÁVEIS

#Resumindo os dados
s=summary(microdados_ti)  
d=describe(microdados_ti)

#Selecionan do só os resumos de interesse
d$TP_SEXO
d$regiao$values
d$CO_REGIAO_CURSO$values$frequency/sum(d$CO_REGIAO_CURSO$values$frequency)
s[1:2,1:4]

#Achando as frequências simples das variáveis de outra forma:
t=table(microdados_ti$estado_civil)
p=prop.table(t)

#resumo estado civil
describe(microdados_ti$estado_civil)
unique(microdados_ti$estado_civil)

#Total, agrupado por Estado civil
microdados_ti %>% 
  select(estado_civil) %>% 
  group_by(estado_civil) %>% 
  summarise(total = n())

#média, agrupada por Estado civil
microdados_ti %>% 
  select(estado_civil,NT_OBJ_FG) %>% 
  group_by(estado_civil) %>% 
  summarise(media = mean(NT_OBJ_FG,na.rm = T))

#Removendo  NAS De Todas As Variáveis, Que possuem NA
microdados_ti_sem_NA=microdados_ti %>% na.omit()

resumo_nas=microdados_ti_sem_NA %>%
  select(everything()) %>%  
  summarise_all(list(~sum(is.na(.))))

View(resumo_nas)

#Quatidade De Linhas Do Banco Original
dim(microdados_ti)[1]
#Quatidade De Linhas Do Banco sem os NAS
dim(microdados_ti_sem_NA)[1]

#Total de linhas removidas que continhm NAS
total_linhas_excluidas=dim(microdados_ti)[1]-dim(microdados_ti_sem_NA)[1]

#Estatísticas descritivas da variável NOTA

#Calulando o Tamanho do vetor de notas
quantidade_de_notas=length(microdados_ti_sem_NA$NT_OBJ_CE)
#Calculando a Média
media=mean(microdados_ti_sem_NA$NT_OBJ_CE)
#Calculando a mediana
#De forma direta
mediana=median(microdados_ti_sem_NA$NT_OBJ_CE)
#teoria
#Como temos n par = 9636, teremos duas posições centrais (n/2) e (n/2+1)
#9636/2=4818 e mediana=(obs4818+obs4819)/2
#Calculando teoricamente
(sort(microdados_ti_sem_NA$NT_OBJ_CE)[4818]+sort(microdados_ti_sem_NA$NT_OBJ_CE)[4819])/2
#Moda
#Primeira etapa: Calcular as frequências simples
fs=table(microdados_ti_sem_NA$NT_OBJ_CE)
#Calcular o máximo das frequências simples
maximo=max(fs)
#trazer os nomes que correspondem as observações das fs
nomes=names(fs)
#trazer os nomes que satisfazem a comparação lógica
moda_texto=nomes[fs==maximo]
#Transformar em número
moda_numero=as.numeric(moda_texto)

microdados_ti_sem_NA %>% select(NT_OBJ_CE)  %>% 
                         table()            %>%
                         which.max ()       %>% 
                         names ()           %>%  
                         as.numeric()

consolidado_notas=data.frame("Quantidade_de_notas"=quantidade_de_notas,
                       "Media"=media,
                       "Mediana"=mediana,
                       "moda"=moda_numero)
#moda= as.numeric (names(table(microdados_ti_sem_NA$NT_OBJ_CE))[table(microdados_ti_sem_NA$NT_OBJ_CE) == max(table(microdados_ti_sem_NA$NT_OBJ_CE))])

#Máximo das frequências é 1161 e corresponde a observação número 50, logo 40 é a moda.
#Logo temos que a média(42.09)>mediana(40)=moda(40), logo não podemos afirmar que a distribuição é assimétrica, contudo apresentando
#uma leve simetria, que só poderemos afirmar pelo cálculo do coeficiente de assimetria de pearson.

#Para calcular a assimetria:
library(e1071)
assimetria=skewness(microdados_ti_sem_NA$NT_OBJ_CE)

#Coefieciente de assimetria de pearson>0, logo terá assimetria positiva e concentração a esquerda dos maiores valores.

#A curtose do que o R calcula, é padronizada, tirando -3, comparada a da normal
curtose=kurtosis(microdados_ti_sem_NA$NT_OBJ_CE)

consolidado_notas_completo=cbind(consolidado_notas,assimetria, curtose)
#pelo R, temos que se 
#k>0, leptocúrtica
#k=0, Mesocúrtica
#k<0, Platicúrtica

#Consideramos então platicúrtica.

#VAMOS FAZER ALGUNS GRÁFICOS PARA IDENTIFICAR O QUE CONSTANTAMOS

#GRÁFICO HISTOGRAMA DA NOTA DOS ALUNOS COM A FREQUÊNCIA RELATIVA DAS NOTAS
g_hist=ggplot(microdados_ti_sem_NA,aes(x=NT_OBJ_CE)) + 
              geom_histogram(color = "black",fill="lightblue",bins =50,aes(y=(..count..)/sum(..count..)))+
              ggtitle("Histograma da nota dos alunos de análise de sistemas")+
              xlab("nota") +
              ylab("Frequência relativa")
g_hist

g_densidade=ggplot(microdados_ti_sem_NA,aes(x=NT_OBJ_CE))+
                   geom_density(col=2,size = 1, aes(y = 27 * (..count..)/sum(..count..))) +
                   ggtitle("Curva de densidade da nota dos alunos de análise de sistemas") +
                   xlab("nota") +
                   ylab("Frequência relativa")
g_densidade
ggplotly(g_densidade)

g_hist_densidade = ggplot(microdados_ti_sem_NA,aes(x=NT_OBJ_CE)) + 
                          geom_histogram(color = "black",fill="lightblue",bins =50,aes(y=(..count..)/sum(..count..)))+
                          geom_density(col=2,size = 1, aes(y = 27 * (..count..)/sum(..count..))) +
                          ggtitle("Histograma e curva de densidade da nota dos alunos de análise de sistemas")+
                          xlab("nota") +
                          ylab("Frequência relativa")
g_hist_densidade
ggplotly(g_hist_densidade)

grid.arrange( g_hist,
              g_densidade,
              g_hist_densidade,
              nrow=3,ncol=1)

# CONTINUAÇÃO BLOCO DA ANÁLISE DESCRITIVA DAS VARIÁVEIS

#Comparar as médias por sexo e estado civil
microdados_ti_mod2= microdados_ti_sem_NA %>% 
        select(estado_civil,NT_GER,sexo) %>% 
             group_by(sexo,estado_civil) %>% 
             summarise(  quantidade=n(),
                         media = mean(NT_GER,na.rm = T),
                         mediana = median(NT_GER,na.rm = T),
                         cv=sd(NT_GER,na.rm=T)/media*100,
                         amplitude_interquartil=IQR(NT_GER)) %>% 
             arrange(desc(mediana))

#Tabulação cruzada
table(microdados_ti_sem_NA$estado_civil,microdados_ti_sem_NA$sexo)

#Tabulação cruzada proporção
prop.table(table(microdados_ti_sem_NA$estado_civil,microdados_ti_sem_NA$sexo))

require(e1071)
#assimetria e curtose

dados_casados = microdados_ti_sem_NA %>% 
         select(estado_civil,NT_GER) %>% 
              group_by(estado_civil) %>% 
              #filter(estado_civil=="Casado(a)") %>% 
              summarise(  quantidade=n(),
                          media = mean(NT_GER),
                          mediana = median(NT_GER),
                          cv=sd(NT_GER)/media*100,
                          amplitude_interquartil=IQR(NT_GER),
                          assimetria=skewness(NT_GER),
                          curtose=kurtosis(NT_GER)
                          ) %>% 
              
              arrange(desc(cv))

#Histograma
dados=microdados_ti_sem_NA 
grafico_histograma1 = ggplot(dados, aes(x=NT_GER,fill=estado_civil)) + 
  geom_histogram() +
  ggtitle("Gráfico histograma da Nota por estado civil") +
  xlab("Notas") +
  ylab("Frequência simples") +
  facet_grid(~estado_civil)

ggplotly(grafico_histograma1)

dados=microdados_ti_sem_NA
grafico_boxplot1 = ggplot(dados, aes(x=estado_civil,y=NT_GER,fill=estado_civil)) + 
  geom_boxplot() +
  ggtitle("Gráfico de Box-plot da Nota por Estado civil e Sexo")+
  xlab("Estado civil") +
  ylab("Notas") +
  facet_grid(~sexo)

ggplotly(grafico_boxplot1)

#----
#Comparar as médias por sexo e região
microdados_ti_mod3= microdados_ti_sem_NA %>% 
  select(estado_civil,NT_GER,regiao,hestudos,sexo) %>% 
  group_by(sexo,regiao) %>% 
  summarise(quantidade=n(),
            media = mean(NT_GER),
            mediana = median(NT_GER),
            cv=sd(NT_GER)/media*100,
            amplitude_interquartil=IQR(NT_GER),
            assimetria=skewness(NT_GER),
            curtose=kurtosis(NT_GER)) %>% 
  arrange(desc(media))

#Tabulação cruzada
table(microdados_ti_sem_NA$regiao,microdados_ti_sem_NA$sexo)

#Tabulação cruzada proporção
prop.table(table(microdados_ti_sem_NA$regiao,microdados_ti_sem_NA$sexo))

#Histograma
dados=microdados_ti_sem_NA
grafico_histograma2 = ggplot(dados, aes(x=NT_GER,fill=regiao)) + 
  geom_histogram()+
  ggtitle("Gráfico histograma da Nota por região e sexo" )+
  xlab("Notas") +
  ylab("Frequência simples") +
  facet_grid(~sexo)

ggplotly(grafico_histograma2)

#box-plot
dados=microdados_ti_sem_NA
grafico_boxplot2 = ggplot(dados, aes(x=regiao,y=NT_GER,fill=regiao)) + 
  geom_boxplot() +
  ggtitle("Gráfico boxplot da Nota por região e sexo")+
  ylab("Notas") +
  facet_grid(~sexo)

ggplotly(grafico_boxplot2)

grid.arrange( grafico_histograma1,
              grafico_boxplot1,
              grafico_histograma2,
              grafico_boxplot2,
              nrow=2,ncol=2)

#BLOCO GRÁFICOS POINT CLICK POR MEIO DO ESQUISSE

#Análises gráficas point click
#microdados_ti_mod= microdados_ti_sem_NA %>% 
#  select(estado_civil,NT_OBJ_FG,regiao,hestudos,sexo) %>% 
#  group_by(estado_civil,regiao,hestudos,sexo) %>% 
#  summarise(media = mean(NT_OBJ_FG,na.rm = T))

#devtools::install_github("dreamRs/esquisse")
#esquisser(viewer = "browser")
