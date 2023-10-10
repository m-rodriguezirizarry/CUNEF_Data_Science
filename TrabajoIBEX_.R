
library(ggplot2)
library(corrplot)
library(readr)
library(dplyr)
library(Hmisc)
library(janitor) # limpieza de datos
library(patchwork)


# Lectura de datos de Yahoo Finance en formato csv
BBVA_MC <- read.csv(file="~/Documents/MASTER CIENCIA DATOS/Fundamentos/TrabajoIBEX/BBVA.MC.csv", header = TRUE, sep = "," ,dec = ".")
CAIXABANK <- read.csv(file="~/Documents/MASTER CIENCIA DATOS/Fundamentos/TrabajoIBEX/CABK.MC.csv", header = TRUE, sep = "," ,dec = ".")
IBEX35 <- read.csv(file="~/Documents/MASTER CIENCIA DATOS/Fundamentos/TrabajoIBEX/IBEX.csv", header = TRUE, sep = "," ,dec = ".")

# Limpieza de datos con la funcion clean_names()
BBVA_MC <- clean_names(BBVA_MC)
CAIXABANK <- clean_names(CAIXABANK)
IBEX35 <- clean_names(IBEX35)

#A través de la función attach(), llamamos a los datos de la columna de "Cierre Ajustado" 
attach(BBVA_MC)
names(BBVA_MC)
Banco_BBVA <- BBVA_MC[,6]

length(Banco_BBVA)

attach(CAIXABANK)
names(CAIXABANK)
Banco_Caixabank <- CAIXABANK[,6]

length(Banco_Caixabank)

attach(IBEX35)
names(IBEX35)
Indice_IBEX35 <-IBEX35 [,6]
length(Indice_IBEX35)

length(date)

#Construcción de los Data Frame

DatosBanca_COVID <- data.frame(date, Banco_BBVA, Banco_Caixabank, Indice_IBEX35)
head(DatosBanca_COVID)
tail(DatosBanca_COVID)
glimpse(DatosBanca_COVID)

#Construcción de fechas
DatosBanca_COVID$date <- as.Date(DatosBanca_COVID$date)

DatosBanca_COVID <- DatosBanca_COVID%>% 
  mutate(
    dcovid = case_when(
      date < "2020-03-15" ~ "precovid",
      date >= "2020-03-15" & date < "2023-06-05" ~ "postcovid",
      TRUE ~ "postcovid"  
    )) 
head(DatosBanca_COVID)
tail(DatosBanca_COVID)


# Series Temporales 

#En estos gráficos podemos observar que la caída de valores que ocurrió al principio del
#encierro por COVID-19 tuvo efectos significativos en los valores de los tres activos. En 
#en el resto del periodo observado, los tres activos se movieron de manera similar, sus valores
#aumentaron pero no regresaron a la tendencia de valores antes del encierro por COVID-19. 
g1<-ggplot(DatosBanca_COVID, aes(x=date, y=Banco_BBVA, col = dcovid))+
  geom_line()
g2<-ggplot(DatosBanca_COVID, aes(x=date, y=Banco_Caixabank, col = dcovid))+
  geom_line()
g3<-ggplot(DatosBanca_COVID, aes(x=date, y=Indice_IBEX35, col = dcovid))+
  geom_line()

g1/g2/g3


# Anadir tendencia

#En este grafico continuamos con las observaciones de las series temporales añadiendo una
#línea de tendencia para facilitar la ilustración de los hechos. 


gg1<-ggplot(DatosBanca_COVID, aes(x=date, y=Banco_BBVA))+
  geom_line()+
  stat_smooth(method="loess", color="turquoise", fill="turquoise") +
  theme_minimal()
gg2<-ggplot(DatosBanca_COVID, aes(x=date, y=Banco_Caixabank))+
  geom_line()+
  stat_smooth(method="loess", color="turquoise", fill="turquoise") +
  theme_minimal()
gg3<-ggplot(DatosBanca_COVID, aes(x=date, y=Indice_IBEX35))+
  geom_line()+
  stat_smooth(method="loess", color="turquoise", fill="turquoise") +
  theme_minimal()

gg1/gg2/gg3

# Combinar las tres series
#ppoint

#En este grafico continuamos con las observaciones de las series temporales combinando las 
#tres en un solo gráfico para facilitar la ilustración de los hechos. 

Gráfico_combinado <- ggplot(data = DatosBanca_COVID, aes(x = date)) +
  geom_line(aes(y = Banco_BBVA, color = "Banco BBVA"), linetype = "solid") +
  geom_line(aes(y = Banco_Caixabank, color = "Banco CaixaBank"), linetype = "solid") +
  geom_line(aes(y = as.numeric(Indice_IBEX35) / 1000, color = "Índice IBEX35"), linetype = "solid") +
  labs(y = "Serie", x = "Fecha") +
  scale_y_continuous(limits = c(1, 10)) +
  scale_color_manual(values = c("Banco BBVA" = "turquoise", "Banco CaixaBank" = "blue", "Índice IBEX35" = "violet")) +
  theme_minimal() +
  theme(legend.position = "top")

Gráfico_combinado

# BoxPlot (Diagramas de caja)

#En este gráfico continuamos observando los valores de nuestros tres activos en el periodo
#que determinamos como post-covid y pre-covid. A través del ejercicio de insertar los valores dentro de 
#los gráficos box plot observamos los efectos del covid en los valores de nuestros activos. La media de
#los valores del BBVA decrecieron en un 34%, los del Caixa 23% y los del IBEX 22%. Los movimientos son  
#similares en los tres activos demostrando una tendencia de pérdida de valor después del covid que no se
#logró recuperar en el periodo de tiempo observado en este etsudio. 

g21<-ggplot(DatosBanca_COVID)+
  geom_boxplot(aes(x=date, y=Banco_BBVA, fill = dcovid))
g22<-ggplot(DatosBanca_COVID)+
  geom_boxplot(aes(x=date, y=Banco_Caixabank, fill = dcovid))
g23<-ggplot(DatosBanca_COVID)+
  geom_boxplot(aes(x=date, y=Indice_IBEX35, fill = dcovid))

g21/g22/g23

# Cálculo de rendimientos

n <- dim(DatosBanca_COVID)[1]
rIBEX <- as.numeric(Indice_IBEX35)[2:n]/as.numeric(Indice_IBEX35)[1:n-1] - 1
rIBEX
rBBVA  <- Banco_BBVA[2:n] / Banco_BBVA[1:n-1] - 1
rCAIXABANK  <- Banco_Caixabank[2:n] / Banco_Caixabank[1:n-1] - 1
date <- date[2:n]


# Construcción del Data frame Rendimientos
rend_datos<-data.frame(date, rIBEX, rBBVA, rCAIXABANK)
head(rend_datos)
tail(rend_datos)

# Construcción de Fechas para el nuevo Data Frame
df_rend <- rend_datos%>% 
  mutate(
    dcovid = case_when(
      date < "2020-03-15" ~ "precovid",
      date >= "2020-03-15" & date < "2023-06-05" ~ "postcovid",
      TRUE ~ "postcovid"  
    )) 
head(df_rend)
tail(df_rend)

df_rend$date <- as.Date(df_rend$date)
class(df_rend$date)

#Gráficos temporales de los rendimientos

#Nuestro cálculo de rendimiento deriva del valor de los activos. La volatiliad de los valores
#de los activos que componen nuestra cartera después del covid se pueden observar en los picos de 
#subidas y caídas del rendimiento de los activos en el periodo después del covid. Los tres activos tienen
#movimientos similares. 

g31<-ggplot(df_rend)+
  geom_line( aes(x=date, y=rBBVA, col = dcovid))
g32<-ggplot(df_rend)+
  geom_line(aes(x=date, y = rCAIXABANK, col = dcovid))
g33<-ggplot(df_rend)+
  geom_line(aes(x=date, y = rIBEX,col= dcovid ))

g31/g32/g33


#df rendimientos
rendimientos <- data.frame( rBBVA, rCAIXABANK, rIBEX)
head(rendimientos)
tail(rendimientos)


summary(rendimientos)
cor(rendimientos) 

#La correlación entre los rednimientos de CaixaBank y BBVA es de 0.79 aproximadamente.
#Esto indica que hay una correlación positiva fuerte entre los rendimientos de estos dos bancos.
#Si los rendimientos de BBVA aumentan, es probable que los de CaixaBank también aumenten.
#La correlación entre los rendimientos de BBVA y el IBEX 35 es aproximadamente 0.8444; y entre 
#La Caixa y el IBEX es aproximadamente 0.7507. Esto indica una correlación positiva fuerte,
#por lo que si alguno de estos activos tiene un buen desempeño, es probable que el resto 
#también lo tenga. Aunque las correlaciones sean positivas, no son perfectas así que puede
#existir algo de riesgo.

cov(rendimientos)

# Correlaciones
correlaciones <- rcorr(as.matrix(rendimientos),type="pearson") 
correlaciones
correlaciones$r

corrplot(correlaciones$r, type = "upper", order="hclust", tl.srt=45)


# Modelos CAPM 

#En el gráfico de CAPM comparando el rendimiento del BBVA y del IBEX podemos observar que existe
#una correlación fuerte entre ellos según observado en el gráfico. Los rendimientos del BBVA aumentan 
#según aumentan los del IBEX35 en la mayoría de los casos observados.Observando la pendiente de la línea de
#regresión confirmamos que los movimientos entre los rendimientos de BBVA y el IBEX son similares. 


CAPM1<-lm(rBBVA ~ rIBEX)
summary(CAPM1)
ggplot(rend_datos) +
  geom_point(aes(x = rIBEX, y = rBBVA)) +
  stat_smooth(aes(x = rIBEX, y = rBBVA), col = "orange", method = "lm") +
  geom_text(
    x = max(rend_datos$rIBEX), 
    y = min(rend_datos$rBBVA), 
    label = paste("Slope =", round(coef(CAPM1)[2], 4)), 
    hjust = 1, vjust = 0
  )

#En el gráfico de CAPM comparando el rendimiento del Caixa y del IBEX podemos observar que existe
#una correlación fuerte entre ellos según observado en el gráfico. Los rendimientos del Caixa aumentan 
#según aumentan los del IBEX35 en la mayoría de los casos observados. Observando la pendiente de la línea de
#regresión confirmamos que los movimientos entre los rendimientos de BBVA y el IBEX son similares.
                                                                                                                                                                                                                                               
CAPM2<-lm(rCAIXABANK ~ rIBEX)
summary(CAPM2)
ggplot(rend_datos) +
  geom_point(aes(x=rIBEX, y=rCAIXABANK)) +
  stat_smooth(aes(x=rIBEX, y=rCAIXABANK), col= "orange", method = "lm")+
geom_text(
  x = max(rend_datos$rIBEX), 
  y = min(rend_datos$rCAIXABANK), 
  label = paste("Slope =", round(coef(CAPM1)[2], 4)), 
  hjust = 1, vjust = 0
)


# Grafico matricial

pairs(rendimientos, panel = panel.smooth, main = "Rendimientos", col = "orange")

# Calcular los rendimientos medios

#Los rendimientos medios de los activos en nuestra bolsa son bajos pero esperados por el contexto 
#del periodo de tiempo. En esta época el Euribor estaba en mínimos históricos (por debajo del 0%) lo cual 
#tenía un efecto en la rentabilidad de los productos que ofrecían los bancos y por consecuente su valor en
#bolsa.

rendimiento_medio_IBEX <- mean(rIBEX)
rendimiento_medio_BBVA <- mean(rBBVA)
rendimiento_medio_CAIXABANK <- mean(rCAIXABANK)


# Calcular el coeficiente de correlación entre BBVA y CaixaBank

BBVA20ultimos <- tail(rBBVA, 20)
Caixa20ultimos <- tail(rCAIXABANK,20)
IBEX20ultimos <- tail(rIBEX, 20) 
rendimiento_medio_IBEX <- mean(IBEX20ultimos)
rendimiento_medio_BBVA <- mean(BBVA20ultimos)
rendimiento_medio_CAIXABANK <- mean(Caixa20ultimos)

m1 <- BBVA20ultimos
m2 <- Caixa20ultimos
m3 <- IBEX20ultimos

# Calcular las desviaciones estándar

#Las desviaciones estándar de nuestros activos indican que la variación de los valores de nuestros activos 
#es baja (menos de 1€ cada activo) lo cual señala el bajo nivel de riesgo que presentan los componentes de 
#nuestra cartera. 

s1 <- sd(BBVA20ultimos)
s2 <- sd(Caixa20ultimos)
s3 <- sd(IBEX20ultimos)

# Covarianza entre BBVA y Caixabank

#La covarianza positiva de entre estos dos activos indica que el movimiento de los valores tiene una correlación
#positiva, los valores de ambas acciones se tiende a mover en las mismas direcciones.


num_bbva_caixa <- sum((BBVA20ultimos - rendimiento_medio_BBVA) * (Caixa20ultimos - rendimiento_medio_CAIXABANK))
cov_bbva_caixa <- num_bbva_caixa / (length(BBVA20ultimos) - 1)

cov_bbva_caixa

# Covarianza entre BBVA y IBEX

#La covarianza positiva de entre estos dos activos indica que el movimiento de los valores tiene una correlación
#positiva, los valores de ambas acciones se tiende a mover en las mismas direcciones.

num_bbva_ibex <- sum((BBVA20ultimos - rendimiento_medio_BBVA) * (IBEX20ultimos - rendimiento_medio_IBEX))
cov_bbva_ibex <- num_bbva_ibex / (length(BBVA20ultimos) - 1)

cov_bbva_ibex

# Covarianza entre CaixaBank y IBEX

#La covarianza positiva de entre estos dos activos indica que el movimiento de los valores tiene una correlación
#positiva, los valores de ambas acciones se tiende a mover en las mismas direcciones.

num_caixa_ibex <- sum((Caixa20ultimos - rendimiento_medio_CAIXABANK) * (IBEX20ultimos - rendimiento_medio_IBEX))
cov_caixa_ibex <- num_caixa_ibex / (length(Caixa20ultimos) - 1)

cov_caixa_ibex

# Matriz de covarianzas

V <- matrix(c(var(BBVA20ultimos), cov_bbva_caixa, cov_bbva_ibex,
              cov_bbva_caixa, var(Caixa20ultimos), cov_caixa_ibex,
              cov_bbva_ibex, cov_caixa_ibex, var(IBEX20ultimos)
), nrow = 3, byrow = TRUE)
V

# Creación de un vector con los rendimientos medios
m <- c(rendimiento_medio_BBVA, rendimiento_medio_CAIXABANK, rendimiento_medio_IBEX)
w <- c(0.3, 0.3, 0.4)  # Vector con los pesos de la cartera

# Rendimiento medio de la cartera
#El rendimiento medio de nuestra cartera es bueno en comparación con el rendimiento del IBEX35.
#Según la información histórica en la web de la Bolsa y Mercados Españoles la rentabilidad media
#en el periodo del 2019-2021 fue de 1.43, por lo cual concluimos que nuestra cartera tiene buen
#rendimiento.

ER <- t(w) %*% m
ER

# Varianza de la cartera
#La viarianza de nuestra cartera es baja lo cual indica un bajo nivel de riesgo en los activos que la componen
#a su vez esto significaría un bajo retorno en la inversión. Este perfil de inversiones es positivo para 
#inversionistas aversos al riesgo. 


VR <- t(w) %*% V %*% w
VR

# Volatilidad de la cartera (raíz cuadrada de la varianza)
#El cálculo de la volailidad de nuestra cartera indica que su valor puede variar por 1,52€.
#Consideramos esta volatilidad comp aceptable dentro del perfil de bajo riesgo que tiene nuestra cartera. 

volatilidad_cartera <- sqrt(VR) * 100  
volatilidad_cartera
