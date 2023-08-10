## Limpieza de la base datos EGYPV2016Turistas
library(haven)
library(foreign)
EGYPV2016Turistas <- read.spss("Datos/EGYPV 2016 F01 - Turistas.sav")
EGYPV2016Turistas<-data.frame(EGYPV2016Turistas)
## Filtrado de las variables que se necesitan para la estimación 
EGYPV2016TN<-subset(EGYPV2016Turistas,select = c(Validas, TipViajero,TipVisitante,Mes,CodRes,RegRes,CodPrimVisita,P29_Mu4,
                                                 GruViaje,P10_3NumNoch,Pernocto,GruNoch,P11_THoteles,P11_TAmigos,P11_TAlojP,
                                                 CodTipMot,CodModViaje,P28_Total,CodEstCivil,P9_GruViaje,P10_Pernocto,
                                                 GastoTotalXPers,
                                                 GastoFin,GastoTotalXPersXDia,Ciudad1,Zona1))
## Filtrado solo para los datos de Gasto y Perfil
EGYPV2016TNF<-subset(EGYPV2016TN,Validas=="Gasto y Perfil")

## Conversión del Gasto Fin a escala Logarítmica
GastoFinN=EGYPV2016TNF$GastoFin
LogGFN=log(GastoFinN)

## Gráficos de las densidades para GastoFinN y LogGFN
library(ggplot2)
ggplot(data = EGYPV2016TNF) +
  geom_density(aes(x =LogGFN, fill = Zona1), bins = 30)+
  xlab("LogGFN") +
  ylab("") +
  ggtitle("Densidades del LogGFN por Zona")+facet_wrap(~ Zona1, nrow = 2)
## Gráficos de otros casos
library(tidyverse)
library(tidyquant)
library(ggdist)
library(ggthemes)
EGYPV2016TNF %>% 
  filter(Zona1 %in% c("Zona Centro", "Zona Insular", "Zona Norte","Zona Occidental",
                      "Zona Oriental","Zona Sur","Desconocido")) %>% 
  ggplot(aes(x = factor(Zona1), y = LogGFN, fill = factor(Zona1)))+
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.5
  )+
  theme_tq() +
  labs(
    title = "Densidades y Box-plots del LogGFN por Zona",
    x = "LogGFN",
    y = "",
    fill = "Zonas"
  ) +
  coord_flip()

### Grafico Por Regiones
EGYPV2016TNF %>% 
  filter(RegRes %in% c("Centroamérica", "Europa", "Norteamérica","Resto del Mundo")) %>% 
  ggplot(aes(x = factor(RegRes), y = LogGFN, fill = factor(RegRes)))+
  
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.5
  ) +
  # Themes and Labels
  scale_fill_tq() +
  theme_tq() +
  labs(
    title = "Densidades y Box-plots del LogGFN por Region de Residencia",
    x = "",
    y = "Gasto Logaritmico",
    fill = "Zonas"
  ) +
  coord_flip()


# Graficos del gasto  final por zonas
library(ggridges)
library(ggplot2)
# basic example
ggplot(EGYPV2016TNF, aes(x = GastoFinN, y = Zona1, fill = Zona1)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")+
  labs(
    title = "Densidades del gasto final por Zonas",
    x = "",
    y = "",
    fill = "Zonas"
  ) 
### Calculo de las estadisticas descriptivas de la variables GastoFin
### y por Zonas respectivamente

summary(EGYPV2016TNF$GastoFin)
##Desviacion
sd(GastoFinN,na.rm = TRUE)
## Asimetria
skewness(GastoFinN)
## Curtosis
kurtosis(GastoFinN)


## Resumen de los turisticas de cuantas noches pernotaron en el pais
summary(EGYPV2016TNF$P10_3NumNoch)
## Base para identificar los turistas que gastaron arriba y menos de 700 dolares

EGYP1<-subset(EGYPV2016TNF,GastoFin > 700)
EGYP2<-subset(EGYPV2016TNF,GastoFin < 700)

## Estadística Descriptivas por Zonas
resumen_zonas <- EGYPV2016TNF %>%
  group_by(Zona1) %>%
  summarise(
    count(),
    q1 = quantile(GastoFin, 0.25,na.rm = TRUE),
    Media = mean(GastoFin, na.rm = TRUE),
    Mediana = median(GastoFin, na.rm = TRUE),
    DesvEstandar = sd(GastoFin, na.rm = TRUE),
    q3 = quantile(GastoFin, 0.75,na.rm = TRUE),
    Minimo = min(GastoFin, na.rm = TRUE),
    Maximo = max(GastoFin, na.rm = TRUE),
    asimetria=skewness(GastoFin,na.rm = TRUE),
    Curtosis=kurtosis(GastoFin,na.rm=TRUE)
  )
print(resumen_zonas)

