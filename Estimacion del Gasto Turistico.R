## Limpieza de la base datos EGYPV2016Turistas
library(haven)
library(foreign)
EGYPV2016Turistas <- read.spss("EGYPV 2016 F01 - Turistas.sav")
EGYPV2016Turistas<-data.frame(EGYPV2016Turistas)
## Filtrado de las variables que se necesitan para la estimación EGYPV2016TN<-subset(EGYPV2016Turistas,select = c(Validas, TipViajero,TipVisitante,Mes,CodRes,RegRes,CodPrimVisita,P29_Mu4,
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
