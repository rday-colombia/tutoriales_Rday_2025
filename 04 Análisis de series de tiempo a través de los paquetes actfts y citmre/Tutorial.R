########## Series de tiempo con ACTFTS usando los datos de cITMre ##########

install.packages("actfts")
install.packages("citmre")

library(actfts)
library(citmre)

help("actfts")
help("citmre")


######################### Aprendiendo a usar cITMre #########################

######### Generalidades #########
TRM <- rmre_data()
plot(TRM)

rmre_data(plot_data = T)

######### Extracción de datos por fechas #########

TRM1 <- rmre_data(start_date = "2015-11-01")
plot(TRM1)

TRM2 <- rmre_data(start_date = "2010-11-01", end_date = "2015-11-04")
plot(TRM2)

######### Obtención de los Retornos #########

TRM3 <- rmre_data(start_date = "2015-11-04", log_return = T)
plot(TRM3)

rmre_data(start_date = "2015-11-04", log_return = T, plot_data = T)

######### Jugando con las frecuencias de tiempo #########

TRM4 <- rmre_data(start_date = "2015-11-04", frequency = 12)
plot(TRM4)

rmre_data(start_date = "2015-11-04", frequency = 4, plot_data = T)

######### Jugando con el tipo de dato #########

TRM5 <- rmre_data(start_date = "2015-11-04", frequency = 12, type = "mean")
plot(TRM5)

TRM6 <- rmre_data(start_date = "2015-11-04", frequency = 12, type = "last_date")
plot(TRM6)


######################### Aprendiendo a usar actfts #########################

TRM_Ejemplo <- rmre_data(start_date = "2020-01-01")

######### Generalidades #########

actfts::acfinter(TRM_Ejemplo)

######### Diferenciando la serie y analisando el resultado #########

actfts::acfinter(TRM_Ejemplo, delta = "diff1", lag = 20)

######### con intervalos de confianza a traves de medias móviles #########

actfts::acfinter(TRM_Ejemplo, ci.method = "ma" , lag = 20)

######### para descargar #########

actfts::acfinter(TRM_Ejemplo, download = T)

