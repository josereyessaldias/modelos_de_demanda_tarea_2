install.packages("weights")
library(weights)
install.packages("corrplot")
library("corrplot")

library(ggplot2)
library(readr)
library(dplyr)

aa <- read_csv("datos_calibracion.csv")
str(aa)
count(aa, Personas)
count(aa, Vehículos)
count(aa, BicicletasAdulto)
count(aa, BicicletasNino)
count(aa, Trabajadores)
count(aa, Estudiantes)
count(aa, Jubilados)
count(aa, DuenosdeCasa)
count(aa, Jubilados)
count(aa, Otros)
count(aa, Discapacidad)
count(aa, ViajesTrabajo)
count(aa, ViajesEstudio)
count(aa, ViajesOtro)

ab <- aa %>% filter(Personas==Hombres+Mujeres)
ab2 <- ab %>% filter(ViajesTrabajo <= 5*Trabajadores)
ab3 <- ab2 %>% filter(ViajesEstudio <= 5*Estudiantes)
ac <- ab3 %>% filter(Personas==Niños+Jovenes+AdultoJovenes+Adultos+AdultoMayores)
ad <- ac %>% filter(!(Jovenes+AdultoJovenes+Adultos+AdultoMayores<LicenciasdeConducir))
ae <- ad %>% filter(!(BicicletasAdulto==98 | BicicletasAdulto==99))
af <- ae %>% filter(!(BicicletasNino==98 | BicicletasNino==99))


ggplot(aa, aes(x=Comuna))+geom_bar()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
hist(aa$Personas, breaks = 10)
hist(aa$Vehículos, breaks = 6, ylab="Frecuencia", xlab="Cantidad de Vehículos", main=NULL)
hist(aa$BicicletasAdulto, breaks = 10)
hist(aa$BicicletasNino, breaks = 10)
hist(aa$IngresoHogar, breaks = 400, ylab="Frecuencia", xlab="Ingreso por Hogar", main=NULL, xlim = c(0,2000000))

hist(aa$LicenciasdeConducir, breaks = 10)
hist(aa$PasesEscolares, breaks = 10)

hist(aa$Hombres, breaks = 10)
hist(aa$Mujeres, breaks = 10)

hist(aa$Trabajadores, breaks = 10)
hist(aa$Estudiantes, breaks = 10)
hist(aa$Jubilados, breaks = 10)
hist(aa$DuenosdeCasa, breaks = 10)
hist(aa$Desempleados, breaks = 10)
hist(aa$Otros, breaks = 10)

hist(aa$Niños, breaks = 10)
hist(aa$Jovenes, breaks = 10)
hist(aa$AdultoJovenes, breaks = 10)
hist(aa$Adultos, breaks = 10)
hist(aa$AdultoMayores, breaks = 10)

hist(aa$Discapacidad, breaks = 10)
hist(aa$ViajesTrabajo, breaks = 10)
hist(aa$ViajesEstudio, breaks = 10)
hist(aa$ViajesOtro, breaks = 10)

ag <- af %>% mutate(ln_ingreso = log(IngresoHogar+1))
ah <- ag %>% mutate(ln_vehiculo = log(Vehículos+1))
ai <- ah %>% mutate(ln_BicicletasAdulto = log(BicicletasAdulto+1))
aj <- ai %>% mutate(ln_BicicletasNino = log(BicicletasNino+1))
ak <- aj %>% mutate(ln_PasesEscolares = log(PasesEscolares+1))
al <- ak %>% mutate(ln_LicenciasdeConducir = log(LicenciasdeConducir+1))

am <- al %>% mutate(j_est = sqrt(Jovenes*Estudiantes))
an <- am %>% mutate(adj_est = sqrt(AdultoJovenes*Estudiantes))
ao <- an %>% mutate(ad_est = sqrt(Adultos*Estudiantes))
ap <- ao %>% mutate(adm_est = sqrt(AdultoMayores*Estudiantes))

aq <- ap %>% mutate(adj_tr = sqrt(AdultoJovenes*Trabajadores))
ar <- aq %>% mutate(ad_tr = sqrt(Adultos*Trabajadores))
as1 <- ar %>% mutate(adm_tr = sqrt(AdultoMayores*Trabajadores))
as <- as1 %>% mutate(m_due = sqrt(Mujeres*DuenosdeCasa))



m1t <- lm(data = as, ViajesTrabajo ~ ln_vehiculo + ln_ingreso + ln_BicicletasAdulto +
           ln_BicicletasNino + ln_PasesEscolares +ln_LicenciasdeConducir +j_est + adj_est +
           ad_est + adm_est + adj_tr + ad_tr +adm_tr + Discapacidad + AdultoMayores + Adultos +
           AdultoJovenes+Jovenes+Niños+Trabajadores+Estudiantes+Jubilados+
           DuenosdeCasa+Desempleados+Otros+Mujeres+m_due, weights = Factor)
summary(m1t)

m2t <- lm(data = as, ViajesTrabajo ~ ln_vehiculo + IngresoHogar +
            ad_tr + Discapacidad + AdultoMayores + Adultos +
            AdultoJovenes+Jovenes+Niños+Trabajadores+Estudiantes+Jubilados+
            DuenosdeCasa+Desempleados+Otros, weights = Factor)
summary(m2t)

par(mfrow = c(1, 1))
hist(m2t$residuals, freq = FALSE, xlab = "Residuos", ylab = "Frecuencia",
     main = "Normalidad de residuos", breaks = 18)
curve(dnorm(x, mean = mean(m2t$residuals), sd = sd(m2t$residuals)), 
      add = TRUE, col = "red")

plot(m2t, 4)
par(mfrow=c(2,2))

plot(m2t, which=1:4)




m1e <- lm(data = as, ViajesEstudio ~ ln_vehiculo + ln_ingreso + ln_BicicletasAdulto +
            ln_BicicletasNino + ln_PasesEscolares +ln_LicenciasdeConducir +j_est + adj_est +
            ad_est + adm_est + adj_tr + ad_tr +adm_tr + Discapacidad + AdultoMayores + Adultos +
            AdultoJovenes+Jovenes+Niños+Trabajadores+Estudiantes+Jubilados+
            DuenosdeCasa+Desempleados+Otros+Mujeres+m_due, weights = Factor)
summary(m1e)

m2e <- lm(data = as, ViajesEstudio ~ ln_BicicletasAdulto +
            ln_BicicletasNino + ln_PasesEscolares +j_est +
            ad_est + Discapacidad + AdultoMayores + Adultos +
            AdultoJovenes+Trabajadores+Estudiantes+Jubilados+
            DuenosdeCasa+Desempleados+Otros, weights = Factor)
summary(m2e)

par(mfrow = c(1, 1))
hist(m2e$residuals, freq = FALSE, xlab = "Residuos", ylab = "Frecuencia",
     main = "Normalidad de residuos", breaks = 18)
curve(dnorm(x, mean = mean(m2e$residuals), sd = sd(m2e$residuals)), 
      add = TRUE, col = "red")

plot(m2e, 4)
par(mfrow=c(2,2))

plot(m2e, which=1:4)



m1o <- lm(data = as, ViajesOtro ~ ln_vehiculo + IngresoHogar + ln_BicicletasAdulto +
            ln_BicicletasNino + ln_PasesEscolares +ln_LicenciasdeConducir +j_est + adj_est +
            ad_est + adm_est + adj_tr + ad_tr +adm_tr + Discapacidad + AdultoMayores + Adultos +
            AdultoJovenes+Jovenes+Niños+Trabajadores+Estudiantes+Jubilados+
            DuenosdeCasa+Desempleados+Otros+Mujeres+m_due, weights = Factor)
summary(m1o)

m2o <- lm(data = as, ViajesOtro ~ ln_vehiculo + IngresoHogar +
            ln_BicicletasNino + ln_PasesEscolares +
            adm_est + ad_tr + AdultoMayores + Adultos +
            Estudiantes+Jubilados+
            DuenosdeCasa+Desempleados+Otros, weights = Factor)
summary(m2o)

par(mfrow = c(1, 1))
hist(m2o$residuals, freq = FALSE, xlab = "Residuos", ylab = "Frecuencia",
     main = "Normalidad de residuos", breaks = 18)
curve(dnorm(x, mean = mean(m2o$residuals), sd = sd(m2o$residuals)), 
      add = TRUE, col = "red")

plot(m2o, 4)
par(mfrow=c(2,2))

plot(m2o, which=1:4)



# ahora predeciré!!!!!

b <- read_csv("datos_prediccion.csv")

bb <- b %>% filter(Personas==Hombres+Mujeres)
bb2 <- bb %>% filter(ViajesTrabajo <= 5*Trabajadores)
bb3 <- bb2 %>% filter(ViajesEstudio <= 5*Estudiantes)
bc <- bb3 %>% filter(Personas==Niños+Jovenes+AdultoJovenes+Adultos+AdultoMayores)
bd <- bc %>% filter(!(Jovenes+AdultoJovenes+Adultos+AdultoMayores<LicenciasdeConducir))
be <- bd %>% filter(!(BicicletasAdulto==98 | BicicletasAdulto==99))
bf <- be %>% filter(!(BicicletasNino==98 | BicicletasNino==99))

bg <- bf %>% mutate(ln_ingreso = log(IngresoHogar+1))
bh <- bg %>% mutate(ln_vehiculo = log(Vehículos+1))
bi <- bh %>% mutate(ln_BicicletasAdulto = log(BicicletasAdulto+1))
bj <- bi %>% mutate(ln_BicicletasNino = log(BicicletasNino+1))
bk <- bj %>% mutate(ln_PasesEscolares = log(PasesEscolares+1))
bl <- bk %>% mutate(ln_LicenciasdeConducir = log(LicenciasdeConducir+1))

bm <- bl %>% mutate(j_est = sqrt(Jovenes*Estudiantes))
bn <- bm %>% mutate(adj_est = sqrt(AdultoJovenes*Estudiantes))
bo <- bn %>% mutate(ad_est = sqrt(Adultos*Estudiantes))
bp <- bo %>% mutate(adm_est = sqrt(AdultoMayores*Estudiantes))

bq <- bp %>% mutate(adj_tr = sqrt(AdultoJovenes*Trabajadores))
br <- bq %>% mutate(ad_tr = sqrt(Adultos*Trabajadores))
bs1 <- br %>% mutate(adm_tr = sqrt(AdultoMayores*Trabajadores))
bs <- bs1 %>% mutate(m_due = sqrt(Mujeres*DuenosdeCasa))


bs$p_trabajo <- predict.lm(object=m2t, newdata = bs)

plot(bs$p_trabajo, bs$ViajesTrabajo, xlab = "Valor Predicho", ylab="Valor Observado")
hist(bs$p_trabajo, breaks = 100)
bs$p_resis_trab <- bs$ViajesTrabajo - bs$p_trabajo
bs$p_resis_trab2 <- (bs$ViajesTrabajo - bs$p_trabajo)^2
sqrt(mean(bs$p_resis_trab2))


hist(bs$p_resis_trab, breaks = 50, xlab="Error de Preducción", ylab="Frecuencia", main=NULL)
mean(abs(bs$p_resis_trab))
mean(bs$p_resis_trab)


par(mfrow = c(1, 1))
hist(m2t$residuals, freq = FALSE, xlab = "Residuos", ylab = "Frecuencia",
     main = "Normalidad de residuos", breaks = 18)
curve(dnorm(x, mean = mean(m2t$residuals), sd = sd(mm2t$residuals)), 
      add = TRUE, col = "red")

plot(m2t, 4)
par(mfrow=c(2,2))

plot(m2t, which=1:4)




bs$p_estudio <- predict.lm(object=m2e, newdata = bs)

plot(bs$p_estudio, bs$ViajesEstudio)
hist(bs$p_estudio, breaks = 100)
bs$p_resis_est <- bs$ViajesEstudio - bs$p_estudio
bs$p_resis_est2 <- (bs$ViajesEstudio - bs$p_estudio)^2
sqrt(mean(bs$p_resis_est2))


hist(bs$p_resis_est, breaks = 50)
mean(abs(bs$p_resis_est))
mean(bs$p_resis_est)


bs$p_otro <- predict.lm(object=m2o, newdata = bs)

plot(bs$p_otro, bs$ViajesOtro)
hist(bs$p_otro, breaks = 100)
bs$p_resis_otr <- bs$ViajesOtro - bs$p_otro
bs$p_resis_otr2 <- (bs$ViajesOtro - bs$p_otro)^2
sqrt(mean(bs$p_resis_otr2))


hist(bs$p_resis_otr, breaks = 50)
mean(abs(bs$p_resis_otr))

mean(bs$p_resis_otr)


sum(bs$Desempleados)
sum(bs$Estudiantes)
sum(bs$Personas)
sum(bs$Trabajadores)
sum(bs$Jubilados)
sum(bs$ViajesTrabajo)

sum(bs$Desempleados)/sum(bs$Personas)
sum(bs$Estudiantes)/sum(bs$Personas)

sum(bs$ln_BicicletasAdulto)
sum(bs$ln_BicicletasNino)
sum(bs$ln_PasesEscolares)
sum(bs$j_est)
sum(bs$adj_est)
sum(bs$Discapacidad)
sum(bs$AdultoMayores)
sum(bs$Adultos)
sum(bs$AdultoJovenes)
sum(bs$Trabajadores)
sum(bs$Estudiantes)
sum(bs$Jubilados)
sum(bs$DuenosdeCasa)
sum(bs$Desempleados)
sum(bs$Otros)



# Para calcular el Error de estimación por propagación de erroes en viaje de Estudio.

as_e <- as %>% select (ln_BicicletasAdulto,ln_BicicletasNino,ln_PasesEscolares,
                         j_est,ad_est,Discapacidad,AdultoMayores,Adultos,
                         AdultoJovenes,Trabajadores,
                         Estudiantes,Jubilados,DuenosdeCasa,Desempleados,Otros)


mean(as$ViajesOtro)
lista <- as_e %>% summarise(ln_BicicletasAdulto=mean(ln_BicicletasAdulto),
                            ln_BicicletasNino=mean(ln_BicicletasNino),
                            ln_PasesEscolares=mean(ln_PasesEscolares),
                            j_est=mean(j_est),
                            ad_est=mean(ad_est),
                            Discapacidad=mean(Discapacidad),
                            AdultoMayores=mean(AdultoMayores),
                            Adultos=mean(Adultos),
                            AdultoJovenes=mean(AdultoJovenes),
                            Trabajadores=mean(Trabajadores),
                            Estudiantes=mean(Estudiantes),
                            Jubilados=mean(Jubilados),
                            DuenosdeCasa=mean(DuenosdeCasa),
                            Desempleados=mean(Desempleados),
                            Otros=mean(Otros))

counter <- 0
for (i in 1:ncol(as_e)) {
  counter <- counter + (summary(m2e)$coefficients[i+1, 1]^2)*((0.08*lista[[i]])^2)
}

for(i in 1:ncol(as_e)) {
  for (j in 1:ncol(as_e)) {
   if (i!=j) {
    counter <- counter +
      summary(m2e)$coefficients[i+1, 1]*summary(m2e)$coefficients[j+1, 1]*
      (lista[[i]]*0.08)*(lista[[j]]*0.08)*cor(as_e)[i,j]
   } 
  }
  
}

counter <- sqrt(counter)
counter


# aquí va el cálculo para Trabajo

as_t <- as %>% select (ln_vehiculo,IngresoHogar,ad_tr,Discapacidad,AdultoMayores,
                         Adultos,AdultoJovenes,Jovenes,Niños,Trabajadores,
                         Estudiantes,Jubilados,DuenosdeCasa,Desempleados,Otros)


lista_2 <- as_t %>% summarise(ln_vehiculo=mean(ln_vehiculo,),
                            IngresoHogar=mean(IngresoHogar),
                            ad_tr=mean(ad_tr),
                            Discapacidad=mean(Discapacidad),
                            AdultoMayores=mean(AdultoMayores),
                            Adultos=mean(Adultos),
                            AdultoJovenes=mean(AdultoJovenes),
                            Jovenes=mean(Jovenes),
                            Niños=mean(Niños),
                            Trabajadores=mean(Trabajadores),
                            Estudiantes=mean(Estudiantes),
                            Jubilados=mean(Jubilados),
                            DuenosdeCasa=mean(DuenosdeCasa),
                            Desempleados=mean(Desempleados),
                            Otros=mean(Otros))



counter_t <- 0
for (i in 1:ncol(as_t)) {
  counter_t <- counter_t + (summary(m2t)$coefficients[i+1, 1]^2)*((0.08*lista_2[[i]])^2)
}
counter_t


for(i in 1:ncol(as_t)) {
  for (j in 1:ncol(as_t)) {
    if (i!=j) {
      counter_t <- counter_t +
        summary(m2t)$coefficients[i+1, 1]*summary(m2t)$coefficients[j+1, 1]*
        (lista_2[[i]]*0.08)*(lista_2[[j]]*0.08)*cor(as_t)[i,j]
    } 
  }
  
}

counter_t <- sqrt(counter_t)
counter_t

# aquí va el cálculo para Otros

as_o <- as %>% select (ln_vehiculo,IngresoHogar,ln_BicicletasNino,ln_PasesEscolares,
                         adm_est,ad_tr,AdultoMayores,Adultos,
                         Estudiantes,Jubilados,DuenosdeCasa,Desempleados,Otros)


lista_3 <- as_o %>% summarise(ln_vehiculo=mean(ln_vehiculo),
                              IngresoHogar=mean(IngresoHogar),
                              ln_BicicletasNino=mean(ln_BicicletasNino),
                              ln_PasesEscolares=mean(ln_PasesEscolares),
                              adm_est=mean(adm_est),
                              ad_tr=mean(ad_tr),
                              AdultoMayores=mean(AdultoMayores),
                              Adultos=mean(Adultos),
                              Estudiantes=mean(Estudiantes),
                              Jubilados=mean(Jubilados),
                              DuenosdeCasa=mean(DuenosdeCasa),
                              Desempleados=mean(Desempleados),
                              Otros=mean(Otros))



counter_o <- 0
for (i in 1:ncol(as_o)) {
  counter_o <- counter_o + (summary(m2o)$coefficients[i+1, 1]^2)*((0.08*lista_3[[i]])^2)
}
counter_o


for(i in 1:ncol(as_o)) {
  for (j in 1:ncol(as_o)) {
    if (i!=j) {
      counter_o <- counter_o +
        summary(m2o)$coefficients[i+1, 1]*summary(m2o)$coefficients[j+1, 1]*
        lista_3[[i]]*0.08*lista_3[[j]]*0.08*cor(as_o)[i,j]
    } 
  }
  
}

counter_o <- sqrt(counter_o)
counter_o


##### Ahora mediré cuál variable conviene medir mejor, para estudio:

counter_m <- c(1:ncol(as_e))
for (i in 1:ncol(as_e)) {
  counter_m[i] <- (summary(m2e)$coefficients[i+1, 1]^2)*((0.08*lista[[i]])/counter)
}
counter_m


for(i in 1:ncol(as_e)) {
  for (j in 1:ncol(as_e)) {
    if (i != j) {
      counter_m[i] <- counter_m[i] + (summary(m2e)$coefficients[i+1, 1]*summary(m2e)$coefficients[j+1, 1]*
        ((0.08*lista[[j]])/counter)*cor(as_e)[i,j])
    }
  }
}


counter_m


##### Ahora mediré cuál variable conviene medir mejor, para Trabajo:
counter_mt <- c(1:ncol(as_t))
for (i in 1:ncol(as_t)) {
  counter_mt[i] <- (summary(m2t)$coefficients[i+1, 1]^2)*((0.08*lista_2[[i]])/counter_t)
}


for(i in 1:ncol(as_t)) {
  for (j in 1:ncol(as_t)) {
    if (i!=j) {
      counter_mt[i] <- summary(m2t)$coefficients[i+1, 1]*summary(m2t)$coefficients[j+1, 1]*
        ((0.08*lista_2[[j]])/counter_t)*cor(as_t)[i,j]
    }
  }
}
counter_mt

##### Ahora mediré cuál variable conviene medir mejor, para Otros:
counter_mo <- c(1:ncol(as_o))
for (i in 1:ncol(as_o)) {
  counter_mo[i] <- (summary(m2o)$coefficients[i+1, 1]^2)*((0.08*lista_3[[i]])/counter_o)
}


for(i in 1:ncol(as_o)) {
  for (j in 1:ncol(as_o)) {
    if (i!=j) {
      counter_mo[i] <- summary(m2o)$coefficients[i+1, 1]*summary(m2o)$coefficients[j+1, 1]*
        ((0.08*lista_3[[j]])/counter_o)*cor(as_o)[i,j]
    }
  }
}
counter_mo

