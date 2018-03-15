# Ejemplo Bacterias 
"rayosXbac" <-
structure(list(tiempo = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 
12, 13, 14, 15), bacteria = c(355, 211, 197, 166, 142, 106, 104, 
60, 56, 38, 36, 32, 21, 19, 15)), .Names = c("tiempo", "bacteria"
), row.names = c("1", "2", "3", "4", "5", "6", "7", "8", "9", 
"10", "11", "12", "13", "14", "15"), class = "data.frame")



# Primer modelo
# Ajustemos un modelo de la forma bacteria=beta0+beta1*tiempo
modelo.1 <- lm(bacteria~tiempo, data=rayosXbac); modelo.1
summary.modelo.1 <- summary(modelo.1); summary.modelo.1
Call:
	lm(formula = bacteria ~ tiempo, data = rayosXbac)

Residuals:
	Min      1Q  Median      3Q     Max 
-43.867 -23.599  -9.652  10.223 114.883 

Coefficients:
	Estimate Std. Error t value Pr(>|t|)    
(Intercept)   259.58      22.73  11.420 3.78e-08 ***
	tiempo        -19.46       2.50  -7.786 3.01e-06 ***
	---
	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 41.83 on 13 degrees of freedom
Multiple R-squared: 0.8234,	Adjusted R-squared: 0.8098 
F-statistic: 60.62 on 1 and 13 DF,  p-value: 3.006e-06 

## Los valores del R2 y los p/valores son razonables, pero...
## si miramos los gr\'aficos de residuos vemos que el modelo
## no se ajusta bien
par(mfrow=c(2,2))  # para que las cuatro gr\'aficas queden juntas
plot(modelo.1)
# Segundo modelo
modelo.2 <- lm(log(bacteria)~tiempo, data=rayosXbac); modelo.2
summary.modelo.2 <- summary(modelo.2); summary.modelo.2
Call:
	lm(formula = log(bacteria) ~ tiempo, data = rayosXbac)

Residuals:
	Min       1Q   Median       3Q      Max 
-0.18445 -0.06190  0.01253  0.05201  0.20021 

Coefficients:
	Estimate Std. Error t value Pr(>|t|)    
(Intercept)  5.973160   0.059778   99.92  < 2e-16 ***
	tiempo      -0.218425   0.006575  -33.22 5.86e-14 ***
	---
	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.11 on 13 degrees of freedom
Multiple R-squared: 0.9884,	Adjusted R-squared: 0.9875 
F-statistic:  1104 on 1 and 13 DF,  p-value: 5.86e-14 

par(mfrow=c(2,2))
plot(modelo.2)

#  Ahora s'i el ajuste es razpnable.


# Los estimadores aparecen en la columna "Estimate" de "Coefficients"
# El estimador de la desviación estándar es el
# Residual standard error

# b)
cuantiles <- qchisq(c(0.95,0.05),modelo.1$df.residual)
se2 <- summary.modelo.1$sigma^2
# Intervalo para la varianza
int.se2 <- (nrow(rayosXbac)-ncol(rayosXbac))*se2/cuantiles
int.se2
# Intervalo para la desviación t??pica
sqrt(int.se2)

# c)
modelo.2.1 <- lm(log(bacteria)~1, data=rayosXbac)
anova(modelo.2.1,modelo.2)


datos <- data.frame(tiempo=8)
pred <- predict(modelo.2,newdata=datos,interval="prediction")
pred
# fit es la predicción puntual y los otros dos valores el
# intervalo para el logaritmo. Para la bacteria hay que tomar
# exponenciales.
exp(pred)

# Diagrama de dispersión
plot(rayosXbac)
# L??nea del primero
abline(modelo.1)
# Valores ajustados del segundo (exponenciales)
points(rayosXbac$tiempo,exp(fitted(modelo.2)),lty=2)
# Evidentemente es preferible el segundo modelo, porque
# se adapta mejor a la forma de la nube de puntos
curve(392.76* exp(-0.22*x),xlim=c(1,15), add=T, lty=2)



################  Calamar
############################################
## Ajuste de un modelo a la hoja de datos ##
##       "calamar"                        ##
##      Contrastes de hip?tesis sobre     ##
##            los coeficientes            ##
############################################

calamar=read.table("http://mat.uab.cat/~acabana/data/calamar.txt",header=T)
attach(calamar)

# Las variables que se midieron corresponden a:
#	y  : peso de los calamares en libras.
# x1     : longitud del rostral (rostral length) en pulgadas.
# x2     : longitud de la aleta (wings) en pulgadas.
# x3     : distancia del rostral a la muesca (notch).
# x4     : distancia de la muesca a la aleta.
# x5     : anchura del calamar en pulgadas.



## Diagrama de dispersi?n ##
############################

# Lo m?s aproximado al diagrama de dispersi?n es realizar 
# diagramas de dispersi?n por parejas: orden plot en el conjunto # de datos

pairs(calamar)

## Ajuste del modelo ##
#######################
#  es indiferente agregar o no el 1, lo pone por default.
calamar.lm <- lm(y~x1+x2+x3+x4+x5, data=calamar)
calamar.lm
# Recordatorio "unclass(calamar.lm)" muestra la lista guardada

attributes(calamar.lm) # nos dice qué información hay en calamar.lm
$names
[1] "coefficients"  "residuals"     "effects"       "rank"         
[5] "fitted.values" "assign"        "qr"            "df.residual"  
[9] "xlevels"       "call"          "terms"         "model"        

$class
[1] "lm"


#  El summary proporciona los p valores de los tests parciales
#  También trae el estad??stico para el contraste de 
#  significación de la regresión  H0: los regresores NO aportan 
#  info. acerca del cambio en la variable independiente.

# La función "summary" realiza los tests parciales y nos da, entre 
# otras informaciones el p-valor de los contrastes PARCIALES.
summary(calamar.lm)

Call:
	lm(formula = peso ~ x1 + x2 + x3 + x4 + x5, data = calamar)

Residuals:
	Min      1Q  Median      3Q     Max 
-1.2610 -0.5373  0.1355  0.5120  0.8611 

Coefficients:
	Estimate Std. Error t value Pr(>|t|)    
(Intercept)  -6.5122     0.9336  -6.976 3.13e-06 ***  # 
	x1            1.9994     2.5733   0.777   0.4485  # \beta_1|los otros
x2           -3.6751     2.7737  -1.325   0.2038  # \beta_2|los otros
x3            2.5245     6.3475   0.398   0.6961    
x4            5.1581     3.6603   1.409   0.1779    
x5           14.4012     4.8560   2.966   0.0091 ** 
	---
	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.7035 on 16 degrees of freedom
Multiple R-Squared: 0.9633,	Adjusted R-squared: 0.9519 
F-statistic: 84.07 on 5 and 16 DF,  p-value: 6.575e-11 

####  Esto nos muestra, por ejemplo, que la variable menos #significativa es x3 
# en presencia de las otras, naturalmente.

# Cómo puede dar negativo el coef de x2????? (ver el plot)
# Hay COLINEALIDAD.  

#  Con ANOVA se tienen las sumas de cuadrados correspondientes
#  a los contrastes SECUENCIALES
# R(\beta_1 | \beta_0)= 199.145
# R(\beta_2 | \beta_1,\beta_0)= 0.127 , etc.

anova(calamar.lm)
Analysis of Variance Table

Response: peso
Df  Sum Sq Mean Sq  F value   Pr(>F)    
x1         1 199.145 199.145 402.4397 9.13e-13 ***
	x2         1   0.127   0.127   0.2560 0.619804    
x3         1   4.120   4.120   8.3249 0.010765 *  
	x4         1   0.263   0.263   0.5325 0.476114    
x5         1   4.352   4.352   8.7951 0.009109 ** 
	Residuals 16   7.918   0.495                      
---
	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 


#  Observacion:  la suma de la suma de cuadrados secuenciales
#  es 208.007, es decir la suma de cuadrados debida a la
# regresión SSReg= sum(\hat y - \bar y)^2

anovita=anova(calamar.lm)
unclass(anovita)
sum(anovita[,3])

haty=calamar.lm$fitted.values
bary=sum(peso)/22  
sum((haty-bary)^2) # = SSReg
#[1] 208.0072

sum((peso-bary)^2) # SSTotal
#[1] 215.9248
sum((haty-bary)^2)/sum((peso-bary)^2)  #  Coeficiente R^2 
#[1] 0.963332

## Funciones que extraen más información del ajuste ##
######################################################

# Coeficientes
coef(calamar.lm)
# Valores ajustados
fitted(calamar.lm)
# Residuales y residuales estandarizados
resid(calamar.lm)
rstandard(calamar.lm)
# Matriz de diseño
model.matrix(calamar.lm)
# Datos con los que se realizó el ajuste
model.frame(calamar.lm)

# Como se puede apreciar todas estas funciones se utilizan 
# igual que en regresión simple. 
# Notar como se utiliza el s??mbolo "+" en las
# regresiones a la hora de incluir varias variables.

## Inferencias sobre los coeficientes de la regresión ##
########################################################


resumencalamar = summary(calamar.lm)
resumencalamar
# Recordatorio "unclass(resumencalamar)" muestra la lista guardada

## Función "anova" - UN MODELO ##
#################################

# La función "anova" con un modelo realiza los tests secuenciales
# NO LA TABLA ANOVA!
calamar.anova <- anova(calamar.lm)
calamar.anova


# Recordatorio "unclass(calamar.anova)" muestra la lista guardada


#attributes(calamar.lm) 
## Tabla ANOVA "completa" - Función "anova" para comparar  dos modelos ##
##############################################################
#  H0: \beta_4=\beta_5=0

reducido123=lm(peso~1+x1+x2+x3)
summary(reducido123)

#  Comparación de los dos modelos
anova(reducido123,calamar.lm)


####  Entonces, al nivel 5% rechazamos H_0, por lo tanto es mejor dejar x4 y x5.

###  Saquemos x3, que es aparentemente el menos significativo 
general=calamar.lm
reducido1245=lm(peso~x1+x2+x4+x5)
# una instrucción equivalente es 
#  reducido1245=update(general,~.-x3)
# R calcula menos haciendo ésto que volviendo a hacer lm
anova(reducido1245,general) #####  Aceptamos H0, es decir, vale el modelo reducido



# Ahora la menos significativa de las variables es x1.

reducido245=lm(peso~x2+x4+x5)
anova(reducido245,reducido1245)

# Nuevamente podemos quedarnos con el reducido 245.
# Como antes, el p-valor del test F coincide con el de la t
# para H0: \beta_1=0 en el modelo con x1,x2,x4,x5.  Esto
# ocurre si el modelo reducido TIENE 1 Var. menos que el general.


# La menos significativa de estas variables es x2, qué tal si la sacamos?

reducido45=lm(peso~x4+x5)
summary(reducido45)

# ajusta bien, y aparentemente no sobran coeficientes.

anova(reducido45,reducido245)

Analysis of Variance Table

Model 1: peso ~ x4 + x5
Model 2: peso ~ x2 + x4 + x5
Res.Df    RSS Df Sum of Sq      F Pr(>F)
1     19 8.9793                           
2     18 8.2814  1    0.6978 1.5167 0.2340

# Pues sirve el modelo con x4 y x5, me quedo con ese.


#  Selección automática

library(MASS)
stepAIC(calamar.lm)

#  Observemos que esto nos deja con el mismo modelo que obtuvimos "a mano".


#  otra observación:  Qué hubiera ocurrido si luego de mirar
summary(calamar.lm) 
#decid??amos sacar x1,x2,x3,x4 simult'aneamente?
reducido5= update(calamar.lm,~.-x1-x2-x3-x4)
summary(reducido5)

anova(reducido5,calamar.lm)
# puedo quedarme con el modelo que s?lo tiene x5 PERO...

anova(reducido5,reducido45)

#NO PUEDO QUEDAREM CON SOLO x5 !!!!!!!!
# Qué ocurre?????? que como hay MULTICOLINEALIDAD, 
# las estimaciones en el modelo general no son fiables.
# Sin embargo, x1 s?o explica bastante de la variablidad...

reducido1=lm(peso~x1)
summary(reducido1)



######  Y entonces?????????


# Hay que utilizar el modelo con y sin restricciones
calamar.lm0 <- lm(peso~1, data=calamar)
anova(calamar.lm0, calamar.lm)


## Función "anova" - DOS MODELOS ##
###################################

# Sirve para hacer cualquier contraste sobre combinaciones 
# lineales sobre los parámetros

# H_0 :beta_1 = 0 y beta_2 = 0 vs H_a : beta_1 != 0 ó beta_2 != 0
calamar345 <- lm(peso~x3+x4+x5, data=calamar)
anova(calamar345, calamar.lm)

# H_0 : beta_1 = beta_2 y beta_3 = beta_4 vs
# H_a : beta_1 != beta_2 Û beta_3 != beta_4
otrocalamar <- lm(peso~I(x1+x2)+I(x3+x4)+x5, data=calamar)
anova(otrocalamar, calamar.lm)

## Validación del modelo ##
###########################

# La función "plot" sobre un objeto de la clase "lm" (ajustes de
# regresiones) da los mismos plots de residuales que en regresión simple
x11()
plot(calamar.lm)
# Se puede observar falta de linealidad y que los residuales tienen
# m·s apuntamiento (colas menos pesadas) que la normal

## Predicción en el modelo ##
#############################

# La función "predict" sirve para realizar predicciones, 
# intervalos de
# confianza e intervalos de predicción.
calamar.predict <- predict(calamar.lm, se.fit=TRUE)
calamar.predict
se.prediccion(calamar.lm, calamar.predict)
# Intervalos de confianza
predict(calamar.lm, interval="confidence")
# Intervalos de predicciÛn
predict(calamar.lm, interval="prediction")
# PredicciÛn en un punto concreto
datos.nue <- data.frame(x1=1.2,x2=0.99,x3=0.45,x4=0.64,x5=0.32)
predict(calamar.lm, newdata=datos.nue, interval="prediction")

library(MASS)
stepAIC(calamar.lm)




##################  El accidente del Challenger  28/01/1986

temp= temperatura en Ferenheit en el momento del lanzamiento
y=0 - lanzamiento exitoso, y=1 fallo del =O-ring
temp=c(66,70,69,68,67,72,73,70,57,63,70,78,67,53,67,75,70,81,76,79,75,76,58)
y=c(0,1,0,0,0,0,0,0,1,1,1,0,0,1,0,0,0,0,0,0,1,0,1)
#el d??a del accidente la temp era de 31F (-6C)
plot(temp,y,pch=20,cex=3,col="blue")

logit<-glm(y~temp,family=binomial)
resu=summary(logit)
b0=resu$coefficients[1];b0
b1= resu$coefficients[2];b1
odds=exp((b0+b1*31));odds  #odds de problem con el o-ring  (cu??nto m??s probable era una falla que que no la hubiera)
pfalla=odds/(1+odds);pfalla

######################################LOESS

####### loess (locally weighted least squares)

require(boot)
data(motor)  ## aceleraci?n/tiempos en accidentes (simulados) de moto
names(motor)
original=loess(motor[,2]~motor[,1],span=1/3)
plot(original)
y=predict(original,newdata=1:60)
lines(1:60,y)



########################### Robust regression
install.packages("car")
library(car)
data(Duncan)
pairs(Duncan)
mod.ls= lm(prestige ~ income + education, data=Duncan);summary(mod.ls)
Call:
  lm(formula = prestige ~ income + education, data = Duncan)

Residuals:
  Min      1Q  Median      3Q     Max 
-29.538  -6.417   0.655   6.605  34.641 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) -6.06466    4.27194  -1.420    0.163    
income       0.59873    0.11967   5.003 1.05e-05 ***
  education    0.54583    0.09825   5.555 1.73e-06 ***
  ---
  Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1

Residual standard error: 13.37 on 42 degrees of freedom
Multiple R-squared:  0.8282,  Adjusted R-squared:   0.82 
F-statistic: 101.2 on 2 and 42 DF,  p-value: < 2.2e-16

plot(mod.ls)

#### observar que tanto ministro como conductor tienen leverage alto (cambian los valores de los coeficientes)
#### veamos qu?? ocurre si los sacamos
mod.ls.2=update(mod.ls, subset=-c(6,16)); summary(mod.ls.2)
Call:
  lm(formula = prestige ~ income + education, data = Duncan, subset = -c(6, 
                                                                         16))

Residuals:
  Min      1Q  Median      3Q     Max 
-28.612  -5.898   1.937   5.616  21.551 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) -6.40899    3.65263  -1.755   0.0870 .  
income       0.86740    0.12198   7.111 1.31e-08 ***
  education    0.33224    0.09875   3.364   0.0017 ** 
  ---
  Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1

Residual standard error: 11.42 on 40 degrees of freedom
Multiple R-squared:  0.876,  Adjusted R-squared:  0.8698 
F-statistic: 141.3 on 2 and 40 DF,  p-value: < 2.2e-16

### En efecto, las estimacones cambian mucho
### alternativas:  regresi??n robusta o bootstrap!

library(MASS)
mod.huber <- rlm(prestige ~ income + education, data=Duncan)
summary(mod.huber)
Call: rlm(formula = prestige ~ income + education, data = Duncan)
Residuals:
  Min      1Q  Median      3Q     Max 
-30.120  -6.889   1.291   4.592  38.603 

Coefficients:
  Value   Std. Error t value
(Intercept) -7.1107  3.8813    -1.8320
income       0.7014  0.1087     6.4516
education    0.4854  0.0893     5.4380

Residual standard error: 9.892 on 42 degrees of freedom

#### los coeficientes est??n entre los originales y los que se obten??an sacando los
### 2 datos influyentes

plot(mod.huber$w, ylab="Huber Weight")
identify(1:45, mod.huber$w, rownames(Duncan))


##############  random effects

library(nlme)
rail=data(Rail)
plot(Rail, aspect=1) #Travel time in nanoseconds for ultrasonic
# head-waves in a sample of six railroad rails. 
#The times shown are the result of subtracting 36,100
# nanoseconds from the original observation.
fixed=lm(travel ~ Rail - 1, data = Rail)
summary(fixed)
Call:
  lm(formula = travel ~ Rail - 1, data = Rail)

Residuals:
  Min      1Q  Median      3Q     Max 
-6.6667 -1.0000  0.1667  1.0000  6.3333 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
Rail2   31.667      2.321   13.64 1.15e-08 ***
  Rail5   50.000      2.321   21.54 5.86e-11 ***
  Rail1   54.000      2.321   23.26 2.37e-11 ***
  Rail6   82.667      2.321   35.61 1.54e-13 ***
  Rail3   84.667      2.321   36.47 1.16e-13 ***
  Rail4   96.000      2.321   41.35 2.59e-14 ***
  ---
  Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1 

Residual standard error: 4.021 on 12 degrees of freedom
Multiple R-squared: 0.9978,  Adjusted R-squared: 0.9967 
F-statistic: 916.6 on 6 and 12 DF,  p-value: 2.971e-15 

fixed2=lm(travel ~ Rail, data = Rail)
summary(fixed2)

Call:
  lm(formula = travel ~ Rail, data = Rail)

Residuals:
  Min      1Q  Median      3Q     Max 
-6.6667 -1.0000  0.1667  1.0000  6.3333 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  66.5000     0.9477  70.169  < 2e-16 ***
  Rail.L       54.3032     2.3214  23.392 2.22e-11 ***
  Rail.Q       -4.6917     2.3214  -2.021 0.066161 .  
Rail.C       -2.6584     2.3214  -1.145 0.274458    
Rail^4       -0.5669     2.3214  -0.244 0.811181    
Rail^5       11.1919     2.3214   4.821 0.000418 ***
  ---
  Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1 

Residual standard error: 4.021 on 12 degrees of freedom
Multiple R-squared: 0.9796,  Adjusted R-squared: 0.9711 
F-statistic: 115.2 on 5 and 12 DF,  p-value: 1.033e-09 


Rail.lme <- lme(travel ~ 1, data = Rail, random = ~ 1 | Rail)

# respuesta: travel  y hay un s??lo efecto fijo (intercept)The third argument indicates
# random ~ 1 dice que para cada grupo hay un unico efecto aleatorio y los grupos
# vienen dados por la variable Rail.

summary(Rail.lme)
Linear mixed-effects model fit by REML
Data: Rail 
AIC      BIC   logLik
128.177 130.6766 -61.0885

Random effects:
  Formula: ~1 | Rail
(Intercept) Residual
StdDev:    24.80547 4.020779      ######## s^2_b  y s^2

Fixed effects: travel ~ 1 
Value Std.Error DF  t-value p-value
(Intercept)  66.5  10.17104 12 6.538173       0

Standardized Within-Group Residuals:
  Min          Q1         Med          Q3         Max 
-1.61882658 -0.28217671  0.03569328  0.21955784  1.61437744 

Number of Observations: 18
Number of Groups: 6
##################  MAXIMA VEROSIMILITUD
RailML.lme <- lme(travel ~ 1, data = Rail, random = ~ 1 | Rail, method="ML")
summary(RailML.lme)
Linear mixed-effects model fit by maximum likelihood
Data: Rail 
AIC      BIC    logLik
134.56 137.2312 -64.28002

Random effects:
  Formula: ~1 | Rail
(Intercept) Residual
StdDev:    22.62435 4.020779

Fixed effects: travel ~ 1 
Value Std.Error DF  t-value p-value
(Intercept)  66.5  9.554026 12 6.960417       0

Standardized Within-Group Residuals:
  Min          Q1         Med          Q3         Max 
-1.61098123 -0.28887045  0.03454166  0.21372780  1.62222279 

Number of Observations: 18
Number of Groups: 6 


intervals(Rail.lme) # I conf para los estimadores de alfa, sigma b y sigma
Approximate 95% confidence intervals

Fixed effects:
  lower est.    upper
(Intercept) 44.33921 66.5 88.66079
attr(,"label")
[1] "Fixed effects:"

Random Effects:
  Level: Rail 
lower     est.    upper
sd((Intercept)) 13.27434 24.80547 46.35341

Within-group standard error:
  lower     est.    upper 
2.695007 4.020779 5.998747 

anova(Rail.lme) ## sigma b no es cero...


#####################  EJEMPLO AFEITADORAS

################## Ejemplo con 1 efecto fijo y uno aleatorio

data(ergo.Stool)
plot(ergoStool,outer=Subject, aspect=1)
plot.design(ergoStool) #medias por tipo e individuo

options(contrasts=c("contr.helmert","contr.poly"))
contrasts(Type)
[,1] [,2] [,3]
T1   -1   -1   -1
T2    1   -1   -1
T3    0    2   -1
T4    0    0    3

analisis=lme(effort ~ Type,data=ergoStool, random= ~1 | Subject, method="ML")
anova(analisis)
numDF denDF  F-value p-value
(Intercept)     1    24 455.0075  <.0001
Type            3    24  22.3556  <.0001        #  se rechaza que las beta 2 3 y 4 sean 0
summary(analisis)

Linear mixed-effects model fit by maximum likelihood
Data: ergoStool 
AIC      BIC    logLik
134.1444 143.6456 -61.07222

Random effects:
  Formula: ~1 | Subject
(Intercept) Residual
StdDev:     1.25626 1.037368   #  sigma_b y sigma

Fixed effects: effort ~ Type 
Value Std.Error DF   t-value p-value
(Intercept) 10.250000 0.4805234 24 21.330905  0.0000
Type1        1.944444 0.2593419 24  7.497610  0.0000
Type2        0.092593 0.1497311 24  0.618392  0.5421
Type3       -0.342593 0.1058759 24 -3.235794  0.0035
Correlation: 
  (Intr) Type1 Type2
Type1 0                 
Type2 0      0          
Type3 0      0     0    

Standardized Within-Group Residuals:
  Min         Q1        Med         Q3        Max 
-1.9113133 -0.6821805  0.0613392  0.7435197  1.7303828 

Number of Observations: 36
Number of Groups: 9 

intervals(analisis)
Approximate 95% confidence intervals

Fixed effects:
  lower        est.      upper
(Intercept)  9.3149676 10.25000000 11.1850324
Type1        1.4398008  1.94444444  2.4490881
Type2       -0.1987635  0.09259259  0.3839487
Type3       -0.5486125 -0.34259259 -0.1365727
attr(,"label")
[1] "Fixed effects:"

Random Effects:
  Level: Subject 
lower    est.    upper
sd((Intercept)) 0.7298653 1.25626 2.162302

Within-group standard error:
  lower      est.     upper 
0.7938016 1.0373677 1.3556683 1527/15


#############  Maquinitas

> m1=c(98,97,99,96)
> m2=c(91,90,93,92)
> m3=c(96,95,97,95)
> m4=c(95,96,99,98)
> maquinitas=rbind(m1,m2,m3,m4)
> maquinitas
[,1] [,2] [,3] [,4]
m1   98   97   99   96
m2   91   90   93   92
m3   96   95   97   95
m4   95   96   99   98
> sum(maquinitas)
[1] 1527

sum(maquinitas^2)
[1] 145845
> sum(c(sum(m1)^2,sum(m2)^2,sum(m3)^2,sum(m4)^2))/4
[1] 145822.2
> (145845-145822.2)/(16-4)
[1] 1.9   ###########  Estimador de sigma^2

SCalpha=sum(maquinitas)^2
SCalpha
plot(ergoStool, aspect=1)
attach(ergoStool)
plot(ergoStool,outer=Subject, aspect=1)
plot.design(ergoStool)
options(contrasts=c("contr.helmert","contr.poly"))


data(ergoStool)
plot(ergoStool,outer=Subject, aspect=1)
plot.design(ergoStool) #medias por tipo e individuo
options(contrasts=c("contr.helmert","contr.poly"))

contrasts(Type)
[,1] [,2] [,3]
T1   -1   -1   -1
T2    1   -1   -1
T3    0    2   -1
T4    0    0    3
analisis=lme(effort ~ Type,data=ergoStool, random= ~1 | Subject)
summary(analisis)
Linear mixed-effects model fit by REML
Data: ergoStool 
AIC      BIC    logLik
139.4869 148.2813 -63.74345

Random effects:
  Formula: ~1 | Subject
(Intercept) Residual
StdDev:    1.332465 1.100295

Fixed effects: effort ~ Type 
Value Std.Error DF   t-value p-value
(Intercept) 10.250000 0.4805234 24 21.330905  0.0000
Type1        1.944444 0.2593419 24  7.497610  0.0000
Type2        0.092593 0.1497311 24  0.618392  0.5421
Type3       -0.342593 0.1058759 24 -3.235794  0.0035
Correlation: 
  (Intr) Type1 Type2
Type1 0                 
Type2 0      0          
Type3 0      0     0    

Standardized Within-Group Residuals:
  Min          Q1         Med          Q3         Max 
-1.80200345 -0.64316591  0.05783115  0.70099706  1.63142054 

Number of Observations: 36
Number of Groups: 9 
analisis=lme(effort ~ Type,data=ergoStool, random= ~1 | Subject, method="ML")
summary(analisis)
Linear mixed-effects model fit by maximum likelihood
Data: ergoStool 
AIC      BIC    logLik
134.1444 143.6456 -61.07222

Random effects:
  Formula: ~1 | Subject
(Intercept) Residual
StdDev:     1.25626 1.037368

Fixed effects: effort ~ Type 
Value Std.Error DF   t-value p-value
(Intercept) 10.250000 0.4805234 24 21.330905  0.0000
Type1        1.944444 0.2593419 24  7.497610  0.0000
Type2        0.092593 0.1497311 24  0.618392  0.5421
Type3       -0.342593 0.1058759 24 -3.235794  0.0035
Correlation: 
  (Intr) Type1 Type2
Type1 0                 
Type2 0      0          
Type3 0      0     0    

Standardized Within-Group Residuals:
  Min         Q1        Med         Q3        Max 
-1.9113133 -0.6821805  0.0613392  0.7435197  1.7303828 

Number of Observations: 36
Number of Groups: 9 
> anova(analisis)
numDF denDF  F-value p-value
(Intercept)     1    24 455.0075  <.0001
Type            3    24  22.3556  <.0001
> plot(Rail, aspect=1) #Travel time in nanoseconds for ultrasonic
> intervals(analisis)
Approximate 95% confidence intervals

Fixed effects:
  lower        est.      upper
(Intercept)  9.3149676 10.25000000 11.1850324
Type1        1.4398008  1.94444444  2.4490881
Type2       -0.1987635  0.09259259  0.3839487
Type3       -0.5486125 -0.34259259 -0.1365727
attr(,"label")
[1] "Fixed effects:"

Random Effects:
  Level: Subject 
lower    est.    upper
sd((Intercept)) 0.7298653 1.25626 2.162302

Within-group standard error:
  lower      est.     upper 
0.7938016 1.0373677 1.3556683 
> 1527/15
[1] 101.8

#########################  Maquinitas
Una f??brica  utiliza una gran cantidad de m??quinas en su producci??n. Se desea que las
m??quinas sean homog??neas para producir objetos de la misma calidad. Para ver si hay variaciones significativas entre las 
m??quinas, se eligen 4 al azar y se mide el % de cierto componente en el producto final. El 
experimento se hace eligiendo mas m??quinas al azar.

> m1=c(98,97,99,96)
> m2=c(91,90,93,92)
> m3=c(96,95,97,95)
> m4=c(95,96,99,98)
> maquinitas=rbind(m1,m2,m3,m4)
> maquinitas
[,1] [,2] [,3] [,4]
m1   98   97   99   96
m2   91   90   93   92
m3   96   95   97   95
m4   95   96   99   98
> sum(maquinitas)
[1] 1527
> sum(maquinitas2)/16
Error: object 'maquinitas2' not found
> sum(maquinitas^2)/16
[1] 9115.312
> sum(maquinitas-mean(maquinitas))^2)/16
Error: unexpected ')' in "sum(maquinitas-mean(maquinitas))^2)"
> maquinitas-mean(maquinitas)
[,1]    [,2]    [,3]    [,4]
m1  2.5625  1.5625  3.5625  0.5625
m2 -4.4375 -5.4375 -2.4375 -3.4375
m3  0.5625 -0.4375  1.5625 -0.4375
m4 -0.4375  0.5625  3.5625  2.5625
> sum((maquinitas-mean(maquinitas))2)
Error: unexpected numeric constant in "sum((maquinitas-mean(maquinitas))2"
> sum((maquinitas-mean(maquinitas))^2)
[1] 111.9375

sum(maquinitas^2)
[1] 145845
sum(maquinitas^2)-sum(maquinitas)^2
[1] -2185884
sum(c(sum(m1)^2,sum(m2)^2,sum(m3)^2,sum(m4)^2))/4
[1] 145822.2
sigma2=(145845-145822.2)/(16-4)
[1] 1.9
SCalpha=sum(maquinitas)^2/16
SCalpha
[1] 145733.1
SCmod=sum(c(sum(m1)^2,sum(m2)^2,sum(m3)^2,sum(m4)^2))/4 
[1] 145822.2

SCred=(SCmod-SCalpha)/(4-1)
SCred
[1] 29.72917

sigma2b=(SCred-sigma2)*(4/16);sigma2b
[1] 6.957292

La mayor parte de la variabilidad se debe a diferencias entre las m??quinas 
pues sigma2b es bastante mayor que sigma^2

Fobs=SCred/sigma2;Fobs
[1] 15.64693

Fcrit=qf(0.95,3,12);Fcrit
[1] 3.490295

###################  GAM
data=read.table("http://mat.uab.cat/~acabana/data/fossil.txt",header=T)
library(mgcv)
data=read.table("http://mat.uab.cat/~acabana/data/fossil.txt",header=T)
x=data$age
y=data$strontium
x=x[order(x)];y=y[order(x)]
plot(x,y,type="l")
plot(x,y)
#########
model <- gam(y~s(x),type="response")
plot(x,y)
lines(x,predict(model),col=2)

muhat <- predict(model,type="response")
e <- y - muhat  ### residuos
e <- e - mean(e) ### para que tengan media 0

n <- length(x)
B <- 500
yboot <- matrix(nrow=n,ncol=B)
for(i in 1:B){yboot[,i] <- muhat + sample(e,n,replace=TRUE)}

fit_gam <- function(y) {predict.gam(gam(y~s(x)),type="response")}
muhatboot <- apply(yboot,2,fit_gam) # matriz (columna para cada boot)

plot(x,y)
matlines(x,muhatboot,col="grey")
lines(x,muhat,col="red")
points(x,y)


quantiles <- function(x,probs=c(0.025,0.975)){quantile(x,probs)}
ic <- apply(muhatboot,1,quantiles)

lower <- ic[1,]; upper <- ic[2,]
plot(x,y,type="n")
matlines(x,muhatboot,col="white")
lines(x,muhat,col=1)
points(x,y,pch=20,col="grey")
lines(x,lower,col=2)
lines(x,upper,col=2)

##############################################################