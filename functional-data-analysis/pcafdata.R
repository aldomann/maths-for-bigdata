require(fda);require(fda.usc)  # perhaps you need to install them first
#source("/Users/acabana/Documents/aa-Trabajos/FDA:SVM/fdata2pc2.R")

data(poblenou);attach(poblenou)  # these data are already in the library fda.usc
?poblenou ### what's in here?
class(nox)
dd <- as.integer(poblenou$df$day.week)
##### split curves into working days and non-working
working = poblenou$nox[poblenou$df$day.festive == 0 & dd < 6] #working days
iw=poblenou$df$day.festive == 0 & dd < 6
iw=which(iw=="TRUE")# index of curves corresponding to working days
inw=poblenou$df$day.festive == 1 | dd > 5
inw=which(inw=="TRUE") # non working
nonworking = poblenou$nox[poblenou$df$day.festive == 1 | dd > 5] #nonworking curves

par(mfrow = c(1, 2)) #2 plots in 1 frame
plot(working, main="NOx levels in Poblenou, working days")
plot(nonworking, main="NOx levels in Poblenou, non-working days")
# Another fancy plot
depth.mode(working, draw = T)
depth.mode(nonworking, draw = T)
			
			################  A Spline basis for the data
argvals=0:23
rangeval=c(0,23)
base=create.bspline.basis(nbasis=10)
data.fd=Data2fd(y=working$data,basisobj=base)
plot.fd(data.fd)
plot(working, main="NOx levels in Poblenou, working days");lines(nonworking)
plot(nonworking, main="NOx levels in Poblenou, non-working days")
################


##### Considering the datas as functions: 
# fdata El arg es una matriz, las filas son las curvas y las col los puntos donde se evalua (horas)
w=working$data
w.fdata=fdata(w,argvals=1:dim(w)[2])
nw.fdata=fdata(nonworking$data,argvals=1:dim(nonworking$data)[2])
plot(func.mean(w.fdata))  ##### media de las curvas

########## PRINCIPAL COMPONENTS
pcworking<-fdata2pc(w.fdata, ncomp = 4,norm = TRUE,lambda=0);summary(pcworking)
pcnonworking<-fdata2pc(nw.fdata, ncomp = 4,norm = TRUE,lambda=0);summary(pcnonworking)
cpnw=pcnonworking$rotation # aqui estan las 4 curvas principales
par(mfrow=c(1,1))
plot(pcworking$rotation, main="Principal Components working-days")
plot(pcnonworking$rotation, main="Principal Components working-days")
pcworking<-fdata2pc(working, ncomp = 4,norm = TRUE,lambda=0);summary(pcworking)
pcnonworking<-fdata2pc(nonworking, ncomp = 4,norm = TRUE,lambda=0);summary(pcnonworking)



