Ente#function to calculate characteristics of life table and to compare these to the average
LH.pars=function(l,m) {
	#pars for each of the life histories
	t.end=dim(l)[1]
	lx.mx=l*m
	R0=colSums(lx.mx)
	gen.time=colSums(l*m*(seq(t.end)-1))/R0
	av.age=colSums(l*(seq(t.end)-1))
	#overall mean life history
	l.mean=apply(l,1,mean)
	m.mean=apply(m,1,mean)
	R0.mean=sum(l.mean*m.mean)
	gen.time.mean=sum(l.mean*m.mean*(seq(t.end)-1))/R0.mean
	av.age.mean=sum(l.mean*(seq(t.end)-1))
	alloc1=gen.time/av.age.mean
	#dividing R0 in before and after mean age
	R0.before.mean.age=apply(lx.mx,2,function(lx.mx)sum(lx.mx[seq(t.end)<(av.age.mean+1)]) )
	R0.after.mean.age=apply(lx.mx,2,function(lx.mx)sum(lx.mx[seq(t.end)>=(av.age.mean+1)]) )
	alloc2=R0.before.mean.age/R0 #proportion of R0 allocated to life before the average age in the mean life history
	return(list(R0=R0,R0.mean=R0.mean, gen.time=gen.time, gen.time.mean=gen.time.mean,av.age=av.age,av.age.mean=av.age.mean, R0.before.mean.age=R0.before.mean.age, R0.after.mean.age=R0.after.mean.age,alloc1=alloc1,alloc2=alloc2))
}
#
# life tables as examples
#1. 2 tables with fixed m but different adult mortality
p0=c(0.2,0.2)
p1=c(0.8,0.6)
mx=c(2,2)
n0=1000
t.end=20
n=m=l=p=matrix(NA,t.end,2)
n[1,]=c(n0,n0)
m[1,]=c(0,0)
l[1,]=c(1,1)
p=matrix(p1,t.end,2,byrow=T)
p[1,]=p0
for (a in 2:t.end) {
	n[a,]=n[(a-1),]*p[(a-1),]
	m[a,]=mx
	l[a,]=l[(a-1),]*p[(a-1),]
}
n
m
l
lx.mx=l*m

pars=LH.pars(l,m) # the slow and fast life table differentiate on the alloc2 parameter as they should
#plots of this
plot(lx.mx[2:t.end,1],col="blue",pch=19,cex=1.5,xlab="Age",ylab="lx.mx",ylim=c(0,0.4))
par(new=T)
plot(lx.mx[2:t.end,2],col="red",pch=19,cex=1.5,xlab="Age",ylab="lx.mx",ylim=c(0,0.4))
lines(c(pars$av.age.mean,pars$av.age.mean),c(0,0.4),col="black") #line drawn at avergae age of mean life history

## life tables as examples
#2.  tables with fixed m but different adult mortality drawn from a random distribution
n.tables=30
p0=plogis(qlogis(0.4)+rnorm(n.tables,0,.3))
p1=plogis(qlogis(0.5)+rnorm(n.tables,0,.3))
mx=rep(1,n.tables)
t.end=20
n=m=l=p=matrix(NA,t.end,n.tables)
m[1,]=rep(0,n.tables)
l[1,]=rep(1,n.tables)
p=matrix(p1,t.end,n.tables,byrow=T)
p[1,]=p0
for (a in 2:t.end) {
	m[a,]=mx
	l[a,]=l[(a-1),]*p[(a-1),]
}

pars2=LH.pars(l,m) # the slow and fast life table differentiate on the alloc2 parameter as they should

# Draw allocation and acquisition plot

x.max=y.max=1.1*max(pars2$R0)
plot(y=pars2$R0.after.mean.age,pars2$R0.before.mean.age,xlim=c(0,x.max),ylim=c(0,y.max),pch=19,col="blue", xlab="Investment in early repro",ylab="investment in late repro")
for (x in 1:length(pars2$R0)) {
	lines(c(pars2$R0[x],0),c(0,pars2$R0[x])) #LRS trade-off lines
	if (pars2$alloc2[x]<0.5){
		y.top=y.max; x.top=pars2$R0.before.mean.age[x]*x.max/pars2$R0.after.mean.age[x]
	} else {
		y.top=pars2$R0.after.mean.age[x]*y.max/pars2$R0.before.mean.age[x]; x.top=x.max
	}
	lines(c(0,x.top),c(0,y.top),col="red") #LRS trade-off lines
}

plot(y=pars2$gen.time, pars2$alloc2, pch=19, cex=1.1, col="blue",xlab="Allocation", ylab="Generation time")
