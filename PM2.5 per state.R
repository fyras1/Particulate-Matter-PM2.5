f99<-read.table("RD_501_88101_1999-0.txt", comment.char="#" , header=FALSE , sep='|', na.strings="")
f12<-read.table("RD_501_88101_2012-0.txt", comment.char="#" , header=FALSE , sep='|', na.strings="")

nm<-readLines("RD_501_88101_1999-0.txt",1)
nm<-strsplit(nm,'|',fixed=TRUE)
nm<-make.names(nm[[1]])
names(f99)<-nm
names(f12)<-nm

x0<-f99$Sample.Value
x1<-f12$Sample.Value

boxplot(log10(x0),log10(x1))

date<-as.Date(as.character(f99$Date) , "%Y%m%d")
f99$Date<-date
date<-as.Date(as.character(f12$Date) , "%Y%m%d")
f12$Date<-date

 site0<-unique(subset(f99,State.Code==36 , c(County.Code,Site.ID)))
 site1<-unique(subset(f12,State.Code==36 , c(County.Code,Site.ID)))
 inter<-intersect(site0,site1)
 
 f99ny<-subset(f99, State.Code==36)
 f12ny<-subset(f12, State.Code==36)
 
for(i in 1:nrow(inter)){
  inter[i,"obsrv99"]<-nrow(subset(f99ny , County.Code==inter[i,1] & Site.ID==inter[i,2]))
  inter[i,"obsrv12"]<-nrow(subset(f12ny , County.Code==inter[i,1] & Site.ID==inter[i,2]))
  
}
 
 mtr99<-subset(f99ny , County.Code==63 & Site.ID==2008)
 mtr12<-subset(f12ny , County.Code==63 & Site.ID==2008)

 rng<-range(mtr99$Sample.Value , mtr12$Sample.Value , na.rm=TRUE)
 par(mfrow=c(1,2),mar=c(4,4,1,1))
 with(mtr99 , plot(Date,Sample.Value , pch=20 , ylim=rng))
 abline(h=median(mtr99$Sample.Value , na.rm=TRUE))
 with(mtr12 , plot(Date,Sample.Value , pch=20 , ylim=rng))
 abline(h=median(mtr12$Sample.Value , na.rm=TRUE))
 
ta99<-with(f99, tapply(Sample.Value, State.Code, mean , na.rm=T))
df99<-data.frame(state=names(ta99), mean=ta99)

ta12<-with(f12, tapply(Sample.Value, State.Code, mean , na.rm=T))
df12<-data.frame(state=names(ta12), mean=ta12)
 
f<-merge(df99,df12,by="state")

par(mfrow=c(1,1))
with(f, plot(rep(1999,52) , f[,2] , col=colors()[f$state] , xlim=c(1998,2013), ylim=c(4,20) , pch=20)  )
with(f, points(rep(2012,52) , f[,3] , col=colors()[f$state] , pch=20) )
segments(1999,f$mean.x,2012,f$mean.y,col=colors()[f$state])
text ( 1999-1 , f$mean.x , labels = f$state , col=colors()[f$state] , cex=0.8)
