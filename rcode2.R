suppressWarnings(library(ggplot2)); suppressMessages(library(data.table)); 
library(grid);
source('multiplot.R');

dt<-data.table(ToothGrowth)
setnames(dt,c('len','supp','dose'),c('Length','Supplement','Dose'))
dt$Dose <- as.factor(dt$Dose)
dt$Supplement<- as.factor(dt$Supplement)
library("ggplot2")

summary(dt)
str(dt)
# plot 2 group by dose
g1<-ggplot(dt,aes(x=Dose,y=Length))
g1<-g1+geom_point(aes(color=Supplement),size=4)+ 
        geom_point(alpha = 0.1)+    
        labs(title="The length of odontoblasts by amount of dose",fill="type")
# plot 2 group by sup
g2<-ggplot(dt,aes(x=Supplement,y=Length))
g2<-g2+geom_point(aes(color=Dose),size=4)+ 
        geom_point(alpha = 0.1)+    
        labs(title="The length of odontoblasts by type of Vitamin C",fill="type")
# plot together
multiplot(g1,g2,cols=2)



g1<-subset(dt,Dose==0.5)$Length
g2<-subset(dt,Dose==1)$Length
test1$p.value
test1<-t.test(g1,g2)
test1$conf.int[1:2]



g1<-subset(dt,Dosage==1)$Length
g2<-subset(dt,Dosage==2)$Length
test2$p.value
test2<-t.test(g1,g2)
test2$conf.int[1:2]




g1<-subset(dt,Supplement=='VC')$Length
g2<-subset(dt,Supplement=='OJ')$Length
test3<-t.test(g1,g2)
test3$p.value
test3$conf.int[1:2]


g1<-subset(dt,Supplement=='VC' & Dosage=='SM')$Length
g2<-subset(dt,Supplement=='OJ' & Dosage=='SM')$Length
test4<-t.test(g1,g2)
test3$p.value
test4$conf.int[1:2]

g1<-subset(dt,Supplement=='VC' & Dosage=='MD')$Length
g2<-subset(dt,Supplement=='OJ' & Dosage=='MD')$Length
test5<-t.test(t1,t2)
test3$p.value
test5$conf.int[1:2]














