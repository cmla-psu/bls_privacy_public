set.seed(1)

nreps=10000
nestab=10
presettheme=theme(axis.text.x = element_blank(),plot.title = element_text(size=7),axis.title.x = element_blank())


shapev=10
scalev=200
genprops=as.data.frame(MCMCprecision::rdirichlet(nreps,rgamma(nestab,shape=shapev,scale=scalev)))
genprops.long=tidyr::pivot_longer(genprops,cols=colnames(genprops),names_to = "estab",values_to = "prop")
genprops.long=dplyr::arrange(genprops.long,prop)
genprops.long$estabid=as.numeric(factor(genprops.long$estab,levels=unique(genprops.long$estab)))

df=genprops.long
df$shape=paste0("Shape=",shapev)
df$scale=paste0("Scale=",scalev)


shapev=1
genprops=as.data.frame(MCMCprecision::rdirichlet(nreps,rgamma(nestab,shape=shapev,scale=scalev)))
genprops.long=tidyr::pivot_longer(genprops,cols=colnames(genprops),names_to = "estab",values_to = "prop")
genprops.long=dplyr::arrange(genprops.long,prop)
genprops.long$estabid=as.numeric(factor(genprops.long$estab,levels=unique(genprops.long$estab)))
genprops.long$shape=paste0("Shape=",shapev)
genprops.long$scale=paste0("Scale=",scalev)
df=rbind(df,genprops.long)

shapev=20
genprops=as.data.frame(MCMCprecision::rdirichlet(nreps,rgamma(nestab,shape=shapev,scale=scalev)))
genprops.long=tidyr::pivot_longer(genprops,cols=colnames(genprops),names_to = "estab",values_to = "prop")
genprops.long=dplyr::arrange(genprops.long,prop)
genprops.long$estabid=as.numeric(factor(genprops.long$estab,levels=unique(genprops.long$estab)))
genprops.long$shape=paste0("Shape=",shapev)
genprops.long$scale=paste0("Scale=",scalev)

df=rbind(df,genprops.long)

shapev=10
scalev=50
genprops=as.data.frame(MCMCprecision::rdirichlet(nreps,rgamma(nestab,shape=shapev,scale=scalev)))
genprops.long=tidyr::pivot_longer(genprops,cols=colnames(genprops),names_to = "estab",values_to = "prop")
genprops.long=dplyr::arrange(genprops.long,prop)
genprops.long$estabid=as.numeric(factor(genprops.long$estab,levels=unique(genprops.long$estab)))
genprops.long$shape=paste0("Shape=",shapev)
genprops.long$scale=paste0("Scale=",scalev)

df=rbind(df,genprops.long)


shapev=1
genprops=as.data.frame(MCMCprecision::rdirichlet(nreps,rgamma(nestab,shape=shapev,scale=scalev)))
genprops.long=tidyr::pivot_longer(genprops,cols=colnames(genprops),names_to = "estab",values_to = "prop")
genprops.long=dplyr::arrange(genprops.long,prop)
genprops.long$estabid=as.numeric(factor(genprops.long$estab,levels=unique(genprops.long$estab)))
genprops.long$shape=paste0("Shape=",shapev)
genprops.long$scale=paste0("Scale=",scalev)

df=rbind(df,genprops.long)

shapev=20
genprops=as.data.frame(MCMCprecision::rdirichlet(nreps,rgamma(nestab,shape=shapev,scale=scalev)))
genprops.long=tidyr::pivot_longer(genprops,cols=colnames(genprops),names_to = "estab",values_to = "prop")
genprops.long=dplyr::arrange(genprops.long,prop)
genprops.long$estabid=as.numeric(factor(genprops.long$estab,levels=unique(genprops.long$estab)))
genprops.long$shape=paste0("Shape=",shapev)
genprops.long$scale=paste0("Scale=",scalev)

df=rbind(df,genprops.long)


shapev=10
scalev=350
genprops=as.data.frame(MCMCprecision::rdirichlet(nreps,rgamma(nestab,shape=shapev,scale=scalev)))
genprops.long=tidyr::pivot_longer(genprops,cols=colnames(genprops),names_to = "estab",values_to = "prop")
genprops.long=dplyr::arrange(genprops.long,prop)
genprops.long$estabid=as.numeric(factor(genprops.long$estab,levels=unique(genprops.long$estab)))

genprops.long$shape=paste0("Shape=",shapev)
genprops.long$scale=paste0("Scale=",scalev)

df=rbind(df,genprops.long)

plt1=ggplot(genprops.long,aes(y=prop,x=estabid,group=estabid))+
  geom_boxplot()+theme_minimal()+
  labs(y="generated proportion")+
  ggtitle(paste0("Shape=",shapev," Scale=",scalev))+presettheme


shapev=1
genprops=as.data.frame(MCMCprecision::rdirichlet(nreps,rgamma(nestab,shape=shapev,scale=scalev)))
genprops.long=tidyr::pivot_longer(genprops,cols=colnames(genprops),names_to = "estab",values_to = "prop")
genprops.long=dplyr::arrange(genprops.long,prop)
genprops.long$estabid=as.numeric(factor(genprops.long$estab,levels=unique(genprops.long$estab)))
genprops.long$shape=paste0("Shape=",shapev)
genprops.long$scale=paste0("Scale=",scalev)

df=rbind(df,genprops.long)
plt2=ggplot(genprops.long,aes(y=prop,x=estabid,group=estabid))+
  geom_boxplot()+theme_minimal()+
  labs(y="generated proportion")+
  ggtitle(paste0("Shape=",shapev," Scale=",scalev))+presettheme

shapev=20
genprops=as.data.frame(MCMCprecision::rdirichlet(nreps,rgamma(nestab,shape=shapev,scale=scalev)))
genprops.long=tidyr::pivot_longer(genprops,cols=colnames(genprops),names_to = "estab",values_to = "prop")
genprops.long=dplyr::arrange(genprops.long,prop)
genprops.long$estabid=as.numeric(factor(genprops.long$estab,levels=unique(genprops.long$estab)))
genprops.long$shape=paste0("Shape=",shapev)
genprops.long$scale=paste0("Scale=",scalev)

df=rbind(df,genprops.long)

df$shape=factor(as.character(df$shape),levels=paste0("Shape=",c(1,10,20)))
df$scale=factor(as.character(df$scale),levels=paste0("Scale=",c(50,200,350)))

ggplot(df,aes(y=prop,x=estabid,group=estabid))+geom_boxplot()+theme_minimal()+
  labs(y="proportion")+presettheme+facet_grid(shape~scale)


nreps=6
nestab=5
shapev=10
scalev=200
genprops=as.data.frame(MCMCprecision::rdirichlet(nreps,rgamma(nestab,shape=shapev,scale=scalev)))
head(genprops)