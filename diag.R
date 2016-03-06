## Diagnostics
{library(ggplot2)
  library(plyr)
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
  }
## sourcing batchmeans function used to calculate MCMC standard
## errors later
source("http://www.stat.psu.edu/~mharan/batchmeans.R")
load(file = "t4c.RData")

# mh.draws<-mcmc(Mult.chains[[6]][[1]][[5]])
# plot(mh.draws)
# autocorr.plot(mh.draws)
# rejectionRate(mh.draws)

## function to plot running means for different starting values
## sample.chains contain the univariate samples in a list for different starting values
rmean.plotter<-function(sample.chains){
  ## defining the data frame for plotting
  f<-list()
  ## defining the running means list
  rmean.chains<-list()
  ## defining the sequence for number of iterations
  n<-length(sample.chains[[1]])
  m<-seq(100,n,by=100)
  ## length of the sequence m
  ml<-length(m)
  ## initializing the means and MCMCse lists
  mean.chains<-list()
  MCMCse.chains<-list()
  for(j in 1:length(sample.chains)){
    mean.chains[[j]]<-rep(NA_real_,ml)
    MCMCse.chains[[j]]<-rep(NA_real_,ml)
    for (k in 1:ml){
      mean.chains[[j]][k]<-mean(sample.chains[[j]][1:m[k]])
      MCMCse.chains[[j]][k]<-bm(sample.chains[[j]][1:m[k]])$se
      #print(k)
    }
    f[[j]]<-data.frame(m,mean.chains[[j]],MCMCse.chains[[j]],j)
  }
  fdata<-do.call(rbind,f)
  names(fdata)<-c("iterations","mean.X1","MCMCse.X1","start.label")
  #names(f)<-c("iterations","mean.X1","mean.X2","mean.X3","MCMCse.X1","MCMCse.X2","MCMCse.X3","start.label")
  p<-ggplot(data=fdata)
  ## plotting mean vs. sample size for different starting values
  p1<-p+ geom_line(mapping = aes(x=iterations,y=mean.X1))+geom_errorbar(mapping = aes(x=iterations,y=mean.X1,ymin=mean.X1-MCMCse.X1,ymax=mean.X1+MCMCse.X1))+
    labs(x="Number of samples",y="Estimate of E(X) with MCMCse")
  p2<-p+ geom_line(mapping = aes(x=iterations,y=MCMCse.X1))+labs(x="Number of samples",y="MCMCse for X")
  #   p1<-p+ geom_line(mapping = aes(x=iterations,y=mean.X1,group=factor(start.label),colour=factor(start.label)),subset=.((iterations%%10)==0))+labs(x="Number of samples",y=expression(paste("Estimate of the E(X) with MCMCse")), colour="Label for starting values")+thema+
  #     geom_errorbar(mapping = aes(x=iterations,y=mean.X1,ymin=mean.X1-MCMCse.X1,ymax=mean.X1+MCMCse.X1,group=factor(start.label),colour=factor(start.label)),subset=.((iterations%%10)==0))
  #   p2<-p+ geom_line(mapping = aes(x=iterations,y=mean.X2,group=factor(start.label),colour=factor(start.label)),subset=.((iterations%%10)==0))+labs(x="Number of samples",y=expression(paste("Estimate of the E(", xi ,") with MCMCse")), colour="Label for starting values")+thema+
  #     geom_errorbar(mapping = aes(x=iterations,y=mean.X2,ymin=mean.X2-MCMCse.X2,ymax=mean.X2+MCMCse.X2,group=factor(start.label),colour=factor(start.label)),subset=.((iterations%%10)==0))
  #   p3<-p+ geom_line(mapping = aes(x=iterations,y=mean.X3,group=factor(start.label),colour=factor(start.label)),subset=.((iterations%%10)==0))+labs(x="Number of samples",y=expression(paste("Estimate of the E(", sigma ,") with MCMCse")), colour="Label for starting values")+thema+
  #     geom_errorbar(mapping = aes(x=iterations,y=mean.X3,ymin=mean.X3-MCMCse.X3,ymax=mean.X3+MCMCse.X3,group=factor(start.label),colour=factor(start.label)),subset=.((iterations%%10)==0))
  #   multiplot(p1, p2, p3,cols=1)
  return (list(p1,p2))
}

margdens.plotter<-function(sample.chains){
  ## defining the data frame for plotting
  n.chain<-length(sample.chains[[1]])
  g<-list()
  for(j in 1:length(sample.chains)){
    g[[j]]<-data.frame(1:n.chain,sample.chains[[j]],j)
  }
  gdata<-do.call(rbind,g)
  names(gdata)<-c("iterations","samples.X1","start.label")
  q<-ggplot(data=gdata)
  q1<-q+ geom_density(data = gdata[gdata[["iterations"]]<=(length(gdata[["iterations"]])/2),],mapping = aes(x=samples.X1),fill="steelblue")+
    labs(x="",y="For X",title="After N/2")
  q2<-q+geom_density(mapping = aes(x=samples.X1),fill="tomato")+labs(x="",y="For X",title="After N")
  #qm<-multiplot(q1,q2, cols=2)
  return(list(q1,q2))
}

a_i.chain<-list()
#a_i.chain[[1]]<-as.vector(T4.c5000[[5]])

#str(T4.c5000[[5]])
#undebug(rmean.plotter)
#debug(rmean.plotter)
#p<-rmean.plotter(a_i.chain)
#multiplot(p[[1]],p[[2]],cols = 2)
# p1<-p[[1]]
# p2<-p[[2]]
# p1
# p2
# 
# density<-margdens.plotter(a_i.chain)