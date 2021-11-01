fsts <- c(0.01,0.05,0.1)

max.prev <- 0.2


anc.prevs <- exp(seq(log(0.00001),log(max.prev),length.out=1000))
thrs <- qnorm(1-anc.prevs)

mean.intervals <- list()
prev.lower <- list()
prev.upper <- list()
conf.inter <- c(0.5,0.8,0.95)

k <- 1
h2 <- 0.5
for(j in 1:length(fsts)){
    mean.intervals[[j]] <- sapply(conf.inter,function(INTER)qnorm(0.5+INTER/2,0,sqrt(0.5*fsts[[j]])))
    prev.lower[[j]] <- 1-sapply(thrs,function(THR) pnorm(THR,-mean.intervals[[j]]))
    prev.upper[[j]] <- 1-sapply(thrs,function(THR) pnorm(THR,mean.intervals[[j]]))
}

upper.ylim <- ceiling(10*max(unlist(prev.upper)))/10



my.cols <- c('darkgrey','grey','lightgrey')

my.names <- c('001','005','01')
for(j in 1:length(fsts)){
    pdf(paste('LT-drift-',my.names[j],'.pdf',sep=''),width=6,height=6)
    plot(
        NA,
        xlim=c(0,max.prev),
        ylim=c(0,upper.ylim),
        bty='n',
        xlab='Ancestral Prevalence',
        ylab='Present Day Prevalence'
    )
    lines(
        x=anc.prevs,
        y=anc.prevs,
        col='black',
        lty=3,
        lwd=1
    )
    for(i in 1:3){
        lines(
            x=anc.prevs,
            y=prev.lower[[j]][i,],
            col=my.cols[i],
            lty=i,
            lwd=2
        )
        lines(
            x=anc.prevs,
            y=prev.upper[[j]][i,],
            col=my.cols[i],
            lty=i,
            lwd=2
        )
    }
    dev.off()
}



