
getOUVar <- function(t,const,rate)
    const*(1-exp(-rate*t))

simORsUnderStabSel <- function(nreps,fst,w2,N,ap,theta=1/w2){
    ## recover()
    thr <- qnorm(ap,lower.tail=FALSE)
    sig_t <- w2/(4*Ne)*(1-exp(-4*Ne*sig/w2*fst))
    my.means <- matrix(rnorm(2*nreps,0,sd=sqrt(sig_t)),ncol=2)
    my.prevs <- pnorm(thr,my.means,lower.tail=FALSE)
    mean.prevs <- pnorm(thr,rowMeans(my.means),lower.tail=FALSE)
    my.odds <- my.prevs/(1-my.prevs)
    my.ors <- my.odds[,1]/my.odds[,2]
    polarized.ors <- apply(my.odds,1,function(ORS)max(ORS)/min(ORS))
    return(list(my.ors,polarized.ors,mean.prevs,my.means,my.prevs))
}

n.anc.prevs <- 100
fsts <- c(0.003,0.01,0.03,0.1)
sig <- 1
w2 <- 100
Ne <- 10000
nreps <- 10000
aps <- 10^seq(-4,log(0.2,10),length.out=n.anc.prevs)






my.ors <- list()
pol.ors <- list()
mean.prevs <- list()
for(j in seq_along(fsts)){
    my.ors[[j]] <- list()
    pol.ors[[j]] <- list()
    mean.prevs[[j]] <- list()
    for(i in seq_along(aps)){
        tmp <- simORsUnderStabSel(nreps,fsts[j],w2=w2,N=Ne,aps[i])
        my.ors[[j]][[i]] <- tmp[[1]]
        pol.ors[[j]][[i]] <- tmp[[2]]
        mean.prevs[[j]][[i]] <- tmp[[3]]
        if(i %% 10 == 0)
            print(paste(j,": ", i,sep=''))
    }
}









if(FALSE){
    fst.test <- exp(seq(log(0.00001),log(0.1),length.out=1000))
    sim.vars <- numeric()
    ou.vars <- numeric()
    for(k in seq_along(fst.test)){
        tmp <- simORsUnderStabSel(nreps,fst.test[k],w2=w2,N=Ne,0.01)[[4]]
        my.vars[k] <- var(c(tmp))
        ou.vars[k] <- getOUVar(t=fst.test[k],const=w2/(4*Ne),rate=4*Ne*sig/w2)
        if(k%%100==0) print(k)
    }

    plot(
        ou.vars,
        my.vars
    )

}
