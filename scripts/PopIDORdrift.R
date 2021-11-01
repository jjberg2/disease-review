source("https://www.r-statistics.com/wp-content/uploads/2010/04/Quantile.loess_.r.txt")

simORs <- function(nreps,fst,anc.prev){
    thr <- qnorm(anc.prev,lower.tail=FALSE)
    my.means <- matrix(rnorm(2*nreps,0,sd=sqrt(fst)),ncol=2)
    my.prevs <- pnorm(thr,my.means,lower.tail=FALSE)
    mean.prevs <- pnorm(thr,rowMeans(my.means),lower.tail=FALSE)
    my.odds <- my.prevs/(1-my.prevs)
    my.ors <- my.odds[,1]/my.odds[,2]
    polarized.ors <- apply(my.odds,1,function(ORS)max(ORS)/min(ORS))

    return(list(my.ors,polarized.ors,mean.prevs))
}

anc.prevs <- 10^seq(-4,log(0.2,10),length.out=100)
fst <- 0.01
nreps <- 1000000

my.ors <- list()
pol.ors <- list()
mean.prevs <- list()
for(i in seq_along(anc.prevs)){
    tmp <- simORs(nreps,fst,anc.prevs[i])
    my.ors[[i]] <- tmp[[1]]
    pol.ors[[i]] <- tmp[[2]]
    mean.prevs[[i]] <- tmp[[3]]
}

my.means <- sapply(my.ors,mean)
plot(
    anc.prevs,
    my.means,
    log='x',
    type='l',
    ylim=c(1,max(my.means)*1.05)
)



## ancient prevs vs polarized ORs

mean.pol.ors <- sapply(pol.ors,mean)
pol.or.quants <- do.call(rbind,lapply(pol.ors,function(OR) quantile(OR,c(0.0275,0.1,0.25,0.5,0.75,0.9,0.975))))
ylim <- c(1,max(pol.or.quants))
my.cols <- viridis(ncol(pol.or.quants))
plot(
    anc.prevs,
    mean.pol.ors,
    log='x',
    lty=3,
    lwd=2,
    ylim=ylim,
    type='l'
)
matplot(
    x=anc.prevs,
    y=pol.or.quants,
    col=my.cols,
    lwd=1.5,
    lty=2,
    type='l',
    add=TRUE
)















