#' Title
#' Select features
#' @param HOSVD output from HOSVD
#' @param input_all  Selected singular value IDs
#' @param de Initial value for optimization of standard deviation
#' @param p0 Threshold P-value
#' @param breaks The number of bins
#'
#' @return List that includes selected features and computed P-value
#' @export
#'
#' @examples
#' set.seed(2)
#' HOSVD <- hosvd(as.tensor(array(runif(10000*3*3),c(10000,3,3))),c(10,3,3))
#' input_all <- c(2,2)
#' index <- selectFeature(HOSVD,input_all,de=0.01,p0=0.01)
selectFeature<-function(HOSVD,input_all,de=1e-4,p0=0.01,breaks=100)
{
    th <- function(sd,breaks,p0){
        P2 <- pchisq((u/sd)^2,1,lower.tail=F)
        hc<- hist(1-P2,breaks=breaks,plot=F)
        return(sd(hc$count[1:sum(hc$breaks<1-min(P2[p.adjust(P2,"BH")>p0]))]))
    }
    column<-1:dim(HOSVD$Z@data)[1]
    for (i in 2:length(HOSVD$U)){
        column<- cbind(column,input_all[i-1])
    }
    u<-HOSVD$U[[1]][,which.max(abs(HOSVD$Z@data[column]))]
    sd <- optim(de,function(x){th(x,breaks,p0)},control=list(warn.1d.NelderMead=F))$par
    sd1 <- seq(0.1*sd,2*sd,by=0.1*sd)
    th0 <- apply(matrix(sd1,ncol=1),1,function(x){th(x,breaks,p0)})
    P2 <- pchisq((u/sd)^2,1,lower.tail=F)
    par(mfrow=c(1,2))
    plot(sd1,th0,type="o")
    arrows(sd,max(th0),sd,min(th0),col=2)
    hist(1-P2,breaks=breaks)
    par(mfrow=c(1,1))
    index <- p.adjust(P2,"BH")<p0
    return(list(index=index,p.value=P2))
}
