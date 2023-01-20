#' Title
#'  Select features (for tensor generated from squared matrix)
#' @param HOSVD output from HOSVD applied to tensor generated from squared matrix
#' @param input_all Selected singular value vector IDs
#' @param Multi Multiomics data
#' @param de Initial value for optimization of standard deviation
#' @param p0 Threshold P-value
#' @param breaks The number of bins
#' @param interact if interact mode or not
#'
#' @return List that includes selected features and computed P-value
#' @export
#'
#' @examples
#' omics1 <- matrix(runif(100000),ncol=10)
#' dimnames(omics1) <- list(1:10000,1:10)
#' omics2 <- matrix(runif(100000),ncol=10)
#' dimnames(omics2) <- dimnames(omics1)
#' Multi <- list(omics1,omics2)
#' Z <- PrepareSummarizedExperimentTensorSquare(sample=matrix(colnames(omics1),1),
#'             feature=list(omics1=rownames(omics1),
#'             omics2=rownames(omics2)),
#'             value=convertSquare(Multi),
#'             sampleData=list(NA))
#'             HOSVD <- computeHosvdSqure(Z)
#'               cond <- list(0,rep(1:2,each=5),c("A","B"))
#'             input_all <- selectSingularValueVectorLarge(HOSVD,cond)
#'             index <- selectFeatureSquare(HOSVD,input_all,Multi,de=c(0.1,0.1))
selectFeatureSquare<-function(HOSVD,input_all,Multi,
                              de=rep(1e-4,dim(HOSVD$U[[3]])[2]),p0=0.01,breaks=100,interact=T)
{
    th <- function(sd,breaks,p0){
        P2 <- pchisq((u/sd)^2,1,lower.tail=F)
        hc<- hist(1-P2,breaks=breaks,plot=F)
        return(sd(hc$count[1:sum(hc$breaks<1-min(P2[p.adjust(P2,"BH")>p0]))]))
    }
    index_all <- rep(list(NA),dim(HOSVD$U[[3]])[2])
    Multi <- lapply(Multi,function(x){x[is.na(x)]<-0;return(x)})
    for (i in 1:dim(HOSVD$U[[3]])[2])
    {
        u<-scale(Multi[[i]] %*% HOSVD$U[[1]][,input_all[1]])
        sd <- optim(de[i],function(x){th(x,breaks,p0)},control=list(warn.1d.NelderMead=F))$par
        sd1 <- seq(0.1*sd,2*sd,by=0.1*sd)
        th0 <- apply(matrix(sd1,ncol=1),1,function(x){th(x,breaks,p0)})
        P2 <- pchisq((u/sd)^2,1,lower.tail=F)
        par(mfrow=c(1,2))
        plot(sd1,th0,type="o")
        arrows(sd,max(th0),sd,min(th0),col=2)
        hist(1-P2,breaks=breaks)
        par(mfrow=c(1,1))
        if (interact)
        {
        cat("Press Enter to proceed:")
        readline()
        }
        index <- p.adjust(P2,"BH")<p0
        index_all[[i]] <- list(index=index,p.value=P2)
    }
    return(index_all)
}
