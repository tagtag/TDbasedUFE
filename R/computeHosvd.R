#' Title
#' Compute higher order singular value decomposition
#' @param Z array that includes omics data
#' @param dims dimensions to be compued by HOSVD
#' @param scale If value is scaled
#'
#' @return List that includes output from HOSVD
#' @export
#'
#' @examples
#'Z <- PrepareSummarizedExperimentTensor(sample=matrix(as.character(1:6),c(3,2)),
#'                                       feature=as.character(1:10),
#'                                       value=array(runif(10*3*2),c(10,3,2)))
#'HOSVD <- computeHosvd(Z)
computeHosvd <-function(Z,dims=c(10,dim(Z@value)[-1]),scale=T)
{
    if (scale)
    {
        hosvd(as.tensor(apply(Z@value,c(1:length(dims))[-1],scale)),dims)
    } else {
        hosvd(as.tensor(Z@value),dims)
    }
}
