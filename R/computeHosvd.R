#' Title
#' Compute higher order singular value decomposition
#' @param Z array that includes omics data
#' @param dims dimensions to be computed by HOSVD
#' @param scale If value is scaled
#'
#' @return List that includes output from HOSVD
#' @export
#'
#' @examples
#'Z <- PrepareSummarizedExperimentTensor(
#'sample=matrix(as.character(seq_len(6)),c(3,2)),
#'feature=as.character(seq_len(10)),
#'value=array(runif(10*3*2),c(10,3,2)))
#'HOSVD <- computeHosvd(Z)
computeHosvd <-function(Z,dims=c(10,dim(attr(Z,"value"))[-1]),scale=TRUE)
{
    if (scale)
    {
        hosvd(as.tensor(apply(attr(Z,"value"),
        c(seq_len(length(dims)))[-1],scale)),dims)
    } else {
        hosvd(as.tensor(attr(Z,"value")),dims)
    }
}
