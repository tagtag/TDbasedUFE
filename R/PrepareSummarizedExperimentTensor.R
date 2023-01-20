#' Title
#' Generate feature values formatted as a tensor format
#' @param sample Sample names
#' @param feature Feature id names
#' @param value Feature values
#' @param featureRange Genomic coordinate attributed to feature id (if any)
#' @param sampleData Sample property (labels etc)
#'
#' @return A tensor including sample names, feature id, feature values, associated with featureRange and sample properties
#' @export
#'
#' @examples
#'Z <- PrepareSummarizedExperimentTensor(sample=matrix(as.character(1:6),c(3,2)),
#'                                       feature=as.character(1:10),
#'                                       value=array(runif(10*3*2),c(10,3,2)))
PrepareSummarizedExperimentTensor <- function(sample,feature,value,featureRange=GRanges(NULL),sampleData=list(NULL))
{
    new("SummarizedExperimentTensor",sample=sample,feature=feature,value=value,
        featureRange=featureRange,sampleData=sampleData)
}
