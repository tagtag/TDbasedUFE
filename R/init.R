#'
#'
#' @slot sample Sample names
#' @slot feature Feature id names
#' @slot value Feature values
#' @slot featureRange  Genomic coordinate attributed to feature id (if any)
#' @slot sampleData   Sample property (labels etc)
setClass("SummarizedExperimentTensor",slot=list(sample="array",
    feature="character",value="array",
    featureRange="GRanges",sampleData="list"))
#' @slot sample Sample names
#' @slot feature Feature id names
#' @slot value Feature values
#' @slot featureRange Genomic coordinate attributed to feature id (if any)
#' @slot sampleData  Sample property (labels etc)
setClass("SummarizedExperimentTensorSquare",slot=list(sample="array",
    feature="list",value="array",
    featureRange="GRanges",sampleData="list"))
