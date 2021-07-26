### Documentation ####
#' Predictions with LiblineaR model
#' 
#' The function applies a model (classification or regression) produced by the \code{LiblineaR} function to every row of a
#' data matrix and returns the model predictions.
#' 
#' @param object Object of class \code{"LiblineaR"}, created by
#'   \code{LiblineaR}.
#' @param newx An n x p matrix containing the new input data. A vector will be
#'   transformed to a n x 1 matrix. Sparse matrices of class matrix.csr from 
#'   package SparseM or sparse matrices of class dgCMatrix or dgRMatrix from
#'   package Matrix are also accepted. Note that C code at the core of LiblineaR
#'   package corresponds to a row-based sparse format. Hence, dgCMatrix inputs
#'   are first transformed into dgRMatrix format, which requires small extra
#'   computation time.
#' @param proba Logical indicating whether class probabilities should be
#'   computed and returned. Only possible if the model was fitted with
#'   \code{type}=0, \code{type}=6 or \code{type}=7, i.e. a Logistic Regression.
#'   Default is \code{FALSE}.
#' @param decisionValues Logical indicating whether model decision values should
#'   be computed and returned. Only possible for classification models
#'   (\code{type}<10). Default is \code{FALSE}.
#' @param ... Currently not used
#' 
#' @return 	By default, the returned value is a list with a single entry:
#' \item{predictions}{A vector of predicted labels (or values for regression).}
#' If \code{proba} is set to \code{TRUE}, and the model is a logistic
#' regression, an additional entry is returned:
#' \item{probabilities}{An n x k matrix (k number of classes) of the class
#'   probabilities. The columns of this matrix are named after class labels.}
#' If \code{decisionValues} is set to \code{TRUE}, and the model is not a
#' regression model, an additional entry is returned:
#' \item{decisionValues}{An n x k matrix (k number of classes) of the model
#'   decision values. The columns of this matrix are named after class labels.}
#' 
#' @references
#' 	\itemize{
#' \item 
#' For more information on LIBLINEAR itself, refer to:\cr
#' R.-E. Fan, K.-W. Chang, C.-J. Hsieh, X.-R. Wang, and C.-J. Lin.\cr
#' \emph{LIBLINEAR: A Library for Large Linear Classification,}\cr
#' Journal of Machine Learning Research 9(2008), 1871-1874.\cr
#' \url{https://www.csie.ntu.edu.tw/~cjlin/liblinear/}
#' }
#' 
#' @author Thibault Helleputte \email{thibault.helleputte@@dnalytics.com} and\cr
#'   Jerome Paul \email{jerome.paul@@dnalytics.com} and Pierre Gramme.\cr
#'   Based on C/C++-code by Chih-Chung Chang and Chih-Jen Lin
#' 
#' @note If the data on which the model has been fitted have been centered
#'   and/or scaled, it is very important to apply the same process on the
#'   \code{newx} data as well, with the scale and center values of the training
#'   data.
#' 
#' @seealso \code{\link{LiblineaR}}
#' 
#' @keywords classif regression multivariate models optimize classes
#' 
#' @importFrom methods as
#' @noRd

### Implementation ####
predict.LiblineaR<-function(object, newx, proba=FALSE, decisionValues=FALSE,...){
  
  # <Arg preparation>
  
  sparse=FALSE
  sparse2=FALSE
  
  if(sparse <- inherits(newx, "matrix.csr")){
    if(requireNamespace("SparseM",quietly=TRUE)){
      # trying to handle the sparse matrix case with SparseM package
      newx = SparseM::t(SparseM::t(newx)) # make sure column index are sorted
      n = newx@dimension[1]
      p = newx@dimension[2]
    } else {
      stop("newx inherits from 'matrix.csr', but 'SparseM' package is not available. Cannot proceed further. You could either use non-sparse matrix, install SparseM package or use sparse matrices based on Matrix package, also supported by LiblineaR.")
    }
  } else if(sparse2 <- (inherits(newx, "dgCMatrix") | inherits(newx,"dgRMatrix"))) {
    if(requireNamespace("Matrix",quietly=TRUE)){
      # trying to handle the sparse matrix case with Matrix package
      if(inherits(newx,"dgCMatrix")){
        # Transform column-based sparse matrix format of class dgCMatrix into 
        # row-based sparse matrix format of class dgRMatrix.
        newx<-as(as(newx,"matrix"),"dgRMatrix")
      }
      newx = Matrix::t(Matrix::t(newx)) # make sure column index are sorted
      n = dim(newx)[1]
      p = dim(newx)[2]
    } else {
      stop("newx inherits from 'dgCMatrix' or 'dgRMatrix', but 'Matrix' package is not available. Cannot proceed further. You could either use non-sparse matrix, install Matrix package or use sparse matrices based on SparseM package, also supported by LiblineaR.")
    }
  } else {
    # Nb samples
    n=dim(newx)[1]
    # Nb features
    p=dim(newx)[2]
  }
  
  # restrict features of newx to those used in model's W
  fNames = colnames(object$W)[colnames(object$W) != "Bias"]
  if(is.null(colnames(newx))) { # also handles SparseM (matrix.csr) case which always returns NULL for colnames, and Matrix (dgCMatrix) case which might sometimes return NULL for colnames.
    if (p != length(fNames))
      stop("dims of 'test' and 'train' differ")
  } else {
    # Check presence of all features and drop irrelevant features (allow reordering)
    if (!all(fNames %in% colnames(newx)))
      stop("columns of 'test' and 'train' differ")
    if(!identical(fNames,colnames(newx)))
      newx <- newx[,fNames,drop=FALSE]
  }
  
  # Type 
  if(!object$Type %in% c(0:7,11:13)){
    stop("Invalid model object: Wrong value for 'type'. Must be an integer between 0 and 7  or between 11 and 13 included.\n")
  }
  isRegression = object$Type>=11
  
  # Bias
  b = object$Bias
  
  # Returned probabilities default storage preparation
  Probabilities=matrix(data=-1)
  
  # Proba allowed?
  if(proba){
    if(!object$Type %in% c(0,6,7)){
      warning("Computing probabilities is only supported for Logistic Regressions (LiblineaR 'type' 0, 6 or 7).\n",
              "Accordingly, 'proba' was set to FALSE.")
      proba=FALSE
    }
    else{
      # Returned probabilities storage preparation 
      Probabilities=matrix(ncol=n*length(object$ClassNames),nrow=1,data=0)
    }
  }
  
  # Returned labels storage preparation
  Y=matrix(ncol=n,nrow=1,data=0)
  
  # Returned decision values default storage preparation
  DecisionValues=matrix(data=-1)
  
  # Returned decision values storage preparation
  if(decisionValues) {
    if(isRegression){
      warning("Computing decision values is only supported for classification (LiblineaR 'type' between 0 and 7).\n",
              "Accordingly, 'decisionValues' was set to FALSE.")
      decisionValues=FALSE
    }
    else{
      DecisionValues=matrix(ncol=n*length(object$ClassNames),nrow=1,data=0)
    }
  }
  
  # Codebook for labels
  cn=c(1:length(object$ClassNames))
  
  #
  # </Arg preparation>
  
  # as.double(t(X)) corresponds to rewrite X as a nxp-long vector instead of a n-rows and p-cols matrix. Rows of X are appended one at a time.
  
  ret <- .C(
    "predictLinear",
    as.double(Y),
    as.double(if(sparse){newx@ra}else if(sparse2){newx@x}else{t(newx)}),
    as.double(object$W),
    as.integer(decisionValues),
    as.double(t(DecisionValues)),
    as.integer(proba),
    as.double(t(Probabilities)),
    as.integer(object$NbClass),
    as.integer(p),
    as.integer(n),
    # sparse index info
    # If sparse or sparse2 is TRUE, then the following expression will return 1
    # And no need for the predictLinear C function to be able to distinguish between sparse and sparse2 as we send same info.
    as.integer(sparse+sparse2),
    as.integer(if(sparse){newx@ia}else if(sparse2){newx@p+1}else{0}), 
    as.integer(if(sparse){newx@ja}else if(sparse2){newx@j+1}else{0}),
    
    as.double(b),
    as.integer(cn),
    as.integer(object$Type),
    PACKAGE = "quanteda.textmodels"
  )
  
  result=list()
  if(isRegression)
    result$predictions=ret[[1]]
  else
    result$predictions=object$ClassNames[as.integer(ret[[1]])]
  
  if(proba){
    result$probabilities=matrix(ncol=length(object$ClassNames),nrow=n,data=ret[[7]],byrow=TRUE)
    colnames(result$probabilities)=object$ClassNames
  }
  
  if(decisionValues){
    result$decisionValues=matrix(ncol=length(object$ClassNames),nrow=n,data=ret[[5]],byrow=TRUE)
    colnames(result$decisionValues)=object$ClassNames
  }
  
  return(result)
  
}