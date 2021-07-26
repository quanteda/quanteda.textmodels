### Documentation ####
#' Linear predictive models estimation based on the LIBLINEAR C/C++ Library.
#' 
#' @description
#' \code{LiblineaR} allows the estimation of predictive linear models for 
#' classification and regression, such as L1- or L2-regularized logistic 
#' regression, L1- or L2-regularized L2-loss support vector classification, 
#' L2-regularized L1-loss support vector classification and multi-class support 
#' vector classification. It also supports L2-regularized support vector regression 
#' (with L1- or L2-loss). The estimation of the models is particularly fast as 
#' compared to other libraries. The implementation is based on the LIBLINEAR C/C++ 
#' library for machine learning.
#' 
#' @details
#' For details for the implementation of LIBLINEAR, see the README file of the
#' original c/c++ LIBLINEAR library at
#' \url{https://www.csie.ntu.edu.tw/~cjlin/liblinear/}.
#' 
#' @param data 	a nxp data matrix. Each row stands for an example (sample,
#'   point) and each column stands for a dimension (feature, variable). Sparse
#'   matrices of class matrix.csr from package SparseM or sparse matrices of
#'   class dgCMatrix or dgRMatrix from package Matrix are also accepted. Note 
#'   that C code at the core of LiblineaR package corresponds to a row-based 
#'   sparse format. Hence, dgCMatrix inputs are first transformed into dgRMatrix
#'   format, which requires small extra computation time.
#' @param target a response vector for prediction tasks with one value for 
#'   each of the n rows of \code{data}. For classification, the values 
#'   correspond to class labels and can be a 1xn matrix, a simple vector or a 
#'   factor. For regression, the values correspond to the values to predict, and
#'   can be a 1xn matrix or a simple vector.
#' @param type \code{LiblineaR} can produce 10 types of (generalized) linear 
#'   models, by combining several types of loss functions and regularization 
#'   schemes. The regularization can be L1 or L2, and the losses can be the
#'   regular L2-loss for SVM (hinge loss), L1-loss for SVM, or the logistic loss
#'   for logistic regression. The default value for \code{type} is 0. See
#'   details below. Valid options are:
#'   \describe{
#'     \item{for multi-class classification}{
#'       \itemize{
#'         \item 0 -- L2-regularized logistic regression (primal)
#'         \item 1 -- L2-regularized L2-loss support vector classification (dual)
#'         \item 2 -- L2-regularized L2-loss support vector classification (primal)
#'         \item 3 -- L2-regularized L1-loss support vector classification (dual)
#'         \item 4 -- support vector classification by Crammer and Singer
#'         \item 5 -- L1-regularized L2-loss support vector classification
#'         \item 6 -- L1-regularized logistic regression
#'         \item 7 -- L2-regularized logistic regression (dual)
#'       }
#'     }
#'     \item{for regression}{
#'       \itemize{
#'         \item 11 -- L2-regularized L2-loss support vector regression (primal)
#'         \item 12 -- L2-regularized L2-loss support vector regression (dual)
#'         \item 13 -- L2-regularized L1-loss support vector regression (dual)
#'       }
#'     }
#'   }
#' 
#' @param cost cost of constraints violation (default: 1). Rules the trade-off
#'   between regularization and correct classification on \code{data}. It can be
#'   seen as the inverse of a regularization constant. See information on the
#'   'C' constant in details below. A usually good baseline heuristics to tune
#'   this constant is provided by the \code{heuristicC} function of this
#'   package.
#' @param epsilon set tolerance of termination criterion for optimization.
#'   If \code{NULL}, the LIBLINEAR defaults are used, which are:
#'   \describe{
#'     \item{if \code{type} is 0, 2, 5 or 6}{\code{epsilon}=0.01}
#'     \item{if \code{type} is 1, 3, 4, 7, 12 or 13}{\code{epsilon}=0.1}
#'   }
#'   
#'   % Watch out, below: in fact, \eqn{LaTeX}{ascii} is counter-intuitive.
#'   % the man pages in terminal mode are extracted from LaTeX argument, not the ascii argument !
#'   
#'   The meaning of \code{epsilon} is as follows:
#'   \describe{
#' 	    \item{if \code{type} is 0 or 2:}{
#' 			      \eqn{|f'(w)|_{2} \le \code{epsilon} \times \min (pos,neg) / l \times |f'(w_{0})|_{2}}{|f'(w)|_2 \le  epsilon * min(pos,neg) / l *|f'(w0)|_2},
#' 			      where f is the primal function and pos/neg are # of positive/negative data (default 0.01)}
#' 	    \item{if \code{type} is 11:}{
#' 			      \eqn{|f'(w)|_{2} \le \code{epsilon} \times |f'(w_{0})|_{2},}{|f'(w)|_2 \le epsilon * |f'(w0)|_2,}
#' 			      where f is the primal function (default 0.001)}
#'      \item{if \code{type} is 1, 3, 4 or 7:}{
#'            Dual maximal violation \eqn{\le \code{epsilon}}{\le epsilon} 
#'            (default 0.1)}
#'      \item{if \code{type} is 5 or 6:}{
#' 			      \eqn{|f'(w)|_\infty \le \code{epsilon}\times \min(pos,neg)/l\ |f'(w_{0})|_\infty,}{|f'(w)|_inf \le epsilon * min(pos,neg) / l*|f'(w0)|_inf,}
#' 			      where f is the primal function (default 0.01)}
#'      \item{if \code{type} is 12 or 13:}{
#' 			      \eqn{|f'(\alpha)|_1 \le \code{epsilon}\times |f'(\alpha_{0})|_1,}{|f'(alpha)|_1 \le epsilon * |f'(alpha0)|_1,}
#' 			      where f is the dual function (default 0.1)}
#' 	}
#' @param svr_eps set tolerance margin (epsilon) in regression loss function of SVR. Not used for classification methods.
#' @param bias if bias > 0, instance \code{data} becomes [\code{data}; \code{bias}]; if <= 0, no bias term added (default 1).
#' @param wi a named vector of weights for the different classes, used for
#'   asymmetric class sizes. Not all factor levels have to be supplied (default
#'   weight: 1). All components have to be named according to the corresponding
#'   class label. Not used in regression mode.
#' @param cross if an integer value k>0 is specified, a k-fold cross validation
#'   on \code{data} is performed to assess the quality of the model via a
#'   measure of the accuracy. Note that this metric might not be appropriate if
#'   classes are largely unbalanced. Default is 0.
#' @param verbose if \code{TRUE}, information are printed. Default is
#'   \code{FALSE}.
#' @param findC if \code{findC} is \code{TRUE} runs a cross-validation of \code{cross} folds to find the best cost (C) value (works only for type 0 and 2).
#'        Cross validation is conducted many times under parameters C = start_C, 2*start_C, 4*start_C, 8*start_C, ..., and finds the best one with the highest cross validation accuracy.
#'        The procedure stops when the models of all folds become stable or C reaches the maximal value of 1024.
#' @param useInitC if \code{useInitC} is \code{TRUE} (default) \code{cost} is used as the smallest start_C value of the search range (\code{findC} has to be \code{TRUE}).
#'        If \code{useInitC} is \code{FALSE}, then the procedure calculates a small enough start_C.
#' @param ... for backwards compatibility, parameter \code{labels} may be
#'   provided instead of \code{target}. A warning will then be issued, or an
#'   error if both are present. Other extra parameters are ignored.
#' 
#' @return
#' 	If \code{cross}>0, the average accuracy (classification) or mean square error (regression) computed over \code{cross} runs of cross-validation is returned.\cr\cr
#' Otherwise, an object of class \code{"LiblineaR"} containing the fitted model is returned, including:
#' \item{TypeDetail}{A string decsribing the type of model fitted, as determined by \code{type}.}
#' \item{Type}{An integer corresponding to \code{type}.}
#' \item{W}{A matrix with the model weights. If \code{bias} >0, \code{W} contains p+1 columns, the last being the bias term. The columns are named according to the names of \code{data}, if provided, or \code{"Wx"} where \code{"x"} ranges from 1 to the number of dimensions. The bias term is named \code{"Bias"}.If the number of classes is 2, or if in regression mode rather than classification, the matrix only has one row. If the number of classes is k>2 (classification), it has k rows. Each row i corresponds then to a linear model discriminating between class i and all the other classes. If there are more than 2 classes, rows are named according to the class i which is opposed to the other classes.}
#' \item{Bias}{The value of \code{bias}}
#' \item{ClassNames}{A vector containing the class names. This entry is not returned in case of regression models.}
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
#' @note Classification models usually perform better if each dimension of the data is first centered and scaled.
#' 
#' @seealso \code{\link{predict.LiblineaR}}, \code{\link{heuristicC}}
#' 
#' @examples
#' data(iris)
#' attach(iris)
#' 
#' x=iris[,1:4]
#' y=factor(iris[,5])
#' train=sample(1:dim(iris)[1],100)
#' 
#' xTrain=x[train,]
#' xTest=x[-train,]
#' yTrain=y[train]
#' yTest=y[-train]
#' 
#' # Center and scale data
#' s=scale(xTrain,center=TRUE,scale=TRUE)
#' 
#' # Find the best model with the best cost parameter via 10-fold cross-validations
#' tryTypes=c(0:7)
#' tryCosts=c(1000,1,0.001)
#' bestCost=NA
#' bestAcc=0
#' bestType=NA
#' 
#' for(ty in tryTypes){
#' 	for(co in tryCosts){
#' 		acc=LiblineaR(data=s,target=yTrain,type=ty,cost=co,bias=1,cross=5,verbose=FALSE)
#' 		cat("Results for C=",co," : ",acc," accuracy.\n",sep="")
#' 		if(acc>bestAcc){
#' 			bestCost=co
#' 			bestAcc=acc
#' 			bestType=ty
#' 		}
#' 	}
#' }
#' 
#' cat("Best model type is:",bestType,"\n")
#' cat("Best cost is:",bestCost,"\n")
#' cat("Best accuracy is:",bestAcc,"\n")
#' 
#' # Re-train best model with best cost value.
#' m=LiblineaR(data=s,target=yTrain,type=bestType,cost=bestCost,bias=1,verbose=FALSE)
#' 
#' # Scale the test data
#' s2=scale(xTest,attr(s,"scaled:center"),attr(s,"scaled:scale"))
#' 
#' # Make prediction
#' pr=FALSE
#' if(bestType==0 || bestType==7) pr=TRUE
#' 
#' p=predict(m,s2,proba=pr,decisionValues=TRUE)
#' 
#' # Display confusion matrix
#' res=table(p$predictions,yTest)
#' print(res)
#' 
#' # Compute Balanced Classification Rate
#' BCR=mean(c(res[1,1]/sum(res[,1]),res[2,2]/sum(res[,2]),res[3,3]/sum(res[,3])))
#' print(BCR)
#' 
#' #' #############################################
#' 
#' # Example of the use of a sparse matrix of class matrix.csr :
#' 
#' if(require(SparseM)){
#' 
#'  # Sparsifying the iris dataset:
#'  iS=apply(iris[,1:4],2,function(a){a[a<quantile(a,probs=c(0.25))]=0;return(a)})
#'  irisSparse<-as.matrix.csr(iS)
#' 
#'  # Applying a similar methodology as above:
#'  xTrain=irisSparse[train,]
#'  xTest=irisSparse[-train,]
#' 
#'  # Re-train best model with best cost value.
#'  m=LiblineaR(data=xTrain,target=yTrain,type=bestType,cost=bestCost,bias=1,verbose=FALSE)
#' 
#'  # Make prediction
#'  p=predict(m,xTest,proba=pr,decisionValues=TRUE)
#' 
#'  # Display confusion matrix
#'  res=table(p$predictions,yTest)
#'  print(res)
#' }
#' 
#' #' #############################################
#' 
#' # Example of the use of a sparse matrix of class dgCMatrix :
#' 
#' if(require(Matrix)){
#' 
#'  # Sparsifying the iris dataset:
#'  iS=apply(iris[,1:4],2,function(a){a[a<quantile(a,probs=c(0.25))]=0;return(a)})
#'  irisSparse<-as(iS,"sparseMatrix")
#' 
#'  # Applying a similar methodology as above:
#'  xTrain=irisSparse[train,]
#'  xTest=irisSparse[-train,]
#' 
#'  # Re-train best model with best cost value.
#'  m=LiblineaR(data=xTrain,target=yTrain,type=bestType,cost=bestCost,bias=1,verbose=FALSE)
#' 
#'  # Make prediction
#'  p=predict(m,xTest,proba=pr,decisionValues=TRUE)
#' 
#'  # Display confusion matrix
#'  res=table(p$predictions,yTest)
#'  print(res)
#' }
#' 
#' #############################################
#' 
#' # Try regression instead, to predict sepal length on the basis of sepal width and petal width:
#' 
#' xTrain=iris[c(1:25,51:75,101:125),2:3]
#' yTrain=iris[c(1:25,51:75,101:125),1]
#' xTest=iris[c(26:50,76:100,126:150),2:3]
#' yTest=iris[c(26:50,76:100,126:150),1]
#' 
#' # Center and scale data
#' s=scale(xTrain,center=TRUE,scale=TRUE)
#' 
#' # Estimate MSE in cross-vaidation on a train set
#' MSECross=LiblineaR(data = s, target = yTrain, type = 13, cross = 10, svr_eps=.01)
#' 
#' # Build the model
#' m=LiblineaR(data = s, target = yTrain, type = 13, cross=0, svr_eps=.01)
#' 
#' # Test it, after test data scaling:
#' s2=scale(xTest,attr(s,"scaled:center"),attr(s,"scaled:scale"))
#' pred=predict(m,s2)$predictions
#' MSETest=mean((yTest-pred)^2)
#' 
#' # Was MSE well estimated?
#' print(MSETest-MSECross)
#' 
#' # Distribution of errors
#' print(summary(yTest-pred))
#' 
#' 
#' 
#' @keywords classif regression multivariate models optimize classes
#' 
#' @importFrom methods as 
#' @noRd

### Implementation ####
LiblineaR <-function(data, target, type=0, cost=1, epsilon=0.01, svr_eps=NULL, bias=1, wi=NULL, cross=0, verbose=FALSE, findC=FALSE, useInitC=TRUE, ...) {
	# <Arg preparation>
  sparse=FALSE
  sparse2=FALSE
  if(sparse <- inherits(data, "matrix.csr")){
    if(requireNamespace("SparseM",quietly=TRUE)){
  		# trying to handle the sparse matrix case with SparseM package
	  	data = SparseM::t(SparseM::t(data)) # make sure column index are sorted
		  n = data@dimension[1]
		  p = data@dimension[2]
    } else {
      stop("data inherits from 'matrix.csr', but 'SparseM' package is not available. Cannot proceed further. You could either use non-sparse matrix, install SparseM package or use sparse matrices based on Matrix package, also supported by LiblineaR.")
    }
  } else if(sparse2 <- (inherits(data, "dgCMatrix") | inherits(data,"dgRMatrix"))) {
    if(requireNamespace("Matrix",quietly=TRUE)){
      # trying to handle the sparse matrix case with Matrix package
      if(inherits(data,"dgCMatrix")){
        # Transform column-based sparse matrix format of class dgCMatrix into 
        # row-based sparse matrix format of class dgRMatrix.
        data<-as(as(data,"matrix"),"dgRMatrix")
      }
      data = Matrix::t(Matrix::t(data)) # make sure column index are sorted
      n = dim(data)[1]
      p = dim(data)[2]
    } else {
      stop("data inherits from 'dgCMatrix' or 'dgRMatrix', but 'Matrix' package is not available. Cannot proceed further. You could either use non-sparse matrix, install Matrix package or use sparse matrices based on SparseM package, also supported by LiblineaR.")
    }
	} else {
		# Nb samples
		n=dim(data)[1]
		# Nb features
		p=dim(data)[2]
	}
  
	# Backwards compatibility after renaming 'labels' to 'target'
	cc = match.call()
	if ("labels" %in% names(cc)) {
		if("target" %in% names(cc)) {
			stop("Argument 'labels' is deprecated and was renamed to 'target'. Stopping due to ambiguous call.")
		} else {
			warning("Argument 'labels' is deprecated and was renamed to 'target'.")
			target=eval(cc$labels, parent.frame())
		}
	}
	rm(cc)

	# Bias
	b = if(bias > 0) bias else -1 # to ensure backward compatibility with boolean
	
	# Type 
	typesLabels = c("L2-regularized logistic regression (primal)", "L2-regularized L2-loss support vector classification (dual)", "L2-regularized L2-loss support vector classification (primal)", "L2-regularized L1-loss support vector classification (dual)",
									"support vector classification by Crammer and Singer", "L1-regularized L2-loss support vector classification", "L1-regularized logistic regression", "L2-regularized logistic regression (dual)",
									"", "", "",
									"L2-regularized L2-loss support vector regression (primal)", "L2-regularized L2-loss support vector regression (dual)", "L2-regularized L1-loss support vector regression (dual)")
	typesLabels = gsub("[()]","",typesLabels)
	typesCodes = c("L2R_LR", "L2R_L2LOSS_SVC_DUAL", "L2R_L2LOSS_SVC", "L2R_L1LOSS_SVC_DUAL",
								 "MCSVM_CS", "L1R_L2LOSS_SVC", "L1R_LR", "L2R_LR_DUAL",
								 "", "", "",
								 "L2R_L2LOSS_SVR", "L2R_L2LOSS_SVR_DUAL", "L2R_L1LOSS_SVR_DUAL")
	types=ifelse(typesCodes=="", "", paste0(typesLabels, " (", typesCodes, ")"))
	if(!type %in% (which(types!="")-1))
		stop("Unknown model type ",type,". Expecting one of: ", paste(which(types!="")-1, collapse=", "))
	isRegression = type>=11
	
	# Epsilon
	if(is.null(epsilon) || epsilon<0){
		# Will use LIBLINEAR default value for epsilon
		epsilon = -1
	}

	# Regression margin (epsilon)
	if(is.null(svr_eps) || svr_eps<0){
		svr_eps = 0.1
		# This default value may not make sense (depends on expected precision)
		if(isRegression)
			warning("No value provided for svr_eps. Using default of 0.1")
	}
	

	# Target
	if(!is.null(dim(target))) {
		if(identical(dim(target), c(n,1L)))
			target = target[,1]
		else
			stop("Wrong dimension for target")
	}
	if(length(target)!=n){
		stop("Number of target elements disagrees with number of data instances.")
	}
	
	if(isRegression) {
		yC = as.double(target)
		isMulticlass = FALSE
		targetLabels = NULL
		nbClass = 2 # necessary for predict
		nrWi = 0
		Wi = NULL
		WiLabels = NULL
	} else {
		if(is.character(target))
			target = factor(target)
		
		# targetLabels are sorted by first occurrence in target ; if target is a factor, targetLabels too, with the same levels.
		targetLabels = unique(target)
		nbClass = length(targetLabels)
		
		yC = as.integer(target)
		
		if (nbClass<2)
			stop("Wrong number of classes ( < 2 ).")
		isMulticlass = (nbClass>2) || (nbClass==2 && type==4)
		
		# Different class penalties? Default to 1
		nrWi=nbClass
		Wi=rep(1,times=nrWi)
		names(Wi)=as.character(targetLabels)
		WiLabels=as.integer(targetLabels)
		
		if(!is.null(wi)) {
			if(is.null(names(wi)))
				stop("wi has to be a named vector!")
			
			if( !all(names(Wi)%in% names(wi)) )
				stop("Mismatch between provided names for 'wi' and class labels.")
			
			for(i in 1:length(wi)){
				Wi[names(wi)[i]]=wi[i]
			}
		}
	}
	
	# Cross-validation?
	if(cross<0){
		stop("Cross-validation argument 'cross' cannot be negative!")
	}
	else if(cross>n){
		stop("Cross-validation argument 'cross' cannot be larger than the number of samples (",n,").",sep="")
	}
	
	# Return storage preparation
	labels_ret = rep(0L, nbClass)
	nr_W = if(isMulticlass) nbClass else 1
	nc_W = if(bias > 0) p+1 else p
	W_ret=matrix(data=0, nrow=nr_W, ncol=nc_W)
	
	#
	# </Arg preparation>
	# as.double(t(X)) corresponds to rewrite X as a nxp-long vector instead of a n-rows and p-cols matrix. Rows of X are appended one at a time.
	ret <- .C("trainLinear",
			as.double(W_ret),
			as.integer(labels_ret),
			as.double(if(sparse){data@ra}else if(sparse2){data@x}else{t(data)}),
			as.double(yC),
			as.integer(n),
			as.integer(p),
			# sparse index info. 
			# If sparse or sparse2 is TRUE, then the following expression will return 1
			# And, no need for the trainLinear C function to be able to distinguish between sparse and sparse2 as we send same info.
			as.integer(sparse+sparse2),
			as.integer(if(sparse){data@ia}else if(sparse2){data@p+1}else{0}), 
			as.integer(if(sparse){data@ja}else if(sparse2){data@j+1}else{0}),

			as.double(b),
			as.integer(type),
			as.double(cost),
			as.double(epsilon),
			as.double(svr_eps),
			as.integer(nrWi),
			as.double(Wi),
			as.integer(WiLabels),
			as.integer(cross),
			as.integer(verbose),
			as.integer(findC),
			as.integer(useInitC),
			PACKAGE = "quanteda.textmodels"
			)
	
	if(cross==0 && !findC){
		labels_ret = if(isRegression) NULL else ret[[2]]
		
		# classNames is a lookup table for conversion between outputs of C code and initial levels of target
		classNames = {
			if(isRegression) NULL 
			else if(is.logical(targetLabels)) as.logical(labels_ret)
			else if(is.null(levels(targetLabels))) labels_ret 
			else factor(levels(targetLabels)[labels_ret], levels=levels(targetLabels))
		}
		
		W_ret = matrix(data=ret[[1]], nrow=nr_W, ncol=nc_W)
		colnames(W_ret) = c(
			if(!is.null(colnames(data))) colnames(data) else paste0("W",1:p),
			if(bias > 0) "Bias" else c()
		)
		
		if(isMulticlass)
			rownames(W_ret) = classNames
		
		m=list()
		class(m)="LiblineaR"
		m$TypeDetail=types[type+1]
		m$Type=type
		m$W=W_ret
		m$Bias=bias
		m$ClassNames=classNames
		m$NbClass=nbClass
		return(m)
	}
	else{
		return(ret[[1]][1])
	}
}

