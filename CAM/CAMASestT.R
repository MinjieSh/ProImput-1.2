#' A and S matrix estimation by CAM
#'
#' This function estimates A and S matrix based on marker gene clusters
#' detected by CAM.
#' @param MGResult An object of class "\code{\link{CAMMGObj}}" obtained from
#'     \code{\link{CAMMGCluster}} function.
#' @param PrepResult An object of class "\code{\link{CAMPrepObj}}" obtained
#'     from \code{\link{CAMPrep}} function.
#' @param data Matrix of mixture expression profiles which need to be the same
#'     as the input of \code{\link{CAMPrep}}.
#'     Data frame, SummarizedExperiment or ExpressionSet object will be
#'     internally coerced into a matrix.
#'     Each row is a gene and each column is a sample.
#'     Data should be in non-log linear space with non-negative numerical values
#'     (i.e. >= 0). Missing values are not supported.
#'     All-zero rows will be removed internally.
#' @param corner.strategy The method to detect corner clusters.
#'     1: minimum sum of margin-of-errors; 2: minimum sum of reconstruction
#'     errors. The default is 2.
#' @param appro3 Estimate A and S matrix by approach 3 or not. Please see
#'     details for further information. The default is TRUE.
#' @param generalNMF If TRUE, the decomposed proportion matrix has no sum-to-one
#'     constraint for each row. Without this constraint, the scale ambiguity of
#'     each column vector in proportion matrix will not be removed.
#'     The default is FALSE.
#' @details This function is used internally by \code{\link{CAM}} function to
#' estimate proportion matrix (A), subpopulation-specific expression matrix (S)
#' and mdl values. It can also be used when you want to perform CAM step by
#' step.
#'
#' The mdl values are calculated in three approaches:
#' (1) based on data and A matrix in dimension-reduced space; (2) based on
#' original data with A matrix estimated by transforming dimension-reduced
#' A matrix back to original space;
#' (3) based on original data with A directly estimated in original space.
#' A and S matrix in original space estimated from the latter two approaches are
#' returned. mdl is the sum of two terms: code length of data under the model
#' and code length of model. Both mdl value and the first term (code length
#' of data) will be returned.
#' @return An object of class "\code{\link{CAMASObj}}" containing the
#' following components:
#' \item{Aest}{Estimated proportion matrix from Approach 2.}
#' \item{Sest}{Estimated subpopulation-specific expression matrix from
#'     Approach 2.}
#' \item{Aest.proj}{Estimated proportion matrix from Approach 2, before
#'     removing scale ambiguity.}
#' \item{Ascale}{The estimated scales to remove scale ambiguity
#'     of each column vector in Aest. Sum-to-one constraint on each row of
#'     Aest is used for scale estimation.}
#' \item{AestO}{Estimated proportion matrix from Approach 3.}
#' \item{SestO}{Estimated subpopulation-specific expression matrix from
#'     Approach 3.}
#' \item{AestO.proj}{Estimated proportion matrix from Approach 3, before
#'     removing scale ambiguity.}
#' \item{AscaleO}{The estimated scales to remove scale ambiguity
#'     of each column vector in AestO. Sum-to-one constraint on each row of
#'     AestO is used for scale estimation.}
#' \item{datalength}{Three values for code length of data. The first is
#'     calculated based on dimension-reduced data. The second and third are
#'     based on the original data.}
#' \item{mdl}{Three mdl values. The first is calculated based on
#'     dimension-reduced data. The second and third are based on the original
#'     data.}
#' @export
#' @examples
#' #obtain data
#' data(ratMix3)
#' data <- ratMix3$X
#'
#' #preprocess data
#' rPrep <- CAMPrep(data, dim.rdc = 3, thres.low = 0.30, thres.high = 0.95)
#'
#' #Marker gene cluster detection with a fixed K
#' rMGC <- CAMMGCluster(3, rPrep)
#'
#' #A and S matrix estimation
#' rASest <- CAMASest(rMGC, rPrep, data)
CAMASestT <- function(MGResult, PrepResult, data, corner.strategy = 2, 
                    appro3 = TRUE, generalNMF = FALSE) {
    if (is.null(MGResult)) {
        return (NULL)
    }
    if (is(data, "data.frame")) {
        data <- as.matrix(data)
    } else if (is(data, "SummarizedExperiment")) {
        data <- SummarizedExperiment::assay(data)
    } else if (is(data, "ExpressionSet")) {
        data <- Biobase::exprs(data)
    } else if (is(data, "matrix") == FALSE) {
        stop("Only matrix, data frame, SummarizedExperiment and ExpressionSet
            object are supported for expression data!")
    }
    if (is.null(rownames(data))) {
        rownames(data) <- seq_len(nrow(data))
        warning('Gene/probe name is missing!')
    }
    data <- data[rowSums(data) > 0,]

    c.valid <- !(PrepResult@cluster$cluster %in% PrepResult@c.outlier)
    geneValid <- PrepResult@Valid
    geneValid[geneValid][!c.valid] <- FALSE

    Xprep <- PrepResult@Xprep[,c.valid]
    Xproj <- PrepResult@Xproj[,c.valid]
    dataSize <- ncol(Xprep)

    Aproj <- PrepResult@centers[,as.character(MGResult@corner[corner.strategy,])]
    Kest <- ncol(Aproj)

    scale <- rep(1, Kest)
    if (generalNMF == FALSE) {
        #scale <- as.vector(coef(nnls(Aproj, rowSums(PrepResult$W))))
        scale <- c(NMF::.fcnnls(Aproj, PrepResult@W%*%PrepResult@SW)$coef)
        scale[scale<1e-10] <- 0.01/(sqrt(colSums(Aproj^2)))[scale<1e-10]
    }
    Apca <- Aproj %*% diag(scale)
    #S1 <- apply(Xprep, 2, function(x) coef(nnls(Apca,x)))
    S1 <- tryCatch({
        NMF::.fcnnls(Apca, Xprep)$coef
    }, error = function(msg) {
        return(apply(Xprep, 2, function(x) (nnls::nnls(Apca,x))$x))
    })
    datalength1 <- (nrow(Xprep) * dataSize) / 2 *
        log(mean((as.vector(Xprep - Apca %*% S1))^2))
    reconErr1 <- sum((as.vector(Xprep - Apca %*% S1))^2)
    penalty1 <- ((Kest - 1) * nrow(Xprep)) / 2 * log(dataSize) +
        (Kest * dataSize) / 2 * log(nrow(Xprep))
    mdl1 <- datalength1 + penalty1

    if (ncol(PrepResult@W) != ncol(data)) {
        stop("Data should be the same with the input of CAMPrep()!")
    }
    X <- t(data) * PrepResult@SW

    Aproj.org <- corpcor::pseudoinverse(PrepResult@W) %*% Aproj
    scale.org <- rep(1, Kest)
    if (generalNMF == FALSE) {
        scale.org <- c(NMF::.fcnnls(Aproj.org, matrix(PrepResult@SW, ncol=1))$coef)
        scale.org[scale.org<1e-10] <-
            0.01/(sqrt(colSums(Aproj.org^2)))[scale.org<1e-10]
    }
    Aest.org <- Aproj.org %*% diag(scale.org)
    Aest.org[Aest.org < 0] <- 0
    S2 <- tryCatch({
        NMF::.fcnnls(Aest.org, X)$coef
    }, error = function(msg) {
        return(apply(X, 2, function(x) (nnls::nnls(Aest.org,x))$x))
    })
    datalength2 <- (nrow(X) * dataSize) / 2 *
        log(mean((as.vector(X[,geneValid] - Aest.org %*% S2[,geneValid]))^2))
    reconErr2 <- sum((as.vector(X[,geneValid] - Aest.org %*% S2[,geneValid]))^2)
    penalty2 <- ((Kest - 1) * nrow(X)) / 2 * log(dataSize) +
        (Kest * dataSize) / 2 * log(nrow(X))
    mdl2 <- datalength2 + penalty2
    if (generalNMF == FALSE) {
        Aest.org <- Aest.org/rowSums(Aest.org)
    }


    MGlist <- lapply(as.character(MGResult@corner[corner.strategy,]),
        function(x) colnames(PrepResult@Xproj)[PrepResult@cluster$cluster == x])
    if (appro3){
        Xproj.all <- scale(X, center = FALSE, scale = colSums(X))
        Aproj.all <- matrix(0, ncol(data), length(MGlist))
        for(i in seq_along(MGlist)){
            Aproj.all[,i] <- pcaPP::l1median(t(Xproj.all[,MGlist[[i]]]))
        }
        scale.all <- rep(1, Kest)
        if (generalNMF == FALSE) {
            scale.all <- 
                c(NMF::.fcnnls(Aproj.all, matrix(PrepResult@SW, ncol=1))$coef)
            scale.all[scale.all<1e-10] <-
                0.01/(sqrt(colSums(Aproj.all^2)))[scale.all<1e-10]
        }
        Aest.all <- Aproj.all %*% diag(scale.all)
        S3 <- NMF::.fcnnls(Aest.all, X)$coef
        datalength3 <- (nrow(X) * dataSize) / 2 *
            log(mean((as.vector(X[,geneValid] - 
                                    Aest.all %*% S3[,geneValid]))^2))
        reconErr3 <- sum((as.vector(X[,geneValid] - 
                                        Aest.all %*% S3[,geneValid]))^2)
        penalty3 <- penalty2
        mdl3 <- datalength3 + penalty3
        if (generalNMF == FALSE) {
            Aest.all <- Aest.all/rowSums(Aest.all)
        }
    } else {
        Aest.all <- matrix(0,0,0)
        S3 <- matrix(0,0,0)
        Aproj.all <- matrix(0,0,0)
        scale.all <- matrix(0,0,0)
        datalength3 <- NULL
        mdl3 <- NULL
    }

    return(new("CAMASObj", Aest=Aest.org, Sest=t(S2), Aest.proj=Aproj.org,
                    Ascale=scale.org,
                    AestO=Aest.all, SestO=t(S3), AestO.proj=Aproj.all,
                    AscaleO=scale.all,
                    datalength=c(datalength1,datalength2,datalength3,
                                 penalty1, penalty2, penalty3,
                                 reconErr1, reconErr2, reconErr3),
                    mdl=c(mdl1,mdl2,mdl3)))
}
