dens.prod.ordi <-
function(y.x,param,var.list=NULL)
{
    y <- y.x[1:length(param$alpha)]   
    if(length(y.x)==length(param$alpha)) x <- NULL
    else x <- y.x[(length(param$alpha)+1):length(y.x)]
    res <- rep(1,times=nrow(param$alpha[[1]]))
    for(k in 1:nrow(param$alpha[[1]])) for(j in 1:length(param$alpha)) if(!is.na(y[j]))
    {
        S.cov <- length(var.list[[j]])
        S.alp <- ncol(param$alpha[[j]])-S.cov+1

        covar.x <- ifelse(S.cov==0,0,sum(param$alpha[[j]][k,S.alp:(S.alp+S.cov-1)]*x[var.list[[j]]]))
        res[k] <- res[k]*p.compute(param$alpha[[j]][k,]+covar.x)[y[j]]
    }
    res
}

