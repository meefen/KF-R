
### Customized Shiny output
### Adapted from https://github.com/trestletech/shiny-sandbox/blob/master/grn/

## For ui.R
forceDirectedNetworkOutput <- function (outputId) {
  HTML(paste0("<div id=\"", outputId, 
              "\" class=\"shiny-network-output\"><svg /></div>"))
}

## For server.R
renderForceDirectedNetwork <- function(expr, env=parent.frame(), quoted=FALSE){
  
  installExprFunction(expr, "func", env, quoted)
  function() {
    
    val <- func()
    
    if (is.null(val)){
      return(list(names=character(), links=list(source=-1, target=-1)))
    }
    
    #TODO: re-arrange columns if necessary
    if (!all(rownames(val) == colnames(val))){
      stop("Colnames and rownames of your matrix must be identical")
    }
    
    diag(val) <- 0
    
    #make the matrix symmetric
    val <- symmetricize(val, method="avg")
    
    #now consider only the upper half of the matrix
    val[lower.tri(val)] <- 0
    
    conns <- cbind(source=row(val)[val>0]-1, target=col(val)[val>0]-1, weight=val[val>0])
    
    if (nrow(conns) == 0){
      conns <- list (source=-1, target=-1, weight=0)
    }
    
    list(names=rownames(val), links=conns)
    
  }
}
