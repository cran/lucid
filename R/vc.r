# vc.r
# Time-stamp: <25 Nov 2014 10:20:19 c:/x/rpack/lucid/R/vc.r>

vc <- function(object, ...) UseMethod("vc")

# ----- default -----

vc.default <- function(object, ...) {
  stop("No default method exists for 'vc'.")
}

# ----- asreml -----

vc.asreml <- function (object, gamma=FALSE, ...) {
  # Kevin Wright

  vv <- summary(object)$varcomp
  if(gamma==FALSE)
    vv$gamma <- NULL

  nm <- rownames(vv)
  nm <- factor(nm, levels=nm) # prevent alphanum sorting
  vv <- cbind(effect=nm, vv)
  rownames(vv) <- NULL

  class(vv) <- c("vc.asreml", class(vv))
  return(vv)
}

print.vc.asreml  <- function(x, dig=4, ...){
  # Kevin Wright
  class(x) <- class(x)[-1] # remove vc.asreml

  # Use 2 signif decimals for z.ratio
  x$z.ratio <- signif(x$z.ratio, 2)

  x[] <- lapply(x, lucid, dig)

  # Rename for printing.  Not all columns are always present--don't use reName
  cn <- colnames(x)
  cn[cn=="constraint"] <- "constr"
  colnames(x) <- cn

  # Shorten constraint to 3-letter code
  levels(x$constr)[levels(x$constr)=="Fixed"] <- "fix"
  levels(x$constr)[levels(x$constr)=="Boundary"] <- "bound"
  levels(x$constr)[levels(x$constr)=="Positive"] <- "pos"
  levels(x$constr)[levels(x$constr)=="Unconstrained"] <- "uncon"

  print(x, row.names=FALSE) # Do not print row numbers
  invisible(x)
}

# ----- lme -----

vc.lme <- function(object, ...) {
  # Kevin Wright
  vv <- nlme::VarCorr(object)
  vv <- as.matrix(vv)

  # Convert from text to numeric matrix, then to data.frame
  nm <- rownames(vv)
  nm <- factor(nm, levels=nm) # prevent alphanum sorting

  v2 <- apply(vv, 2, function(x) suppressWarnings(as.numeric(x)))
  v2 <- as.data.frame(v2)
  v2 <- cbind(effect=nm, v2)
  rownames(v2) <- NULL

  names(v2) <- tolower(names(v2))

  class(v2) <- c("vc.lme", class(v2))
  return(v2)
}
print.vc.lme <- function(x, dig=4, ...) {
  class(x) <- class(x)[-1] # remove vc.lme
  x[] <- lapply(x, lucid, dig)
  print(x, quote=FALSE, row.names=FALSE)
  invisible()
}

# ----- lme4 -----

vc.glmerMod <- function(object, ...) {
  dd <- as.data.frame(VarCorr(object))
  class(dd) <- c("vc.lmerMod", class(dd))
  return(dd)

}

vc.lmerMod <- function(object, ...) {
  dd <- as.data.frame(VarCorr(object))
  class(dd) <- c("vc.lmerMod", class(dd))
  return(dd)
}

print.vc.lmerMod <- function(x, dig=4, ...){
  class(x) <- class(x)[-1] # remove vc.lmerMod
  x[] <- lapply(x, lucid, dig)
  print(x, row.names=FALSE)
  invisible(x)
}

# ----- tests -----

if(FALSE) {

  require("nlme")
  #data(Rail)
  m1n <- lme(travel~1, random=~1|Rail, data=Rail)
  vc(m1n)

  require("lme4")
  m1l <- lmer(travel~1 + (1|Rail), data=Rail)
  vc(m1l)

  require("asreml")
  m1a <- asreml(travel~1, random=~Rail, data=Rail)
  vc(m1a)

}

