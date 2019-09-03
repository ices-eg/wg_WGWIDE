# Created by Alfonso Perez, adjusted by Esther Beukhof

##########################################################################################-
#################  HURDLE models ##########################
##########################################################################################-

rm(list=ls())

library(pscl)

datpath="C:/git/wg_WGWIDE/NSHM/data/"
resultpath="C:/git/wg_WGWIDE/NSHM/results/"
figPath <- "C:/git/wg_WGWIDE/NSHM/figures/model/"

# Load cleaned survey data
#data <- read.csv("M:/My Documents/WGWIDE/Assessment 2019/NSHM/data_exploration/survey_data/survey_data_1992-2018_clean.csv",stringsAsFactors = FALSE)
data <- read.csv(paste0(resultpath,"survey_data_1992-2018_clean.csv",sep=""),stringsAsFactors = FALSE)


#### Prepare the data for modelling
str(data)
# data.n$Survey <- as.factor(data.n$Survey)
data$Year <- as.factor(data$Year)
str(data)



#######################################################-
####  Larger than the defined cutoff length  #########
#######################################################-

datos  <- data[data$groupsize=="exploitable",]                 ##---when Number=0, LngtClas is set as 0
dat <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulNo + ShootLong + ShootLat + Depth, FUN=sum, data=datos)
dim(dat);str(dat)

# Check how many zeroes
table(dat$CPUE>0) # bit more than 50%

# Round off CPUE values to make sure it's count data
dat$CPUE <- round(dat$CPUE,0)
# dat$Year <- as.factor(dat$Year)
str(dat)


### Hurdle model: gives warning!
hurdle2=hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin", zero.dist = "binomial")
# aic2=AIC(hurdle2)
summary(hurdle2) # this gives a warning!!
vcov <- data.frame(hurdle2$vcov)
# Calculate dispersion statistic
chi2 <- sum(resid(hurdle2)^2)
df <- hurdle2$df.residual
dis.st <- chi2/df # 1.09. with Poisson: 34, so much higher and not good in dealing with overdispersion


### Try zero-inflated model: no warning, but different theta and intercept, but similar coefficients for predictors
zi <- zeroinfl(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin") # with binomial for zeroes
summary(zi)
# Calculate dispersion statistic
chi2 <- sum(resid(zi)^2)
df <- zi$df.residual
dis.st <- chi2/df # 2.15


### What about removing 2018: no warning!
dat17 <- dat[!dat$Year%in%2018,]
h17=hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat17, dist = "negbin", zero.dist = "binomial") # no warning!
summary(h17) 
# Calculate dispersion statistic
chi2 <- sum(resid(h17)^2)
df <- h17$df.residual
dis.st <- chi2/df # 1.91


### Remove 1992: gives warning!
dat93 <- dat[!dat$Year%in%1992,]
h93=hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat93, dist = "negbin", zero.dist = "binomial") # no warning!
summary(h93) 
# Calculate dispersion statistic
chi2 <- sum(resid(h93)^2)
df <- h93$df.residual
dis.st <- chi2/df # 1.11


### Remove 2003: gives warning!
dat03 <- dat[!dat$Year%in%2003,]
h03=hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat03, dist = "negbin", zero.dist = "binomial") # no warning!
summary(h03) 
# Calculate dispersion statistic
chi2 <- sum(resid(h03)^2)
df <- h03$df.residual
dis.st <- chi2/df # 1.11


### Replace 2017 values by those of 2018 (and thus have no 2018): no warning
#data <- read.csv("M:/My Documents/WGWIDE/Assessment 2019/NSHM/data_exploration/survey_data/survey_data_1992-2018_clean.csv",stringsAsFactors = FALSE)
data <- read.csv(paste0(resultpath,"survey_data_1992-2018_clean.csv",sep=""),stringsAsFactors = FALSE)
datos  <- data[data$groupsize=="exploitable",]                 ##---when Number=0, LngtClas is set as 0
dat <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulNo + ShootLong + ShootLat + Depth, FUN=sum, data=datos)
dat1718 <- dat[!dat$Year %in% "2017",]
sort(unique(dat1718$Year))
dat1718$Year <- with(dat1718, ifelse(Year %in% "2018","2017",Year))
sort(unique(dat1718$Year))
dat1718$Year <- as.factor(dat1718$Year)
# Round off CPUE values to make sure it's count data
dat1718$CPUE <- round(dat1718$CPUE,0)
# Model
h1718=hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat1718, dist = "negbin", zero.dist = "binomial")
summary(h1718) # no warning!


### Swap 2014 and 2018: gives warning!
#data <- read.csv("M:/My Documents/WGWIDE/Assessment 2019/NSHM/data_exploration/survey_data/survey_data_1992-2018_clean.csv",stringsAsFactors = FALSE)
data <- read.csv(paste0(resultpath,"survey_data_1992-2018_clean.csv",sep=""),stringsAsFactors = FALSE)
datos  <- data[data$groupsize=="exploitable",]                 ##---when Number=0, LngtClas is set as 0
dat <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulNo + ShootLong + ShootLat + Depth, FUN=sum, data=datos)
dat1418 <- dat[!dat$Year %in% "2014",]
sort(unique(dat1418$Year))
dat1418$Year <- with(dat1418, ifelse(Year %in% "2018","2014",Year))
dat14 <- dat[dat$Year %in% "2014",]
dat14$Year <- with(dat14, ifelse(Year %in% "2014","2018",Year))
dat1418 <- rbind(dat1418,dat14)
sort(unique(dat1418$Year))
dat1418$Year <- as.factor(dat1418$Year)
# Round off CPUE values to make sure it's count data
dat1418$CPUE <- round(dat1418$CPUE,0)
# Model
h1418=hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat1418, dist = "negbin", zero.dist = "binomial")
summary(h1418) # gives warning!


### Swap 2005 and 2018: gives warning!
#data <- read.csv("M:/My Documents/WGWIDE/Assessment 2019/NSHM/data_exploration/survey_data/survey_data_1992-2018_clean.csv",stringsAsFactors = FALSE)
data <- read.csv(paste0(resultpath,"survey_data_1992-2018_clean.csv",sep=""),stringsAsFactors = FALSE)
datos  <- data[data$groupsize=="exploitable",]                 ##---when Number=0, LngtClas is set as 0
dat <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulNo + ShootLong + ShootLat + Depth, FUN=sum, data=datos)
dat0518 <- dat[!dat$Year %in% "2005",]
sort(unique(dat0518$Year))
dat0518$Year <- with(dat0518, ifelse(Year %in% "2018","2005",Year))
dat05 <- dat[dat$Year %in% "2005",]
dat05$Year <- with(dat05, ifelse(Year %in% "2005","2018",Year))
dat0518 <- rbind(dat0518,dat05)
sort(unique(dat0518$Year))
dat0518$Year <- as.factor(dat0518$Year)
# Round off CPUE values to make sure it's count data
dat0518$CPUE <- round(dat0518$CPUE,0)
# Model
h0518=hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat0518, dist = "negbin", zero.dist = "binomial")
summary(h0518) # gives warning!


### Randomize CPUE within 2018: gives warning!
dat18 <- dat[dat$Year %in% "2018",]
dat18$CPUE <- sample(dat18$CPUE)
dat17 <- dat[!dat$Year %in% "2018",]
dat.rnd <- rbind(dat17,dat18)
dat.rnd$Year <- as.factor(dat.rnd$Year)
# Round off CPUE values to make sure it's count data
dat.rnd$CPUE <- round(dat.rnd$CPUE,0)
# Model
h.rnd=hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat.rnd, dist = "negbin", zero.dist = "binomial")
summary(h.rnd) # gives warning!


### Put hurdle() in 'my' function to check where it goes wrong
my.hurdle <- function (formula, data, subset, na.action, weights, offset, 
                       dist = c("poisson", "negbin", "geometric"), 
                       zero.dist = c("binomial", "poisson", "negbin", 
                                     "geometric"), link = c("logit", "probit", 
                                                            "cloglog", "cauchit", "log"), control = hurdle.control(...), 
                       model = TRUE, y = TRUE, x = FALSE, ...) 
{
  zeroPoisson <- function(parms) {
    mu <- as.vector(exp(Z %*% parms + offsetz))
    loglik0 <- -mu
    loglik <- sum(weights[Y0] * loglik0[Y0]) + sum(weights[Y1] * 
                                                     log(1 - exp(loglik0[Y1])))
    loglik
  }
  countPoisson <- function(parms) {
    mu <- as.vector(exp(X %*% parms + offsetx))[Y1]
    loglik0 <- -mu
    loglik1 <- dpois(Y[Y1], lambda = mu, log = TRUE)
    loglik <- sum(weights[Y1] * loglik1) - sum(weights[Y1] * 
                                                 log(1 - exp(loglik0)))
    loglik
  }
  zeroNegBin <- function(parms) {
    mu <- as.vector(exp(Z %*% parms[1:kz] + offsetz))
    theta <- exp(parms[kz + 1])
    loglik0 <- suppressWarnings(dnbinom(0, size = theta, 
                                        mu = mu, log = TRUE))
    loglik <- sum(weights[Y0] * loglik0[Y0]) + sum(weights[Y1] * 
                                                     log(1 - exp(loglik0[Y1])))
    loglik
  }
  countNegBin <- function(parms) {
    mu <- as.vector(exp(X %*% parms[1:kx] + offsetx))[Y1]
    theta <- exp(parms[kx + 1])
    loglik0 <- suppressWarnings(dnbinom(0, size = theta, 
                                        mu = mu, log = TRUE))
    loglik1 <- suppressWarnings(dnbinom(Y[Y1], size = theta, 
                                        mu = mu, log = TRUE))
    loglik <- sum(weights[Y1] * loglik1) - sum(weights[Y1] * 
                                                 log(1 - exp(loglik0)))
    loglik
  }
  zeroGeom <- function(parms) zeroNegBin(c(parms, 0))
  countGeom <- function(parms) countNegBin(c(parms, 0))
  zeroBinom <- function(parms) {
    mu <- as.vector(linkinv(Z %*% parms + offsetz))
    loglik <- sum(weights[Y0] * log(1 - mu[Y0])) + sum(weights[Y1] * 
                                                         log(mu[Y1]))
    loglik
  }
  countGradPoisson <- function(parms) {
    eta <- as.vector(X %*% parms + offsetx)[Y1]
    mu <- exp(eta)
    colSums(((Y[Y1] - mu) - exp(ppois(0, lambda = mu, log.p = TRUE) - 
                                  ppois(0, lambda = mu, lower.tail = FALSE, log.p = TRUE) + 
                                  eta)) * weights[Y1] * X[Y1, , drop = FALSE])
  }
  countGradGeom <- function(parms) {
    eta <- as.vector(X %*% parms + offsetx)[Y1]
    mu <- exp(eta)
    colSums(((Y[Y1] - mu * (Y[Y1] + 1)/(mu + 1)) - exp(pnbinom(0, 
                                                               mu = mu, size = 1, log.p = TRUE) - pnbinom(0, mu = mu, 
                                                                                                          size = 1, lower.tail = FALSE, log.p = TRUE) - log(mu + 
                                                                                                                                                              1) + eta)) * weights[Y1] * X[Y1, , drop = FALSE])
  }
  countGradNegBin <- function(parms) {
    eta <- as.vector(X %*% parms[1:kx] + offsetx)[Y1]
    mu <- exp(eta)
    theta <- exp(parms[kx + 1])
    logratio <- pnbinom(0, mu = mu, size = theta, log.p = TRUE) - 
      pnbinom(0, mu = mu, size = theta, lower.tail = FALSE, 
              log.p = TRUE)
    rval <- colSums(((Y[Y1] - mu * (Y[Y1] + theta)/(mu + 
                                                      theta)) - exp(logratio + log(theta) - log(mu + theta) + 
                                                                      eta)) * weights[Y1] * X[Y1, , drop = FALSE])
    rval2 <- sum((digamma(Y[Y1] + theta) - digamma(theta) + 
                    log(theta) - log(mu + theta) + 1 - (Y[Y1] + theta)/(mu + 
                                                                          theta) + exp(logratio) * (log(theta) - log(mu + theta) + 
                                                                                                      1 - theta/(mu + theta))) * weights[Y1]) * theta
    c(rval, rval2)
  }
  zeroGradPoisson <- function(parms) {
    eta <- as.vector(Z %*% parms + offsetz)
    mu <- exp(eta)
    colSums(ifelse(Y0, -mu, exp(ppois(0, lambda = mu, log.p = TRUE) - 
                                  ppois(0, lambda = mu, lower.tail = FALSE, log.p = TRUE) + 
                                  eta)) * weights * Z)
  }
  zeroGradGeom <- function(parms) {
    eta <- as.vector(Z %*% parms + offsetz)
    mu <- exp(eta)
    colSums(ifelse(Y0, -mu/(mu + 1), exp(pnbinom(0, mu = mu, 
                                                 size = 1, log.p = TRUE) - pnbinom(0, mu = mu, size = 1, 
                                                                                   lower.tail = FALSE, log.p = TRUE) - log(mu + 1) + 
                                           eta)) * weights * Z)
  }
  zeroGradNegBin <- function(parms) {
    eta <- as.vector(Z %*% parms[1:kz] + offsetz)
    mu <- exp(eta)
    theta <- exp(parms[kz + 1])
    logratio <- pnbinom(0, mu = mu, size = theta, log.p = TRUE) - 
      pnbinom(0, mu = mu, size = theta, lower.tail = FALSE, 
              log.p = TRUE)
    rval <- colSums(ifelse(Y0, -mu * theta/(mu + theta), 
                           exp(logratio + log(theta) - log(mu + theta) + eta)) * 
                      weights * Z)
    rval2 <- sum(ifelse(Y0, log(theta) - log(mu + theta) + 
                          1 - theta/(mu + theta), -exp(logratio) * (log(theta) - 
                                                                      log(mu + theta) + 1 - theta/(mu + theta))) * weights * 
                   theta)
    c(rval, rval2)
  }
  zeroGradBinom <- function(parms) {
    eta <- as.vector(Z %*% parms + offsetz)
    mu <- linkinv(eta)
    colSums(ifelse(Y0, -1/(1 - mu), 1/mu) * linkobj$mu.eta(eta) * 
              weights * Z)
  }
  dist <- match.arg(dist)
  zero.dist <- match.arg(zero.dist)
  countDist <- switch(dist, poisson = countPoisson, geometric = countGeom, 
                      negbin = countNegBin)
  zeroDist <- switch(zero.dist, poisson = zeroPoisson, geometric = zeroGeom, 
                     negbin = zeroNegBin, binomial = zeroBinom)
  countGrad <- switch(dist, poisson = countGradPoisson, geometric = countGradGeom, 
                      negbin = countGradNegBin)
  zeroGrad <- switch(zero.dist, poisson = zeroGradPoisson, 
                     geometric = zeroGradGeom, negbin = zeroGradNegBin, binomial = zeroGradBinom)
  loglikfun <- function(parms) countDist(parms[1:(kx + (dist == 
                                                          "negbin"))]) + zeroDist(parms[(kx + (dist == "negbin") + 
                                                                                           1):(kx + kz + (dist == "negbin") + (zero.dist == 
                                                                                                                                 "negbin"))])
  gradfun <- function(parms) c(countGrad(parms[1:(kx + (dist == 
                                                          "negbin"))]), zeroGrad(parms[(kx + (dist == "negbin") + 
                                                                                          1):(kx + kz + (dist == "negbin") + (zero.dist == 
                                                                                                                                "negbin"))]))
  linkstr <- match.arg(link)
  linkobj <- make.link(linkstr)
  linkinv <- linkobj$linkinv
  if (control$trace) 
    cat("Hurdle Count Model\n", paste("count model:", 
                                      dist, "with log link\n"), paste("zero hurdle model:", 
                                                                      zero.dist, "with", ifelse(zero.dist == "binomial", 
                                                                                                linkstr, "log"), "link\n"), sep = "")
  cl <- match.call()
  if (missing(data)) 
    data <- environment(formula)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", 
               "na.action", "weights", "offset"), 
             names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  if (length(formula[[3]]) > 1 && identical(formula[[3]][[1]], 
                                            as.name("|"))) {
    ff <- formula
    formula[[3]][1] <- call("+")
    mf$formula <- formula
    ffc <- . ~ .
    ffz <- ~.
    ffc[[2]] <- ff[[2]]
    ffc[[3]] <- ff[[3]][[2]]
    ffz[[3]] <- ff[[3]][[3]]
    ffz[[2]] <- NULL
  }
  else {
    ffz <- ffc <- ff <- formula
    ffz[[2]] <- NULL
  }
  if (inherits(try(terms(ffz), silent = TRUE), "try-error")) {
    ffz <- eval(parse(text = sprintf(paste("%s -", 
                                           deparse(ffc[[2]])), deparse(ffz))))
  }
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  mtX <- terms(ffc, data = data)
  X <- model.matrix(mtX, mf)
  mtZ <- terms(ffz, data = data)
  mtZ <- terms(update(mtZ, ~.), data = data)
  Z <- model.matrix(mtZ, mf)
  Y <- model.response(mf, "numeric")
  if (length(Y) < 1) 
    stop("empty model")
  if (all(Y > 0)) 
    stop("invalid dependent variable, minimum count is not zero")
  if (!isTRUE(all.equal(as.vector(Y), as.integer(round(Y + 
                                                       0.001))))) 
    stop("invalid dependent variable, non-integer values")
  Y <- as.integer(round(Y + 0.001))
  if (any(Y < 0)) 
    stop("invalid dependent variable, negative counts")
  if (zero.dist == "negbin" & isTRUE(all.equal(as.vector(Z), 
                                               rep.int(Z[1], length(Z))))) 
    stop("negative binomial zero hurdle model is not identified with only an intercept")
  if (control$trace) {
    cat("dependent variable:\n")
    tab <- table(factor(Y, levels = 0:max(Y)), exclude = NULL)
    names(dimnames(tab)) <- NULL
    print(tab)
  }
  n <- length(Y)
  kx <- NCOL(X)
  kz <- NCOL(Z)
  Y0 <- Y <= 0
  Y1 <- Y > 0
  weights <- model.weights(mf)
  if (is.null(weights)) 
    weights <- 1
  if (length(weights) == 1) 
    weights <- rep.int(weights, n)
  weights <- as.vector(weights)
  names(weights) <- rownames(mf)
  offsetx <- model_offset_2(mf, terms = mtX, offset = TRUE)
  if (is.null(offsetx)) 
    offsetx <- 0
  if (length(offsetx) == 1) 
    offsetx <- rep.int(offsetx, n)
  offsetx <- as.vector(offsetx)
  offsetz <- model_offset_2(mf, terms = mtZ, offset = FALSE)
  if (is.null(offsetz)) 
    offsetz <- 0
  if (length(offsetz) == 1) 
    offsetz <- rep.int(offsetz, n)
  offsetz <- as.vector(offsetz)
  start <- control$start
  if (!is.null(start)) {
    valid <- TRUE
    if (!("count" %in% names(start))) {
      valid <- FALSE
      warning("invalid starting values, count model coefficients not specified")
      start$count <- rep.int(0, kx)
    }
    if (!("zero" %in% names(start))) {
      valid <- FALSE
      warning("invalid starting values, zero-inflation model coefficients not specified")
      start$zero <- rep.int(0, kz)
    }
    if (length(start$count) != kx) {
      valid <- FALSE
      warning("invalid starting values, wrong number of count model coefficients")
    }
    if (length(start$zero) != kz) {
      valid <- FALSE
      warning("invalid starting values, wrong number of zero-inflation model coefficients")
    }
    if (dist == "negbin" | zero.dist == "negbin") {
      if (!("theta" %in% names(start))) 
        start$theta <- c(1, 1)
      start <- list(count = start$count, zero = start$zero, 
                    theta = rep(start$theta, length.out = 2))
      if (is.null(names(start$theta))) 
        names(start$theta) <- c("count", "zero")
      if (dist != "negbin") 
        start$theta <- start$theta["zero"]
      if (zero.dist != "negbin") 
        start$theta <- start$theta["count"]
    }
    else {
      start <- list(count = start$count, zero = start$zero)
    }
    if (!valid) 
      start <- NULL
  }
  if (is.null(start)) {
    if (control$trace) 
      cat("generating starting values...")
    model_count <- glm.fit(X, Y, family = poisson(), weights = weights, 
                           offset = offsetx)
    model_zero <- switch(zero.dist, poisson = glm.fit(Z, 
                                                      Y, family = poisson(), weights = weights, offset = offsetz), 
                         negbin = glm.fit(Z, Y, family = poisson(), weights = weights, 
                                          offset = offsetz), geometric = suppressWarnings(glm.fit(Z, 
                                                                                                  factor(Y > 0), family = binomial(), weights = weights, 
                                                                                                  offset = offsetz)), binomial = suppressWarnings(glm.fit(Z, 
                                                                                                                                                          factor(Y > 0), family = binomial(link = linkstr), 
                                                                                                                                                          weights = weights, offset = offsetz)))
    start <- list(count = model_count$coefficients, zero = model_zero$coefficients)
    start$theta <- c(count = if (dist == "negbin") 1 else NULL, 
                     zero = if (zero.dist == "negbin") 1 else NULL)
    if (control$trace) 
      cat("done\n")
  }
  method <- control$method
  hessian <- control$hessian
  separate <- control$separate
  ocontrol <- control
  control$method <- control$hessian <- control$separate <- control$start <- NULL
  if (separate) {
    if (control$trace) 
      cat("calling optim() for count component estimation:\n")
    fit_count <- optim(fn = countDist, gr = countGrad, par = c(start$count, 
                                                               if (dist == "negbin") log(start$theta["count"]) else NULL), 
                       method = method, hessian = hessian, control = control)
    if (control$trace) 
      cat("calling optim() for zero hurdle component estimation:\n")
    fit_zero <- optim(fn = zeroDist, gr = zeroGrad, par = c(start$zero, 
                                                            if (zero.dist == "negbin") log(start$theta["zero"]) else NULL), 
                      method = method, hessian = hessian, control = control)
    if (control$trace) 
      cat("done\n")
    fit <- list(count = fit_count, zero = fit_zero)
    coefc <- fit_count$par[1:kx]
    coefz <- fit_zero$par[1:kz]
    theta <- c(count = if (dist == "negbin") as.vector(exp(fit_count$par[kx + 
                                                                           1])) else NULL, zero = if (zero.dist == "negbin") as.vector(exp(fit_zero$par[kz + 
                                                                                                                                                          1])) else NULL)
    vc_count <- -solve(as.matrix(fit_count$hessian))
    vc_zero <- -solve(as.matrix(fit_zero$hessian))
    SE.logtheta <- list()
    if (dist == "negbin") {
      SE.logtheta$count <- as.vector(sqrt(diag(vc_count)[kx + 
                                                           1]))
      vc_count <- vc_count[-(kx + 1), -(kx + 1), drop = FALSE]
    }
    if (zero.dist == "negbin") {
      SE.logtheta$zero <- as.vector(sqrt(diag(vc_zero)[kz + 
                                                         1]))
      vc_zero <- vc_zero[-(kz + 1), -(kz + 1), drop = FALSE]
    }
    vc <- rbind(cbind(vc_count, matrix(0, kx, kz)), cbind(matrix(0, 
                                                                 kz, kx), vc_zero))
    SE.logtheta <- unlist(SE.logtheta)
  }
  else {
    if (control$trace) 
      cat("calling optim() for joint count and zero hurlde estimation:\n")
    fit <- optim(fn = loglikfun, gr = gradfun, par = c(start$count, 
                                                       if (dist == "negbin") log(start$theta["count"]) else NULL, 
                                                       start$zero, if (zero.dist == "negbin") log(start$theta["zero"]) else NULL), 
                 method = method, hessian = hessian, control = control)
    if (fit$convergence > 0) 
      warning("optimization failed to converge")
    if (control$trace) 
      cat("done\n")
    coefc <- fit$par[1:kx]
    coefz <- fit$par[(kx + (dist == "negbin") + 1):(kx + 
                                                      kz + (dist == "negbin"))]
    vc <- -solve(as.matrix(fit$hessian))
    np <- c(if (dist == "negbin") kx + 1 else NULL, 
            if (zero.dist == "negbin") kx + kz + 1 + (dist == 
                                                        "negbin") else NULL)
    if (length(np) > 0) {
      theta <- as.vector(exp(fit$par[np]))
      SE.logtheta <- as.vector(sqrt(diag(vc)[np]))
      names(theta) <- names(SE.logtheta) <- c(if (dist == 
                                                  "negbin") "count" else NULL, if (zero.dist == 
                                                                                   "negbin") "zero" else NULL)
      vc <- vc[-np, -np, drop = FALSE]
    }
    else {
      theta <- NULL
      SE.logtheta <- NULL
    }
  }
  names(coefc) <- names(start$count) <- colnames(X)
  names(coefz) <- names(start$zero) <- colnames(Z)
  colnames(vc) <- rownames(vc) <- c(paste("count", colnames(X), 
                                          sep = "_"), paste("zero", colnames(Z), sep = "_"))
  phi <- if (zero.dist == "binomial") 
    linkinv(Z %*% coefz + offsetz)[, 1]
  else exp(Z %*% coefz + offsetz)[, 1]
  p0_zero <- switch(zero.dist, binomial = log(phi), poisson = ppois(0, 
                                                                    lambda = phi, lower.tail = FALSE, log.p = TRUE), negbin = pnbinom(0, 
                                                                                                                                      size = theta["zero"], mu = phi, lower.tail = FALSE, 
                                                                                                                                      log.p = TRUE), geometric = pnbinom(0, size = 1, mu = phi, 
                                                                                                                                                                         lower.tail = FALSE, log.p = TRUE))
  mu <- exp(X %*% coefc + offsetx)[, 1]
  p0_count <- switch(dist, poisson = ppois(0, lambda = mu, 
                                           lower.tail = FALSE, log.p = TRUE), negbin = pnbinom(0, 
                                                                                               size = theta["count"], mu = mu, lower.tail = FALSE, 
                                                                                               log.p = TRUE), geometric = pnbinom(0, size = 1, mu = mu, 
                                                                                                                                  lower.tail = FALSE, log.p = TRUE))
  Yhat <- exp((p0_zero - p0_count) + log(mu))
  res <- sqrt(weights) * (Y - Yhat)
  nobs <- sum(weights > 0)
  rval <- list(coefficients = list(count = coefc, zero = coefz), 
               residuals = res, fitted.values = Yhat, optim = fit, method = method, 
               control = control, start = start, weights = if (identical(as.vector(weights), 
                                                                         rep.int(1L, n))) NULL else weights, offset = list(count = if (identical(offsetx, 
                                                                                                                                                 rep.int(0, n))) NULL else offsetx, zero = if (identical(offsetz, 
                                                                                                                                                                                                         rep.int(0, n))) NULL else offsetz), n = nobs, df.null = nobs - 
                 2, df.residual = nobs - (kx + kz + (dist == "negbin") + 
                                            (zero.dist == "negbin")), terms = list(count = mtX, 
                                                                                   zero = mtZ, full = mt), theta = theta, SE.logtheta = SE.logtheta, 
               loglik = if (separate) fit_count$value + fit_zero$value else fit$value, 
               vcov = vc, dist = list(count = dist, zero = zero.dist), 
               link = if (zero.dist == "binomial") linkstr else NULL, 
               linkinv = if (zero.dist == "binomial") linkinv else NULL, 
               separate = separate, converged = if (separate) fit_count$convergence < 
                 1 & fit_zero$convergence < 1 else fit$convergence < 
                 1, call = cl, formula = ff, levels = .getXlevels(mt, 
                                                                  mf), contrasts = list(count = attr(X, "contrasts"), 
                                                                                        zero = attr(Z, "contrasts")))
  if (model) 
    rval$model <- mf
  if (y) 
    rval$y <- Y
  if (x) 
    rval$x <- list(count = X, zero = Z)
  class(rval) <- "hurdle"
  return(rval)
}
model_offset_2 <- function(x, terms = NULL, offset = TRUE)
  ## allow optionally different terms
  ## potentially exclude "(offset)"
{
  if(is.null(terms)) terms <- attr(x, "terms")
  offsets <- attr(terms, "offset")
  if(length(offsets) > 0) {
    ans <- if(offset) x$"(offset)" else NULL
    if(is.null(ans)) ans <- 0
    for(i in offsets) ans <- ans + x[[deparse(attr(terms, "variables")[[i + 1]])]]
    ans
  }
  else {
    ans <- if(offset) x$"(offset)" else NULL
  }
  if(!is.null(ans) && !is.numeric(ans)) stop("'offset' must be numeric")
  ans
}
my.h <- my.hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin", zero.dist = "binomial")


#------------------------------------------------------------------------------------------------------------#
##### Continue with the initial hurdle model for adults----

datos  <- data[data$groupsize=="exploitable",]                 ##---when Number=0, LngtClas is set as 0
dat <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulNo + ShootLong + ShootLat + Depth, FUN=sum, data=datos)
dim(dat);str(dat)

# Check how many zeroes
table(dat$CPUE>0) # bit more than 50%

# Round off CPUE values to make sure it's count data
dat$CPUE <- round(dat$CPUE,0)
# dat$Year <- as.factor(dat$Year)
str(dat)


### Hurdle model: gives warning!
hurdle2=hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin", zero.dist = "binomial")
# aic2=AIC(hurdle2)
summary(hurdle2) # this gives a warning!!
vcov <- data.frame(hurdle2$vcov)
# Calculate dispersion statistic
chi2 <- sum(resid(hurdle2)^2)
df <- hurdle2$df.residual
dis.st <- chi2/df # 1.09. with Poisson: 34, so much higher and not good in dealing with overdispersion

# Weights parameters to multiply the fitted values. These weighting factors consider  both the survey area covered and the wing spread.
weightCGFS=0.24
weightIBTS=0.76

dat$weight=ifelse(dat$Survey=="CGFS",weightCGFS,weightIBTS)

# Multiply the model prediction by the weighting factors
dat$fit=predict(hurdle2)*dat$weight

# Estimate the index for the CGFS survey
dat_CGFS=dat[dat$Survey=="CGFS",]
survpred_CGFS=ddply(dat_CGFS,.(Year),summarize,idx=mean(fit))
survpred_CGFS$Year=as.integer(as.character(survpred_CGFS$Year))

# Estimate the index for the IBTS survey
dat_IBTS=dat[dat$Survey=="NS-IBTS",]
survpred_IBTS=ddply(dat_IBTS,.(Year),summarize,idx=mean(fit))
survpred_IBTS$Year=as.integer(as.character(survpred_IBTS$Year))


large=data.frame(year=survpred_CGFS$Year, cgfs=survpred_CGFS$idx, ibts=survpred_IBTS$idx)

# Estimate the combined index as the average of the two survey indexes
large$all=rowMeans(large[,!names(large)=="year"])


write.csv(large,paste(resultpath,"model_fit_large_nshm.csv",sep=""),row.names=F)

### Plot average and two survey indices ----
png(filename=paste(figPath,"hurdle_model_adults.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
par(mar=c(5,5,3,1))
plot(large$year,large$all,type="b",pch=19,col="blue",cex=1,lwd=1.5,ylim=c(0,max(large[,c(2:4)])),xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list(">20cm substock",cex=2),bty="l")
lines(large$year,large$cgfs,type="b",pch=19,col="grey40",cex=1,lwd=1.5)
lines(large$year,large$ibts,type="b",pch=19,col="red",cex=1,lwd=1.5)
legend("topright",legend=c("Average", "CGFS", "IBTS"),col=c("blue","grey40","red"),pch=19,bty="n",cex=1.5)
dev.off()


png(filename=paste(figPath,"hurdle_model_adults_oldColours.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
par(mar=c(5,5,3,1))
plot(large$year,large$all,type="b",pch=19,col="grey40",cex=1,lwd=1.5,ylim=c(0,max(large[,c(2:4)])),xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list(">20cm substock",cex=2),bty="l")
lines(large$year,large$cgfs,type="b",pch=19,col="red",cex=1,lwd=1.5)
lines(large$year,large$ibts,type="b",pch=19,col="blue",cex=1,lwd=1.5)
legend("topright",legend=c("all surveys", "CGFS", "IBTS"),col=c("grey40","red","blue"),pch=19,bty="n",cex=1.5)
dev.off()


### Plot joint survey index with bootstrapped confidence interval ----
model=hurdle2
choice= "Year x Survey & Year + Survey"

# Bootstrap
fit <- predict(model)
Pearson <- resid(model, type = "pearson")
VarComp <- resid(model, type = "response") / Pearson

Year=dat$Year
Survey=dat$Survey
depthrange=dat$depthrange


# Weights parameters to multiply the fitted values. These weighting factors consider  both the survey area covered and the wing spread.
areaCGFS=0.24
areaIBTS=0.76
dat$percarea=ifelse(dat$Survey=="CGFS",areaCGFS,areaIBTS)

# Model prediction with confidence intervals
boot_idx_adult_2 <- replicate(999, {
  CPUE <- pmax(round(fit + sample(Pearson) * VarComp, 0), 0)
  predict(hurdle(CPUE ~ Year * Survey | Year + Survey, link="logit", dist="negbin"), newdata = dat)*dat$percarea
})

boot_idx_adult=cbind(dat,boot_idx_adult_2)

write.csv(boot_idx_adult,paste(resultpath,"adult_boot_idx_",choice,".csv",sep=""),row.names=F)

newdata0 <- dat
newdata0$fit <- predict(model, newdata = dat, type = "response")*dat$percarea
newdata0[, (dim(newdata0)[2]+1):(dim(newdata0)[2]+2)] <- t(apply(boot_idx_adult_2, 1, quantile, c(0.025, 0.975)))
colnames(newdata0)=c("Survey",  "Year", "StatRec",  "HaulDur",  "HaulNo", "ShootLon", "ShootLat", "Depth",  "CPUE","percarea", "fit", "Perclow","Perchigh")
newdata0$model <- choice

# Estimate the index for the CGFS survey
dat_CGFS=newdata0[newdata0$Survey=="CGFS",]
survpred_CGFS=ddply(dat_CGFS,.(Year),summarize,idx=mean(fit),CI_low=mean(Perclow),CI_high=mean(Perchigh))
survpred_CGFS$Year=as.integer(as.character(survpred_CGFS$Year))

# Estimate the index for the IBTS survey
dat_IBTS=newdata0[newdata0$Survey=="NS-IBTS",]
survpred_IBTS=ddply(dat_IBTS,.(Year),summarize,idx=mean(fit),CI_low=mean(Perclow),CI_high=mean(Perchigh))
survpred_IBTS$Year=as.integer(as.character(survpred_IBTS$Year))

large=data.frame(year=survpred_CGFS$Year, cgfs=survpred_CGFS$idx, ibts=survpred_IBTS$idx, cgfs_low=survpred_CGFS$CI_low, ibts_low=survpred_IBTS$CI_low,cgfs_high=survpred_CGFS$CI_high, ibts_high=survpred_IBTS$CI_high)

# Estimate the combined index as the average of the two survey indexes
large$all=rowMeans(large[,names(large) %in% c("cgfs","ibts")])
large$all_low=rowMeans(large[,names(large) %in% c("cgfs_low","ibts_low")])
large$all_high=rowMeans(large[,names(large) %in% c("cgfs_high","ibts_high")])

write.csv(large,paste(resultpath,"adult_surv_idx_",choice,".csv",sep=""),row.names=F)

large_all=large[,names(large) %in% c("all","all_low","all_high")]
ymax=max(large_all)
ymin=min(large_all)

png(filename=paste(figPath,"adult_surv_idx_boot_",choice,".png",sep=""), heigh=1500, width=2500, units="px", pointsize=8, res=450)
plot(large$year,large$all,type="b",pch=19,col="grey40",ylim=c(ymin,ymax),
     xlab="Year", ylab="Survey index (indv/hour)",cex.lab=1.3,
     main=paste("Hurdle model >",Cutoff_Lngt,"cm  ",choice,sep=""))
polygon(c(sort(unique(large$year)),rev(sort(unique(large$year)))), 
        c(large$all_high,rev(large$all_low)), lty=0, col=alpha(2, 0.5))
dev.off()


#######################################################-
####  Smaller than the defined cutoff length  #########
#######################################################-

datos  <- data[data$groupsize=="juveniles",]
dat <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulNo + ShootLong + ShootLat + Depth, FUN=sum, data=datos)
dim(dat)

# Check how many zeroes
table(dat$CPUE>0) # bit more than 50%

# Round off CPUE values to make sure it's count data
dat$CPUE <- round(dat$CPUE,0)
# dat$Year <- as.factor(dat$Year)
str(dat)


# Models
hurdle.juv=hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin", zero.dist = "binomial")
summary(hurdle.juv)


# Weights parameters to multiply the fitted values. These weighting factors consider  both the survey area covered and the wing spread.
weightCGFS=0.24
weightIBTS=0.76

dat$weight=ifelse(dat$Survey=="CGFS",weightCGFS,weightIBTS)

# Multiply the model prediction by the weighting factors
dat$fit=predict(hurdle.juv)*dat$weight

# Estimate the index for the CGFS survey
dat_CGFS=dat[dat$Survey=="CGFS",]
survpred_CGFS=ddply(dat_CGFS,.(Year),summarize,idx=mean(fit))
survpred_CGFS$Year=as.integer(as.character(survpred_CGFS$Year))

# Estimate the index for the IBTS survey
dat_IBTS=dat[dat$Survey=="NS-IBTS",]
survpred_IBTS=ddply(dat_IBTS,.(Year),summarize,idx=mean(fit))
survpred_IBTS$Year=as.integer(as.character(survpred_IBTS$Year))

small=data.frame(year=survpred_CGFS$Year, cgfs=survpred_CGFS$idx, ibts=survpred_IBTS$idx)

# Estimate the combined index as the average of the two survey indexes
small$all=rowMeans(small[,!names(small)=="year"])


write.csv(small,paste(resultpath,"model_fit_small_nshm.csv",sep=""),row.names=F)


### Plot average and two survey indices ----
png(filename=paste(figPath,"hurdle_model_juveniles.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
par(mar=c(5,5,3,1))
plot(small$year,small$all,type="b",pch=19,col="blue",cex=1,lwd=1.5,ylim=c(0,max(small[,c(2:4)])),xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list("<20cm substock",cex=2),bty="l")
lines(small$year,small$cgfs,type="b",pch=19,col="grey40",cex=1,lwd=1.5)
lines(small$year,small$ibts,type="b",pch=19,col="red",cex=1,lwd=1.5)
legend("topright",legend=c("Average", "CGFS", "IBTS"),col=c("blue","grey40","red"),pch=19,bty="n",cex=1.5)
dev.off()

png(filename=paste(figPath,"hurdle_model_juveniles_oldColours.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
par(mar=c(5,5,3,1))
plot(small$year,small$all,type="b",pch=19,col="grey40",cex=1,lwd=1.5,ylim=c(0,max(small[,c(2:4)])),xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list("<20cm substock",cex=2),bty="l")
lines(small$year,small$cgfs,type="b",pch=19,col="red",cex=1,lwd=1.5)
lines(small$year,small$ibts,type="b",pch=19,col="blue",cex=1,lwd=1.5)
legend("topright",legend=c("all surveys", "CGFS", "IBTS"),col=c("grey40","red","blue"),pch=19,bty="n",cex=1.5)
dev.off()



### Plot joint survey index with bootstrapped confidence interval ----
model=hurdle2
choice= "Year x Survey & Year + Survey"

# Bootstrap
fit <- predict(model)
Pearson <- resid(model, type = "pearson")
VarComp <- resid(model, type = "response") / Pearson

Year=dat$Year
Survey=dat$Survey

# Weights parameters to multiply the fitted values. These weighting factors consider  both the survey area covered and the wing spread.
areaCGFS=0.24
areaIBTS=0.76
dat$percarea=ifelse(dat$Survey=="CGFS",areaCGFS,areaIBTS)

# Model prediction with confidence intervals
boot_idx_juv_2 <- replicate(999, {
  CPUE <- pmax(round(fit + sample(Pearson) * VarComp, 0), 0)
  predict(hurdle(CPUE ~ Year * Survey | Year + Survey, link="logit", dist="negbin"), newdata = dat)*dat$percarea
})


boot_idx_juv=cbind(dat,boot_idx_juv_2)

write.csv(boot_idx_juv,paste(resultpath,"juvenile_boot_surv_idx_",choice,".csv",sep=""),row.names=F)

newdata0 <- dat
newdata0$fit <- predict(model, newdata = dat, type = "response")*dat$percarea
newdata0[, (dim(newdata0)[2]+1):(dim(newdata0)[2]+2)] <- t(apply(boot_idx_juv_2, 1, quantile, c(0.025, 0.975)))
colnames(newdata0)=c("Survey",  "Year", "StatRec",  "HaulDur",  "HaulNo", "ShootLon", "ShootLat", "Depth",  "CPUE","percarea", "fit", "Perclow","Perchigh")
newdata0$model <- choice

# Estimate the index for the CGFS survey
dat_CGFS=newdata0[newdata0$Survey=="CGFS",]
survpred_CGFS=ddply(dat_CGFS,.(Year),summarize,idx=mean(fit),CI_low=mean(Perclow),CI_high=mean(Perchigh))
survpred_CGFS$Year=as.integer(as.character(survpred_CGFS$Year))

# Estimate the index for the IBTS survey
dat_IBTS=newdata0[newdata0$Survey=="NS-IBTS",]
survpred_IBTS=ddply(dat_IBTS,.(Year),summarize,idx=mean(fit),CI_low=mean(Perclow),CI_high=mean(Perchigh))
survpred_IBTS$Year=as.integer(as.character(survpred_IBTS$Year))

small=data.frame(year=survpred_CGFS$Year, cgfs=survpred_CGFS$idx, ibts=survpred_IBTS$idx, cgfs_low=survpred_CGFS$CI_low, ibts_low=survpred_IBTS$CI_low,cgfs_high=survpred_CGFS$CI_high, ibts_high=survpred_IBTS$CI_high)


# Estimate the combined index as the average of the two survey indexes
small$all=rowMeans(small[,names(small) %in% c("cgfs","ibts")])
small$all_low=rowMeans(small[,names(small) %in% c("cgfs_low","ibts_low")])
small$all_high=rowMeans(small[,names(small) %in% c("cgfs_high","ibts_high")])

write.csv(small,paste(resultpath,"juvenile_surv_idx_",choice,".csv",sep=""),row.names=F)

small_all=small[,names(small) %in% c("all","all_low","all_high")]
ymax=max(small_all)
ymin=min(small_all)

png(filename=paste(figPath,"juvenile_surv_idx_boot_",choice,".png",sep=""), heigh=1500, width=2500, units="px", pointsize=8, res=450)
plot(small$year,small$all,type="b",pch=19,col="grey40",ylim=c(ymin,ymax),xlab="Year", ylab="Survey index (indv/hour)",cex.lab=1.3,main=paste("Hurdle model <",Cutoff_Lngt,"cm  ",choice,sep=""))
polygon(c(sort(unique(small$year)),rev(sort(unique(small$year)))), c(small$all_high,rev(small$all_low)), lty=0, col=alpha(2, 0.5))
dev.off()
