rm(list = ls())
pcks <- c('MASS', 'ncpen', 'ncvreg', 'glmnet', 'gsubfn', 'data.table', 'foreach', 'doParallel')
pcks <- sapply(pcks, require, character.only = 1)
if (any(! pcks)) install.packages(names(pcks)[! pcks])
source("lda.ncpen.R")
sam.gen.fun = function(n, p.vec, b.vec, s.mat, cs.mat) {
    c.vec = sample(c(1, 2), n, replace = TRUE, prob = p.vec)
    n1 = sum(c.vec == 1)
    n2 = sum(c.vec == 2)
    p = length(b.vec)
    x1.mat = matrix(rnorm(n1 * p), nrow = n1) %*% cs.mat
    x2.mat = matrix(rnorm(n2 * p), nrow = n2) %*% cs.mat
    x2.mat = sweep(x2.mat, 2, drop(s.mat %*% b.vec), "+")
    x.mat = rbind(x1.mat, x2.mat)
    c.vec = c(rep(1, n1), rep(2, n2))
    return(list(c.vec = c.vec, x.mat = x.mat))
}
cv.index.fun = function(y.vec, k.val = 10) {
    n = length(y.vec)
    m = k.val * trunc(n/k.val)
    o.vec = order(y.vec, decreasing = T)
    a.vec = o.vec[1:m]
    r.vec = o.vec[-(1:m)]
    o.mat = matrix(a.vec, nrow = k.val)
    s.mat = apply(o.mat, 2, FUN = sample)
    o.vec[s.mat] = row(s.mat)
    o.vec[r.vec] = sample(1:k.val, length(r.vec))
    return(id.vec = o.vec)
}
err.fun = function(c.vec, y.vec, x.mat, b.mat, m1.vec, m2.vec, 
    ps.mat, nr) {
    c.err = NULL
    for (bid in 1:ncol(b.mat)) {
        b.vec = b.mat[-1, bid]
        sgn = drop(t(m2.vec - m1.vec) %*% b.vec)
        if (sum(b.vec != 0) == 0) {
            c.err = c(c.err, min(sum(c.vec == 2), sum(c.vec == 
              1))/length(c.vec))
        }
        else {
            c.err = c(c.err, mean((c.vec == 2) != (sign(sgn) * 
              (x.mat %*% b.vec - sum((m2.vec + m1.vec) * 
                b.vec)/2 + drop(t(b.vec) %*% ps.mat %*% b.vec)/sgn * 
                log(nr)) > 0)))
        }
    }
    r.err = colMeans((y.vec - cbind(1, x.mat) %*% b.mat)^2)
    return(list(c.err = c.err, r.err = r.err))
}
bayes.fun = function(p.vec, b.vec, s.mat) {
    pi1 <- p.vec[1]
    pi2 <- p.vec[2]
    bsb <- t(b.vec) %*% s.mat %*% b.vec
    loc <- log(pi2/pi1)
    b.err <- pnorm(0, (loc + bsb/2), sqrt(bsb)) * pi2 + (1 - 
        pnorm(0, (loc - bsb/2), sqrt(bsb))) * pi1
    return(b.err)
}
con.bayes.fun = function(p.vec, b.vec, s.mat, err = 0.2) {
    a = 0
    b = 100
    for (iter in 1:1000) {
        c = (a + b)/2
        a_bayes = bayes.fun(p.vec, a * b.vec, s.mat) - err
        c_bayes = bayes.fun(p.vec, c * b.vec, s.mat) - err
        if (a_bayes * c_bayes > 0) {
            a = c
        }
        else {
            b = c
        }
        if (abs(a_bayes - c_bayes) < 1e-07) 
            break
    }
    return(a * b.vec)
}
do.sim <- function(viz) {
    opt.lam.mat = err.mat = acc.mat = sen.mat = spc.mat = NULL
    for (n in n.vec) {
        err = acc = sen = spc = NULL
        pb <- txtProgressBar(1, n.sim, style = 3)
        for (sim in 1:n.sim) {
            if (viz) 
              print(c(p = dm, q = q, n = n, sim = sim))
            sam = sam.gen.fun(4 * n, p.vec, b.vec, s.mat, 
              cs.mat)
            cv.id = cv.index.fun(sam$c.vec, k.val = 4)
            c.vec = sam$c.vec[cv.id == 1]
            n1 = sum(c.vec == 1)
            n2 = sum(c.vec == 2)
            nr = n2/n1
            y.vec = rep(-n/n1, length(c.vec))
            y.vec[c.vec == 2] = n/n2
            x.mat = sam$x.mat[cv.id == 1, ]
            x1.mat = x.mat[c.vec == 1, ]
            x2.mat = x.mat[c.vec == 2, ]
            m1.vec = colMeans(x1.mat)
            m2.vec = colMeans(x2.mat)
            s1.mat = var(x1.mat)
            s2.mat = var(x2.mat)
            ps.mat = ((n1 - 1) * s1.mat + (n2 - 1) * s2.mat)/(n1 + 
              n2 - 2)
            vc.vec = sam$c.vec[cv.id == 2]
            vx.mat = sam$x.mat[cv.id == 2, ]
            vy.vec = rep(max(y.vec), length(vc.vec))
            vy.vec[vc.vec == 1] = min(y.vec)
            nc.vec = sam$c.vec[cv.id > 2]
            nx.mat = sam$x.mat[cv.id > 2, ]
            ny.vec = rep(max(y.vec), length(nc.vec))
            ny.vec[nc.vec == 1] = min(y.vec)
            cb.mat = c(0, b.vec)
            rb.mat = c(0, b.vec)
            lam.max = max(abs(-t(x.mat) %*% (y.vec - mean(y.vec))/n))
            lam.vec = exp(seq(log(lam.max), log(lam.max/100), 
              length.out = 300))
            
            fit = ncpen(y.vec, x.mat, family = "gaussian", 
              penalty = "lasso", x.standardize = F, intercept = T, 
              lambda = lam.vec, df.max = n)
            verr = err.fun(vc.vec, vy.vec, vx.mat, coef(fit), 
              m1.vec, m2.vec, ps.mat, nr)
            c.opt = which.min(verr$c.err)
            c.gam = fit$lambda[c.opt]
            r.opt = which.min(verr$r.err)
            r.gam = fit$lambda[r.opt]
            
            if (viz) {
              par(mfrow = c(1, 2))
              plot(verr$r.err/max(verr$r.err), ylim = c(0, 
                1), main = "LASSO")
              lines(verr$c.err)
              abline(v = c.opt)
              abline(v = r.opt)
            }
            cb.mat = cbind(cb.mat, coef(fit)[, c.opt])
            cdf.max = sum(cb.mat[, 2] != 0)
            rb.mat = cbind(rb.mat, coef(fit)[, r.opt])
            rdf.max = sum(rb.mat[, 2] != 0)
            
            fit = ncpen(y.vec, x.mat, family = "gaussian", tau = 1.0001,
              penalty = "mcp", x.standardize = F, intercept = T, 
              df.max = max(cdf.max, rdf.max) + 30)
            verr = err.fun(vc.vec, vy.vec, vx.mat, coef(fit), 
              m1.vec, m2.vec, ps.mat, nr)
            c.opt = which.min(verr$c.err)
            r.opt = which.min(verr$r.err)
            if (viz) {
              plot(verr$c.err, main = fn$identity("SCAD.c"))
              abline(v = c.opt)
              plot(verr$r.err, main = fn$identity("SCAD.r"))
              abline(v = r.opt)
            }
            cb.mat = cbind(cb.mat, coef(fit)[, c.opt])
            rb.mat = cbind(rb.mat, coef(fit)[, r.opt])
            
            fit = ncpen(y.vec, x.mat, family = "gaussian", tau = 2.0001,
              penalty = "scad", x.standardize = F, intercept = T, 
              df.max = max(cdf.max, rdf.max) + 30)
            verr = err.fun(vc.vec, vy.vec, vx.mat, coef(fit), 
              m1.vec, m2.vec, ps.mat, nr)
            c.opt = which.min(verr$c.err)
            r.opt = which.min(verr$r.err)
            if (viz) {
              plot(verr$c.err, main = fn$identity("SCAD.c"))
              abline(v = c.opt)
              plot(verr$r.err, main = fn$identity("SCAD.r"))
              abline(v = r.opt)
            }
            cb.mat = cbind(cb.mat, coef(fit)[, c.opt])
            rb.mat = cbind(rb.mat, coef(fit)[, r.opt])
            
            fit = ncpen(y.vec, x.mat, family = "gaussian", 
              penalty = "tlp", x.standardize = F, intercept = T, 
              df.max = max(cdf.max, rdf.max) + 30)
            verr = err.fun(vc.vec, vy.vec, vx.mat, coef(fit), 
              m1.vec, m2.vec, ps.mat, nr)
            c.opt = which.min(verr$c.err)
            r.opt = which.min(verr$r.err)
            if (viz) {
              plot(verr$c.err, main = fn$identity("TLP.c"))
              abline(v = c.opt)
              plot(verr$r.err, main = fn$identity("TLP.r"))
              abline(v = r.opt)
            }
            cb.mat = cbind(cb.mat, coef(fit)[, c.opt])
            rb.mat = cbind(rb.mat, coef(fit)[, r.opt])
            
            fit = ncpen(y.vec, x.mat, family = "gaussian", 
              penalty = "mlog", x.standardize = F, intercept = T, 
              df.max = max(cdf.max, rdf.max) + 30)
            verr = err.fun(vc.vec, vy.vec, vx.mat, coef(fit), 
              m1.vec, m2.vec, ps.mat, nr)
            c.opt = which.min(verr$c.err)
            r.opt = which.min(verr$r.err)
            if (viz) {
              plot(verr$c.err, main = fn$identity("Mlog.c"))
              abline(v = c.opt)
              plot(verr$r.err, main = fn$identity("Mlog.r"))
              abline(v = r.opt)
            }
            cb.mat = cbind(cb.mat, coef(fit)[, c.opt])
            rb.mat = cbind(rb.mat, coef(fit)[, r.opt])
            
            fit = ncpen(y.vec, x.mat, family = "gaussian", 
              penalty = "mbridge", x.standardize = F, intercept = T, 
              df.max = max(cdf.max, rdf.max) + 30)
            verr = err.fun(vc.vec, vy.vec, vx.mat, coef(fit), 
              m1.vec, m2.vec, ps.mat, nr)
            c.opt = which.min(verr$c.err)
            r.opt = which.min(verr$r.err)
            if (viz) {
              plot(verr$c.err, main = fn$identity("Mbridge.c"))
              abline(v = c.opt)
              plot(verr$r.err, main = fn$identity("Mbridge.r"))
              abline(v = r.opt)
            }
            cb.mat = cbind(cb.mat, coef(fit)[, c.opt])
            rb.mat = cbind(rb.mat, coef(fit)[, r.opt])
            
            b.mat = cbind(cb.mat, rb.mat)
            ai = which(b.mat != 0, arr.ind = 1)
            err.a = err.fun(nc.vec, ny.vec, nx.mat, b.mat, 
              m1.vec, m2.vec, ps.mat, nr)$c.err
            sen.a = colSums(b.mat[-1, ][b.vec != 0, ] != 
              0)
            spc.a = colSums(b.mat[-1, ][b.vec == 0, ] != 
              0)
            acc.a = (colSums(b.mat[-1, ][1:q, ] != 0) + colSums(b.mat[-1, 
              ][-(1:q), ] == 0)) == dm
            err = rbind(err, err.a)
            sen = rbind(sen, sen.a)
            spc = rbind(spc, spc.a)
            acc = rbind(acc, acc.a)
            opt.lam.mat = rbind(opt.lam.mat, c(p = dm, q = q, n = n, c.opt.lam = c.gam, r.opt.lam = r.gam))
            tem = round(rbind(colMeans(err, na.rm = T), colMeans(sen, 
              na.rm = T), colMeans(spc, na.rm = T), colMeans(acc, 
              na.rm = T)), digits = 3)
            c.name = c("Bayes", "Lasso", "MCP", "SCAD", "TLP", "Mlog", 'Mbridge')
            colnames(tem) = rep(c.name, 2)
            rownames(tem) = c("err", "sen", "spc", "acc")
            if (viz) 
              print(tem)
            setTxtProgressBar(pb, sim)
#             b.mat = sparseMatrix(ai[,'row'], ai[,'col'], x = b.mat[ai])
        } # sim
        close(pb)
        err.mat = rbind(err.mat, c(p = dm, q = q, n = n, colMeans(err, 
            na.rm = T)))
        sen.mat = rbind(sen.mat, c(p = dm, q = q, n = n, colMeans(sen, 
            na.rm = T)))
        spc.mat = rbind(spc.mat, c(p = dm, q = q, n = n, colMeans(spc, 
            na.rm = T)))
        acc.mat = rbind(acc.mat, c(p = dm, q = q, n = n, colMeans(acc, 
            na.rm = T)))
        tem = rbind(err.mat, sen.mat, spc.mat, acc.mat)
        colnames(tem)[4:ncol(tem)] = rep(c.name, 2)
        print(round(tem, digits = 3))
    } # n
    return(
    list(res = cbind(tem, m = c("err", "sen", "spc", "acc")),
        b.mat = b.mat,
        lambdas = opt.lam.mat))
}