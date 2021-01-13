require(stargazer)
require(dplyr)
require(data.table)
require(gsubfn)

p = 1e3
q = 5e0

s.mat1 = diag(1, p, p) # design 1
s.mat2 = 0.5 ^ abs(outer(1:p, 1:p, "-")) # design 2
s.mat3 = matrix(0.5, p, p)
diag(s.mat3) = 1  # design 3

p.vec1 = c(1, 1) / 2 # balanced
p.vec2 = c(2, 1) / 3 # unbalanced

s.list = list(s.mat1, s.mat2)
p.list = list(p.vec1, p.vec2)
res <- NULL

for (pq in c('p.1000.q.5', 'p.2000.q.10')) {
  for (s.mat in s.list) {
    for (p.vec in p.list) {
      file.name <-
        fn$identity(
          "./Dropbox/Multiclass C LASSO/Simulation_jh/2020 0710 JH/`pq`/s.mat`s.mat[1, 3]`_prob`round(p.vec[1],digits=2)`_sim.valid.Rdata"
        )
      load(file.name)
      message(file.name)
      print(b.vec[1])
      sim.result <- as.data.table(sim.result)
      sim.result$m <-
        c(rep('err', 3), rep('sen', 3), rep('spc', 3), rep('acc', 3))
      tmp <-
        cbind(s.mat = s.mat[1, 3],
              prob = round(p.vec[1], digits = 2),
              sim.result)
      res <- rbind(res, tmp)
    }
  }
}

colnames(res)[6:25] <-
  c(paste('c', colnames(res)[6:15], sep = '.'),
    paste('r', colnames(res)[16:25], sep = '.'))
sw <- c(ncol(res), 1:(ncol(res) - 1))
res <- res[, ..sw]
fin = list(
  acc = res[m == 'acc'],
  err = res[m == 'err'],
  sen = res[m == 'sen'],
  spc = res[m == 'spc']
)

for (i in 1:length(fin)) {
  fin[[i]] <- fin[[i]] %>% mutate_at(vars(p:r.Mcp3), function(x) {
    if (fin[[i]]$m[1] == 'err') {
      return(round(as.numeric(x), 4))
    }
    as.numeric(x)
  }) %>% as.data.table()
}

fin$err[, `:=`(c.Bayes = .2, r.Bayes = .2)]
err <- fin$err[s.mat == .25] %>% t %>% .[c(2:6, 1, 17:26), ]
sen <- fin$sen[s.mat == .25] %>% t %>% .[c(1, 17:26), ]
spc <- fin$spc[s.mat == .25] %>% t %>% .[c(1, 17:26), ]
acc <- fin$acc[s.mat == .25] %>% t %>% .[c(1, 17:26), ]

fin.t <- rbind(err, sen, spc, acc)
fin.t <- cbind(1, fin.t)
fin.t[, 1] <-
  c(rownames(fin.t)[1:5],
    rep(
      c(
        'm',
        'Bayes',
        'Lasso',
        'cLasso1',
        'cLasso2',
        'cLasso3',
        'cLasso4',
        'cLasso5',
        'Mcp1',
        'Mcp2',
        'Mcp3'
      ),
      4
    ))
fin.t %>% stargazer(out = 'Dropbox/Multiclass C LASSO/Text/sim.tables/stargazer.tex',
                    summary = F,
                    rownames = F)