### version 2.0 tasks: 1) many functions 2) piter, siter, qiter, aiter 3) parameter controll 
### version 2.0 tasks: 1) cox,mult narrow lambda grid 
### version 2.0 tasks: 1) mult x.mat conversion function; y.vec label 
### version 2.0 tasks: 1) tun.fun return df.max, initial solution, df.max ...
### version 2.0 tasks: 1) one-step estimator tun.fun return df.max, initial solution, df.max ...
### version 2.0 tasks: 1) fold id manual .. cv.ncpen
### later: add control, lam.min control 
### later: ridge lambda control 


  get.pen.fun = function(pen){
    if(pen=="lasso"){ 
      pen.fun = function(b.vec,lam,gam,tau){ return(lam*abs(b.vec)) }
      pen.grad.fun = function(b.vec,lam,gam,tau){ return(lam*sign(b.vec)) }
    }
    if(pen=="scad"){
      pen.fun = function(b.vec,lam,gam,tau){
        ab.vec = abs(b.vec); tem0.vec = (lam*ab.vec)*(ab.vec<lam)
        tem1.vec = ((tau*lam*(ab.vec-lam)-(ab.vec^2-lam^2)/2)/(tau-1)+lam^2)*(ab.vec>=lam)*(ab.vec<tau*lam)
        tem2.vec = ((tau+1)*lam^2/2)*(ab.vec>=tau*lam)
        return(tem0.vec+tem1.vec+tem2.vec)
      }
      pen.grad.fun = function(b.vec,lam,gam,tau){
        ab.vec = abs(b.vec); sb.vec = sign(b.vec);
        tem0.vec = (lam)*(ab.vec<lam); tem1.vec = ((tau*lam-ab.vec)/(tau-1))*(ab.vec>=lam)*(ab.vec<tau*lam)
        return((tem0.vec+tem1.vec)*sb.vec)
      }
    }
    if(pen=="mcp"){
      pen.fun = function(b.vec,lam,gam,tau){
        ab.vec = abs(b.vec); tem0.vec = (lam*ab.vec-ab.vec^2/2/tau)*(ab.vec<tau*lam); 
        tem1.vec = (tau*lam^2/2)*(ab.vec>=tau*lam); return(tem0.vec+tem1.vec)
      }
      pen.grad.fun = function(b.vec,lam,gam,tau){
        ab.vec = abs(b.vec); sb.vec = sign(b.vec); tem0.vec = (lam-ab.vec/tau)*(ab.vec<tau*lam)
        return(tem0.vec*sb.vec)
      }
    }
    if(pen=="tlp"){
      pen.fun = function(b.vec,lam,gam,tau){
        ab.vec = abs(b.vec); tem0.vec = (lam*ab.vec)*(ab.vec<tau); tem1.vec = (lam*tau)*(ab.vec>=tau)
        return(tem0.vec+tem1.vec)
      }
      pen.grad.fun = function(b.vec,lam,gam,tau){
        ab.vec = abs(b.vec); sb.vec = sign(b.vec); tem0.vec = (lam)*(ab.vec<tau)
        return(tem0.vec*sb.vec)
      }
    }
    if(pen=="classo"){
      pen.fun = function(b.vec,lam,gam,tau){
        ab.vec = abs(b.vec); tem0.vec = (-ab.vec^2/tau/2+lam*ab.vec)*(ab.vec<tau*(lam-gam))
        tem1.vec = (gam*ab.vec-tau^2*(lam-gam)^2/tau/2+lam*tau*(lam-gam)-tau*gam*(lam-gam))*(ab.vec>=tau*(lam-gam))
        return(tem0.vec+tem1.vec)
      }
      pen.grad.fun = function(b.vec,lam,gam,tau){
        ab.vec = abs(b.vec); sb.vec = sign(b.vec)
        tem0.vec = (lam-ab.vec/tau)*(ab.vec<tau*(lam-gam)); tem1.vec = (gam) * (ab.vec>=tau*(lam-gam))
        return((tem0.vec+tem1.vec)*sb.vec)
      }
    }
    if(pen=="sridge"){ 
      pen.fun = function(b.vec,lam,gam,tau){
        ab.vec = abs(b.vec)
        tem0.vec = (-ab.vec^2/2/tau+lam*ab.vec)*(ab.vec<tau*lam/(1+tau*gam))
        tem1.vec = (gam*ab.vec^2/2+tau*lam^2/(1+tau*gam)/2)*(ab.vec>=tau*lam/(1+tau*gam))
        return(tem0.vec+tem1.vec)
      }
      pen.grad.fun = function(b.vec,lam,gam,tau){
        ab.vec = abs(b.vec); sb.vec = sign(b.vec)
        tem0.vec = (lam-ab.vec/tau)*(ab.vec<tau*lam/(1+tau*gam))
        tem1.vec = gam*ab.vec*(ab.vec>=tau*lam/(1+tau*gam))
        return((tem0.vec+tem1.vec)*sb.vec)
      }
    }
    if(pen=="mbridge"){
      pen.fun = function(b.vec,lam,gam,tau){
        ab.vec = abs(b.vec); pen.vec = rep(0,length(b.vec))
        pen.vec[ab.vec< tau] = lam*ab.vec[ab.vec<tau]
        pen.vec[ab.vec>=tau] = lam*(2*sqrt(tau*ab.vec[ab.vec>=tau])-tau)
        return(pen.vec)
      }
      pen.grad.fun = function(b.vec,lam,gam,tau){
        ab.vec = abs(b.vec); sb.vec = sign(b.vec); grad.vec = rep(0,length(b.vec))
        grad.vec[ab.vec< tau] = lam
        grad.vec[ab.vec>=tau] = lam*sqrt(tau/ab.vec[ab.vec>=tau])
        return(grad.vec*sb.vec)
      }
    }
    if(pen=="mlog"){
      pen.fun = function(b.vec,lam,gam,tau){
        ab.vec = abs(b.vec); pen.vec = rep(0,length(b.vec))
        pen.vec[ab.vec< tau] = lam*ab.vec[ab.vec<tau]
        pen.vec[ab.vec>=tau] = lam*tau*(1+log(ab.vec[ab.vec>=tau]/tau))
        return(pen.vec)
      }
      pen.grad.fun = function(b.vec,lam,gam,tau){
        ab.vec = abs(b.vec); sb.vec = sign(b.vec); grad.vec = rep(0,length(b.vec))
        grad.vec[ab.vec< tau] = lam; grad.vec[ab.vec>=tau] = lam*tau/ab.vec[ab.vec>=tau]
        return(grad.vec*sb.vec)
      }
    }

    # par(mfrow=c(2,2))
    # tau = 2 
    # lam = 1 
    # gam = 10
    # b.vec = seq(0,tau+1,length.out=1001)[-1]
    # plot(b.vec,pen.grad.fun(b.vec,lam,gam,tau))
    # plot(b.vec,pen.grad.fun(b.vec,lam,gam,tau)-lam)
    # plot(b.vec,pen.grad.fun(b.vec,lam,gam,tau)-lam-gam*b.vec)
    # par(mfrow=c(2,2))
    # b.vec = seq(0,0.1*tau+1,length.out=1001)[-1]
    # plot(b.vec,pen.fun(b.vec,lam,gam,tau))
    # plot(b.vec,pen.fun(b.vec,lam,gam,tau)-lam*b.vec)
    # plot(b.vec,pen.fun(b.vec,lam,gam,tau)-lam*b.vec-gam*b.vec^2/2)
    # 
    # which.min(b.vec+pen.grad.fun(b.vec,lam,gam,tau))
  
    return(list(pen.fun=pen.fun,pen.grad.fun=pen.grad.fun))
  }
  
  get.fam.fun = function(fam){
    if(fam=="gaussian"){
      obj.fun = function(y.vec,x.mat,b.vec){
        xb.vec = drop(x.mat%*%b.vec)
        return(sum((xb.vec-y.vec)^2)/length(y.vec)/2)
      }
      obj.grad.fun = function(y.vec,x.mat,b.vec){
        xb.vec = drop(x.mat%*%b.vec)
        return(drop(t(x.mat)%*%(xb.vec-y.vec)/length(y.vec)))
      }
      obj.hess.fun = function(y.vec,x.mat,b.vec){
        return(t(x.mat)%*%x.mat/length(y.vec))
      }
    }
    if(fam=="poisson"){
      obj.fun = function(y.vec,x.mat,b.vec){
        xb.vec = pmin(drop(x.mat%*%b.vec),700);
        return(sum(exp(xb.vec)-y.vec*xb.vec)/length(y.vec))
      }
      obj.grad.fun = function(y.vec,x.mat,b.vec){
        xb.vec = pmin(drop(x.mat%*%b.vec),700);
        return(drop(t(x.mat)%*%(exp(xb.vec)-y.vec))/length(y.vec) )
      }
      obj.hess.fun = function(y.vec,x.mat,b.vec){
        xb.vec = pmin(drop(x.mat%*%b.vec),700);
        exb.vec = exp(xb.vec)
        return(t(x.mat)%*%diag(exb.vec)%*%x.mat/length(y.vec))
      }
    }
    if(fam=="binomial"){
      obj.fun = function(y.vec,x.mat,b.vec){
        xb.vec = pmin(drop(x.mat%*%b.vec),700)
        return(sum(log(1+exp(xb.vec))-y.vec*xb.vec)/length(y.vec))
      }
      obj.grad.fun = function(y.vec,x.mat,b.vec){
        xb.vec = pmin(drop(x.mat%*%b.vec),700); exb.vec = exp(xb.vec)
        p.vec = exb.vec/(1+exb.vec)
        return(drop(t(x.mat)%*%(p.vec-y.vec)/length(y.vec)))
      }
      obj.hess.fun = function(y.vec,x.mat,b.vec){
        xb.vec = pmin(drop(x.mat%*%b.vec),700); exb.vec = exp(xb.vec)
        p.vec = exb.vec/(1+exb.vec); d.vec = p.vec*(1-p.vec); d.vec[d.vec<1e-7] = 1e-7
        return(t(x.mat)%*%diag(d.vec)%*%x.mat/length(y.vec))
      }
    }
    if(fam=="multinomial"){
      obj.fun = function(y.vec,sx.mat,sb.vec){ 
        k = max(y.vec); n = length(y.vec)
        sy.vec = as.numeric(rep(y.vec,k-1)==rep(1:(k-1),each=n))
        sxb.vec = pmin(drop(sx.mat%*%sb.vec),700); esxb.vec = exp(sxb.vec); esxb.mat = matrix(esxb.vec,ncol=k-1)
        loss = sum(log(1+rowSums(esxb.mat)))-sum(sy.vec*sxb.vec)
        return(loss/length(y.vec)) 
      }
      obj.grad.fun = function(y.vec,sx.mat,sb.vec){
        k = max(y.vec); n = length(y.vec)
        sy.vec = as.numeric(rep(y.vec,k-1)==rep(1:(k-1),each=n))
        sxb.vec = pmin(drop(sx.mat%*%sb.vec),700); esxb.vec = exp(sxb.vec); esxb.mat = matrix(esxb.vec,ncol=k-1)
        sp.mat = esxb.mat/(1+rowSums(esxb.mat)); sp.vec = as.vector(sp.mat)
        grad.vec = -as.vector(t(sx.mat)%*%(sy.vec-sp.vec))
        return(grad.vec/length(y.vec))
      }
      obj.hess.fun = function(y.vec,sx.mat,sb.vec){
        k = max(y.vec); n = length(y.vec)
        sy.vec = as.numeric(rep(y.vec,k-1)==rep(1:(k-1),each=n))
        sxb.vec = pmin(drop(sx.mat%*%sb.vec),700); esxb.vec = exp(sxb.vec); esxb.mat = matrix(esxb.vec,ncol=k-1)
        sp.mat = esxb.mat/(1+rowSums(esxb.mat)); sp.vec = as.vector(sp.mat) 
        sp.vec[sp.vec<1e-7] = 1e-7; sp.vec[sp.vec>(1-(1e-7))] = 1-(1e-7)
        d.mat = diag(sp.vec)-outer(sp.vec,sp.vec)*kronecker(matrix(1,k-1,k-1),diag(n))
        hess.mat = t(sx.mat)%*%d.mat%*%sx.mat 
        return(hess.mat/n)
      }
    }
    if(fam=="cox"){
      Outer <- function(vec){ as.vector(outer(vec,vec)) }
      obj.fun = function(y.vec,sx.mat,b.vec){ 
        s.vec = sx.mat[,dim(sx.mat)[2]]; x.mat = sx.mat[,-dim(sx.mat)[2],drop=F]
        n = length(y.vec); r = sum(s.vec); f.vec = s.vec==1; 
        f.mat = matrix(rep(y.vec[f.vec],n),ncol=r,byrow=T)-matrix(rep(y.vec,r),ncol=r) <= 0 
        xb.vec = drop(x.mat%*%b.vec); xb.vec[xb.vec>100] = 100
        exb.mat = matrix(rep(exp(xb.vec),r),ncol=r)  
        w.vec = pmax(colSums(f.mat*exb.mat),exp(-100))
        loss = -drop(f.vec%*%xb.vec)+sum(log(w.vec))
        return(loss/n)
      }
      obj.grad.fun = function(y.vec,sx.mat,b.vec){
        s.vec = sx.mat[,dim(sx.mat)[2]]; x.mat = sx.mat[,-dim(sx.mat)[2],drop=F]
        n = length(y.vec); r = sum(s.vec); f.vec = s.vec==1; 
        f.mat = matrix(rep(y.vec[f.vec],n),ncol=r,byrow=T)-matrix(rep(y.vec,r),ncol=r) <= 0 
        xb.vec = drop(x.mat%*%b.vec); xb.vec[xb.vec>100] = 100
        exb.mat = matrix(rep(exp(xb.vec),r),ncol=r)  
        w.vec = pmax(colSums(f.mat*exb.mat),exp(-100))
        w.mat = t(f.mat)%*%(x.mat*exp(xb.vec))
        return((-drop(f.vec%*%x.mat)+colSums(w.mat/w.vec))/n)
      }
      obj.hess.fun = function(y.vec,sx.mat,b.vec){
        s.vec = sx.mat[,dim(sx.mat)[2]]; x.mat = sx.mat[,-dim(sx.mat)[2],drop=F]
        n = length(y.vec); r = sum(s.vec); f.vec = s.vec==1;
        f.mat = matrix(rep(y.vec[f.vec],n),ncol=r,byrow=T)-matrix(rep(y.vec,r),ncol=r) <= 0
        xb.vec = drop(x.mat%*%b.vec); xb.vec[xb.vec>100] = 100
        exb.mat = matrix(rep(exp(xb.vec),r),ncol=r)
        w.vec <- pmax(colSums(f.mat*exb.mat),exp(-100))
        w1.mat <- t(f.mat)%*%(x.mat*exp(xb.vec))
        if(length(b.vec)==1){
          w1.mat <- w1.mat^2; w2.mat <- t(f.mat)%*%(x.mat^2*exp(xb.vec))
        } else {
          w1.mat <- t(apply(w1.mat,1,Outer)); w2.mat <- t(f.mat)%*%(t(apply(x.mat,1,Outer))*exp(xb.vec))
        }
        return(matrix(colSums(w2.mat/w.vec-w1.mat/(w.vec^2)),nrow=dim(x.mat)[2])/n)
      }
    }
    return(list(obj.fun=obj.fun,obj.grad.fun=obj.grad.fun,obj.hess.fun=obj.hess.fun))
  }
  
  soft.fun = function(est,del){ return(sign(est)*(abs(est)-del)*(abs(est)>del)) }
  
  nr.fun = function(y.vec,x.mat,iter.max,b.eps,fam){                                  
    obj.fun = get.fam.fun(fam)[[1]]; obj.grad.fun = get.fam.fun(fam)[[2]]; obj.hess.fun = get.fam.fun(fam)[[3]]
    p = dim(x.mat)[2]
    b.vec = rep(0,p); if(fam=="cox") b.vec = rep(0,p-1)
    for(iter in 1:iter.max){
      hess.mat = obj.hess.fun(y.vec,x.mat,b.vec); grad.vec = obj.grad.fun(y.vec,x.mat,b.vec); 
      nb.vec = b.vec-solve(hess.mat)%*%grad.vec; if(sum(abs(nb.vec-b.vec))<b.eps) break; b.vec = nb.vec
    }
    return(b.vec)
  }
  
  ### it minimizes t(b.vec)%*%(q.mat)%*%b.vec + t(l.vec)%*%b.vec + lam*sum(abs(w.vec*b.vec))
  ##################################################################################################################################
  ##################################################################################################################################
  ##################################################################################################################################
  
  qlasso.fun = function(q.mat,l.vec,b.vec,w.vec,lam,iter.max,iiter.max,b.eps,k.eps,p.eff,q.rank,cut,c.eps){
    p = length(b.vec)
    f.vec = rep(0,iter.max)
    for(iter in 1:iter.max){#iter
      pp.eff = p.eff                                                          
      ##### active set #####
      a1.set = c(1:p)[(b.vec!=0)&(w.vec!=0)] 
      a2.set = c(1:p)[w.vec==0]
      a.set = c(a1.set,a2.set)
      for(iiter in 1:iiter.max){#iiter 
        ob.vec = b.vec
        for(pos in a1.set){
          est = -(2*sum(q.mat[-pos,pos]*b.vec[-pos])+l.vec[pos])/q.mat[pos,pos]/2
          del = lam*w.vec[pos]/q.mat[pos,pos]/2
          b.vec[pos] = soft.fun(est,del)
        }
        for(pos in a2.set){
          est = -(2*sum(q.mat[-pos,pos]*b.vec[-pos])+l.vec[pos])/q.mat[pos,pos]/2
          b.vec[pos] = est
        }
        con = sum(abs(b.vec-ob.vec))<b.eps
        if(con) break                                                   
        ##### projection: reduced model #####
        if(cut==TRUE){ b.vec[abs(b.vec)<c.eps] = 0; } #cat("cut small values in a.set \n") } ### mod
        if(iiter>pp.eff){#pp.eff 
          oob.vec = b.vec
          a.set = c(1:p)[b.vec!=0]
          if(length(a.set)<=q.rank){#q.rank   ### mod 
            pb.vec = b.vec*0
            pb.vec[a.set] = -solve(q.mat[a.set,a.set])%*%(l.vec[a.set]+lam*w.vec[a.set]*sign(b.vec[a.set]))/2
            pbf = t(pb.vec[a.set])%*%(q.mat[a.set,a.set])%*%pb.vec[a.set]
            pbf = pbf+sum(l.vec[a.set]*pb.vec[a.set])+lam*sum(abs(w.vec[a.set]*pb.vec[a.set]))
            bf  = t(b.vec[a.set])%*%(q.mat[a.set,a.set])%*% b.vec[a.set]
            bf  = bf + sum(l.vec[a.set]*b.vec[a.set])+lam*sum(abs(w.vec[a.set]*b.vec[a.set]))
            if(pbf<bf){
              b.vec = pb.vec
              #cat("projection works in a.set:","lam=",lam,"df=",sum(b.vec!=0),"iiter",iiter,"\n") ### mod    
            } else {
              b.vec = oob.vec
              #cat("projection fails in a.set:","lam=",lam,"df=",sum(b.vec!=0),"iiter",iiter,"\n") ### mod    
            }
            pp.eff = 2*pp.eff
          }#q.rank
        }#pp.eff
        con = sum(abs(b.vec-ob.vec))<b.eps 
        if(con) break 
      }#iiter 
      #cat("a.set=",length(a.set),"lambda=",lam,"iiter=",iiter,"con=",con,"\n") ### mod 
      ##### null set #####
      n.set = c(1:p)[(b.vec==0)&(w.vec!=0)]
      for(pos in n.set){
        est = -(2*sum(q.mat[-pos,pos]*b.vec[-pos])+l.vec[pos])/q.mat[pos,pos]/2
        del = lam*w.vec[pos]/q.mat[pos,pos]/2
        b.vec[pos] = soft.fun(est,del)
      }
      ##### projection: full model #####
      if(cut==TRUE){ b.vec[abs(b.vec)<c.eps] = 0; } #cat("cut small values in n.set \n") } ### mod
      if(iter>p.eff){# proj
        oob.vec = b.vec
        a.set = c(1:p)[b.vec!=0]
        if(length(a.set)<=q.rank){
          pb.vec = b.vec*0
          pb.vec[a.set] = -solve(q.mat[a.set,a.set])%*%(l.vec[a.set]+lam*w.vec[a.set]*sign(b.vec[a.set]))/2
          pbf = t(pb.vec[a.set])%*%(q.mat[a.set,a.set])%*%pb.vec[a.set] 
          pbf = pbf + sum(l.vec[a.set]*pb.vec[a.set])+lam*sum(abs(w.vec[a.set]*pb.vec[a.set]))
          bf  = t( b.vec[a.set])%*%(q.mat[a.set,a.set])%*% b.vec[a.set] 
          bf = bf + sum(l.vec[a.set]* b.vec[a.set])+lam*sum(abs(w.vec[a.set]* b.vec[a.set]))
          if(pbf<bf){
            b.vec = pb.vec   
            #cat("projection works:","lam=",lam,"df=",sum(b.vec!=0),"iter",iiter,"\n") ### mod    
          } else {
            b.vec = oob.vec
            #cat("projection fails:","lam=",lam,"df=",sum(b.vec!=0),"iter",iiter,"\n") ### mod    
          }
          p.eff = 2*p.eff
        }
      }# proj
      if(cut==TRUE){ b.vec[abs(b.vec)<c.eps] = 0; } #cat("cut small values in n.set \n") } ### mod
      ##### check KKT #####
      f.vec[iter] = drop(t(b.vec)%*%q.mat%*%b.vec+sum(l.vec*b.vec))+lam*sum(abs(w.vec*b.vec))
      g.vec = 2*drop(q.mat%*%b.vec)+l.vec
      p.vec = lam*w.vec*sign(b.vec)
      a.set = c(1:p)[(b.vec!=0)|(w.vec==0)] #### b.vec!0 ?
      n.set = c(1:p)[(b.vec==0)&(w.vec!=0)] 
      kkt0 = sum(abs(g.vec[a.set]+p.vec[a.set])<k.eps)==length(a.set)
      kkt1 = sum(abs(g.vec[n.set])-lam*w.vec[n.set]<k.eps)==length(n.set) 
      if(kkt0&kkt1) break
    }#iter
    #cat("qlasso.fun: a.set=",length(a.set),"lambda =",lam,"iter=",iter,"kkt0=",kkt0,"kkt1=",kkt1,"\n")  ####### 
    return(list(g.vec=g.vec+p.vec,b.vec=b.vec,f.vec=f.vec[1:iter],con=(kkt0&kkt1)))
  }
  
  ##################################################################################################################################
  ##################################################################################################################################
  ##################################################################################################################################
  
  p.ncpen.fun = function(y.vec,x.mat,b.vec,w.vec,lam,gam,tau,alp,
                         iter.max,qiter.max,qiiter.max,b.eps,k.eps,p.eff,cut,c.eps,fam,pen){
    if(pen!="ridge"){ 
      pen.fun = get.pen.fun(pen)[[1]]; 
      pen.grad.fun = get.pen.fun(pen)[[2]] 
    } else { 
      pen.fun = get.pen.fun("scad")[[1]]; 
      pen.grad.fun = get.pen.fun("scad")[[2]] 
    }
    obj.fun = get.fam.fun(fam)[[1]]; 
    obj.grad.fun = get.fam.fun(fam)[[2]]; 
    obj.hess.fun = get.fam.fun(fam)[[3]]
    q.rank = dim(x.mat)[2]; 
    p = length(b.vec); 
    
    f.vec = rep(0,iter.max)
    for(iter in 1:iter.max){
      ob.vec = b.vec
      h.mat = obj.hess.fun(y.vec,x.mat,b.vec); g.vec = obj.grad.fun(y.vec,x.mat,b.vec)
      q.mat = h.mat/2+(1-alp)*lam*w.vec*diag(rep(1,p))   
      l.vec = g.vec-drop(h.mat%*%b.vec)+alp*w.vec*pen.grad.fun(b.vec,lam,gam,tau)-alp*lam*w.vec*sign(b.vec) 
      if(pen=="sridge"){ q.mat = q.mat+gam*w.vec*diag(rep(1,p))/2; l.vec = l.vec-gam*w.vec*b.vec }
      b.vec = qlasso.fun(q.mat,l.vec,b.vec,w.vec,alp*lam,qiter.max,qiiter.max,b.eps,k.eps,p.eff,q.rank,cut,c.eps)$b.vec
      t.vec = w.vec*pen.grad.fun(ob.vec,lam,gam,tau) - lam*w.vec*sign(ob.vec)
      if(pen=="sridge"){ t.vec = t.vec-gam*w.vec*ob.vec }
      oob = obj.fun(y.vec,x.mat,ob.vec) + alp*sum(t.vec*ob.vec) + alp*lam*sum(w.vec*abs(ob.vec)) + (1-alp)*lam*sum(w.vec*ob.vec^2)
      ob  = obj.fun(y.vec,x.mat, b.vec) + alp*sum(t.vec* b.vec) + alp*lam*sum(w.vec*abs( b.vec)) + (1-alp)*lam*sum(w.vec* b.vec^2)
      if(pen=="sridge"){ oob = oob + gam*sum(w.vec*ob.vec^2)/2; ob = ob + gam*sum(w.vec*b.vec^2)/2 }
      if(ob>(oob+b.eps)){#mlqa
        cat("golden section algorithm in p.ncpen.fun","\n")
        gold = (sqrt(5)-1)/2; a = -1; b = 1
        for(iiter in 1:iter.max){
          c = gold*a+(1-gold)*b; 
          d = (1-gold)*a+gold*b
          b1.vec = c* b.vec + (1-c)*ob.vec; 
          b2.vec = d* b.vec + (1-d)*ob.vec
          ob1 = obj.fun(y.vec,x.mat,b1.vec) + alp*sum(t.vec*b1.vec) + alp*lam*sum(w.vec*abs(b1.vec)) + (1-alp)*lam*sum(w.vec*b1.vec^2)
          ob2 = obj.fun(y.vec,x.mat,b2.vec) + alp*sum(t.vec*b2.vec) + alp*lam*sum(w.vec*abs(b2.vec)) + (1-alp)*lam*sum(w.vec*b2.vec^2)
          if(pen=="sridge"){ ob1 = ob1 + gam*sum(w.vec*b1.vec^2)/2; ob2 = ob2 + gam*sum(w.vec*b2.vec^2)/2 }
          if(ob1 > ob2){ a = c } else { b = d }
          if(sum(abs(b.vec-ob.vec))<b.eps) break
        }
      }#mlqa
      f.vec[iter] = obj.fun(y.vec,x.mat,b.vec) + alp*sum(w.vec*pen.fun(b.vec,lam,gam,tau)) + (1-alp)*lam*sum(w.vec*b.vec^2)
      g.vec = obj.grad.fun(y.vec,x.mat,b.vec) + alp*w.vec*pen.grad.fun(b.vec,lam,gam,tau) 
      g.vec = g.vec - alp*lam*w.vec*sign(b.vec) + 2*(1-alp)*lam*w.vec*b.vec 
      p.vec = alp*lam*w.vec*sign(b.vec); 
      a.set = c(1:p)[(b.vec!=0)|(w.vec==0)] #### b.vec!0 ?
      n.set = c(1:p)[(b.vec==0)&(w.vec!=0)] 
      kkt0 = sum(abs(g.vec[a.set]+p.vec[a.set])<k.eps)==length(a.set)
      kkt1 = sum(abs(g.vec[n.set])-alp*lam*w.vec[n.set]<k.eps)==length(n.set)
      #cat("         p.ncpen.fun: a.set=",length(a.set),"lambda =",lam,"iter=",iter,"kkt0=",kkt0,"kkt1=",kkt1,"\n")  
      if(kkt0&kkt1) break 
      #ob.vec = b.vec
    }
    cat("         p.ncpen.fun: a.set=",length(a.set),"lambda =",lam,"iter=",iter,"kkt0=",kkt0,"kkt1=",kkt1,"\n")  
    return(list(g.vec=g.vec+p.vec,b.vec=b.vec,f.vec=f.vec[1:iter],con=(kkt0&kkt1)))
  }

  ##################################################################################################################################
  ##################################################################################################################################
  ##################################################################################################################################
  ncpen.fun = function(y.vec,x.mat,w.vec,lam.vec,gam,tau,alp,
                       d.max,iter.max,qiter.max,qiiter.max,b.eps,k.eps,p.eff,cut,c.eps,add,
                       fam,pen,loc,ob.vec,div){ 
    if(pen!="ridge"){ pen.fun = get.pen.fun(pen)[[1]]; pen.grad.fun = get.pen.fun(pen)[[2]] 
    } else { pen.fun = get.pen.fun("scad")[[1]]; pen.grad.fun = get.pen.fun("scad")[[2]] }
    obj.fun = get.fam.fun(fam)[[1]]; obj.grad.fun = get.fam.fun(fam)[[2]]; obj.hess.fun = get.fam.fun(fam)[[3]]
    if(fam!="cox"){ p = dim(x.mat)[2] } else { p = dim(x.mat)[2]-1 }
    if(pen=="ridge"){
      b.vec = rep(0.001,p); a.set = c(1:p); d.max = p; cut = F
    } else {
      b.vec = rep(0,p); a.set = c(1:p)[(b.vec!=0)|(w.vec==0)]; 
      if(length(a.set)!=0){
        if(fam!="cox"){
          b.vec[a.set] = drop(native_cpp_nr_fun_(fam,y.vec,x.mat[,a.set,drop=F],iter.max,b.eps))
        } else {
          b.vec[a.set] = drop(native_cpp_nr_fun_(fam,y.vec,x.mat[,c(a.set,p+1),drop=F],iter.max,b.eps))
        }
      }
    }
    n.set = c(1:p)[(b.vec==0)&(w.vec!=0)]
    r = length(lam.vec); 
    b.mat = matrix(0,p,r); g.mat = matrix(0,p,r); c.mat = matrix(0,2,r)
    f.vec = rep(0,r); d.vec = rep(0,r);
    for(pos in 1:r){
      lam = lam.vec[pos]; 
      if(loc==TRUE) { b.vec = ob.vec }
      g.vec = obj.grad.fun(y.vec,x.mat,b.vec)+alp*w.vec*pen.grad.fun(b.vec,lam,gam,tau)
      g.vec = g.vec-alp*lam*w.vec*sign(b.vec)+2*(1-alp)*lam*w.vec*b.vec
      p.vec = alp*lam*w.vec*sign(b.vec); 
      kkt0 = sum(abs(g.vec[a.set]+p.vec[a.set])<k.eps)==length(a.set)
      kkt1 = sum(abs(g.vec[n.set])-alp*lam*w.vec[n.set]<k.eps)==length(n.set)
      if(!(kkt0&kkt1)){
        for(iter in 1:iter.max){#iter
          
          p.set = n.set[(abs(g.vec[n.set])-alp*lam*w.vec[n.set])>k.eps]
          if(length(p.set)<=1){
            ad = p.set[which.max(abs(g.vec[p.set]))]
          } else {
            od = order(abs(g.vec[p.set]),decreasing=T)
            ed = min(length(p.set),add)
            ad= p.set[od[1:ed]]
          }
          a.set = c(a.set,ad)
          if(pen=="ridge"){
            b.vec = p.ncpen.fun(y.vec,x.mat,b.vec,w.vec,lam,gam,tau,alp,
                                iter.max,qiter.max,qiiter.max,b.eps,k.eps,p.eff,cut,c.eps,fam,pen)$b.vec
          } else { 
            if(fam=="cox"){
              b.vec[a.set] = p.ncpen.fun(y.vec,x.mat[,c(a.set,p+1),drop=F],b.vec[a.set],w.vec[a.set],lam,gam,tau,alp,
                                         iter.max,qiter.max,qiiter.max,b.eps,k.eps,p.eff,cut,c.eps,fam,pen)$b.vec
            } else {
              b.vec[a.set] = p.ncpen.fun(y.vec,x.mat[,a.set,drop=F] ,b.vec[a.set],w.vec[a.set],lam,gam,tau,alp,
                                         iter.max,qiter.max,qiiter.max,b.eps,k.eps,p.eff,cut,c.eps,fam,pen)$b.vec
            }
          }
          g.vec = obj.grad.fun(y.vec,x.mat,b.vec) + alp*w.vec*pen.grad.fun(b.vec,lam,gam,tau)
          g.vec = g.vec-alp*lam*w.vec*sign(b.vec) + 2*(1-alp)*lam*w.vec*b.vec
          p.vec = alp*lam*w.vec*sign(b.vec); 
          a.set = c(1:p)[b.vec!=0]; n.set = c(1:p)[b.vec==0]
          kkt0 = sum(abs(g.vec[a.set]+p.vec[a.set])<k.eps)==length(a.set)
          kkt1 = sum(abs(g.vec[n.set])-alp*lam*w.vec[n.set]<k.eps)==length(n.set)
          # cat("================== ncpen.fun: a.set=",length(a.set),
          #     "lambda =",lam,"iter=",iter,"kkt0=",kkt0,"kkt1=",kkt1,"family=",fam,"penalty=",pen,"\n")  
          if(kkt0&kkt1) break #iter break
        }#iter
      }
      cat("================== ncpen.fun: a.set=",length(a.set),
          "lambda =",lam,"pos=",pos,"kkt0=",kkt0,"kkt1=",kkt1,"family=",fam,"penalty=",pen,"\n")  
      b.mat[,pos] = b.vec; g.mat[,pos] = g.vec+p.vec; c.mat[,pos] = c(kkt0,kkt1)
      f.vec[pos] = obj.fun(y.vec,x.mat,b.vec) + alp*sum(w.vec*pen.fun(b.vec,lam,gam,tau)) + (1-alp)*lam*sum(w.vec*b.vec^2)
      d.vec[pos] = length(a.set)

      if(length(a.set)>d.max){ 
        cat("(warning 1) ncpen stops because the number of non-zero parameters exceeds df.max=",d.max,"\n")
        break #pos break
      }
      if(fam=="binomial"){
        if(max(abs(b.vec))>div){                                                                
          cat("(warning 2) ncpen stops because a paramter becomes larger than cf.max=",div,"\n")
          cat("(warning 2) try larger ridge effect by reducing current alpha=",alp,"\n")
          break #pos break
        }
      }
    }#pos
    return(list(beta=b.mat[,1:pos],grad=g.mat[,1:pos],f.vec=f.vec[1:pos],conv=c.mat[,1:pos],
                lambda=lam.vec[1:pos],df=d.vec[1:pos],w.lambda=w.vec))
  }
  
  ##################################################################################################################################
  ##################################################################################################################################
  ##################################################################################################################################
  
  control.ncpen = function(y.vec,x.mat,
                          family=c("gaussian","binomial","multinomial","cox","poisson"),
                          penalty=c("scad","mcp","tlp","lasso","classo","ridge","sridge","mbridge","mlog"),
                          x.standardize=TRUE,intercept=TRUE,
                          lambda=NULL,n.lambda=NULL,r.lambda=NULL,w.lambda=NULL,gamma=NULL,tau=NULL,alpha=NULL,
                          aiter.max=1e+2,b.eps = 1e-7){
    ### family and penalty 
    family = match.arg(family); penalty = match.arg(penalty); 
    ### alpha
    if(is.null(alpha)){ alpha = switch(penalty,scad=1,mcp=1,tlp=1,lasso=1,classo=1,ridge=0,sridge=1,mbridge=1,mlog=1)
    } else {
      if(penalty=="ridge"){ if(alpha!=0){ warning("alpha is set to 0 \n") }; alpha = 0; 
      } else if(penalty=="sridge"){ if(alpha!=1){ warning("alpha is set to 1 \n") }; alpha = 1; 
      } else { if(alpha<0){ warning("alpha is set to 0 \n"); alpha = 0 }; if(alpha>1){ warning("alpha is set to 1 \n"); alpha = 1 } }
    }
    ### tau
      
    if(is.null(tau)){ tau = switch(penalty,scad=3.7,mcp=2.1,tlp=0.001,lasso=2,classo=2.1,ridge=2,sridge=2.1,mbridge=0.001,mlog=0.001)
    } else { 
      if(penalty=="scad"){ if(tau<=2){ tau = 2.001; warning("tau is set to ",tau,"\n") } 
#       if(penalty=="scad"){ if(tau<=1){ tau = 1.001; warning("tau is set to ",tau,"\n") } 
      } else if(penalty=="mcp"){ if(tau<=1){ tau = 1.001; warning("tau is set to ",tau,"\n") } 
      } else if((penalty=="classo")|(penalty=="sridge")){ if(tau<=1){ tau = 1.001; warning("tau is set to ",tau,"\n") }
      } else { if(tau<=0){ tau = 0.001; warning("tau is set to ",tau,"\n") } }
    }
        
    ### w.lambda
    n = dim(x.mat)[1]; p = dim(x.mat)[2]; if(family=="cox"){ p = p-1 } 
    if(is.null(w.lambda)){ w.lambda = rep(1,p)
    } else {
      if(length(w.lambda)!=p){ stop("the number of elements in w.lambda should be the number of input variables") 
      } else if(sum(w.lambda==0)>=n){ stop("the number of zero elements in w.lambda should be less than the sample size") 
      } else if(min(w.lambda)<0){ stop("elements in w.lambda should be non-negative") 
      } else { w.lambda = (p-sum(w.lambda==0))*w.lambda/sum(w.lambda) }
    }
    ### target and design matrix 
    if(x.standardize==FALSE){ std = rep(1,p) 
    } else {
      if(family=="cox"){ std = sqrt(colSums(x.mat[,-(p+1)]^2)/n); std[std==0] = 1; x.mat[,-(p+1)] = sweep(x.mat[,-(p+1)],2,std,"/") 
      } else { std = sqrt(colSums(x.mat^2)/n); std[std==0] = 1; x.mat = sweep(x.mat,2,std,"/") }
    }
    if(family=="cox") intercept = FALSE; 
    if(intercept==TRUE){ x.mat = cbind(1,x.mat); std = c(1,std); w.lambda = c(0,w.lambda); p = p+1; } 
    if(family=="multinomial"){ 
      k = max(y.vec)
      if(length(unique(y.vec))!=k){ stop("label must be denoted by 1,2,...: ") }
      #y.vec = colSums((matrix(rep(y.vec,k),nrow=k,byrow=TRUE)==uy.vec)*(1:k)); 
      #if(sum(uy.vec)!=sum(1:k)){ warning("label has been changed: ",uy.vec," to ",sort(unique(y.vec))); }
      x.mat = kronecker(diag(k-1),x.mat); w.lambda = rep(w.lambda,k-1); p = p*(k-1); std = rep(std,k-1);
    } 
    ### lambda (r.lambda)
    if(is.null(r.lambda)){ 
      if((family=="cox")|(family=="multinomial")){
        r.lambda = ifelse(n<p,0.1,0.01); 
      } else {
        r.lambda = ifelse(n<p,0.01,0.001); 
      }
    } else { 
      if((r.lambda<=0)|(r.lambda>=1)) stop("r.lambda should be between 0 and 1"); 
    }
    ### lambda (n.lambda)
    if(is.null(n.lambda)){ n.lambda = 100; 
    } else { if((n.lambda<1e+1)|(n.lambda>1e+3)) stop("n.lambda should be between 10 and 1000") }
    ### lambda 
    if(sum(w.lambda==0)==0){ b.vec = rep(0,p);
    } else { b.vec = rep(0,p);
      if(family!="cox"){ a.set = c(1:p)[w.lambda==0]; } else { a.set = c(c(1:p)[w.lambda==0],p+1); } 
      ax.mat = x.mat[,a.set,drop=F]; b.vec[a.set] = drop(native_cpp_nr_fun_(family,y.vec,ax.mat,aiter.max,b.eps));
    }
    g.vec = abs(native_cpp_obj_grad_fun_(family,y.vec,x.mat,b.vec))
    if(alpha!=0){
      lam.max = max(g.vec[w.lambda!=0]/w.lambda[w.lambda!=0])/alpha; lam.max = lam.max+lam.max/10
      lam.vec = exp(seq(log(lam.max),log(lam.max*r.lambda),length.out=n.lambda))
      if(is.null(lambda)){ lambda = lam.vec
      } else { 
        if(min(lambda)<=0){ stop("elements in lambda should be positive"); 
        } else { 
          lambda = sort(lambda,decreasing=TRUE); 
          #if(max(lambda)<lam.max){ lambda = c(lam.vec[lam.vec>lambda[1]],lambda); warning("lambda is extended up to ",lam.max,"\n") }
        } 
      }
    }
    if(alpha==0) lambda = exp(seq(log(1e-2),log(1e-5),length.out=n.lambda))
    
    ### gamma 
    if(is.null(gamma)){ 
      gamma = switch(penalty,scad=0,mcp=0,tlp=0,lasso=0,classo=min(lambda)/2,ridge=0,sridge=min(lambda)/2,mbridge=0,mlog=0)
    } else {
      if((penalty!="classo")&(penalty!="sridge")){ gamma = 0
      } else {
        if(gamma<0){ gamma = min(lambda)/2; warning("gamma is negative and set to ",gamma,"\n") }
        if(gamma>=max(lambda)){ gamma = max(lambda)/2; warning("gamma is larger than lambda and set to ",gamma,"\n") }
        if(gamma>min(lambda)){ lambda = lambda[lambda>=gamma]; warning("lambda is set larger than gamma=",gamma,"\n") }
      }
    }
    ### return 
    ret = list(y.vec=y.vec,x.mat=x.mat,
               family=family,penalty=penalty,
               x.standardize=x.standardize,intercept=intercept,std=std,
               lambda=lambda,n.lambda=n.lambda,r.lambda=r.lambda,w.lambda=w.lambda,gamma=gamma,tau=tau,alpha=alpha)
    class(ret) = "control.ncpen";
    return(ret)
  }
  
  ##################################################################################################################################
  ##################################################################################################################################
  ##################################################################################################################################
  
  ncpen = function(y.vec,x.mat,
                     family=c("gaussian","binomial","multinomial","cox","poisson"),
                     penalty=c("scad","mcp","tlp","lasso","classo","ridge","sridge","mbridge","mlog"),
                     x.standardize=TRUE,intercept=TRUE,
                     lambda=NULL,n.lambda=NULL,r.lambda=NULL,w.lambda=NULL,gamma=NULL,tau=NULL,alpha=NULL,
                     df.max=50,cf.max=100,proj.min=10,add.max=10,niter.max=30,qiter.max=10,aiter.max=100,
                     b.eps=1e-7,k.eps=1e-4,c.eps=1e-6,cut=TRUE,local=FALSE,local.initial=NULL){
    family = match.arg(family); penalty = match.arg(penalty)
    tun = control.ncpen(y.vec,x.mat,family,penalty,x.standardize,intercept,
                    lambda,n.lambda,r.lambda,w.lambda,gamma,tau,alpha,aiter.max,b.eps)
    if(local==TRUE){ 
      if(is.null(local.initial)){ stop(" supply 'local.initial' \n") }
      warning("       'local==TRUE' option may take a long time \n") 
    } else { local.initial = rep(0,length(tun$w.lambda)) }
    fit = native_cpp_ncpen_fun_(tun$y.vec,tun$x.mat,
                                        tun$w.lambda,tun$lambda,tun$gamma,tun$tau,tun$alpha,
                                        df.max,niter.max,qiter.max,aiter.max,
                                        b.eps,k.eps,proj.min,cut,c.eps,add.max,family,penalty,local,local.initial,cf.max)
    if(x.standardize==TRUE){ fit$beta = fit$beta/tun$std  }
    ret = list(y.vec=tun$y.vec,x.mat=tun$x.mat,
               family=family,penalty=penalty, 
               x.standardize=x.standardize,intercept=tun$intercept,std=tun$std, 
               lambda=drop(fit$lambda),w.lambda=tun$w.lambda,gamma=tun$gamma,tau=tun$tau,alpha=tun$alpha,
               local.initial=local.initial,
               beta=fit$beta,df=drop(fit$df))
    
    class(ret) = "ncpen";
    return(ret);
  }
  #################################################################################################################################
  
  coef.ncpen = function(fit){
    p = dim(fit$beta)[1]
    k = max(fit$y.vec)
    p = ifelse(fit$fam=="multinomial",p/(k-1),p)
    p = ifelse(fit$int==T,p-1,p)
    lam.name = paste("lambda",1:length(fit$lam),sep="")
    var.name = paste("x",1:p,sep="") 
    if(fit$int==TRUE){ var.name = c("intercept",var.name) }
    beta = fit$beta 
    if(fit$fam=="multinomial"){
      beta = list(); 
      for(i in 1:dim(fit$beta)[2]){ 
        beta[[i]] = cbind(matrix(fit$beta[,i],ncol=k-1),0)
        rownames(beta[[i]]) = var.name 
        colnames(beta[[i]]) = paste("class",1:k,sep="")
      }
      names(beta) = lam.name
    } else {
      colnames(beta) = lam.name; rownames(beta) = var.name
    }
    return(beta)
  }
  
  #################################################################################################################################
  
  gic.ncpen = function(fit,weight=NULL,verbose=FALSE,...){
    n = length(fit$y.vec)
    p = dim(fit$beta)[1]
    k = max(fit$y.vec) 
    if(is.null(weight)){ weight = ifelse(n>p,log(n),log(log(p))*log(n)) }
    dev = 2*n*apply(fit$beta,2,FUN="native_cpp_obj_fun_",name=fit$fam,y_vec=fit$y.vec,x_mat=fit$x.mat)
    gic = dev+weight*drop(fit$df)
    opt = which.min(gic)
    if(verbose==TRUE){
      plot(fit$lam,gic,xlab="lambda",ylab="",main="information criterion",...)
      abline(v=fit$lam[opt])
    }
    opt.beta=fit$beta[,opt]
    if(fit$fam=="multinomial"){
      opt.beta = cbind(matrix(fit$beta[,opt],ncol=k-1),0); colnames(opt.beta) = paste("class",1:k,sep="")
    }
    return(list(gic=gic,lambda=fit$lambda,opt.lambda=fit$lambda[opt],opt.beta=opt.beta))
  }
  
  #################################################################################################################################
  
  plot.ncpen = function(fit,log.scale=FALSE,mult.type=c("mat","vec"),...){
    beta = fit$beta 
    lambda = fit$lam 
    if(log.scale==T) lambda = log(lambda)
    if(fit$fam!="multinomial"){
      if(fit$int==T){ beta = beta[-1,] } 
      plot(lambda,beta[1,],type="n",ylim=c(min(beta),max(beta)),main="trace of coefficients",ylab="",...)
      for(j in 1:dim(beta)[1]){ lines(lambda,beta[j,],col=j,...) } #col=iter,lty=iter) }
    } else {
      k = max(fit$y.vec)
      p = dim(beta)[1]/(k-1)
      if(match.arg(mult.type)=="mat"){#"mat
        par(mfrow=c(1,k-1))
        for(i in 1:(k-1)){
          mat = beta[(1+(i-1)*p):(i*p),]
          if(fit$int==T){ mat = mat[-1,] }
          plot(lambda,mat[1,],type="n",ylim=c(min(mat),max(mat)),main="trace of coefficients",ylab="",...)
          for(j in 1:dim(mat)[1]){ lines(lambda,mat[j,],col=j,...) } #col=iter,lty=iter) }
        }
      } else {#"vec
        if(fit$int==T){ beta = beta[-(1+(0:(k-2))*p),] }
        plot(lambda,beta[1,],type="n",ylim=c(min(beta),max(beta)),main="trace of coefficients",ylab="",...)
        for(j in 1:dim(beta)[1]){ lines(lambda,beta[j,],col=j,...) } #col=iter,lty=iter) }
      }
    }
  }
  
  ##################################################################################################################################

  predict.ncpen = function(fit,type=c("y","reg","prob","rmse","like"),new.y.vec=NULL,new.x.mat=NULL,prob.cut=0.5){
    if(is.null(new.x.mat)){ stop("'new.x.mat' should be supplied for prediction") }
    if(match.arg(type)%in%c("y","reg","prob")){ new.y.vec = NULL 
    } else { if(is.null(new.y.vec)) stop("'new.y.vec' should be supplied for prediction") }
    l.name = paste("lam",1:length(fit$lam),sep="")
    if(fit$int==TRUE){ new.x.mat = cbind(1,new.x.mat) } 
    if(fit$fam=="multinomial"){ 
      k = max(fit$y.vec); new.x.mat = kronecker(diag(k-1),new.x.mat); c.name = paste("class",1:k,sep="") 
    }
    if(fit$fam!="cox"){ 
      new.reg = new.x.mat%*%fit$beta
    } else { new.reg = new.x.mat[,-dim(new.x.mat)[2]]%*%fit$beta }
    
    if(fit$fam=="gaussian"){ new.y = new.reg; new.prob = NULL 
    } else if(fit$fam=="binomial"){ new.prob = exp(new.reg)/(1+exp(new.reg)); new.y = 1*(new.prob>prob.cut) 
    } else if(fit$fam=="multinomial"){ 
      reg.list = list()
      for(i in 1:dim(new.reg)[2]){ reg.list[[i]] = cbind(matrix(new.reg[,i],ncol=k-1),0); colnames(reg.list[[i]]) = c.name }
      new.reg = reg.list; new.prob = lapply(reg.list,function(x) exp(x)/rowSums(exp(x)))
      new.y = matrix(unlist(lapply(new.prob,function(x) apply(x,1,which.max))),ncol=length(fit$lam))
    } else if(fit$fam=="poisson"){ new.y = exp(new.reg); new.prob = NULL 
    } else { new.y = exp(new.reg); new.prob = NULL }
    
    if(match.arg(type)=="reg"){ return(reg=new.reg)
    } else if(match.arg(type)=="prob"){ return(prob=new.prob)
    } else if(match.arg(type)=="y"){ return(y=new.y) 
    } else if(match.arg(type)=="rmse"){
      new.rmse = sqrt(colSums((new.y.vec-new.y)^2)); return(rmse=new.rmse)
    } else {
      new.like = apply(fit$beta,2,FUN="native_cpp_obj_fun_",name=fit$fam,y_vec=new.y.vec,x_mat=new.x.mat)
      return(rmse=new.like)
    }
  }


  ##################################################################################################################################
  #################################################################################################################################
  #################################################################################################################################
  
  fold.cv.ncpen = function(c.vec,n.fold=10,family=c("gaussian","binomial","multinomial","cox","poisson")){
    family = match.arg(family)
    n = length(c.vec); idx = c.vec*0
    if(family=="gaussian"){ c.vec = c.vec-median(c.vec)+0.5 }
    if(family!="multinomial"){
      set0 = c(1:n)[c.vec<0.5]; idx[set0] = sample(rep(1:n.fold,length=length(set0)))
      set1 = c(1:n)[-set0]; idx[set1] = sample(rep(1:n.fold,length=length(set1)))
    } else { u.vec = unique(c.vec); k = length(u.vec); 
    for(i in 1:k){ set = c(1:n)[c.vec==u.vec[i]]; idx[set] = sample(rep(1:n.fold,length=length(set))) }
    }
    ret = list(family=family,n.fold=n.fold,idx=idx)
    
    print(ret)  ##### 
    
    return(ret)
  }

  ##################################################################################################################################
  ##################################################################################################################################
  ##################################################################################################################################
  cv.ncpen = function(y.vec,x.mat,n.fold=10,fold.id=NULL,
                      family=c("gaussian","binomial","multinomial","cox","poisson"),
                      penalty=c("scad","mcp","tlp","lasso","classo","ridge","sridge","mbridge","mlog"),
                      x.standardize=TRUE,intercept=TRUE,
                      lambda=NULL,n.lambda=NULL,r.lambda=NULL,w.lambda=NULL,gamma=NULL,tau=NULL,alpha=NULL,
                      df.max=50,cf.max=100,proj.min=10,add.max=10,niter.max=30,qiter.max=10,aiter.max=100,
                      b.eps=1e-6,k.eps=1e-4,c.eps=1e-6,cut=TRUE,local=FALSE,local.initial=NULL){
    family = match.arg(family); penalty = match.arg(penalty)
    fit = ncpen(y.vec,x.mat,family,penalty,
                x.standardize,intercept,
                lambda,n.lambda,r.lambda,w.lambda,gamma,tau,alpha,
                df.max,cf.max,proj.min,add.max,niter.max,qiter.max,aiter.max,
                b.eps,k.eps,c.eps,cut,local,local.initial) 
    if(is.null(fold.id)){
      if(family=="cox"){ c.vec = x.mat[,dim(x.mat)[2]] } else { c.vec = y.vec }
      idx = fold.cv.ncpen(c.vec,n.fold,family)$idx
    } else { idx = fold.id }
    if(min(table(idx))<4) stop("at least 3 samples in each fold")
    rmse = like = list()
    for(s in 1:n.fold){
      #cat("cv fold number:",s,"\n")
      yset = idx==s; xset = yset; if(family=="multinomial"){ oxset = xset; xset = rep(xset,max(fit$y.vec)-1) } 
      fold.fit = native_cpp_ncpen_fun_(fit$y.vec[!yset],fit$x.mat[!xset,],
                                       fit$w.lambda,fit$lambda,fit$gamma,fit$tau,fit$alpha,
                                       df.max,niter.max,qiter.max,aiter.max,
                                       b.eps,k.eps,proj.min,cut,c.eps,add.max,family,penalty,
                                       local,fit$local.initial,cf.max)
      if(x.standardize==TRUE){ fold.fit$beta = fold.fit$beta/fit$std }
      fold.fit$intercept = fit$intercept; fold.fit$family = family; fold.fit$y.vec = fit$y.vec[!yset]
      if(family!="multinomial"){
        if(fit$int==TRUE){ 
          rmse[[s]] = predict.ncpen(fold.fit,type="rmse",new.y.vec=fit$y.vec[yset],new.x.mat=fit$x.mat[xset,-1])
          like[[s]] = predict.ncpen(fold.fit,type="like",new.y.vec=fit$y.vec[yset],new.x.mat=fit$x.mat[xset,-1])
        } else {
          rmse[[s]] = predict.ncpen(fold.fit,type="rmse",new.y.vec=fit$y.vec[yset],new.x.mat=fit$x.mat[xset,])
          like[[s]] = predict.ncpen(fold.fit,type="like",new.y.vec=fit$y.vec[yset],new.x.mat=fit$x.mat[xset,])
        }
      } else {
        if(fit$int==TRUE){ 
          rmse[[s]] = predict.ncpen(fold.fit,type="rmse",new.y.vec=fit$y.vec[yset],
                                    new.x.mat=fit$x.mat[1:length(oxset),][oxset,2:(dim(x.mat)[2]+1)])
          like[[s]] = predict.ncpen(fold.fit,type="like",new.y.vec=fit$y.vec[yset],
                                    new.x.mat=fit$x.mat[1:length(oxset),][oxset,2:(dim(x.mat)[2]+1)])
        } else {
          rmse[[s]] = predict.ncpen(fold.fit,type="rmse",new.y.vec=fit$y.vec[yset],
                                    new.x.mat=fit$x.mat[1:length(oxset),][oxset,1:dim(x.mat)[2]])
          like[[s]] = predict.ncpen(fold.fit,type="like",new.y.vec=fit$y.vec[yset],
                                    new.x.mat=fit$x.mat[1:length(oxset),][oxset,1:dim(x.mat)[2]])
        }
      }
    }
    nlam = min(unlist(lapply(rmse,length)))
    rmse = rowSums(matrix(unlist(lapply(rmse,function(x) x[1:nlam])),ncol=n.fold))
    nlam = min(unlist(lapply(like,length)))
    like = rowSums(matrix(unlist(lapply(like,function(x) x[1:nlam])),ncol=n.fold))
    ret = list(ncpen.fit=fit,
               fold.index = idx,
               rmse = rmse,
               like = like,
               lambda = fit$lambda[1:nlam]
    )
    class(ret) = "cv.ncpen"
    return(ret)
  }

  ##################################################################################################################################
  ##################################################################################################################################
  ##################################################################################################################################
  ###### ncpen.cv accessories  #####
  
  coef.cv.ncpen = function(cv.fit,type=c("rmse","like")){
    type = match.arg(type)
    if(type=="rmse"){ opt = which.min(cv.fit$rmse) }
    if(type=="like"){ opt = which.min(cv.fit$like) }
    beta=cv.fit$ncpen.fit$beta[,opt]
    if(cv.fit$ncpen.fit$fam=="multinomial"){ k = max(cv.fit$ncpen.fit$y.vec); beta = cbind(matrix(beta,ncol=k-1),0) } 
    return(list(type=match.arg(type),lambda=cv.fit$ncpen.fit$lambda[opt],beta=beta))
  }
  
  plot.cv.ncpen = function(cv.fit,type=c("rmse","like"),log.scale=FALSE,...){
    type = match.arg(type)
    if(type=="rmse"){
      opt = which.min(cv.fit$rmse)
      plot(cv.fit$lambda,cv.fit$rmse,main="root mean square error",type="b",pch="*")
    }
    if(type=="like"){
      opt = which.min(cv.fit$like)
      plot(cv.fit$lambda,cv.fit$like,main="negative log-likelihood",type="b",pch="*")
    }
    abline(v=cv.fit$lambda[opt])
  }

  sam.gen.ncpen = function(n=100,p=50,q=10,k=3,r=0.3,cf.min=0.5,cf.max=1,corr=0.5,
                           family=c("gaussian","binomial","multinomial","cox","poisson")){
    family = match.arg(family)
    co = corr^(abs(outer(c(1:p),c(1:p),"-"))); chco = chol(co)
    x.mat = matrix(rnorm(n*p),n,p)%*%t(chco)
    b.vec = seq(cf.max,cf.min,length.out=q)*(-1)^(1:q); b.vec = c(b.vec,rep(0,p-q))
    xb.vec = pmin(as.vector(x.mat%*%b.vec),700); exb.vec = exp(xb.vec)
    if(family=="gaussian"){ y.vec = xb.vec + rnorm(n) }
    if(family=="binomial"){ p.vec = exb.vec/(1+exb.vec); y.vec = rbinom(n,1,p.vec) }
    if(family=="poisson"){ m.vec = exb.vec; y.vec = rpois(n,m.vec) }
    if(family=="multinomial"){ 
      b.mat = matrix(0,p,k-1)
      for(i in 1:(k-1)) b.mat[1:q,i] = sample(seq(cf.max,cf.min,length.out=q)*(1)^(1:q))
      xb.mat = pmin(x.mat%*%b.mat,700); exb.mat = exp(xb.mat)
      p.mat = exb.mat / (1+rowSums(exb.mat))
      p.mat0 = cbind(p.mat,1-rowSums(p.mat))
      y.vec = rep(0,n)
      for(i in 1:n){ y.vec[i] = sample(1:k,1,prob=p.mat0[i,]) }
      b.vec = b.mat
    }
    if(family=="cox"){ 
      u = runif(n,0,1); y.vec = -log(u)/exb.vec
      cen = rbinom(n,prob=r,size=1); x.mat = cbind(x.mat,1-cen)
    }
    return(list(x.mat=x.mat,y.vec=y.vec,b.vec=b.vec))
  }
  
  
  