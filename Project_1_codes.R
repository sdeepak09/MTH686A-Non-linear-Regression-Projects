## Starting 04-02-2017
## Model we have y(t) = 1.5 + 2.0t + epsilon_(t);

lse=function(dsgn_mat,resp_var){
  if(!is.matrix(dsgn_mat)){
    dsgn_mat=as.matrix(dsgn_mat)
  }
  if(!is.vector(resp_var)){
    resp_var=as.vector(resp_var)
  }
  est_coef=solve(t(dsgn_mat)%*%dsgn_mat)%*%t(dsgn_mat)%*%resp_var
  return(as.numeric(est_coef))
}

q1=function(n,r,true_a,true_b,prntile){
  results=list()
  est_coefs=list()
  mean_bias=list()
  mean_sqrd_error=list()
  percentile_points_for_a=list()
  percentile_points_for_b=list()
  true_coef_mat=data.frame(true_a=c(rep(true_a,r)),true_b=c(rep(true_b,r)))
  true_coef_mat=as.matrix(true_coef_mat)
  for(i in 1:length(n)){
    est_coef=matrix(,nrow = r,ncol = 2)
    est_coef_names=paste0("est_coef_for_n=",n[i])
    mean_bias_names=paste0("mean_bias_for_n=",n[i])
    mean_sqr_names=paste0("mean_sqrd_error_for_n=",n[i])
    percnt_names_a=paste0("percentile_points_for_a_for_n=",n[i])
    percnt_names_b=paste0("percentile_points_for_b_for_n=",n[i])
    for(j in 1:r){
      dsgn_mat_1=c(rep(1,n[i]))
      dsgn_mat_2=c(1:n[i])
      dsgn_mat=data.frame(dsgn_mat_1,dsgn_mat_2)
      dsgn_mat=as.matrix(dsgn_mat)
      epsn=rnorm(n[i],0,1)
      resp_vec=true_a*dsgn_mat[,1]+true_b*dsgn_mat[,2]+epsn
      est_coef[j,]=lse(dsgn_mat,resp_vec)
    }
    est_coefs[[est_coef_names]]=est_coef
    bias_mat=est_coef-true_coef_mat
    bias_2_mat=bias_mat^2
    mean_bias_vec=colMeans(bias_mat)
    mean_sqrd_vec=colMeans(bias_2_mat)
    mean_bias[[mean_bias_names]]=mean_bias_vec
    mean_sqrd_error[[mean_sqr_names]]=mean_sqrd_vec
    vec_for_a=est_coef[,1]
    ordrd_vec_for_a=sort(vec_for_a)
    perctl_vec_for_a=ordrd_vec_for_a[c(r*prntile)]
    percentile_points_for_a[[percnt_names_a]]=perctl_vec_for_a
    vec_for_b=est_coef[,2]
    ordrd_vec_for_b=sort(vec_for_b)
    perctl_vec_for_b=ordrd_vec_for_b[c(r*prntile)]
    percentile_points_for_b[[percnt_names_b]]=perctl_vec_for_b
  }
  results=list(est_coefs=est_coefs,mean_bias=mean_bias,mean_sqrd_error=mean_sqrd_error,percentile_points_for_a=percentile_points_for_a,percentile_points_for_b=percentile_points_for_b)
  return(results)
}


