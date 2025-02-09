### Historical simulation 

hs_f<-function(rt,w,tau,begin_period,end_period){

myDate_b <- as.Date(time(first(rt[begin_period])))
myDate_e <- as.Date(time(last(rt[end_period])))

beg_sample<-which(as.Date(time(rt))==myDate_b) - w # new begin

beg_sample_f<-which(as.Date(time(rt))==myDate_b)

end_sample<-which(as.Date(time(rt))==myDate_e)   # end


r_t_hs<-coredata(rt[beg_sample:end_sample])

VaR_HS<-list()
ES_HS<-list()

for(tt in (w+1):length(r_t_hs)){

ret<-r_t_hs[(tt-w):(tt-1)]

VaR_HS[[tt]]<-quantile(ret,tau)
Ind<-ifelse(ret<=rep(VaR_HS[[tt]],w),1,0)

ES_HS[[tt]]<-sum(ret*Ind)/sum(Ind)

}

VaR_HS<-unlist(VaR_HS)
VaR_HS<-as.xts(VaR_HS,time(rt[beg_sample_f:end_sample]))

ES_HS<-unlist(ES_HS)
ES_HS<-as.xts(ES_HS,time(rt[beg_sample_f:end_sample]))

### final results


res<-list(
VaR_HS=VaR_HS,
ES_HS=ES_HS)


return(res)

}

########################## Quantile loss


QL<-function(ret,VAR,alpha){

Ind<-ifelse(ret<=VAR,1,0)

return((ret-VAR)*(alpha-Ind))

}
