library(tm)
library(stm)
library(readr)
library(knitr)


#Folders=c("None","State and Rating","State")
prev=topicwords_p=avg_corr=list()


State=data2$State
Rating=ifelse(data2$Rating>3,"High","Low")
meta=data.frame(State,Rating)
names(meta)=c("State","Rating")

## Run STM on Confidential Data
stm <- stm(documents = mm$documents, vocab = mm$vocab, K = K, prevalence=~Rating+State,content=~State,max.em.its = iter,data=meta, init.type = "Spectral", verbose = FALSE)
topicwords=labelTopics(stm)$topics[,1:3]
prep <- estimateEffect(1:K ~ State+Rating,stm,metadata = meta, uncertainty = "Global")

prev_stm=matrix(0,nrow=K,ncol=4)
colnames(prev_stm)=c("High Rating Prevalence","Low Rating Coefficient","p-value","Square of Std Error")

for (k in 1:K){
prev_high=(summary(prep)$tables[[k]][1,1])                      ## High Rating prevenalnce
prev_low=(summary(prep)$tables[[k]][nlevels(State)+1,1])        ## Low Rating COEFFICIENT
prev_pvalue=summary(prep)$tables[[k]][(nlevels(State)+1),4]       ## p-value of difference
prev_var=(summary(prep)$tables[[k]][(nlevels(State)+1),2])^2       ## square of std error of COEFFICIENT

prev_stm[k,]=c(prev_high,prev_low,prev_pvalue,prev_var)

}

conf_plot <- plot(prep, model=stm, covariate="Rating", method="difference", cov.value1="High", cov.value2="Low", topic=1:K)
conf_cis <- do.call(rbind, conf_plot$cis)
write.csv(conf_cis, paste("output/", today, "/confidential_cis.csv", sep = ""))

write.csv(prev_stm,paste("output/",today,"/Prevalence_stm.csv",sep=""))

##################################################
### THIS RUNS STM for different protection #######
##################################################

for (P in 2:3){
avg_corr[[P-1]]=list()
topicwords_p[[P-1]]=list()
prev[[P-1]]=list()   #prevalences of Rating covariate

print(P)
  for (i in 1:length(LAMBDA)){
    l=LAMBDA[i]
    print(LAMBDA[i]);print("lambda")
    
  avg_corr[[P-1]][[i]]=list()
  topicwords_p[[P-1]][[i]]=list()
  prev[[P-1]][[i]]=list()
  
    
    for (ii in 1:m){
    print(ii);print("sim")

      if (P==2){data_p=syndata_Z_Y[[i]][[ii]]}
      if (P==3){data_p=syndata_Y[[i]][[ii]]}
    
      Rating_p=data_p[,1]
      State_p=reference[data_p[,2],2]
      Rating_p=factor(ifelse(Rating_p>3,"High","Low"))
      meta_p=data.frame(State_p,Rating_p)
      names(meta_p)=c("State","Rating")
          
      
      stm_p <- stm(documents = mm$documents, vocab = mm$vocab, K = K, prevalence=~Rating+State,content=~State,max.em.its = iter,data=meta_p, init.type = "Spectral", verbose = FALSE)
      
      wordtopic=wordtopic_p=list()  ## values of betas in content coefficients
      
      
      prev[[P-1]][[i]][[ii]]=matrix(0,nrow=K,ncol=4)
      colnames( prev[[P-1]][[i]][[ii]])=c("High Rating Prevalence","Low Rating Coefficient","p-value","Square Std Error")
      prep_p <- estimateEffect(1:K ~ State+Rating,stm_p,metadata = meta_p, uncertainty = "Global")
      
      for (k in 1:K){
      prev_high_p=(summary(prep_p)$tables[[k]][1,1])                      ## High Rating prevalence
      prev_low_p=summary(prep_p)$tables[[k]][nlevels(State)+1,1]          ## Low Rating coefficient
      prev_low_Var_p=(summary(prep_p)$tables[[k]][nlevels(State)+1,2])^2
      prev_pvalue_p=summary(prep_p)$tables[[k]][(nlevels(State)+1),4]       ## p-value of difference
      prev[[P-1]][[i]][[ii]][k,]=c(prev_high_p,prev_low_p,prev_pvalue_p,prev_low_Var_p)
      
      }
      
      
      corrs=matrix(nrow=8,ncol=K)   ## Matrix of 8 rows for State and 5 columns for topics. Entries are correlation of word usage between protected data and confidential data
      
      for (j in 1:8){
      wordtopic[[j]]=(t(exp(stm$beta$logbeta[[j]])))
      wordtopic_p[[j]]=(t(exp(stm_p$beta$logbeta[[j]])))
      
      
      corrs[j,]=diag(cor(cbind(wordtopic[[j]], wordtopic_p[[j]]))[1:5,6:10])  # model-based protection, K correlations for topics 
      }
      avg_corr[[P-1]][[i]][[ii]]=corrs
      topicwords_p[[P-1]][[i]][[ii]]=labelTopics(stm_p)$topics[,1:3]
    
    }
  
  }

}


### Calculations of analysis-specific utility ###

asu=list()

for (P in 2:3){
  asu[[P-1]]=as.data.frame(matrix(0,nrow=K*(length(LAMBDA)+1),ncol=4))  ## analysis-specific utility
  colnames(asu[[P-1]])=c("ASU","Lambda","Synthesis","Topic")
  
  top_avg=matrix(0,nrow=K,ncol=length(LAMBDA))
  prev_lambda=list()
  
  
  for (i in 1:length(LAMBDA)){
   
    
    prev_avg=matrix(0,nrow=K,ncol=10) 
    temp3=temp4=temp5=matrix(nrow=K,ncol=m)
    for (ii in 1:m){
      
      temp=prev[[P-1]][[i]][[ii]]   ## prevalences and p-value
      temp2=ifelse(temp[,3]<.05,1,0)
      prev_avg[,9]=prev_avg[,9]+temp2/m  ## for lambda i and P, average prevalance across m simulation for all topics, percent of times p-value <.05
      
      temp3[1:5,ii]=temp[,1]## high rating prevelance
      temp4[1:5,ii]=temp[,2]## low rating coefficient
      temp5[1:5,ii]=temp[,4]## square of standard error
      
    }
    prev_avg[,1]=rowMeans(temp3)  ## average of high rating , qbarm
    prev_avg[,2]=rowMeans(temp4)  ## average of low rating coefficients, qbarm
    prev_avg[,3]=apply(temp3, 1, var)  ## var of high rating , bm
    prev_avg[,4]=apply(temp4, 1, var)  ## var of low rating coefficients, bm
    
    prev_avg[,10]=rowMeans(temp5)  ## ubarm
    
    Tf_high=prev_avg[,3]/m+prev_avg[,10]
    Tf_low=prev_avg[,4]/m+prev_avg[,10]
    v_high=(m-1)*(1+prev_avg[,10]/(prev_avg[,3]/m))^2  ## degrees of freedom
    v_low=(m-1)*(1+prev_avg[,10]/(prev_avg[,4]/m))^2   ## degrees of freddom
    
    
    prev_avg[,5]=CI_Lower_high=prev_avg[,1] - qt(0.975, v_high)*sqrt(Tf_high)
    prev_avg[,6]=CI_Upper_high=prev_avg[,1] + qt(0.975, v_high)*sqrt(Tf_high)
    
    prev_avg[,7]=CI_Lower_low=prev_avg[,2] - qt(0.975, v_low)*sqrt(Tf_low)
    prev_avg[,8]=CI_Upper_low=prev_avg[,2] + qt(0.975, v_low)*sqrt(Tf_low)
    
    #Qbarm <- rowMeans(Qm)
    #Bm <- apply(Qm,1,var)
    #Ubarm <- rowMeans(Um)
    #Tf <- Bm/m + Ubarm
    #v <- (m-1)*(1+Ubarm/(Bm/m))^2
    #CI_Lower <- Qbarm - qt(0.975, v)*sqrt(Tf)
    #CI_Upper <- Qbarm + qt(0.975, v)*sqrt(Tf)
    
    
    
    colnames(prev_avg)=c("Avg High Rating Prevalence","Avg Low Rating Coefficient","Var High Rating Prevalence","Var Low Rating Prevalence","CI_Lower High Rating","CI_Upper High Rating","CI_Lower Low Rating","CI_Upper Low Rating","p-value percent","Avg of Var of Low Rating Coef")
    prev_lambda[[i]]=prev_avg
    
    for (k in 1:K){
    asu[[P-1]][(k-1)*(length(LAMBDA)+1)+i,]=c(colMeans(t(sapply(avg_corr[[P-1]][[i]],colMeans)))[k],LAMBDA[i],Folders[P],combine_words(topicwords[k,],and=""))    ## average over the levels of state and M 
    asu[[P-1]][(k)*(length(LAMBDA)+1),]=c(1,0,Folders[1],combine_words(topicwords[k,],and=""))    ## average over the levels of state and M  
   
    
    
    top_total=0  
    for (top in 1:3){  
      run_total=0
            for (ii in 1:m){
           temp=sum(str_count(topicwords_p[[P-1]][[i]][[ii]][k,],topicwords[k,top]))  ## count the number of times in the top 3
           run_total=run_total+temp
            }
      top_total=top_total+run_total ## number of times the top 3 words appear in m simulations for topic k lambda i 
          
    }
    top_avg[k,i]=round(top_total/(3*m),3)
    colnames(top_avg)=paste(LAMBDA,"_Lambda",sep="")
    }
    
    write.csv(prev_lambda[[i]],paste("output/",today,"/Prevalence_",m,"syntheses_",Folders[P],LAMBDA[i],"_Lambda",".csv",sep=""))
    
  }
  
  write.csv(top_avg,paste("output/",today,"/Top3_TopicWords_",m,"syntheses_",Folders[P],".csv",sep=""))
  
  
}


write.csv(topicwords,paste("output/",today,"/Top3_TopicWords.csv",sep=""))

P=2
asu_results=rbind(asu[[P-1]],asu[[P]])
asu_results$ASU=as.numeric(asu_results$ASU)


write.csv(asu_results,paste("output/",today,"/AnalysisSpecificUtility_",m,"syntheses",".csv",sep=""))

pdf(paste("output/",today,"/AnalysisSpecificUtility_",m,"syntheses",".pdf",sep=""))

# The palette with grey:
cbPalette <- c("#E69F00", "#56B4E9", "#999999", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

temp_level=factor(asu_results$Topic,levels=levels(factor(asu_results$Topic))[c(4,1,2,5,3)])

asu_results$Topics=temp_level

#temp <- asu_results$Lambda

asu_results$Lambda <- as.character(round(as.numeric(asu_results$Lambda), 4))
LAMBDA <- round(LAMBDA, 4)

Name_Lambda_prob <- asu_results %>% 
  filter(Synthesis=="State") %>%
  group_by(Lambda, Synthesis,Topic) %>%
  ungroup() %>%
  mutate(Lambda=factor(Lambda,levels(factor(asu_results$Lambda))[c(1,4,2,3)]),Topic=factor(Topic,levels(factor(asu_results$Topic))[c(4,1,2,5,3)]))

Name_Lambda_prob_none <- asu_results %>% 
  filter(Synthesis=="None") %>%
  group_by(Lambda, Synthesis,Topic) %>%
  ungroup() %>%
  mutate(Lambda=factor(Lambda,levels(factor(asu_results$Lambda))[c(1,4,2,3)]),Topic=factor(Topic,levels(factor(asu_results$Topic))[c(4,1,2,5,3)]))

Name_Lambda_prob_none$Confidential <- Name_Lambda_prob_none$ASU


s<-ggplot(Name_Lambda_prob, aes(ASU, Topic)) +
  #geom_line(aes(group = Topic)) +
  geom_point(data = Name_Lambda_prob_none, aes(size = "Confidential"),
             shape = 8, color = "#009E73") + 
  scale_colour_manual(values=cbPalette) + 
  theme_bw(base_family = "") +
  theme(legend.title=element_blank()) +
  geom_point(aes(color = factor(Lambda, levels = c(paste(LAMBDA[1]),paste(LAMBDA[2]),paste(LAMBDA[3])))),size=2) + xlim(.4,1) +
  labs(title = "State Only",x="Average Correlation")

Name_Lambda_prob <- asu_results %>% 
  filter(Synthesis=="State and Rating") %>%
  group_by(Lambda, Synthesis) %>%
  ungroup() %>%
  mutate(Lambda=factor(Lambda,levels(factor(asu_results$Lambda))[c(1,4,2,3)]),Topic=factor(Topic,levels(factor(asu_results$Topic))[c(4,1,2,5,3)]))


sr<-ggplot(Name_Lambda_prob, aes(ASU, Topic)) +
  #geom_line(aes(group = Topic)) +
  geom_point(data = Name_Lambda_prob_none, aes(size = "Confidential"),
             shape = 8, color = "#009E73") + 
  scale_colour_manual(values=cbPalette) + 
  theme_bw(base_family = "") +
  theme(legend.title=element_blank()) +
  geom_point(aes(color = factor(Lambda, levels = c(paste(LAMBDA[1]),paste(LAMBDA[2]),paste(LAMBDA[3])))),size=2) + xlim(.4,1) +
  labs(title = "State and Rating",x="Average Correlation")

#grid.arrange(s, sr, nrow = 2)
#grid.arrange(sr, nrow = 2)
grid.arrange(s, nrow = 2)
dev.off()

    ## Post Analysis ##
    
    
    #prep <- estimateEffect(1:K ~ State+Rating,stm,meta = meta, uncertainty = "Global")
    #plot(prep, model=stm, covariate="State", method="difference", cov.value1="NM", cov.value2="GA", topic=1:K)
    #plot(prep, model=stm, covariate="Rating", method="difference", cov.value1="High", cov.value2="Low", topic=1:K)
    #plot(stm)
    #summary(prep)
    
    
    ##
    #topic=3
    #state=j
    #levels(meta3$State)
    #expbeta=wordtopic[[j]][,topic]
    #aa=as.data.frame(cbind(expbeta,stm$vocab))
    #o=order(expbeta,decreasing=TRUE)[1:20]
    #bestwords=data.frame(stm$vocab[o],expbeta[o])
    #names(bestwords)=c("Word","Probability of Word in Topic 2")
    #bestwords
    # theta is for a specific document i...has topic proportions
    ## stm$theta[i,]
    #plot(stm, type = "summary", xlim = c(0, .3))
    # plot(prep, "Rating", method = "pointestimate", topics = 1:5,model = stm, printlegend = FALSE, xlab = "Prevalence")
    
    #labelTopics(stm)$topics[,1:3]
    
    #topicwords=labelTopics(stm)$topics[,1:3]
        
    
    #logbeta=stm$beta$logbeta
    #vocab <- stm$vocab
    #topics <- 1:nrow(logbeta[[1]])
    #aspect <- length(logbeta) > 1
    #out <- list()
    
    ## 53 is the number of covariates.  8*5 + 8+5
    #labs <- lapply(stm$beta$kappa$params, function(x) {
    #  windex <- order(x, decreasing = TRUE)[1:n]
    #  ifelse(x[windex] > 0.001, vocab[windex], "")
    #})
    
    #labs <- do.call(rbind, labs)
    
    #A <- stm$settings$dim$A
    #anames <- stm$settings$covariates$yvarlevels
    #i1 <- K + 1
    #i2 <- K + A
    #intnums <- (i2 + 1):nrow(labs)
    #out$topics <- labs[topics, , drop = FALSE]  ## labs is 53 x 821, take first 5= topics only
    #out$covariate <- labs[i1:i2, , drop = FALSE]
    #rownames(out$covariate) <- anames
    #if (stm$settings$kappa$interactions) {
    #  tindx <- rep(1:K, each = A)
    #  intnums <- intnums[tindx %in% topics]
    #  out$interaction <- labs[intnums, , drop = FALSE]
    #}
    #out$topicnums <- topics
    
    #out$topics[,1:3]  5 x 3


  
