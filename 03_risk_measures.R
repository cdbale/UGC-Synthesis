
### This program calculates the privacy risk ####

source("04_text_match.R")

lambda=LAMBDA             ## Specify Shrinkage parameter for data protection, from 02_ file
probabilities=read.csv("./precomputed_probabilities_text/precomputed_probabilities_reviews_raw.csv") ## precomputed probabilities for target t and review j

## Data Protection Options
Folders=c("None","State and Rating","State")#,"TextOnly","CharLimit")

for (P in 1:length(Folders)){  ## Large Loop, 
#P=1  

  fold=Folders[P]

  if (P==1){I=1;len=1}            ## No Protection, 1 database
  if (P==2){I=m;len=length(LAMBDA)}            ## Synthetic State AND Rating
  if (P==3){I=m;len=length(LAMBDA)}            ## Synthetic State Only
  if (P==4){  ## TextOnly rewriting protection
    mech_text=read.csv(paste("../data/MTURK/",data_targets$Name[t],"_Batch.csv",sep=""))
    II=dim(mech_text)[1]
    I=II
  }
  
  if (P==5){  ## Character Limit rewriting protection
    mech_text=read.csv(paste("../data/MTURK/",data_targets$Name[t],"_Batch_charlimit.csv",sep=""))
    II=dim(mech_text)[1]
    I=II
  }
  
  for (l in 1:len){
  
  probs6_text=probs6=matrix(0,nrow=dim(data2)[1],ncol=T)  ## matching probabilities of row j to target t
  trueprobs=as.data.frame(matrix(0,nrow=T,ncol=8))           ## No protection, row is target t, columns are privacy measures
 
  colnames(trueprobs)=rep(c("Name","FirstStage","Rank_First","SecondStage","Rank_Second","N","Lambda","Synthesis"),1)
  N=matrix(nrow=T,ncol=I)   ## Number of rows matching target t for simulation i
 
    ## Loop over all T targets to calculate risk for each target
    for (t in 1:T){  
      print(t)
      whois<-list()
      target=data_targets[t,]          ## select specific target t

        
      

          for (i in 1:I){
  
            
            
  
             ################################
             ### LOAD DATA   ###########
             ################################
              reference=cbind(seq(1,8),c("CA","FL","GA","IL","NC","NM","TX","VA"))
              reference=as.data.frame(reference)
              names(reference)=c("NumState","State")
              
            
              if (P==1){protected=data2}
              if (P==2){protected$Rating=syndata_Z_Y[[l]][[i]][,1];protected$State=reference[syndata_Z_Y[[l]][[i]][,2],2]}
              if (P==3){protected$Rating=syndata_Y[[l]][[i]][,1];protected$State=reference[syndata_Y[[l]][[i]][,2],2]}
              if (P==4){}
              if (P==5){}

          ######################################
          ##### State and Rating Matching ######
          ######################################
                            
          matches=matrix(0,nrow=dim(data2)[1],ncol=2)  ## indicator matrix of review j matching target on 2 variables
          matches[,1]=(ifelse(((protected$State)==(target$State)),1,0))
          matches[,2]=1
          #matches[,2]=(ifelse(((protected$Rating)==(target$Rating)),1,0))           ## this is for matching rating too, which is different
         
          totalmatches=rowSums(matches)                                               ## row sums.  
          whois[[i]]=which(totalmatches==2)                                           ## which rows match the target t? store it as a list
          N[t,i]=sum(ifelse((totalmatches==2),1,0),na.rm=T)                           ## total number of matches for a target, within simulation i
        
          }

        ################################
        ### TEXT MATCHING  ###########
        ################################
        
        pr_text=pr=matrix(0,nrow=dim(data2)[1],ncol=I)  ##  matrix of 821 rows by I simulations to store probabilities
        probabilities_t=probabilities[which(probabilities$target==t),1]
        
        
      
             for (i in 1:I){
           
              
           
                    popups=as.numeric(unlist(whois[[i]]))     ## take the matches and store as a vector
                        if (length(popups)>0){
                        
                        ## text matching
                        #rvw <- iconv(as.character(protected$Text[popups]), "UTF-8", "ASCII", sub = "")
                        #rvw_t <- iconv(as.character(data_targets$Text[t]), "UTF-8", "ASCII", sub = "")
                           
                             #if (P>=4){
                              #temp_text=mech_text$Answer.Q6MultiLineTextInput[i]
                              #rvw_t <- iconv(as.character(temp_text), "UTF-8", "ASCII", sub = "")
                            #}
                            
                        ## calculate probability from text matching
                        #prob_allvars <- second_stage_prob(rvw, rvw_t, bag_words = TRUE, stop_words = TRUE, stylometry = TRUE, punctuation = TRUE)
                        #prob_allvars=as.vector(prob_allvars[[1]])
                        
                        pr[popups,i]=1/N[t,i]                      ## set the probability for the matches to 1/N where N is number of matches
                        pr_text[popups,i]=probabilities_t[popups]/sum(probabilities_t[popups]) ## store the probabilities from the text matching
                        #pr_text[popups,i]=prob_allvars             ## store the probabilities from the text matching
                        
                        }
                        
                      }
     
        ################################
        ### TOTAL RISK EVALUATION   ####
        ################################

      probs6[,t]=(rowMeans(pr))              ## Average of 1/N probability across I simulations
      probs6_text[,t]=rowMeans(pr_text)      ## Average of text probability across I simulations
      
      nonzeros=which(probs6[,t]>0)           ## which rows j have non-zero 1/N probabilities
      nonzeros_text=which(probs6_text[,t]>0) ## which rows j have non-zero text probabilities
      
      trueprobs[t,1]=as.character(data_targets$Name)[t]
      trueprobs[t,2]=probs6[jtargets[t],t]
      trueprobs[t,4]=probs6_text[jtargets[t],t]
      
      trueprobs[t,3]=rank(-probs6[,t],ties.method="average")[jtargets[t]]           ## calculate rank of average 1/N probs
      trueprobs[t,5]=rank(-probs6_text[,t],ties.method="average")[jtargets[t]]      ## calculate rank of average text probs
        
      trueprobs[t,6]=length(nonzeros)   ## calculate N
      trueprobs[t,7]=LAMBDA[l]          ## save the lambda value from the synthetic data
      if (P==1){trueprobs[t,7]=0}          ## save the lambda value from the synthetic data
      trueprobs[t,8]=Folders[P]         ## save the lambda value from the synthetic data
      
      }
  ## cycle through l here, each value of lambda
  trueprobs[,2:7]=(round(trueprobs[,2:7],4))
  if (P==1){probability_results=trueprobs}
  if (P==2){probability_results=rbind(probability_results,trueprobs)}
  if (P==3){probability_results=rbind(probability_results,trueprobs)}

}
  ## cycle through P here
}

write.csv(probability_results,paste("output/",today,"/probability_results",m,"syntheses.csv",sep=""),row.names=FALSE)


#probability_results=tibble(probability_results)

#probability_results=read_excel("output/probability_results.xlsx",sheet="Sheet1")

#probability_results=read_excel("probability_results.xlsx",sheet="Sheet1")

pdf(paste("output/",today,"/State_SecondStageProbabilities_",m,"syntheses",".pdf",sep=""))

# The palette with grey:
cbPalette <- c("#E69F00", "#56B4E9", "#999999", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

Name_Lambda_prob_none <- probability_results %>% 
  filter(Synthesis=="None") %>%
  group_by(Name, Lambda, Synthesis) %>%
  ungroup() %>%
  mutate(Lambda=factor(Lambda))

Name_Lambda_prob_none$Confidential <- Name_Lambda_prob_none$SecondStage


Name_Lambda_prob <- probability_results %>% 
  filter(Synthesis=="State") %>%
  group_by(Name, Lambda, Synthesis) %>%
  ungroup() %>%
  mutate(Lambda=factor(Lambda))

# Name_Lambda_prob$Lambda <- c(rep("lambda = 1e-04", 9),
#                              rep("lambda = 0.03", 9),
#                              rep("lambda = 0.5", 9))
# Name_Lambda_prob$Lambda <- ordered(Name_Lambda_prob$Lambda, levels = c("lambda = 1e-04",
#                                                                       "lambda = 0.03",
#                                                                       "lambda = 0.5"))
s<-ggplot(Name_Lambda_prob, aes(SecondStage, Name)) +
  #geom_line(aes(group = Name)) +
  geom_point(data = Name_Lambda_prob_none, aes(size = "Confidential"),
             shape = 8, color = "#009E73") + 
  scale_colour_manual(values=cbPalette) + 
  theme_bw(base_family = "") +
  theme(legend.title=element_blank()) +
  geom_point(aes(color = factor(Lambda)),size=2) + xlim(0,.12) +
  labs(title = "State Only",x="Identification  Probability",y="First Name of Target Records")


 Name_Lambda_prob <- probability_results %>% 
   filter(Synthesis=="State and Rating") %>%
   group_by(Name, Lambda, Synthesis) %>%
   ungroup() %>%
   mutate(Lambda=factor(Lambda))
 
 
 sr<-ggplot(Name_Lambda_prob, aes(SecondStage, Name)) +
   #geom_line(aes(group = Name)) +
   geom_point(data = Name_Lambda_prob_none, aes(size = "Confidential"),
              shape = 8, color = "#009E73") + 
   scale_colour_manual(values=cbPalette) + 
   theme_bw(base_family = "") +
   theme(legend.title=element_blank()) +
   geom_point(aes(color = Lambda),size=2) + xlim(0,.12) +
   labs(title = "State and Rating",x="Identification  Probability",y="First Name of Target Records")
 
 #grid.arrange(s, sr, nrow = 2)
 #grid.arrange(sr, nrow = 2)
 grid.arrange(s, nrow = 2)
 dev.off()
 
 
 
 pdf(paste("output/",today,"/Rank_Second_",m,"syntheses",".pdf",sep=""))
 
 Name_Lambda_prob <- probability_results %>% 
   filter(Synthesis=="State") %>%
   group_by(Name, Lambda, Synthesis) %>%
   ungroup() %>%
   mutate(Lambda=factor(Lambda))
 
 s<-ggplot(Name_Lambda_prob, aes(Rank_Second, Name)) +
   #geom_line(aes(group = Name)) + 
   xlim(0,821) +
   geom_point(aes(color = Lambda),size=2) + 
   labs(title = "State Only",x="Identification  Probability Rank",y="First Name of Target Records")
 
 Name_Lambda_prob <- probability_results %>% 
   filter(Synthesis=="State and Rating") %>%
   group_by(Name, Lambda, Synthesis) %>%
   ungroup() %>%
   mutate(Lambda=factor(Lambda))
 
 
 sr<-ggplot(Name_Lambda_prob, aes(Rank_Second, Name)) +
   #geom_line(aes(group = Name)) +
   geom_point(aes(color = Lambda),size=2) + xlim(0,821) +
   labs(title = "State and Rating",x="Identification  Probability Rank",y="First Name of Target Records")
 
 
 Name_Lambda_prob <- probability_results %>% 
   filter(Synthesis=="None") %>%
   group_by(Name, Lambda, Synthesis) %>%
   ungroup() %>%
   mutate(Lambda=factor(Lambda))
 
 n<-ggplot(Name_Lambda_prob, aes(Rank_Second, Name)) +
   #geom_line(aes(group = Name)) +
   geom_point(aes(color = Lambda),size=2) + xlim(0,821) +
   labs(title = "No Synthesis",
        subtitle = "Confidential data.")
 
 grid.arrange(s, sr,n, nrow = 3)
 dev.off()
 
 