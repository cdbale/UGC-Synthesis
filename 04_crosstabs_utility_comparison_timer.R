library(reshape2)

Utility <- function(origdata, syndata, m, n, dj, p){
  origdata = origdata  ## this is n x p  where p is the number of variables
  syndata = syndata   ## this is a list.  each list is n x p
  m = m               ##  the number of lists
  n = n               ## the number of records
  dj = dj             ## the number of categories
  p = p               ## the number of variables
  
  ########################## one-way table ################################
  ColIndex1 = combn(p, 1)
  dimension1 = dim(ColIndex1)[2]
  origdata_results1 = rep(NA, sum(dj))
  syn_results1_all = matrix(rep(NA, sum(dj)*m), ncol = m)
  for (i in 1:dimension1){
    origdata_results1[(sum(dj[1:i])-dj[i]+1):(sum(dj[1:i]))] = as.data.frame(table(factor(origdata[,ColIndex1[,i]], levels = 1:dj[i])))$Freq
    for (k in 1:m){
      syndata_k = syndata[[k]]
      syn_results1_all[(sum(dj[1:i])-dj[i]+1):(sum(dj[1:i])), k] = as.data.frame(table(factor(syndata_k[,ColIndex1[,i]], levels = 1:dj[i])))$Freq
    }
  }
  diff_1 = matrix(rep(NA, sum(dj)*m), ncol = m)
  for (k in 1:m){
    diff_1[, k] = origdata_results1/100 - syn_results1_all[, k]/100
  }
  
  ########################## two-way table ################################
  ColIndex2 = combn(p, 2)
  dimension2 = dim(ColIndex2)[2]
  level_matrix2 = ColIndex2
  for (j in 1:dimension2){
    level_matrix2[,j] = dj[ColIndex2[,j]]
  }
  prod_vector2 = apply(level_matrix2, 2, prod)
  origdata_results2 = rep(NA, sum(prod_vector2))
  syn_results2_all = matrix(rep(NA, sum(prod_vector2)*m), ncol = m)
  for (i in 1:dimension2){
    origdata_results2[(sum(prod_vector2[1:i])-prod_vector2[i]+1):(sum(prod_vector2[1:i]))] = as.data.frame(table(factor(as.data.frame(origdata[,ColIndex2[,i]])[,1], levels = 1:dj[ColIndex2[1,i]]), 
                                                                                                                 factor(as.data.frame(origdata[,ColIndex2[,i]])[,2], levels = 1:dj[ColIndex2[2,i]])))$Freq
    for (k in 1:m){
      syndata_k = syndata[[k]]
      syn_results2_all[(sum(prod_vector2[1:i])-prod_vector2[i]+1):(sum(prod_vector2[1:i])), k] = as.data.frame(table(factor(as.data.frame(syndata_k[,ColIndex2[,i]])[,1], levels = 1:dj[ColIndex2[1,i]]), 
                                                                                                                     factor(as.data.frame(syndata_k[,ColIndex2[,i]])[,2], levels = 1:dj[ColIndex2[2,i]])))$Freq
    }
  }
  diff_2 = matrix(rep(NA, sum(prod_vector2)*m), ncol = m)
  for (k in 1:m){
    diff_2[, k] = origdata_results2/100 - syn_results2_all[, k]/100
  }

  
  res_u <- list(diff_1 = diff_1,
                diff_2 = diff_2
  )
  return(res_u)
}


##################################### model 1 syndata ######################################
## UPDATE THE INPUTS
n=dim(origdata)[1]
dj=c(5,8)
p=dim(origdata)[2]

model1=model2=list()

for (i in 1:length(LAMBDA)){
model1[[i]] <- Utility(origdata, Y_syn_priv[[33]], m, n, dj, p)
model2[[i]] <- Utility(origdata, syndata_Z_Y[[i]], m, n, dj, p)
}
# The palette with grey:
cbPalette <- c("#E69F00", "#56B4E9", "#999999", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

pdf(paste("output/",today,"/","50_state_rating_combined_Utility Comparison_",m,"syntheses.pdf",sep=""))

par(mfrow=c(2,2))

################################################################################################
plot(density(model1[[1]]$diff_1[6:13,],bw=.002), main = "one-way, State Only",col=cbPalette[1],lty=1,xlim=xaxis,ylim=c(0,160),xlab="cell-level relative difference")

  for (i in 1:length(LAMBDA)){
  ## must remove the 0s for model 1
  lines(density(model1[[i]]$diff_1[6:13,],bw=.002), col = cbPalette[i])

  }

legend("topright","(x,y)",round(LAMBDA,4), lty=c(rep(1,length(LAMBDA)),rep(2,length(LAMBDA))),col=cbPalette[1:length(LAMBDA)], title = "Lambda")

###################################################################################################
###################################################################################################
# ds1 <- c()
# for (i in 1:length(LAMBDA)) ds1 <- c(ds1, model1[[i]]$diff_1[6:13])
# dm1 <- tibble(
#   Lambda = sort(rep(LAMBDA, 8)),
#   Diff = ds1
# )
# 
# p1 <- dm1 %>% ggplot(aes(x = Diff, fill = factor(round(Lambda,4)))) + 
#   geom_density(alpha = 0.5) + 
#   xlim(c(-1,1)) + 
#   ylim(c(0,40)) +
#   labs(title = "One Way, State Only",
#        x = "cell-level relative differences",
#        y = "Density",
#        fill = "Lambda") +
#   scale_fill_manual(values = cbPalette[1:length(LAMBDA)]) +
#   theme(legend.position = c(0.1,0.8))

###################################################################################################
###################################################################################################

plot(density(model2[[1]]$diff_1[6:13,],bw=.002), main = "one-way, State and Rating",col=cbPalette[1],lty=1,xlim=xaxis,ylim=c(0,160),xlab="cell-level relative difference")

for (i in 1:length(LAMBDA)){
  ## must remove the 0s for model 1

  lines(density(model2[[i]]$diff_1[6:13,],bw=.002), lty=1,col = cbPalette[i])
}

legend("topright","(x,y)",round(LAMBDA,4), lty=c(rep(1,length(LAMBDA)),rep(2,length(LAMBDA))),col=cbPalette[1:length(LAMBDA)], title = "Lambda")

###################################################################################################
###################################################################################################
# ds2 <- c()
# for (i in 1:length(LAMBDA)) ds2 <- c(ds2, model2[[i]]$diff_1[6:13])
# dm2 <- tibble(
#   Lambda = sort(rep(LAMBDA, 8)),
#   Diff = ds2
# )
# 
# p2 <- dm2 %>% ggplot(aes(x = Diff, fill = factor(round(Lambda,4)))) + 
#   geom_density(alpha = 0.5) + 
#   xlim(c(-1,1)) + 
#   ylim(c(0,40)) +
#   labs(title = "One Way, State State and Rating",
#        x = "cell-level relative differences",
#        y = "Density",
#        fill = "Lambda") +
#   scale_fill_manual(values = cbPalette[1:length(LAMBDA)]) +
#   theme(legend.position = c(0.1,0.8))

###################################################################################################
###################################################################################################

plot(density(model1[[1]]$diff_2,bw=.002), main = "two-way, State Only",xlim=xaxis,ylim=c(0,160),lty=1,col=cbPalette[1],xlab="cell-level relative difference")

for (i in 1:length(LAMBDA)){
  
  lines(density(model1[[i]]$diff_2,bw=.002), col = cbPalette[i])

}

legend("topright","(x,y)",round(LAMBDA,4), lty=c(rep(1,length(LAMBDA)),rep(2,length(LAMBDA))),col=cbPalette[1:length(LAMBDA)], title = "Lambda")

###################################################################################################
###################################################################################################
# ds3 <- c()
# for (i in 1:length(LAMBDA)) ds3 <- c(ds3, model1[[i]]$diff_2)
# dm3 <- tibble(
#   Lambda = sort(rep(LAMBDA, 40)),
#   Diff = ds3
# )
# 
# p3 <- dm3 %>% ggplot(aes(x = Diff, fill = factor(round(Lambda,4)))) + 
#   geom_density(alpha = 0.5) + 
#   xlim(c(-1,1)) + 
#   ylim(c(0,115)) +
#   labs(title = "Two Way, State Only",
#        x = "cell-level relative differences",
#        y = "Density",
#        fill = "Lambda") +
#   scale_fill_manual(values = cbPalette[1:length(LAMBDA)]) +
#   theme(legend.position = c(0.1,0.8))

###################################################################################################
###################################################################################################

plot(density(model2[[1]]$diff_2,bw=.002), main = "two-way, State and Rating",xlim=xaxis,ylim=c(0,160),lty=1,col=cbPalette[1],xlab="cell-level relative difference")
for (i in 1:length(LAMBDA)){

  lines(density(model2[[i]]$diff_2,bw=.002), lty=1, col = cbPalette[i])
}

legend("topright","(x,y)",round(LAMBDA,4), lty=c(rep(1,length(LAMBDA)),rep(2,length(LAMBDA))),col=cbPalette[1:length(LAMBDA)], title = "Lambda")

###################################################################################################
###################################################################################################
# ds4 <- c()
# for (i in 1:length(LAMBDA)) ds4 <- c(ds4, model2[[i]]$diff_2)
# dm4 <- tibble(
#   Lambda = sort(rep(LAMBDA, 40)),
#   Diff = ds4
# )
# 
# p4 <- dm4 %>% ggplot(aes(x = Diff, fill = factor(round(Lambda,4)))) + 
#   geom_density(alpha = 0.5) + 
#   xlim(c(-1,1)) + 
#   ylim(c(0,115)) +
#   labs(title = "Two Way, State and Rating",
#        x = "cell-level relative differences",
#        y = "Density",
#        fill = "Lambda") +
#   scale_fill_manual(values = cbPalette[1:length(LAMBDA)]) +
#   theme(legend.position = c(0.1,0.8))


#grid.arrange(p1,p2,p3,p4, nrow=2)
###################################################################################################
###################################################################################################

dev.off()


## combine all m = 20 synthetic datasets
abs_diff=matrix(nrow=length(LAMBDA),ncol=4)
rownames(abs_diff)=(paste(LAMBDA,"Lambda"))
colnames(abs_diff)=c("State Only Absolute Diff 1","State and Rating Absolute Diff 1","State Only Absolute Diff 1","State and Rating Absolute Diff 2")

for (i in 1:length(LAMBDA)){
abs_diff[i,1]=sum(abs(model1[[i]]$diff_1[6:13,]))/m
abs_diff[i,2]=sum(abs(model2[[i]]$diff_1[6:13,]))/m

abs_diff[i,3]=sum(abs(model1[[i]]$diff_2))/m
abs_diff[i,4]=sum(abs(model2[[i]]$diff_2))/m
}

abs_diff=round(abs_diff,2)

write.csv(abs_diff,paste("output/",today,"/","Absolute Differences of Utility_",m,"syntheses.csv",sep=""))

Lambda=rep(LAMBDA,4)
Label=rep(c("one-way, State Only","one-way, State and Rating","two-way, State Only","two-way, State and Rating"),each=3)
Difference=c(abs_diff[,1],abs_diff[,2],abs_diff[,3],abs_diff[,4])


abs_diff2=data.frame(Lambda,Label,Difference)
names(abs_diff2)=c("Lambda","Label","Difference")

pdf(paste("output/",today,"/","Absolute Differences_",m,"syntheses.pdf",sep=""))

abs2 <- abs_diff2 %>% 
   group_by(Label) %>%
  ungroup() %>%
  mutate(Lambda=factor(Lambda))

ggplot(abs2, aes(Difference, factor(Label,levels=rev(c("one-way, State Only","one-way, State and Rating","two-way, State Only","two-way, State and Rating"))))) +
  geom_line(aes(group = Label)) + xlim(0,10) +
  scale_colour_manual(values=cbPalette) + 
  theme_bw(base_family = "") + 
  theme(legend.title=element_blank()) +
  geom_point(aes(color = Lambda),size=2) + 
  labs(title = "Average Absolute Differences to Confidential Data",y='')



dev.off()
