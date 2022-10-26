
model1_cart=model2_cart=list()

for (i in 1:length(CART_CP)){
  model1_cart[[i]] <- Utility(origdata, CART_Y[[i]], m, n, dj, p)
  model2_cart[[i]] <- Utility(origdata, CART_Z_Y[[i]], m, n, dj, p)
}

pdf(paste("output/","CART ",today,"/","CART Utility Comparison_",m,"syntheses.pdf",sep=""))

par(mfrow=c(2,2))

plot(density(model1_cart[[1]]$diff_1[6:13,],bw=.002), main = "one-way, State Only",col=cbPalette[1],lty=1,xlim=xaxis,ylim=c(0,40),xlab="cell-level relative difference")

for (i in 1:length(CART_CP)){
  ## must remove the 0s for model 1 
  lines(density(model1_cart[[i]]$diff_1[6:13,],bw=.002), col = cbPalette[i])
  
}

#legend("topright","(x,y)",c(round(CART_CP[1:2],8), round(CART_CP[3],4)), lty=c(rep(1,length(CART_CP)),rep(2,length(CART_CP))),col=cbPalette[1:length(CART_CP)],title="Alpha")
legend("topright","(x,y)",c(round(CART_CP[1],8), round(CART_CP[2], 4), round(CART_CP[3],4)), lty=c(rep(1,length(CART_CP)),rep(2,length(CART_CP))),col=cbPalette[1:length(CART_CP)],title="Alpha")

plot(density(model2_cart[[1]]$diff_1[6:13,],bw=.002), main = "one-way, State and Rating",col=cbPalette[1],lty=1,xlim=xaxis,ylim=c(0,40),xlab="cell-level relative difference")

for (i in 1:length(CART_CP)){
  ## must remove the 0s for model 1 
  
  lines(density(model2_cart[[i]]$diff_1[6:13,],bw=.002), lty=1,col = cbPalette[i])
}

#legend("topright","(x,y)",c(round(CART_CP[1:2],8), round(CART_CP[3],4)), lty=c(rep(1,length(CART_CP)),rep(2,length(CART_CP))),col=cbPalette[1:length(CART_CP)],title="Alpha")
legend("topright","(x,y)",c(round(CART_CP[1],8), round(CART_CP[2], 4), round(CART_CP[3],4)), lty=c(rep(1,length(CART_CP)),rep(2,length(CART_CP))),col=cbPalette[1:length(CART_CP)],title="Alpha")

plot(density(model1_cart[[1]]$diff_2,bw=.002), main = "two-way, State Only",xlim=xaxis,ylim=c(0,40),lty=1,col=cbPalette[1],xlab="cell-level relative difference")
for (i in 1:length(CART_CP)){
  lines(density(model1_cart[[i]]$diff_2,bw=.002), col = cbPalette[i])
  
}

#legend("topright","(x,y)",c(round(CART_CP[1:2],8), round(CART_CP[3],4)), lty=c(rep(1,length(CART_CP)),rep(2,length(CART_CP))),col=cbPalette[1:length(CART_CP)],title="Alpha")
legend("topright","(x,y)",c(round(CART_CP[1],8), round(CART_CP[2], 4), round(CART_CP[3],4)), lty=c(rep(1,length(CART_CP)),rep(2,length(CART_CP))),col=cbPalette[1:length(CART_CP)],title="Alpha")

plot(density(model2_cart[[1]]$diff_2,bw=.002), main = "two-way, State and Rating",xlim=xaxis,ylim=c(0,40),lty=1,col=cbPalette[1],xlab="cell-level relative difference")
for (i in 1:length(CART_CP)){
  
  lines(density(model2_cart[[i]]$diff_2,bw=.002), lty=1, col = cbPalette[i])
}

#legend("topright","(x,y)",c(round(CART_CP[1:2],8), round(CART_CP[3],4)), lty=c(rep(1,length(CART_CP)),rep(2,length(CART_CP))),col=cbPalette[1:length(CART_CP)],title="Alpha")
legend("topright","(x,y)",c(round(CART_CP[1],8), round(CART_CP[2], 4), round(CART_CP[3],4)), lty=c(rep(1,length(CART_CP)),rep(2,length(CART_CP))),col=cbPalette[1:length(CART_CP)],title="Alpha")

dev.off()


## combine all m = 20 synthetic datasets
abs_diff_cart=matrix(nrow=length(CART_CP),ncol=4)
rownames(abs_diff_cart)=(paste(CART_CP,"CP"))
colnames(abs_diff_cart)=c("State Only Absolute Diff 1","State and Rating Absolute Diff 1","State Only Absolute Diff 1","State and Rating Absolute Diff 2")

for (i in 1:length(CART_CP)){
  abs_diff_cart[i,1]=sum(abs(model1_cart[[i]]$diff_1[6:13,]))/m
  abs_diff_cart[i,2]=sum(abs(model2_cart[[i]]$diff_1[6:13,]))/m
  
  abs_diff_cart[i,3]=sum(abs(model1_cart[[i]]$diff_2))/m
  abs_diff_cart[i,4]=sum(abs(model2_cart[[i]]$diff_2))/m
}

abs_diff_cart=round(abs_diff_cart,2)

write.csv(abs_diff_cart,paste("output/","CART ",today,"/","CART_Absolute Differences of Utility_",m,"syntheses.csv",sep=""))

cmb=rep(CART_CP,4)
Label=rep(c("one-way, State Only","one-way, State and Rating","two-way, State Only","two-way, State and Rating"),each=3)
Difference_cart=c(abs_diff_cart[,1],abs_diff_cart[,2],abs_diff_cart[,3],abs_diff_cart[,4])


abs_diff_cart2=data.frame(cmb,Label,Difference_cart)
names(abs_diff_cart2)=c("MinBucket","Label","Difference")

pdf(paste("output/","CART ",today,"/","CART_Absolute Differences_",m,"syntheses.pdf",sep=""))

abs2 <- abs_diff_cart2 %>% 
  group_by(Label) %>%
  ungroup() %>%
  mutate(cmb=factor(cmb))

ggplot(abs2, aes(Difference, factor(Label,levels=rev(c("one-way, State Only","one-way, State and Rating","two-way, State Only","two-way, State and Rating"))))) +
  geom_line(aes(group = Label)) + xlim(0,10) +
  scale_colour_manual(values=cbPalette) + 
  theme_bw(base_family = "") + 
  theme(legend.title=element_blank()) +
  geom_point(aes(color = cmb),size=2) + 
  labs(title = "Average Absolute Differences to Confidential Data",y='')


dev.off()
