library(tidyverse)
library(data.table)
library(report) 
library(patchwork)

DIRout="PATH"

# read final classified data set
final_DATA_gender_classif=fread("DATA_gender_classif_v2_final_fullInfo.txt.gz")
# add category of second author
for(PUB in unique(final_DATA_gender_classif$publication)){
  X=final_DATA_gender_classif[publication==PUB]
  if(X$NoAuthors[1]>3){
    X=X[order(row)]
    ORDER=X$order
    ORDER[2]="second"
    final_DATA_gender_classif$order[final_DATA_gender_classif$publication==PUB]=ORDER
  }
}
# re-classifying missing authors
final_DATA_gender_classif=final_DATA_gender_classif%>%group_by(publication)%>%mutate(MissAuthors=sum(is.na(gender)))%>%data.table()
final_DATA_gender_classif$MissProp=final_DATA_gender_classif$MissAuthors/final_DATA_gender_classif$NoAuthors

# explanation of dataset
length(unique(final_DATA_gender_classif$publication))
names(final_DATA_gender_classif)
# cols 1 - 10 original columns of dataset with metadata for 918 papers
# cols 11 - 16 columns with information about each individual author
# cols 17 - 22 columns with summary information about each paper
# [20] "MissAuthors": number of authors without gender classification                                                          
# [21] "MissProp" : prop of  MissAuthors  
# [22] "round" : some authors were classified in a second round (not relevant)
#hist(final_DATA_gender_classif$NoAuthors)
#final_DATA_gender_classif[publication=="pub10"]
# table(final_DATA_gender_classif$order)
# length(unique(final_DATA_gender_classif$publication))
# min(final_DATA_gender_classif$`Publication statuses and dates > Date > Year-2`)
# max(final_DATA_gender_classif$`Publication statuses and dates > Date > Year-2`)
# length(unique(final_DATA_gender_classif$author))
# table(str_length(unique(final_DATA_gender_classif$firstName)))


# proportion of authors not clasified per paper
hist(final_DATA_gender_classif$MissProp)
# 40 papers had 100% of their authors with missing classification
final_DATA_gender_classif[MissProp==1] 
length(unique(final_DATA_gender_classif[MissProp==1]$publication))
# 118 names could not be classified
length(unique(final_DATA_gender_classif[is.na(gender)]$firstName))
# 3350 of names were successfully classified
length(unique(final_DATA_gender_classif[!is.na(gender)]$firstName))
# of which 2252 were classified with a confidence of 80% or above
length(unique(final_DATA_gender_classif[proportion_male>=0.8 | proportion_female>=0.8]$firstName))


# plot summarising missing classification and confidence of classifications for male and females
p_male=ggplot(final_DATA_gender_classif[gender=="male"],aes(proportion_male)) + geom_histogram() + theme_minimal() + ggtitle("Male classification") + xlab("count") + xlab("confidence is male") 
p_female=ggplot(final_DATA_gender_classif[gender=="female"],aes(proportion_female)) + geom_histogram() + theme_minimal() + ggtitle("Female classification") + xlab("") + xlab("confidence is female") 
p_missing=ggplot(final_DATA_gender_classif,aes(MissProp)) + geom_histogram() + theme_minimal() + ggtitle("Missing classification") + xlab("") + xlab("proportion of missing authors") 
P=(p_male|p_female|p_missing) + plot_annotation(tag_levels = "A")
ggsave(paste0(DIRout,"0_plot_classification_confidence_new.png"),P,height = 4,width = 11.8)
# missing rate is low and confidence is high

# reclassify authors with low confidence as missing and estimate missing prop again
final_DATA_gender_classif$gender[final_DATA_gender_classif$gender=="male" & final_DATA_gender_classif$proportion_male<0.8]=NA
final_DATA_gender_classif$gender[final_DATA_gender_classif$gender=="female" & final_DATA_gender_classif$proportion_female<0.8]=NA
final_DATA_gender_classif=final_DATA_gender_classif%>%group_by(publication)%>%mutate(MissAuthors=sum(is.na(gender)))%>%data.table()
final_DATA_gender_classif$MissProp=final_DATA_gender_classif$MissAuthors/final_DATA_gender_classif$NoAuthors
p_male=ggplot(final_DATA_gender_classif[gender=="male"],aes(proportion_male)) + geom_histogram() + theme_minimal() + ggtitle("Male classification") + xlab("count") + xlab("confidence is male") 
p_female=ggplot(final_DATA_gender_classif[gender=="female"],aes(proportion_female)) + geom_histogram() + theme_minimal() + ggtitle("Female classification") + xlab("") + xlab("confidence is female") 
p_missing=ggplot(final_DATA_gender_classif,aes(MissProp)) + geom_histogram() + theme_minimal() + ggtitle("Missing classification") + xlab("") + xlab("proportion of missing authors") 
P=(p_male|p_female|p_missing) + plot_annotation(tag_levels = "A")
ggsave(paste0(DIRout,"0_plot_classification_confidence_reclass_new.png"),P,height = 4,width = 11.8)


#filter out publications with high missing props (>20%)
final_DATA_gender_classif=final_DATA_gender_classif[MissProp<=0.2]
length(unique(final_DATA_gender_classif$publication))
length(unique(final_DATA_gender_classif$firstName))

# final_DATA_gender_classif$category
# edit categories to make it more self-explanatory and remove categories that do not add explanatory power to the analysis
unique(final_DATA_gender_classif$category)
final_DATA_gender_classif[category%in%unique(final_DATA_gender_classif$category)[-c(1:4)]]$category=NA
final_DATA_gender_classif[category%in%unique(final_DATA_gender_classif$category)[1]]$category="Ext"
final_DATA_gender_classif[category%in%unique(final_DATA_gender_classif$category)[2]]$category="Int"
final_DATA_gender_classif[category%in%unique(final_DATA_gender_classif$category)[3]]$category="Int_corrsp"
final_DATA_gender_classif[category%in%unique(final_DATA_gender_classif$category)[4]]$category="Ext_corrsp"
unique(final_DATA_gender_classif$category)

# overall gender props
genderOrder=table(final_DATA_gender_classif$gender,final_DATA_gender_classif$order)
genderOrder[1,]/(genderOrder[2,]+genderOrder[1,])
genderOrder=table(final_DATA_gender_classif$gender,final_DATA_gender_classif$category)
genderOrder=genderOrder[1,]/(genderOrder[2,]+genderOrder[1,])
data.table(t(genderOrder))

# remove single categories
data.table(table(final_DATA_gender_classif$category))
final_DATA_gender_classif=final_DATA_gender_classif[category%in%unique(final_DATA_gender_classif$category)[c(1:4)]]

# new categories of external vs internal
final_DATA_gender_classif$categoryExtInt=final_DATA_gender_classif$category
final_DATA_gender_classif$categoryExtInt[final_DATA_gender_classif$categoryExtInt=="Ext_corrsp"]="Ext"
final_DATA_gender_classif$categoryExtInt[final_DATA_gender_classif$categoryExtInt=="Ext"]="External"
final_DATA_gender_classif$categoryExtInt[final_DATA_gender_classif$categoryExtInt=="Int"]="Internal"
final_DATA_gender_classif$categoryExtInt[final_DATA_gender_classif$categoryExtInt=="Int_corrsp"]="Internal"
X=data.table((table(final_DATA_gender_classif$categoryExtInt,final_DATA_gender_classif$gender)))
names(X)=c("category","gender","count")
p1=ggplot(X,aes(category,count,fill=gender)) + geom_bar(position="stack", stat="identity") + facet_wrap("category",scales = "free",nrow=1) + theme_minimal()
X=data.table((table(final_DATA_gender_classif$order,final_DATA_gender_classif$gender)))
names(X)=c("order","gender","count")
p2=ggplot(X,aes(order,count,fill=gender)) + geom_bar(position="stack", stat="identity") + facet_wrap("order",scales = "free",nrow=1) + theme_minimal()
P=(p1|p2) + plot_layout(widths = c(2,4)) + plot_annotation(tag_levels = "A") +  plot_layout(guides = 'collect') & theme(legend.position = "bottom")
ggsave(paste0(DIRout,"0_plot_categories_order_new.png"),P,height = 4,width = 11.8)

# raw counts
X=data.table((table(final_DATA_gender_classif[categoryExtInt=="Internal"]$order,final_DATA_gender_classif[categoryExtInt=="Internal"]$gender)))
names(X)=c("order","gender","count")
p1=ggplot(X,aes(order,count,fill=gender)) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap("order",scales = "free",nrow=1) + theme_minimal()
# count stats
X=data.table((table(final_DATA_gender_classif$categoryExtInt,final_DATA_gender_classif$gender)))
X2=X%>%group_by(V1)%>%summarise(male=N[V2=="male"],
                                female=N[V2=="female"],
                                ratio=N[V2=="male"]/N[V2=="female"],
                                prop=N[V2=="female"]/(N[V2=="male"]+N[V2=="female"]))
p2=ggplot(X2,aes(V1,ratio)) + geom_point() + 
  geom_hline(yintercept = 1,linetype="dashed",color="grey50",linewidth=1.4)+ 
  theme_minimal() + ylab("ratio male/female") + xlab("author category")
p3=ggplot(X2,aes(V1,prop)) + geom_point() + 
  geom_hline(yintercept = 0.5,linetype="dashed",color="grey50",linewidth=1.4)+ 
  theme_minimal() + ylab("proportion of females") + xlab("author category")
P=(p1|p2|p3)+ plot_layout(widths = c(2,1,1)) + plot_annotation(tag_levels = "A") +  plot_layout(guides = 'collect') & theme(legend.position = "bottom") 
ggsave(paste0(DIRout,"0_plot_categories_stats_new.png"),P,height = 4,width = 11.8)


##### FULL SUMMARY, counts and ratios
x_summ=final_DATA_gender_classif%>%group_by(publication)%>%summarise(publication=unique(publication),
                                                                     NoAuthors=unique(NoAuthors),
                                                                     MissAuthors=unique(MissAuthors),
                                                                     MissProp=unique(MissProp),
                                                                     women=sum(gender=="female",na.rm = T),
                                                                     men=sum(gender=="male",na.rm = T),
                                                                     ratio=sum(gender=="male",na.rm = T)/sum(gender=="female",na.rm = T),
                                                                     prop=women/(NoAuthors-MissAuthors),
                                                                     first=ifelse(length(gender[order=="first"])>0, gender[order=="first"], NA),
                                                                     second=ifelse(length(gender[order=="second"])>0, gender[order=="second"], NA),
                                                                     last=ifelse(length(gender[order=="last"])>0, gender[order=="last"], NA),
                                                                     count_Ext=sum(category=="Ext"),
                                                                     count_Int=sum(category=="Int"),
                                                                     count_Int_corrsp=sum(category=="Int_corrsp"),
                                                                     count_Ext_corrsp=sum(category=="Ext_corrsp"))%>%data.table()

# count women per paper
x_summ$womenCat=NA
x_summ$womenCat[x_summ$women==0]="0"
x_summ$womenCat[x_summ$women==1]="1"
x_summ$womenCat[x_summ$women==2]="2"
x_summ$womenCat[x_summ$women==3]="3"
x_summ$womenCat[x_summ$women==4]="4"
x_summ$womenCat[x_summ$women==5]="5"
x_summ$womenCat[x_summ$women>5]="5+"
womenCat=data.table((table(x_summ$womenCat)/sum(table(x_summ$womenCat)))*100)
womenCat$N=round(womenCat$N,1)
womenCat$label=paste0(womenCat$V1,"=",womenCat$N,"%")
# Compute the position of labels
data <- womenCat %>% 
  arrange(desc(V1)) %>%
  mutate(prop = N / sum(womenCat$N) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
# Basic piechart
P=ggplot(data, aes(x="", y=prop, fill=V1)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = label), color = "black", size=4,angle = -25) +
  scale_fill_brewer(palette="Set1")
summ_plot_pie=P
ggsave(paste0(DIRout,"0_plot_women_count_pieChart_new.png"),P,height = 4.8,width = 4.8)

# count men per paper
x_summ$menCat=NA
x_summ$menCat[x_summ$men==0]="0"
x_summ$menCat[x_summ$men==1]="1"
x_summ$menCat[x_summ$men==2]="2"
x_summ$menCat[x_summ$men==3]="3"
x_summ$menCat[x_summ$men==4]="4"
x_summ$menCat[x_summ$men==5]="5"
x_summ$menCat[x_summ$men>5]="5+"
menCat=data.table((table(x_summ$menCat)/sum(table(x_summ$menCat)))*100)
menCat$N=round(menCat$N,1)
menCat$label=paste0(menCat$V1,"=",menCat$N,"%")
# Compute the position of labels
data <- menCat %>% 
  arrange(desc(V1)) %>%
  mutate(prop = N / sum(menCat$N) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
# Basic piechart
P=ggplot(data, aes(x="", y=prop, fill=V1)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = label), color = "black", size=4,angle = -25) +
  scale_fill_brewer(palette="Set1")
summ_plot_pie=P
ggsave(paste0(DIRout,"0_plot_men_count_pieChart_new.png"),P,height = 4.8,width = 4.8)

# proportion of women
P=ggplot(x_summ, aes(prop)) +
  stat_bin(aes(y=..density..), breaks = seq(min(x_summ$prop), max(x_summ$prop), by = .1), color="white",fill="grey80") +
  geom_line(stat="density", size = 1,col="grey20") + geom_vline(xintercept = 0.5,linetype="dashed",size=2,col="blue") +
  theme_minimal() + xlab("proportion of women")
ggsave(paste0(DIRout,"0_plot_women_prop_perPublication_new.png"),P,height = 4.8,width = 4.8)

# no obviously driven by number of authors
p1=ggplot(x_summ,aes((NoAuthors-MissAuthors),prop)) + geom_point() +theme_minimal()+ ylab("Female proportion") + xlab("Number of authors")
p2=ggplot(x_summ[NoAuthors<=50],aes((NoAuthors-MissAuthors),prop)) + geom_point() +theme_minimal() + ylab("Female proportion") + xlab("Number of authors")
P=(p1|p2)
ggsave(paste0(DIRout,"0_plot_women_prop_perPublication_NoAuthors_new.png"),P,height = 4.8,width = 4.8*2)
# relationship is not significant
summary(lm(x_summ$prop~x_summ$NoAuthors))

# since there are very few papers with MANY authors, and those are heavily biased towards males, we focus on papers with 20 or less authors
# since there are very few papers with only one author, we focus on papers with 2 or more authors
hist(x_summ$NoAuthors)
sum(x_summ$NoAuthors>20)/nrow(x_summ) # removing only 15% of papers
1-sum(x_summ$NoAuthors>1&x_summ$NoAuthors<20)/nrow(x_summ) # further removing single author paper removes only 17.2%
# remove papers with single authors and more than 20 authors (17%)
x_summ=x_summ[NoAuthors>1 & NoAuthors<=20]
# Note that we tested the stats below without removing these papers and results are the same

# random distribution
Noauthors_x=unique(x_summ$NoAuthors)
randDraw=data.table()
for(NoAut in Noauthors_x){
  for(sim in 1:100){
    DRAW=final_DATA_gender_classif[sample(1:nrow(final_DATA_gender_classif),NoAut)]
    randDraw=rbind(randDraw,data.table(Noauthors=NoAut,male=sum(DRAW$gender=="male"),female=sum(DRAW$gender=="female")))
  }
}
randDraw$type="random"
randDraw$ratio=randDraw$female/randDraw$male
randDraw$prop=randDraw$female/randDraw$Noauthors
randDraw=randDraw[!is.na(prop)]

mean(randDraw$prop)
sd(randDraw$prop)

globe_ratio=0.4
P1=ggplot(randDraw, aes(prop)) +
  stat_bin(aes(y=..density..), breaks = seq(min(randDraw$prop), max(randDraw$prop), by = .1), color="white",fill="grey80") +
  geom_line(stat="density", size = 1,col="grey20") + 
  geom_vline(xintercept = 0.5,linetype="dashed",size=1,col="blue") +
  geom_vline(xintercept = globe_ratio,linetype="dashed",size=1,col="red") +
  theme_minimal() + xlab("Female proportion") + ggtitle("Random distribution")

# observed distribution
P2=ggplot(x_summ, aes(prop)) +
  stat_bin(aes(y=..density..), breaks = seq(min(x_summ$prop), max(x_summ$prop), by = .1), color="white",fill="grey80") +
  geom_line(stat="density", size = 1,col="grey20") + 
  geom_vline(xintercept = 0.5,linetype="dashed",size=1,col="blue") +
  geom_vline(xintercept = globe_ratio,linetype="dashed",size=1,col="red") +
  theme_minimal() + xlab("Female proportion")  + ggtitle("Observed distribution")
P=(P1|P2) + plot_annotation(tag_levels = "A")
ggsave(paste0(DIRout,"0_plot_rand_observed_distributions_new.png"),P,height = 4,width = 11.8)

mean(x_summ$prop)
sd(x_summ$prop)

t.test(x = x_summ$prop,mu = globe_ratio) %>% report()

# effect of author order on gene ratio
# effect of number of authors per paper
# no effect, so this is not of major concern
p1=ggplot(x_summ[!is.na(first)],aes(first,NoAuthors)) + geom_jitter(alpha=0.5,col="grey80") + geom_boxplot(fill="transparent",col="red") + theme_minimal() + scale_y_log10() + ylab("Number of authors") + xlab("Gender of first author")
p2=ggplot(x_summ[!is.na(last)],aes(last,NoAuthors)) + geom_jitter(alpha=0.5,col="grey80") + geom_boxplot(fill="transparent",col="red") + theme_minimal() + scale_y_log10() + ylab("") + xlab("Gender of last author")
P=(p1|p2) + plot_annotation(tag_levels = "A")
ggsave(paste0(DIRout,"0_plot_women_prop_perPublication_authorLeadNoAuthors_new.png"),P,height = 4.8,width = 4.8*2)

# effect of author order
p1=ggplot(x_summ[!is.na(first)],aes(first,prop)) + geom_jitter(alpha=0.5,col="grey80") + geom_boxplot(fill="transparent",col="red") + theme_minimal() + ylab("Female proportion") + xlab("Gender of first author")
p2=ggplot(x_summ[!is.na(last)],aes(last,prop)) + geom_jitter(alpha=0.5,col="grey80") + geom_boxplot(fill="transparent",col="red") + theme_minimal()+ ylab("") + xlab("Gender of last author")
P=(p1|p2) + plot_annotation(tag_levels = "A")
summ_plot_1stLast=(p1|p2)
ggsave(paste0(DIRout,"0_plot_women_prop_perPublication_authorLead_new.png"),P,height = 4.8,width = 4.8*2)
y <- cbind(as.integer(as.logical(x_summ$first=="male")), as.integer(as.logical(x_summ$first=="female")))
m1 <- glm(y ~ x_summ$prop, family = binomial)
summary(m1) # highly significant
m1 %>% report()
