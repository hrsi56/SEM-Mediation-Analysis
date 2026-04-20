###   להריץ את זה
setwd("C:/Users/hagitc/OneDrive - Ben Gurion University of the Negev/hagitcloud/RESULT/October 7 -2023 - trauma 1/R")
 data = read.csv("NoVA.csv");
 data[is.na(data)] <- 0
 data$SEX.cod[(data$SEX.cod==2)] <- 0


########################REG#################
model_PDS_ALC = lm(data$PDS ~ data$Alcohol.vs..all )  #  
summary(model_PDS_ALC)  #  p 0.00577   B 11.236   R-squared:  0.05672

model_VAS_ALC = lm(data$VAS ~ data$Alcohol.vs..all)  # 
summary(model_VAS_ALC)  #  P NOT

model_INT_ALC = lm(data$Intrusive ~ data$Alcohol.vs..all)  # 
summary(model_INT_ALC)  #   p 0.000257    B 4.2822     R-squared:  0.09729

model_AVO_ALC = lm(data$Avoidence ~ data$Alcohol.vs..all)  # 
summary(model_AVO_ALC)  #  p  0.0103  B 1.2676    R-squared:  0.04912

model_MOOD_ALC = lm(data$Negative.mood ~ data$Alcohol.vs..all)  # 
summary(model_MOOD_ALC)  #  p-value: 0.002322    B 4.6502    R-squared:  0.06861

model_HYP_ALC = lm(data$Hyperarousal ~ data$Alcohol.vs..all)  # 
summary(model_HYP_ALC)  #  p-value: 0.002623     B 3.9848     R-squared:  0.06702

model_DIS_ALC = lm(data$Dissociation ~ data$Alcohol.vs..all)  # 
summary(model_DIS_ALC)  #   p-value: 0.01451      B 5.658    R-squared:  0.04475

model_GAD_ALC = lm(data$GAD ~ data$Alcohol.vs..all)  # 
summary(model_GAD_ALC)  #  p-value: 0.0008621     B 4.5191   Multiple R-squared:  0.08156

model_PHQ_ALC = lm(data$PHQ~ data$Alcohol.vs..all)  # 
summary(model_PHQ_ALC)  #  p-value: 0.0002713   B 5.8608    R-squared:  0.09661  

#####################################################

model_PDS_SEX = lm(data$PDS ~ data$SEX.cod)  # 
summary(model_PDS_SEX)  #   p-value: 0.06294    B  6.640        R-squared:  0.02615   

model_VAS_SEX = lm(data$VAS ~ data$SEX.cod)  # 
summary(model_VAS_SEX)  #  NOT 

model_INT_SEX = lm(data$Intrusive ~ data$SEX.cod)  # 
summary(model_INT_SEX)  #   p-value: 0.01184   B 2.6007      R-squared:  0.04738 

model_AVO_SEX = lm(data$Avoidence ~ data$SEX.cod)  # 
summary(model_AVO_SEX)  #   p-value: 0.003226    B 1.2623    R-squared:  0.06431

model_MOOD_SEX = lm(data$Negative.mood ~ data$SEX.cod)  # 
summary(model_MOOD_SEX)  #  p-value: 0.05497    B 2.5772        R-squared:  0.02783

model_HYP_SEX = lm(data$Hyperarousal ~ data$SEX.cod)  # 
summary(model_HYP_SEX)  #   p-value: 0.004974    B 3.2444      R-squared:  0.05866 

model_DIS_SEX = lm(data$Dissociation ~ data$SEX.cod)  # 
summary(model_DIS_SEX)  #   p-value: 1.214e-05    B 0.15440   R-squared:  0.1364 

model_GAD_SEX = lm(data$GAD ~ data$SEX.cod)  # 
summary(model_GAD_SEX)  #     p-value: 0.0231     B 2.7118   R-squared:  0.03878 

model_PHQ_SEX = lm(data$PHQ~ data$SEX.cod)  # 
summary(model_PHQ_SEX)  #    p-value: 0.005376   B 3.9400  R-squared:  0.05765

###############################################


model_PDS_Age = lm(data$PDS ~ data$Age)  # 
summary(model_PDS_Age)  #  p-value: 0.0008259  B 0.5169     R-squared:  0.08212

model_VAS_Age = lm(data$VAS ~ data$Age)  #  NOT
summary(model_VAS_Age)  #   p-value: 0.02254   B 0.06175    R-squared:  0.03909  NOT

model_INT_Age = lm(data$Intrusive ~ data$Age)  # 
summary(model_INT_Age)  #    p-value: 0.0004096  B 0.15856    R-squared:  0.09125


model_AVO_Age = lm(data$Avoidence ~ data$Age)  # 
summary(model_AVO_Age)  #  p-value: 0.009642  R-squared:  0.05004 0.04892    

model_MOOD_Age = lm(data$Negative.mood ~ data$Age)  # 
summary(model_MOOD_Age)  #  p-value: 0.02354   B 0.13324    R-squared:  0.03853

model_HYP_Age = lm(data$Hyperarousal ~ data$Age)  # 
summary(model_HYP_Age)  #   p-value: 0.006459  B 0.13833   R-squared:  0.05525

model_DIS_Age = lm(data$Dissociation ~ data$Age)  # 
summary(model_DIS_Age)  #  p-value: 0.007427   B 0.23637  R-squared:  0.05344 

model_GAD_Age = lm(data$GAD ~ data$Age)  # 
summary(model_GAD_Age)  #  p-value: 0.0005596   B 0.1787    R-squared:  0.08719

model_PHQ_Age = lm(data$PHQ~ data$Age)  # 
summary(model_PHQ_Age)  # p-value: 0.007086   B 0.16760    R-squared:  0.05405


####################################################

model_SEX_ALC = lm(data$Alcohol.vs..all ~ data$SEX.cod)  # 
summary(model_SEX_ALC)  #  NOT 

####################################################

model_PDS_DIS = lm(data$PDS ~ data$Dissociation)  # 
summary(model_PDS_DIS)  #  p-value: < 2.2e-16   B  1.1379    R-squared:  0.416 

model_VAS_DIS = lm(data$VAS ~ data$Dissociation)  # 
summary(model_VAS_DIS)  #  p-value: 1.631e-10    B 0.15838   R-squared:  0.2688

model_INT_DIS = lm(data$Intrusive ~ data$Dissociation)  # 
summary(model_INT_DIS)  #  p-value: 3.008e-12   B 0.28642         R-squared:  0.3113

model_AVO_DIS = lm(data$Avoidence ~ data$Dissociation)  # 
summary(model_AVO_DIS)  #  p-value: 1.378e-09    B 0.10587     R-squared:  0.2451

model_MOOD_DIS = lm(data$Negative.mood ~ data$Dissociation)  # 
summary(model_MOOD_DIS)  #   p-value: 7.116e-15   B 0.40445      R-squared:  0.3713

model_HYP_DIS = lm(data$Hyperarousal ~ data$Dissociation)  # 
summary(model_HYP_DIS)  #  p-value: 7.826e-14    B 0.3396       R-squared:  0.3481

model_GAD_DIS = lm(data$GAD ~ data$Dissociation)  # 
summary(model_GAD_DIS)  #   p-value: 4.793e-15   B 0.36232    R-squared:  0.375

model_PHQ_DIS = lm(data$PHQ~ data$Dissociation)  # 
summary(model_PHQ_DIS)  #   p-value: 6.489e-12      B 0.38831     R-squared:  0.3033



########################################################

model_PDS_DIS_ALC_SEX = lm(data$PDS~ data$Alcohol.vs..all : data$SEX.cod)  # 
summary(model_PDS_DIS_ALC_SEX)  #    p-value: 0.005068 

model_PDS_DIS_ALC_SEX = lm(data$PDS~ data$Dissociation + data$Alcohol.vs..all : data$SEX.cod)  # 
summary(model_PDS_DIS_ALC_SEX)  #  NO INT




library(tidySEM)
library(semPlot)
library(lavaan)


# Define the extended SEM model with regressions, interactions, and covariances
model <- '

  PDS ~ Intrusive
  PDS ~ Avoidence
  PDS ~ Negative.mood
  PDS ~ Hyperarousal
  
  
  Intrusive ~~ Avoidence
  Intrusive ~~ Negative.mood
  Intrusive ~~ Hyperarousal
  Intrusive ~~ VAS
  Intrusive ~~ GAD
  Intrusive ~~ PHQ

  Avoidence ~~ Negative.mood
  Avoidence ~~ Hyperarousal
  Avoidence ~~ VAS
  Avoidence ~~ GAD
  Avoidence ~~ PHQ
  
  Negative.mood ~~ Hyperarousal
  Negative.mood ~~ VAS
  Negative.mood ~~ GAD
  Negative.mood ~~ PHQ

  Hyperarousal ~~ VAS
  Hyperarousal ~~ GAD
  Hyperarousal ~~ PHQ
  
  VAS ~~ GAD
  VAS ~~ PHQ
  
  GAD ~~ PHQ

  
   VAS ~ Alcohol.vs..all
   Intrusive ~ Alcohol.vs..all
   Avoidence ~ Alcohol.vs..all
   Negative.mood ~ Alcohol.vs..all
   Hyperarousal ~ Alcohol.vs..all
  Dissociation ~ Alcohol.vs..all
   GAD ~ Alcohol.vs..all
   PHQ ~ Alcohol.vs..all
                      Alcohol.vs..all ~ SEX.cod
 Alcohol.vs..all ~  Age 
   
      VAS ~ dosa.vs..all
   Intrusive ~ dosa.vs..all
   Avoidence ~ dosa.vs..all
   Negative.mood ~ dosa.vs..all
   Hyperarousal ~ dosa.vs..all
  Dissociation ~ dosa.vs..all
   GAD ~ dosa.vs..all
   PHQ ~ dosa.vs..all
                 dosa.vs..all ~ SEX.cod
dosa.vs..all~  Age 

      VAS ~ Canabis.vd.all
   Intrusive ~ Canabis.vd.all
   Avoidence ~ Canabis.vd.all
   Negative.mood ~ Canabis.vd.all
   Hyperarousal ~ Canabis.vd.all
  Dissociation ~ Canabis.vd.all
   GAD ~ Canabis.vd.all
   PHQ ~ Canabis.vd.all
              Canabis.vd.all ~ SEX.cod
Canabis.vd.all~  Age 
  
        VAS ~ DOCTOR.vs..all
   Intrusive ~ DOCTOR.vs..all
   Avoidence ~ DOCTOR.vs..all
   Negative.mood ~ DOCTOR.vs..all
   Hyperarousal ~ DOCTOR.vs..all
  Dissociation ~ DOCTOR.vs..all
   GAD ~ DOCTOR.vs..all
   PHQ ~ DOCTOR.vs..all
           DOCTOR.vs..all~ SEX.cod
DOCTOR.vs..all~  Age 
   
         VAS ~ MDMA.vs.all
   Intrusive ~ MDMA.vs.all
   Avoidence ~ MDMA.vs.all
   Negative.mood ~ MDMA.vs.all
   Hyperarousal ~ MDMA.vs.all
  Dissociation ~ MDMA.vs.all
   GAD ~ MDMA.vs.all
   PHQ ~ MDMA.vs.all
        MDMA.vs.all~ SEX.cod
MDMA.vs.all~  Age 
  
           VAS ~ LSD.vs.all
   Intrusive ~ LSD.vs.all
   Avoidence ~ LSD.vs.all
   Negative.mood ~ LSD.vs.all
   Hyperarousal ~ LSD.vs.all
  Dissociation ~ LSD.vs.all
   GAD ~ LSD.vs.all
   PHQ ~ LSD.vs.all
     LSD.vs.all~ SEX.cod
LSD.vs.all~  Age 
  
             VAS ~ Cocaine.vs.all
   Intrusive ~ Cocaine.vs.all
   Avoidence ~ Cocaine.vs.all
   Negative.mood ~ Cocaine.vs.all
   Hyperarousal ~ Cocaine.vs.all
  Dissociation ~ Cocaine.vs.all
   GAD ~ Cocaine.vs.all
   PHQ ~ Cocaine.vs.all
  
  Cocaine.vs.all~ SEX.cod
Cocaine.vs.all~  Age 
  
   VAS ~ SEX.cod
   Intrusive  ~ SEX.cod
   Avoidence ~ SEX.cod
   Negative.mood ~ SEX.cod
   Hyperarousal ~ SEX.cod
   Dissociation ~ SEX.cod
   GAD ~ SEX.cod
   PHQ ~ SEX.cod
  
  
   VAS ~ Age
   Intrusive ~ Age
   Avoidence ~ Age
   Negative.mood ~ Age
   Hyperarousal ~ Age
   Dissociation ~ Age
   GAD ~ Age
   PHQ ~ Age
  
   VAS ~ Dissociation
   Intrusive ~ Dissociation
   Avoidence ~ Dissociation
   Negative.mood ~ Dissociation
   Hyperarousal ~ Dissociation
   GAD ~ Dissociation
   PHQ ~ Dissociation
'

# Fit the extended model
fit <- sem(model, data = data )

# Capture the summary text
summary_text <- capture.output(summary(fit, standardize = T))

# Save the summary text to a text file
writeLines(summary_text, paste("summary.txt"))




###############ALC#########################

# Get layout information
layout <- get_layout( 
NA,NA,NA,"SEX.cod",NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,"Negative.mood",NA,NA,NA,
NA,NA,NA,NA,NA,NA,"Hyperarousal",NA,NA,NA,
"Alcohol.vs..all",NA,NA,"Dissociation",NA,NA,NA,NA,NA,"PDS",
NA,NA,NA,NA,NA,NA,"Intrusive",NA,NA,NA,
NA,NA,NA,NA,NA,NA,"Avoidence",NA,NA,NA,
NA,NA,NA,"Age",NA,NA,NA,NA,NA,NA,
  rows = 7
)



# Prepare the graph with the layout
graph <- prepare_graph(fit, layout = layout  )


graph$edges$alpha[1:length(graph$edges$pval)] = NA
graph$edges$linewidth[1:length(graph$edges$pval)] = NA
graph$edges$alpha[!is.na(graph$edges$pval)] = 1.2 -  as.numeric(graph$edges$pval[!is.na(graph$edges$pval)] )^0.5
graph$edges$linewidth[!is.na(graph$edges$est_std)] = 3 * abs(as.numeric(graph$edges$est_std )) +0.5




graph$edges$colour[1:length(graph$edges$to)]="1"
graph$edges$colour[graph$edges$to== "Avoidence"]="2"
graph$edges$colour[graph$edges$to== "Intrusive"]="3"
graph$edges$colour[graph$edges$to== "Hyperarousal"]="4"
graph$edges$colour[graph$edges$to== "Negative.mood"]="orange" 
graph$edges$label_colour = graph$edges$colour



graph$edges$label_size[1:length(graph$edges$name)] = 9
graph$edges$label_family[1:length(graph$edges$name)] = "serif"
graph$edges$label_alpha[1:length(graph$edges$name)] = 1

graph$nodes$colour[1:length(graph$nodes$name)]="1"
graph$nodes$colour[graph$nodes$name== "Avoidence"]="2"
graph$nodes$colour[graph$nodes$name== "Intrusive"]="3"
graph$nodes$colour[graph$nodes$name== "Hyperarousal"]="4"
graph$nodes$colour[graph$nodes$name== "Negative.mood"]="orange" 
graph$nodes$alpha[1:length(graph$nodes$name)]=1
graph$nodes$size[1:length(graph$nodes$name)]=1.5

graph$nodes$label_family[1:length(graph$nodes$name)] = "serif"
graph$nodes$label_size[1:length(graph$nodes$name)] = 10.5



label_text= paste(graph$edges$est_sig_std)
graph$edges$show[graph$edges$op == "~~"]  =FALSE


graph$edges$label_location=0.3

graph$edges$connect_from[graph$edges$from=="Alcohol.vs..all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Alcohol.vs..all" & graph$edges$to== "Intrusive"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Alcohol.vs..all" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_from[graph$edges$from=="Alcohol.vs..all" & graph$edges$to== "Hyperarousal"] = "top"
graph$edges$connect_from[graph$edges$from=="Alcohol.vs..all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "Hyperarousal"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "Negative.mood"] = "top"

graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Intrusive"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Avoidence"] = "left"
graph$edges$connect_to[graph$edges$from=="Negative.mood" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Hyperarousal" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Avoidence" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Intrusive" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Negative.mood"] = "left"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Avoidence"] = "bottom"


graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Alcohol.vs..all"] = "bottom"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Alcohol.vs..all"] = "top"



graph$edges$label= label_text

graph$nodes$label[graph$nodes$name=="SEX.cod"] = "Female gender"
graph$nodes$label[graph$nodes$name=="Alcohol.vs..all"] = "Alcohol"
graph$nodes$label[graph$nodes$name=="Negative.mood"] = "Negative Mood"
graph$nodes$node_xmax= graph$nodes$x+1.8
graph$nodes$node_xmin= graph$nodes$x-1.8
graph$nodes$node_ymax= graph$nodes$y+0.6
graph$nodes$node_ymin= graph$nodes$y-0.6
graph$nodes$node_ymax[graph$nodes$name=="Alcohol.vs..all"]= graph$nodes$y[graph$nodes$name=="Alcohol.vs..all"]+2
graph$nodes$node_ymin[graph$nodes$name=="Alcohol.vs..all"]= graph$nodes$y[graph$nodes$name=="Alcohol.vs..all"]-2


jpeg('ALCOHOL-PDS.jpg', quality=100, height=945, width=1512)
plot(graph)
dev.off()








#ALC
#######################################

# Get layout information
layout <- get_layout( 
  NA,NA,NA,"SEX.cod",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"VAS",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"GAD",NA,NA,NA,
  "Alcohol.vs..all",NA,NA,"Dissociation",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"PHQ",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
  NA,NA,NA,"Age",NA,NA,NA,NA,NA,NA,
  rows = 7
)



# Prepare the graph with the layout
graph <- prepare_graph(fit, layout = layout  )


graph$edges$alpha[1:length(graph$edges$pval)] = NA
graph$edges$linewidth[1:length(graph$edges$pval)] = NA
graph$edges$alpha[!is.na(graph$edges$pval)] = 1.2 -  as.numeric(graph$edges$pval[!is.na(graph$edges$pval)] )^0.5
graph$edges$linewidth[!is.na(graph$edges$est_std)] = 3 * abs(as.numeric(graph$edges$est_std )) +0.5



graph$edges$colour[1:length(graph$edges$to)]="1"
graph$edges$colour[graph$edges$to== "Avoidence"]="2"
graph$edges$colour[graph$edges$to== "PHQ"]="3"
graph$edges$colour[graph$edges$to== "GAD"]="4"
graph$edges$colour[graph$edges$to== "VAS"]="orange" 
graph$edges$label_colour = graph$edges$colour



graph$edges$label_size[1:length(graph$edges$name)] = 9
graph$edges$label_family[1:length(graph$edges$name)] = "serif"
graph$edges$label_alpha[1:length(graph$edges$name)] = 1

graph$nodes$colour[1:length(graph$nodes$name)]="1"
graph$nodes$colour[graph$nodes$name== "Avoidence"]="2"
graph$nodes$colour[graph$nodes$name== "PHQ"]="3"
graph$nodes$colour[graph$nodes$name== "GAD"]="4"
graph$nodes$colour[graph$nodes$name== "VAS"]="orange" 
graph$nodes$alpha[1:length(graph$nodes$name)]=1
graph$nodes$size[1:length(graph$nodes$name)]=1.5

graph$nodes$label_family[1:length(graph$nodes$name)] = "serif"
graph$nodes$label_size[1:length(graph$nodes$name)] = 10.5



label_text= paste(graph$edges$est_sig_std)
graph$edges$show[graph$edges$op == "~~"]  =FALSE


graph$edges$label_location=0.3

graph$edges$connect_from[graph$edges$from=="Alcohol.vs..all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Alcohol.vs..all" & graph$edges$to== "PHQ"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Alcohol.vs..all" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_from[graph$edges$from=="Alcohol.vs..all" & graph$edges$to== "GAD"] = "top"
graph$edges$connect_from[graph$edges$from=="Alcohol.vs..all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "GAD"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "VAS"] = "top"

graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "PHQ"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Avoidence"] = "left"
graph$edges$connect_to[graph$edges$from=="VAS" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="GAD" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Avoidence" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="PHQ" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "VAS"] = "left"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Avoidence"] = "bottom"

graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Alcohol.vs..all"] = "bottom"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Alcohol.vs..all"] = "top"


graph$edges$label= label_text

graph$nodes$label[graph$nodes$name=="SEX.cod"] = "Female gender"
graph$nodes$label[graph$nodes$name=="Alcohol.vs..all"] = "Alcohol"
graph$nodes$node_xmax= graph$nodes$x+1.8
graph$nodes$node_xmin= graph$nodes$x-1.8
graph$nodes$node_ymax= graph$nodes$y+0.6
graph$nodes$node_ymin= graph$nodes$y-0.6
graph$nodes$node_ymax[graph$nodes$name=="Alcohol.vs..all"]= graph$nodes$y[graph$nodes$name=="Alcohol.vs..all"]+2
graph$nodes$node_ymin[graph$nodes$name=="Alcohol.vs..all"]= graph$nodes$y[graph$nodes$name=="Alcohol.vs..all"]-2


jpeg('ALCOHOL-SIMP.jpg', quality=100, height=945, width=1512)
plot(graph)
dev.off()












#################Dosa#######################

# Get layout information
layout <- get_layout( 
  NA,NA,NA,"SEX.cod",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"Negative.mood",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"Hyperarousal",NA,NA,NA,
  "dosa.vs..all",NA,NA,"Dissociation",NA,NA,NA,NA,NA,"PDS",
  NA,NA,NA,NA,NA,NA,"Intrusive",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"Avoidence",NA,NA,NA,
  NA,NA,NA,"Age",NA,NA,NA,NA,NA,NA,
  rows = 7
)



# Prepare the graph with the layout
graph <- prepare_graph(fit, layout = layout  )


graph$edges$alpha[1:length(graph$edges$pval)] = NA
graph$edges$linewidth[1:length(graph$edges$pval)] = NA
graph$edges$alpha[!is.na(graph$edges$pval)] = 1.2 -  as.numeric(graph$edges$pval[!is.na(graph$edges$pval)] )^0.5
graph$edges$linewidth[!is.na(graph$edges$est_std)] = 3 * abs(as.numeric(graph$edges$est_std )) +0.5




graph$edges$colour[1:length(graph$edges$to)]="1"
graph$edges$colour[graph$edges$to== "Avoidence"]="2"
graph$edges$colour[graph$edges$to== "Intrusive"]="3"
graph$edges$colour[graph$edges$to== "Hyperarousal"]="4"
graph$edges$colour[graph$edges$to== "Negative.mood"]="orange" 
graph$edges$label_colour = graph$edges$colour



graph$edges$label_size[1:length(graph$edges$name)] = 9
graph$edges$label_family[1:length(graph$edges$name)] = "serif"
graph$edges$label_alpha[1:length(graph$edges$name)] = 1

graph$nodes$colour[1:length(graph$nodes$name)]="1"
graph$nodes$colour[graph$nodes$name== "Avoidence"]="2"
graph$nodes$colour[graph$nodes$name== "Intrusive"]="3"
graph$nodes$colour[graph$nodes$name== "Hyperarousal"]="4"
graph$nodes$colour[graph$nodes$name== "Negative.mood"]="orange" 
graph$nodes$alpha[1:length(graph$nodes$name)]=1
graph$nodes$size[1:length(graph$nodes$name)]=1.5

graph$nodes$label_family[1:length(graph$nodes$name)] = "serif"
graph$nodes$label_size[1:length(graph$nodes$name)] = 10.5



label_text= paste(graph$edges$est_sig_std)
graph$edges$show[graph$edges$op == "~~"]  =FALSE


graph$edges$label_location=0.3

graph$edges$connect_from[graph$edges$from=="dosa.vs..all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="dosa.vs..all" & graph$edges$to== "Intrusive"] = "bottom"
graph$edges$connect_from[graph$edges$from=="dosa.vs..all" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_from[graph$edges$from=="dosa.vs..all" & graph$edges$to== "Hyperarousal"] = "top"
graph$edges$connect_from[graph$edges$from=="dosa.vs..all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "Hyperarousal"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "Negative.mood"] = "top"

graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Intrusive"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Avoidence"] = "left"
graph$edges$connect_to[graph$edges$from=="Negative.mood" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Hyperarousal" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Avoidence" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Intrusive" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Negative.mood"] = "left"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Avoidence"] = "bottom"



graph$edges$label= label_text

graph$nodes$label[graph$nodes$name=="SEX.cod"] = "Female gender"
graph$nodes$label[graph$nodes$name=="dosa.vs..all"] = "Dosa"
graph$nodes$label[graph$nodes$name=="Negative.mood"] = "Negative Mood"
graph$nodes$node_xmax= graph$nodes$x+1.8
graph$nodes$node_xmin= graph$nodes$x-1.8
graph$nodes$node_ymax= graph$nodes$y+0.6
graph$nodes$node_ymin= graph$nodes$y-0.6
graph$nodes$node_ymax[graph$nodes$name=="dosa.vs..all"]= graph$nodes$y[graph$nodes$name=="dosa.vs..all"]+2
graph$nodes$node_ymin[graph$nodes$name=="dosa.vs..all"]= graph$nodes$y[graph$nodes$name=="dosa.vs..all"]-2
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "dosa.vs..all"] = "bottom"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "dosa.vs..all"] = "top"


jpeg('Dosa-PDS.jpg', quality=100, height=945, width=1512)
plot(graph)
dev.off()







####

#Dosa-SYMP

#######################################

# Get layout information
layout <- get_layout( 
  NA,NA,NA,"SEX.cod",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"VAS",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"GAD",NA,NA,NA,
  "dosa.vs..all",NA,NA,"Dissociation",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"PHQ",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
  NA,NA,NA,"Age",NA,NA,NA,NA,NA,NA,
  rows = 7
)



# Prepare the graph with the layout
graph <- prepare_graph(fit, layout = layout  )


graph$edges$alpha[1:length(graph$edges$pval)] = NA
graph$edges$linewidth[1:length(graph$edges$pval)] = NA
graph$edges$alpha[!is.na(graph$edges$pval)] = 1.2 -  as.numeric(graph$edges$pval[!is.na(graph$edges$pval)] )^0.5
graph$edges$linewidth[!is.na(graph$edges$est_std)] = 3 * abs(as.numeric(graph$edges$est_std )) +0.5



graph$edges$colour[1:length(graph$edges$to)]="1"
graph$edges$colour[graph$edges$to== "Avoidence"]="2"
graph$edges$colour[graph$edges$to== "PHQ"]="3"
graph$edges$colour[graph$edges$to== "GAD"]="4"
graph$edges$colour[graph$edges$to== "VAS"]="orange" 
graph$edges$label_colour = graph$edges$colour



graph$edges$label_size[1:length(graph$edges$name)] = 9
graph$edges$label_family[1:length(graph$edges$name)] = "serif"
graph$edges$label_alpha[1:length(graph$edges$name)] = 1

graph$nodes$colour[1:length(graph$nodes$name)]="1"
graph$nodes$colour[graph$nodes$name== "Avoidence"]="2"
graph$nodes$colour[graph$nodes$name== "PHQ"]="3"
graph$nodes$colour[graph$nodes$name== "GAD"]="4"
graph$nodes$colour[graph$nodes$name== "VAS"]="orange" 
graph$nodes$alpha[1:length(graph$nodes$name)]=1
graph$nodes$size[1:length(graph$nodes$name)]=1.5

graph$nodes$label_family[1:length(graph$nodes$name)] = "serif"
graph$nodes$label_size[1:length(graph$nodes$name)] = 10.5



label_text= paste(graph$edges$est_sig_std)
graph$edges$show[graph$edges$op == "~~"]  =FALSE


graph$edges$label_location=0.3

graph$edges$connect_from[graph$edges$from=="dosa.vs..all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="dosa.vs..all" & graph$edges$to== "PHQ"] = "bottom"
graph$edges$connect_from[graph$edges$from=="dosa.vs..all" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_from[graph$edges$from=="dosa.vs..all" & graph$edges$to== "GAD"] = "top"
graph$edges$connect_from[graph$edges$from=="dosa.vs..all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "GAD"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "VAS"] = "top"

graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "PHQ"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Avoidence"] = "left"
graph$edges$connect_to[graph$edges$from=="VAS" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="GAD" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Avoidence" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="PHQ" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "VAS"] = "left"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Avoidence"] = "bottom"

graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "dosa.vs..all"] = "bottom"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "dosa.vs..all"] = "top"


graph$edges$label= label_text

graph$nodes$label[graph$nodes$name=="SEX.cod"] = "Female gender"
graph$nodes$label[graph$nodes$name=="dosa.vs..all"] = "Dosa"
graph$nodes$node_xmax= graph$nodes$x+1.8
graph$nodes$node_xmin= graph$nodes$x-1.8
graph$nodes$node_ymax= graph$nodes$y+0.6
graph$nodes$node_ymin= graph$nodes$y-0.6
graph$nodes$node_ymax[graph$nodes$name=="dosa.vs..all"]= graph$nodes$y[graph$nodes$name=="dosa.vs..all"]+2
graph$nodes$node_ymin[graph$nodes$name=="dosa.vs..all"]= graph$nodes$y[graph$nodes$name=="dosa.vs..all"]-2


jpeg('Dosa-SIMP.jpg', quality=100, height=945, width=1512)
plot(graph)
dev.off()















###################Canabis#####################

# Get layout information
layout <- get_layout( 
  NA,NA,NA,"SEX.cod",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"Negative.mood",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"Hyperarousal",NA,NA,NA,
  "Canabis.vd.all",NA,NA,"Dissociation",NA,NA,NA,NA,NA,"PDS",
  NA,NA,NA,NA,NA,NA,"Intrusive",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"Avoidence",NA,NA,NA,
  NA,NA,NA,"Age",NA,NA,NA,NA,NA,NA,
  rows = 7
)



# Prepare the graph with the layout
graph <- prepare_graph(fit, layout = layout  )


graph$edges$alpha[1:length(graph$edges$pval)] = NA
graph$edges$linewidth[1:length(graph$edges$pval)] = NA
graph$edges$alpha[!is.na(graph$edges$pval)] = 1.2 -  as.numeric(graph$edges$pval[!is.na(graph$edges$pval)] )^0.5
graph$edges$linewidth[!is.na(graph$edges$est_std)] = 3 * abs(as.numeric(graph$edges$est_std )) +0.5




graph$edges$colour[1:length(graph$edges$to)]="1"
graph$edges$colour[graph$edges$to== "Avoidence"]="2"
graph$edges$colour[graph$edges$to== "Intrusive"]="3"
graph$edges$colour[graph$edges$to== "Hyperarousal"]="4"
graph$edges$colour[graph$edges$to== "Negative.mood"]="orange" 
graph$edges$label_colour = graph$edges$colour



graph$edges$label_size[1:length(graph$edges$name)] = 9
graph$edges$label_family[1:length(graph$edges$name)] = "serif"
graph$edges$label_alpha[1:length(graph$edges$name)] = 1

graph$nodes$colour[1:length(graph$nodes$name)]="1"
graph$nodes$colour[graph$nodes$name== "Avoidence"]="2"
graph$nodes$colour[graph$nodes$name== "Intrusive"]="3"
graph$nodes$colour[graph$nodes$name== "Hyperarousal"]="4"
graph$nodes$colour[graph$nodes$name== "Negative.mood"]="orange" 
graph$nodes$alpha[1:length(graph$nodes$name)]=1
graph$nodes$size[1:length(graph$nodes$name)]=1.5

graph$nodes$label_family[1:length(graph$nodes$name)] = "serif"
graph$nodes$label_size[1:length(graph$nodes$name)] = 10.5



label_text= paste(graph$edges$est_sig_std)
graph$edges$show[graph$edges$op == "~~"]  =FALSE


graph$edges$label_location=0.3

graph$edges$connect_from[graph$edges$from=="Canabis.vd.all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Canabis.vd.all" & graph$edges$to== "Intrusive"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Canabis.vd.all" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_from[graph$edges$from=="Canabis.vd.all" & graph$edges$to== "Hyperarousal"] = "top"
graph$edges$connect_from[graph$edges$from=="Canabis.vd.all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "Hyperarousal"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "Negative.mood"] = "top"

graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Intrusive"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Avoidence"] = "left"
graph$edges$connect_to[graph$edges$from=="Negative.mood" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Hyperarousal" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Avoidence" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Intrusive" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Negative.mood"] = "left"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Avoidence"] = "bottom"

graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Canabis.vd.all"] = "bottom"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Canabis.vd.all"] = "top"


graph$edges$label= label_text

graph$nodes$label[graph$nodes$name=="SEX.cod"] = "Female gender"
graph$nodes$label[graph$nodes$name=="Canabis.vd.all"] = "Canabis"
graph$nodes$label[graph$nodes$name=="Negative.mood"] = "Negative Mood"
graph$nodes$node_xmax= graph$nodes$x+1.8
graph$nodes$node_xmin= graph$nodes$x-1.8
graph$nodes$node_ymax= graph$nodes$y+0.6
graph$nodes$node_ymin= graph$nodes$y-0.6
graph$nodes$node_ymax[graph$nodes$name=="Canabis.vd.all"]= graph$nodes$y[graph$nodes$name=="Canabis.vd.all"]+2
graph$nodes$node_ymin[graph$nodes$name=="Canabis.vd.all"]= graph$nodes$y[graph$nodes$name=="Canabis.vd.all"]-2


jpeg('Canabis-PDS.jpg', quality=100, height=945, width=1512)
plot(graph)
dev.off()









#######################################

# Get layout information
layout <- get_layout( 
  NA,NA,NA,"SEX.cod",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"VAS",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"GAD",NA,NA,NA,
  "Canabis.vd.all",NA,NA,"Dissociation",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"PHQ",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
  NA,NA,NA,"Age",NA,NA,NA,NA,NA,NA,
  rows = 7
)



# Prepare the graph with the layout
graph <- prepare_graph(fit, layout = layout  )


graph$edges$alpha[1:length(graph$edges$pval)] = NA
graph$edges$linewidth[1:length(graph$edges$pval)] = NA
graph$edges$alpha[!is.na(graph$edges$pval)] = 1.2 -  as.numeric(graph$edges$pval[!is.na(graph$edges$pval)] )^0.5
graph$edges$linewidth[!is.na(graph$edges$est_std)] = 3 * abs(as.numeric(graph$edges$est_std )) +0.5



graph$edges$colour[1:length(graph$edges$to)]="1"
graph$edges$colour[graph$edges$to== "Avoidence"]="2"
graph$edges$colour[graph$edges$to== "PHQ"]="3"
graph$edges$colour[graph$edges$to== "GAD"]="4"
graph$edges$colour[graph$edges$to== "VAS"]="orange" 
graph$edges$label_colour = graph$edges$colour



graph$edges$label_size[1:length(graph$edges$name)] = 9
graph$edges$label_family[1:length(graph$edges$name)] = "serif"
graph$edges$label_alpha[1:length(graph$edges$name)] = 1

graph$nodes$colour[1:length(graph$nodes$name)]="1"
graph$nodes$colour[graph$nodes$name== "Avoidence"]="2"
graph$nodes$colour[graph$nodes$name== "PHQ"]="3"
graph$nodes$colour[graph$nodes$name== "GAD"]="4"
graph$nodes$colour[graph$nodes$name== "VAS"]="orange" 
graph$nodes$alpha[1:length(graph$nodes$name)]=1
graph$nodes$size[1:length(graph$nodes$name)]=1.5

graph$nodes$label_family[1:length(graph$nodes$name)] = "serif"
graph$nodes$label_size[1:length(graph$nodes$name)] = 10.5



label_text= paste(graph$edges$est_sig_std)
graph$edges$show[graph$edges$op == "~~"]  =FALSE


graph$edges$label_location=0.3

graph$edges$connect_from[graph$edges$from=="Canabis.vd.all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Canabis.vd.all" & graph$edges$to== "PHQ"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Canabis.vd.all" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_from[graph$edges$from=="Canabis.vd.all" & graph$edges$to== "GAD"] = "top"
graph$edges$connect_from[graph$edges$from=="Canabis.vd.all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "GAD"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "VAS"] = "top"

graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "PHQ"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Avoidence"] = "left"
graph$edges$connect_to[graph$edges$from=="VAS" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="GAD" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Avoidence" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="PHQ" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "VAS"] = "left"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Avoidence"] = "bottom"


graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Canabis.vd.all"] = "bottom"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Canabis.vd.all"] = "top"



graph$edges$label= label_text

graph$nodes$label[graph$nodes$name=="SEX.cod"] = "Female gender"
graph$nodes$label[graph$nodes$name=="Canabis.vd.all"] = "Canabis"
graph$nodes$node_xmax= graph$nodes$x+1.8
graph$nodes$node_xmin= graph$nodes$x-1.8
graph$nodes$node_ymax= graph$nodes$y+0.6
graph$nodes$node_ymin= graph$nodes$y-0.6
graph$nodes$node_ymax[graph$nodes$name=="Canabis.vd.all"]= graph$nodes$y[graph$nodes$name=="Canabis.vd.all"]+2
graph$nodes$node_ymin[graph$nodes$name=="Canabis.vd.all"]= graph$nodes$y[graph$nodes$name=="Canabis.vd.all"]-2


jpeg('Canabis-SIMP.jpg', quality=100, height=945, width=1512)
plot(graph)
dev.off()













###############DOCTOR########################

# Get layout information
layout <- get_layout( 
  NA,NA,NA,"SEX.cod",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"Negative.mood",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"Hyperarousal",NA,NA,NA,
  "DOCTOR.vs..all",NA,NA,"Dissociation",NA,NA,NA,NA,NA,"PDS",
  NA,NA,NA,NA,NA,NA,"Intrusive",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"Avoidence",NA,NA,NA,
  NA,NA,NA,"Age",NA,NA,NA,NA,NA,NA,
  rows = 7
)



# Prepare the graph with the layout
graph <- prepare_graph(fit, layout = layout  )


graph$edges$alpha[1:length(graph$edges$pval)] = NA
graph$edges$linewidth[1:length(graph$edges$pval)] = NA
graph$edges$alpha[!is.na(graph$edges$pval)] = 1.2 -  as.numeric(graph$edges$pval[!is.na(graph$edges$pval)] )^0.5
graph$edges$linewidth[!is.na(graph$edges$est_std)] = 3 * abs(as.numeric(graph$edges$est_std )) +0.5




graph$edges$colour[1:length(graph$edges$to)]="1"
graph$edges$colour[graph$edges$to== "Avoidence"]="2"
graph$edges$colour[graph$edges$to== "Intrusive"]="3"
graph$edges$colour[graph$edges$to== "Hyperarousal"]="4"
graph$edges$colour[graph$edges$to== "Negative.mood"]="orange" 
graph$edges$label_colour = graph$edges$colour



graph$edges$label_size[1:length(graph$edges$name)] = 9
graph$edges$label_family[1:length(graph$edges$name)] = "serif"
graph$edges$label_alpha[1:length(graph$edges$name)] = 1

graph$nodes$colour[1:length(graph$nodes$name)]="1"
graph$nodes$colour[graph$nodes$name== "Avoidence"]="2"
graph$nodes$colour[graph$nodes$name== "Intrusive"]="3"
graph$nodes$colour[graph$nodes$name== "Hyperarousal"]="4"
graph$nodes$colour[graph$nodes$name== "Negative.mood"]="orange" 
graph$nodes$alpha[1:length(graph$nodes$name)]=1
graph$nodes$size[1:length(graph$nodes$name)]=1.5

graph$nodes$label_family[1:length(graph$nodes$name)] = "serif"
graph$nodes$label_size[1:length(graph$nodes$name)] = 10.5



label_text= paste(graph$edges$est_sig_std)
graph$edges$show[graph$edges$op == "~~"]  =FALSE


graph$edges$label_location=0.3

graph$edges$connect_from[graph$edges$from=="DOCTOR.vs..all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="DOCTOR.vs..all" & graph$edges$to== "Intrusive"] = "bottom"
graph$edges$connect_from[graph$edges$from=="DOCTOR.vs..all" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_from[graph$edges$from=="DOCTOR.vs..all" & graph$edges$to== "Hyperarousal"] = "top"
graph$edges$connect_from[graph$edges$from=="DOCTOR.vs..all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "Hyperarousal"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "Negative.mood"] = "top"

graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Intrusive"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Avoidence"] = "left"
graph$edges$connect_to[graph$edges$from=="Negative.mood" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Hyperarousal" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Avoidence" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Intrusive" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Negative.mood"] = "left"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Avoidence"] = "bottom"


graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "DOCTOR.vs..all"] = "bottom"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "DOCTOR.vs..all"] = "top"



graph$edges$label= label_text

graph$nodes$label[graph$nodes$name=="SEX.cod"] = "Female gender"
graph$nodes$label[graph$nodes$name=="DOCTOR.vs..all"] = "DOCTOR"
graph$nodes$label[graph$nodes$name=="Negative.mood"] = "Negative Mood"
graph$nodes$node_xmax= graph$nodes$x+1.8
graph$nodes$node_xmin= graph$nodes$x-1.8
graph$nodes$node_ymax= graph$nodes$y+0.6
graph$nodes$node_ymin= graph$nodes$y-0.6
graph$nodes$node_ymax[graph$nodes$name=="DOCTOR.vs..all"]= graph$nodes$y[graph$nodes$name=="DOCTOR.vs..all"]+2
graph$nodes$node_ymin[graph$nodes$name=="DOCTOR.vs..all"]= graph$nodes$y[graph$nodes$name=="DOCTOR.vs..all"]-2


jpeg('DOCTOR-PDS.jpg', quality=100, height=945, width=1512)
plot(graph)
dev.off()









#######################################

# Get layout information
layout <- get_layout( 
  NA,NA,NA,"SEX.cod",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"VAS",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"GAD",NA,NA,NA,
  "DOCTOR.vs..all",NA,NA,"Dissociation",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"PHQ",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
  NA,NA,NA,"Age",NA,NA,NA,NA,NA,NA,
  rows = 7
)



# Prepare the graph with the layout
graph <- prepare_graph(fit, layout = layout  )


graph$edges$alpha[1:length(graph$edges$pval)] = NA
graph$edges$linewidth[1:length(graph$edges$pval)] = NA
graph$edges$alpha[!is.na(graph$edges$pval)] = 1.2 -  as.numeric(graph$edges$pval[!is.na(graph$edges$pval)] )^0.5
graph$edges$linewidth[!is.na(graph$edges$est_std)] = 3 * abs(as.numeric(graph$edges$est_std )) +0.5



graph$edges$colour[1:length(graph$edges$to)]="1"
graph$edges$colour[graph$edges$to== "Avoidence"]="2"
graph$edges$colour[graph$edges$to== "PHQ"]="3"
graph$edges$colour[graph$edges$to== "GAD"]="4"
graph$edges$colour[graph$edges$to== "VAS"]="orange" 
graph$edges$label_colour = graph$edges$colour



graph$edges$label_size[1:length(graph$edges$name)] = 9
graph$edges$label_family[1:length(graph$edges$name)] = "serif"
graph$edges$label_alpha[1:length(graph$edges$name)] = 1

graph$nodes$colour[1:length(graph$nodes$name)]="1"
graph$nodes$colour[graph$nodes$name== "Avoidence"]="2"
graph$nodes$colour[graph$nodes$name== "PHQ"]="3"
graph$nodes$colour[graph$nodes$name== "GAD"]="4"
graph$nodes$colour[graph$nodes$name== "VAS"]="orange" 
graph$nodes$alpha[1:length(graph$nodes$name)]=1
graph$nodes$size[1:length(graph$nodes$name)]=1.5

graph$nodes$label_family[1:length(graph$nodes$name)] = "serif"
graph$nodes$label_size[1:length(graph$nodes$name)] = 10.5



label_text= paste(graph$edges$est_sig_std)
graph$edges$show[graph$edges$op == "~~"]  =FALSE


graph$edges$label_location=0.3

graph$edges$connect_from[graph$edges$from=="DOCTOR.vs..all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="DOCTOR.vs..all" & graph$edges$to== "PHQ"] = "bottom"
graph$edges$connect_from[graph$edges$from=="DOCTOR.vs..all" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_from[graph$edges$from=="DOCTOR.vs..all" & graph$edges$to== "GAD"] = "top"
graph$edges$connect_from[graph$edges$from=="DOCTOR.vs..all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "GAD"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "VAS"] = "top"

graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "PHQ"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Avoidence"] = "left"
graph$edges$connect_to[graph$edges$from=="VAS" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="GAD" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Avoidence" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="PHQ" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "VAS"] = "left"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Avoidence"] = "bottom"


graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "DOCTOR.vs..all"] = "bottom"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "DOCTOR.vs..all"] = "top"

graph$edges$label= label_text

graph$nodes$label[graph$nodes$name=="SEX.cod"] = "Female gender"
graph$nodes$label[graph$nodes$name=="DOCTOR.vs..all"] = "DOCTOR"
graph$nodes$node_xmax= graph$nodes$x+1.8
graph$nodes$node_xmin= graph$nodes$x-1.8
graph$nodes$node_ymax= graph$nodes$y+0.6
graph$nodes$node_ymin= graph$nodes$y-0.6
graph$nodes$node_ymax[graph$nodes$name=="DOCTOR.vs..all"]= graph$nodes$y[graph$nodes$name=="DOCTOR.vs..all"]+2
graph$nodes$node_ymin[graph$nodes$name=="DOCTOR.vs..all"]= graph$nodes$y[graph$nodes$name=="DOCTOR.vs..all"]-2


jpeg('DOCTOR-SIMP.jpg', quality=100, height=945, width=1512)
plot(graph)
dev.off()













###############MDMA#########################

# Get layout information
layout <- get_layout( 
  NA,NA,NA,"SEX.cod",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"Negative.mood",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"Hyperarousal",NA,NA,NA,
  "MDMA.vs.all",NA,NA,"Dissociation",NA,NA,NA,NA,NA,"PDS",
  NA,NA,NA,NA,NA,NA,"Intrusive",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"Avoidence",NA,NA,NA,
  NA,NA,NA,"Age",NA,NA,NA,NA,NA,NA,
  rows = 7
)



# Prepare the graph with the layout
graph <- prepare_graph(fit, layout = layout  )


graph$edges$alpha[1:length(graph$edges$pval)] = NA
graph$edges$linewidth[1:length(graph$edges$pval)] = NA
graph$edges$alpha[!is.na(graph$edges$pval)] = 1.2 -  as.numeric(graph$edges$pval[!is.na(graph$edges$pval)] )^0.5
graph$edges$linewidth[!is.na(graph$edges$est_std)] = 3 * abs(as.numeric(graph$edges$est_std )) +0.5




graph$edges$colour[1:length(graph$edges$to)]="1"
graph$edges$colour[graph$edges$to== "Avoidence"]="2"
graph$edges$colour[graph$edges$to== "Intrusive"]="3"
graph$edges$colour[graph$edges$to== "Hyperarousal"]="4"
graph$edges$colour[graph$edges$to== "Negative.mood"]="orange" 
graph$edges$label_colour = graph$edges$colour



graph$edges$label_size[1:length(graph$edges$name)] = 9
graph$edges$label_family[1:length(graph$edges$name)] = "serif"
graph$edges$label_alpha[1:length(graph$edges$name)] = 1

graph$nodes$colour[1:length(graph$nodes$name)]="1"
graph$nodes$colour[graph$nodes$name== "Avoidence"]="2"
graph$nodes$colour[graph$nodes$name== "Intrusive"]="3"
graph$nodes$colour[graph$nodes$name== "Hyperarousal"]="4"
graph$nodes$colour[graph$nodes$name== "Negative.mood"]="orange" 
graph$nodes$alpha[1:length(graph$nodes$name)]=1
graph$nodes$size[1:length(graph$nodes$name)]=1.5

graph$nodes$label_family[1:length(graph$nodes$name)] = "serif"
graph$nodes$label_size[1:length(graph$nodes$name)] = 10.5



label_text= paste(graph$edges$est_sig_std)
graph$edges$show[graph$edges$op == "~~"]  =FALSE


graph$edges$label_location=0.3

graph$edges$connect_from[graph$edges$from=="MDMA.vs.all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="MDMA.vs.all" & graph$edges$to== "Intrusive"] = "bottom"
graph$edges$connect_from[graph$edges$from=="MDMA.vs.all" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_from[graph$edges$from=="MDMA.vs.all" & graph$edges$to== "Hyperarousal"] = "top"
graph$edges$connect_from[graph$edges$from=="MDMA.vs.all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "Hyperarousal"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "Negative.mood"] = "top"

graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Intrusive"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Avoidence"] = "left"
graph$edges$connect_to[graph$edges$from=="Negative.mood" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Hyperarousal" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Avoidence" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Intrusive" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Negative.mood"] = "left"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Avoidence"] = "bottom"


graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "MDMA.vs.all"] = "bottom"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "MDMA.vs.all"] = "top"

graph$edges$label= label_text

graph$nodes$label[graph$nodes$name=="SEX.cod"] = "Female gender"
graph$nodes$label[graph$nodes$name=="MDMA.vs.all"] = "MDMA"
graph$nodes$label[graph$nodes$name=="Negative.mood"] = "Negative Mood"
graph$nodes$node_xmax= graph$nodes$x+1.8
graph$nodes$node_xmin= graph$nodes$x-1.8
graph$nodes$node_ymax= graph$nodes$y+0.6
graph$nodes$node_ymin= graph$nodes$y-0.6
graph$nodes$node_ymax[graph$nodes$name=="MDMA.vs.all"]= graph$nodes$y[graph$nodes$name=="MDMA.vs.all"]+2
graph$nodes$node_ymin[graph$nodes$name=="MDMA.vs.all"]= graph$nodes$y[graph$nodes$name=="MDMA.vs.all"]-2


jpeg('MDMA-PDS.jpg', quality=100, height=945, width=1512)
plot(graph)
dev.off()









#######################################

# Get layout information
layout <- get_layout( 
  NA,NA,NA,"SEX.cod",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"VAS",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"GAD",NA,NA,NA,
  "MDMA.vs.all",NA,NA,"Dissociation",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"PHQ",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
  NA,NA,NA,"Age",NA,NA,NA,NA,NA,NA,
  rows = 7
)



# Prepare the graph with the layout
graph <- prepare_graph(fit, layout = layout  )


graph$edges$alpha[1:length(graph$edges$pval)] = NA
graph$edges$linewidth[1:length(graph$edges$pval)] = NA
graph$edges$alpha[!is.na(graph$edges$pval)] = 1.2 -  as.numeric(graph$edges$pval[!is.na(graph$edges$pval)] )^0.5
graph$edges$linewidth[!is.na(graph$edges$est_std)] = 3 * abs(as.numeric(graph$edges$est_std )) +0.5



graph$edges$colour[1:length(graph$edges$to)]="1"
graph$edges$colour[graph$edges$to== "Avoidence"]="2"
graph$edges$colour[graph$edges$to== "PHQ"]="3"
graph$edges$colour[graph$edges$to== "GAD"]="4"
graph$edges$colour[graph$edges$to== "VAS"]="orange" 
graph$edges$label_colour = graph$edges$colour



graph$edges$label_size[1:length(graph$edges$name)] = 9
graph$edges$label_family[1:length(graph$edges$name)] = "serif"
graph$edges$label_alpha[1:length(graph$edges$name)] = 1

graph$nodes$colour[1:length(graph$nodes$name)]="1"
graph$nodes$colour[graph$nodes$name== "Avoidence"]="2"
graph$nodes$colour[graph$nodes$name== "PHQ"]="3"
graph$nodes$colour[graph$nodes$name== "GAD"]="4"
graph$nodes$colour[graph$nodes$name== "VAS"]="orange" 
graph$nodes$alpha[1:length(graph$nodes$name)]=1
graph$nodes$size[1:length(graph$nodes$name)]=1.5

graph$nodes$label_family[1:length(graph$nodes$name)] = "serif"
graph$nodes$label_size[1:length(graph$nodes$name)] = 10.5



label_text= paste(graph$edges$est_sig_std)
graph$edges$show[graph$edges$op == "~~"]  =FALSE


graph$edges$label_location=0.3

graph$edges$connect_from[graph$edges$from=="MDMA.vs.all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="MDMA.vs.all" & graph$edges$to== "PHQ"] = "bottom"
graph$edges$connect_from[graph$edges$from=="MDMA.vs.all" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_from[graph$edges$from=="MDMA.vs.all" & graph$edges$to== "GAD"] = "top"
graph$edges$connect_from[graph$edges$from=="MDMA.vs.all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "GAD"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "VAS"] = "top"

graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "PHQ"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Avoidence"] = "left"
graph$edges$connect_to[graph$edges$from=="VAS" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="GAD" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Avoidence" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="PHQ" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "VAS"] = "left"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Avoidence"] = "bottom"


graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "MDMA.vs.all"] = "bottom"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "MDMA.vs.all"] = "top"


graph$edges$label= label_text

graph$nodes$label[graph$nodes$name=="SEX.cod"] = "Female gender"
graph$nodes$label[graph$nodes$name=="MDMA.vs.all"] = "MDMA"
graph$nodes$node_xmax= graph$nodes$x+1.8
graph$nodes$node_xmin= graph$nodes$x-1.8
graph$nodes$node_ymax= graph$nodes$y+0.6
graph$nodes$node_ymin= graph$nodes$y-0.6
graph$nodes$node_ymax[graph$nodes$name=="MDMA.vs.all"]= graph$nodes$y[graph$nodes$name=="MDMA.vs.all"]+2
graph$nodes$node_ymin[graph$nodes$name=="MDMA.vs.all"]= graph$nodes$y[graph$nodes$name=="MDMA.vs.all"]-2


jpeg('MDMA-SIMP.jpg', quality=100, height=945, width=1512)
plot(graph)
dev.off()










###############LSD#########################

# Get layout information
layout <- get_layout( 
  NA,NA,NA,"SEX.cod",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"Negative.mood",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"Hyperarousal",NA,NA,NA,
  "LSD.vs.all",NA,NA,"Dissociation",NA,NA,NA,NA,NA,"PDS",
  NA,NA,NA,NA,NA,NA,"Intrusive",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"Avoidence",NA,NA,NA,
  NA,NA,NA,"Age",NA,NA,NA,NA,NA,NA,
  rows = 7
)



# Prepare the graph with the layout
graph <- prepare_graph(fit, layout = layout  )


graph$edges$alpha[1:length(graph$edges$pval)] = NA
graph$edges$linewidth[1:length(graph$edges$pval)] = NA
graph$edges$alpha[!is.na(graph$edges$pval)] = 1.2 -  as.numeric(graph$edges$pval[!is.na(graph$edges$pval)] )^0.5
graph$edges$linewidth[!is.na(graph$edges$est_std)] = 3 * abs(as.numeric(graph$edges$est_std )) +0.5




graph$edges$colour[1:length(graph$edges$to)]="1"
graph$edges$colour[graph$edges$to== "Avoidence"]="2"
graph$edges$colour[graph$edges$to== "Intrusive"]="3"
graph$edges$colour[graph$edges$to== "Hyperarousal"]="4"
graph$edges$colour[graph$edges$to== "Negative.mood"]="orange" 
graph$edges$label_colour = graph$edges$colour



graph$edges$label_size[1:length(graph$edges$name)] = 9
graph$edges$label_family[1:length(graph$edges$name)] = "serif"
graph$edges$label_alpha[1:length(graph$edges$name)] = 1

graph$nodes$colour[1:length(graph$nodes$name)]="1"
graph$nodes$colour[graph$nodes$name== "Avoidence"]="2"
graph$nodes$colour[graph$nodes$name== "Intrusive"]="3"
graph$nodes$colour[graph$nodes$name== "Hyperarousal"]="4"
graph$nodes$colour[graph$nodes$name== "Negative.mood"]="orange" 
graph$nodes$alpha[1:length(graph$nodes$name)]=1
graph$nodes$size[1:length(graph$nodes$name)]=1.5

graph$nodes$label_family[1:length(graph$nodes$name)] = "serif"
graph$nodes$label_size[1:length(graph$nodes$name)] = 10.5



label_text= paste(graph$edges$est_sig_std)
graph$edges$show[graph$edges$op == "~~"]  =FALSE


graph$edges$label_location=0.3

graph$edges$connect_from[graph$edges$from=="LSD.vs.all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="LSD.vs.all" & graph$edges$to== "Intrusive"] = "bottom"
graph$edges$connect_from[graph$edges$from=="LSD.vs.all" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_from[graph$edges$from=="LSD.vs.all" & graph$edges$to== "Hyperarousal"] = "top"
graph$edges$connect_from[graph$edges$from=="LSD.vs.all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "Hyperarousal"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "Negative.mood"] = "top"

graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Intrusive"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Avoidence"] = "left"
graph$edges$connect_to[graph$edges$from=="Negative.mood" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Hyperarousal" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Avoidence" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Intrusive" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Negative.mood"] = "left"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Avoidence"] = "bottom"


graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "LSD.vs.all"] = "bottom"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "LSD.vs.all"] = "top"


graph$edges$label= label_text

graph$nodes$label[graph$nodes$name=="SEX.cod"] = "Female gender"
graph$nodes$label[graph$nodes$name=="LSD.vs.all"] = "LSD"
graph$nodes$label[graph$nodes$name=="Negative.mood"] = "Negative Mood"
graph$nodes$node_xmax= graph$nodes$x+1.8
graph$nodes$node_xmin= graph$nodes$x-1.8
graph$nodes$node_ymax= graph$nodes$y+0.6
graph$nodes$node_ymin= graph$nodes$y-0.6
graph$nodes$node_ymax[graph$nodes$name=="LSD.vs.all"]= graph$nodes$y[graph$nodes$name=="LSD.vs.all"]+2
graph$nodes$node_ymin[graph$nodes$name=="LSD.vs.all"]= graph$nodes$y[graph$nodes$name=="LSD.vs.all"]-2


jpeg('LSD-PDS.jpg', quality=100, height=945, width=1512)
plot(graph)
dev.off()









#######################################

# Get layout information
layout <- get_layout( 
  NA,NA,NA,"SEX.cod",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"VAS",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"GAD",NA,NA,NA,
  "LSD.vs.all",NA,NA,"Dissociation",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"PHQ",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
  NA,NA,NA,"Age",NA,NA,NA,NA,NA,NA,
  rows = 7
)



# Prepare the graph with the layout
graph <- prepare_graph(fit, layout = layout  )


graph$edges$alpha[1:length(graph$edges$pval)] = NA
graph$edges$linewidth[1:length(graph$edges$pval)] = NA
graph$edges$alpha[!is.na(graph$edges$pval)] = 1.2 -  as.numeric(graph$edges$pval[!is.na(graph$edges$pval)] )^0.5
graph$edges$linewidth[!is.na(graph$edges$est_std)] = 3 * abs(as.numeric(graph$edges$est_std )) +0.5



graph$edges$colour[1:length(graph$edges$to)]="1"
graph$edges$colour[graph$edges$to== "Avoidence"]="2"
graph$edges$colour[graph$edges$to== "PHQ"]="3"
graph$edges$colour[graph$edges$to== "GAD"]="4"
graph$edges$colour[graph$edges$to== "VAS"]="orange" 
graph$edges$label_colour = graph$edges$colour



graph$edges$label_size[1:length(graph$edges$name)] = 9
graph$edges$label_family[1:length(graph$edges$name)] = "serif"
graph$edges$label_alpha[1:length(graph$edges$name)] = 1

graph$nodes$colour[1:length(graph$nodes$name)]="1"
graph$nodes$colour[graph$nodes$name== "Avoidence"]="2"
graph$nodes$colour[graph$nodes$name== "PHQ"]="3"
graph$nodes$colour[graph$nodes$name== "GAD"]="4"
graph$nodes$colour[graph$nodes$name== "VAS"]="orange" 
graph$nodes$alpha[1:length(graph$nodes$name)]=1
graph$nodes$size[1:length(graph$nodes$name)]=1.5

graph$nodes$label_family[1:length(graph$nodes$name)] = "serif"
graph$nodes$label_size[1:length(graph$nodes$name)] = 10.5



label_text= paste(graph$edges$est_sig_std)
graph$edges$show[graph$edges$op == "~~"]  =FALSE


graph$edges$label_location=0.3

graph$edges$connect_from[graph$edges$from=="LSD.vs.all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="LSD.vs.all" & graph$edges$to== "PHQ"] = "bottom"
graph$edges$connect_from[graph$edges$from=="LSD.vs.all" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_from[graph$edges$from=="LSD.vs.all" & graph$edges$to== "GAD"] = "top"
graph$edges$connect_from[graph$edges$from=="LSD.vs.all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "GAD"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "VAS"] = "top"

graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "PHQ"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Avoidence"] = "left"
graph$edges$connect_to[graph$edges$from=="VAS" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="GAD" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Avoidence" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="PHQ" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "VAS"] = "left"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Avoidence"] = "bottom"

graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "LSD.vs.all"] = "bottom"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "LSD.vs.all"] = "top"

graph$edges$label= label_text

graph$nodes$label[graph$nodes$name=="SEX.cod"] = "Female gender"
graph$nodes$label[graph$nodes$name=="LSD.vs.all"] = "LSD"
graph$nodes$node_xmax= graph$nodes$x+1.8
graph$nodes$node_xmin= graph$nodes$x-1.8
graph$nodes$node_ymax= graph$nodes$y+0.6
graph$nodes$node_ymin= graph$nodes$y-0.6
graph$nodes$node_ymax[graph$nodes$name=="LSD.vs.all"]= graph$nodes$y[graph$nodes$name=="LSD.vs.all"]+2
graph$nodes$node_ymin[graph$nodes$name=="LSD.vs.all"]= graph$nodes$y[graph$nodes$name=="LSD.vs.all"]-2


jpeg('LSD-SIMP.jpg', quality=100, height=945, width=1512)
plot(graph)
dev.off()













###############Cocaine#########################

# Get layout information
layout <- get_layout( 
  NA,NA,NA,"SEX.cod",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"Negative.mood",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"Hyperarousal",NA,NA,NA,
  "Cocaine.vs.all",NA,NA,"Dissociation",NA,NA,NA,NA,NA,"PDS",
  NA,NA,NA,NA,NA,NA,"Intrusive",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"Avoidence",NA,NA,NA,
  NA,NA,NA,"Age",NA,NA,NA,NA,NA,NA,
  rows = 7
)



# Prepare the graph with the layout
graph <- prepare_graph(fit, layout = layout  )


graph$edges$alpha[1:length(graph$edges$pval)] = NA
graph$edges$linewidth[1:length(graph$edges$pval)] = NA
graph$edges$alpha[!is.na(graph$edges$pval)] = 1.2 -  as.numeric(graph$edges$pval[!is.na(graph$edges$pval)] )^0.5
graph$edges$linewidth[!is.na(graph$edges$est_std)] = 3 * abs(as.numeric(graph$edges$est_std )) +0.5




graph$edges$colour[1:length(graph$edges$to)]="1"
graph$edges$colour[graph$edges$to== "Avoidence"]="2"
graph$edges$colour[graph$edges$to== "Intrusive"]="3"
graph$edges$colour[graph$edges$to== "Hyperarousal"]="4"
graph$edges$colour[graph$edges$to== "Negative.mood"]="orange" 
graph$edges$label_colour = graph$edges$colour



graph$edges$label_size[1:length(graph$edges$name)] = 9
graph$edges$label_family[1:length(graph$edges$name)] = "serif"
graph$edges$label_alpha[1:length(graph$edges$name)] = 1

graph$nodes$colour[1:length(graph$nodes$name)]="1"
graph$nodes$colour[graph$nodes$name== "Avoidence"]="2"
graph$nodes$colour[graph$nodes$name== "Intrusive"]="3"
graph$nodes$colour[graph$nodes$name== "Hyperarousal"]="4"
graph$nodes$colour[graph$nodes$name== "Negative.mood"]="orange" 
graph$nodes$alpha[1:length(graph$nodes$name)]=1
graph$nodes$size[1:length(graph$nodes$name)]=1.5

graph$nodes$label_family[1:length(graph$nodes$name)] = "serif"
graph$nodes$label_size[1:length(graph$nodes$name)] = 10.5



label_text= paste(graph$edges$est_sig_std)
graph$edges$show[graph$edges$op == "~~"]  =FALSE


graph$edges$label_location=0.3

graph$edges$connect_from[graph$edges$from=="Cocaine.vs.all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Cocaine.vs.all" & graph$edges$to== "Intrusive"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Cocaine.vs.all" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_from[graph$edges$from=="Cocaine.vs.all" & graph$edges$to== "Hyperarousal"] = "top"
graph$edges$connect_from[graph$edges$from=="Cocaine.vs.all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "Hyperarousal"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "Negative.mood"] = "top"

graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Negative.mood"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Intrusive"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Avoidence"] = "left"
graph$edges$connect_to[graph$edges$from=="Negative.mood" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Hyperarousal" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Avoidence" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Intrusive" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Negative.mood"] = "left"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Avoidence"] = "bottom"

graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Cocaine.vs.all"] = "bottom"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Cocaine.vs.all"] = "top"

graph$edges$label= label_text

graph$nodes$label[graph$nodes$name=="SEX.cod"] = "Female gender"
graph$nodes$label[graph$nodes$name=="Cocaine.vs.all"] = "Cocaine"
graph$nodes$label[graph$nodes$name=="Negative.mood"] = "Negative Mood"
graph$nodes$node_xmax= graph$nodes$x+1.8
graph$nodes$node_xmin= graph$nodes$x-1.8
graph$nodes$node_ymax= graph$nodes$y+0.6
graph$nodes$node_ymin= graph$nodes$y-0.6
graph$nodes$node_ymax[graph$nodes$name=="Cocaine.vs.all"]= graph$nodes$y[graph$nodes$name=="Cocaine.vs.all"]+2
graph$nodes$node_ymin[graph$nodes$name=="Cocaine.vs.all"]= graph$nodes$y[graph$nodes$name=="Cocaine.vs.all"]-2


jpeg('Cocaine-PDS.jpg', quality=100, height=945, width=1512)
plot(graph)
dev.off()









#######################################

# Get layout information
layout <- get_layout( 
  NA,NA,NA,"SEX.cod",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"VAS",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"GAD",NA,NA,NA,
  "Cocaine.vs.all",NA,NA,"Dissociation",NA,NA,NA,NA,NA,NA,
  NA,NA,NA,NA,NA,NA,"PHQ",NA,NA,NA,
  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
  NA,NA,NA,"Age",NA,NA,NA,NA,NA,NA,
  rows = 7
)



# Prepare the graph with the layout
graph <- prepare_graph(fit, layout = layout  )


graph$edges$alpha[1:length(graph$edges$pval)] = NA
graph$edges$linewidth[1:length(graph$edges$pval)] = NA
graph$edges$alpha[!is.na(graph$edges$pval)] = 1.2 -  as.numeric(graph$edges$pval[!is.na(graph$edges$pval)] )^0.5
graph$edges$linewidth[!is.na(graph$edges$est_std)] = 3 * abs(as.numeric(graph$edges$est_std )) +0.5



graph$edges$colour[1:length(graph$edges$to)]="1"
graph$edges$colour[graph$edges$to== "Avoidence"]="2"
graph$edges$colour[graph$edges$to== "PHQ"]="3"
graph$edges$colour[graph$edges$to== "GAD"]="4"
graph$edges$colour[graph$edges$to== "VAS"]="orange" 
graph$edges$label_colour = graph$edges$colour



graph$edges$label_size[1:length(graph$edges$name)] = 9
graph$edges$label_family[1:length(graph$edges$name)] = "serif"
graph$edges$label_alpha[1:length(graph$edges$name)] = 1

graph$nodes$colour[1:length(graph$nodes$name)]="1"
graph$nodes$colour[graph$nodes$name== "Avoidence"]="2"
graph$nodes$colour[graph$nodes$name== "PHQ"]="3"
graph$nodes$colour[graph$nodes$name== "GAD"]="4"
graph$nodes$colour[graph$nodes$name== "VAS"]="orange" 
graph$nodes$alpha[1:length(graph$nodes$name)]=1
graph$nodes$size[1:length(graph$nodes$name)]=1.5

graph$nodes$label_family[1:length(graph$nodes$name)] = "serif"
graph$nodes$label_size[1:length(graph$nodes$name)] = 10.5



label_text= paste(graph$edges$est_sig_std)
graph$edges$show[graph$edges$op == "~~"]  =FALSE


graph$edges$label_location=0.3

graph$edges$connect_from[graph$edges$from=="Cocaine.vs.all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Cocaine.vs.all" & graph$edges$to== "PHQ"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Cocaine.vs.all" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_from[graph$edges$from=="Cocaine.vs.all" & graph$edges$to== "GAD"] = "top"
graph$edges$connect_from[graph$edges$from=="Cocaine.vs.all" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "Avoidence"] = "bottom"
graph$edges$connect_from[graph$edges$from=="Dissociation" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "GAD"] = "top"
graph$edges$connect_from[graph$edges$from=="Age" & graph$edges$to== "VAS"] = "top"

graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "VAS"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "PHQ"] = "top"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Avoidence"] = "left"
graph$edges$connect_to[graph$edges$from=="VAS" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="GAD" & graph$edges$to== "PDS"] = "top"
graph$edges$connect_to[graph$edges$from=="Avoidence" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="PHQ" & graph$edges$to== "PDS"] = "bottom"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "VAS"] = "left"
graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Avoidence"] = "bottom"


graph$edges$connect_to[graph$edges$from=="Age" & graph$edges$to== "Cocaine.vs.all"] = "bottom"
graph$edges$connect_to[graph$edges$from=="SEX.cod" & graph$edges$to== "Cocaine.vs.all"] = "top"


graph$edges$label= label_text

graph$nodes$label[graph$nodes$name=="SEX.cod"] = "Female gender"
graph$nodes$label[graph$nodes$name=="Cocaine.vs.all"] = "Cocaine"
graph$nodes$node_xmax= graph$nodes$x+1.8
graph$nodes$node_xmin= graph$nodes$x-1.8
graph$nodes$node_ymax= graph$nodes$y+0.6
graph$nodes$node_ymin= graph$nodes$y-0.6
graph$nodes$node_ymax[graph$nodes$name=="Cocaine.vs.all"]= graph$nodes$y[graph$nodes$name=="Cocaine.vs.all"]+2
graph$nodes$node_ymin[graph$nodes$name=="Cocaine.vs.all"]= graph$nodes$y[graph$nodes$name=="Cocaine.vs.all"]-2


jpeg('Cocaine-SIMP.jpg', quality=100, height=945, width=1512)
plot(graph)
dev.off()






######## Model Fit ##########






# Capture the summary text
fitest <- capture.output(
  fitMeasures(fit, output = "text",
              fit.measures = c(
                "chisq", "df", "pvalue", 
                "cfi", "gfi", "rmsea")))


# Save the summary text to a text file
writeLines(fitest, paste("fitest.txt"))


modificationindices= capture.output(modificationindices(fit, sort. = TRUE,
                                   minimum.value = 5))
writeLines(modificationindices, paste("modificationindices.txt"))
