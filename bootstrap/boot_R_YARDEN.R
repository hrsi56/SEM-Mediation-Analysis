setwd("C:/Users/hagitc/OneDrive - Ben Gurion University of the Negev/hagitcloud/RESULT/October 7 -2023 - trauma 1/R/bootstrap")
fit = readRDS( file = "bootstrap_fit.rds")

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


library(semhelpinghands)

# Save the summary text to a text file
write.csv((standardizedSolution_boot_ci(fit)), paste("standardizedSolution_boot_ci.csv"))

