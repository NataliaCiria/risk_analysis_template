#### Set-up ####

#open risk_analysis_template.Rproj (RECOMMENDED)
#or set working directory to this folder:
#setwd("[directory]\\risk_analysis_template")
#install.packages(c("mc2d","ggplot2"))

#Load packages
library(mc2d) # Monte-Carlo simulations
library(ggplot2) #Plot graphs

#Load data
data<-read.csv("data.csv", sep=";" ,stringsAsFactors=TRUE, na.strings=c(NA,"NA",""))
print(data) #One row equals one variate in Monte Carlo simulation

#### Create mc objects (mcnodes) #### 

##### Fixed variables ##### 
mcnodes_name<-names(data[sapply(data,is.numeric)])

for(i in 1:length(mcnodes_name)){
  mcnode_i<-mcdata(data[[mcnodes_name[i]]],type="0",nvariates=nrow(data))
  assign(mcnodes_name[i],mcnode_i)
}

##### Stochastic Variables ##### 
h_prev<-mcstoc(func=runif,
               min=h_prev_min, 
               max=h_prev_max, 
               nvariates=nrow(data))

w_prev<-mcstoc(func=runif,
               min=w_prev_min, 
               max=w_prev_max, 
               nvariates=nrow(data))

n_animals<-mcstoc(func=rnorm,
                  mean=n_animals_mean,
                  sd=n_animals_sd,
                  nvariates=nrow(data))

test_sensi<-mcstoc(func=rpert,
                   min=test_sensi_min, 
                   mode=test_sensi_mode, 
                   max=test_sensi_max, 
                   nvariates=nrow(data))


#### Write model ####



#### Model output ####

##### Analyze #####

##### Plot #####

##### Save #####


