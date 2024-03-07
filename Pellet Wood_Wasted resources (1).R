# Load data and package
data1<-read.csv("C:/Users/ACER/Dropbox/Research/@_Team/@_Fandom/BMF collaborative projects/2023/BMF CP 17 (Pellet wood_wasted resources)/Data/Survey_data.csv",header = TRUE,stringsAsFactors = TRUE)
library(bayesvl)

data1$Age<-data1$q25_year
data1$Sex<-data1$q26_male
data1$LandownerOrg<-data1$q28_landowner
data1$ForestryOrg<-data1$q28_forestry
data1$Income<-data1$q31_income

keeps <- c("Sex","Age","LandownerOrg","Income","ForestryOrg","WastedOpportunity")
data1 <- data1[keeps]
data1<-na.omit(data1) 



# Model construction
model1<-bayesvl()
model1<-bvl_addNode(model1,"WastedOpportunity","binom")
model1<-bvl_addNode(model1,"Age","norm")
model1<-bvl_addNode(model1,"Sex","binom")
model1<-bvl_addNode(model1,"Income","norm")
model1<-bvl_addNode(model1,"LandownerOrg","norm")
model1<-bvl_addNode(model1,"ForestryOrg","norm")

model1<-bvl_addArc(model1,"Age","WastedOpportunity","slope")
model1<-bvl_addArc(model1,"Income","WastedOpportunity","slope")
model1<-bvl_addArc(model1,"LandownerOrg","WastedOpportunity","slope")
model1<-bvl_addArc(model1,"ForestryOrg","WastedOpportunity","slope")
model1<-bvl_addArc(model1,"Sex","WastedOpportunity","slope")

bvl_bnPlot(model1)

# Generate Stan code
model_string1a<- bvl_model2Stan(model1)
cat(model_string1a) 

# Model Fit
model1<-bvl_modelFit(model1, data1, warmup = 2000, iter = 5000, chains = 4,cores = 4) 

bvl_plotTrace(model1)
bvl_plotGelman(model1)
bvl_plotAcfs(model1,param=NULL,2,3)
bvl_plotDensity(model1,c("b_Age_WastedOpportunity","b_Income_WastedOpportunity","b_LandownerOrg_WastedOpportunity","b_ForestryOrg_WastedOpportunity","b_Sex_WastedOpportunity"))+theme_bw()
bvl_plotIntervals(model1,c("b_Age_WastedOpportunity","b_Income_WastedOpportunity","b_LandownerOrg_WastedOpportunity","b_ForestryOrg_WastedOpportunity","b_Sex_WastedOpportunity"))+theme_bw()

bvl_plotParams(model1,2,3,credMass = 0.89,params = NULL)

loo1<-bvl_stanLoo(model1)
plot(loo1)

# Load data and package
data1<-read.csv("C:/Users/ACER/Dropbox/Research/@_Team/@_Fandom/BMF collaborative projects/2023/BMF CP 17 (Pellet wood_wasted resources)/Data/Survey_data_wasted.csv",header = TRUE,stringsAsFactors = TRUE)
library(bayesvl)


data1$AlternativetoFossil<-data1$q22_viable
data1$FutureSaleforBiomass<-data1$q21_bioenergy
data1$CostsOverBenefits<-data1$q22_environcost

keeps <- c("AlternativetoFossil","CostsOverBenefits","FutureSaleforBiomass")
data1 <- data1[keeps]
data1<-na.omit(data1) 

# Model construction
model2<-bayesvl()
model2<-bvl_addNode(model2,"FutureSaleforBiomass","norm")
model2<-bvl_addNode(model2,"AlternativetoFossil","norm")
model2<-bvl_addNode(model2,"CostsOverBenefits","norm")

model2<-bvl_addArc(model2,"AlternativetoFossil","FutureSaleforBiomass","slope")
model2<-bvl_addArc(model2,"CostsOverBenefits","FutureSaleforBiomass","slope")

bvl_bnPlot(model2)

# Generate Stan code
model_string2<- bvl_model2Stan(model2)
cat(model_string2) 

# Model Fit
model2<-bvl_modelFit(model2, data1, warmup = 2000, iter = 5000, chains = 4,cores = 4) 
summary(model2)
s
bvl_plotTrace(model2)
bvl_plotGelmans(model2,param=NULL,1,3)
bvl_plotAcfs(model2,param=NULL,1,3)
bvl_plotDensity(model2,c("b_AlternativetoFossil_FutureSaleforBiomass","b_CostsOverBenefits_FutureSaleforBiomass"))+theme_bw()
bvl_plotIntervals(model2,c("b_AlternativetoFossil_FutureSaleforBiomass","b_CostsOverBenefits_FutureSaleforBiomass"))+theme_bw()

bvl_plotParams(model2,1,3,credMass = 0.89,params = NULL)

loo2<-bvl_stanLoo(model2)
plot(loo2)
