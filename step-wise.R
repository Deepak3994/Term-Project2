
Dataset <- read.table("/home/deepak/Documents/projects/Term-Project2/properdata.csv", header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)
GLM.1 <- glm(Class ~ Age + Albumin + Anemia + Appetite + Bacteria + Blood.Glucose.Random + 
  Blood.Pressure + Blood.Urea + Coronary.Artery.Disease + Diabetes.Mellitus + Hemoglobin + 
  Hypertension + Packed.Cell.Volume + Pedal.Edema + Pus.Cell + Pus.Cell.clumps + 
  Red.Blood.Cell.Count + Red.Blood.Cells + Serum.Creatinine + Sodium + Specific.Gravity + Sugar + 
  White.Blood.Cell.Count, family=binomial(logit), data=Dataset)
summary(GLM.1)
exp(coef(GLM.1))  # Exponentiated coefficients ("odds ratios")
stepwise(GLM.1, direction='forward', criterion='BIC')
GLM.2 <- glm(Class ~ Albumin + Appetite + Diabetes.Mellitus + Hemoglobin + 
  Serum.Creatinine + Specific.Gravity, family=binomial(logit), data=Dataset)
summary(GLM.2)
exp(coef(GLM.2))  # Exponentiated coefficients ("odds ratios")
vif(GLM.2)
GLM.3 <- glm(Class ~ Albumin + Appetite + Diabetes.Mellitus +  Serum.Creatinine + 
  Specific.Gravity, family=binomial(logit), data=Dataset)
summary(GLM.3)
exp(coef(GLM.3))  # Exponentiated coefficients ("odds ratios")
vif(GLM.3)
GLM.4 <- glm(Class ~ Albumin + Appetite +  Serum.Creatinine + Specific.Gravity, 
  family=binomial(logit), data=Dataset)
summary(GLM.4)
exp(coef(GLM.4))  # Exponentiated coefficients ("odds ratios")
GLM.5 <- glm(Class ~ Albumin + Serum.Creatinine + Specific.Gravity, 
  family=binomial(logit), data=Dataset)
summary(GLM.5)
exp(coef(GLM.5))  # Exponentiated coefficients ("odds ratios")

