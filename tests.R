Dataset <- read.table("/home/deepak/Documents/projects/Term-Project2/properdata.csv", header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)
local({
  .Table <- xtabs(~Anemia+Class, data=Dataset)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~Appetite+Class, data=Dataset)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~Bacteria+Class, data=Dataset)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~Coronary.Artery.Disease+Class, data=Dataset)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~Diabetes.Mellitus+Class, data=Dataset)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~Hypertension+Class, data=Dataset)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~Pedal.Edema+Class, data=Dataset)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~Pus.Cell+Class, data=Dataset)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~Pus.Cell.clumps+Class, data=Dataset)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~Red.Blood.Cells+Class, data=Dataset)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
numSummary(Dataset[,"Age"], statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,.5,
  .75,1))
numSummary(Dataset[,"Albumin"], statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,
  .25,.5,.75,1))
numSummary(Dataset[,"Blood.Glucose.Random"], statistics=c("mean", "sd", "IQR", "quantiles"), 
  quantiles=c(0,.25,.5,.75,1))
numSummary(Dataset[,"Blood.Pressure"], statistics=c("mean", "sd", "IQR", "quantiles"), 
  quantiles=c(0,.25,.5,.75,1))
numSummary(Dataset[,"Blood.Urea"], statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,
  .25,.5,.75,1))
numSummary(Dataset[,"Hemoglobin"], statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,
  .25,.5,.75,1))
numSummary(Dataset[,"Packed.Cell.Volume"], statistics=c("mean", "sd", "IQR", "quantiles"), 
  quantiles=c(0,.25,.5,.75,1))
numSummary(Dataset[,"Potassium"], statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,
  .25,.5,.75,1))
numSummary(Dataset[,"Red.Blood.Cell.Count"], statistics=c("mean", "sd", "IQR", "quantiles"), 
  quantiles=c(0,.25,.5,.75,1))
numSummary(Dataset[,"Serum.Creatinine"], statistics=c("mean", "sd", "IQR", "quantiles"), 
  quantiles=c(0,.25,.5,.75,1))
numSummary(Dataset[,"Sodium"], statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,
  .5,.75,1))
numSummary(Dataset[,"Specific.Gravity"], statistics=c("mean", "sd", "IQR", "quantiles"), 
  quantiles=c(0,.25,.5,.75,1))
numSummary(Dataset[,"Sugar"], statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,
  .5,.75,1))
numSummary(Dataset[,"White.Blood.Cell.Count"], statistics=c("mean", "sd", "IQR", "quantiles"), 
  quantiles=c(0,.25,.5,.75,1))
with(Dataset, tapply(Age, Class, var, na.rm=TRUE))
leveneTest(Age ~ Class, data=Dataset, center="mean")
t.test(Age~Class, alternative='two.sided', conf.level=.95, var.equal=TRUE, data=Dataset)
with(Dataset, tapply(Blood.Pressure, Class, var, na.rm=TRUE))
leveneTest(Blood.Pressure ~ Class, data=Dataset, center="mean")
t.test(Blood.Pressure~Class, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=Dataset)
with(Dataset, tapply(Specific.Gravity, Class, var, na.rm=TRUE))
leveneTest(Specific.Gravity ~ Class, data=Dataset, center="mean")
t.test(Specific.Gravity~Class, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=Dataset)
with(Dataset, tapply(Blood.Glucose.Random, Class, var, na.rm=TRUE))
leveneTest(Blood.Glucose.Random ~ Class, data=Dataset, center="mean")
t.test(Blood.Glucose.Random~Class, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=Dataset)
with(Dataset, tapply(Sodium, Class, var, na.rm=TRUE))
leveneTest(Sodium ~ Class, data=Dataset, center="mean")
t.test(Sodium~Class, alternative='two.sided', conf.level=.95, var.equal=TRUE, data=Dataset)
with(Dataset, tapply(Hemoglobin, Class, var, na.rm=TRUE))
leveneTest(Hemoglobin ~ Class, data=Dataset, center="mean")
t.test(Hemoglobin~Class, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=Dataset)
with(Dataset, tapply(Packed.Cell.Volume, Class, var, na.rm=TRUE))
leveneTest(Packed.Cell.Volume ~ Class, data=Dataset, center="mean")
t.test(Packed.Cell.Volume~Class, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=Dataset)
with(Dataset, tapply(White.Blood.Cell.Count, Class, var, na.rm=TRUE))
leveneTest(White.Blood.Cell.Count ~ Class, data=Dataset, center="mean")
t.test(White.Blood.Cell.Count~Class, alternative='two.sided', conf.level=.95, var.equal=TRUE, data=Dataset)
with(Dataset, tapply(Red.Blood.Cell.Count, Class, var, na.rm=TRUE))
leveneTest(Red.Blood.Cell.Count ~ Class, data=Dataset, center="mean")
t.test(Red.Blood.Cell.Count~Class, alternative='two.sided', conf.level=.95, var.equal=TRUE, data=Dataset)
with(Dataset, tapply(Albumin, Class, median, na.rm=TRUE))
wilcox.test(Albumin ~ Class, alternative="two.sided", data=Dataset)
with(Dataset, tapply(Blood.Urea, Class, median, na.rm=TRUE))
wilcox.test(Blood.Urea ~ Class, alternative="two.sided", data=Dataset)
with(Dataset, tapply(Serum.Creatinine, Class, median, na.rm=TRUE))
wilcox.test(Serum.Creatinine ~ Class, alternative="two.sided", data=Dataset)
with(Dataset, tapply(Potassium, Class, median, na.rm=TRUE))
wilcox.test(Potassium ~ Class, alternative="two.sided", data=Dataset)
with(Dataset, tapply(Sugar, Class, median, na.rm=TRUE))
wilcox.test(Sugar ~ Class, alternative="two.sided", data=Dataset)

