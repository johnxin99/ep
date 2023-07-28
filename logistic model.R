data=read.csv("C:\\Users\\yuche\\OneDrive\\Desktop\\epy\\afam.csv")
data_no_na=data[complete.cases(data[,c('advfib', 'afam', 'age', 'male', 't2dm', 'bmi', 'hypertens', 'triglyc')]),]
model1=glm(advfib ~ afam + age + male + t2dm + bmi + hypertens + triglyc, 
             data = data_no_na, family = binomial)
summary(model1)
afam_coef1=coef(model1)["afam"]
afam_se1=summary(model1)$coef["afam", "Std. Error"]
afam_odds_ratio1=exp(afam_coef1)
afam_lower_ci1=exp(afam_coef1 - 1.96 * afam_se1)
afam_upper_ci1=exp(afam_coef1 + 1.96 * afam_se1)


data1=subset(data, pnpla3 != "")
data1$pnpla3[data1$pnpla3 %in% c("GG", "CG")]=1
data1$pnpla3[data1$pnpla3 == "CC"]=0
data1
data1_no_na=data1[complete.cases(data1[,c('advfib', 'afam', 'age', 'male', 't2dm', 'bmi', 'hypertens', 'triglyc')]),]
model2=glm(advfib ~ afam + age + male + t2dm + bmi + hypertens + triglyc + pnpla3, 
           data = data1_no_na, family = binomial)
summary(model2)
afam_coef2=coef(model2)["afam"]
afam_se2=summary(model2)$coef["afam", "Std. Error"]
afam_odds_ratio2=exp(afam_coef2)
afam_lower_ci2=exp(afam_coef2 - 1.96 * afam_se2)
afam_upper_ci2=exp(afam_coef2 + 1.96 * afam_se2)