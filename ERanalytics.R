#Group 12 - Final Project

###----------------------------------------###

#Note: for variables used see detailed descriptions in the Headers Descriptions sheet 
# in PatientArrivalData.xlsx file. 

# Questions:
# 1. What variables affect the length of stay for patients discharged from the emergency department?
# 2. What variables affect how long a patient will wait before being seen by a provider in the emergency department?

#Setting up the data
getwd()
setwd("C:/Users/ilona_2/Downloads/data/1 group project")
#setwd("F:/Docs/Downloads/ilka/data/project")
#setwd("C:/Users/ilona/Downloads/data/group assign 1")
readdata=read.csv( "FINAL_R_PatientArrivalData.csv")
dim(readdata)
mydata=readdata[complete.cases(readdata),]
dim(mydata)
attach(mydata)
View(mydata)
head(mydata)
tail(mydata)
dim(mydata)
sapply(mydata, class)

mydata3=subset(mydata, EDDispositionNum==2)
dim(mydata3)

###----------------------------------------###
#SECTION 1
#Deciding on the model and number of variables

#Selecting a model for DoorToExamStart
library(leaps)
regfit <- regsubsets(DoorToExamStart~. - DoorToRoom -EDDispositionNum -LengthOfStay, data=mydata, nvmax=13)
summaryofreg <-summary(regfit)
#summary(regfit)


#adjr2
summaryofreg$adjr2
plot(summaryofreg$adjr2)
which.max(summaryofreg$adjr2) 
summaryofreg$adjr2
coef(regfit,9)
# adjr2: 0.08935593
      # (Intercept)                MonthNum          ArrivalTimeNum                     Age                  PCPBin        ArrivalMethodNum                  Acuity 
      # -30.1915878               2.6047705               0.5834352              -0.0442872              -0.5000675              -5.4880306               1.6555807 
      # TempF      FirstEDMidlevelNum NumberOfPatientsPresent 
      # 0.1896928              -6.4432466               0.9389346 


#cp
summaryofreg$cp
plot(summaryofreg$cp)
which.min(summaryofreg$cp)
summaryofreg$cp
coef(regfit,8)
# cp: 5.896469
      # (Intercept)                MonthNum          ArrivalTimeNum                     Age        ArrivalMethodNum                  Acuity                   TempF 
      # -30.57288666              2.61042095              0.58359458             -0.04770944             -5.46298447              1.67317973              0.19054110 
      # FirstEDMidlevelNum NumberOfPatientsPresent 
      # -6.43714118              0.93743955 

#bic
summaryofreg$bic
plot(summaryofreg$bic)
which.min(summaryofreg$bic)
summaryofreg$bic
coef(regfit,7)
# bic: -774.3058
      # (Intercept)                MonthNum          ArrivalTimeNum                     Age        ArrivalMethodNum                  Acuity      FirstEDMidlevelNum 
      # -11.83346931              2.61092181              0.58378106             -0.04839076             -5.46287394              1.66973982             -6.43822164 
      # NumberOfPatientsPresent 
      # 0.93710210 

#selecting a model for LengthOfStay
library(leaps)
regfit <- regsubsets(LengthOfStay~.- DoorToRoom -DoorToExamStart, data=mydata3, nvmax=14)
summaryofreg <-summary(regfit)
summary(regfit)

#adjr2
summaryofreg$adjr2
plot(summaryofreg$adjr2)
which.max(summaryofreg$adjr2) 
summaryofreg$adjr2
coef(regfit,10)
# adjr2: 0.2027686
        # (Intercept)                MonthNum         EDArrivalDayNum          ArrivalTimeNum 
        # 354.0545867               5.5227161              -0.7027543               0.6738476 
        # Age                  PCPBin        ArrivalMethodNum                  Acuity 
        # 0.5266104              -3.2905919              23.1521379             -69.2354043 
        # BPSystolicNum      FirstEDMidlevelNum NumberOfPatientsPresent 
        # -0.3635822             -24.4817714               1.4854488           -119.4573950 

#cp
summaryofreg$cp
plot(summaryofreg$cp)
which.min(summaryofreg$cp)
summaryofreg$cp
coef(regfit,8)
# cp: 6.042138
      # (Intercept)                MonthNum          ArrivalTimeNum                     Age 
      # 349.7391975               5.4252167               0.6833192               0.5022320 
      # ArrivalMethodNum                  Acuity           BPSystolicNum      FirstEDMidlevelNum 
      # 23.3102801             -69.1058339              -0.3584244             -24.5958815 
      # NumberOfPatientsPresent 
      # 1.4945543 


#bic
summaryofreg$bic
plot(summaryofreg$bic)
which.min(summaryofreg$bic)
summaryofreg$bic
coef(regfit,7)
# bic: -1588.651
      # (Intercept)                MonthNum                     Age        ArrivalMethodNum 
      # 360.0114525               5.2887115               0.4904691              23.4578551 
      # Acuity           BPSystolicNum      FirstEDMidlevelNum NumberOfPatientsPresent 
      # -69.6969847              -0.3554480             -23.3262097               1.8269876        -0.3567591        -23.3720830       -119.6432563 


###----------------------------------------###
#SECTION 2
#linear regression

#linear regression for DoorToExamStart 
lm1<-lm(DoorToExamStart~.-DoorToRoom -EDDispositionNum -LengthOfStay, data=mydata)
summary(lm1)
# Multiple R-squared:  0.09035,	Adjusted R-squared:  0.08903 

#improved linear regression for DoorToExamStart 
lm1<-lm(DoorToExamStart~MonthNum+ ArrivalTimeNum+Age+PCPBin+ArrivalMethodNum+Acuity+TempF+FirstEDMidlevelNum +NumberOfPatientsPresent, data=mydata)
summary(lm1)
#Multiple R-squared:  0.09027,	Adjusted R-squared:  0.08936

#linear regression for LengthOfStay
lm3<-lm(LengthOfStay~.-DoorToRoom -DoorToExamStart, data=mydata3)
summary(lm3)
#Multiple R-squared:  0.2039,	Adjusted R-squared:  0.2025 

#improved linear regression for LengthOfStay
lm4<-lm(LengthOfStay~MonthNum+Age+ArrivalMethodNum+Acuity+BPSystolicNum+Pulse+FirstEDMidlevelNum+NumberOfPatientsPresent-EDDispositionNum , data=mydata)
summary(lm4)
#Multiple R-squared:  0.2141,	Adjusted R-squared:  0.2134 



###########################
#Conclusion: linear regression model is not a good model based on low adjusted R-squared values.
###########################




###----------------------------------------###
#SECTION 3
#logistic regression


#logisitc regression for DoorToExamStart
#assign wait var: if wait is longer than median then 1 and if shorter or equal then 0.
#wait=(mydata$DoorToExamStart>15)
wait=(mydata$DoorToExamStart>median(mydata$DoorToExamStart))
median(mydata$DoorToExamStart)

View(wait)

mydata2=data.frame(mydata,wait)
View(mydata2)

waitBin=as.integer(as.logical(mydata2$wait))
mydata2=data.frame(mydata,waitBin)
View(mydata2)
attach(mydata2)

mydata2$waitBin <- factor(mydata2$waitBin)


training=(mydata2$MonthNum>8)
#training=seq(1,nrow(mydata2)/2)
length(training)
testing=!training
length(testing)


View(training)
View(testing)

trainingdata=mydata2[training,]
length(trainingdata)
testingdata=mydata2[testing,]
length(testingdata)

Waittesting=waitBin[testing]
length(Waittesting)

waitmodel=glm(waitBin~.-DoorToExamStart -DoorToRoom -EDDispositionNum -LengthOfStay, data=trainingdata, family=binomial)
summary(waitmodel)

            #Output:
                # Call:
                #   glm(formula = waitBin ~ . - DoorToExamStart - DoorToRoom - EDDispositionNum - 
                #         LengthOfStay, family = binomial, data = trainingdata)
                # 
                # Deviance Residuals: 
                #   Min       1Q   Median       3Q      Max  
                # -1.8715  -1.1424   0.7344   1.1022   1.9376  
                # 
                # Coefficients:
                #   Estimate Std. Error z value Pr(>|z|)    
                # (Intercept)             -4.3859262  2.4221207  -1.811   0.0702 .  
                # MonthNum                 0.0095544  0.0529390   0.180   0.8568    
                # EDArrivalDayNum          0.0747736  0.0131153   5.701 1.19e-08 ***
                #   ArrivalTimeNum           0.0504173  0.0050221  10.039  < 2e-16 ***
                #   Age                     -0.0026054  0.0013523  -1.927   0.0540 .  
                # PCPBin                  -0.0496883  0.0607413  -0.818   0.4133    
                # ArrivalMethodNum        -0.4925822  0.0580711  -8.482  < 2e-16 ***
                #   Acuity                   0.1147321  0.0509137   2.253   0.0242 *  
                #   BPSystolicNum           -0.0022121  0.0014874  -1.487   0.1369    
                # Pulse                   -0.0011595  0.0019045  -0.609   0.5426    
                # RespRate                -0.0001231  0.0063014  -0.020   0.9844    
                # TempF                    0.0367156  0.0240998   1.523   0.1276    
                # FirstEDMidlevelNum      -0.4490329  0.0725996  -6.185 6.21e-10 ***
                #   NumberOfPatientsPresent  0.0811411  0.0094514   8.585  < 2e-16 ***
                #   ---
                #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
                # 
                # (Dispersion parameter for binomial family taken to be 1)
                # 
                # Null deviance: 8462.0  on 6104  degrees of freedom
                # Residual deviance: 8097.3  on 6091  degrees of freedom
                # AIC: 8125.3
                # 
                # Number of Fisher Scoring iterations: 4

modelpredprobs=predict(waitmodel, testingdata, type="response")

modelpredwait=rep("1",2883)
View(modelpredwait)
modelpredwait[modelpredprobs>0.5]="0"


table(modelpredwait,Waittesting)

      #Output: 
          #             Waittesting
          # modelpredwait   0   1
          #             0 740 694
          #             1 929 520
(740/(740+929))*100 #short wait time
#[1] 44.33793
(520/(520+694))*100 #long wait time
#[1] 42.83361

((740+520)/(740+520+694+929))*100 #total
#[1] 43.70447

#Answer: This confusion table shows that when the wait is short, then the model is right 
#       approximately 44% of the time. When the wait is long, the model is right 
#       approximately 43% of the time. In total, the model is right 44% of the time. 


#improved model
waitmodel=glm(waitBin~EDArrivalDayNum +ArrivalTimeNum +ArrivalMethodNum +FirstEDMidlevelNum +NumberOfPatientsPresent, data=trainingdata, family=binomial)
summary(waitmodel)

      #Output:
        # Call:
        #   glm(formula = waitBin ~ EDArrivalDayNum + ArrivalTimeNum + ArrivalMethodNum + 
        #         FirstEDMidlevelNum + NumberOfPatientsPresent, family = binomial, 
        #       data = trainingdata)
        # 
        # Deviance Residuals: 
        #   Min       1Q   Median       3Q      Max  
        # -1.8955  -1.1444   0.7503   1.1039   1.8786  
        # 
        # Coefficients:
        #   Estimate Std. Error z value Pr(>|z|)    
        # (Intercept)             -0.827416   0.119494  -6.924 4.38e-12 ***
        #   EDArrivalDayNum          0.075963   0.013080   5.808 6.33e-09 ***
        #   ArrivalTimeNum           0.050138   0.004989  10.050  < 2e-16 ***
        #   ArrivalMethodNum        -0.530856   0.056878  -9.333  < 2e-16 ***
        #   FirstEDMidlevelNum      -0.329217   0.058516  -5.626 1.84e-08 ***
        #   NumberOfPatientsPresent  0.076882   0.009367   8.208 2.26e-16 ***
        #   ---
        #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
        # 
        # (Dispersion parameter for binomial family taken to be 1)
        # 
        # Null deviance: 8462.0  on 6104  degrees of freedom
        # Residual deviance: 8119.2  on 6099  degrees of freedom
        # AIC: 8131.2
        # 
        # Number of Fisher Scoring iterations: 4

modelpredprobs=predict(waitmodel, testingdata, type="response")

modelpredwait=rep("1",2883)
View(modelpredwait)
modelpredwait[modelpredprobs>0.5]="0"


table(modelpredwait,Waittesting)

      #Output:
        #             Waittesting
        # modelpredwait   0   1
        #             0 757 698
        #             1 912 516

(757/(757+912))*100 #short wait time
#[1] 45.3565
(516/(516+698))*100 #long wait time
#[1] 42.50412

((757+516)/(757+516+698+912))*100 #total
#[1] 44.15539

#Answer: This confusion table shows that when the wait is short, then the model is right 
#       approximately 45% of the time. When the wait is long, the model is right 
#       approximately 42% of the time. In total, the model is right 44% of the time.
#       This is not much improvement from the previous model. 



#logisitc regression for LengthOfStay

#assign stay var: if stay is longer than median then 1 and if shorter or equal then 0.
stay=(mydata3$LengthOfStay>median(mydata3$LengthOfStay))
median(mydata3$LengthOfStay)
View(stay)

mydata3=data.frame(mydata3,stay)
View(mydata3)

stayBin=as.integer(as.logical(mydata3$stay))
mydata3=data.frame(mydata3,stayBin)
View(mydata3)
attach(mydata3)

mydata3$stayBin <- factor(mydata3$stayBin)


training=(mydata3$MonthNum>8)
#training=seq(1,nrow(mydata2)/2)
length(training)
testing=!training
length(testing)


View(training)
View(testing)

trainingdata=mydata3[training,]
length(trainingdata)
testingdata=mydata3[testing,]
length(testingdata)

Staytesting=stayBin[testing]
length(Staytesting)

staymodel=glm(stayBin~.-DoorToExamStart -DoorToRoom -EDDispositionNum -LengthOfStay -stay, data=trainingdata, family=binomial)
summary(staymodel)

      #Output:
      # Call:
      #   glm(formula = stayBin ~ . - DoorToExamStart - DoorToRoom - EDDispositionNum - 
      #         LengthOfStay - stay, family = binomial, data = trainingdata)
      # 
      # Deviance Residuals: 
      #   Min       1Q   Median       3Q      Max  
      # -2.3389  -0.8245   0.3600   0.9018   2.2585  
      # 
      # Coefficients:
      #   Estimate Std. Error z value Pr(>|z|)    
      # (Intercept)              4.813928   2.422387   1.987  0.04689 *  
      #   MonthNum                 0.140112   0.064336   2.178  0.02942 *  
      #   EDArrivalDayNum          0.020629   0.015831   1.303  0.19254    
      # ArrivalTimeNum           0.012289   0.005889   2.087  0.03690 *  
      #   Age                      0.009529   0.001703   5.595 2.20e-08 ***
      #   PCPBin                  -0.089162   0.071883  -1.240  0.21484    
      # ArrivalMethodNum         0.236207   0.072059   3.278  0.00105 ** 
      #   Acuity                  -1.316964   0.069977 -18.820  < 2e-16 ***
      #   BPSystolicNum           -0.008651   0.001902  -4.548 5.42e-06 ***
      #   Pulse                   -0.001273   0.002423  -0.525  0.59930    
      # RespRate                -0.022187   0.012384  -1.792  0.07320 .  
      # TempF                   -0.011036   0.023745  -0.465  0.64208    
      # FirstEDMidlevelNum      -0.630701   0.083625  -7.542 4.63e-14 ***
      #   NumberOfPatientsPresent  0.044366   0.011263   3.939 8.19e-05 ***
      #   ---
      #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      # 
      # (Dispersion parameter for binomial family taken to be 1)
      # 
      # Null deviance: 6914.8  on 4987  degrees of freedom
      # Residual deviance: 5752.1  on 4974  degrees of freedom
      # AIC: 5780.1
      # 
      # Number of Fisher Scoring iterations: 4

modelpredprobs=predict(staymodel, testingdata, type="response")

modelpredstay=rep("1",2338)
View(modelpredstay)
modelpredstay[modelpredprobs>0.5]="0"


table(modelpredstay,Staytesting)

      #Output: 
        #               Staytesting
        # modelpredstay   0   1
        #             0 432 885
        #             1 772 249
(432/(432+772))*100 #short stay
#[1] 35.8804
(249/(249+885))*100 #long stay
#[1] 21.95767

((432+249)/(432+249+885+772))*100 #total
#[1] 29.12746

#Answer: This confusion table shows that when the stay is short, then the model is right 
#       approximately 36% of the time. When the stay is long, the model is right 
#       approximately 22% of the time. In total, the model is right 29% of the time. 


#improved model
staymodel=glm(stayBin~Age +Acuity +BPSystolicNum +FirstEDMidlevelNum +NumberOfPatientsPresent, data=trainingdata, family=binomial)
summary(staymodel)


    #Output:
        # Call:
        #   glm(formula = stayBin ~ Age + Acuity + BPSystolicNum + FirstEDMidlevelNum + 
        #         NumberOfPatientsPresent, family = binomial, data = trainingdata)
        # 
        # Deviance Residuals: 
        #   Min       1Q   Median       3Q      Max  
        # -2.2854  -0.8319   0.3724   0.9133   2.3108  
        # 
        # Coefficients:
        #   Estimate Std. Error z value Pr(>|z|)    
        # (Intercept)              5.031834   0.323775  15.541  < 2e-16 ***
        #   Age                      0.009721   0.001557   6.245 4.24e-10 ***
        #   Acuity                  -1.322806   0.069132 -19.135  < 2e-16 ***
        #   BPSystolicNum           -0.008828   0.001888  -4.676 2.93e-06 ***
        #   FirstEDMidlevelNum      -0.630364   0.082672  -7.625 2.44e-14 ***
        #   NumberOfPatientsPresent  0.048133   0.010872   4.427 9.55e-06 ***
        #   ---
        #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
        # 
        # (Dispersion parameter for binomial family taken to be 1)
        # 
        # Null deviance: 6914.8  on 4987  degrees of freedom
        # Residual deviance: 5779.5  on 4982  degrees of freedom
        # AIC: 5791.5
        # 
        # Number of Fisher Scoring iterations: 4


modelpredprobs=predict(staymodel, testingdata, type="response")

modelpredstay=rep("1",2338)
View(modelpredstay)
modelpredstay[modelpredprobs>0.5]="0"


table(modelpredstay,Staytesting)

      #Output:
          #               Staytesting
          # modelpredstay   0   1
          #             0 469 928
          #             1 735 206

(469/(469+735))*100 #short stay
#[1] 38.95349
(206/(206+928))*100 #long stay
#[1] 18.16578

((469+206)/(469+206+928+735))*100 #total
#[1] 28.87083

#Answer: This confusion table shows that when the stay is short, then the model is right 
#       approximately 39% of the time. When the stay is long, the model is right 
#       approximately 18% of the time. In total, the model is right 29% of the time.
#       This is not much improvement from the previous model.


###----------------------------------------###
#SECTION 4
#plots

#plots we found meaningful
plot(mydata$ArrivalTimeNum, mydata$DoorToExamStart)
plot(mydata$ArrivalTimeNum, mydata$NumberOfPatientsPresent)



###----------------------------------------###
#SECTION 5
#the tree model

#tree for LengthOfStay
library(tree)
library(ISLR)
attach(mydata3)
#View(mydata3)
High=ifelse(LengthOfStay<median(LengthOfStay), "No", "Yes")
median(LengthOfStay)
Stay= data.frame(mydata3, High)
treeStay = tree(High~.-DoorToExamStart -DoorToRoom -EDDispositionNum -LengthOfStay -stay -stayBin, Stay)

summary(treeStay)
plot(treeStay)
text(treeStay,pretty=0)
treeStay

      #Output: 
          # 1) root 7326 10160 Yes ( 0.4989 0.5011 )  
          # 2) Acuity < 3.5 4357  5427 Yes ( 0.3147 0.6853 ) *
          # 3) Acuity > 3.5 2969  3207 No ( 0.7693 0.2307 ) *

#tree for DoorToExamStart
library(tree)
library(ISLR)
attach(mydata)
#View(mydata)
High=ifelse(DoorToExamStart<median(DoorToExamStart), "No", "Yes")
median(DoorToExamStart)
LongerWait= data.frame(mydata, High)
treeLongWait = tree(High~.-DoorToExamStart -DoorToRoom -EDDispositionNum -LengthOfStay, LongerWait)

summary(treeLongWait)
plot(treeLongWait)
text(treeLongWait,pretty=0)
treeLongWait

