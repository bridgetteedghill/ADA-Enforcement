## ADA Enforcement Across Presidential Administrations

## DV is RslvTot, ChrgTot, MonTot, MonTotMill, AvgMon. (How many resolutions were 
## filed, how many were in favor of the charging party, how much was monetarily 
## paid out to the charging party, an easier way to read that variable, and
## how much individuals received in monetary payout on average.)
## IV is presidential administration/party of.

##Descriptive statistics for DV
summary(ADATot01$RslvTot)
summary(ADATot01$ChrgTot)
summary(ADATot01$MonMillTot)
summary(ADATot01$AvgMon)

plot(ADATot01$Year, ADATot01$RslvTot, xlab = "Year Resolution was Filed", 
     ylab = "Amount of Resolutions Filed", 
     main = "Amount of ADA Resolutions Filed")
coefplot(RslvTotAdminDum, 
         title="Amount of ADA Resolutions Filed by President",
         innerCI=2, outerCI=0, intercept=FALSE)

plot(ADATot01$Year, ADATot01$ChrgTot, xlab = "Year Merit Resolution was Filed", 
     ylab = "Amount of Merit Resolutions Filed", 
     main = "Amount of ADA Merit Resolutions Filed")
coefplot(ChrgTotAdminDum, 
         title="Amount of ADA Merit Resolutions Filed by President",
         innerCI=2, outerCI=0, intercept=FALSE)

plot(ADATot01$Year, ADATot01$MonTotMill, xlab = "Year Merit Resolution was Filed", 
     ylab = "Monetary Amount Awarded (in Millions)", 
     main = "Monetary Amount Awarded for Merit Resolutions Filed")
coefplot(MonTotMillAdminDum, 
         title="Monetary Amount Awarded for Merit Resolutions Filed by President",
         innerCI=2, outerCI=0, intercept=FALSE)

plot(ADATot01$Year, ADATot01$AvgMon, xlab = "Year Merit Resolution was Filed",
     ylab = "Average Monetary Amount Paid Out",
     main = "Average Monetary Amount Awarded for Merit Resolutions Filed")
coefplot(AvgMonTotAdminDum, 
         title="Average Monetary Amount Awarded by President",
         innerCI=2, outerCI=0, intercept=FALSE)

plot(ADATot01$Year, ADATot01$NumRecp, xlab = "Year Merit Resolution was Filed", 
     ylab = "Number of Monetary Recipients",
     main = "Number of Receipents for Monetary Settlements")
        ## One year to call out here is 2015. This would have been near the end
        ## of Obama's time in office. There's several more recipients of monetary
        ## settlements, though they all made a relatively low amount. Kind of like
        ## spreading the weath and holding more accountable. There's several reasons
        ## why this could have been.
        ## It's also strange because there was a similar push in 2002, which would
        ## have been the middle of WBush's first term. It's less exaggerated than
        ## Obama's (less people received and they got smaller amounts.) But, it
        ## begs the question what was going on for each. Similar motivations or
        ## not? 

## Standard deviation for the DV
sd(ADATot01$RslvTot, na.rm = TRUE)
sd(ADATot01$ChrgTot, na.rm = TRUE)
sd(ADATot01$MonTot, na.rm = TRUE)
sd(ADATot01$MonTotMill, na.rm = TRUE)
sd(ADATot01$AvgMon, na.rm = TRUE)

## Dummy out Admins
ADATot01$Clinton <- recode(ADATot01$Admin, "1=1; 2:4=0")
table(ADATot01$Clinton)
ADATot01$WBush <- recode(ADATot01$Admin, "2=1; 1:4=0")
table(ADATot01$WBush)
ADATot01$Obama <- recode(ADATot01$Admin, "3=1; 1:4=0")
table(ADATot01$Obama)
ADATot01$Trump <- recode(ADATot01$Admin, "4=1; 1:4=0")
table(ADATot01$Trump)

## Compute correlation between DV, IV
cor(ADATot01$RslvTot,ADATot01$Clinton, use = "complete.obs")
## Weak relationship at -.003.
cor(ADATot01$RslvTot,ADATot01$WBush, use = "complete.obs")
## Example of a strong relationship at -.825.
cor(ADATot01$RslvTot,ADATot01$Obama, use = "complete.obs")
cor(ADATot01$RslvTot,ADATot01$Trump, use = "complete.obs")
cor(ADATot01$RslvTot,ADATot01$Party, use = "complete.obs")

cor(ADATot01$ChrgTot,ADATot01$Clinton, use = "complete.obs")
cor(ADATot01$ChrgTot,ADATot01$WBush, use = "complete.obs")
cor(ADATot01$ChrgTot,ADATot01$Obama, use = "complete.obs")
cor(ADATot01$ChrgTot,ADATot01$Trump, use = "complete.obs")
cor(ADATot01$ChrgTot,ADATot01$Party, use = "complete.obs")

cor(ADATot01$MonTotMill,ADATot01$Clinton, use = "complete.obs")
cor(ADATot01$MonTotMill,ADATot01$WBush, use = "complete.obs")
cor(ADATot01$MonTotMill,ADATot01$Obama, use = "complete.obs")
cor(ADATot01$MonTotMill,ADATot01$Trump, use = "complete.obs")
cor(ADATot01$MonTotMill,ADATot01$Party, use = "complete.obs")

cor(ADATot01$AvgMon,ADATot01$Clinton, use = "complete.obs")
cor(ADATot01$AvgMon,ADATot01$WBush, use = "complete.obs")
cor(ADATot01$AvgMon,ADATot01$Obama, use = "complete.obs")
cor(ADATot01$AvgMon,ADATot01$Trump, use = "complete.obs")
cor(ADATot01$AvgMon,ADATot01$Party, use = "complete.obs")


qt(c(.05, .95), df=21) ## -1.72 / 1.72 are our critical values.
## Difference of means test: Impact of Party on Resolutions, Merit Resolutions Filed,
## Settlement paid out, Avg amount individuals paid out.
t.test(RslvTot~Party, data = ADATot01)
        ## Only this one (above) has a t value higher than the critical value.
t.test(ChrgTot~Party, data = ADATot01)
t.test(MonTotMill~Party, data = ADATot01)
t.test(AvgMon~Party, data = ADATot01)

## Regular regression run
PartyRslvReg <- lm(RslvTot ~ Party, data = ADATot01)
summary(PartyRslvReg)
        ## Poor fit on R squared. Statistically significant p value .02.
PartyChrgReg <- lm(ChrgTot ~ Party, data = ADATot01)
summary(PartyChrgReg)
        ## Again, poor fit on R squared. This is not statistically significant.
PartyMonReg <- lm(MonTot ~ Party, data = ADATot01)
summary(PartyMonReg)
PartyMonMillReg <- lm(MonTotMill ~ Party, data = ADATot01)
summary(PartyMonMillReg)
        ## For ease of use, Mon Mill will be used to interpret both Mon categories.
        ## A negative R squared which only has a .003 fit. P value is not statistically
        ## significant.
PartyAvgMonReg <- lm(AvgMon ~ Party, data = ADATot01)
summary(PartyAvgMonReg)
        ## And again, not a great fit on the R squared (and it's negative.) The p value
        ## of .96 is not statistically significant.

## Based on the dry run, for party the amount of res filed is significant, 
## but the ones that go through and amount of money they're awarded are not.
        ## (DV ~ IV)

ClintonRslvReg <- lm(RslvTot ~ Clinton, data = ADATot01)
summary(ClintonRslvReg)
        ## Not statistically significant. High standard error of 2929.35?
ClintonChrgReg <- lm(ChrgTot ~ Clinton, data = ADATot01)
summary(ClintonChrgReg)
        ## Statistically significant at the .1 level at .07. SE smaller at 553.7.
ClintonMonReg <- lm(MonTot ~ Clinton, data = ADATot01)
summary(ClintonMonReg)
ClintonMonMillReg <- lm(MonTotMill ~ Clinton, data = ADATot01)
summary(ClintonMonMillReg)
        ## Statistically significant at the .1 level at .08. SE 17.90.
ClintonAvgMonReg <- lm(AvgMon ~ Clinton, data = ADATot01)
summary(ClintonAvgMonReg)
        ## Not statistically significant. SE high at 1991.8.

WBushRslvReg <- lm(RslvTot ~ WBush, data = ADATot01)
summary(WBushRslvReg)
        ## This is highly statistically significant at 1.25e-06. A better fit than
        ## Clinton's metrics at .66 R squared and an SE of 1316.1.
WBushChrgReg <- lm(ChrgTot ~ WBush, data = ADATot01)
summary(WBushChrgReg)
        ## Not quite as strong, but still statistically significant at .007. Not as good
        ## of a fit at .26. SE of 399.
WBushMonReg <- lm(MonTot ~ WBush, data = ADATot01)
summary(WBushMonReg)
WBushMonMillReg <- lm(MonTotMill ~ WBush, data = ADATot01)
summary(WBushMonMillReg)
        ## And again, a statistically significant finding of .001. A slightly better fit
        ## at .36 and SE of 11.9.
WBushAvgMonReg <- lm(AvgMon ~ WBush, data = ADATot01)
summary(WBushAvgMonReg)
        ## Here's where the statistical significance stops with .282. Not a great fit at
        ## R squared of .009. SE of 1636.1.

ObamaRslvReg <- lm(RslvTot ~ Obama, data = ADATot01)
summary(ObamaRslvReg)
        ## Statistical significance at .018. Not a fabulous fit at .20 R squared. 
        ## Standard error seems inflated at 2036.
ObamaChrgReg <- lm(ChrgTot ~ Obama, data = ADATot01)
summary(ObamaChrgReg)
        ## Statistical significance of .004. Fit is at .289 R squared. SE of 392.
ObamaMonReg <- lm(MonTot ~ Obama, data = ADATot01)
summary(ObamaMonReg)
ObamaMonMillReg <- lm(MonTotMill ~ Obama, data = ADATot01)
summary(ObamaMonMillReg)
        ## Statistical significance at .012. Fit is holding steady at .226. Again,
        ## inflated SE at 13.19 (compared to coefficient of 35.96?)
ObamaAvgMonReg <- lm(AvgMon ~ Obama, data = ADATot01)
summary(ObamaAvgMonReg)
        ## And statistical significance again stops at .238 - just a bit off from WBush's
        ## .282. Fit is again in the 20% range and a very high SE (1,626.7 to coefficient
        ## 1975.6.)

TrumpRslvReg <- lm(RslvTot ~ Trump, data = ADATot01)
summary(TrumpRslvReg)
        ## Statistically significant at .0197. A very poor fit at .196. And inflated SE
        ## at 2,888 to 7,288.
TrumpChrgReg <- lm(ChrgTot ~ Trump, data = ADATot01)
summary(TrumpChrgReg)
        ## Meets the .1 level of statistical significance at .062. Again, poor fit at
        ## .11 and a SE of 618.5.
TrumpMonReg <- lm(MonTot ~ Trump, data = ADATot01)
summary(TrumpMonReg)
TrumpMonMillReg <- lm(MonTotMill ~ Trump, data = ADATot01)
summary(TrumpMonMillReg)
        ## Certainly statistically significant at .003. A slightly better fit at .314.
        ## SE slightly high at 17.56.
TrumpAvgMonReg <- lm(AvgMon ~ Trump, data = ADATot01)
summary(TrumpAvgMonReg)
        ## Interestingly, no statistical significance again at .105. Poor fit at .07 and
        ## very high SE of 2,232.3 of coefficient 3,782.

## Putting all the admins together:
RslvAdminDum <- lm(RslvTot ~ Party + Clinton + WBush + Obama, data = ADATot01)
summary(RslvAdminDum)
ChrgAdminDum <- lm(ChrgTot ~ Party + Clinton + WBush + Obama, data = ADATot01)
summary(ChrgAdminDum)
MonMillAdminDum <- lm(MonTotMill ~ Party + Clinton + WBush + Obama, data = ADATot01)
summary(MonMillAdminDum)
AvgMonAdminDum <- lm(AvgMon ~ Party + Clinton + WBush + Obama, data = ADATot01)
summary(AvgMonAdminDum)
        ## Trump is reference category.

## Estimate regression model adding admin as additional IV.
RslvTotAdminDum <- lm(RslvTot ~ Clinton+WBush+Obama, data = ADATot01)
summary(RslvTotAdminDum)
        ## The coefficient for each admin compare mean resolution rate vs. Trump
        ## e.g., resolution rate in Clinton (number 1) is about 6371 lower than 
        ## resolution rate for Trump admin.
ChrgTotAdminDum <- lm(ChrgTot ~ Clinton+WBush+Obama, data = ADATot01)
summary(ChrgTotAdminDum)
        ## The coefficient for each admin compare mean merit resolution rate vs. Trump
        ## e.g., merit resolution rate in Clinton (number 1) is about 1917.7 lower than 
        ## merit resolution rate for Trump admin.
MonTotMillAdminDum <- lm(MonTotMill ~ Clinton+WBush+Obama, data = ADATot01)
summary(MonTotMillAdminDum)
        ## The coefficient for each admin compare the mean monetary payout rate vs. Trump
        ## e.g., monetary rate in Clinton (number 1) is about $77.9 million lower than 
        ## monetary rate for Trump admin.
AvgMonTotAdminDum <- lm(AvgMon ~ Clinton+WBush+Obama, data = ADATot01)
summary(AvgMonTotAdminDum)
        ## The coefficient for each admin compare average mean monetary payout rate vs. 
        ## Trump e.g., average mean monetary payout rate in Clinton (number 1) is 
        ## about $5977.00 lower than  average mean monetary payout rate for Trump 
        ## admin.

## The next line tests whether Clinton and WBush are statistically different on
## RslvTot DV.
linearHypothesis(RslvTotAdminDum, "Clinton=WBush")
## WBush and Obama difference RslvTot.
linearHypothesis(RslvTotAdminDum, "WBush=Obama")
## Clinton and Obama difference RslvTot.
linearHypothesis(RslvTotAdminDum, "Clinton=Obama")
## The next line tests whether Clinton and WBush are statistically different on
## ChrgTot DV.
linearHypothesis(ChrgTotAdminDum, "Clinton=WBush")
## WBush and Obama difference ChrgTot.
linearHypothesis(ChrgTotAdminDum, "WBush=Obama")
## Clinton and Obama difference ChrgTot.
linearHypothesis(ChrgTotAdminDum, "Clinton=Obama")
## The next line tests whether Clinton and WBush are statistically different on
## MonTotMill DV.
linearHypothesis(MonTotMillAdminDum, "Clinton=WBush")
## WBush and Obama difference MonTotMill.
linearHypothesis(MonTotMillAdminDum, "WBush=Obama")
## Clinton and Obama difference MonTotMill.
linearHypothesis(MonTotMillAdminDum, "Clinton=Obama")
## The next line tests whether Clinton and WBush are statistically different on
## AvgMonTotMill DV.
linearHypothesis(AvgMonAdminDum, "Clinton=WBush")
## WBush and Obama difference AvgMon.
linearHypothesis(AvgMonAdminDum, "WBush=Obama")
## Clinton and Obama difference AvgMon
linearHypothesis(AvgMonAdminDum, "Clinton=Obama")
        ## Small difference on the last one which I didn't expect, seeing as there
        ## there was no difference between the others.


svycontrast(ChrgAdminDum, c(0, 0, 1, -1, 0))
## The difference between the coefficients for WBush and Obama is 172.
## Not statistically significant. (.28, less than 2.)
svycontrast(RslvAdminDum, c(0, 1, -1, 0, 0))
## The difference between the coefficients for Clinton and WBush is 6371.2.
## The contrast is larger than SE so the difference between coefficients 
## is statistically significant (3.34, more than 2.)
svycontrast(RslvAdminDum, c(0, 0, 1, -1, 0))
## The difference between the coefficients for WBush and Obama is 8662.9.
## The contrast is larger than SE so the difference between coefficients 
## is statistically significant (3.80, more than 2.)
svycontrast(MonMillAdminDum, c(0, 1, -1, 0, 0))
## The difference between the coefficients for Clinton and WBush is 77.94.
## The contrast is much larger than SE so the difference between coefficients 
## is statistically significant (7.02, more than 2.)
svycontrast(MonMillAdminDum, c(0, 0, 1, -1, 0))
## The difference between the coefficients for WBush and Obama is 29.17.
## The contrast is just larger than SE so the difference between coefficients 
## is statistically significant (2.19, more than 2.)
svycontrast(AvgMonAdminDum, c(0, 1, -1, 0, 0))
## The difference between the coefficients for Clinton and WBush is 5977.4.
## The contrast is just larger than SE so the difference between coefficients 
## is statistically significant (2.27, more than 2.)
svycontrast(AvgMonAdminDum, c(0, 0, 1, -1, 0))
## The difference between the coefficients for WBush and Obama is 488.88.
## Not statistically significant. (.15, less than 2.)

 
## Multiple regression controlling for GDP (GDPA), Difference in Budget allocated by
## legislature compared to what Pres asked for (DiffBudget), Difference in Staffing
## allocated by legislature compared to what the Pres filled (DiffStaffing).
RslvTotMult <- lm(RslvTot ~ Clinton + WBush + Obama + GDPA + DiffBudget + DiffStaff, 
                      data = ADATot01)
summary(RslvTotMult)
        ## And look at that, WBush and Intercept (Trump) are the only two that are
        ## statistically significant out of the IVs. But, WBush wasn't filing
        ## Resolutions, while Trump was filing them all over the place. Weird...
RslvTotPartyMult <- lm(RslvTot ~ Party + GDPA + DiffBudget + DiffStaff, 
                         data = ADATot01)
summary(RslvTotPartyMult)
        ## GDP is obviously highly statistically significant, which makes sense.
        ## Party is significant .001 level though, which isn't nothing. Less of
        ## a good fit than the admins broken out, which tracks.

ChrgTotMult <- lm(ChrgTot ~ Clinton + WBush + Obama + GDPA + DiffBudget + DiffStaff,
                      data = ADATot01)
summary(ChrgTotMult)
        ## Interestingly, Clinton, WBush, and Trump are statistically significant
        ## with their Merit Resolutions. But, it could be argued that Clinton admin
        ## wasn't sure how to enforce because it was so new. A calibration test
        ## of sorts? Again, Trump overenforced?
ChrgTotPartyMult <- lm(ChrgTot ~ Party + GDPA + DiffBudget + DiffStaff,
                  data = ADATot01)
summary(ChrgTotPartyMult)
        ## GDP again is highly statistically significant. Nothing else is... Not
        ## looking great for the hypothesis home team.

MonMillTotMult <- lm(MonTotMill ~ Clinton + WBush + Obama + GDPA + DiffBudget + DiffStaff, 
                      data = ADATot01)
summary(MonMillTotMult)
        ## WBush underpaid. GDP did play a role in this, which was expected.
MonMillPartyTotMult <- lm(MonTotMill ~ Party + GDPA + DiffBudget + DiffStaff, 
                     data = ADATot01)
summary(MonMillPartyTotMult)
        ## Okay, back to statistical significance for Party. GDPA is again, highly
        ## highly significant and so too is the intercept. Odd.

AvgMonTotMult <- lm(AvgMon ~ Clinton + WBush + Obama + GDPA + DiffBudget + 
                            DiffStaff, data = ADATot01)
summary(AvgMonTotMult)
        ## And none of the admins was statistically significant in how much they
        ## paid out on average. Again, weird.
AvgMonPartyTotMult <- lm(AvgMon ~ Party + GDPA+ DiffBudget + DiffStaff, 
                         data = ADATot01)
summary(AvgMonPartyTotMult)
        ## The intercept is statistically significant at the .001 level? Otherwise,
        ## it's a little bit of GDPA and nothing else (aka Party.)

## Improved regression (heteroskedasticity)
ImprRslvTot <- lm(RslvTot ~ Clinton + WBush + Obama, data = ADATot01, 
                    na.action = na.exclude)
summary(ImprRslvTot)
bptest(ImprRslvTot, studentize = FALSE)
        ## Not statistically significant at the .05 level. (No heteroskedasticity.)

ImprChrgTot <- lm(ChrgTot ~ Clinton + WBush + Obama, data = ADATot01, 
                  na.action = na.exclude)
summary(ImprChrgTot)
bptest(ImprChrgTot, studentize = FALSE)
        ## Not statistically significant at the .05 level. (No heteroskedasticity.)

ImprMonTot <- lm(MonTot ~ Clinton + WBush + Obama, data = ADATot01, 
                 na.action = na.exclude)
summary(ImprMonTot)
ImprMonMillTot <- lm(MonTotMill ~ Clinton + WBush + Obama, data = ADATot01, 
                    na.action = na.exclude)
summary(ImprMonMillTot)
bptest(ImprMonMillTot, studentize = FALSE)
        ## This does have heteroskedasticity, as it's .003.
ImprMonMillTotGDP <- lm(MonTotMill ~ Clinton + WBush + Obama + GDPA, data = ADATot01, 
                     na.action = na.exclude)
summary(ImprMonMillTotGDP)
bptest(ImprMonMillTotGDP, studentize = FALSE)
        ## And this fixes the issue.

ImprAvgMon <- lm(AvgMon ~ Clinton + WBush + Obama, data = ADATot01, 
                     na.action = na.exclude)
summary(ImprAvgMon)
bptest(ImprAvgMon, studentize = FALSE)
        ## Not statistically significant at the .05 level. (No heteroskedasticity.)

## Calculate confidence intervals for the regression parameters
confint(MultRslvTotAdmin, level = 0.90)
confint(MultRslvTotParty, level = 0.90)
confint(MultChrgTotAdmin, level = 0.90)
confint(MultChrgTotParty, level = 0.90)
confint(MultMonTotAdmin, level = 0.90)
confint(MultMonTotParty, level = 0.90)
confint(MultMonTotMillAdmin, level = 0.90)
confint(MultMonTotMillParty, level = 0.90)
confint(MultAvgMonAdmin, level = 0.90)
confint(MultAvgMonParty, level = 0.90)

## Checking for multicollinearity in variables
vif(RslvTotMult)
vif(ChrgTotMult)
vif(MonMillTotMult)
vif(AvgMonTotMult)
        ## So. These VIFs are ridiculous. All the admins and the GDPA are off the charts.
        ## Most are above 10. But, DiffBudget/DiffStaff are both okay though. Under 2.
        ## What about parties? Would that control this before the other tests?
vif(RslvTotPartyMult)
        ## Well. That fixed it. According to Dr. Kimball, we can go ahead and use
        ## the administration breakout, but have to disclose the high multicollinearity.
vif(ChrgTotPartyMult)
vif(MonMillPartyTotMult)
vif(AvgMonPartyTotMult)

## Checking autocorrelation of variables
dwtest(RslvTotMult)
        ## D score of 1.67, so right around 2 which is what we want. It is
        ## technically statistically significant at .044, but only just.
cochrane.orcutt(RslvTotMult)
        summary(cochrane.orcutt(RslvTotMult))
        dwtest(cochrane.orcutt(RslvTotMult))
        ## Yep. That worked.
dwtest(ChrgTotMult)
        ## D score of 1.34, not quite as good as the last DV. It is
        ## certainly statistically significant at .04057, which isn't great.
cochrane.orcutt(ChrgTotMult)
        summary(cochrane.orcutt(ChrgTotMult))
        dwtest(cochrane.orcutt(ChrgTotMult))
        ## Yep. That worked.
dwtest(MonMillTotMult)
        ## D score of 1.47, not too bad. It is statistically significant at 
        ## .013, which isn't great but not as egregious as ChrgTot.
cochrane.orcutt(MonMillTotMult)
        summary(cochrane.orcutt(MonMillTotMult))
        dwtest(cochrane.orcutt(MonMillTotMult))
dwtest(AvgMonTotMult)
        ## D score of 2.002 which is phenomenal because it's bang on. And it's 
        ## not statistically significant at .204.
dwtest(RslvTotPartyMult)
dwtest(ChrgTotPartyMult)
dwtest(MonMillPartyTotMult)
dwtest(AvgMonPartyTotMult)
        ## Well, these scores aren't quite as good as for the admin breakouts
        ## listed above. Go figure. But, at least those don't have too much 
        ## trouble with autocorrelation if nothing else.
