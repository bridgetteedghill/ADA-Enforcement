## ADA Enforcement for Specific Disabilities

## Visualization library
library(coefplot)
library(ggplot2)
library(texreg)

## Dummy out Admins
ADACat01$ClintonCat <- car::recode(ADACat01$Admin, "1=1; 2:4=0")
table(ADACat01$ClintonCat)
ADACat01$WBushCat <- car::recode(ADACat01$Admin, "2=1; 1:4=0")
table(ADACat01$WBushCat)
ADACat01$ObamaCat <- car::recode(ADACat01$Admin, "3=1; 1:4=0")
table(ADACat01$ObamaCat)
ADACat01$TrumpCat <- car::recode(ADACat01$Admin, "4=1; 1:4=0")
table(ADACat01$TrumpCat)

## Anxiety and Depression Comparison (Visualization)
ggplot() +
  geom_line(data=ADACat01, aes(y=AnxRslv, x=Year, color="darkred"), size = 1.1) +
  geom_line(data=ADACat01, aes(y=DeprRslv, x=Year, color="steelblue"), size = 1.1) +
  xlab("Year") +
  ylab("Number of Resolutions Filed") +
  ggtitle("Anxiety and Depression-Related Resolutions Filed") +
  scale_color_discrete(name = "Disability Type", labels = c("Anxiety", 
                                                            "Depression"))
ggplot() + 
  geom_line(data=ADACat01, aes(y = AnxChrg, x=Year, color = "darkred"), size = 1.1) + 
  geom_line(data=ADACat01, aes(y = DeprChrg, x=Year, color="steelblue"), size = 1.1)+
  xlab("Year") +
  ylab("Number of Merit Resolutions Filed") +
  ggtitle("Anxiety and Depression-Related Merit Resolutions Filed") +
  scale_color_discrete(name = "Disability Type", labels = c("Anxiety", 
                                                            "Depression"))
ggplot() + 
  geom_line(data=ADACat01, aes(y = AnxMonMill, x=Year, color = "darkred"), size = 1.1) + 
  geom_line(data=ADACat01, aes(y = DeprMonMill, x=Year, color="steelblue"), size = 1.1) +
  xlab("Year") +
  ylab("Monetary Amount (in Millions)") +
  ggtitle("Anxiety and Depression-Related Monetary Amounts Issued for 
          Settlements") +
  scale_color_discrete(name = "Disability Type", labels = c("Anxiety", 
                                                            "Depression"))
ggplot() + 
  geom_line(data=ADACat01, aes(y = AnxAvgMon, x=Year, color = "darkred"), size = 1.1) + 
  geom_line(data=ADACat01, aes(y = DeprAvgMon, x=Year, color="steelblue"), size = 1.1) +
  xlab("Year") +
  ylab("Monetary Amount") +
  ggtitle("Anxiety and Depression-Related Average Monetary Settlement") +
  scale_color_discrete(name = "Disability Type", labels = c("Anxiety", 
                                                            "Depression"))
ggplot() + 
  geom_line(data=ADACat01, aes(y = AnxNumRecp, x=Year, color = "darkred"), size = 1.1) + 
  geom_line(data=ADACat01, aes(y = DeprNumRecp, x=Year, color="steelblue"), size = 1.1) +
  xlab("Year") +
  ylab("Number of Recipients of Settlements") +
  ggtitle("Number of Anxiety and Depression-Related Monetary Settlement 
          Recipients") +
  scale_color_discrete(name = "Disability Type", labels = c("Anxiety", 
                                                            "Depression"))

## Diagnostics for Anxiety, Depression
par(mfrow = c(2,2))
plot(AnxRslvAdminMult, which = 1:4)
par(mfrow = c(2,2))
plot(DeprRslvAdminMult, which = 1:4)
par(mfrow = c(2,2))
plot(AnxChrgAdminMult, which = 1:4)
par(mfrow = c(2,2))
plot(DeprChrgAdminMult, which = 1:4)
par(mfrow = c(2,2))
plot(AnxMonMillAdminMult, which = 1:4)
par(mfrow = c(2,2))
plot(DeprMonMillAdminMult, which = 1:4)
par(mfrow = c(2,2))
plot(AnxAvgMonAdminMult, which = 1:4)
par(mfrow = c(2,2))
plot(DeprAvgMonAdminMult, which = 1:4)


## The Blind and Deaf Comparison
ggplot() + 
  geom_line(data=ADACat01, aes(y = DeafRslv, x=Year, color = "darkred"), size = 1.1) + 
  geom_line(data=ADACat01, aes(y = BlindRslv, x=Year, color="steelblue"), size = 1.1)+
  xlab("Year") +
  ylab("Number of Recipients of Settlements") +
  ggtitle("Hearing and Vision Impaired-Related Resolutions Filed") +
  scale_color_discrete(name = "Disability Type", labels = c("Hearing Impairment", 
                                                            "Vision Impairment"))
ggplot() + 
  geom_line(data=ADACat01, aes(y = DeafChrg, x=Year, color = "darkred"), size = 1.1) + 
  geom_line(data=ADACat01, aes(y = BlindChrg, x=Year, color="steelblue"), size = 1.1) +
  xlab("Year") +
  ylab("Number of Merit Resolutions Filed") +
  ggtitle("Hearing and Vision Impaired-Related Merit Resolutions Filed") +
  scale_color_discrete(name = "Disability Type", labels = c("Hearing Impairment", 
                                                            "Vision Impairment"))
ggplot(aes(x=Year)) + 
  geom_line(data=ADACat01, aes(y = DeafMonMill, x=Year, color = "darkred"), size = 1.1) + 
  geom_line(data=ADACat01, aes(y = BlindMonMill, x=Year, color="steelblue"), size = 1.1) +
  xlab("Year") +
  ylab("Monetary Amount (in Millions)") +
  ggtitle("Hearing and Vision Impaired-Related Monetary Amounts Issued for 
          Settlements") +
  scale_color_discrete(name = "Disability Type", labels = c("Hearing Impairment", 
                                                            "Vision Impairment"))
ggplot() + 
  geom_line(data=ADACat01, aes(y = DeafAvgMon, x=Year, color = "darkred"), size = 1.1) + 
  geom_line(data=ADACat01, aes(y = BlindAvgMon, x=Year, color="steelblue"), size = 1.1) +
  xlab("Year") +
  ylab("Monetary Amount") +
  ggtitle("Hearing and Vision Impaired-Related Average Monetary Settlement") +
  scale_color_discrete(name = "Disability Type", labels = c("Hearing Impairment", 
                                                            "Vision Impairment"))
ggplot() + 
  geom_line(data=ADACat01, aes(y = DeafNumRecp, x=Year, color = "darkred"), size = 1.1) + 
  geom_line(data=ADACat01, aes(y = BlindNumRecp, x=Year, color="steelblue"), size = 1.1) +
  xlab("Year") +
  ylab("Number of Recipients of Settlements") +
  ggtitle("Number of Hearing and Vision Impaired-Related Monetary Settlement 
          Recipients") +
  scale_color_discrete(name = "Disability Type", labels = c("Hearing Impairment", 
                                                            "Vision Impairment"))

## Diagnostics for Blind, Deaf
par(mfrow = c(2,2))
plot(BlindRslvAdminMult, which = 1:4)
par(mfrow = c(2,2))
plot(DeafRslvAdminMult, which = 1:4)
par(mfrow = c(2,2))
plot(BlindChrgAdminMult, which = 1:4)
par(mfrow = c(2,2))
plot(DeafChrgAdminMult, which = 1:4)
par(mfrow = c(2,2))
plot(BlindMonMillAdminMult, which = 1:4)
par(mfrow = c(2,2))
plot(DeafMonMillAdminMult, which = 1:4)
par(mfrow = c(2,2))
plot(BlindAvgMonAdminMult, which = 1:4)
par(mfrow = c(2,2))
plot(DeafAvgMonAdminMult, which = 1:4)

## Large Variable Set
ggplot() +
  geom_line(data=ADACat01, aes(y=AnxRslv, x=Year, color="sienna"), size = 1.1) +
  geom_line(data=ADACat01, aes(y=DeprRslv, x=Year, color="deepskyblue2"), size = 1.1) +
  geom_line(data=ADACat01, aes(y=AutsRslv, x=Year, color = "blue"), size = 1.1,
            linetype = "dotted") +
  geom_line(data=ADACat01, aes(y=OrthRslv, x=Year, color = "black"), size = 1.1, 
            linetype = "dotted") +
  geom_line(data=ADACat01, aes(y=DeafRslv, x=Year, color = "gold2"), size = 1.1, 
            linetype = "dashed") + 
  geom_line(data=ADACat01, aes(y=BlindRslv, x=Year, color = "midnightblue"), size = 1.1, 
            linetype = "dashed") + 
  xlab("Year") +
  ylab("Number of Resolutions Filed") +
  ggtitle("Resolutions Filed by Disability") +
  scale_color_discrete(name = "Disability Type", labels = c("Orthopedic Back Impairment",
                                                            "Autism", "Depression",
                                                            "Hearing Impairment",
                                                            "Vision Impairment",
                                                            "Anxiety"))

ggplot() +
  geom_line(data=ADACat01, aes(y=AnxChrg, x=Year, color="sienna"), size = 1.1) +
  geom_line(data=ADACat01, aes(y=DeprChrg, x=Year, color="deepskyblue2"), size = 1.1) +
  geom_line(data=ADACat01, aes(y=AutsChrg, x=Year, color = "blue"), size = 1.1,
            linetype = "dotted") +
  geom_line(data=ADACat01, aes(y=OrthChrg, x=Year, color = "black"), size = 1.1, 
            linetype = "dotted") +
  geom_line(data=ADACat01, aes(y=DeafChrg, x=Year, color = "gold2"), size = 1.1, 
            linetype = "dashed") + 
  geom_line(data=ADACat01, aes(y=BlindChrg, x=Year, color = "midnightblue"), size = 1.1, 
            linetype = "dashed") +
  xlab("Year") +
  ylab("Number of Merit Resolutions Filed") +
  ggtitle("Merit Resolutions Filed by Disability") +
  scale_color_discrete(name = "Disability Type", labels = c("Orthopedic Back Impairment",
                                                            "Autism", "Depression",
                                                            "Hearing Impairment",
                                                            "Vision Impairment",
                                                            "Anxiety"))

ggplot() +
  geom_line(data=ADACat01, aes(y=AnxMonMill, x=Year, color="sienna"), size = 1.1) +
  geom_line(data=ADACat01, aes(y=DeprMonMill, x=Year, color="deepskyblue2"), size = 1.1) +
  geom_line(data=ADACat01, aes(y=AutsMonMill, x=Year, color = "blue"), size = 1.1,
            linetype = "dotted") +
  geom_line(data=ADACat01, aes(y=OrthMonMill, x=Year, color = "black"), size = 1.1, 
            linetype = "dotted") +
  geom_line(data=ADACat01, aes(y=DeafMonMill, x=Year, color = "gold2"), size = 1.1, 
            linetype = "dashed") + 
  geom_line(data=ADACat01, aes(y=BlindMonMill, x=Year, color = "midnightblue"), size = 1.1, 
            linetype = "dashed") + 
  xlab("Year") +
  ylab("Monetary Amount (in Millions)") +
  ggtitle("Total Monetary Amounts Issued for Settlements by Disability") +
  scale_color_discrete(name = "Disability Type", labels = c("Orthopedic Back Impairment",
                                                            "Autism", "Depression",
                                                            "Hearing Impairment",
                                                            "Vision Impairment",
                                                            "Anxiety"))

ggplot() +
  geom_line(data=ADACat01, aes(y=AnxNumRecp, x=Year, color="sienna"), size = 1.1) +
  geom_line(data=ADACat01, aes(y=DeprNumRecp, x=Year, color="deepskyblue2"), size = 1.1,
            linetype = "dashed") +
  geom_line(data=ADACat01, aes(y=AutsNumRecp, x=Year, color = "blue"), size = 1.1,
            linetype = "dotted") +
  geom_line(data=ADACat01, aes(y=OrthNumRecp, x=Year, color = "black"), size = 1.1, 
            linetype = "dotted") +
  geom_line(data=ADACat01, aes(y=DeafNumRecp, x=Year, color = "gold2"), size = 1.1, 
            linetype = "dashed") + 
  geom_line(data=ADACat01, aes(y=BlindNumRecp, x=Year, color = "khaki4"), size = 1.1) + 
  xlab("Year") +
  ylab("Monetary Amount") +
  ggtitle("Average Monetary Amounts Issued for Settlements by Disability") +
  scale_color_discrete(name = "Disability Type", labels = c("Orthopedic Back Impairment",
                                                            "Autism", "Depression",
                                                            "Hearing Impairment",
                                                            "Vision Impairment",
                                                            "Anxiety"))

ggplot() +
  geom_line(data=ADACat01, aes(y=AnxNumRecp, x=Year, color="sienna"), size = 1.1) +
  geom_line(data=ADACat01, aes(y=DeprNumRecp, x=Year, color="deepskyblue2"), size = 1.1,
            linetype = "dashed") +
  geom_line(data=ADACat01, aes(y=AutsNumRecp, x=Year, color = "blue"), size = 1.1,
            linetype = "dotted") +
  geom_line(data=ADACat01, aes(y=OrthNumRecp, x=Year, color = "black"), size = 1.1, 
            linetype = "dotted") +
  geom_line(data=ADACat01, aes(y=DeafNumRecp, x=Year, color = "gold2"), size = 1.1, 
            linetype = "dashed") + 
  geom_line(data=ADACat01, aes(y=BlindNumRecp, x=Year, color = "khaki4"), size = 1.1) + 
  xlab("Year") +
  ylab("Number of Recipients of Settlements") +
  ggtitle("Number of Monetary Settlement Recipients by Disability") +
  scale_color_discrete(name = "Disability Type", labels = c("Orthopedic Back Impairment",
                                                            "Autism", "Depression",
                                                            "Hearing Impairment",
                                                            "Vision Impairment",
                                                            "Anxiety"))

## Anxiety and Depression Comparison (By the Numbers)
AnxRslvPartyMult <- lm(AnxRslv ~ Party + GDPA + DiffBudget + DiffStaff,
                      data = ADACat01)
summary(AnxRslvPartyMult)
DeprRslvPartyMult <- lm(DeprRslv ~ Party + GDPA + DiffBudget + DiffStaff,
                       data = ADACat01)
summary(DeprRslvPartyMult)
AnxRslvAdminMult <- lm(AnxRslv ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                        DiffBudget + DiffStaff, data = ADACat01)
summary(AnxRslvAdminMult)
DeprRslvAdminMult <- lm(DeprRslv ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                          DiffBudget + DiffStaff, data = ADACat01)
summary(DeprRslvAdminMult)

AnxChrgPartyMult <- lm(AnxChrg ~ Party + GDPA + DiffBudget + DiffStaff,
                      data = ADACat01)
summary(AnxChrgPartyReg)
DeprChrgPartyMult <- lm(DeprChrg ~ Party + GDPA + DiffBudget + DiffStaff,
                       data = ADACat01)
summary(DeprChrgPartyMult)
AnxChrgAdminMult <- lm(AnxChrg ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                        DiffBudget + DiffStaff, data = ADACat01)
summary(AnxChrgAdminMult)
DeprChrgAdminMult <- lm(DeprChrg ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                          DiffBudget + DiffStaff, data = ADACat01)
summary(DeprChrgAdminMult)

AnxMonMillPartyMult <- lm(AnxMonMill ~ Party + GDPA + DiffBudget + DiffStaff,
                         data = ADACat01)
summary(AnxMonMillPartyMult)
DeprMonMillPartyReg <- lm(DeprMonMill ~ Party + GDPA + DiffBudget + DiffStaff,
                          data = ADACat01)
summary(DeprRslvPartyMult)
AnxMonMillAdminMult <- lm(AnxMonMill ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                            DiffBudget + DiffStaff, data = ADACat01)
summary(AnxMonMillAdminMult)
DeprMonMillAdminMult <- lm(DeprMonMill ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                             DiffBudget + DiffStaff, data = ADACat01)
summary(DeprMonMillAdminMult)

AnxAvgMonPartyMult <- lm(AnxAvgMon ~ Party + GDPA + DiffBudget + DiffStaff,
                        data = ADACat01)
summary(AnxAvgMonPartyMult)
DeprAvgMonPartyMult <- lm(DeprAvgMon ~ Party + GDPA + DiffBudget + DiffStaff,
                         data = ADACat01)
summary(DeprAvgMonPartyMult)
AnxAvgMonAdminMult <- lm(AnxAvgMon ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                          DiffBudget + DiffStaff, data = ADACat01)
summary(AnxAvgMonAdminMult)
DeprAvgMonAdminMult <- lm(DeprAvgMon ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                            DiffBudget + DiffStaff, data = ADACat01)
summary(DeprAvgMonAdminMult)

AnxNumRecpPartyMult <- lm(AnxNumRecp ~ Party + GDPA + DiffBudget + DiffStaff,
                    data = ADACat01)
summary(AnxNumRecpPartyMult)
DeprNumRecpPartyMult <- lm(DeprNumRecp ~ Party + GDPA + DiffBudget + DiffStaff,
                          data = ADACat01)
summary(DeprNumRecpPartyMult)
AnxNumRecpAdminMult <- lm(AnxNumRecp ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                            DiffBudget + DiffStaff, data = ADACat01)
summary(AnxNumRecpAdminMult)
DeprNumRecpAdminMult <- lm(DeprNumRecp ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                             DiffBudget + DiffStaff, data = ADACat01)
summary(DeprNumRecpAdminMult)

## Improved Regression, Anxiety and Depression (Heteroskedasticity)
ImprAnxRslvAdmin <- lm(AnxRslv ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                         DiffBudget + DiffStaff, data = ADACat01, 
                       na.action = na.exclude)
  summary(ImprAnxRslvAdmin)
  bptest(ImprAnxRslvAdmin, studentize = FALSE)
  ## No heteroskedasticity unless otherwise noted.
ImprDeprRslvAdmin <- lm(DeprRslv ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                           DiffBudget + DiffStaff, data = ADACat01, 
                         na.action = na.exclude)
  summary(ImprDeprRslvAdmin)
  bptest(ImprDeprRslvAdmin, studentize = FALSE)
ImprAnxChrgAdmin <- lm(AnxChrg ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                           DiffBudget + DiffStaff, data = ADACat01, 
                         na.action = na.exclude)
  summary(ImprAnxChrgAdmin)
  bptest(ImprAnxChrgAdmin, studentize = FALSE)
ImprDeprChrgAdmin <- lm(DeprChrg ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                           DiffBudget + DiffStaff, data = ADACat01, 
                         na.action = na.exclude)
  summary(ImprDeprChrgAdmin)
  bptest(ImprDeprChrgAdmin, studentize = FALSE)
ImprAnxMonMillAdmin <- lm(AnxMonMill ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                           DiffBudget + DiffStaff, data = ADACat01, 
                         na.action = na.exclude)
  summary(ImprAnxMonMillAdmin)
  bptest(ImprAnxMonMillAdmin, studentize = FALSE)
ImprDeprMonMillAdmin <- lm(DeprMonMill ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                              DiffBudget + DiffStaff, data = ADACat01, 
                            na.action = na.exclude)
  summary(ImprDeprMonMillAdmin)
  bptest(ImprDeprMonMillAdmin, studentize = FALSE)
ImprAnxAvgMonAdmin <- lm(AnxAvgMon ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                              DiffBudget + DiffStaff, data = ADACat01, 
                            na.action = na.exclude)
  summary(ImprAnxAvgMonAdmin)
  bptest(ImprAnxAvgMonAdmin, studentize = FALSE)
ImprDeprAvgMonAdmin <- lm(DeprAvgMon ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                             DiffBudget + DiffStaff, data = ADACat01, 
                           na.action = na.exclude)
  summary(ImprDeprAvgMonAdmin)
  bptest(ImprDeprAvgMonAdmin, studentize = FALSE)
  ## Almost statistically significant at .07, but not quite.
ImprAnxNumRecpAdmin <- lm(AnxNumRecp ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                             DiffBudget + DiffStaff, data = ADACat01, 
                           na.action = na.exclude)
  summary(ImprAnxNumRecpAdmin)
  bptest(ImprAnxNumRecpAdmin, studentize = FALSE)
  ## 2014 looks to be our outlier, making this statistically significant and thus
  ## heteroskedastic.
ImprDeprNumRecpAdmin <- lm(DeprNumRecp ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                              DiffBudget + DiffStaff, data = ADACat01, 
                            na.action = na.exclude)
  summary(ImprDeprNumRecpAdmin)
  bptest(ImprDeprNumRecpAdmin, studentize = FALSE)
  ## Mostly the same as anxiety, but the outliers are 2011 and 2018.
  ## Weighting could be done, but for the sake of some kind of simplicity, we'll
  ## forgo it here.

## Multicollinearity and Autocorrelation, Anxiety and Depression
vif(AnxRslvAdminMult)
  ## Same story of multicollinearity as the overall number set.
dwtest(AnxRslvAdminMult)
  cochrane.orcutt(AnxRslvAdminMult)
  summary(cochrane.orcutt(AnxRslvAdminMult))
  dwtest(cochrane.orcutt(AnxRslvAdminMult))
  ## And again, Orcutt to the rescue.
vif(DeprRslvAdminMult)
dwtest(DeprRslvAdminMult)
  cochrane.orcutt(DeprRslvAdminMult)
  summary(cochrane.orcutt(DeprRslvAdminMult))
  dwtest(cochrane.orcutt(DeprRslvAdminMult))
vif(AnxChrgAdminMult)
dwtest(AnxChrgAdminMult)
  cochrane.orcutt(AnxChrgAdminMult)
  summary(cochrane.orcutt(AnxChrgAdminMult))
  dwtest(cochrane.orcutt(AnxRslvAdminMult))
## Dropping vif, because it's the same thing for each.
dwtest(DeprChrgAdminMult)
  cochrane.orcutt(DeprChrgAdminMult)
  summary(cochrane.orcutt(DeprChrgAdminMult))
  dwtest(cochrane.orcutt(DeprRslvAdminMult))
dwtest(AnxMonMillAdminMult)
  cochrane.orcutt(AnxMonMillAdminMult)
  summary(cochrane.orcutt(AnxMonMillAdminMult))
  dwtest(cochrane.orcutt(AnxMonMillAdminMult))
  ## Only just misses statistical significance at .06.
dwtest(DeprMonMillAdminMult)
dwtest(AnxNumRecpAdminMult)
dwtest(DeprNumRecpAdminMult)

## Blind and Deaf Comparison (By the Numbers)
BlindRslvPartyMult <- lm(BlindRslv ~ Party + GDPA + DiffBudget + DiffStaff,
                       data = ADACat01)
summary(BlindRslvPartyMult)
DeafRslvPartyMult <- lm(DeafRslv ~ Party + GDPA + DiffBudget + DiffStaff,
                        data = ADACat01)
summary(DeafRslvPartyMult)
BlindRslvAdminMult <- lm(BlindRslv ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                         DiffBudget + DiffStaff, data = ADACat01)
summary(BlindRslvAdminMult)
DeafRslvAdminMult <- lm(DeafRslv ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                          DiffBudget + DiffStaff, data = ADACat01)
summary(DeafRslvAdminMult)

BlindChrgPartyMult <- lm(BlindChrg ~ Party + GDPA + DiffBudget + DiffStaff,
                       data = ADACat01)
summary(BlindChrgPartyReg)
DeafChrgPartyMult <- lm(DeafChrg ~ Party + GDPA + DiffBudget + DiffStaff,
                        data = ADACat01)
summary(DeafChrgPartyMult)
BlindChrgAdminMult <- lm(BlindChrg ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                         DiffBudget + DiffStaff, data = ADACat01)
summary(BlindChrgAdminMult)
DeafChrgAdminMult <- lm(DeafChrg ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                          DiffBudget + DiffStaff, data = ADACat01)
summary(DeafChrgAdminMult)

BlindMonMillPartyMult <- lm(BlindMonMill ~ Party + GDPA + DiffBudget + DiffStaff,
                          data = ADACat01)
summary(BlindMonMillPartyMult)
DeafMonMillPartyReg <- lm(DeafMonMill ~ Party + GDPA + DiffBudget + DiffStaff,
                          data = ADACat01)
summary(DeafRslvPartyMult)
BlindMonMillAdminMult <- lm(BlindMonMill ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                            DiffBudget + DiffStaff, data = ADACat01)
summary(BlindMonMillAdminMult)
DeafMonMillAdminMult <- lm(DeafMonMill ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                             DiffBudget + DiffStaff, data = ADACat01)
summary(DeafMonMillAdminMult)

BlindAvgMonPartyMult <- lm(BlindAvgMon ~ Party + GDPA + DiffBudget + DiffStaff,
                         data = ADACat01)
summary(BlindAvgMonPartyMult)
DeafAvgMonPartyMult <- lm(DeafAvgMon ~ Party + GDPA + DiffBudget + DiffStaff,
                          data = ADACat01)
summary(DeafAvgMonPartyMult)
BlindAvgMonAdminMult <- lm(BlindAvgMon ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                           DiffBudget + DiffStaff, data = ADACat01)
summary(BlindAvgMonAdminMult)
DeafAvgMonAdminMult <- lm(DeafAvgMon ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                            DiffBudget + DiffStaff, data = ADACat01)
summary(DeafAvgMonAdminMult)

BlindNumRecpPartyMult <- lm(BlindNumRecp ~ Party + GDPA + DiffBudget + DiffStaff,
                          data = ADACat01)
summary(BlindNumRecpPartyMult)
DeafNumRecpPartyMult <- lm(DeafNumRecp ~ Party + GDPA + DiffBudget + DiffStaff,
                           data = ADACat01)
summary(DeafNumRecpPartyMult)
BlindNumRecpAdminMult <- lm(BlindNumRecp ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                            DiffBudget + DiffStaff, data = ADACat01)
summary(BlindNumRecpAdminMult)
DeafNumRecpAdminMult <- lm(DeafNumRecp ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                             DiffBudget + DiffStaff, data = ADACat01)
summary(DeafNumRecpAdminMult)

## Improved Regression, Anxiety and Depression (Heteroskedasticity)
ImprBlindRslvAdmin <- lm(BlindRslv ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                         DiffBudget + DiffStaff, data = ADACat01, 
                       na.action = na.exclude)
summary(ImprBlindRslvAdmin)
bptest(ImprBlindRslvAdmin, studentize = FALSE)
## No heteroskedasticity unless otherwise noted.
ImprDeafRslvAdmin <- lm(DeafRslv ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                          DiffBudget + DiffStaff, data = ADACat01, 
                        na.action = na.exclude)
summary(ImprDeafRslvAdmin)
bptest(ImprDeafRslvAdmin, studentize = FALSE)
ImprBlindChrgAdmin <- lm(BlindChrg ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                         DiffBudget + DiffStaff, data = ADACat01, 
                       na.action = na.exclude)
summary(ImprBlindChrgAdmin)
bptest(ImprBlindChrgAdmin, studentize = FALSE)
ImprDeafChrgAdmin <- lm(DeafChrg ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                          DiffBudget + DiffStaff, data = ADACat01, 
                        na.action = na.exclude)
summary(ImprDeafChrgAdmin)
bptest(ImprDeafChrgAdmin, studentize = FALSE)
ImprBlindMonMillAdmin <- lm(BlindMonMill ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                            DiffBudget + DiffStaff, data = ADACat01, 
                          na.action = na.exclude)
summary(ImprBlindMonMillAdmin)
bptest(ImprBlindMonMillAdmin, studentize = FALSE)
## Almost statistically significant at .07, but not quite.
ImprDeafMonMillAdmin <- lm(DeafMonMill ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                             DiffBudget + DiffStaff, data = ADACat01, 
                           na.action = na.exclude)
summary(ImprDeafMonMillAdmin)
bptest(ImprDeafMonMillAdmin, studentize = FALSE)
ImprBlindAvgMonAdmin <- lm(BlindAvgMon ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                           DiffBudget + DiffStaff, data = ADACat01, 
                         na.action = na.exclude)
summary(ImprBlindAvgMonAdmin)
bptest(ImprBlindAvgMonAdmin, studentize = FALSE)
ImprDeafAvgMonAdmin <- lm(DeafAvgMon ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                            DiffBudget + DiffStaff, data = ADACat01, 
                          na.action = na.exclude)
summary(ImprDeafAvgMonAdmin)
bptest(ImprDeafAvgMonAdmin, studentize = FALSE)
ImprBlindNumRecpAdmin <- lm(BlindNumRecp ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                            DiffBudget + DiffStaff, data = ADACat01, 
                          na.action = na.exclude)
summary(ImprBlindNumRecpAdmin)
bptest(ImprBlindNumRecpAdmin, studentize = FALSE)
ImprDeafNumRecpAdmin <- lm(DeafNumRecp ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                             DiffBudget + DiffStaff, data = ADACat01, 
                           na.action = na.exclude)
summary(ImprDeafNumRecpAdmin)
bptest(ImprDeafNumRecpAdmin, studentize = FALSE)

## Multicollinearity and Autocorrelation, Blind and Deaf
vif(BlindRslvAdminMult)
## Same story of multicollinearity as the overall number set.
dwtest(BlindRslvAdminMult)
  ## Not statistically significant, but just barely at .08.
vif(DeafRslvAdminMult)
dwtest(DeafRslvAdminMult)
vif(BlindChrgAdminMult)
dwtest(BlindChrgAdminMult)
## Dropping vif, because it's the same thing for each.
dwtest(DeafChrgAdminMult)
dwtest(BlindMonMillAdminMult)
dwtest(DeafMonMillAdminMult)
dwtest(BlindNumRecpAdminMult)
dwtest(DeafNumRecpAdminMult)
  ## Autocorrelation is no issue here, which is a relief. A couple of slightly
  ## high D scores, but nothing outlandish.

## Autism and Orthopedic Back Impairment Comparison (By the Numbers)
AutsRslvPartyMult <- lm(AutsRslv ~ Party + GDPA + DiffBudget + DiffStaff,
                       data = ADACat01)
summary(AutsRslvPartyMult)
OrthRslvPartyMult <- lm(OrthRslv ~ Party + GDPA + DiffBudget + DiffStaff,
                        data = ADACat01)
summary(OrthRslvPartyMult)
AutsRslvAdminMult <- lm(AutsRslv ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                         DiffBudget + DiffStaff, data = ADACat01)
summary(AutsRslvAdminMult)
OrthRslvAdminMult <- lm(OrthRslv ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                          DiffBudget + DiffStaff, data = ADACat01)
summary(OrthRslvAdminMult)

AutsChrgPartyMult <- lm(AutsChrg ~ Party + GDPA + DiffBudget + DiffStaff,
                       data = ADACat01)
summary(AutsChrgPartyReg)
OrthChrgPartyMult <- lm(OrthChrg ~ Party + GDPA + DiffBudget + DiffStaff,
                        data = ADACat01)
summary(OrthChrgPartyMult)
AutsChrgAdminMult <- lm(AutsChrg ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                         DiffBudget + DiffStaff, data = ADACat01)
summary(AutsChrgAdminMult)
OrthChrgAdminMult <- lm(OrthChrg ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                          DiffBudget + DiffStaff, data = ADACat01)
summary(OrthChrgAdminMult)

AutsMonMillPartyMult <- lm(AutsMonMill ~ Party + GDPA + DiffBudget + DiffStaff,
                          data = ADACat01)
summary(AutsMonMillPartyMult)
OrthMonMillPartyReg <- lm(OrthMonMill ~ Party + GDPA + DiffBudget + DiffStaff,
                          data = ADACat01)
summary(OrthMonMillPartyMult)
AutsMonMillAdminMult <- lm(AutsMonMill ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                            DiffBudget + DiffStaff, data = ADACat01)
summary(AnxMonMillAdminMult)
OrthMonMillAdminMult <- lm(OrthMonMill ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                             DiffBudget + DiffStaff, data = ADACat01)
summary(OrthMonMillAdminMult)

AutsAvgMonPartyMult <- lm(AutsAvgMon ~ Party + GDPA + DiffBudget + DiffStaff,
                         data = ADACat01)
summary(AutsAvgMonPartyMult)
OrthAvgMonPartyMult <- lm(OrthAvgMon ~ Party + GDPA + DiffBudget + DiffStaff,
                          data = ADACat01)
summary(OrthAvgMonPartyMult)
AutsAvgMonAdminMult <- lm(AutsAvgMon ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                           DiffBudget + DiffStaff, data = ADACat01)
summary(AutsAvgMonAdminMult)
OrthAvgMonAdminMult <- lm(OrthAvgMon ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                            DiffBudget + DiffStaff, data = ADACat01)
summary(OrthAvgMonAdminMult)

AutsNumRecpPartyMult <- lm(AutsNumRecp ~ Party + GDPA + DiffBudget + DiffStaff,
                          data = ADACat01)
summary(AutsNumRecpPartyMult)
OrthNumRecpPartyMult <- lm(OrthNumRecp ~ Party + GDPA + DiffBudget + DiffStaff,
                           data = ADACat01)
summary(OrthNumRecpPartyMult)
AutsNumRecpAdminMult <- lm(AutsNumRecp ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                            DiffBudget + DiffStaff, data = ADACat01)
summary(AutsNumRecpAdminMult)
OrthNumRecpAdminMult <- lm(OrthNumRecp ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                             DiffBudget + DiffStaff, data = ADACat01)
summary(OrthNumRecpAdminMult)

## Improved Regression, Anxiety and Orthopedic Back Impairment (Heteroskedasticity)
ImprAutsRslvAdmin <- lm(AutsRslv ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                         DiffBudget + DiffStaff, data = ADACat01, 
                       na.action = na.exclude)
summary(ImprAutsRslvAdmin)
bptest(ImprAutsRslvAdmin, studentize = FALSE)
## No heteroskedasticity unless otherwise noted.
ImprOrthRslvAdmin <- lm(OrthRslv ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                          DiffBudget + DiffStaff, data = ADACat01, 
                        na.action = na.exclude)
summary(ImprOrthRslvAdmin)
bptest(ImprOrthRslvAdmin, studentize = FALSE)
  ## Heteroskedastic, but only just at .051. This is just down to the weird high
  ## of 3,800 filed in 1997.
ImprAutsChrgAdmin <- lm(AutsChrg ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                         DiffBudget + DiffStaff, data = ADACat01, 
                       na.action = na.exclude)
summary(ImprAutsChrgAdmin)
bptest(ImprAutsChrgAdmin, studentize = FALSE)
  ## I would wager to guess this heteroskedasticity was attributable to the last
  ## few years which has seen marginally more wins than 0.
ImprOrthChrgAdmin <- lm(DeprChrg ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                          DiffBudget + DiffStaff, data = ADACat01, 
                        na.action = na.exclude)
summary(ImprOrthChrgAdmin)
bptest(ImprOrthChrgAdmin, studentize = FALSE)
ImprAutsMonMillAdmin <- lm(AutsMonMill ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                            DiffBudget + DiffStaff, data = ADACat01, 
                          na.action = na.exclude)
summary(ImprAutsMonMillAdmin)
bptest(ImprAutsMonMillAdmin, studentize = FALSE)
  ## This is also heteroskedastic and again, because there have actually been more
  ## than zero wins for this category in the last few years.
ImprOrthMonMillAdmin <- lm(OrthMonMill ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                             DiffBudget + DiffStaff, data = ADACat01, 
                           na.action = na.exclude)
summary(ImprOrthMonMillAdmin)
bptest(ImprOrthMonMillAdmin, studentize = FALSE)
ImprAutsAvgMonAdmin <- lm(AutsAvgMon ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                           DiffBudget + DiffStaff, data = ADACat01, 
                         na.action = na.exclude)
summary(ImprAutsAvgMonAdmin)
bptest(ImprAutsAvgMonAdmin, studentize = FALSE)
ImprOrthAvgMonAdmin <- lm(OrthAvgMon ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                            DiffBudget + DiffStaff, data = ADACat01, 
                          na.action = na.exclude)
summary(ImprOrthAvgMonAdmin)
bptest(ImprOrthAvgMonAdmin, studentize = FALSE)
ImprAutsNumRecpAdmin <- lm(AutsNumRecp ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                            DiffBudget + DiffStaff, data = ADACat01, 
                          na.action = na.exclude)
summary(ImprAutsNumRecpAdmin)
bptest(ImprAutsNumRecpAdmin, studentize = FALSE)
## 2016 looks to be our outlier, making this statistically significant and thus
## heteroskedastic.
ImprOrthNumRecpAdmin <- lm(OrthNumRecp ~ ClintonCat + WBushCat + ObamaCat + GDPA + 
                             DiffBudget + DiffStaff, data = ADACat01, 
                           na.action = na.exclude)
summary(ImprOrthNumRecpAdmin)
bptest(ImprOrthNumRecpAdmin, studentize = FALSE)

## Multicollinearity and Autocorrelation, Autism and Orthopedic Back Impairment
vif(AutsRslvAdminMult)
## Same story of multicollinearity as the overall number set.
dwtest(AutsRslvAdminMult)
cochrane.orcutt(AutsRslvAdminMult)
summary(cochrane.orcutt(AutsRslvAdminMult))
dwtest(cochrane.orcutt(AutsRslvAdminMult))
## And again, Orcutt to the rescue.
vif(OrthRslvAdminMult)
dwtest(OrthRslvAdminMult)
cochrane.orcutt(OrthRslvAdminMult)
summary(cochrane.orcutt(OrthRslvAdminMult))
dwtest(cochrane.orcutt(OrthRslvAdminMult))
vif(AutsChrgAdminMult)
dwtest(AutsChrgAdminMult)
## Dropping vif, because it's the same thing for each.
dwtest(OrthChrgAdminMult)
dwtest(AutsMonMillAdminMult)
dwtest(OrthMonMillAdminMult)
dwtest(AutsNumRecpAdminMult)
dwtest(OrthNumRecpAdminMult)
cochrane.orcutt(OrthNumRecpAdminMult)
summary(cochrane.orcutt(OrthNumRecpAdminMult))
dwtest(cochrane.orcutt(OrthNumRecpAdminMult))

## Tables for Paper
htmlreg(list(RslvTotMult, ChrgTotMult, MonMillTotMult, AvgMonTotMult), 
        file = "../Desktop/MPPA/POL SCI 6402 Int Stats/ADA Research/Vizualization/RslvTablePaper.doc",
        inline.css = FALSE, doctype = TRUE, html.tag = TRUE, head.tag = TRUE, 
        body.tag = TRUE)
unlink("texreg.doc")
htmlreg(list(RslvTotAdminDum, ChrgTotAdminDum, MonTotMillAdminDum, AvgMonTotAdminDum), 
        file = "../Desktop/MPPA/POL SCI 6402 Int Stats/ADA Research/Visualization/RegTablePaper.doc",
        inline.css = FALSE, doctype = TRUE, html.tag = TRUE, head.tag = TRUE, 
        body.tag = TRUE)
unlink("texreg.doc")

htmlreg(list(DeafRslvAdminMult, DeafChrgAdminMult, DeafMonMillAdminMult, 
             DeafAvgMonMult), 
        file = "../Desktop/MPPA/POL SCI 6402 Int Stats/ADA Research/Visualization/DeafTablePaper.doc",
        inline.css = FALSE, doctype = TRUE, html.tag = TRUE, head.tag = TRUE, 
        body.tag = TRUE)
unlink("texreg.doc")
htmlreg(list(DeafRslvPartyMult, DeafChrgPartyMult, DeafMonMillPartyReg, DeafAvgMonPartyMult),
        file = "../Desktop/MPPA/POL SCI 6402 Int Stats/ADA Research/Visualization/DeafPartyTablePaper.doc",
        inline.css = FALSE, doctype = TRUE, html.tag = TRUE, head.tag = TRUE, 
        body.tag = TRUE)
unlink("texreg.doc")