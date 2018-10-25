#Data for Humpback Assessment Model - Preliminary Runs Sep 2018
#Sent to Grant Adams and John Best on 25 Sep 2018
##################################################################

#2005 Absoulute Abundance
Abs.Abundance.2005 <- data.frame(Year=c(2005),
                                 N.obs=c(6404),
                                 CV.obs=c(0.12))

#2008 Absolute Abundance
Abs.Abundance.2008 <- data.frame(Year=c(2008),
                                 N.obs=c(14264),
                              CV.obs=c(0.084))

#2008 Absolute Abundance
Abs.Abundance.2012 <- data.frame(Year=c(2012),
                                 N.obs=c(20389),
                              CV.obs=c(0.071))


#Index of Abundance from Pavanato et al. 2017, Ecol. Modelling, Table 3
Rel.Abundance.Pavanato <- data.frame(
    Index = rep(1,3),
    Year=c(2008, 2011, 2015),
    IA.obs=c(7689, 8652, 12123),
    CV.IA.obs=c(0.078, 0.066, 0.07))

#Catch Series Modern
Core.Catches <- data.frame(Year=seq(1901,2017),
                           Catch=c(0, 0, 0, 180, 288, 240, 1261, 1849, 3391, 6468, 5832, 2881, 999, 1155,
                                  1697, 447, 121, 129, 111, 102, 9, 364, 133, 266, 254, 7, 0, 19, 51, 107,
                                  18, 23, 132, 57, 48, 105, 242, 0, 2, 36, 13, 0, 4, 60, 238, 30, 35, 48,
                                  83, 698, 45, 34, 140, 44, 96, 167, 61, 16, 15, 27, 13, 24, 12, 0, 52, 0,
                                  189, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0 ,0))
Core.Catches$Period <- 1

#Catch Series Pre-Modern
PreModern.Catch <- data.frame(Year=seq(1830,1924),
                          Catch=c(rep(400, 10), #1830-1839
                                  rep(402.8, 10), #1840-1849
                                  rep(400, 10), #1850-1859
                                  rep(418.1, 10), #1860-1869
                                  rep(400, 24), #1870-1893
                                  448, #1894
                                  rep(400, 6), #1895-1900
                                  rep(581.7, 2), #1901-1902
                                  400, #1903
                                  581.7, #1904 (first year with pre-modern + modern)
                                  581.7, #1905 (pre-modern + modern)
                                  400, #1906
                                  400, #1907
                                  406.5, #1908
                                  228.03, #1909
                                  246.62, #1910
                                  rep(30, 14))) #1911-1924

