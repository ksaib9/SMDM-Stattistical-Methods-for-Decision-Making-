#a for loop to classify females and males data seperately

GPAMales <- c()
GPAFemales <- c()
SalaryMales <- c()
SalaryFemales <- c()
SpendMales <- c()
SpendFemales <- c()
WealthMales <-c()
WealthFemales <- c()




UnderGradGener <- c(UnderGradSurvey$Gender)
UnderGradGPA <- c(UnderGradSurvey$GPA)
UnderGradSalary <- c(UnderGradSurvey$Salary)
UnderGradSpend <- c(UnderGradSurvey$Spending)
UnderGradWealth <- c(UnderGradSurvey$Wealth)


for ( i in  1:length(UnderGradSurvey$Gender))
{
  
    
    if(UnderGradGener[i] == "Male")
    {
    GPAMales <- c(GPAMales , UnderGradGPA[i])
    SalaryMales <- c(SalaryMales ,UnderGradSalary[i] )
    SpendMales <- c(SpendMales, UnderGradSpend[i])
    WealthMales <- c(WealthMales ,UnderGradWealth[i])
    
    
    }
    else{
    
      GPAFemales <- c(GPAFemales ,UnderGradGPA[i])
      SalaryFemales <- c(SalaryFemales ,UnderGradSalary[i] )
      SpendFemales <- c(SpendFemales, UnderGradSpend[i])
      WealthFemales <- c(WealthFemales ,UnderGradWealth[i])
      
      
    }
  
}

BoxUnderGradSal <- data.frame(UnderGradSalary,SalaryMales,SalaryFemales)

# 95% confidence intervals for GPA , Male GPA and FEmale GPA

stat.desc(UnderGradGPA)

stat.desc(GPAMales)

stat.desc(GPAFemales)


# Salary


stat.desc(UnderGradSalary)

stat.desc(SalaryMales)

stat.desc(SalaryFemales)


# Spending

stat.desc(UnderGradSpend)

stat.desc(SpendMales)

stat.desc(SpendFemales)


#Wealth

stat.desc(UnderGradWealth)

stat.desc(WealthMales)

stat.desc(WealthFemales)



# Chi square test to test if variances are equal or not

tb1 = table(UnderGradSurvey$Employment , UnderGradSurvey$Gender)

chisq.test(tb1)


tb2 = table(UnderGradSurvey$Employment , UnderGradSurvey$`Grad Intention`)

chisq.test(tb2)

Gender <- table(UnderGradGener)


par(oma = c()) # Sets outside margins: b, l, t, r
par(mar = c())  # Sets plot margins
par(mai = c(1,2,1,1))
barplot(Gender[order(Gender)], 
        horiz  = TRUE,
        xlim = c(0,50),
        las    = 1,  # las gives orientation of axis labels
        col    = c("beige", "blanchedalmond"),
        border = NA,  # No borders on bars
        main   = "Males and females in the dataset",  # \n = line break
        xlab   = "Number of people")

Class <- table(UnderGradSurvey$Class)


barplot(Class[order(Class)], 
        horiz  = TRUE,
        xlim = c(0,50),
        las    = 1,  # las gives orientation of axis labels
        col    = c("beige", "blanchedalmond","bisque2"),
        border = NA,  # No borders on bars
        main   = "Class",  # \n = line break
        xlab   = "Number of people")


Major <- table(UnderGradSurvey$Major)


barplot(Major[order(Major)], 
        horiz  = TRUE,
        xlim = c(0,50),
        las    = 1,  # las gives orientation of axis labels
        col    = c("#338cf0", "#2d7ed8", "#2870c0", "#2362a8", "#1e5490", "#194678","#143860","#0f2a48"),
        border = NA,  # No borders on bars
        main   = "Major",  # \n = line break
        xlab   = "Number of people")

Employment <- table(UnderGradSurvey$Employment)


barplot(Employment[order(Employment)], 
        horiz  = TRUE,
        xlim = c(0,50),
        las    = 1,  # las gives orientation of axis labels
        col    = c("#338cf0", "#9733f0", "#30d8aa"),
        border = NA,  # No borders on bars
        main   = "Employment",  # \n = line break
        xlab   = "Number of people")


Computer <- table(UnderGradSurvey$Computer)


barplot(Computer[order(Computer)], 
        horiz  = TRUE,
        xlim = c(0,80),
        las    = 1,  # las gives orientation of axis labels
        col    = c("#338cf0", "#9733f0", "#30d8aa"),
        border = NA,  # No borders on bars
        main   = "Computer",  # \n = line break
        xlab   = "Number of people")












boxplot(UnderGradSalary,col ="Blue" ,ylab = "Salary",las=2,main = "Salary of Males and Females")
boxplot(SalaryMales,col="brown",las=2,main = "Salary of Males ",ylab = "Salary")
boxplot(SalaryFemales,col="darkgreen",las=2,main = "Salary of Females ",ylab = "Salary")
boxplot(UnderGradSpend,col ="Blue" ,ylab = "Spending",las=2,main = "Spending of Males and Females")
boxplot(SpendMales,col="brown",las=2,main = "Spending of Males ",ylab = "Spending")
boxplot(SpendFemales,col="darkgreen",las=2,main = "Spending of Females ",ylab = "Spending")

boxplot(UnderGradWealth,col ="Blue" ,ylab = "Weath",las=2,main = "Wealth of Males and Females")

boxplot(WealthMales,col="brown",las=2,ylab = "Wealth",main = "Wealth of Males ")
boxplot(WealthFemales,col="darkgreen",las=2,ylab = "Wealth",main = "Wealth of Females ")

