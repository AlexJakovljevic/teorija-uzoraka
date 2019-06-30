install.packages("ggplot2")
library(ggplot2)
library(scales)
podaci = read.table(file="StudentsPerformance.csv", header=TRUE, sep=",")
podaci
# analiza podataka
ggplot(podaci, aes(x=podaci$math.score)) + geom_histogram()

ggplot(podaci, aes(x=podaci$math.score, color=podaci$gender)) +
  geom_histogram(fill="white", bins = 20)

ggplot(podaci, aes(x=podaci$writing.score, color=podaci$gender)) +
geom_histogram(fill = "white", alpha=0.8, position="identity", bins = 20)

total_score = (podaci$writing.score + podaci$reading.score + podaci$math.score) / 3.0
podaci$total =total_score
ggplot(podaci, aes(x=total_score, color=podaci$gender)) +
  geom_histogram(fill="white", alpha=0.5, position="identity", bins = 100)

# pita
mytable = table(podaci$race.ethnicity)
lbls = paste(names(mytable), "=", mytable/10,"%", sep=" ")
pie(mytable, labels = lbls, 
    main="Rasna pripadnosti u populaciji")
mytable = table(podaci$parental.level.of.education)
lbls = paste(names(mytable), "=", mytable/10,"%", sep=" ")
pie(mytable, labels = lbls, 
    main="Stepen obrazovanja roditelja")
mytable = table(podaci$lunch)
lbls = paste(names(mytable), "=", mytable/10,"%", sep=" ")
pie(mytable, labels = lbls, 
    main="Konzumiranje obroka u skoli")
mytable = table(podaci$test.preparation.course)
lbls = paste(names(mytable), "=", mytable/10,"%", sep=" ")
pie(mytable, labels = lbls, 
    main="Probni test")
mytable = table(podaci$gender)
lbls = paste(names(mytable), "=", mytable/10,"%", sep=" ")
pie(mytable, labels = lbls, 
    main="Pol")
mytable = table(podaci$lunch)
lbls = paste(names(mytable), "=", mytable/10,"%", sep=" ")
pie(mytable, labels = lbls, 
    main="Obrok")
x1 = rnorm(100)
x2 = rnorm(100)+rep(2,100)
par(mfrow=c(2,1))

#Make the plot
par(mar=c(0,5,3,3))
hist(total_score[podaci$gender == "male"] , main="" , xlim=c(0,100), ylab="Ucestalost muske pop.", xlab="", ylim=c(0,80) , xaxt="n", las=1 , col="slateblue1", breaks=15)
#d_m = density(total_score[podaci$gender == "male"])
par(mar=c(5,5,0,3))
hist(total_score[podaci$gender == "female"], main="" , xlim=c(0,100), ylab="Ucestalost zenske pop.", xlab="Prosecan broj poena", ylim=c(80,0) , las=1 , col="tomato3"  , breaks=15)


par(mfrow=c(2,1))

#Make the plot
par(mar=c(0,5,3,3))
hist(total_score[podaci$lunch == "standard"] , main="" , xlim=c(0,100), ylab="standard", xlab="", ylim=c(0,100) , xaxt="n", las=1 , col="slateblue1", breaks=20)
par(mar=c(5,5,0,3))
hist(total_score[podaci$lunch == "free/reduced"], main="" , xlim=c(0,100), ylab="free/reduced", xlab="Prosecan broj poena", ylim=c(100,0) , las=1 , col="tomato3"  , breaks=20)

par(mfrow=c(1,1))
hist(total_score[podaci$race.ethnicity  == "group A"] , main="" , xlim=c(0,100), ylab="group A", xlab="Prosecan broj poena", ylim=c(0,15), las=1 , col="slateblue1", breaks=20)
hist(total_score[podaci$race.ethnicity  == "group B"] , main="" , xlim=c(0,100), ylab="group B", xlab="Prosecan broj poena", ylim=c(0,35) , las=1 , col="slateblue1", breaks=20)
hist(total_score[podaci$race.ethnicity  == "group C"] , main="" , xlim=c(0,100), ylab="group C", xlab="Prosecan broj poena", ylim=c(0,55) ,  las=1 , col="slateblue1", breaks=20)
hist(total_score[podaci$race.ethnicity  == "group D"] , main="" , xlim=c(0,100), ylab="group D", xlab="Prosecan broj poena", ylim=c(0,50) ,  las=1 , col="slateblue1", breaks=20)
hist(total_score[podaci$race.ethnicity  == "group E"] , main="" , xlim=c(0,100), ylab="group E", xlab="Prosecan broj poena", ylim=c(0,25) ,  las=1 , col="slateblue1", breaks=20)

par(mfrow=c(2,1))

#Make the plot
par(mar=c(0,5,3,3))
hist(total_score[podaci$race.ethnicity  == "group A"] , main="" , xlim=c(0,100), ylab="group A", xlab="", xaxt ="n", ylim=c(0,15), las=1 , col="slateblue1", breaks=20)
par(mar=c(5,5,0,3))
hist(total_score[podaci$race.ethnicity  == "group E"] , main="" , xlim=c(0,100), ylab="group E", xlab="Prosecan broj poena", ylim=c(25,0) ,  las=1 , col="tomato3", breaks=20)

par(mfrow=c(1,1))
hist(total_score[podaci$parental.level.of.education  == "bachelor's degree"] , main="" , xlim=c(0,100), ylab="bachelor's degree", xlab="Prosecan broj poena", ylim=c(0,25), las=1 , col="slateblue1", breaks=20)
hist(total_score[podaci$parental.level.of.education  == "some college"] , main="" , xlim=c(0,100), ylab="some college", xlab="Prosecan broj poena", ylim=c(0,35) , las=1 , col="slateblue1", breaks=20)
hist(total_score[podaci$parental.level.of.education  == "master's degree"] , main="" , xlim=c(0,100), ylab="master's degree", xlab="Prosecan broj poena", ylim=c(0,8) ,  las=1 , col="slateblue1", breaks=20)
hist(total_score[podaci$parental.level.of.education  == "associate's degree"] , main="" , xlim=c(0,100), ylab="associate's degree", xlab="Prosecan broj poena", ylim=c(0,50) ,  las=1 , col="slateblue1", breaks=20)
hist(total_score[podaci$parental.level.of.education  == "high school"] , main="" , xlim=c(0,100), ylab="high school", xlab="Prosecan broj poena", ylim=c(0,40) ,  las=1 , col="slateblue1", breaks=20)
hist(total_score[podaci$parental.level.of.education  == "some high school"] , main="" , xlim=c(0,100), ylab="some high school", xlab="Prosecan broj poena", ylim=c(0,30) ,  las=1 , col="slateblue1", breaks=20)


par(mfrow=c(2,1))
par(mar=c(0,5,3,3))
hist(total_score[podaci$parental.level.of.education  == "bachelor's degree"] , main="" , xlim=c(0,100), ylab="bachelor's degree", xlab = "", xaxt = "n", ylim=c(0,25), las=1 , col="slateblue1", breaks=20)
par(mar=c(5,5,0,3))
hist(total_score[podaci$parental.level.of.education  == "high school"] , main="" , xlim=c(0,100), ylab="high school", xlab="Prosecan broj poena", ylim=c(40,0) ,  las=1 , col="tomato3", breaks=20)

par(mfrow=c(2,1))
par(mar=c(0,5,3,3))
hist(total_score[podaci$test.preparation.course  == "completed"] , main="" , xlim=c(0,100), ylab="completed", xlab = "", xaxt = "n", ylim=c(0,70), las=1 , col="slateblue1", breaks=20)
par(mar=c(5,5,0,3))
hist(total_score[podaci$test.preparation.course  == "none"] , main="" , xlim=c(0,100), ylab="none", xlab="Prosecan broj poena", ylim=c(100,0) ,  las=1 , col="tomato3", breaks=20)

hist(x = podaci$math.score,col="slateblue1", breaks=100)
abline(v = mean(podaci$math.score), col = "tomato3", lwd = 2)
hist(x = podaci$writing.score,col="slateblue1", breaks=100)
abline(v = mean(podaci$writing.score), col = "tomato3", lwd = 2)
hist(x = podaci$reading.score,col="slateblue1", breaks=100)
abline(v = mean(podaci$reading.score), col = "tomato3", lwd = 2)
hist(x = total_score ,col="slateblue1", breaks=100)
abline(v = mean(total_score), col = "tomato3", lwd = 2)

# prost slucajan uzorak

n = 278 # velicina uzorka
N = length(podaci$gender)
set.seed(42)
s = sample(N, n)
uzorak = podaci[s,]
uzorak

m_ocena_mat = mean(uzorak$math.score)
m_ocena_read = mean(uzorak$reading.score)
m_ocena_writ = mean(uzorak$writing.score)
m_ocena_tot = mean(uzorak$total)

mean(podaci$math.score)
m_ocena_mat

mean(podaci$writing.score)
m_ocena_writ
mean(podaci$reading.score)
m_ocena_read
mean(podaci$total)
#ocena totala
m_ocena_tot

# ocena disperzije
s_2h = var(uzorak$total)
d_ocena_tot = s_2h / n * (1 - n/N)

# 95% interval poverenja
alpha = 0.05
z = qnorm(1 - alpha/2)
interval = c(m_ocena_tot - z * sqrt(d_ocena_tot),
             m_ocena_tot + z * sqrt(d_ocena_tot) )
# writing score

# ocena disperzije
s_2h_writ = var(uzorak$writing.score)
d_ocena_writ = s_2h_writ / n * (1 - n/N)

# 95% interval poverenja
alpha = 0.05
z = qnorm(1 - alpha/2)
interval_writ = c(m_ocena_writ - z * sqrt(d_ocena_writ),
             m_ocena_writ + z * sqrt(d_ocena_writ) )
# math score

# ocena disperzije
s_2h_math = var(uzorak$math.score)
d_ocena_math = s_2h_math / n * (1 - n/N)

# 95% interval poverenja
alpha = 0.05
z = qnorm(1 - alpha/2)
interval_math = c(m_ocena_mat - z * sqrt(d_ocena_math),
             m_ocena_mat + z * sqrt(d_ocena_math) )
# reading score

# ocena disperzije
s_2h_read = var(uzorak$reading.score)
d_ocena_read = s_2h_read / n * (1 - n/N)

# 95% interval poverenja
alpha = 0.05
z = qnorm(1 - alpha/2)
interval_read = c(m_ocena_read - z * sqrt(d_ocena_read),
             m_ocena_read + z * sqrt(d_ocena_read) )


# stratifikovani slucajan uzorak - proporcionalni raspored
# pol
N_male = length(podaci[podaci$gender == "male",]$gender)
N_female = N - N_male
N_h_pol = c(N_male, N_female)
n_h_pol = N_h_pol * n / N
set.seed(42)
male = podaci[podaci$gender == "male", ][sample(1:N_male, n_h_pol[1]),]
male
set.seed(42)
female = podaci[podaci$gender == "female", ][sample(1:N_female, n_h_pol[2]),]
female

m_h_tot_pol = c(mean(male$total), mean(female$total))
t_h_tot_pol = sum(m_h_tot_pol * c(N_male, N_female))
m_h_tot_str_pol = t_h_tot_pol / N

s_2h_str_pol = c(var(male$total), var(female$total))
d_ocena_tot_str_pol = sum((N_h_pol/N)^2 * s_2h_str_pol/n_h_pol * ( 1 - n_h_pol/N_h_pol))

interval_str = c(m_h_tot_str_pol - z* sqrt(d_ocena_tot_str_pol),
                 m_h_tot_str_pol + z* sqrt(d_ocena_tot_str_pol))

#gender writing
m_h_writ_pol = c(mean(male$writing.score), mean(female$writing.score))
t_h_writ_pol = sum(m_h_writ_pol * c(N_male, N_female))
m_h_writ_str_pol = t_h_writ_pol / N

s_2h_str_pol_writ = c(var(male$writing.score), var(female$writing.score))
d_ocena_writ_str_pol = sum((N_h_pol/N)^2 * s_2h_str_pol_writ/n_h_pol * ( 1 - n_h_pol/N_h_pol))

interval_str_writ = c(m_h_writ_str_pol - z* sqrt(d_ocena_writ_str_pol),
                 m_h_writ_str_pol + z* sqrt(d_ocena_writ_str_pol))

# gender math
m_h_math_pol = c(mean(male$math.score), mean(female$math.score))
t_h_math_pol = sum(m_h_math_pol * c(N_male, N_female))
m_h_math_str_pol = t_h_math_pol / N

s_2h_str_pol_math = c(var(male$math.score), var(female$math.score))
d_ocena_math_str_pol = sum((N_h_pol/N)^2 * s_2h_str_pol_math/n_h_pol * ( 1 - n_h_pol/N_h_pol))

interval_str_math = c(m_h_math_str_pol - z* sqrt(d_ocena_math_str_pol),
                 m_h_math_str_pol + z* sqrt(d_ocena_math_str_pol))

# gender reading
m_h_read_pol = c(mean(male$reading.score), mean(female$reading.score))
t_h_read_pol = sum(m_h_read_pol * c(N_male, N_female))
m_h_read_str_pol = t_h_read_pol / N

s_2h_str_pol_read = c(var(male$reading.score), var(female$reading.score))
d_ocena_read_str_pol = sum((N_h_pol/N)^2 * s_2h_str_pol_read/n_h_pol * ( 1 - n_h_pol/N_h_pol))

interval_str_read = c(m_h_read_str_pol - z* sqrt(d_ocena_read_str_pol),
                 m_h_read_str_pol + z* sqrt(d_ocena_read_str_pol))

# obrok


N_standard = length(podaci[podaci$lunch == "standard",]$lunch)
N_reduced = N - N_standard
N_h_lunch = c(N_standard, N_reduced)
n_h_lunch = round(N_h_lunch * n / N)
set.seed(42)
standard = podaci[podaci$lunch == "standard", ][sample(1:N_standard, n_h_lunch[1]),]
standard
set.seed(42)
reduced = podaci[podaci$lunch == "free/reduced", ][sample(1:N_reduced, n_h_lunch[2]),]
reduced
# lunch total
m_h_tot_lunch = c(mean(standard$total), mean(reduced$total))
t_h_tot_lunch = sum(m_h_tot_lunch * c(N_standard, N_reduced))
m_h_tot_str_lunch = t_h_tot_lunch / N

s_2h_str_lunch = c(var(standard$total), var(reduced$total))
d_ocena_tot_str_lunch = sum((N_h_lunch/N)^2 * s_2h_str_lunch/n_h_lunch * ( 1 - n_h_lunch/N_h_lunch))

interval_str_lunch = c(m_h_tot_str_lunch - z* sqrt(d_ocena_tot_str_lunch),
                 m_h_tot_str_lunch + z* sqrt(d_ocena_tot_str_lunch))

# lunch math
m_h_math_lunch = c(mean(standard$math.score), mean(reduced$math.score))
t_h_math_lunch = sum(m_h_math_lunch * c(N_standard, N_reduced))
m_h_math_str_lunch = t_h_math_lunch / N

s_2h_str_lunch_math = c(var(standard$math.score), var(reduced$math.score))
d_ocena_math_str_lunch = sum((N_h_lunch/N)^2 * s_2h_str_lunch_math/n_h_lunch * ( 1 - n_h_lunch/N_h_lunch))

interval_str_lunch_math = c(m_h_math_str_lunch - z* sqrt(d_ocena_math_str_lunch),
                       m_h_math_str_lunch + z* sqrt(d_ocena_math_str_lunch))

# lunch reading

m_h_read_lunch = c(mean(standard$reading.score), mean(reduced$reading.score))
t_h_read_lunch = sum(m_h_read_lunch * c(N_standard, N_reduced))
m_h_read_str_lunch = t_h_read_lunch / N

s_2h_str_lunch_read = c(var(standard$reading.score), var(reduced$reading.score))
d_ocena_read_str_lunch = sum((N_h_lunch/N)^2 * s_2h_str_lunch_read/n_h_lunch * ( 1 - n_h_lunch/N_h_lunch))

interval_str_lunch_read = c(m_h_read_str_lunch - z* sqrt(d_ocena_read_str_lunch),
                       m_h_read_str_lunch + z* sqrt(d_ocena_read_str_lunch))
# lunch writing

m_h_writ_lunch = c(mean(standard$writing.score), mean(reduced$writing.score))
t_h_writ_lunch = sum(m_h_writ_lunch * c(N_standard, N_reduced))
m_h_writ_str_lunch = t_h_writ_lunch / N

s_2h_str_lunch_writ = c(var(standard$writing.score), var(reduced$writing.score))
d_ocena_writ_str_lunch = sum((N_h_lunch/N)^2 * s_2h_str_lunch_writ/n_h_lunch * ( 1 - n_h_lunch/N_h_lunch))

interval_str_lunch_writ = c(m_h_writ_str_lunch - z* sqrt(d_ocena_writ_str_lunch),
                       m_h_writ_str_lunch + z* sqrt(d_ocena_writ_str_lunch))
#rasna pripadnost

N_group_A = length(podaci[podaci$race.ethnicity == "group A",]$race.ethnicity)
N_group_B = length(podaci[podaci$race.ethnicity == "group B",]$race.ethnicity)
N_group_C = length(podaci[podaci$race.ethnicity == "group C",]$race.ethnicity)
N_group_D = length(podaci[podaci$race.ethnicity == "group D",]$race.ethnicity)
N_group_E = length(podaci[podaci$race.ethnicity == "group E",]$race.ethnicity)
N_h_rasa = c(N_group_A, N_group_B, N_group_C, N_group_D, N_group_E)
n_h_rasa = round(N_h_rasa * n / N)
set.seed(42)
group_A = podaci[podaci$race.ethnicity == "group A", ][sample(1:N_group_A, n_h_rasa[1]),]
group_A
set.seed(42)
group_B = podaci[podaci$race.ethnicity == "group B", ][sample(1:N_group_B, n_h_rasa[2]),]
group_B
set.seed(42)
group_C = podaci[podaci$race.ethnicity == "group C", ][sample(1:N_group_C, n_h_rasa[3]),]
group_C
set.seed(42)
group_D = podaci[podaci$race.ethnicity == "group D", ][sample(1:N_group_D, n_h_rasa[4]),]
group_D
set.seed(42)
group_E = podaci[podaci$race.ethnicity == "group E", ][sample(1:N_group_E, n_h_rasa[5]),]
group_E
# race total
m_h_tot_rasa = c(mean(group_A$total),mean(group_B$total),mean(group_C$total),mean(group_D$total),mean(group_E$total))
t_h_tot_rasa = sum(m_h_tot_rasa * N_h_rasa)
m_h_tot_str_rasa = t_h_tot_rasa / N

s_2h_str_rasa = c(var(group_A$total),var(group_B$total),var(group_C$total),var(group_D$total),var(group_E$total))
d_ocena_tot_str_rasa = sum((N_h_rasa/N)^2 * s_2h_str_rasa/n_h_rasa * ( 1 - n_h_rasa/N_h_rasa))

interval_str_rasa = c(m_h_tot_str_rasa - z* sqrt(d_ocena_tot_str_rasa),
                 m_h_tot_str_rasa + z* sqrt(d_ocena_tot_str_rasa))



# rasa math
m_h_math_rasa = c(mean(group_A$math.score),mean(group_B$math.score),mean(group_C$math.score),mean(group_D$math.score),mean(group_E$math.score))
t_h_math_rasa = sum(m_h_math_rasa * N_h_rasa)
m_h_math_str_rasa = t_h_math_rasa / N

s_2h_str_rasa_math = c(var(group_A$math.score),var(group_B$math.score),var(group_C$math.score),var(group_D$math.score),var(group_E$math.score))
d_ocena_math_str_rasa = sum((N_h_rasa/N)^2 * s_2h_str_rasa_math/n_h_rasa * ( 1 - n_h_rasa/N_h_rasa))

interval_str_rasa_math = c(m_h_math_str_rasa - z* sqrt(d_ocena_math_str_rasa),
                            m_h_math_str_rasa + z* sqrt(d_ocena_math_str_rasa))

# rasa reading

m_h_read_rasa =  c(mean(group_A$reading.score),mean(group_B$reading.score),mean(group_C$reading.score),mean(group_D$reading.score),mean(group_E$reading.score))
t_h_read_rasa = sum(m_h_read_rasa * N_h_rasa)
m_h_read_str_rasa = t_h_read_rasa / N

s_2h_str_rasa_read = c(var(group_A$reading.score),var(group_B$reading.score),var(group_C$reading.score),var(group_D$reading.score),var(group_E$reading.score))
d_ocena_read_str_rasa = sum((N_h_rasa/N)^2 * s_2h_str_rasa_read/n_h_rasa * ( 1 - n_h_rasa/N_h_rasa))

interval_str_rasa_read = c(m_h_read_str_rasa - z* sqrt(d_ocena_read_str_rasa),
                            m_h_read_str_rasa + z* sqrt(d_ocena_read_str_rasa))
# rasa writing

m_h_writ_rasa = c(mean(group_A$writing.score),mean(group_B$writing.score),mean(group_C$writing.score),mean(group_D$writing.score),mean(group_E$writing.score))
t_h_writ_rasa = sum(m_h_writ_rasa * N_h_rasa)
m_h_writ_str_rasa = t_h_writ_rasa / N

s_2h_str_rasa_writ = c(var(group_A$writing.score),var(group_B$writing.score),var(group_C$writing.score),var(group_D$writing.score),var(group_E$writing.score))
d_ocena_writ_str_rasa = sum((N_h_rasa/N)^2 * s_2h_str_rasa_writ/n_h_rasa * ( 1 - n_h_rasa/N_h_rasa))

interval_str_rasa_writ = c(m_h_writ_str_rasa - z* sqrt(d_ocena_writ_str_rasa),
                            m_h_writ_str_rasa + z* sqrt(d_ocena_writ_str_rasa))

#stepen obrazovanja

N_high_school = length(podaci[podaci$parental.level.of.education == "high school",]$parental.level.of.education)
N_some_high_school = length(podaci[podaci$parental.level.of.education == "some high school",]$parental.level.of.education)
N_bachelors_degree = length(podaci[podaci$parental.level.of.education == "bachelor's degree",]$parental.level.of.education)
N_masters_degree = length(podaci[podaci$parental.level.of.education == "master's degree",]$parental.level.of.education)
N_associates_degree = length(podaci[podaci$parental.level.of.education == "associate's degree",]$parental.level.of.education)
N_some_college = length(podaci[podaci$parental.level.of.education == "some college",]$parental.level.of.education)
N_h_obrazovanje = c(N_high_school, N_some_high_school, N_bachelors_degree, N_masters_degree, N_associates_degree, N_some_college)
n_h_obrazovanje = round(N_h_obrazovanje * n / N)
set.seed(42)
high_school = podaci[podaci$parental.level.of.education == "high school", ][sample(1:N_high_school, n_h_obrazovanje[1]),]
high_school
set.seed(42)
some_high_school = podaci[podaci$parental.level.of.education == "some high school", ][sample(1:N_some_high_school, n_h_obrazovanje[2]),]
some_high_school
set.seed(42)
bachelors_degree = podaci[podaci$parental.level.of.education == "bachelor's degree", ][sample(1:N_bachelors_degree, n_h_obrazovanje[3]),]
bachelors_degree
set.seed(42)
masters_degree = podaci[podaci$parental.level.of.education == "master's degree", ][sample(1:N_masters_degree, n_h_obrazovanje[4]),]
masters_degree
set.seed(42)
associates_degree = podaci[podaci$parental.level.of.education == "associate's degree", ][sample(1:N_associates_degree, n_h_obrazovanje[5]),]
associates_degree
set.seed(42)
some_college = podaci[podaci$parental.level.of.education == "some college", ][sample(1:N_some_college, n_h_obrazovanje[6]),]
some_college

# obrazovanje total
m_h_tot_obrazovanje = c(mean(high_school$total),mean(some_high_school$total),mean(bachelors_degree$total),mean(masters_degree$total),mean(associates_degree$total), mean(some_college$total))
t_h_tot_obrazovanje = sum(m_h_tot_obrazovanje * N_h_obrazovanje)
m_h_tot_str_obrazovanje = t_h_tot_obrazovanje / N

s_2h_str_obrazovanje = c(var(high_school$total),var(some_high_school$total),var(bachelors_degree$total),var(masters_degree$total),var(associates_degree$total), var(some_college$total))
d_ocena_tot_str_obrazovanje = sum((N_h_obrazovanje/N)^2 * s_2h_str_obrazovanje/n_h_obrazovanje * ( 1 - n_h_obrazovanje/N_h_obrazovanje))

interval_str_obrazovanje = c(m_h_tot_str_obrazovanje - z* sqrt(d_ocena_tot_str_obrazovanje),
                      m_h_tot_str_obrazovanje + z* sqrt(d_ocena_tot_str_obrazovanje))



# obrazovanje math
m_h_math_obrazovanje = c(mean(high_school$math),mean(some_high_school$math),mean(bachelors_degree$math),mean(masters_degree$math),mean(associates_degree$math), mean(some_college$math))
t_h_math_obrazovanje = sum(m_h_math_obrazovanje * N_h_obrazovanje)
m_h_math_str_obrazovanje = t_h_math_obrazovanje / N

s_2h_str_obrazovanje_math = c(var(high_school$math),var(some_high_school$math),var(bachelors_degree$math),var(masters_degree$math),var(associates_degree$math), var(some_college$math))
d_ocena_math_str_obrazovanje = sum((N_h_obrazovanje/N)^2 * s_2h_str_obrazovanje_math/n_h_obrazovanje * ( 1 - n_h_obrazovanje/N_h_obrazovanje))

interval_str_obrazovanje_math = c(m_h_math_str_obrazovanje - z* sqrt(d_ocena_math_str_obrazovanje),
                           m_h_math_str_obrazovanje + z* sqrt(d_ocena_math_str_obrazovanje))

# obrazovanje reading

m_h_read_obrazovanje =  c(mean(high_school$reading.score),mean(some_high_school$reading.score),mean(bachelors_degree$reading.score),mean(masters_degree$reading.score),mean(associates_degree$reading.score), mean(some_college$reading.score))
t_h_read_obrazovanje = sum(m_h_read_obrazovanje * N_h_obrazovanje)
m_h_read_str_obrazovanje = t_h_read_obrazovanje / N

s_2h_str_obrazovanje_read = c(var(high_school$reading.score),var(some_high_school$reading.score),var(bachelors_degree$reading.score),var(masters_degree$reading.score),var(associates_degree$reading.score), var(some_college$reading.score))
d_ocena_read_str_obrazovanje = sum((N_h_obrazovanje/N)^2 * s_2h_str_obrazovanje_read/n_h_obrazovanje * ( 1 - n_h_obrazovanje/N_h_obrazovanje))

interval_str_obrazovanje_read = c(m_h_read_str_obrazovanje - z* sqrt(d_ocena_read_str_obrazovanje),
                           m_h_read_str_obrazovanje + z* sqrt(d_ocena_read_str_obrazovanje))
# obrazovanje writing

m_h_writ_obrazovanje = c(mean(high_school$writing.score),mean(some_high_school$writing.score),mean(bachelors_degree$writing.score),mean(masters_degree$writing.score),mean(associates_degree$writing.score), mean(some_college$writing.score))
t_h_writ_obrazovanje = sum(m_h_writ_obrazovanje * N_h_obrazovanje)
m_h_writ_str_obrazovanje = t_h_writ_obrazovanje / N

s_2h_str_obrazovanje_writ = c(var(high_school$writing.score),var(some_high_school$writing.score),var(bachelors_degree$writing.score),var(masters_degree$writing.score),var(associates_degree$writing.score), var(some_college$writing.score))
d_ocena_writ_str_obrazovanje = sum((N_h_obrazovanje/N)^2 * s_2h_str_obrazovanje_writ/n_h_obrazovanje * ( 1 - n_h_obrazovanje/N_h_obrazovanje))

interval_str_obrazovanje_writ = c(m_h_writ_str_obrazovanje - z* sqrt(d_ocena_writ_str_obrazovanje),
                           m_h_writ_str_obrazovanje + z* sqrt(d_ocena_writ_str_obrazovanje))


# probni test


N_completed = length(podaci[podaci$test.preparation.course == "completed",]$test.preparation.course)
N_none = N - N_completed
N_h_probni_test = c(N_completed, N_none)
n_h_probni_test = round(N_h_probni_test * n / N)
set.seed(42)
completed = podaci[podaci$test.preparation.course == "completed", ][sample(1:N_completed, n_h_probni_test[1]),]
completed
set.seed(42)
none = podaci[podaci$test.preparation.course == "none", ][sample(1:N_none, n_h_probni_test[2]),]
none
# probni_test tot
m_h_tot_probni_test = c(mean(completed$total), mean(none$total))
t_h_tot_probni_test = sum(m_h_tot_probni_test * c(N_completed, N_none))
m_h_tot_str_probni_test = t_h_tot_probni_test / N

s_2h_str_probni_test = c(var(completed$total), var(none$total))
d_ocena_tot_str_probni_test = sum((N_h_probni_test/N)^2 * s_2h_str_probni_test/n_h_probni_test * ( 1 - n_h_probni_test/N_h_probni_test))

interval_str_probni_test = c(m_h_tot_str_probni_test - z* sqrt(d_ocena_tot_str_probni_test),
                 m_h_tot_str_probni_test + z* sqrt(d_ocena_tot_str_probni_test))

# probni_test math
m_h_math_probni_test = c(mean(completed$math.score), mean(none$math.score))
t_h_math_probni_test = sum(m_h_math_probni_test * c(N_completed, N_none))
m_h_math_str_probni_test = t_h_math_probni_test / N

s_2h_str_probni_test_math = c(var(completed$math.score), var(none$math.score))
d_ocena_math_str_probni_test = sum((N_h_probni_test/N)^2 * s_2h_str_probni_test_math/n_h_probni_test * ( 1 - n_h_probni_test/N_h_probni_test))

interval_str_probni_test_math = c(m_h_math_str_probni_test - z* sqrt(d_ocena_math_str_probni_test),
                            m_h_math_str_probni_test + z* sqrt(d_ocena_math_str_probni_test))

# probni_test reading

m_h_read_probni_test = c(mean(completed$reading.score), mean(none$reading.score))
t_h_read_probni_test = sum(m_h_read_probni_test * c(N_completed, N_none))
m_h_read_str_probni_test = t_h_read_probni_test / N

s_2h_str_probni_test_read = c(var(completed$reading.score), var(none$reading.score))
d_ocena_read_str_probni_test = sum((N_h_probni_test/N)^2 * s_2h_str_probni_test_read/n_h_probni_test * ( 1 - n_h_probni_test/N_h_probni_test))

interval_str_probni_test_read = c(m_h_read_str_probni_test - z* sqrt(d_ocena_read_str_probni_test),
                            m_h_read_str_probni_test + z* sqrt(d_ocena_read_str_probni_test))
# probni_test writing

m_h_writ_probni_test = c(mean(completed$writing.score), mean(none$writing.score))
t_h_writ_probni_test = sum(m_h_writ_probni_test * c(N_completed, N_none))
m_h_writ_str_probni_test = t_h_writ_probni_test / N

s_2h_str_probni_test_writ = c(var(completed$writing.score), var(none$writing.score))
d_ocena_writ_str_probni_test = sum((N_h_probni_test/N)^2 * s_2h_str_probni_test_writ/n_h_probni_test * ( 1 - n_h_probni_test/N_h_probni_test))

interval_str_probni_test_writ = c(m_h_writ_str_probni_test - z* sqrt(d_ocena_writ_str_probni_test),
                            m_h_writ_str_probni_test + z* sqrt(d_ocena_writ_str_probni_test))


#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################

# OCENE MATH #
m_ocena_mat
m_h_math_str_lunch
m_h_math_str_obrazovanje
m_h_math_str_pol
m_h_math_str_probni_test
m_h_math_str_rasa


d_ocena_math
d_ocena_math_str_lunch
d_ocena_math_str_obrazovanje
d_ocena_math_str_pol
d_ocena_math_str_probni_test
d_ocena_math_str_rasa

interval_math
interval_str_lunch_math
interval_str_obrazovanje_math
interval_str_math
interval_str_probni_test_math
interval_str_rasa_math

# OCENE READ #
m_ocena_read
m_h_read_str_lunch
m_h_read_str_obrazovanje
m_h_read_str_pol
m_h_read_str_probni_test
m_h_read_str_rasa


d_ocena_read
d_ocena_read_str_lunch
d_ocena_read_str_obrazovanje
d_ocena_read_str_pol
d_ocena_read_str_probni_test
d_ocena_read_str_rasa

interval_read
interval_str_lunch_read
interval_str_obrazovanje_read
interval_str_read
interval_str_probni_test_read
interval_str_rasa_read
# OCENE WRIT #

m_ocena_writ
m_h_writ_str_lunch
m_h_writ_str_obrazovanje
m_h_writ_str_pol
m_h_writ_str_probni_test
m_h_writ_str_rasa


d_ocena_writ
d_ocena_writ_str_lunch
d_ocena_writ_str_obrazovanje
d_ocena_writ_str_pol
d_ocena_writ_str_probni_test
d_ocena_writ_str_rasa

interval_writ
interval_str_lunch_writ
interval_str_obrazovanje_writ
interval_str_writ
interval_str_probni_test_writ
interval_str_rasa_writ
# OCENE TOT #

m_ocena_tot
m_h_tot_str_lunch
m_h_tot_str_obrazovanje
m_h_tot_str_pol
m_h_tot_str_probni_test
m_h_tot_str_rasa


d_ocena_tot
d_ocena_tot_str_lunch
d_ocena_tot_str_obrazovanje
d_ocena_tot_str_pol
d_ocena_tot_str_probni_test
d_ocena_tot_str_rasa

interval
interval_str_lunch
interval_str_obrazovanje
interval_str
interval_str_probni_test
interval_str_rasa