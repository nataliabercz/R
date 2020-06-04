library(tidyverse)
library(stringr)
options(digits = 3)
options(scipen = 999)

x20  = 1:20
x100 = 1:100
y20_p05 = dbinom(x20, 20, 0.5)
dbin20_p05 = data.frame(y20_p05)

mean = mean(y20_p05)
sd = sd(y20_p05)
mean = 200*mean
sd = 35*sd

ggplot(dbin20_p05, aes(x20, y20_p05))+
  geom_histogram(stat = 'identity', color = 'black', fill = 'white')+
  stat_function(fun = dnorm, args = list(mean, sd), color = 'midnightblue')+
  labs(title = 'BINOMIAL AND NORMAL DISTRIBUTIONS: n = 20, p = 0.5')+
  xlim(1.5, 20)+xlab('X')+ylab('DENSITY')+theme_bw()

y100_p05 = dbinom(x100, 100, 0.5)
dbin100_p05 = data.frame(y100_p05)

mean = mean(y100_p05)
sd = sd(y100_p05)
mean = 5000*mean
sd = 230*sd

ggplot(dbin100_p05, aes(x100, y100_p05))+
  geom_histogram(stat = 'identity', color = 'black', fill = 'white')+
  stat_function(fun = dnorm, args = list(mean, sd), color = 'midnightblue')+
  labs(title = 'BINOMIAL AND NORMAL DISTRIBUTIONS: n = 100, p = 0.5')+
  xlim(25, 75)+xlab('X')+ylab('DENSITY')+theme_bw()

y20_p01 = dbinom(x20, 20, 0.1)
dbin20_p01 = data.frame(y20_p01)

mean = mean(y20_p01)
sd = sd(y20_p01)
mean = 45*mean
sd = 14.5*sd

ggplot(dbin20_p01, aes(x20, y20_p01))+
  geom_histogram(stat = 'identity', color = 'black', fill = 'white')+
  stat_function(fun = dnorm, args = list(mean, sd), color = 'midnightblue')+
  labs(title = 'BINOMIAL AND NORMAL DISTRIBUTIONS: n = 20, p = 0.1')+
  xlim(1.5, 20)+xlab('X')+ylab('DENSITY')+theme_bw()

y100_p01 = dbinom(x100, 100, 0.1)
dbin100_p01 = data.frame(y100_p01)

mean = mean(y100_p01)
sd = sd(y100_p01)
mean = 1000*mean
sd = 102*sd

ggplot(dbin100_p01, aes(x100, y100_p01))+
  geom_histogram(stat = 'identity', color = 'black', fill = 'white')+
  stat_function(fun = dnorm, args = list(mean, sd), color = 'midnightblue')+
  labs(title = 'BINOMIAL AND NORMAL DISTRIBUTIONS: n = 100, p = 0.1')+
  xlim(1.5, 36)+xlab('X')+ylab('DENSITY')+theme_bw()

confidence_interval_t2 = function(n){
  sample = rnorm(n, 0, 1)
  mean = mean(sample)
  sigma = sd(sample)
  qse = qnorm(0.975)*(sigma/sqrt(n))
  left = mean - qse
  right = mean + qse
  if (0 >= left & 0 <= right){
    contain = 1
  } else {
    contain = 0}
  return(c(left, right, contain))}

set.seed(1)
conf = confidence_interval_t2(100)
confid = round(conf, digits = 3)
cat('confidence interval:\n', '[',
    str_c(confid[1:2],c(', ', '')), ']', sep = '')

vec = c()
width100 = c()
set.seed(10)
for (i in 1:1000){
  con100 = confidence_interval_t2(100)
  width100 = c(width100, (con100[2] - con100[1]))
  if (con100[3] == 1){
    vec = c(vec, 1)
  } else {
    vec = c(vec, 0)}}

len = length(vec[vec == 1])/1000
cat(str_c('probability: ', len*100, '%', '\n'))

vec = c()
width200 = c()
set.seed(1)
for (i in 1:1000){
  con200 = confidence_interval_t2(200)
  width200 = c(width200, (con200[2] - con200[1]))
  if (con200[3] == 1){
    vec = c(vec, 1)
  } else {
    vec = c(vec, 0)}}
prob = length(vec[vec == 1])/length(vec)
mean_width = mean(width100) > mean(width200)
cat(str_c('probability: ', prob*100, '%', '\nis it wider? ',
          mean_width, '\n'), sep = '')

income = read.table('income.dat.txt')
colnames(income) = (c('nb', 'age', 'edu', 'gender', 'inc', 'job_cl'))
income = filter(income, inc >= 0)
inc = income$inc
U = sqrt(inc)

ggplot()+
  geom_histogram(aes(U), color = 'black', fill = 'white')+
  xlab('SQUARE ROOT OF THE INCOME')+ylab('NUMBER OF PEOPLE')+ggtitle('INCOME')+
  theme_bw()

miU = round(mean(U), digits = 3)
miD = round(mean(inc), digits = 3)
cat(str_c('µU: ', miU, '\n'), str_c('µD: ', miD, '\n'), sep = '')
sampU = sample(U, 200)
est_miU = round(mean(sampU), digits = 3)
sampD = sample(inc, 200)
est_miD = round(mean(sampD), digits = 3)
cat('estimators:\n',
    str_c('est_µU: ', est_miU, '\n'),
    str_c('est_µD: ', est_miD, '\n'), sep = '')

confidence_interval_t3 = function(sample, mean, mi){
  sd = sd(sample)
  qse = qt(0.975, length(sample)-1)*(sd/sqrt(length(sample)))
  left = mean - qse
  right = mean + qse
  conf = c(left, right)
  if (mi >= left & mi <= right){
    contain = 1
  } else {
    contain = 0
  }
  return(c(conf, contain))}

ciU = confidence_interval_t3(sampU, est_miU, miU)
ciD = confidence_interval_t3(sampD, est_miD, miD)
ciU = round(ciU, digits = 3)

if (ciU[3] == 1){
  ciU[3] = 'interval contains actual value'
} else {
  ciU[3] = 'interval doesn\'\t contain actual value'}
ciD = round(ciD, digits = 3)
if (ciD[3] == 1){
  ciD[3] = 'interval contains actual value'
} else {
  ciD[3] = 'interval doesn\'\t contain actual value'}

cat('confidence intervals:\nU: [',
    str_c(ciU[1:2], c(', ', '')), ']\n   ', ciU[3], '\nD: [',
    str_c(ciD[1:2], c(', ', '')), ']\n   ', ciD[3], '\n', sep = '')

vec_m_U = c()
vec_m_D = c()
contU = c()
contD = c()
set.seed(90)
for (i in 1:200){
  sampU = sample(U, 200)
  est_miU = mean(sampU)
  sampD = sample(inc, 200)
  est_miD = mean(sampD)
  conU = confidence_interval_t3(sampU, est_miU, miU)
  conD = confidence_interval_t3(sampD, est_miD, miD)
  vec_m_U = c(vec_m_U, est_miU)
  vec_m_D = c(vec_m_D, est_miD)
  if (conU[3] == 1){
    contU = c(contU, 1)
    if (conD[3] == 1){
      contD = c(contD, 1)
    }}}

ggplot()+geom_histogram(aes(vec_m_U, ..density..), color = 'black', fill = 'white')+
  xlab('ESTIMATOR µU')+ylab('DENSITY')+labs(title = 'DISTRIBUTION OF ESTIMATOR µU')+
  theme_bw()
ggplot()+geom_histogram(aes(vec_m_D, ..density..), color = 'black', fill = 'white')+
  xlab('ESTIMATOR µD')+ylab('DENSITY')+labs(title = 'DISTRIBUTION OF ESTIMATOR µD')+
  theme_bw()

probU = round(100*length(contU)/200, digits = 3)
probD = round(100*length(contD)/200, digits = 3)
cat(str_c('probability µU: ', probU, '%\n'),
    str_c('probability µD: ', probD, '%\n'), sep = '')

grades = read.table('grades.txt')
colnames(grades) = c('nb', 'gpa', 'iq', 'gender', 'pt')
iq = grades$iq
pt = grades$pt

confidence_interval_t4 = function(vec){
  mean = mean(vec)
  sd = sd(vec)
  qse = qt(0.975, length(vec)-1)*(sd/sqrt(length(vec)))
  left = mean - qse
  right = mean + qse
  conf = c(left, right)
  names(conf) = c('left', 'right')
  return(conf)}

con_iq = round(confidence_interval_t4(iq), digits = 3)
con_pt = round(confidence_interval_t4(pt), digits = 3)
cat('confidence intervals:\nIQ: [',
    str_c(con_iq, c(', ', '')), ']\nPT: [',
    str_c(con_pt, c(', ', '')), ']\n', sep = '')