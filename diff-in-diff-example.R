# Kevin Goulding
# ECNS 562 - Assignment 2
 
##########################################################################
# Load the foreign package
require(foreign)

# Import data from web site
eitc = read.dta("http://www.montana.edu/econ/cstoddard/562/eitc.dta")

# Import data from your hard drive
#eitc = read.dta("C:\DATA\Courses\Econ 562\homework\eitc.dta")

# load sumstats function
source('sumstats.r')

sapply(eitc,class)
summary(eitc)
sumstats(eitc)

require(xtable)                   # xtable package helps create LaTeX code from R.
xtable(sumstats(eitc))


# The following code utilizes the sumstats function:
sumstats(eitc[eitc$children == 0, ])
sumstats(eitc[eitc$children == 1, ])
sumstats(eitc[eitc$children >= 1, ])
sumstats(eitc[eitc$children >= 1 & eitc$year == 1994, ])

# Alternately, you can use the built-in summary function
summary(eitc[eitc$children == 0, ])
summary(eitc[eitc$children == 1, ])
summary(eitc[eitc$children >= 1, ])
summary(eitc[eitc$children >= 1 & eitc$year == 1994, ])

# Another example: Summarize variable 'work' for women with one child from 1993 onwards.
summary(subset(eitc, year >= 1993 & children == 1, select=work))


mean(eitc[eitc$children == 0, 'work'])
mean(eitc[eitc$children == 1, 'work'])
mean(eitc[eitc$children >= 1, 'work'])



eitc$c.earn=eitc$earn*eitc$work
z = names(eitc)
X = as.data.frame(eitc$c.earn)
X[] = lapply(X, function(x){replace(x, x == 0, NA)})
eitc = cbind(eitc,X)
eitc$c.earn = NULL
names(eitc) = z



eitc$post93 = as.numeric(eitc$year >= 1994)
eitc$anykids = as.numeric(eitc$children > 0)


# Take average value of 'work' by year, conditional on anykids
minfo = aggregate(eitc$work, list(eitc$year,eitc$anykids == 1), mean)

# rename column headings (variables)
names(minfo) = c("YR","Treatment","LFPR")

# Attach a new column with labels
minfo$Group[1:6] = "Single women, no children"
minfo$Group[7:12] = "Single women, children"

minfo

require(ggplot2)	#package for creating nice plots

qplot(YR, LFPR, data=minfo, geom=c("point","line"), colour=Group,
        xlab="Year", ylab="Labor Force Participation Rate")
        
        
a = colMeans(subset(eitc, post93 == 0 & anykids == 0, select=work))
b = colMeans(subset(eitc, post93 == 0 & anykids == 1, select=work))
c = colMeans(subset(eitc, post93 == 1 & anykids == 0, select=work))
d = colMeans(subset(eitc, post93 == 1 & anykids == 1, select=work))
(d-c)-(b-a)


reg1 = lm(work ~ post93 + anykids + post93*anykids, data = eitc)
summary(reg1)


reg2 = lm(work ~ anykids + post93 + post93*anykids + nonwhite
                + age + I(age^2) + ed + finc + I(finc-earn), data = eitc)
summary(reg2)


# The state unemployment rate interacted with number of children
eitc$urate.int = eitc$urate*eitc$anykids

##
# Creating a new treatment term:

# First, we'll create a new dummy variable to distinguish between one child and 2+.
eitc$manykids = as.numeric(eitc$children >= 2)

# sub set the data, including only years before 1994.
eitc.sub = eitc[eitc$year <= 1993,]

# Create a new "after treatment" dummy variable
# and interaction term
eitc.sub$post91 = as.numeric(eitc.sub$year >= 1992)

# Run a placebo regression
reg3 <- lm(work ~ anykids + post91 + post91*anykids, data = eitc.sub)
summary(reg3)


