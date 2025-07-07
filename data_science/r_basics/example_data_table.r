# install the data.table package before you use it!
install.packages("data.table")

# load data.table package
library(data.table)

# load other packages and datasets
library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)

# convert the data frame into a data.table object
murders <- setDT(murders)

# selecting in dplyr
select(murders, state, region)

# selecting in data.table - 2 methods
murders[, c("state", "region")] |> head()
murders[, .(state, region)] |> head()

# adding or changing a column in dplyr
murders <- mutate(murders, rate = total / population * 10^5)

# adding or changing a column in data.table
murders[, rate := total / population * 100000]
head(murders)
murders[, ":="(rate = total / population * 100000, rank = rank(population))]

# y is referring to x and := changes by reference
x <- data.table(a = 1)
y <- x

x[,a := 2]
y

y[,a := 1]
x

# use copy to make an actual copy
x <- data.table(a = 1)
y <- copy(x)
x[,a := 2]
y
