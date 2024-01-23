# %in%
animals %in% c("rat", "frog", "cat", "duck", "dog")
c("rat", "frog", "cat", "duck", "dog") %in% animals 
animals[animals %in% c("rat", "frog", "cat", "duck", "dog")]
animals[c("rat", "frog", "cat", "duck", "dog") %in% animals]


heights <- c(2, 4, 4, NA, 6)
mean(heights)
mean(heights, na.rm=T)
max(heights, na.rm=T)

is.na(heights)
heights[!is.na(heights)]

na.omit(heights)

heights[complete.cases(heights)]

heights <- c(63, 69, 60, 65, NA, 68, 61, 70, 61, 59, 64, 69, 63)
mean(heights)
heights_no_na <- na.omit(heights)
heights_no_na <- heights[!is.na(heights)]

median (heights, na.rm=T)
heights_no_na[heights_no_na>67]
length(heights_no_na[heights_no_na>67])
