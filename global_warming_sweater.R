needs(tidyverse, magrittr, scales, zoo) # load dependencies

temp_data <- read.csv("Global_Land_Ocean_Index_NASA.csv") # import dataset

# area chart of the index
ggplot(temp_data, aes(Year, Temperature_index)) +
  geom_ribbon(aes(min = pmin(temp_data$Temperature_index,0), max = 0)) +
  geom_ribbon(aes(min = 0, max = pmax(temp_data$Temperature_index,0))) +
  theme_bw()

## knitting pattern ##
# we will convert the area chart into a tileplot where each tile is representing a knitting stitch
# for the pattern we have 73 rows of 80 columns for each side of the sweater
columns = 80 
rows = 73 

# because we want to split the chart on front and back side, we define the years we want to display on each side
front <- seq(min(temp_data$Year), 
             length.out = length(unique(temp_data$Year))/2)
back <- seq(max(front)+1, 
            length.out = (length(unique(temp_data$Year))/2)-1)

# pattern for front #
# first we project the temperature index on a scale from 1 to the number of stiches we have for the pattern
# we do this for all years so that the scale on both sweater sides will be the same, then we filter
tcolumn <- data.frame(Year = temp_data$Year, 
                      trow = round(rescale(temp_data$Temperature_index, to = c(1, rows)))) %>%
  filter(Year %in% front)
# now we rescale the years for the front
trow <- data.frame(Year = temp_data$Year[temp_data$Year %in% front], 
                   tcolumn = round(rescale(temp_data$Year[temp_data$Year %in% front], to = c(1, columns))))

# now we match the rescaled data
transformed <- left_join(trow, tcolumn) 

# rescaling to an integer scale always comes with little inaccuracies, we have to live with those
# there are more rows than years, therefore we have years that will be represented by two stitches instead of only one
# we now add these missing rows to the data set and fill them with the value of the previous 
transformed <- left_join(data.frame(tcolumn = 1:max(transformed$tcolumn)), transformed)
transformed$tcolumn <-  na.locf(transformed$tcolumn, fromLast = TRUE)
transformed$trow <-  na.locf(transformed$trow, fromLast = TRUE)
# we now have a data set that tells us how many knitted stitches are supposed to be colored for every column in order to create the plot

# now for the tile plot data set: for the length we want the number of rows, and for every stitch in a row an entry to represent the width
# we also add a column for the pattern we set to zero for now
grid <- data.frame(columns=rep(1:columns, rows), rows = rep(1:rows, each=columns), pattern=0) %>% 
  arrange(rows,columns)

# now we combine the grid data set with the data that tells us how many stitches are to be colored
# for each row in the transformed data set this simple for-loop finds all the rows in the grid data set that matches the knitting row number and has a column number between one and the stitch number that need to be colored, then overwrites the zero in the pattern column to one
for(i in 1:nrow(transformed)){
  grid$pattern[grid$columns %in% transformed$tcolumn[i] & grid$rows %in% 1:transformed$trow[i]] <- 1
}

# for this chart, row 24 is our zero line: every rescaled temperature index value that is above 24 equals an increase of temperature realtive to the base line, every value below 24 equals a decrease
# therefore we want the pattern for the rows 1 to 24 to be inverted, meaning the zeros should be ones and the ones zeros
grid$pattern[grid$rows <= 24] <- recode(grid$pattern[grid$rows <= 24], `0`=1, `1`=0) 

# last but not least, we add a new row with the column label we want to plot onto the grids
# actually we could just use the column number for this, but there is a little hurdle: when knitting you add row by row like a printer, you start at column one and go through to the last one, and then you'll add a new stitch on the last column which now becomes your first for the new row and knit you way "backwards" to column one of the first row
# this is why we have to reverse the label for every second row:
grid$label <- grid$columns

is.even <- function(x) x %% 2 == 0 # define even numbers
grid[is.even(grid$rows),] <- grid %>% filter(is.even(rows)) %>% group_by(rows) %>% mutate(label = rev(label))

# You don't have to do this because it adds even more inaccuracies to the sweater. There may be years where the rescaled index can't be interpreted as a mesh because it's value is too small, thus we'll have "holes" in our pattern. 
# To avoid this, we draw a colored line with the value zero = row 24 into the pattern.
grid$pattern[grid$rows %in% 24] <- 1 

# now for the plot
ggplot(grid, aes(x = columns, y = rows, fill = as.factor(pattern))) + 
  geom_tile(size = 0.5, color = "black") +
  coord_equal() +
  geom_text(aes(label = label)) +
  scale_y_continuous(breaks = seq(0, max(grid$rows), by = 1), expand=c(0,0), sec.axis = dup_axis()) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_text(size=12),
        axis.title.y=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=22)) +
  scale_fill_manual(values = c("#315265", "#FE503A")) +
  ggtitle("Winterly reminder of global warming - Front")

# save the plot as a pdf
ggsave("sweater_front.pdf", plot = last_plot(),
       width = 50, height = 56, units = "cm",
       dpi = 300, limitsize = TRUE, device = "pdf")


# now we simply do the same thing for the back

tcolumn <- data.frame(Year = temp_data$Year, 
                      trow = round(rescale(temp_data$Temperature_index, to = c(1, rows)))) %>%
  filter(Year %in% back)
trow <- data.frame(Year = temp_data$Year[temp_data$Year %in% back], 
                   tcolumn = round(rescale(temp_data$Year[temp_data$Year %in% back], to = c(1, columns))))

transformed <- left_join(trow, tcolumn) 

transformed <- left_join(data.frame(tcolumn = 1:max(transformed$tcolumn)), transformed)
transformed$tcolumn <-  na.locf(transformed$tcolumn, fromLast = TRUE)
transformed$trow <-  na.locf(transformed$trow, fromLast = TRUE)

grid <- data.frame(columns=rep(1:columns, rows), rows = rep(1:rows, each=columns), pattern=0) %>% 
  arrange(rows,columns)

for(i in 1:nrow(transformed)){
  grid$pattern[grid$columns %in% transformed$tcolumn[i] & grid$rows %in% 1:transformed$trow[i]] <- 1
}

grid$pattern[grid$rows <= 24] <- recode(grid$pattern[grid$rows <= 24], `0`=1, `1`=0) 
grid$label <- grid$columns

is.even <- function(x) x %% 2 == 0
grid[is.even(grid$rows),] <- grid %>% filter(is.even(rows)) %>% group_by(rows) %>% mutate(label = rev(label))

grid$pattern[grid$rows %in% 24] <- 1 #######

ggplot(grid, aes(x = columns, y = rows, fill = as.factor(pattern))) + 
  geom_tile(size = 0.5, color = "black") +
  coord_equal() +
  geom_text(aes(label = label)) +
  scale_y_continuous(breaks = seq(0, max(grid$rows), by = 1), expand=c(0,0), sec.axis = dup_axis()) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_text(size=12),
        axis.title.y=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=22)) +
  scale_fill_manual(values = c("#315265", "#FE503A")) +
  ggtitle("Winterly reminder of global warming - Back")

ggsave("sweater_back.pdf", plot = last_plot(),
       width = 50, height = 56, units = "cm",
       dpi = 300, limitsize = TRUE, device = "pdf")


