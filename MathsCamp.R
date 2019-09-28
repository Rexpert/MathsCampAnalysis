# setwd("~/R/Machine Learning/MathsCampAnalysis/")

library(pacman)
p_load(dplyr, ggplot2, tidyr, readxl, stringr, htmlwidgets, echarts4r)

## Fixing bug in saveWiget() in HTMLWidget library
saveWidgetFix <- function (widget,file,...) {
  ## A wrapper to saveWidget which compensates for arguable BUG in
  ## saveWidget which requires `file` to be in current working
  ## directory.
  wd<-getwd()
  on.exit(setwd(wd))
  outDir<-dirname(file)
  file<-basename(file)
  setwd(outDir);
  saveWidget(widget,file=file,...)
}

## Formatting the echarts default title
e_formatted_title <- function(e, title) {
  e %>%
    e_title(title, x = "center", textStyle = list(fontSize = 30)) %>%
    e_legend(y = "bottom")
}

# Objective:
# 1. Strategy/Pattern - Gambling 1-4
# 2. Strategy/Pattern - Millionaire
# 3. Come Again?

# 1a. difficulty of question ---------------------------------------------------
data <- read.csv("data/data.csv")
scores <- select(data, starts_with("Gambling"))

correct <- colSums(scores > 0)
wrong <- 10 - correct

# a <-
  data.frame(Game = colnames(scores), correct, wrong) %>%
  e_charts(Game) %>%
  e_bar(correct) %>%
  e_bar(wrong) %>%
  e_formatted_title("Statistics for Gambling Game")
# saveWidgetFix(a, file = "./static/gambling.html", selfcontained = F)


## ggplot approach
# tidyData <- data.frame(Game = colnames(scores), correct, wrong) %>%
#   gather("result", "freq", -Game)
# tidyData %>%
#   ggplot(aes(x = Game, y = freq, fill = result)) + 
#   geom_col(colour = "white", width = 0.7, position = "dodge") + 
#   scale_y_continuous(breaks = seq(from = 0, to = 10, by = 2)) + 
#   ggtitle("Statistics for Gambling Game") +
#   theme(plot.title = element_text(hjust = 0.5, size = 30))


# 1b. Gambling Game: confidence to gamble --------------------------------------
increase <- abs(scores) %>%
  mutate(first = Gambling2 - Gambling1, second = Gambling3 - Gambling2,
         third = Gambling4 - Gambling3) %>%
  select(-contains("Gambling"))

longData <-
  sapply(increase, function(x) {
    sapply(x, function(y){
      if(y > 0)
        y = "+"
      else if(y < 0)
        y = "-"
      else
        y = "0"
    })
  }) %>%
  cbind(scores) %>%
  as.data.frame.matrix() %>%
  select(-Gambling4) %>%
  mutate(Gambling1 = Gambling1 > 0, Gambling2 = Gambling2 > 0,
         Gambling3 = Gambling3 > 0)

long1 <- select(longData, prevGame = Gambling1, adjustment = first)
long2 <- select(longData, prevGame = Gambling2, adjustment = second)  
long3 <- select(longData, prevGame = Gambling3, adjustment = third)

tidyData <-
  rbind(long1, long2, long3) %>%
  group_by(prevGame, adjustment) %>%
  summarise(frequency = n()) %>%
  mutate(adjustment = factor(adjustment, levels = c("-", "0", "+"), 
                           labels = c("Decrease", "Constant", "Increase"))) %>%
  .[order(.$adjustment),] %>%
  filter(prevGame) %>%
  mutate(ymin = lag(cumsum(frequency), default = 0), 
         ymax = cumsum(frequency),  
         pos = cumsum(frequency)- frequency/2)

ggplot(tidyData) +
  geom_rect(aes(xmin = 2, xmax = 4, ymin = ymin, ymax = ymax, fill = adjustment), 
            colour = "white") +
  geom_text(aes(x= 3, y = pos, label = frequency), size = 5) +
  ggtitle("Reaction toward adjustment of Gambling Score\nwhen previous game is won") +
  coord_polar("y") +
  xlim(c(0,4)) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=20, face="bold", hjust = 0.5)
  )

# 2a. Milllionaire: Difficulty of Section --------------------------------------
data <- read.csv("data/millionaire.csv")
colnames(data)[1] <- "Group"
data$Group <- factor(data$Group, levels = data$Group)

scores <-
  mutate(data, Game1 = Bank1+Invest1, Game2 = Bank2+Invest2, 
         Game3 = Bank3+Invest3) %>%
  select(Group, contains("Game"))

# a <-
  scores %>%
  e_chart(Group) %>%
  e_bar(Game1) %>%
  e_bar(Game2) %>%
  e_bar(Game3) %>%
  e_formatted_title("Scores from Millionaire") %>%
  e_y_axis(min = 0, max = max(scores[,-1]) + 20)
# saveWidgetFix(a, file = "./static/scores.html", selfcontained = F)

# plotly approach
# g <- ggplot(scores) +
#   geom_col(aes(x = Group, y = Score, fill = Game), position = "dodge") + 
#   ggtitle("Scores from Millionaire") +
#   theme(plot.title = element_text(hjust = 0.5, size = 30))
# ggplotly(g) %>% 
#   config(displayModeBar = F) %>% 
#   layout(xaxis=list(fixedrange=TRUE)) %>% 
#   layout(yaxis=list(fixedrange=TRUE))
# saveWidgetFix(ggplotly(g), "output.html")

# 2b. Millionaire: strategy to gamble-------------------------------------------
clean <- function(df, i) {
  df <- switch (i, "1" = select(df, Group, Bank1, Invest1),
                "2" = select(df, Group, Bank2, Invest2),
                "3" = select(df, Group, Bank3, Invest3)
  )
  df %>%
    # The 0.00001 is the small trick to make the e_bar's animation more reliable
    mutate(Bank = df[,2] / (df[,2] + df[,3]) - 0.00001,
           Invest = Bank - 1 + 0.00001,
           gameround = i) %>%
    select(-2:-3)
}
  
gameround = 1
gamedata1 = clean(data, gameround)
gameround = 2
gamedata2 = clean(data, gameround)
gameround = 3
gamedata3 = clean(data, gameround)
gamedata = rbind(gamedata1, gamedata2, gamedata3)

# a <-
  gamedata %>%
  group_by(gameround) %>%
  e_charts(Group, timeline = T) %>%
  e_bar(Bank, stack = "grp") %>%
  e_bar(Invest, stack = "grp") %>%
  e_y_axis(min = -1, max = 1) %>%
  e_timeline_serie(title = list(
    # ===================================TRY DO.CALL===============================================
    
    list(text = "Money Allocation for Game 1", textStyle = list(fontSize = 30), x = "center"),
    list(text = "Money Allocation for Game 2", textStyle = list(fontSize = 30), x = "center"),
    list(text = "Money Allocation for Game 3", textStyle = list(fontSize = 30), x = "center")
  )) %>%
  e_timeline_opts(autoPlay = T)
# saveWidgetFix(a, file = "./static/moneyAllocation.html", selfcontained = F)

# Another Approach
# animation <-
#   ggplot(gamedata, aes(x = Group, y = Percentage, fill = Allocation)) +
#   geom_bar(stat = "identity", position = "identity") + 
#   labs(title = "Money Allocation for Game {closest_state}") +
#   theme(plot.title=element_text(size=20, hjust = 0.5)) +
#   transition_states(Gameround, wrap = F)
# 
# animate(animation, height = 400, width = 500)
# anim_save("MoneyAllocation.gif")

# Another Approach
# h <- ggplot(gamedata, aes(x = Group, y = Percentage, fill = Allocation)) +
#   geom_bar(stat = "identity", position = "identity", aes(frame = Gameround)) +
#   labs(title = "Money Allocation for Game {closest_state}") +
#   theme(plot.title=element_text(size=20, hjust = 0.5))
# ggplotly(h)

# 3. Will participant join again? ----------------------------------------------
cellrange <- c("B2:D44", "B2:D43", "B2:D14")
name2018 <- lapply(seq_along(cellrange), function(i){
  read_excel("./data/namelist2018.xlsx", sheet = i, range = cellrange[i], 
             col_names = c("name", "grade"), 
             col_types = c("text", "skip", "text"))
}) %>%
  purrr::reduce(full_join)

cellrange <- c("C4:E13", "C19:E29", "C34:E43", "C49:E58", "C64:E73", "C79:E88", 
               "C94:E103", "C109:E118", "C124:E134", "C139:E148")
name2019 <- lapply(cellrange, function(r){
  read_excel("./data/namelist2019.xlsx", sheet = 1, range = r, 
             col_names = c("name", "grade"), 
             col_types = c("text", "skip", "text"))
}) %>%
  purrr::reduce(full_join)

# check for extra white space in namelist
# sapply(name2019[,1], str_detect, "\\s+") %>% sum()

comeback <- sum(pull(name2019, name) %in% pull(name2018, name))

# Total number of participants who is not 6th grade in 2018
not6grade <- sapply(name2018[,2], str_extract, "[:digit:]") %>%
  str_detect(pattern = "6", negate = T) %>%
  sum()

percent <- round(comeback / not6grade, digits = 3)

n = 3 # number of wave, distribute the wave with uniformly increasing distance

m = n:1
# a <-
  data.frame(val = (2*m*n - m*(m-1)) / (n*(n+1)) * percent) %>% 
  e_charts() %>% 
  e_liquid(val)
# saveWidgetFix(a, "./static/liquid.html", selfcontained = F)

