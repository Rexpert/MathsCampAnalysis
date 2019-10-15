# setwd("~/R/Machine Learning/MathsCampAnalysis/")

library(pacman)
p_load(dplyr, readxl, stringr, htmlwidgets, echarts4r, purrr)

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
e_formatted_title <- function(e, title, show) {
  e %>%
    e_title(title, x = "center", textStyle = list(fontSize = 30)) %>%
    e_legend(show = show, y = "bottom")
}

saveLocal = F # save local or display in RStudio?

# Objective:
# 1. Strategy/Pattern - Gambling 1-4
# 2. Strategy/Pattern - Millionaire
# 3. Come Again?

# 1a. difficulty of question ---------------------------------------------------
data <- read.csv("data/data.csv")
scores <- select(data, starts_with("Gambling"))

correct <- colSums(scores > 0)
wrong <- 10 - correct

a <- data.frame(Game = colnames(scores), correct, wrong) %>%
  e_charts(Game) %>%
  e_bar(correct, barGap = "10%") %>%
  e_bar(wrong) %>%
  e_formatted_title("Statistics for Gambling Game", T)
if (saveLocal) {
  saveWidgetFix(a, file = "./static/gambling.html", selfcontained = F)
} else a

# 1b. Gambling Game: confidence to gamble --------------------------------------
longData <- abs(scores) %>%
  mutate(first = Gambling2 - Gambling1, second = Gambling3 - Gambling2,
         third = Gambling4 - Gambling3) %>%
  select(-contains("Gambling")) %>%
  sign() %>%
  cbind(scores[,-4]) %>%
  mutate(Gambling1 = Gambling1 > 0, Gambling2 = Gambling2 > 0,
         Gambling3 = Gambling3 > 0)

tidyData <- lapply(1:3, function(x) {
  select(longData, prevGame = x+3, adjustment = x)
}) %>% 
  reduce(rbind) %>%
  mutate(adjustment = factor(adjustment, labels = c("Decrease", "Constant", 
                                                    "Increase"))) %>%
  table() %>%
  t() %>%
  as.data.frame.matrix() %>%
  mutate(action = row.names(.)) %>%
  tidyr::gather("prevGame", "frequency", -action) %>%
  mutate(prevGame = if_else(as.logical(prevGame), "won", "lose"))

a <- tidyData %>%
  group_by(prevGame) %>%
  e_charts(action, timeline = T) %>%
  e_pie(frequency, radius = c("50%", "70%")) %>%
  e_formatted_title("Action Towards Results of Previous Gambling", F) %>%
  e_tooltip()
if (saveLocal) {
  saveWidgetFix(a, file = "./static/confidence.html", selfcontained = F)
} else a

# 2a. Milllionaire: Difficulty of Section --------------------------------------
data <- read.csv("data/millionaire.csv")
colnames(data)[1] <- "Group"
data$Group <- factor(data$Group, levels = data$Group)

scores <-
  mutate(data, Game1 = Bank1+Invest1, Game2 = Bank2+Invest2, 
         Game3 = Bank3+Invest3) %>%
  select(Group, contains("Game"))

a <- scores %>%
  e_chart(Group) %>%
  e_bar(Game1) %>%
  e_bar(Game2) %>%
  e_bar(Game3) %>%
  e_formatted_title("Scores from Millionaire", T) %>%
  e_y_axis(min = 0, max = max(scores[,-1]) + 20)
if (saveLocal) {
  saveWidgetFix(a, file = "./static/scores.html", selfcontained = F)
} else a

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

gamedata <- lapply(1:3, clean, df = data) %>%
  reduce(rbind)

a <- gamedata %>%
  group_by(gameround) %>%
  e_charts(Group, timeline = T) %>%
  e_bar(Bank, stack = "grp") %>%
  e_bar(Invest, stack = "grp") %>%
  e_y_axis(min = -1, max = 1) %>%
  e_timeline_serie(title = lapply(1:3, function (i) {
    list(text = paste("Money Allocation for Game", i), 
         textStyle = list(fontSize = 30), x = "center")
  })) %>%
  e_timeline_opts(autoPlay = T, padding = -12)
if (saveLocal) {
  saveWidgetFix(a, file = "./static/moneyAllocation.html", selfcontained = F)
} else a

# 3. Will participant join again? ----------------------------------------------
cellrange <- c("B2:D44", "B2:D43", "B2:D14")
name2018 <- lapply(seq_along(cellrange), function(i){
  read_excel("./data/namelist2018.xlsx", sheet = i, range = cellrange[i], 
             col_names = c("name", "grade"), 
             col_types = c("text", "skip", "text"))
}) %>%
  reduce(full_join)

cellrange <- c("C4:E13", "C19:E29", "C34:E43", "C49:E58", "C64:E73", "C79:E88", 
               "C94:E103", "C109:E118", "C124:E134", "C139:E148")
name2019 <- lapply(cellrange, function(r){
  read_excel("./data/namelist2019.xlsx", sheet = 1, range = r, 
             col_names = c("name", "grade"), 
             col_types = c("text", "skip", "text"))
}) %>%
  reduce(full_join)

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
a <- data.frame(val = (2*m*n - m*(m-1)) / (n*(n+1)) * percent) %>% 
  e_charts() %>% 
  e_liquid(val)
if (saveLocal) {
  saveWidgetFix(a, "./static/liquid.html", selfcontained = F)
} else a

# -------------------------------------------------- #
# --------- system call to tidy up widgets --------- #
# -------------------------------------------------- #
# check for both "python3" and "python" commands
# since some system will use "python3" as Python 3 command
# Note: sometimes "python" is used to represent Python 2,
#       this is true in some system that consists of both
#       Python 2 and Python 3. Therefore, an additional
#       check is needed for "python" command
system2Args <- list(
  python3 = list(command = "python3", args = "--version"),
  python =  list(command = "python", args = "--version")
)

system2WithStdout <- function(s = TRUE) partial(system2, stdout = s)

getPythonExitCodes <- function() lapply(system2Args, system2WithStdout(FALSE))

isPythonExists <- function() 0 %in% getPythonExitCodes()

isPython3 <- function(successCommand) {
  successCommandLength = length(successCommand)
  
  # Sanity check for zeroExitCode to have contents
  # As a precautionary step if client does not call
  # isPythonExists prior to this func
  if (successCommandLength == 0) return(FALSE)
  
  stdout <- do.call(system2WithStdout(), system2Args[[successCommand]])
  
  pythonVersion <- stdout %>% str_extract("\\d\\.\\d") %>% as.numeric
  return(pythonVersion >= 3.5)
}

# get success command of Python, either "python" or "python3"
successCommand <- getPythonExitCodes() %>% Filter(function(x) x == 0, .) %>% names
successCommandLength = length(successCommand)
# take the first success command if there are more than one
# command success. e.g. when the environemnt has Python 2 and 3
# installed
if (successCommandLength > 1) {
  successCommand <- successCommand[1]
}

# if Python exists in client's compueter, update widget files
# and delete their corresponding folders
if (!isPythonExists()) {
  stop("Sorry, Python is not found in your environment variables. Please check it for further execution...")
} else if (isPythonExists() && !isPython3(successCommand)) {
  stop("Seems like you are not using Python 3. Please install it for futher execution...")
} else {
  print("Python is found on your machine. Starting to tidy up widgets now...")
  system2(successCommand,  "./tidyWidgets.py")
}
# -------------------------------------------------- #
# --------- system call to tidy up widgets --------- #