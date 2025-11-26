u <- "https://kenpom.com/cbbga26.txt"
x <- readLines(u)
y <- paste(x, collapse=" ")
p <- "(\\d{1,2}/\\d{1,2}/\\d{4})"
m <- gregexpr(p, y, perl=TRUE)[[1]]
d <- regmatches(y, gregexpr(p, y, perl=TRUE))[[1]]
s <- as.integer(m)
n <- length(s)
blocks <- list()
for (i in 1:n) {
  a <- s[i]
  e <- if (i < n) s[i+1] - 1 else nchar(y)
  blocks[[i]] <- substr(y, a, e)
}
res <- list()
for (i in 1:n) {
  b <- blocks[[i]]
  dt <- as.Date(d[i], "%m/%d/%Y")
  nums <- regmatches(b, gregexpr("\\b\\d{1,3}\\b", b, perl=TRUE))[[1]]
  nums <- as.numeric(nums)
  nums <- nums[!is.na(nums)]
  if (length(nums) %% 2 == 1) nums <- nums[-length(nums)]
  if (length(nums) >= 2) {
    g <- length(nums) / 2
    away <- sum(nums[seq(1, length(nums), 2)])
    home <- sum(nums[seq(2, length(nums), 2)])
    avg <- (away + home) / g
  } else {
    g <- 0
    away <- 0
    home <- 0
    avg <- NA
  }
  res[[i]] <- data.frame(date=dt, away_points=away, home_points=home, games=g, avg_points_per_game=avg)
}
final <- do.call(rbind, res)

if (dir.exists("data") == FALSE) dir.create("data")

f <- "data/kenpom_summary.csv"

if (file.exists(f)) {
  old <- read.csv(f)
  old$date <- as.Date(old$date)
  final <- rbind(old, final)
  final <- final[!duplicated(final$date, fromLast=TRUE), ]
}

write.csv(final, f, row.names=FALSE)
