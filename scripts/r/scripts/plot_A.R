plot.A <- function(out.file, path1, path2, iteration1 = 10000, iteration2 = 10000)
{
    A.noLT <- as.matrix(read.table(paste(path1, "/G/block_A/", iteration1, ".txt", sep = "")))
    A.LT <- as.matrix(read.table(paste(path2, "/G/block_A/", iteration2, ".txt", sep = "")))
    J.noLT <- nrow(A.noLT)
    J.LT <- nrow(A.LT)
    pdf(out.file, width = 8, height = 4)
    par(mfrow = c(1,2), mar = c(1,1,1,1))
    image(t(A.noLT)[J.noLT:1,], col = heat.colors(100), xaxt = "n", yaxt = "n")
    ## axis(side = 1, at = seq(0,1,length=J.noLT), labels = 1:J.noLT)
    ## axis(side = 2, at = seq(0,1,length=J.noLT), labels = 1:J.noLT)
    image(t(A.LT)[J.LT:1,], col = heat.colors(100), xaxt = "n", yaxt = "n")
    ## axis(side = 1, at = seq(0,1,length=J.LT), labels = 1:J.LT)
    ## axis(side = 2, at = seq(0,1,length=J.LT), labels = 1:J.LT)
    dev.off()
}

plot.A.binary <- function(out.file, path1, path2, iteration1 = 10000, iteration2 = 10000)
{
    A.noLT <- as.matrix(read.table(paste(path1, "/G/zero_one/", iteration1, ".txt", sep = "")))
    A.LT <- as.matrix(read.table(paste(path2, "/G/zero_one/", iteration2, ".txt", sep = "")))
    J.noLT <- nrow(A.noLT)
    J.LT <- nrow(A.LT)
    pdf(out.file, width = 8, height = 4)
    par(mfrow = c(1,2), mar = c(1,1,1,1))
    image(t(A.noLT)[J.noLT:1,], xaxt = "n", yaxt = "n")
    ## axis(side = 1, at = seq(0,1,length=J.noLT), labels = 1:J.noLT)
    ## axis(side = 2, at = seq(0,1,length=J.noLT), labels = 1:J.noLT)
    image(t(A.LT)[J.LT:1,], xaxt = "n", yaxt = "n")
    ## axis(side = 1, at = seq(0,1,length=J.LT), labels = 1:J.LT)
    ## axis(side = 2, at = seq(0,1,length=J.LT), labels = 1:J.LT)
    dev.off()
}
