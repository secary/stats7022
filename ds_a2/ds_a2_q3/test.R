pacman::p_load("import")
import::from("ds_a2_q3.R", get_lda2)

# Test
x1 <- c(5,10,11,4,6,9,3,7,8)
x2 <- 8:0
y <- rep(LETTERS[19:17], each = 3)
new1 <- c(6,5)
new2 <- c(2,8)
get_lda2(x1 = x1, x2 = x2, y = y, new1 = new1, new2 = new2)