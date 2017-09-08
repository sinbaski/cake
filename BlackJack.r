rm(list=ls())
## setClass("player", slots=c(role="numeric", num="numeric"));
## args <- commandArgs();
player <- matrix(NA, 2, 2);
dealer <- matrix(NA, 2, 2);
cards <- matrix(NA, 52, 2);
for (i in 1:dim(cards)[1]) {
    cards[i, 1] <- i %% 4 + 1;
    cards[i, 2] <- i %% 13 + 2;
}
for (i in 1:2) {
    player[i, 1] <- floor(runif(1, min=1, max=5));
    player[i, 2] <- floor(runif(1, min=2, max=15));
}
for (i in 1:2) {
    dealer[i, 1] <- floor(runif(1, min=1, max=5));
    dealer[i, 2] <- floor(runif(1, min=2, max=15));
}
print(player);
print(dealer);
status <- c(sum(player[, 2]), dealer[, 2]);
while (TRUE) {
    cat("stand or hit? (0 for stand, 1 for hit) ");
    instruction <- readLines("stdin", n=1);
    if (instruction == 0) { # stand
        s <- sum(dealer[, 2]);
        while (s <= 16) {
            c <- floor(runif(n=1, min=1, max=dim(cards)[1] + 1));
            dealer <- rbind(dealer, cards[c, ]);
            s <- sum(dealer[, 2]);
            cards <- cards[-c, ];
        }
        print(dealer);
        status[2] <- s;
    } else if (instruction == 1) { # hit
        c <- floor(runif(n=1, min=1, max=dim(cards)[1] + 1));
        player <- rbind(player, cards[c, ]);
        s <- sum(player[, 2]);
        cards <- cards[-c, ];
        print(player);
        status[1] <- s;
    }
    if (status[1] <= 21 && status[2] <= 21 && status[1] > status[2]) {
        cat("You have won!\n");
        break;
    } else if (status[1] <= 21 && status[2] > 21) {
        cat("You have won!\n");
        break;
    } else if (status[1] > 21 && status[2] > 21) {
        cat("It's a draw!\n");
        break;
    } else if (instruction == 0 && status[1] == status[2]) {
        cat("It's a draw!\n");
        break;
    } else if (status[1] > 21 && status[2] <= 21) {
        cat("You have lost!\n");
        break;
    } else if (instruction == 0 && status[1] < status[2]) {
        cat("You have lost!\n");
        break;
    }
}
