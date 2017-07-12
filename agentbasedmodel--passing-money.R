# knb 20170711
# Counterintuitive problem: Everyone in a room keeps giving dollars to random others.
# You’ll never guess what happens next.
# http://www.decisionsciencenews.com/2017/06/19/counterintuitive-problem-everyone-room-keeps-giving-dollars-random-others-youll-never-guess-happens-next/
# when-random-people-give-money-to-random-other-people
#  this is basically the same computation physicists do
#  when they compute the Boltzmann distribution,
# https://quomodocumque.wordpress.com/2017/06/27/when-random-people-give-money-to-random-other-people/
library(tidyverse)

if (! require("gganimate")) {
        library(devtools)
        install_github("dgrtwo/gganimate")
}
library(gganimate) # on github

NUMPLAYERS = 45
ROUNDS = 5000
INITWEALTH = 45

#initialize the bank
#columns wealths of the NUMPLAYERS players
#rows show wealths of each of the ROUNDS ticks of the clocks
bank = matrix(0, nrow = ROUNDS, ncol = NUMPLAYERS)
bank[1,] =  c(rep(INITWEALTH, NUMPLAYERS))

#function to give a dollar to someone other than oneself
get_recipient = function(player) {
        sample(setdiff(1:NUMPLAYERS, player), 1)}

#execute trades and update the ledger
for (i in 2:ROUNDS) {
        #every player with wealth chooses another person to receive a buck
        recipients = sapply(which(bank[i - 1,] > 0), get_recipient)

        #table of the dollars owed each person
        count_table = table(recipients)

        #get the indices of the people owed money
        indices = as.integer(names(count_table))

        #everyone gives up a dollar, unless they are at zero
        bank[i,] = ifelse(bank[i - 1,] > 0, bank[i - 1,] - 1, bank[i - 1,])

        #selected people receive dollars
        bank[i, indices] = bank[i, indices] + count_table
}

####################Animate it
#Make a suitable long data frame
df = as.data.frame(bank)
names(df) = 1:NUMPLAYERS
df = df %>%
        mutate(frame = 1:ROUNDS) %>%
        gather(person, wealth, 1:NUMPLAYERS) %>%
        mutate(person = as.numeric(person)) %>%
        arrange(frame) %>%
        group_by(frame) %>%
        mutate(rank = rank(wealth, ties.method = "random")) %>%
        ungroup() %>%
        gather(histtype,playerid,c(person,rank)) %>%
        mutate(histtype = sprintf("Ordered by %s", histtype))

p <- ggplot(df, aes(x = playerid, y = wealth, frame = frame, fill=histtype)) +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank()) +
        geom_rect(aes( xmin = playerid - .4, xmax = playerid +.4, ymin = 0, ymax = wealth)) +
        scale_x_continuous(breaks = 1:NUMPLAYERS) +
        coord_cartesian(xlim = c(0, NUMPLAYERS), y = c(0, 5 * INITWEALTH)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(x='players',y='dollars') +
        facet_wrap( ~ histtype,ncol=1) +
        theme(legend.position = "none")
p

#set options for the animation package. Needs ImageMagick installed on your computer
# 5000 frames will take at least one hour (at 40 frames / minute: 2 hours)
animation::ani.options(nmax = ROUNDS,
                       #convert = 'C:\\Program Files\\ImageMagick-7.0.6-Q16')
                       convert = 'convert')
#save the movie
gganimate(p, "dollar_stacked.mp4", interval = .01)
