#vervollst?ndige Deckungskapital-Daten
W <- completeV(V, trans, payoffPost, payoffPre, i, state)

# plotte verlauf
ggplot(data=W, mapping=aes(x=time, y=v, color=state)) +
  geom_line()


