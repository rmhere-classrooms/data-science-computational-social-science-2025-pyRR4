library(igraph)

g <- erdos.renyi.game(n = 100, p.or.m = 0.05)

summary(g)
# Podsumowanie (U---), a konkretnie jego trzecia wartość wskazuje,
# że graf jest nieważony.

V(g)
E(g)

E(g)$weight <- runif(length(E(g)), 0.01, 1)
summary(g)
# Podsumowanie (U-W-), a konkretnie jego trzecia wartość (W) wskazuje,
# że graf jest ważony, logiczne po ustawieniu wag dla krawędzi.

degree(g)
hist(degree(g))


cl <- clusters(g)
cl
plot(g, vertex.color=cl$membership)
cl$no
# Na podstawie wartości $no, która dla różnych wykresów 
# może być różnej wartości lub wykresu, można określić liczbę klastrów.
# W moim przypadku jest to jeden klaster.

pr <- page.rank(g)$vector
plot(g, vertex.size=pr*300,
     vertex.label=NA, edge.arrow.size=.2)