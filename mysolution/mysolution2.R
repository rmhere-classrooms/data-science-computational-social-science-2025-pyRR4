library(igraph)

g <- barabasi.game(1000)
layout <- layout.fruchterman.reingold(g)
plot(g, layout=layout, vertex.size=2,
     vertex.label=NA, edge.arrow.size=.2)

V(g)[betweenness(g)==max(betweenness(g))]
# Numer węzła w moim przypadku wynosi 4 i jest to indeks tego węzła.

diameter(g)
# Średnica grafu w moim przypadku wynosi 11.

# Graf Erdős-Rényi ma stałą liczbę węzłów, a każda możliwa para węzłów jest
# łączona krawędzią z jednakowym, stałym prawdopodobieństwem. Na podstawie
# histogramu w pierwszym zadaniu, dla większej ilości sąsiadów, można zauważyć,
# że rozkład stopni tego grafu jest zazwyczaj rozkładem normalnym (Gaussa),
# a PageRank czy betweenness (nie mierzone w zadaniu, ale można sprawdzić)
# rozłożone dość równomiernie.

# W grafie Barabási-Albert węzły są dodawane sekwencyjnie, działa mechanizm
# preferencyjnego przyłączania - nowe węzły wolą łączyć się z węzłami, które
# mają już wysoki stopień. Istnieje kilka węzłów o bardzo wysokim stopniu
# i wiele węzłów o niskim stopniu. Te największe stają się najbardziej
# centralnymi węzłami w sieci, gromadząc bardzo wysokie wartości PageRank oraz
# Betweenness, co widzieliśmy w zadaniu. Wpływa to znacznie na sąsiedztwo - 
# wiele węzłów o niskim stopniu jest połączonych głównie z tymi kilkoma
# centralnymi.