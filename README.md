# Euwe

The goal is to grow a library for tournament management.
Since this is a pet project, there is no schedule nor
concrete spec.

The idea is to create primitives for drawing lots, scoring
games, and sorting players - all operating on the same data
type, a map with entries for players and games. Thus, making
these primitives composable, and hopefully yielding a simple
DSL for defining tournaments.

## Name
The library is named after [Max Euwe][euwe] a mathematician
and former world chess champion.

[euwe]: http://en.wikipedia.org/wiki/Max_Euwe "Wikipedia"

## To do
* Tests for euwe.score
* Support for sorting via game against each other
* Documentation

## License

Copyright © 2013 Markus Wappler.

Distributed under the Eclipse Public License, the same as Clojure.
