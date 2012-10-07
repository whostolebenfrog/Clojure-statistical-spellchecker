# spelling

A Clojure implementation of Peter Norvig's probabalistic spell checking algorithm.

http://norvig.com/spell-correct.html

This version varies from Peter's in that it doesn't first create a set of splits to 
base each of the variations on. I found that this wasn't particularly helpful in
Clojure.

## License

Copyright © 2012 Ben Griffiths

Distributed under the Eclipse Public License, the same as Clojure.
