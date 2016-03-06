Welcome to Wagon Challenge1!
============================

The included program is called getstats. To compile it, run

    $ cabal sandbox init
    $ cabal install -j

If you have the generator program, it's a good idea to
run it by itself to generate the input the the stats
program. That way the stats program can be timed in
isolation.

    $ generator 10000 > gen10k
    $ time .cabal-sandbox/bin/getstats < gen10k

If you don't have the generator program, you can use
input from the test_data directory

    $ time .cabal-sandbox/bin/getstats < test_data/gen10k

Translations of getstats to Python and Go are also provided
for comparison:

    $ time ./getstats.py < test_data/gen10k
    
    $ time go run getstats.go < test_data/gen10k

