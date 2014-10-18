To run the main tests

Do once:
cabal install --only-dependencies --enable-tests
cabal configure --enable-tests

Then:
cabal build
cabal test testexe

On newer versions of cabal you can probably just run "cabal test testexe", and not "cabal build", since "cabal test"  will do any recompilation of roshask as necessary.


To show all the tests that were run even if they all pass, run
cabal test testexe --show-details=always
