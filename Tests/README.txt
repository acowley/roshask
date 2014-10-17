To run the main tests

Do once:
cabal configure --enable-tests

Then:
cabal build
cabal test

On newer versions of cabal you can probably just run "cabal test", and not "cabal build", since "cabal test"  will do any recompilation of roshask as necessary.


To show all the tests that were run even if they all pass, run
cabal test --show-details=always
