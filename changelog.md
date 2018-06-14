0.2.0.1

* Import `eitherT` when using old EitherT instance.

0.2.0.0

* Trying to make things agreeable for removed modules (Control.Monad.Trans.Either) and new instances (Control.Monad.Except(T)).
* Applied the stylish-haskell hammer, and may remove unicode syntax for the proper hackage release.
* Update readme, added tested-with to cabal file to prepare for travis CI integration.
* Added travis yaml, and tested-with to cabal.
* Updated gitignore file.
* Removed 7.6 from tested GHC versions, and removed a command from travis to change to a directory that doesn't exist.
* Moved the lower bound of mtl to 2.1 to permit building using transformers 0.3 for slightly older GHC versions.
* Added CPP to not include Except instances unless MTL is min version 2.2.2. Added generous bounds to either package.
* Add ErrorT instance for pre-mtl-2.2.2.
* Added concrete type examples to Haddock, ala lens examples.

0.1.0.0

* Actually add the code
