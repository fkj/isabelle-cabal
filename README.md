# The Isabelle Cabal

The Isabelle Cabal provides support for automatically building Haskell applications which depend on code extracted from Isabelle theories.
It is implemented as a custom build type for Cabal, and supports all features of Isabelle export.

Please note that The Isabelle Cabal has seen very limited testing.
If it doesn't work for your project, please open an issue describing the problems you have experienced!

## Why would I want The Isabelle Cabal?

You are building a Haskell application, and would like to formally verify some properties of the internals of your implementation in Isabelle.
Or maybe you have formally verified the correctness of some algorithm in Isabelle and would like to package it as a Haskell library.
Or maybe you have some definitions in Isabelle and would like to test some properties of them at a large scale.
In any case, your Isabelle formalization exports Haskell code, and you want to use this code as a library which is consumed by some other Haskell code.

The "traditional" approach is to ask the Isabelle system to export some Haskell files, which can then be copied into a directory in the overall Haskell package.
Having done this, Cabal can then be used to build the application as with any other Haskell package.
This is annoying to do whenever you decide to change something in your Isabelle theories, and the approach requires storing both your Isabelle theories and the exported code, which can easily get out of sync.
A slightly less manual approach is to write a Makefile (or something similar) which asks the Isabelle system to export the Haskell files into the right directory, then asks Cabal to build the Haskell package.
This is also annoying, since you must "transplant" every Cabal function you need into the Makefile, and confusing, because people who are new to your project can not just call Cabal, but must instead remember to call Make.
If someone does forget to call Make instead of Cabal, you may again experience that the Isabelle theories and the exported Haskell files get out of sync.

The Isabelle Cabal integrates Isabelle exports directly into the Cabal build system.
It allows you to simply point Cabal at an Isabelle session which it will then compile into a library ready for linking with other code.
This means that you do not have to have exported Haskell code lying around.

## How do I use The Isabelle Cabal?

[Here](https://github.com/fkj/isabelle-cabal-demo) is a small demo application showing a simple example of how to use The Isabelle Cabal.

The Isabelle Cabal adds five fields to the library section of your Cabal package description:
| Field                | Required? | Description                                                                                                                | Isabelle option |
|----------------------|-----------|----------------------------------------------------------------------------------------------------------------------------|-----------------|
| `x-isabelle-session` | Yes       | tells Cabal which Isabelle session to export                                                                               | `SESSION`       |
| `x-isabelle-pattern` | Yes       | tells Cabal which theories from the session to export (if you don't know what to put here, `*:**code**` is a good default) | `-x`            |
| `x-isabelle-src-dir` | No        | tells Cabal where to look for the ROOT file defining the session                                                           | `-d`            |
| `x-isabelle-prune`   | No        | tells Cabal how many layers of directories to remove from module names (try `3`, but BEWARE if you have overlapping names)   | `-p`            |
| `x-isabelle-options` | No        | tells Cabal about any additional options it should pass to the Isabelle system                                             | `-o`            |

See the [Isabelle system manual section 2.5](https://isabelle.in.tum.de/doc/system.pdf#section.2.5) for much more information about each of these options (use the Isabelle option column in the table above to cross-reference).

Once you have filled in these fields, and set up The Isabelle Cabal, you can simply build your package as normal using Cabal.
When you build your package, The Isabelle Cabal will tell you which modules it has generated from your Isabelle session (or possibly give you an error message).
You should still use the `exposed-modules` and `other-modules` fields in your package description as usual.
To make distribution work, you should also:
 - add any auto-generated modules to the `autogen-modules` field
 - add your Isabelle theories and ROOT file to the `extra-source-files` field (in the top-level package properties, not in the library section)

To set up The Isabelle Cabal, you will need the `isabelle-cabal` package.
This package is currently not on Hackage, so you can instead set up your Cabal package to get it from the git distribution by adding a `cabal.project` file containing the following to your package directory:
```Cabal
packages: .

source-repository-package
  type: git
  location: git@github.com:fkj/isabelle-cabal.git
```

You can then add a `Setup.hs` file containing the following code to your package directory:
```Haskell
import Isabelle.Cabal (isabelleCabalMain)

main :: IO ()
main = isabelleCabalMain
```
and setting the `build-type` field in your package description to `Custom`.

Your `Setup.hs` now depends on the `isabelle-cabal` package, and you need to tell Cabal about this by adding something like the following stanza to your package description:
```Cabal
custom-setup
  setup-depends: base ^>=4.17.0.0
               , isabelle-cabal ^>=0.1.0.0
```

## Limitations

Cabal does not support per-component builds for custom build types.
For this reason, The Isabelle Cabal cannot support internal libraries.
In practice, this means that packages built with The Isabelle Cabal can only have a single library.
If you need to have multiple libraries, you will have to put each Isabelle session in its own package, then build multiple packages at once as a [Cabal project](https://cabal.readthedocs.io/en/stable/cabal-project.html).

Please note that The Isabelle Cabal uses the unstable internal Cabal modules, which means that updates to Cabal may break The Isabelle Cabal at any time.

## How does The Isabelle Cabal work?

The Isabelle Cabal hooks into the build phase of Cabal.
Before executing the usual Cabal build, The Isabelle Cabal creates a temporary directory in the build directory, then instructs the Isabelle system to export code from the given session into that directory.
Cabal then builds the package as usual, and the temporary directory is deleted after the build along with the usual temporary files used during a Cabal build.
The Isabelle Cabal is not that complicated, but it does require some understanding of the internals of Cabal.

## Acknowledgements

The Isabelle Cabal is strongly inspired by [Clashilator](https://github.com/gergoerdi/clashilator), which provides a similar integration between Cabal, Clash and Verilator.
