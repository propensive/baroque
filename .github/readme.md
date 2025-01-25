[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/baroque/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/baroque/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.com/invite/MBUrkTgMnA)
<img src="/doc/images/github.png" valign="middle">

# Baroque

__Make coding with complex numbers more real__

_Baroque_ provides a single generic type, `Complex`, comprising of real and
imaginary values of the same numeric type. This could be a `Double`, `Int` or a
`Quantity` with units (as provided by
[Quantitative](https://github.com/propensive/quantitative/)), or any other type
which defines [Symbolism](https://github.com/propensive/symbolism) typeclass
instances.

## Features

- provides a generic representation of complex numbers
- extends any numerical type to the complex
- provides many standard complex numerical operations on values
- allows conversion between cartesian and polar forms


## Availability







## Getting Started

All terms and types are defined in the `baroque` package, which can be imported
with:
```scala
import baroque.*
```

### Constructing complex numbers

There are several ways to construct complex numbers. The factory method of
specifying real and imaginary part is the most straightforward, for example,
```scala
val complex = Complex(3.2, -1.25)
```
represents the complex number 3.2 - 1.25i.

This is an instance of `Complex[Double]`, since the real and imaginary parts of
the number are both `Double`s. With Quantitative, we could similarly create a
complex quantity, for example,
```scala
val complex2 = Complex(3.2*Metre, -1.25*Metre)
```
which would be a `Complex[Quantity[Metres[1]]]`.

But it is also possible construct a complex number by adding the real part to
the imaginary part, where the imaginary part is created by multiplying a real
number by _i_, the imaginary unit value, which is called `I` in Baroque:
```scala
val complex3 = 0.8 + 1.8*I
```

I further possibility is to specify the complex number in polar form, using the
`Complex.polar` constructor. This takes two parameter, _magnitude_ and
_argument_. The magnitude should have the same type a the real and imaginary
parts of the number, and the argument must be a `Double`, in radians. For
example,
```scala
val complex4 = Complex.polar(12*Kilo(Gram), 0.3845)
```

### Operations with Complex Numbers

Standard arithmetic operations between `Complex` instances, using the `+`, `-`,
`*` and `/` operators work intuitively. Additionally, the prefix `~` operator
can be used to find the complex conjugate of a number.

The methods `modulus` and `argument` on `Complex` values provide, predictably,
the modulus and argument. And `sqrt` will yield one of the two square roots of
a `Complex` number; the other will be its negation.

These operations are defined, in general, so long as the necessary operations
(such as addition, multiplication and square root) are defined on the
underlying type of the real and imaginary parts.




## Status

Baroque is classified as __embryotic__. For reference, Soundness projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Baroque is designed to be _small_. Its entire source code currently consists
of 143 lines of code.

## Building

Baroque will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Baroque?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Baroque's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Baroque and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `baroque`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Baroque's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Baroque are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/baroque/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Baroque
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Baroque was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

Baroque music, architecture and design is characterized by its ornate _complexity_ which alludes to _complex numbers_.

In general, Soundness project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

The logo shows an [alto clef](https://en.wikipedia.org/wiki/Clef), used
predominantly in music for viola (which features in Baroque music), and also
resembles a capital B, the first letter of "Baroque".

## License

Baroque is copyright &copy; 2025 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

