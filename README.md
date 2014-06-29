This is a [TOML] parser. As of 2014-06-29 it passes all 64 tests in
[toml-test].

[TOML]: https://github.com/toml-lang/toml
[toml-test]: https://github.com/BurntSushi/toml-test

The format came to my attention after I read that Mozilla [Rust]'s
package manager is using this format. Since there isn't currently a
[Racket] parser for TOML, I thought I'd create one.

[Racket]: http://www.racket-lang.org
[Rust]: http://www.rust-lang.org

## Usage

```racket
(require toml)
(parse-toml s) ;; where is a `string?`
```

## Goals

- Pass all [toml-test] tests.

- Provide useful error messages with positions (line:col:ofs). Do so
  for both syntax errors and semantic errors (such as table
  conflicts).

- Return a Racket `hasheq` that satisfies the `json` library's
  `jsexpr?` predicate, so that you can run it through `jsexpr->string`
  to produce a JSON string.

  > Caveat: Any TOML datetime values are `date` structs, which won't
  satisfy `jsexpr?`. Originally I parsed these to a
  `current-seconds`-style integer value. But `toml-tests` needs things
  to be tagged with types, so that's why I had to switch. I should
  probably provide a conversion function to turn any such instances
  back into a datetime string so that it can be passed to
  `jsexpr->string`.

## Implementation

Uses [Parsack], a Parsec-like monadic parser combinator library
implementation in Racket.

[Parsack]: https://github.com/stchang/parsack
