This is a [TOML] parser. It supports TOML **0.2.0**, including
arrays-of-tables. As of 2014-06-29 it passed all 64 tests in
[toml-test].

[TOML]: https://github.com/toml-lang/toml
[toml-test]: https://github.com/BurntSushi/toml-test

The format came to my attention after I read that Mozilla [Rust]'s
package manager is using this format. Since there was't currently a
[Racket] parser for TOML, I thought I'd create one.

My motivation was **only** to get more experience using [Parsack] -- a
Parsec-like monadic parser combinator library implementation in Racket
-- after having used it in a [markdown parser].

[Parsack]: https://github.com/stchang/parsack
[markdown parser]: https://github.com/greghendershott/markdown

- I did **not** make this available on the Racket package manager.

- I do **not** have plans to update this to support TOML versions
  newer than 0.2.0.
  
- I updated this 2019-08-19 to make it work with a breaking change in
  [Parsack] that happened many years ago. Beyond that, I do **not**
  plan to fix bugs or keep it up to date. In fact I might "archive"
  this repo on GitHub.
  
If you would like to fork this (or throw it away and start from
scratch) you are welcome to do so!

[Racket]: http://www.racket-lang.org
[Rust]: http://www.rust-lang.org

## Usage

```racket
(require toml)
(parse-toml s) ;; where `s` is a `string?`
```

## Goals

- Pass all [toml-test] tests for TOML **0.2.0**.

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
