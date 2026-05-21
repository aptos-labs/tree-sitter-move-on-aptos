# Bytecode v10 (Move Language v2.4 / v2.5) — Grammar Delta

**Status:** Draft — awaiting review before editing `grammar.js`.
**Authoritative source:** `/Users/greg/git/aptos-core` @ `multisig-timelock`
(`third_party/move/move-compiler-v2/legacy-move-compiler/src/parser/syntax.rs`,
`third_party/move/move-model/src/metadata.rs`,
`third_party/move/move-binary-format/src/file_format_common.rs`).

## Version mapping

`file_format_common.rs:584–587`:

```rust
pub const VERSION_DEFAULT_LANG_V2_4: u32 = VERSION_10;
pub const VERSION_DEFAULT_LANG_V2_5: u32 = VERSION_10;
```

So **bytecode v10 ↔ language v2.4 *and* v2.5**. The grammar delta below covers
both language versions in one pass.

`LanguageVersion` variant order (`metadata.rs:192–206`):
`V1, V2_0, V2_1, V2_2, V2_3, V2_4, V2_5`. Both v2.4 and v2.5 are still marked
`unstable` in this checkout, but the parser accepts them under the appropriate
flag.

## What's already covered

Landed in commit `f32c1ee feat: add Move 2.4/2.5 grammar support for spec
language and patterns`:

| Feature                                       | Min ver | grammar.js rule              |
| --------------------------------------------- | ------- | ---------------------------- |
| `proof { ... }` blocks (spec + lemma trailing, spec member) | V2_4    | `proof_block`, `proof_statement`, `proof_body` |
| `lemma name<T>(params) { ... } [proof { ... }]`             | V2_4    | `spec_lemma`, `lemma_spec_body`, `lemma_spec_member` |
| `modifies_of<param>(args) targets;` / `modifies_of<param>*;`| V2_4    | `spec_modifies_of`           |
| `reads_of<param> Type1, Type2;` / `reads_of<param>*;`       | V2_4    | `spec_reads_of`              |
| Function access specifiers: `pure`, `reads R`, `writes T`, `!reads X`, `!writes Y` (chained) | V2_5 | `access_specifier`, `access_specifier_arg` |
| `reads Type1, Type2;` spec block member (all versions)      | —       | `spec_reads`                 |

## Syntactic gaps (parser-level — must add)

### Gap 1 — `|~` state-label operator  (V2_4)

**Compiler reference:** `syntax.rs:2630–2782` (`is_state_label`,
`is_post_only_state_label`, `parse_state_label`,
`parse_post_only_state_label`), token `Tok::PipeTilde` at
`lexer.rs:56, 569`.

Appears as a low-precedence operator in **spec-context expressions**
(condition bodies inside `ensures`, `requires`, `aborts_if`, etc.) and inside
proof statements. The RHS is parsed with `parse_exp` — i.e. `|~` binds weaker
than every other operator.

Four forms:

```move
spec foo {
    // single state label
    ensures s1 |~ result == old(x) + 1;

    // pre-only range  (state from `s1` onwards)
    ensures s1.. |~ result > 0;

    // full range  (state from `s1` up to `s2`)
    ensures s1..s2 |~ result == 0;

    // post-only range  (state up to `s1`)
    ensures ..s1 |~ result == 0;
}
```

**Proposed grammar:**

- Add new external/literal token `|~` (tree-sitter handles multi-char punctuators
  by listing them as a literal string in the rule — no scanner work needed).
- Add `state_labeled_expression` rule. Keep it loose; the four shapes share a
  common prefix:

  ```js
  state_labeled_expression: $ => prec.right(SPEC_LABEL_PREC, seq(
      choice(
          // ..ident |~ expr  (post-only)
          seq('..', field('post', $.identifier)),
          // ident |~ expr        (single)
          // ident.. |~ expr      (pre-only range)
          // ident..ident |~ expr (full range)
          seq(
              field('pre', $.identifier),
              optional(seq('..', optional(field('post', $.identifier))))
          )
      ),
      '|~',
      field('body', $._expression)
  )),
  ```

  `SPEC_LABEL_PREC` should be the lowest precedence among the spec-context
  expression alternatives.

- Wire it into `spec_condition`. Today `spec_condition` accepts
  `commaSep1($._expression)`. Replace with a new `_spec_condition_body`:

  ```js
  _spec_condition_body: $ => choice($._expression, $.state_labeled_expression),
  ```

  Tree-sitter is permissive enough that we can simply alternate state-labeled
  vs. ordinary expressions at the body level. Allowing the construct in
  non-spec expressions would over-accept; gating it to spec contexts keeps the
  syntax tree clean.

**Test corpus to add:** `test/corpus/spec_v24_state_label.txt` exercising all
four shapes, plus a negative example confirming `|~` is not valid in regular
function bodies.

### Gap 2 — Behavioral predicates `requires_of` / `aborts_of` / `ensures_of` / `result_of`  (V2_4)

**Compiler reference:** `syntax.rs:2607–2628` (`behavior_kind_from_str`,
`is_bare_behavior`), plus `parse_bare_behavior` (search the same file for
`parse_bare_behavior` — it consumes the keyword, optional type args, then a
parenthesized arg list).

Source form:

```
behavior_keyword < TypeArgs? > ( exprs )
```

Where `behavior_keyword ∈ { requires_of, aborts_of, ensures_of, result_of }`.

```move
spec foo {
    ensures ensures_of<bar>(x, result);
    aborts_if aborts_of<baz<u64>>(x);
    requires requires_of<f>(arg);
    ensures result == result_of<bar>(x);
}
```

These are **soft keywords** — the identifier itself isn't reserved; only the
`identifier <` token sequence in a spec context triggers the parse.

**Proposed grammar:**

```js
behavior_predicate: $ => seq(
    field('kind', choice('requires_of', 'aborts_of', 'ensures_of', 'result_of')),
    field('function', $.type_arguments),       // forced — `<` is required
    '(',
    commaSep($._expression),
    ')'
),
```

Note: the `<...>` part is **not optional** in the compiler — `is_bare_behavior`
specifically checks that the next token is `<`. That asymmetry (vs. ordinary
calls where type args are optional) is what makes the soft-keyword disambiguation
work.

Wire `behavior_predicate` into the spec expression alternative (same hook point
as `state_labeled_expression`). It can also appear nested inside ordinary
expressions in spec conditions, so adding it as one of the choices in
`_spec_condition_body` and inside `proof_statement` is sufficient — we don't
need to add it everywhere `$._expression` is used.

**Test corpus:** `test/corpus/spec_v24_behavior_predicates.txt` covering all
four keywords with and without nested type args.

### Gap 3 — Visibility on `const`  (V2_4)

**Compiler reference:** `syntax.rs:3744–3750` —
`require_move_version(V2_4, ...)` inside `parse_constant_decl` when a
visibility modifier precedes `const`. Semantic check at
`module_builder.rs:4130–4131`.

Source form:

```move
public const MAX: u64 = 100;
package const LIMIT: u64 = 50;
public(friend) const X: u8 = 0;   // any visibility flavor the grammar already supports
```

**Current grammar.js (line 214):**

```js
constant_declaration: $ =>
    seq(
        optional($.attributes),
        'const',
        field('name', $.identifier),
        ...
    ),
```

— no visibility slot.

**Proposed grammar:**

```js
constant_declaration: $ =>
    seq(
        optional($.attributes),
        optional(field('visibility', $._visibility)),
        'const',
        ...
    ),
```

The existing `_visibility` rule (line 357) already accepts `public`,
`public(friend)`, `public(package)`, `public(script)`, `friend`, and `package` —
exactly what the compiler accepts.

**Test corpus:** extend `test/corpus/constants.txt` (or create one if absent)
with each visibility flavor.

### Gap 4 — `!acquires` negation  (V2_4? — opportunistic)

**Compiler reference:** `syntax.rs:3269–3282`. The same `negated` flag that
fronts `reads` / `writes` *also* fronts `acquires`. Today
`acquires_clause` (line 374) is `seq('acquires', commaSep1($.name_access_chain))`
with no optional `!`.

The survey didn't explicitly version-gate `!acquires` — it falls out of the
unified clause-loop. Whether the compiler reports `!acquires` as a feature
introduced in V2_4 or just an accidental side-effect of the new specifier loop
isn't 100% clear. Two options:

1. **Add it:** small change, follows the compiler exactly. Grammar accepts a
   bit more than strict-Move-1 would have, but tree-sitter is meant to be
   permissive.
2. **Skip:** leave `acquires_clause` as-is until a real-world failure shows up.

Recommendation: **add it.** One-line change:

```js
acquires_clause: $ => seq(optional('!'), 'acquires', commaSep1($.name_access_chain)),
```

## Semantic-only "v2.4 features" — verification, not new rules

The survey flagged these as v2.4-gated, but the compiler's **parser** accepts
them at all versions; the gates fire in later passes. The grammar should
already handle them — we'll add corpus tests to confirm.

| Feature                              | Why parser-agnostic                                                  | Proposed test |
| ------------------------------------ | -------------------------------------------------------------------- | ------------- |
| Primitive-type `match`               | `Exp_::Match` is unconditional; gate is in `match_transforms.rs:282` | `match (n) { 0 => …, _ => … }` |
| `assert!(cond, fmt, args…)`          | macro expansion gate in `macros.rs:168`                              | call with 3+ args |
| `assert_eq!` / `assert_ne!`          | macro expansion gate in `macros.rs:251`                              | call expression |
| `abort b"message"`                   | type-check gate in `exp_builder.rs:6605`                             | `abort b"oops";` |
| `public struct` / `public enum` etc. | parser `parse_struct_decl` accepts visibility unconditionally; gate is in `module_builder.rs:4806` | already covered by `_visibility` |

If any of these fail to parse, that's a separate bug from v2.4 — most likely a
gap in an unrelated grammar rule (e.g., a missing pattern shape for `match`).

## Decisions (resolved 2026-05-20)

1. **Context scope for `|~` and behavior predicates:** spec contexts only.
   Add a `_spec_expression` wrapper and wire it into `spec_condition`,
   `lemma_spec_member`, and `proof_statement`. Faithful to the compiler.
2. **`!acquires`:** add it. One-line change to `acquires_clause` — cheap
   insurance even though no current `.move` file uses it. Verified that the
   compiler's parser threads `negated` through the `acquires` branch and that
   the model's pretty-printer (`sourcifier.rs:509`) enumerates `!acquires`
   as a real specifier kind.
3. **Corpus-level verification:** yes. After the grammar additions land, run
   `batch-test.py` against `aptos-framework` / `move-stdlib` in
   `/Users/greg/git/aptos-core` at the current `multisig-timelock` tip.
   Triage any parse failures as a separate follow-up.

## Proposed implementation order (after review)

1. **`public const` / `package const`** — smallest, lowest risk, isolated rule
   edit. Land first, run `tree-sitter generate && tree-sitter test`.
2. **`!acquires`** — one-line change to `acquires_clause`.
3. **Behavioral predicates** — add `behavior_predicate` rule, wire into spec
   contexts, corpus tests.
4. **`|~` state-label operator** — add token + `state_labeled_expression`
   rule, wire into `spec_condition` / proof statements, corpus tests. (Most
   intricate; some grammar conflict resolution likely needed because
   `identifier ..` overlaps with existing range patterns.)
5. **Verification tests for the semantic-only items** — corpus tests only.
6. **Optional:** run `batch-test.py` against the v10-era framework to confirm
   nothing else parses-failing surfaced.

Each step is a separate commit so it can be reverted independently if it
regresses anything.
