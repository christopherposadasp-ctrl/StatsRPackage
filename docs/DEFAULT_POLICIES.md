# Default Policies

This document records mathematically important default behavior that should not be changed casually.

When a policy changes, update:
- implementation
- tests
- cheat sheet comments/examples
- this document

## Proportion Family Defaults

Applies to:
- `p_test()`
- `power_p_z()`
- `n_required_p_z()`

### Two-Sample Case

| Null difference (`p0`) | Default pooled setting | Default continuity correction |
| --- | --- | --- |
| `p0 = 0` | pooled (`TRUE`) | off (`FALSE`) |
| `p0 != 0` | unpooled (`FALSE`) | off (`FALSE`) |

Notes:
- continuity correction is available explicitly
- requesting `pooled = TRUE` when `p0 != 0` is invalid and errors

### One-Sample Case

- `p_test()` uses the exact binomial test
- `pooled` and `continuity` are ignored in one-sample calls

## Variance Family Two-Sided Rejection Regions

Applies to:
- `var_test_chisq()` one-sample branch (chi-square)
- `var_test_chisq()` two-sample branch (F test)

Policy:
- two-sided rejection regions are printed and stored as lower-tail OR upper-tail regions
- they are not represented as absolute-value regions

Examples of intended form:
- `Chi^2 < c1 or Chi^2 > c2`
- `F < c1 or F > c2`

## Consistency Conventions

Across major families:
- include `alpha` or `conf.level` where appropriate
- support `digits` for printed summaries
- support `quiet` for print suppression
- return invisible classed objects containing full computational details

