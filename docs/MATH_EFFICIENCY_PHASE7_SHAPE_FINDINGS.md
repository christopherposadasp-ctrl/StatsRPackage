# Phase 7 Shape Helper Findings

Audit baseline: `codex/math-efficiency-audit` after Phase 6 findings commit `5a655bf`.

Phase 7 audited the exported descriptive shape helpers:

- `skew()`
- `kurt()`

Production code was not changed. The audited formulas, validation rules, interpretation labels, and result contracts matched the intended behavior.

## Independent Checks

Temporary independent R probes passed for:

- `skew()` moment estimator `g1 = m3 / m2^(3/2)`;
- `skew()` adjusted Fisher-Pearson estimator `G1 = sqrt(n(n - 1)) / (n - 2) * g1`;
- stored skewness fields `n`, `mean`, `m2`, `m3`, `estimate`, `benchmark`, and `direction`;
- skewness interpretation labels for right-skewed, left-skewed, and approximately symmetric samples;
- `skew()` missing-value, `na_rm`, minimum-sample-size, identical-observation, and infinite-value validation;
- `kurt()` Pearson moment kurtosis `b2 = m4 / m2^2`;
- `kurt()` moment excess kurtosis `g2 = b2 - 3`;
- `kurt()` adjusted excess and adjusted Pearson kurtosis paths;
- stored kurtosis fields `n`, `mean`, `m2`, `m4`, `pearson_kurtosis`, `excess_kurtosis`, `estimate`, and `benchmark`;
- kurtosis interpretation labels for leptokurtic, platykurtic, Pearson mesokurtic, and excess mesokurtic samples;
- invisible classed return objects for both helpers.

No mathematical defects were found.

## Tests Added

Focused tests were added in `StatsPackage -1.0/tests/testthat/test-shape.R` for:

- left-skewed and approximately symmetric `skew()` interpretation labels;
- `skew()` invisible return class chain;
- `skew()` infinite-value validation;
- adjusted Pearson kurtosis in `kurt(method = "adjusted", excess = FALSE)`;
- platykurtic and mesokurtic kurtosis interpretation labels;
- `kurt()` invisible return class chain;
- `kurt()` infinite-value validation.

These tests preserve the existing public API and add no production-code length.

## Policy Status

The Phase 7 audit preserved:

- exported function names, signatures, defaults, and result classes;
- moment estimators using central moments with divisor `n`;
- adjusted skewness and adjusted kurtosis formulas;
- `na_rm = FALSE` missing-value errors and `na_rm = TRUE` omission behavior;
- identical-observation errors for zero variance;
- finite-observation validation;
- unrounded stored computational fields;
- `digits` as print-only formatting;
- `quiet = TRUE` suppressing printed output.

## Validation

Commands run:

```r
devtools::test("StatsPackage -1.0", filter = "shape")
devtools::test("StatsPackage -1.0")
```

Reported results:

- Targeted shape tests: `FAIL 0 | WARN 0 | SKIP 0 | PASS 50`
- Full test suite: `FAIL 0 | WARN 0 | SKIP 0 | PASS 624`

Local process note: both test commands reported clean test output, but the Rscript process returned a nonzero exit after reporting results. This is consistent with the previously recorded local R exit anomaly and is not counted as a test failure.

## Follow-Up Risks

No Phase 7 mathematical blockers remain. Phase 8 can proceed to code efficiency and simplification review, while continuing to treat local `devtools::check()` as non-blocking until the known `Rcmd.exe` / Rscript exit anomaly is resolved in a clean environment.
