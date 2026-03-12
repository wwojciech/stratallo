## Resubmission

This is a resubmission addressing CRAN feedback.

Unit tests have been updated to use a more appropriate tolerance
for numeric comparisons in expect_identical() and expect_equal().

"You are not using a reasonable tolerance for numeric quantities."

## Test environments

* macOS R 4.5.0
* Windows Server 2022 x64 (build 20348)

## R CMD check results

There were no ERRORs, WARNINGs. The following 1 NOTE has been reported by
`devtools::check()`.

```
---R CMD check results --- stratallo 3.0.1 ---
Duration: 1m 26.2s

❯ checking for future file timestamps ... NOTE
  unable to verify current time

0 errors ✔ | 0 warnings ✔ | 1 note ✖

R CMD check succeeded

0 errors | 0 warnings | 1 note
```
