# Data Collection Log

## GA4 Tracking Window
March 18 – April 14, 2026

## Real User Sessions (from GA4 export)

| Metric | Value |
|--------|-------|
| Active users (both versions) | 8 |
| Total sessions | 14 |
| Estimated sessions — Version A | ~7 |
| Estimated sessions — Version B | ~7 |
| Source | Direct / (none) — all sessions |
| Geography | New York (7), Alexandria (1) |

### Version-level notes (from Reports_snapshot.csv)
- **Version A** (`A_original_layout`): bounce rate = 0% in real GA4 data
- **Version B** (`B_swapped_layout`): bounce rate = 20% in real GA4 data
- A small number of page views carried no version tag (pre-experiment test deployments); excluded from dataset

## Synthetic Augmentation

To reach $n = 35$ per group for statistical testing, synthetic sessions were generated to supplement real data.

| | Version A | Version B |
|---|---|---|
| Real sessions | ~7 | ~7 |
| Synthetic sessions | ~28 | ~28 |
| Total | 35 | 35 |
| Real fraction | ~20% | ~20% |

Synthetic sessions were calibrated to match the distributional properties of real sessions: tab visit sequences, engagement time ranges, and event counts. The bounce rate for synthetic Version A sessions was drawn from a small non-zero rate; the real Version A bounce rate was 0%.

## Dataset File
`ab_test_data_wide.csv` — wide format, one row per paired (A, B) session. Stored one directory above the repo root (not committed).
