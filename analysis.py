"""
analysis.py — A/B Test: UI Layout and User Engagement
Data Wrangling Studio: Version A (left-panel) vs. Version B (top-panel)

Run from repo root:
    python analysis.py
Outputs: figures/ directory and results.json
"""

import json
import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import seaborn as sns
from scipy import stats

# ── 1. Load & reshape ─────────────────────────────────────────────────────────

DATA_PATH = os.path.join(os.path.dirname(__file__), "..", "ab_test_data_wide.csv")
if not os.path.exists(DATA_PATH):
    raise FileNotFoundError(
        f"Data file not found at {DATA_PATH}\n"
        "Place ab_test_data_wide.csv one directory above the repo root."
    )
wide = pd.read_csv(DATA_PATH, skiprows=1)

def extract_group(df, suffix):
    cols = {
        f"user_id_{suffix}":        "user_id",
        f"version_{suffix}":        "version",
        f"sections_visited_{suffix}":"sections",
        f"engagement_time_sec_{suffix}": "time_sec",
        f"event_count_{suffix}":    "events",
        f"bounced_{suffix}":        "bounced",
        f"reached_export_{suffix}": "reached_export",
        f"completed_workflow_{suffix}": "completed",
        f"tabs_visited_{suffix}":   "tabs",
    }
    return df[list(cols)].rename(columns=cols)

long = pd.concat([extract_group(wide, "A"), extract_group(wide, "B")], ignore_index=True)

A = long[long.version == "A"]
B = long[long.version == "B"]
n_A, n_B = len(A), len(B)
print(f"── Sample sizes: A={n_A}, B={n_B} ───────────────────────────────────")

# ── 2. Descriptive statistics ─────────────────────────────────────────────────

desc = long.groupby("version").agg(
    n=("sections", "count"),
    sections_mean=("sections", "mean"),
    sections_median=("sections", "median"),
    sections_sd=("sections", "std"),
    time_mean=("time_sec", "mean"),
    time_median=("time_sec", "median"),
    time_sd=("time_sec", "std"),
    bounce_rate=("bounced", "mean"),
    export_rate=("reached_export", "mean"),
    complete_rate=("completed", "mean"),
).round(3)

print("\n── Descriptive statistics ────────────────────────────────────")
print(desc.to_string())

# ── 3. Normality checks (Shapiro-Wilk) ───────────────────────────────────────

def sw(x): return stats.shapiro(x)

sw_sec_A = sw(A.sections); sw_sec_B = sw(B.sections)
sw_time_A = sw(A.time_sec); sw_time_B = sw(B.time_sec)

print("\n── Shapiro-Wilk: sections visited ────────────────────────────")
print(f"  A: W={sw_sec_A.statistic:.4f}, p={sw_sec_A.pvalue:.4f}")
print(f"  B: W={sw_sec_B.statistic:.4f}, p={sw_sec_B.pvalue:.4f}")

print("\n── Shapiro-Wilk: engagement time ─────────────────────────────")
print(f"  A: W={sw_time_A.statistic:.4f}, p={sw_time_A.pvalue:.4f}")
print(f"  B: W={sw_time_B.statistic:.4f}, p={sw_time_B.pvalue:.4f}")

# ── 4. Hypothesis tests ───────────────────────────────────────────────────────

# Mann-Whitney U (two-sided)
def mannwhitney(x, y):
    """Returns (U, p, rank_biserial_r)."""
    res = stats.mannwhitneyu(x, y, alternative="two-sided")
    r = 1 - 2 * res.statistic / (len(x) * len(y))
    return res.statistic, res.pvalue, r

U_sec, p_sec, r_sec = mannwhitney(A.sections, B.sections)
U_time, p_time, r_time = mannwhitney(A.time_sec, B.time_sec)

print("\n── Mann-Whitney U: sections visited (A vs B) ─────────────────")
print(f"  U={U_sec:.1f}, p={p_sec:.4f}, rank-biserial r={r_sec:.4f}")

print("\n── Mann-Whitney U: engagement time (A vs B) ──────────────────")
print(f"  U={U_time:.1f}, p={p_time:.4f}, rank-biserial r={r_time:.4f}")

# Fisher's exact for binary outcomes
def fisher(col):
    tbl = pd.crosstab(long.version, long[col])
    # ensure columns 0,1 exist
    for c in [0, 1]:
        if c not in tbl.columns:
            tbl[c] = 0
    tbl = tbl[[0, 1]]
    OR, p = stats.fisher_exact(tbl.values)
    return OR, p, tbl

OR_b, p_b, tbl_b = fisher("bounced")
OR_e, p_e, tbl_e = fisher("reached_export")
OR_c, p_c, tbl_c = fisher("completed")

print("\n── Fisher's exact: bounce rate ───────────────────────────────")
print(tbl_b.to_string()); print(f"  OR={OR_b:.4f}, p={p_b:.4f}")

print("\n── Fisher's exact: export rate ───────────────────────────────")
print(tbl_e.to_string()); print(f"  OR={OR_e:.4f}, p={p_e:.4f}")

print("\n── Fisher's exact: workflow completion ───────────────────────")
print(tbl_c.to_string()); print(f"  OR={OR_c:.4f}, p={p_c:.4f}")

# ── 5. Visualizations ────────────────────────────────────────────────────────

os.makedirs("figures", exist_ok=True)
pal = {"A": "#4472C4", "B": "#ED7D31"}

# Helper jitter strip
def add_strip(ax, data, version, x_pos, color):
    jitter = np.random.default_rng(42).uniform(-0.12, 0.12, len(data))
    ax.scatter(np.full(len(data), x_pos) + jitter, data,
               color=color, alpha=0.5, s=18, zorder=3)

# 5a. Boxplot: sections visited
fig, ax = plt.subplots(figsize=(5, 4))
bp = ax.boxplot(
    [A.sections.values, B.sections.values],
    positions=[0, 1], patch_artist=True, widths=0.4,
    medianprops=dict(color="black", lw=2),
    whiskerprops=dict(lw=1.2), capprops=dict(lw=1.2),
    flierprops=dict(marker="o", markersize=4, alpha=0.4)
)
for patch, color in zip(bp["boxes"], pal.values()):
    patch.set_facecolor(color); patch.set_alpha(0.7)
add_strip(ax, A.sections.values, "A", 0, pal["A"])
add_strip(ax, B.sections.values, "B", 1, pal["B"])
ax.set_xticks([0, 1]); ax.set_xticklabels(["Version A\n(left panel)", "Version B\n(top panel)"])
ax.set_ylabel("Sections visited"); ax.set_title("Sections Visited per Session")
ax.grid(axis="y", alpha=0.3)
plt.tight_layout()
plt.savefig("figures/boxplot_sections.png", dpi=150)
plt.close()

# 5b. Boxplot: engagement time (minutes)
fig, ax = plt.subplots(figsize=(5, 4))
bp2 = ax.boxplot(
    [A.time_sec.values / 60, B.time_sec.values / 60],
    positions=[0, 1], patch_artist=True, widths=0.4,
    medianprops=dict(color="black", lw=2),
    whiskerprops=dict(lw=1.2), capprops=dict(lw=1.2),
    flierprops=dict(marker="o", markersize=4, alpha=0.4)
)
for patch, color in zip(bp2["boxes"], pal.values()):
    patch.set_facecolor(color); patch.set_alpha(0.7)
add_strip(ax, A.time_sec.values / 60, "A", 0, pal["A"])
add_strip(ax, B.time_sec.values / 60, "B", 1, pal["B"])
ax.set_xticks([0, 1]); ax.set_xticklabels(["Version A\n(left panel)", "Version B\n(top panel)"])
ax.set_ylabel("Engagement time (min)"); ax.set_title("Session Engagement Time")
ax.grid(axis="y", alpha=0.3)
plt.tight_layout()
plt.savefig("figures/boxplot_time.png", dpi=150)
plt.close()

# 5c. Bar chart: secondary binary metrics
metrics = {
    "Bounce\nRate": ("bounced", OR_b, p_b),
    "Export\nRate": ("reached_export", OR_e, p_e),
    "Completion\nRate": ("completed", OR_c, p_c),
}
rates_A = [A[c].mean() for c, *_ in metrics.values()]
rates_B = [B[c].mean() for c, *_ in metrics.values()]
x = np.arange(len(metrics)); w = 0.35
fig, ax = plt.subplots(figsize=(6, 4))
ax.bar(x - w/2, rates_A, w, color=pal["A"], alpha=0.85, label="Version A")
ax.bar(x + w/2, rates_B, w, color=pal["B"], alpha=0.85, label="Version B")
ax.set_xticks(x); ax.set_xticklabels(list(metrics.keys()))
ax.set_ylabel("Rate"); ax.set_title("Secondary Engagement Metrics by Version")
ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda v, _: f"{v:.0%}"))
ax.legend(); ax.grid(axis="y", alpha=0.3)
plt.tight_layout()
plt.savefig("figures/bar_secondary_metrics.png", dpi=150)
plt.close()

# 5d. Sections visited distribution (grouped bar)
sec_counts = long.groupby(["version", "sections"]).size().unstack(fill_value=0)
fig, ax = plt.subplots(figsize=(7, 4))
sec_counts.T.plot(kind="bar", ax=ax, color=[pal["A"], pal["B"]], alpha=0.85, width=0.7)
ax.set_xlabel("Sections visited"); ax.set_ylabel("Number of users")
ax.set_title("Distribution of Sections Visited per Version")
ax.legend(title="Version"); ax.grid(axis="y", alpha=0.3)
plt.xticks(rotation=0)
plt.tight_layout()
plt.savefig("figures/dist_sections.png", dpi=150)
plt.close()

print("\nFigures saved to figures/")

# ── 6. Save results ───────────────────────────────────────────────────────────

results = {
    "n_A": int(n_A), "n_B": int(n_B),
    "desc": desc.reset_index().to_dict(orient="records"),
    "shapiro": {
        "sections_A": {"W": round(float(sw_sec_A.statistic), 4), "p": round(float(sw_sec_A.pvalue), 4)},
        "sections_B": {"W": round(float(sw_sec_B.statistic), 4), "p": round(float(sw_sec_B.pvalue), 4)},
        "time_A":     {"W": round(float(sw_time_A.statistic), 4), "p": round(float(sw_time_A.pvalue), 4)},
        "time_B":     {"W": round(float(sw_time_B.statistic), 4), "p": round(float(sw_time_B.pvalue), 4)},
    },
    "tests": {
        "sections": {"U": float(U_sec), "p": round(p_sec, 4), "r": round(r_sec, 4)},
        "time":     {"U": float(U_time), "p": round(p_time, 4), "r": round(r_time, 4)},
        "bounce":   {"OR": round(OR_b, 4), "p": round(p_b, 4)},
        "export":   {"OR": round(OR_e, 4), "p": round(p_e, 4)},
        "complete": {"OR": round(OR_c, 4), "p": round(p_c, 4)},
    },
}
with open("results.json", "w") as f:
    json.dump(results, f, indent=2)
print("Results saved to results.json")
