# Code Audit — `resazurin-dashboard.qmd`

**File:** `resazurin-dashboard.qmd` (1,835 lines)
**Date:** 2026-05-06

The file is a Quarto document with three execution contexts: a YAML header, one R chunk that gates rendering, and a single ~1,770-line OJS (Observable JS) chunk that builds the entire interactive dashboard. The dashboard works, but the OJS chunk has accumulated significant technical debt: large blocks of duplicated logic, dead code, one labeling bug, and a few ergonomic and performance issues. Nothing is on fire, but the file is approaching the point where a small change will be hard to make safely.

## Top findings (prioritized)

### 1. Significance-label vertical positions are computed from the wrong order — bug

In the inner statistics block inside the rendering IIFE (lines 1685–1755), `aucSignificanceLabels` is built like this:

```javascript
return aucSeriesIds.map((seriesId, index) => ({
  series_id: seriesId,
  sig_label: (lettersBySeries.get(seriesId) || []).join(""),
  label_y: labelY + index * labelStep
}))
```

The outer/earlier copy of the same computation in `aucStatsContext` (lines 922–926) maps over `orderedSeries` (sorted by significance degree, then median) — which is correct, because `labelY + index * labelStep` is meant to stack labels by visual ordering. The inner copy iterates `aucSeriesIds` (the unsorted set), so the stacked vertical offsets don't line up with the visually sorted dots.

In practice the inner `aucSignificanceLabels` is **never read** (see #2), so the bug is latent — but it will activate the moment someone refactors the rendering to use the locally-computed labels. Either delete the dead block or fix the iteration target to `orderedSeries`.

### 2. The entire stats-context computation is duplicated, and the duplicate is dead

Lines 828–946 define `aucStatsContext` as an IIFE that produces `statsArr`, `statsGroupCol`, `relevantStats`, `kwStats`, `pairwiseStats`, `significantPairs`, `kwBest`, `analysisUnitType`, `formatP`, `aucSignificanceLabels`, and `aucSignificanceNote`.

Lines 1662–1759 then recompute every one of those values inside the rendering IIFE. The render block uses its locally-recomputed `kwBest`, `significantPairs`, `pairwiseStats`, `formatP`, `analysisUnitType`, `relevantStats`, and `statsGroupCol` for the summary HTML and table — but it **never reads** the locally-recomputed `aucSignificanceLabels` or `aucSignificanceNote`. Those two consumers (the plot text marks at line 1480 and the subtitle line at line 1098) read from the outer `aucStatsContext`.

So roughly 70 lines of identical logic is computed twice, and ~30 lines of it is computed and thrown away. This is the single most worthwhile cleanup: drop the inner re-computation entirely and read from `aucStatsContext` everywhere.

### 3. The render IIFE is ~770 lines and inlines four entire plot definitions

Lines 1063–1834 are a single IIFE that handles unavailable-data messages, builds the subtitle DOM, defines a 300-line `attachHoverEmphasis` helper, and then has four parallel `Plot.plot(...)` definitions (AUC, cumulative AUC, box, line) with nearly identical scaffolding (height, style, color domain/range, gridX/gridY). After the plot it builds the stats summary and table.

This is the single biggest maintainability risk in the file. Recommend splitting into named OJS cells:
- `attachHoverEmphasis` as its own cell (so it's reusable and tested in isolation).
- One cell per plot type that returns a `Plot.plot` node, parameterized by the data view.
- A small dispatcher that picks which plot to render.
- A separate cell for the stats summary/table block.

This also gives Observable's reactive system finer-grained invalidation — currently any change to any input re-runs the whole render block.

### 4. Dependency-order reliance in OJS makes the reading order misleading

`isBlank` is referenced on lines 196 and 280 but defined on line 392. `effectiveSeriesValues` is referenced on line 253 but defined on line 276. OJS resolves cells by dependency graph rather than textual order, so this works — but the file reads top-to-bottom like a script, and a developer porting any of this logic to plain JS would have an immediate problem. Move definitions above their first use.

### 5. `attachHoverEmphasis` depends on Observable Plot's internal class names

Line 1293 selects legend swatches with `span[class*='-swatch'], label[class*='-swatch'], [style*='--color']`. Plot's class naming has changed across versions before. If/when Plot is upgraded, the legend hover behavior will silently stop working. Two options:
- Add a small comment noting the version of Plot this was written against.
- Use Plot's documented legend hooks (`color: { legend: true }` exposes the legend node via the returned plot's `.legend` element) instead of DOM-class scraping.

### 6. `plotWrap.appendChild(subtitleWrap); plotWrap.innerHTML = ""; plotWrap.appendChild(subtitleWrap);`

Lines 1115–1117. The first `appendChild` is wiped on the next line, then re-appended. It's harmless but reads as a leftover from a refactor. Delete the first `appendChild`.

### 7. Performance — repeated linear scans over the full dataset

Several computations rebuild filtered arrays from scratch on every input change:
- `selectionRows`, `seriesRows`, `filtered`, `excludedCandidateRows`, `plottedBase`, `plotted` are five sequential filter passes over `cleanDashboard`.
- `selectedWells.includes(d.well_id)` (lines 268, 300) is O(n) per row; should be a `Set`.
- `[...new Set(...)]` rebuilds for `wells`, `groupValues`, `effectiveSeriesValues`, `presentSeries`, `boxPresentSeries` — fine for small datasets, but could be precomputed once via `d3.group` for a multi-thousand-row CSV.
- `attachHoverEmphasis`'s `findHoveredNode` walks every line/path on every `mousemove`, calling `getTotalLength`/`getPointAtLength` ~140 times per path. With many traces this can get visible; consider memoizing path lengths and using bounding-box pre-filtering more aggressively.

These probably aren't user-visible at current data sizes, but they're worth knowing about before the dataset grows.

### 8. Regex tolerates a typo instead of fixing it

Line 212: `/_measur(?:e)?ment$/` exists to accommodate columns named `*.measurment` (missing 'e') as well as `*.measurement`. The dashboard is the wrong layer to absorb that. Fix it in the upstream layout-metadata writer / `build_resazurin_dashboard_data.R`, then tighten the regex.

### 9. The "precomputed AUC" branch reports `n_timepoints` incorrectly

Lines 561–576: when `auc_value` is present in the CSV, the code counts rows-with-finite-auc as `n_timepoints`. But `auc_value` is one number per trajectory; it's typically attached to every row of that trajectory by the upstream script, so `n_timepoints` ends up being the row count of the trajectory, not the integration window length. The tooltip then shows `Timepoints used: <row count>`, which conflates "rows" with "timepoints" and may not match what the on-the-fly branch reports. Either compute the actual unique-time count, or drop the field for the precomputed branch.

### 10. Smaller items worth a sweep

- Lines 559, 563, 620, 624: `d.auc_value !== undefined && d.auc_value !== null && Number.isFinite(d.auc_value)` — `Number.isFinite` already returns false for null/undefined; the guards are redundant.
- Line 445: `paperMetabolismValue` rejects non-positive measurements but allows negative `initVal`. Decide whether negative initial fluorescence should be a NaN.
- Line 101: `plates = ["All", ...new Set(...)]` is unsorted. Sorting (`.sort(naturalCompare)`) gives users a stable ordering.
- Lines 12–21: the inline `<style>` block is a workaround for Quarto's chunk-hiding. Using `#| echo: false` (already set on line 60) and Quarto's normal CSS should be sufficient — the manual `.hidden` overrides hint at a past rendering bug worth re-checking.
- Line 9: `<!-- deploy trigger -->` HTML comment as a deployment poke. Fine, but a comment explaining why it's there would help the next person.
- Line 1801: the table header omits a `<thead>`/`<tbody>` split — fine, but it makes the table less stylable from CSS.
- Indentation drifts inside `aucStatsContext` (lines 862–914 sit at deeper indent than their surroundings); a Prettier pass over the OJS would normalize this and make diffs cleaner.
- The R chunk (lines 23–47) reads the CSV only to test for existence and emptiness, then does nothing with it — `file.exists()` plus `file.size()` would skip the parse.

## Suggested order of work

1. Delete the duplicated stats block (item 2). Single biggest readability win, also fixes the latent label bug (item 1).
2. Extract `attachHoverEmphasis` and the four plot builders into separate OJS cells (item 3).
3. Move `isBlank` and `effectiveSeriesValues` above their first use (item 4).
4. Fix the upstream `measurment`/`measurement` typo and tighten the regex (item 8).
5. Address the smaller items in a single style-pass commit.

Performance items (7) and the Plot-class-name fragility (5) are worth doing once the structural cleanup is in — both become easier when the render block is decomposed.
