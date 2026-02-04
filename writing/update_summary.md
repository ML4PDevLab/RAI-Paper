# Update Summary (2026-02-04)

**Data Updates**
- Regenerated `writing/output/rai_latest.csv` from the new `mlp-data-intro` pipeline and standardized influencer labels (Russia/China/Combined).
- Synced `data/rai_vars.csv` to the new variable schema (including `id`).
- Regenerated `writing/event_themes.csv` and removed the diaspora category everywhere.
- Added the new classifier performance metrics from `new_rai_training/RAI_Training_05272025.ipynb`.

**Paper Revisions (Main Text)**
- Reframed *RAI* as a reporting-based measure and removed all “event data” language.
- Updated the taxonomy/table to “categories,” and aligned all narrative and captions to the new definitions.
- Strengthened scope conditions and limitations (aid‑receiving LMICs, media capacity/ownership bias, reporting salience).
- Added explicit note that AI‑assisted summaries are used only for audit/validation and do not feed into *RAI* measures.
- Updated classifier performance description with new accuracy/F1 metrics.

**Appendix Revisions**
- Renamed “Event Definitions” to “Category Definitions” and updated method language to match.
- Updated figure captions to refer to categories/reporting rather than events.
- Trimmed training figures to a small set (class distribution, classification report, normalized confusion matrix).

**Pipeline/Build**
- New script `code/refresh_rai_from_mlp_intro.R` produces paper-ready outputs from the updated pipeline.
- Local helper `code/local_sources.R` used to replace `ml4p.forecast` dependency in the paper build.
- Re-rendered `writing/full_paper.pdf` and `writing/appendix.pdf` with the new data and terminology.
