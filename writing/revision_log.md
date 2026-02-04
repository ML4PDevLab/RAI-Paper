# Revision Log

**2026-02-04**
- Updated the pipeline to read the new `mlp-data-intro` outputs, standardized influencer labels, and regenerated `writing/output/rai_latest.csv` and `data/rai_vars.csv`.
- Removed the diaspora event from the data and narrative; updated event definitions and theme mappings across the paper and appendix.
- Replaced `ml4p.forecast` dependencies with a local `code/local_sources.R` helper and made missing-country source selection non-fatal.
- Regenerated figures and successfully rendered `writing/full_paper.pdf` with the updated data and definitions.

**2026-02-04 (IO feedback revisions)**
- Reframed the introduction to clarify scope (aid-receiving LMICs), tradeoffs, and the value of a multi-dimensional influence measure.
- Strengthened theoretical motivation for tool choice and added context on recent qualitative accounts of influence behavior.
- Clarified that *RAI* measures *reported* activity, not verified events; added a validation/benchmarking discussion and limits of media-based data.
- Updated the event taxonomy table to the new 21-category definitions and added a note on the `-999` placeholder.
- Revised figure descriptions to align with the updated plots and to avoid over-interpretation of relative shares.
- Expanded the Discussion to address scope, media bias, salience, and comparison to existing datasets (AidData, GDELT).
- Fixed typos and citation issues, including “language,” “onset,” and the `@blair2022foreign` citation.

**2026-02-04 (Classifier update)**
- Updated model description to the new ModernBERT-large classifier trained on 10,329 labeled articles.
- Added cross-validation and test-set performance metrics (accuracy, macro precision/recall/F1, weighted F1) based on the new training figures.
- Documented input construction (title + first sentences, adaptive length, 256 tokens) and class-weighted training to address imbalance.

**2026-02-04 (Training figures added)**
- Added a new appendix section "Classifier Training and Evaluation" and embedded training/validation figures.
- Copied training figures into `writing/figures/training/` with normalized filenames for portability.

**2026-02-04 (Terminology + rigor pass)**
- Removed "event data" framing for *RAI* throughout the paper and appendix; shifted wording to "categories," "reporting," and "influence activity."
- Renamed "Event Definitions" to "Category Definitions" and updated figure captions and methods language to match.
- Clarified scope conditions, interpretation (reporting salience vs. verified actions), and the descriptive nature of the analyses.
- Added an explicit note that AI-assisted summaries are used only for audit/validation and do not feed into *RAI* measures.
- Expanded limitations to note cross-country differences in media capacity and the resulting caution for comparisons.
