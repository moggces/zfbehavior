# Worked example: Biobide per-plate Excel export (zebrafish DNT study)

This is ONE provider's validated format (a 127-plate batch, one `.xlsx`
per plate, single "Results" sheet). It is an example to speed up the column
mapping step -- **not a default**. Always inspect the user's own files and
confirm the mapping with the user, even if the headers look identical.

## Columns (23, in file order) and validated `col_types`

| # | Column | col_types | Note |
|---|--------|-----------|------|
| 1 | DTXSID | text | |
| 2 | CHEMICAL_NAME | text | |
| 3 | NIH_BLINDED_ID | text | 1:1 with plate in this study |
| 4 | BIOBIDE_ID | text | -> `chemical_id`; 1:1 with plate |
| 5 | VEHICLE | text | |
| 6 | PLATE_NAME | text | -> `plate_id` (see landmine 2) |
| 7 | WELL_POSITION | text | -> `well_id` |
| 8 | IS_VEHICLE | logical | -> `is_vehicle_control` ("T"/"F") |
| 9 | CONCENTRATION | numeric | -> `concentration` |
| 10 | CONCENTRATION_UNIT | text | -> `concentration_unit` |
| 11 | KEEP_WELL_DATA | logical | -> `keep_well` ("True"/"False") |
| 12 | HOURS_POST_EMBRYONIC | numeric | |
| 13 | BINNING | text | -> `time_value` via `parse_binning_to_time()` |
| 14 | PHASE | text | |
| 15 | DISTANCE_MOVED | numeric | -> `response_value` |
| 16 | DISTANCE_UNIT | text | |
| 17 | AVG_VELOCITY | numeric | |
| 18 | VELOCITY_UNIT | text | |
| 19 | COMMENTS | text | mostly empty -- must be typed explicitly |
| 20 | DEVELOPMENTAL_DEFFECTS | text | mostly empty -- must be typed explicitly |
| 21 | SOLVENT_CONCENTRATION | numeric | |
| 22 | SOLVENT_UNIT | text | |
| 23 | TA | text | |

## yml fragment

```yaml
input:
  pattern: "data/raw/260416/*.xlsx"
  format: one_file_per_plate
column_mapping:
  plate_id: PLATE_NAME
  well_id: WELL_POSITION
  chemical_id: BIOBIDE_ID
  is_vehicle_control: IS_VEHICLE
  concentration: CONCENTRATION
  concentration_unit: CONCENTRATION_UNIT
  response_value: DISTANCE_MOVED
  keep_well: KEEP_WELL_DATA
optional_roles: [time_value]
derive_time_value_from_binning: true
col_types: [text, text, text, text, text, text, text, logical, numeric, text,
            logical, numeric, text, text, numeric, text, numeric, text, text,
            text, numeric, text, text]
plate_qc:
  expected_row_count: 4800   # 96 wells x 50 one-minute bins
  expected_vc_wells: 16
  expected_dose_wells: 16
dataset_qc:
  unique_id_cols: [chemical_id, NIH_BLINDED_ID]
  expected_n_distinct: {CHEMICAL_NAME: 123, DTXSID: 123}  # study-specific!
```

## Landmines seen with this provider

1. **Type guessing.** Without explicit `col_types`, `readxl` guessed `COMMENTS`
   and `DEVELOPMENTAL_DEFFECTS` as logical in some files and silently turned
   real text (e.g. `"dead not hatching"`) into `NA`. `read_behavior_data()` now
   treats such coercions as fatal, but explicit types avoid the failure.
2. **`PLATE_NAME` is not guaranteed unique.** In an earlier export
   small run numbers were reused across files, and one file had wells labeled
   with a different `PLATE_NAME`. A re-export fixed this. Always
   keep `unique_id_cols` (1:1 with plate) in the dataset-level QC, and never
   repair identifiers silently -- ask the user / provider.
3. **`BINNING` labels.** The first bin may read `"0:00:00-0:01:00"` or
   `"Start-0:01:00"`; both parse to `time_value = 1`.
4. **Replicated chemicals.** 127 plates but 123 distinct chemicals (two
   chemicals run on 3 plates each). `expected_n_distinct` must reflect what
   the user expects for their batch.
