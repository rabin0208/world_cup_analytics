# Player ratings Parquet export schema

## `player_ratings_2022_2026.parquet`

**Grain:** One row per ``player_id`` per ``year``.

**Column order** in the Parquet file matches the export query (table below, top to bottom).

**Parquet schema nullability:** With pandas ``df.to_parquet`` (PyArrow engine), Arrow fields are typically **marked nullable for every column**, even when values are fully populated. Treat that as encoding detail, not proof that missing values are expected in downstream logic. Columns that actually carry **NULL values** in the export are called out in the type column below.

| Column | Parquet / pandas type | Description |
| :--- | :--- | :--- |
| `player_id` | INT32 | Surrogate key: one integer per **distinct** mart ``player_id`` string (``row_number()`` ordered lexicographically on that VARCHAR). Re-reads can renumber if the mart's distinct-id set changes. |
| `player_name` | STRING | Player display name |
| `nationality` | STRING | Nationality or country label |
| `club` | STRING | Club name |
| `league` | STRING | League label after export normalization (trim whitespace; strip trailing `` (1)`` / `` (2)``-style tier suffixes; collapse repeated spaces) |
| `positions` | STRING | Listed playing positions as text |
| `primary_position` | STRING | Primary position code |
| `year` | INT32 | Calendar year from the slug; rows are restricted to ``EXPORT_TITLE_YEARS`` (**2022**–**2026**) in code |
| `overall` | INT64 | Overall rating |
| `age` | INT64 (nullable) | Age in years; null when missing from the source for that player-year (often coexists with null ``height_cm`` / ``weight_kg``) |
| `height_cm` | INT64 (nullable) | Height in centimeters; null when missing from the source |
| `weight_kg` | INT64 (nullable) | Weight in kilograms; null when missing from the source |
| `pace` | INT64 | Pace rating |
| `shooting` | INT64 | Shooting rating |
| `passing_rating` | INT64 | Passing rating |
| `dribbling` | INT64 | Dribbling rating |
| `defending` | INT64 | Defending rating |
| `physical` | INT64 | Physical rating |
| `dribbling_attr` | INT64 | Fine-grained dribbling attribute |
| `acceleration` | INT64 | Acceleration attribute |
| `sprint_speed` | INT64 | Sprint speed attribute |
| `finishing` | INT64 | Finishing attribute |
| `shot_power` | INT64 | Shot power attribute |
| `long_shots` | INT64 | Long shots attribute |
| `volleys` | INT64 | Volleys attribute |
| `penalties` | INT64 | Penalties attribute |
| `vision` | INT64 | Vision attribute |
| `crossing` | INT64 | Crossing attribute |
| `free_kick_accuracy` | INT64 | Free kick accuracy attribute |
| `short_passing` | INT64 | Short passing attribute |
| `long_passing` | INT64 | Long passing attribute |
| `curve` | INT64 | Curve attribute |
| `agility` | INT64 | Agility attribute |
| `balance` | INT64 | Balance attribute |
| `reactions` | INT64 | Reactions attribute |
| `ball_control` | INT64 | Ball control attribute |
| `composure` | INT64 | Composure attribute |
| `positioning` | INT64 | Attacking positioning attribute |
| `interceptions` | INT64 | Interceptions attribute |
| `heading_accuracy` | INT64 | Heading accuracy attribute |
| `defensive_awareness` | INT64 (nullable) | Defensive awareness attribute; null when missing from the source row |
| `standing_tackle` | INT64 | Standing tackle attribute |
| `sliding_tackle` | INT64 | Sliding tackle attribute |
| `jumping` | INT64 | Jumping attribute |
| `stamina` | INT64 | Stamina attribute |
| `strength` | INT64 | Strength attribute |
| `aggression` | INT64 | Aggression attribute |
| `gk_diving` | INT64 (nullable) | Goalkeeper diving attribute; null when not present on the source row |
| `gk_handling` | INT64 (nullable) | Goalkeeper handling attribute; null when not present on the source row |
| `gk_kicking` | INT64 (nullable) | Goalkeeper kicking attribute; null when not present on the source row |
| `gk_positioning` | INT64 (nullable) | Goalkeeper positioning attribute; null when not present on the source row |
| `gk_reflexes` | INT64 (nullable) | Goalkeeper reflexes attribute; null when not present on the source row |
| `world_cup_squad_tournament_year` | INT64 (nullable) | World Cup tournament year from the mart |
| `was_world_cup_squad_member` | BOOL | World Cup squad membership flag (pandas export still marks the Arrow field nullable; **values** are true/false for every row in the current file) |

Physical Arrow types follow pandas ``df.to_parquet`` (PyArrow engine) after ``fetchdf()`` from DuckDB: outfield attributes are signed 64-bit integers; ``player_id`` and ``year`` are 32-bit integers in typical runs. Any column can use Parquet/Arrow null slots where the warehouse or join leaves a SQL NULL—see the ``(nullable)`` markings above. GK and sparse mart fields intentionally carry many nulls for non-goalkeepers or non-eligible rows; other INT64 ratings are populated for all rows in the current export aside from ``age``, ``height_cm``, ``weight_kg``, and ``defensive_awareness``.
