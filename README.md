# Smart Revenue Optimisation Survey — Nigeria (36 States + FCT)

## Project Overview
This repository contains the survey dataset, R analysis scripts, and outputs for the **Smart Revenue Optimisation Project**, a nationwide research effort assessing revenue generation, collection efficiency, and optimisation opportunities across Nigeria. The study covers **14,000 respondents** drawn from all **36 states** and the **Federal Capital Territory (FCT)**, with analysis conducted at the **geopolitical zone level**.

## Objectives
- Assess current revenue collection practices and gaps across Nigeria's states and FCT.
- Identify zone-level patterns, disparities, and opportunities for revenue optimisation.
- Generate actionable, data-driven recommendations for policymakers and revenue authorities.
- Provide a reproducible analytical pipeline (in R) for ongoing or future survey waves.

## Geographic Coverage
Respondents are grouped into Nigeria's six geopolitical zones for analysis:

| Zone | States Covered |
|------|-----------------|
| North Central | Benue, Kogi, Kwara, Nasarawa, Niger, Plateau, FCT |
| North East | Adamawa, Bauchi, Borno, Gombe, Taraba, Yobe |
| North West | Jigawa, Kaduna, Kano, Katsina, Kebbi, Sokoto, Zamfara |
| South East | Abia, Anambra, Ebonyi, Enugu, Imo |
| South South | Akwa Ibom, Bayelsa, Cross River, Delta, Edo, Rivers |
| South West | Ekiti, Lagos, Ogun, Ondo, Osun, Oyo |

> **Note:** Confirm this table matches the exact zone mapping used in your dataset before publishing — adjust if your project uses a different classification.

## Dataset
- **Sample size:** 14,000 respondents
- **Unit of analysis:** Individual survey respondent, aggregated to state and zone level
- **Coverage:** All 36 states + FCT
- **Format:** [CSV / SPSS / Excel — *specify actual format*]
- **Key variables:** [e.g., state, zone, revenue source, collection method, satisfaction score, compliance rate — *update with your actual variable names*]

Data files are stored in the `data/` directory:
```
data/
├── raw/            # Original, unmodified survey export
├── cleaned/        # Cleaned and validated dataset used for analysis
└── codebook.xlsx   # Variable definitions and coding scheme
```

## Methodology
Analysis was conducted in **R**, following this general workflow:
1. **Data cleaning** — handling missing values, recoding variables, validating state/zone mappings.
2. **Descriptive analysis** — response distribution across states and zones.
3. **Zonal aggregation** — grouping state-level data into the six geopolitical zones.
4. **Statistical analysis** — [e.g., comparative tests, regression, correlation — *update based on your actual analysis*].
5. **Visualisation** — charts and maps summarising findings by zone and state.

