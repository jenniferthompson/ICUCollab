---
title: "Codebook for ICU Liberation Time Series Data"
output:
  pdf_document
---

# General Info

There are seven elements of the ABCDEF Bundle (element B has two components).
Each quantity is looked at in terms of 1) **compliance** and 2) **performance**.
For elements A, C, D, and F, these are the same, but each has a column in the
dataset for convenience. Full definitions of compliance and performance are
included at the end of this document.

The dataset has columns for each individual element as well as overall
compliance/performance (yes/no) and "dose" of compliance/performance (number of
elements compliant/performed divided by the number of elements eligible to be
done).

On days the patient was in the ICU a full 24 hours,

- Patients were *always* eligible to have elements A, C, D, and E done.
- Element B - SAT could only be done if the patient was receiving continuous or intermittent sedation.
- Element B - SBT could only be done if the patient was receiving mechanical ventilation.
- Element F could only be done if family were present in the ICU.

**Note**: In the progress reports and so far in the analysis document, months
1-6 have been **combined** into a single "baseline" time period. Numbers for
each baseline month individually are provided in the dataset, but for
consistency and clinical relevance, you may want to combine them.

# Numerators and Denominators for Each Element

| Quantity/bundle element | Numerator, **compliance** |  Numerator, **performance** | Denominator (both) |
|-------------------|---------------|---------------|---------------|
| Overall, yes/no | `comp_yn` | `perf_yn` | `icu_day` |
| Overall, dose | `elements_comp` | `elements_comp` | `elements_elig` |
| Element A | `comp_a` | `perf_a` | `icu_day` |
| Element B - SAT | `comp_b_sat` | `perf_b_sat` | `on_sedation_icu` |
| Element B - SBT | `comp_b_sbt` | `perf_b_sbt` | `on_mv_icu` |
| Element C | `comp_c` | `perf_c` | `icu_day` |
| Element D | `comp_d` | `perf_d` | `icu_day` |
| Element E | `comp_e` | `perf_e` | `icu_day` |
| Element F | `comp_f` | `perf_f` | `family_present_icu` |

# Definitions of Compliance and Performance {#definitions}

There are six main elements of the ABCDEF Bundle, with element B comprised of
two main components. Definitions of "compliance" (completely following the ABCDEF Bundle protocol as specified, with documentation of each step) and "performance" (performing the main component of the Bundle element, possibly without complete documentation) are detailed below, referring to any single day. For each element,
compliance and performance are only measured if the patient was in the ICU the
full 24 hours; additional eligibility for specific elements are detailed below.
More details are in the analysis plan, found [here](https://docs.google.com/document/d/1GebrA6Lt-rEh70Aw9r4mrhRlggWUfgUl_LaQePOCWZA/edit?usp=sharing).


| Bundle element    | Days eligible | Compliant if: | Performed if: |
|-------------------|---------------|---------------|---------------|
| **A: Assess, prevent and manage pain**  | | >=6 documented pain assessments using a validated instrument | *same* |
| **B: Both SAT and SBT** |   |   |   |
| &nbsp;&nbsp;&nbsp;SAT | Received continuous/ intermittent sedation | **If safety screen documented:**<br>If screen failed, SAT not performed; if screen passed, SAT was performed<br>**If safety screen not documented:**<br>SAT marked not done due to  failed screen/contraindication | SAT performed and documented |
| &nbsp;&nbsp;&nbsp;SBT | On mechanical ventilation | **If safety screen documented:**<br>If screen failed, SBT not performed; if screen passed, SBT was performed<br>**If safety screen not documented:**<br>SBT marked not done due to  failed screen/contraindication | SBT performed and documented |
| **C: Choice of analgesia and sedation** | | >=6 documented sedation assessments using a validated instrument | *same* |
| **D: Delirium - assess, prevent and manage** | | >=2 documented delirium assessments using a validated instrument | *same* |
| **E: Exercise and early mobility** | | **If safety screen documented:**<br>If screen failed, mobility not performed; if screen passed, mobility higher than active range of motion was performed<br>**If safety screen not documented:**<br>Mobility marked not done due to failed screen/contraindication | Mobility higher than active range of motion performed and documented |
| **F: Family engagement and empowerment** | Family present | Family member was educated on ABCDEF bundle and/or participated in at least one of: rounds; conference; plan of care; ABCDEF care | *same* |
| **Overall (complete)** | | All elements eligible to be compliant are compliant | All elements eligible to be performed are performed |
| **Overall (dose)** | | Elements compliant / elements eligible to be compliant | Elements performed / elements eligible to be performed |
