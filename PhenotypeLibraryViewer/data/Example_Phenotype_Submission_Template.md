# Author Submission Template (Example)

<style>
  .flat-table {
    border: 1px solid #1C6EA4;
    background-color: #EEEEEE;
    width: 100%;
    text-align: left;
    border-collapse: collapse;
  }
  td, th{
    border: 1px solid #AAAAAA;
    padding: 3px 2px;
  }
  thead {
    background: #ffeabf;
    background: -moz-linear-gradient(top, #f9d895 0%, #ffe1a5 66%, #ffeabf 100%);
    background: -webkit-linear-gradient(top, #f9d895 0%, #ffe1a5 66%, #ffeabf 100%);
    background: linear-gradient(to bottom, #f9d895 0%, #ffe1a5 66%, #ffeabf 100%);
    border-bottom: 2px solid #444444;
  }
  tbody {
    font-size: 16px;
    color: #000000;
  }  
  tr:nth-child(even) {
    background: #edfaff;
  }
  thead th {
    font-size: 20px;
    font-weight: bold;
    color: #000000;
    border-left: 2px solid #D0E4F5;
  }
</style>

## Summary
| Characteristic | Entry |
| ------ | ------ |
| Phenotype Title | Rheumatoid Arthritis |
| Author(s) and Affiliations | Jane Doe, Example University </br> John Doe, Example University |
| Date of Submission | March 21, 2019 |
| Modality | Computable |

## Source Data
| Link Type | Link |
| ------ | ------ |
| Phenotype GitHub Page | https://www.github.com |
| Implementation File | https://www.github.com |
| Hash of Implementation File | 7245cf0ee90b52deb5b9965f42a5f32cff585d29 |
| Configuration File | https://www.github.com |

## Development
### Purpose and Intended Use
This definition is intended to capture patients with a first-observed diagnosis of chronic rheumatoid arthritis (RA), taking care to rule out patients with short-term joint pain or fibromyalgia. Please note this definition is intended to be used with US-only data.
### Development Methodology
This phenotype was developed by a group of 4 expert rheumatologists, who gathered and identified key condition codes that manifest in individuals with RA, along with drug combinations that almost exclusively identify RA patients.
### Additional Author Comments (if any)
...

### Process Diagram / Flowchart
![process diagram](../data/example_diagram.png)

## Dependencies
### CDM-Based
**OMOP CDM Version Number: v6.0**

| Data Type | Uses? |
| ------ | ------ |
|Conditions| Yes |
|Drug Exposures| No |
|Labs| Yes |
|Notes NLP| No |
|Observations| Yes |
|Procedures| Yes |
|Visits| Yes |

### Demographic-Based
| Data Type | Uses? |
| ------ | ------ |
|Gender| No |
|Age Category| Yes |

### Provenance
Implementation(s) this definition was derived from or inspired by, if any:

| Definition Title | Definition Link  | Definition Hash | Provenance Reason |
| ------ | ------ | ------ | ------ |
| Rheumatoid Arthritis | https://www.github.com | cfa99770da20b404a28c3241defec892a7c542c3 | Prior Version of the Same Definition |
| Joint Pain | https://www.github.com | 2c661a0e5ded1d0c4b9ee8988ad047d9055630f3 | Concepts from this definition are used |

### References
