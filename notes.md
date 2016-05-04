# Notes on `download_extract_data_api.R`

1. Contributions can only processed if they follow the format "Contribution: INITIALS". As a consequence, all variables that are the result of processing contributions will be based only on those that follow this format.
2. Why? Because otherwise the extracted information becomes unreliable, and making highly specific code requires substantial amount of code for a(n expected) small part of the literature.
3. Do not extract all the contributions per se, but do use initials from all contributions to check whether first author is present
4. Check occurrence of initials in author list with those in contributions (assumes that author list inits are nested in contributions; RYI in contribution and RI in author list) and vice versa (assumes contribution inits are nested author list; RI in contribution and RYI author list)

