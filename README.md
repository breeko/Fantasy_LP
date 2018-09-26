# Fantasy_LP

## Files

### history.R: Allows you to download history performance

Usage:

``` 
# downloads history from 2014 - today
history <- updateHistory()

# update history
history <- updateHistory(history)

# saves the history (default "history.csv")
saveHistory(history, "history.csv")

# returns latest history (default "history.csv")
history <- getLatestHistory()
```

### available.R: Downloads the latest lineup and formats it to history format so that it can be used in analysis.

Usage:
```
# Must provide a WEEKS_ID. 
# I don't have an easy way to get it other than log in to the contestand heck the api request sent to api.draftkings.com (e.g. Request URL: https://api.draftkings.com/draftgroups/v1/21720?format=json)

WEEKS_ID = 21720

# Get latest available for weeks_id 21720
available <- getLatestAvailable(21720)

# Since your model would likely be trained on historic format, you want to convert available to historic format
availableHistory <- availableToHistory(available, 3, 2018)

# Historic format has "first last" name convention and lower-case 3 letter team abbreviations as defined by mappings.R
```

### solve.R: Uses linear programming to optimize selections

Usage:
```
# Solves for optimal selection based on $prediction. Must have $prediction, $pos and $salary
# Assumes 1 qb, 2 rb, 3 wr, 1 te, 1 def and 1 flex

# Make some predictions about score for this week
availableHistory$prediction <- availableHistory$salary / 500.0 + 10.0

# Solve
results <- solve(availableHistory, budget=50000)
```
|     |       displayName | week | pos | team | oppt | h.a | teamYear |  opptYear |  salary |  prediction |
|-----|-------------------|------|-----|------|------|-----|----------|-----------|---------|-------------|
| 8   |     Alex Erickson | 3    |  WR |  cin |  atl |   a | cin-2018 |  cin-2018 | 3000    | 16          |
| 13  |      Alvin Kamara | 3    |  RB |  nor |  nyg |   a | nor-2018 |  nor-2018 | 9600    | 29.2        |
| 17  |     Andre Roberts | 3    |  WR |  njg |  jac |   a | njg-2018 |  njg-2018 | 3000    | 16          |
| 22  |    Antony Auclair | 3    |  TE |  tam |  chi |   a | tam-2018 |  tam-2018 | 2500    | 15          |
| 37  |           Buffalo | 3    | Def |  buf |  gnb |   a | buf-2018 |  buf-2018 | 2000    | 14          |
| 111 |        Derek Carr | 3    |  QB |  oak |  cle |   h | oak-2018 |  oak-2018 | 5100    | 20.2        |
| 204 |       Julio Jones | 3    |  WR |  atl |  cin |   h | atl-2018 |  atl-2018 | 8200    | 26.4        |
| 210 |      Keenan Allen | 3    |  WR |  lac |  sfo |   h | lac-2018 |  lac-2018 | 8300    | 26.6        |
| 253 | Melvin Gordon III | 3    |  RB |  lac |  sfo |   h | lac-2018 |  lac-2018 | 8300    | 26.6        |


### mappings.R: useful mappings to convert from available format to history format for fields (e.g. "NO" vs "nor")

### utils.R: Utilities referenced by various files


## Complete Example

```
source("history.R")
source("solve.R")
source("available.R")

WEEKS_ID <- 21720

h <- getLatestHistory()
a <- getLatestAvailable(WEEKS_ID)
ah <- availableToHistory(a, 3, 2018)
ah$prediction <- ah$salary / 500.0 + 10
sol <- solve(ah)
```