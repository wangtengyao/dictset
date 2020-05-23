# Overview
This package implements Dictionary and Set data types as S3 objects. Internally it uses hashed R environments. It allows for arbitrary objects to be used as keys and values. Both Dictionary and Set objects are passed by reference.

# Dictionary 
The following methods are implemented for Dictionary class.
* `isEmpty(dict)`: check if dictionary is empty
* `contains(dict, key)`: check if dictionary contains key
* `get(dict, key)`: get associated value for key
* `add(dict, key, val, overwrite=TRUE)`: add (key, val) pair to dictionary
* `copy(dict)`: make a copy of dictionary
* `clear(dict)`: remove all entries in dictionary
* `keys(dict)`: return a list of all keys
* `vals(dict)`: return a list of all values
* `pop(dict)`: remove an entry from dictionary and return its value

# Set 
The following methods are implemented for Set class.
* `isEmpty(set)`: check if set is empty
* `contains(set, element)`: check if set contains element
* `add(set, element)`: add element to set
* `copy(set)`: make a copy of set
* `clear(set)`: remove all entries in set
* `vals(set)`: return a list of all elements in set
* `pop(set)`: remove an entry from set and return its value

Example:
```r
library(magrittr)
dict <- Dictionary()
for (i in 1:10){
    dat <- data.frame(x=1:10, y=rnorm(10)+(1:10)*i/10)
    dict %>% add(dat, lm(y~x, data=dat))
}
dict
while (! dict %>% isEmpty){
    lm <- dict %>% pop
    print(unname(lm$coefficients[2]))
}

set <- Set()
set %>% add(dict)
set %>% add(123)
set
```


