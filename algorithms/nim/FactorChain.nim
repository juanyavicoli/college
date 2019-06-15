import unittest


proc factor_chain(values: openarray[int]): bool =
    for i in countdown(values.len() - 1, 1):
        if values[i] mod values[i - 1] != 0:
          return false

    return true


suite "factor_chain":
    test "equals":
        check factor_chain( [1,2] ) == true
        check factor_chain( [2,4,8] ) == true
        check factor_chain( [12,24,48] ) == true
        check factor_chain( [6,6,6] ) == true

        check factor_chain( [13,25] ) == false
        check factor_chain( [2,4,6] ) == false
        check factor_chain( [1,2,3] ) == false
        check factor_chain( [2,4,8,15] ) == false

