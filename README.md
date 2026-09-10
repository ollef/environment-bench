# environment-bench

Benchmarks for dictionaries keyed by a small number (<= 20) of consecutive
integers where 0 maps to the most recently added element, as typically used in
De Bruijn-indexed environments in compilers.

* `extension/n`: Create an environment by extending it n times
* `lookup/n`: Lookup up all variables [0..n - 1] in an environment of size n
* `combined/n`: Create an environment of size n and lookup all variables

## TL;DR:

Use lists at these sizes.

## Full results:

```
All
  combined
    1
      Data.List:                 OK
        5.28 ns ± 408 ps
      Data.Vector:               OK
        5.19 ns ± 418 ps
      Data.Map:                  OK
        5.18 ns ± 412 ps
      Data.IntMap:               OK
        5.51 ns ± 502 ps
      Data.HashMap.Lazy:         OK
        5.20 ns ± 406 ps
      Data.Sequence:             OK
        5.34 ns ± 430 ps
      Data.SkewList.Lazy:        OK
        5.32 ns ± 476 ps
      Data.Primitive.SmallArray: OK
        5.23 ns ± 494 ps
    3
      Data.List:                 OK
        21.5 ns ± 1.9 ns
      Data.Vector:               OK
        42.1 ns ± 3.4 ns
      Data.Map:                  OK
        40.9 ns ± 3.2 ns
      Data.IntMap:               OK
        36.2 ns ± 2.2 ns
      Data.HashMap.Lazy:         OK
        55.0 ns ± 3.8 ns
      Data.Sequence:             OK
        29.1 ns ± 1.7 ns
      Data.SkewList.Lazy:        OK
        28.9 ns ± 1.7 ns
      Data.Primitive.SmallArray: OK
        32.5 ns ± 1.8 ns
    5
      Data.List:                 OK
        38.1 ns ± 3.5 ns
      Data.Vector:               OK
        73.6 ns ± 3.9 ns
      Data.Map:                  OK
        93.6 ns ± 6.7 ns
      Data.IntMap:               OK
        67.4 ns ± 2.2 ns
      Data.HashMap.Lazy:         OK
        107  ns ± 9.3 ns
      Data.Sequence:             OK
        47.4 ns ± 4.0 ns
      Data.SkewList.Lazy:        OK
        50.4 ns ± 3.3 ns
      Data.Primitive.SmallArray: OK
        56.1 ns ± 3.7 ns
    7
      Data.List:                 OK
        59.5 ns ± 1.9 ns
      Data.Vector:               OK
        107  ns ± 9.6 ns
      Data.Map:                  OK
        152  ns ±  10 ns
      Data.IntMap:               OK
        104  ns ± 8.4 ns
      Data.HashMap.Lazy:         OK
        158  ns ±  13 ns
      Data.Sequence:             OK
        98.2 ns ± 8.0 ns
      Data.SkewList.Lazy:        OK
        75.7 ns ± 4.6 ns
      Data.Primitive.SmallArray: OK
        80.4 ns ± 6.8 ns
    10
      Data.List:                 OK
        101  ns ± 6.5 ns
      Data.Vector:               OK
        159  ns ±  13 ns
      Data.Map:                  OK
        251  ns ±  15 ns
      Data.IntMap:               OK
        157  ns ±  14 ns
      Data.HashMap.Lazy:         OK
        244  ns ±  13 ns
      Data.Sequence:             OK
        174  ns ±  16 ns
      Data.SkewList.Lazy:        OK
        110  ns ± 8.2 ns
      Data.Primitive.SmallArray: OK
        118  ns ±  10 ns
    15
      Data.List:                 OK
        213  ns ±  17 ns
      Data.Vector:               OK
        253  ns ±  13 ns
      Data.Map:                  OK
        449  ns ±  28 ns
      Data.IntMap:               OK
        256  ns ±  15 ns
      Data.HashMap.Lazy:         OK
        386  ns ±  27 ns
      Data.Sequence:             OK
        319  ns ±  26 ns
      Data.SkewList.Lazy:        OK
        181  ns ±  15 ns
      Data.Primitive.SmallArray: OK
        188  ns ± 7.3 ns
    20
      Data.List:                 OK
        433  ns ±  27 ns
      Data.Vector:               OK
        368  ns ±  14 ns
      Data.Map:                  OK
        671  ns ±  52 ns
      Data.IntMap:               OK
        353  ns ±  27 ns
      Data.HashMap.Lazy:         OK
        541  ns ±  54 ns
      Data.Sequence:             OK
        428  ns ±  29 ns
      Data.SkewList.Lazy:        OK
        282  ns ±  28 ns
      Data.Primitive.SmallArray: OK
        261  ns ±  14 ns
  extension
    1
      Data.List:                 OK
        4.61 ns ± 428 ps
      Data.Vector:               OK
        13.8 ns ± 804 ps
      Data.Map:                  OK
        4.66 ns ± 402 ps
      Data.IntMap:               OK
        6.29 ns ± 404 ps
      Data.HashMap.Lazy:         OK
        6.31 ns ± 404 ps
      Data.Sequence:             OK
        5.62 ns ± 412 ps
      Data.SkewList.Lazy:        OK
        6.13 ns ± 418 ps
      Data.Primitive.SmallArray: OK
        10.2 ns ± 814 ps
    3
      Data.List:                 OK
        6.82 ns ± 424 ps
      Data.Vector:               OK
        39.5 ns ± 3.0 ns
      Data.Map:                  OK
        32.0 ns ± 1.6 ns
      Data.IntMap:               OK
        17.2 ns ± 922 ps
      Data.HashMap.Lazy:         OK
        21.1 ns ± 1.7 ns
      Data.Sequence:             OK
        13.0 ns ± 416 ps
      Data.SkewList.Lazy:        OK
        12.4 ns ± 834 ps
      Data.Primitive.SmallArray: OK
        29.3 ns ± 1.7 ns
    5
      Data.List:                 OK
        8.29 ns ± 800 ps
      Data.Vector:               OK
        66.9 ns ± 6.4 ns
      Data.Map:                  OK
        73.1 ns ± 7.2 ns
      Data.IntMap:               OK
        31.8 ns ± 1.8 ns
      Data.HashMap.Lazy:         OK
        57.9 ns ± 4.6 ns
      Data.Sequence:             OK
        20.6 ns ± 1.7 ns
      Data.SkewList.Lazy:        OK
        18.9 ns ± 1.9 ns
      Data.Primitive.SmallArray: OK
        50.2 ns ± 3.8 ns
    7
      Data.List:                 OK
        9.82 ns ± 478 ps
      Data.Vector:               OK
        97.9 ns ± 7.3 ns
      Data.Map:                  OK
        125  ns ± 2.8 ns
      Data.IntMap:               OK
        46.7 ns ± 3.6 ns
      Data.HashMap.Lazy:         OK
        96.1 ns ± 7.6 ns
      Data.Sequence:             OK
        30.2 ns ± 3.0 ns
      Data.SkewList.Lazy:        OK
        25.5 ns ± 2.4 ns
      Data.Primitive.SmallArray: OK
        72.6 ns ± 6.6 ns
    10
      Data.List:                 OK
        12.0 ns ± 842 ps
      Data.Vector:               OK
        145  ns ±  13 ns
      Data.Map:                  OK
        208  ns ±  13 ns
      Data.IntMap:               OK
        72.2 ns ± 6.5 ns
      Data.HashMap.Lazy:         OK
        155  ns ±  13 ns
      Data.Sequence:             OK
        47.9 ns ± 3.5 ns
      Data.SkewList.Lazy:        OK
        35.5 ns ± 3.4 ns
      Data.Primitive.SmallArray: OK
        107  ns ± 6.6 ns
    15
      Data.List:                 OK
        15.9 ns ± 820 ps
      Data.Vector:               OK
        237  ns ±  13 ns
      Data.Map:                  OK
        378  ns ±  26 ns
      Data.IntMap:               OK
        121  ns ± 6.4 ns
      Data.HashMap.Lazy:         OK
        263  ns ±  26 ns
      Data.Sequence:             OK
        75.8 ns ± 6.5 ns
      Data.SkewList.Lazy:        OK
        49.8 ns ± 3.4 ns
      Data.Primitive.SmallArray: OK
        174  ns ±  15 ns
    20
      Data.List:                 OK
        19.8 ns ± 1.6 ns
      Data.Vector:               OK
        343  ns ±  26 ns
      Data.Map:                  OK
        559  ns ±  52 ns
      Data.IntMap:               OK
        170  ns ±  14 ns
      Data.HashMap.Lazy:         OK
        384  ns ±  14 ns
      Data.Sequence:             OK
        99.3 ns ± 8.1 ns
      Data.SkewList.Lazy:        OK
        64.6 ns ± 6.4 ns
      Data.Primitive.SmallArray: OK
        239  ns ±  13 ns
  lookup
    1
      Data.List:                 OK
        4.32 ns ± 414 ps
      Data.Vector:               OK
        4.62 ns ± 404 ps
      Data.Map:                  OK
        5.24 ns ± 414 ps
      Data.IntMap:               OK
        4.62 ns ± 404 ps
      Data.HashMap.Lazy:         OK
        5.34 ns ± 450 ps
      Data.Sequence:             OK
        4.38 ns ± 400 ps
      Data.SkewList.Lazy:        OK
        4.35 ns ± 428 ps
      Data.Primitive.SmallArray: OK
        4.70 ns ± 356 ps
    3
      Data.List:                 OK
        12.9 ns ± 888 ps
      Data.Vector:               OK
        6.60 ns ± 448 ps
      Data.Map:                  OK
        11.0 ns ± 834 ps
      Data.IntMap:               OK
        16.1 ns ± 800 ps
      Data.HashMap.Lazy:         OK
        19.7 ns ± 1.7 ns
      Data.Sequence:             OK
        14.4 ns ± 896 ps
      Data.SkewList.Lazy:        OK
        14.7 ns ± 818 ps
      Data.Primitive.SmallArray: OK
        5.95 ns ± 462 ps
    5
      Data.List:                 OK
        26.9 ns ± 1.6 ns
      Data.Vector:               OK
        9.15 ns ± 832 ps
      Data.Map:                  OK
        22.1 ns ± 2.0 ns
      Data.IntMap:               OK
        31.7 ns ± 1.8 ns
      Data.HashMap.Lazy:         OK
        34.5 ns ± 1.7 ns
      Data.Sequence:             OK
        24.2 ns ± 1.8 ns
      Data.SkewList.Lazy:        OK
        29.8 ns ± 1.9 ns
      Data.Primitive.SmallArray: OK
        8.84 ns ± 464 ps
    7
      Data.List:                 OK
        45.8 ns ± 3.4 ns
      Data.Vector:               OK
        11.5 ns ± 818 ps
      Data.Map:                  OK
        30.0 ns ± 2.0 ns
      Data.IntMap:               OK
        47.7 ns ± 3.6 ns
      Data.HashMap.Lazy:         OK
        47.8 ns ± 3.2 ns
      Data.Sequence:             OK
        50.1 ns ± 3.4 ns
      Data.SkewList.Lazy:        OK
        46.4 ns ± 3.2 ns
      Data.Primitive.SmallArray: OK
        10.7 ns ± 820 ps
    10
      Data.List:                 OK
        83.2 ns ± 7.6 ns
      Data.Vector:               OK
        14.6 ns ± 800 ps
      Data.Map:                  OK
        47.1 ns ± 3.2 ns
      Data.IntMap:               OK
        75.1 ns ± 6.4 ns
      Data.HashMap.Lazy:         OK
        72.9 ns ± 3.8 ns
      Data.Sequence:             OK
        93.0 ns ± 7.5 ns
      Data.SkewList.Lazy:        OK
        70.4 ns ± 6.8 ns
      Data.Primitive.SmallArray: OK
        13.8 ns ± 1.1 ns
    15
      Data.List:                 OK
        192  ns ±  13 ns
      Data.Vector:               OK
        22.5 ns ± 1.2 ns
      Data.Map:                  OK
        78.2 ns ± 4.4 ns
      Data.IntMap:               OK
        119  ns ± 7.9 ns
      Data.HashMap.Lazy:         OK
        108  ns ± 7.7 ns
      Data.Sequence:             OK
        180  ns ±  17 ns
      Data.SkewList.Lazy:        OK
        124  ns ± 6.9 ns
      Data.Primitive.SmallArray: OK
        21.6 ns ± 1.9 ns
    20
      Data.List:                 OK
        411  ns ±  33 ns
      Data.Vector:               OK
        31.9 ns ± 1.8 ns
      Data.Map:                  OK
        115  ns ± 6.5 ns
      Data.IntMap:               OK
        165  ns ±  13 ns
      Data.HashMap.Lazy:         OK
        140  ns ±  13 ns
      Data.Sequence:             OK
        248  ns ±  16 ns
      Data.SkewList.Lazy:        OK
        206  ns ±  14 ns
      Data.Primitive.SmallArray: OK
        30.3 ns ± 1.9 ns
```
