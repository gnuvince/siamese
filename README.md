# siamese: a simple symbol table module


## Introduction

siamese is a very simple, general purpose symbol table written in
Erlang.  The exported functions emulate, when possible, the
functionality of the `maps` module functions.


## Compilation

```sh
$ rebar3 compile
```


## Example

```erlang
1> c(siamese).
{ok,siamese}

2> S1 = siamese:from_list([{abc, 2}, {def, 3}]).
[#{abc => 2,def => 3}]

3> siamese:find(abc, S1).
{ok,2}

4> siamese:get(def, S1).
3

5> siamese:find(ghi, S1).
undefined

6> siamese:get(ghi, S1).
** exception error: {badkey,ghi}
     in function  siamese:get/2 (/home/vfoley/adgear/repos/siamese/src/siamese.erl, line 75)

7> siamese:get(ghi, S1, 42).
42

8> S2 = siamese:put(mno, 5, S1).
[#{abc => 2,def => 3,mno => 5}]

9> siamese:size(S2).
3

10> siamese:put(mno, 7, S2).
key_already_exists

11> S3 = siamese:open_scope(S2).
[#{},#{abc => 2,def => 3,mno => 5}]

12> S4 = siamese:put(mno, 7, S3).
[#{mno => 7},#{abc => 2,def => 3,mno => 5}]

13> siamese:get(mno, S4).
7

14> S5 = siamese:close_scope(S4).
[#{abc => 2,def => 3,mno => 5}]

15> siamese:get(mno, S5).
5
```


## TODO

- [ ] Property tests
- [ ] Performance tests
