# xt_trans

See samples in ```xt_trans_tests.erl```.

### record_copy/2

Copy same fields between two records.

```Erlang
-spec record_copy(Dst, Src) -> Tuple when
	  Dst :: tuple(),
	  Src :: tuple().
```

### record_assign/2

Assign value to a record.

```Erlang
-spec record_assign(Dst, Src) -> Tuple when
	  Dst :: tuple(),
	  Src :: tuple().
```
