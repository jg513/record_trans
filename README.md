# xt_trans

parse_transform module provides record_copy

See the samples in ```xt_trans_sample.erl```.

### record_copy/3

Copy same fields between two records.

```Erlang
-spec record_copy(DstRec, SrcRec, SrcVar) -> Tuple when
	  DstRec :: atom(),
	  SrcRec :: atom(),
	  SrcVar :: tuple().
```

### record_copy/4

Copy same fields between two records.

```Erlang
-spec record_copy(DstRec, DstVar, SrcRec, SrcVar) -> Tuple when
	  DstRec :: atom(),
	  DstVar :: tuple(),
	  SrcRec :: atom(),
	  SrcVar :: tuple().
```
