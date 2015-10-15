# xt_trans

parse_transform module provides record_copy

See the samples in ```xt_trans_sample.erl```.

### record_copy

Copy same fields between two records.

```Erlang
-spec record_copy(DstRec, DstVar, SrcRec, SrcVar) -> Tuple when
	  DstRec :: atom(),
	  DstVar :: tuple(),
	  SrcRec :: atom(),
	  SrcVar :: tuple().
```
