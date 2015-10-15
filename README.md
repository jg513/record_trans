# xt_trans

See samples in ```xt_trans_tests.erl```.

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

### record_assign/2

Assign value to a record.

```Erlang
-spec record_assign(DstRec, SrcVar) -> Tuple when
	  DstRec :: atom(),
	  SrcVar :: [term()].
```

### record_assign/3

Assign value to a record.

```Erlang
-spec record_assign(DstRec, SrcRec, SrcVar) -> Tuple when
	  DstRec :: atom(),
	  SrcRec :: atom() | [atom()],
	  SrcVar :: [term()].
```

### record_assign/4

Assign value to a record.

```Erlang
-spec record_assign(DstRec, DstVar, SrcRec, SrcVar) -> Tuple when
	  DstRec :: atom(),
	  DstVar :: tuple(),
	  SrcRec :: atom() | [atom()],
	  SrcVar :: [term()].
```
