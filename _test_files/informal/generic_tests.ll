; ModuleID = 'generic_tests.gon'
source_filename = "generic_tests.gon"

%string = type { %"#dynarray" }
%"#dynarray" = type { ptr, i64, i64 }
%string_chars = type { %string, ptr }
%"option<char>" = type { i1, ptr }
%"listiterator<int>" = type { %string, i64 }

@_tmpl_print = private unnamed_addr constant [6 x i8] c"%.*s\0A\00", align 1
@_tmpl_int_to_string = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@_tmpl_float_to_string = private unnamed_addr constant [4 x i8] c"%#f\00", align 1
@_tmpl_char_to_string = private unnamed_addr constant [4 x i8] c"%lc\00", align 1
@throw_msg = private unnamed_addr constant [14 x i8] c"invalid slice\00", align 1
@throw_msg.1 = private unnamed_addr constant [31 x i8] c"cannot take element from array\00", align 1
@throw_msg.3 = private unnamed_addr constant [23 x i8] c"division by zero error\00", align 1
@throw_msg.4 = private unnamed_addr constant [17 x i8] c"no value present\00", align 1
@_write.1 = private unnamed_addr constant [2 x i8] c"w\00", align 1
@throw_msg.2.5 = private unnamed_addr constant [26 x i8] c"array index out of bounds\00", align 1
@locale.6 = private unnamed_addr constant [12 x i8] c"en_US.UTF-8\00", align 1

; Function Attrs: nofree nounwind
define void @print(ptr nocapture readonly %s) local_unnamed_addr #0 {
body:
  %s.0.1 = getelementptr inbounds %string, ptr %s, i64 0, i32 0, i32 1
  %s.0.1.load = load i64, ptr %s.0.1, align 4
  %s.0.0.load = load ptr, ptr %s, align 8
  %0 = tail call i64 (ptr, ...) @printf(ptr nonnull @_tmpl_print, i64 %s.0.1.load, ptr %s.0.0.load)
  ret void
}

; Function Attrs: nofree nounwind
declare noundef i64 @printf(ptr nocapture noundef readonly, ...) local_unnamed_addr #0

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define noalias ptr @"#ptr::null"() local_unnamed_addr #1 {
body:
  ret ptr null
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %string_chars @"string::chars"(ptr nocapture readonly %self) local_unnamed_addr #2 {
body:
  %.unpack.unpack.i = load ptr, ptr %self, align 8
  %0 = insertvalue %"#dynarray" undef, ptr %.unpack.unpack.i, 0
  %.unpack.elt1.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %.unpack.unpack2.i = load i64, ptr %.unpack.elt1.i, align 8
  %1 = insertvalue %"#dynarray" %0, i64 %.unpack.unpack2.i, 1
  %.unpack.elt3.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %.unpack.unpack4.i = load i64, ptr %.unpack.elt3.i, align 8
  %.unpack5.i = insertvalue %"#dynarray" %1, i64 %.unpack.unpack4.i, 2
  %2 = insertvalue %string undef, %"#dynarray" %.unpack5.i, 0
  %3 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  %4 = insertvalue %string_chars zeroinitializer, %string %2, 0
  %5 = insertvalue %string_chars %4, ptr %3, 1
  ret %string_chars %5
}

; Function Attrs: inaccessiblememonly mustprogress nofree nounwind willreturn allockind("alloc,uninitialized") allocsize(0)
declare noalias noundef ptr @malloc(i64 noundef) local_unnamed_addr #3

; Function Attrs: mustprogress nofree nounwind willreturn
define %string_chars @"string_chars::new"(ptr nocapture readonly %str) local_unnamed_addr #2 {
body:
  %.unpack.unpack = load ptr, ptr %str, align 8
  %0 = insertvalue %"#dynarray" undef, ptr %.unpack.unpack, 0
  %.unpack.elt1 = getelementptr inbounds %"#dynarray", ptr %str, i64 0, i32 1
  %.unpack.unpack2 = load i64, ptr %.unpack.elt1, align 8
  %1 = insertvalue %"#dynarray" %0, i64 %.unpack.unpack2, 1
  %.unpack.elt3 = getelementptr inbounds %"#dynarray", ptr %str, i64 0, i32 2
  %.unpack.unpack4 = load i64, ptr %.unpack.elt3, align 8
  %.unpack5 = insertvalue %"#dynarray" %1, i64 %.unpack.unpack4, 2
  %2 = insertvalue %string undef, %"#dynarray" %.unpack5, 0
  %3 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  %4 = insertvalue %string_chars zeroinitializer, %string %2, 0
  %5 = insertvalue %string_chars %4, ptr %3, 1
  ret %string_chars %5
}

define %string @"string::slice_bytes"(ptr nocapture readonly %self, i64 %start, i64 %end) local_unnamed_addr {
body:
  %i_le = icmp sgt i64 %start, -1
  %i_le4 = icmp sge i64 %end, %start
  %and_result = select i1 %i_le, i1 %i_le4, i1 false
  br i1 %and_result, label %and_true6, label %else

else:                                             ; preds = %and_true6, %body
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg, i64 13, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

merge:                                            ; preds = %and_true6
  %self.0.0.load = load ptr, ptr %self, align 8
  %gep = getelementptr i8, ptr %self.0.0.load, i64 %start
  %2 = insertvalue %"#dynarray" zeroinitializer, ptr %gep, 0
  %i_sub = sub i64 %end, %start
  %3 = insertvalue %"#dynarray" %2, i64 %i_sub, 1
  %self.0.2 = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 2
  %self.0.2.load = load i64, ptr %self.0.2, align 4
  %i_sub12 = sub i64 %self.0.2.load, %start
  %4 = insertvalue %"#dynarray" %3, i64 %i_sub12, 2
  %5 = insertvalue %string zeroinitializer, %"#dynarray" %4, 0
  ret %string %5

and_true6:                                        ; preds = %body
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %i_le9.not = icmp slt i64 %self.0.1.load.i, %end
  br i1 %i_le9.not, label %else, label %merge
}

; Function Attrs: nofree nounwind
declare noalias noundef ptr @fdopen(i64 noundef, ptr nocapture noundef readonly) local_unnamed_addr #0

; Function Attrs: nofree nounwind
declare noundef i64 @fwrite(ptr nocapture noundef, i64 noundef, i64 noundef, ptr nocapture noundef) local_unnamed_addr #0

declare i64 @fputwc(i32, ptr) local_unnamed_addr

declare void @exit(i64) local_unnamed_addr

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define i64 @"string::len"(ptr nocapture readonly %self) local_unnamed_addr #4 {
body:
  %self.0.1 = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load = load i64, ptr %self.0.1, align 4
  ret i64 %self.0.1.load
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define %string @"string::to_string"(ptr nocapture readonly %self) local_unnamed_addr #4 {
body:
  %.unpack.unpack = load ptr, ptr %self, align 8
  %0 = insertvalue %"#dynarray" undef, ptr %.unpack.unpack, 0
  %.unpack.elt1 = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %.unpack.unpack2 = load i64, ptr %.unpack.elt1, align 8
  %1 = insertvalue %"#dynarray" %0, i64 %.unpack.unpack2, 1
  %.unpack.elt3 = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %.unpack.unpack4 = load i64, ptr %.unpack.elt3, align 8
  %.unpack5 = insertvalue %"#dynarray" %1, i64 %.unpack.unpack4, 2
  %2 = insertvalue %string undef, %"#dynarray" %.unpack5, 0
  ret %string %2
}

; Function Attrs: mustprogress nounwind willreturn
define %string @"string::add_string"(ptr nocapture readonly %self, ptr nocapture readonly %other) local_unnamed_addr #5 {
body:
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %self.0.1.i10 = getelementptr inbounds %string, ptr %other, i64 0, i32 0, i32 1
  %self.0.1.load.i11 = load i64, ptr %self.0.1.i10, align 4
  %i_add = add i64 %self.0.1.load.i11, %self.0.1.load.i
  %0 = tail call ptr @malloc(i64 %i_add)
  %self.0.0.load = load ptr, ptr %self, align 8
  %i_lt.i.i21 = icmp slt i64 %i_add, %self.0.1.load.i
  br i1 %i_lt.i.i21, label %"#dynarray::extend.exit31", label %"#dynarray::extend.exit"

"#dynarray::extend.exit31":                       ; preds = %body
  %1 = tail call ptr @malloc(i64 %self.0.1.load.i)
  tail call void @free(ptr %0)
  br label %"#dynarray::extend.exit"

"#dynarray::extend.exit":                         ; preds = %body, %"#dynarray::extend.exit31"
  %.sink = phi ptr [ %1, %"#dynarray::extend.exit31" ], [ %0, %body ]
  %result_inner.sroa.15.1 = phi i64 [ %self.0.1.load.i, %"#dynarray::extend.exit31" ], [ %i_add, %body ]
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %.sink, ptr align 1 %self.0.0.load, i64 %self.0.1.load.i, i1 false)
  %other.0.0.load11 = load ptr, ptr %other, align 8
  %gep.i = getelementptr i8, ptr %.sink, i64 %self.0.1.load.i
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %gep.i, ptr align 1 %other.0.0.load11, i64 %self.0.1.load.i11, i1 false)
  %2 = insertvalue %"#dynarray" undef, ptr %.sink, 0
  %3 = insertvalue %"#dynarray" %2, i64 %i_add, 1
  %4 = insertvalue %"#dynarray" %3, i64 %result_inner.sroa.15.1, 2
  %5 = insertvalue %string zeroinitializer, %"#dynarray" %4, 0
  ret %string %5
}

; Function Attrs: argmemonly mustprogress nocallback nofree nounwind willreturn
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #6

; Function Attrs: inaccessiblemem_or_argmemonly mustprogress nounwind willreturn allockind("free")
declare void @free(ptr allocptr nocapture noundef) local_unnamed_addr #7

; Function Attrs: mustprogress nofree nounwind willreturn
define %"#dynarray" @"#dynarray::new"(i64 %cap) local_unnamed_addr #2 {
body:
  %0 = tail call ptr @malloc(i64 %cap)
  %1 = insertvalue %"#dynarray" zeroinitializer, ptr %0, 0
  %2 = insertvalue %"#dynarray" %1, i64 0, 1
  %3 = insertvalue %"#dynarray" %2, i64 %cap, 2
  ret %"#dynarray" %3
}

; Function Attrs: mustprogress nounwind willreturn
define void @"#dynarray::extend"(ptr nocapture %self, ptr nocapture readonly %add_buf, i64 %add_len) local_unnamed_addr #5 {
body:
  %self.1 = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %self.1.load = load i64, ptr %self.1, align 4
  %i_add = add i64 %self.1.load, %add_len
  %self.2.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %self.2.load.i = load i64, ptr %self.2.i, align 4
  %i_lt.i = icmp slt i64 %self.2.load.i, %i_add
  br i1 %i_lt.i, label %then.i, label %"#dynarray::resize.exit"

then.i:                                           ; preds = %body
  %self.0.load.i = load ptr, ptr %self, align 8
  %0 = tail call ptr @malloc(i64 %i_add)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %0, ptr align 1 %self.0.load.i, i64 %self.1.load, i1 false)
  store ptr %0, ptr %self, align 8
  store i64 %i_add, ptr %self.2.i, align 4
  tail call void @free(ptr %self.0.load.i)
  %self.13.load.pre = load i64, ptr %self.1, align 4
  br label %"#dynarray::resize.exit"

"#dynarray::resize.exit":                         ; preds = %then.i, %body
  %self.13.load = phi i64 [ %self.13.load.pre, %then.i ], [ %self.1.load, %body ]
  %self.0.load = load ptr, ptr %self, align 8
  %gep = getelementptr i8, ptr %self.0.load, i64 %self.13.load
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %gep, ptr align 1 %add_buf, i64 %add_len, i1 false)
  %self.14.load = load i64, ptr %self.1, align 4
  %i_add5 = add i64 %self.14.load, %add_len
  store i64 %i_add5, ptr %self.1, align 4
  ret void
}

; Function Attrs: mustprogress nounwind willreturn
define void @"#dynarray::resize"(ptr nocapture %self, i64 %new_cap) local_unnamed_addr #5 {
body:
  %self.2 = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %self.2.load = load i64, ptr %self.2, align 4
  %i_lt = icmp slt i64 %self.2.load, %new_cap
  br i1 %i_lt, label %then, label %merge

then:                                             ; preds = %body
  %self.0.load = load ptr, ptr %self, align 8
  %0 = tail call ptr @malloc(i64 %new_cap)
  %self.1 = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %self.1.load = load i64, ptr %self.1, align 4
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %0, ptr align 1 %self.0.load, i64 %self.1.load, i1 false)
  store ptr %0, ptr %self, align 8
  store i64 %new_cap, ptr %self.2, align 4
  tail call void @free(ptr %self.0.load)
  br label %merge

merge:                                            ; preds = %then, %body
  ret void
}

define %"option<char>" @"string_chars::next"(ptr nocapture %self) local_unnamed_addr {
body:
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %i_eq = icmp eq i64 %self.0.1.load.i, 0
  br i1 %i_eq, label %merge, label %else

else:                                             ; preds = %body
  %self.1 = getelementptr inbounds %string_chars, ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  %self.0.0.0.load = load ptr, ptr %self, align 8
  %0 = tail call i64 @llvm.smin.i64(i64 %self.0.1.load.i, i64 6)
  %1 = tail call i64 @mbtowc(ptr %self.1.load, ptr %self.0.0.0.load, i64 %0)
  %i_lt = icmp slt i64 %1, 1
  br i1 %i_lt, label %block, label %block8

merge:                                            ; preds = %"string::slice_bytes.exit", %block, %body
  %if_result14 = phi %"option<char>" [ zeroinitializer, %block ], [ %6, %"string::slice_bytes.exit" ], [ zeroinitializer, %body ]
  ret %"option<char>" %if_result14

block:                                            ; preds = %else
  %2 = tail call ptr @malloc(i64 0)
  store ptr %2, ptr %self, align 8
  tail call void @llvm.memset.p0.i64(ptr noundef nonnull align 8 dereferenceable(16) %self.0.1.i, i8 0, i64 16, i1 false)
  br label %merge

block8:                                           ; preds = %else
  %self.0.1.load.i13 = load i64, ptr %self.0.1.i, align 4
  %i_le4.i.not = icmp slt i64 %self.0.1.load.i13, %1
  br i1 %i_le4.i.not, label %else.i, label %"string::slice_bytes.exit"

else.i:                                           ; preds = %block8
  %stderr.i = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %3 = tail call i64 @fwrite(ptr nonnull @throw_msg, i64 13, i64 1, ptr %stderr.i)
  %4 = tail call i64 @fputwc(i32 10, ptr %stderr.i)
  tail call void @exit(i64 1)
  unreachable

"string::slice_bytes.exit":                       ; preds = %block8
  %self.0.0.load.i = load ptr, ptr %self, align 8
  %gep.i = getelementptr i8, ptr %self.0.0.load.i, i64 %1
  %i_sub.i = sub i64 %self.0.1.load.i13, %1
  %self.0.2.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 2
  %self.0.2.load.i = load i64, ptr %self.0.2.i, align 4
  %i_sub12.i = sub i64 %self.0.2.load.i, %1
  store ptr %gep.i, ptr %self, align 8
  store i64 %i_sub.i, ptr %self.0.1.i, align 8
  store i64 %i_sub12.i, ptr %self.0.2.i, align 8
  %self.113.load = load ptr, ptr %self.1, align 8
  %deref = load i32, ptr %self.113.load, align 4
  %5 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  store i32 %deref, ptr %5, align 4
  %6 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %5, 1
  br label %merge
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.smin.i64(i64, i64) #8

declare i64 @mbtowc(ptr, ptr, i64) local_unnamed_addr

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define %"option<char>" @"option<char>::none"() local_unnamed_addr #1 {
body:
  ret %"option<char>" zeroinitializer
}

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::min"(i64 %self, i64 %o) local_unnamed_addr #9 {
body:
  %0 = tail call i64 @llvm.smin.i64(i64 %self, i64 %o)
  ret i64 %0
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %string @"string::new"() local_unnamed_addr #2 {
body:
  %0 = tail call ptr @malloc(i64 0)
  %1 = insertvalue %"#dynarray" zeroinitializer, ptr %0, 0
  %2 = insertvalue %"#dynarray" %1, i64 0, 1
  %3 = insertvalue %"#dynarray" %2, i64 0, 2
  %4 = insertvalue %string zeroinitializer, %"#dynarray" %3, 0
  ret %string %4
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %"option<char>" @"option<char>::some"(i32 %t) local_unnamed_addr #2 {
body:
  %0 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  store i32 %t, ptr %0, align 4
  %1 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %0, 1
  ret %"option<char>" %1
}

define %string @"option<char>::to_string"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %0 = alloca ptr, align 8
  %self.0.load = load i1, ptr %self, align 1
  br i1 %self.0.load, label %then, label %else

then:                                             ; preds = %body
  %self.1 = getelementptr inbounds %"option<char>", ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  %deref = load i32, ptr %self.1.load, align 4
  %1 = alloca [5 x i8], align 1
  store i8 115, ptr %1, align 1
  %.repack4 = getelementptr inbounds [5 x i8], ptr %1, i64 0, i64 1
  store i8 111, ptr %.repack4, align 1
  %.repack5 = getelementptr inbounds [5 x i8], ptr %1, i64 0, i64 2
  store i8 109, ptr %.repack5, align 1
  %.repack6 = getelementptr inbounds [5 x i8], ptr %1, i64 0, i64 3
  store i8 101, ptr %.repack6, align 1
  %.repack7 = getelementptr inbounds [5 x i8], ptr %1, i64 0, i64 4
  store i8 40, ptr %.repack7, align 1
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %0)
  %2 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %0, ptr nonnull @_tmpl_char_to_string, i32 %deref)
  %deref.i = load ptr, ptr %0, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %0)
  %i_add.i1 = add i64 %2, 5
  %3 = call ptr @malloc(i64 %i_add.i1)
  %i_lt.i.i21.i = icmp ugt i64 %2, 9223372036854775802
  br i1 %i_lt.i.i21.i, label %"#dynarray::extend.exit31.i", label %"string::add_string.exit"

"#dynarray::extend.exit31.i":                     ; preds = %then
  %4 = call dereferenceable_or_null(5) ptr @malloc(i64 5)
  call void @free(ptr %3)
  br label %"string::add_string.exit"

"string::add_string.exit":                        ; preds = %then, %"#dynarray::extend.exit31.i"
  %.sink.i = phi ptr [ %4, %"#dynarray::extend.exit31.i" ], [ %3, %then ]
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %.sink.i, ptr noundef nonnull align 1 dereferenceable(5) %1, i64 5, i1 false)
  %gep.i.i = getelementptr i8, ptr %.sink.i, i64 5
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %gep.i.i, ptr align 1 %deref.i, i64 %2, i1 false)
  %5 = call dereferenceable_or_null(1) ptr @malloc(i64 1)
  store i8 41, ptr %5, align 1
  %i_add.i6 = add i64 %2, 6
  %6 = call ptr @malloc(i64 %i_add.i6)
  %i_lt.i.i21.i8 = icmp slt i64 %i_add.i6, %i_add.i1
  br i1 %i_lt.i.i21.i8, label %"#dynarray::extend.exit31.i9", label %"string::add_string.exit14"

"#dynarray::extend.exit31.i9":                    ; preds = %"string::add_string.exit"
  %7 = call ptr @malloc(i64 %i_add.i1)
  call void @free(ptr %6)
  br label %"string::add_string.exit14"

"string::add_string.exit14":                      ; preds = %"string::add_string.exit", %"#dynarray::extend.exit31.i9"
  %.sink.i10 = phi ptr [ %7, %"#dynarray::extend.exit31.i9" ], [ %6, %"string::add_string.exit" ]
  %result_inner.sroa.15.1.i11 = phi i64 [ %i_add.i1, %"#dynarray::extend.exit31.i9" ], [ %i_add.i6, %"string::add_string.exit" ]
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %.sink.i10, ptr nonnull align 1 %.sink.i, i64 %i_add.i1, i1 false)
  %gep.i.i13 = getelementptr i8, ptr %.sink.i10, i64 %i_add.i1
  %8 = load i8, ptr %5, align 1
  store i8 %8, ptr %gep.i.i13, align 1
  br label %merge

else:                                             ; preds = %body
  %9 = alloca [4 x i8], align 4
  store i8 110, ptr %9, align 4
  %.repack1 = getelementptr inbounds [4 x i8], ptr %9, i64 0, i64 1
  store i8 111, ptr %.repack1, align 1
  %.repack2 = getelementptr inbounds [4 x i8], ptr %9, i64 0, i64 2
  store i8 110, ptr %.repack2, align 2
  %.repack3 = getelementptr inbounds [4 x i8], ptr %9, i64 0, i64 3
  store i8 101, ptr %.repack3, align 1
  %10 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  %11 = load i32, ptr %9, align 4
  store i32 %11, ptr %10, align 1
  br label %merge

merge:                                            ; preds = %else, %"string::add_string.exit14"
  %.sink.i10.pn = phi ptr [ %.sink.i10, %"string::add_string.exit14" ], [ %10, %else ]
  %i_add.i6.pn = phi i64 [ %i_add.i6, %"string::add_string.exit14" ], [ 4, %else ]
  %result_inner.sroa.15.1.i11.pn = phi i64 [ %result_inner.sroa.15.1.i11, %"string::add_string.exit14" ], [ 4, %else ]
  %.pn16 = insertvalue %"#dynarray" undef, ptr %.sink.i10.pn, 0
  %.pn = insertvalue %"#dynarray" %.pn16, i64 %i_add.i6.pn, 1
  %.pn15 = insertvalue %"#dynarray" %.pn, i64 %result_inner.sroa.15.1.i11.pn, 2
  %if_result = insertvalue %string zeroinitializer, %"#dynarray" %.pn15, 0
  ret %string %if_result
}

; Function Attrs: argmemonly mustprogress nocallback nofree nosync nounwind willreturn
declare void @llvm.lifetime.start.p0(i64 immarg, ptr nocapture) #10

; Function Attrs: argmemonly mustprogress nocallback nofree nosync nounwind willreturn
declare void @llvm.lifetime.end.p0(i64 immarg, ptr nocapture) #10

declare i64 @asprintf(ptr, ptr, ...) local_unnamed_addr

; Function Attrs: mustprogress nofree nounwind willreturn
define %string @"string::from_raw"(ptr nocapture readonly %contents, i64 %len) local_unnamed_addr #2 {
body:
  %0 = tail call ptr @malloc(i64 %len)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %0, ptr align 1 %contents, i64 %len, i1 false)
  %1 = insertvalue %"#dynarray" undef, ptr %0, 0
  %2 = insertvalue %"#dynarray" %1, i64 %len, 1
  %3 = insertvalue %"#dynarray" %2, i64 %len, 2
  %4 = insertvalue %string zeroinitializer, %"#dynarray" %3, 0
  ret %string %4
}

define %string @"char::to_string"(i32 %self) local_unnamed_addr {
body:
  %0 = alloca ptr, align 8
  %1 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %0, ptr nonnull @_tmpl_char_to_string, i32 %self)
  %deref = load ptr, ptr %0, align 8
  %i_add = add i64 %1, 1
  %2 = insertvalue %"#dynarray" zeroinitializer, ptr %deref, 0
  %3 = insertvalue %"#dynarray" %2, i64 %1, 1
  %4 = insertvalue %"#dynarray" %3, i64 %i_add, 2
  %5 = insertvalue %string zeroinitializer, %"#dynarray" %4, 0
  ret %string %5
}

define ptr @"#dynarray::take"(ptr nocapture %self, i64 %sub_len) local_unnamed_addr {
body:
  %self.1 = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %self.1.load = load i64, ptr %self.1, align 4
  %i_ge.not = icmp slt i64 %self.1.load, %sub_len
  br i1 %i_ge.not, label %else, label %block

else:                                             ; preds = %body
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.1, i64 30, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

block:                                            ; preds = %body
  %i_sub = sub i64 %self.1.load, %sub_len
  store i64 %i_sub, ptr %self.1, align 4
  %self.0.load = load ptr, ptr %self, align 8
  %gep = getelementptr i8, ptr %self.0.load, i64 %i_sub
  ret ptr %gep
}

define i32 @"option<char>::get"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %self.0.load = load i1, ptr %self, align 1
  br i1 %self.0.load, label %merge, label %else

else:                                             ; preds = %body
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.4, i64 16, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

merge:                                            ; preds = %body
  %self.1 = getelementptr inbounds %"option<char>", ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  %deref = load i32, ptr %self.1.load, align 4
  ret i32 %deref
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %string @"bool::to_string"(i1 %self) local_unnamed_addr #2 {
body:
  br i1 %self, label %then, label %else

then:                                             ; preds = %body
  %0 = alloca [4 x i8], align 4
  store i8 116, ptr %0, align 4
  %.repack5 = getelementptr inbounds [4 x i8], ptr %0, i64 0, i64 1
  store i8 114, ptr %.repack5, align 1
  %.repack6 = getelementptr inbounds [4 x i8], ptr %0, i64 0, i64 2
  store i8 117, ptr %.repack6, align 2
  %.repack7 = getelementptr inbounds [4 x i8], ptr %0, i64 0, i64 3
  store i8 101, ptr %.repack7, align 1
  %1 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  %2 = load i32, ptr %0, align 4
  store i32 %2, ptr %1, align 1
  br label %merge

else:                                             ; preds = %body
  %3 = alloca [5 x i8], align 1
  store i8 102, ptr %3, align 1
  %.repack1 = getelementptr inbounds [5 x i8], ptr %3, i64 0, i64 1
  store i8 97, ptr %.repack1, align 1
  %.repack2 = getelementptr inbounds [5 x i8], ptr %3, i64 0, i64 2
  store i8 108, ptr %.repack2, align 1
  %.repack3 = getelementptr inbounds [5 x i8], ptr %3, i64 0, i64 3
  store i8 115, ptr %.repack3, align 1
  %.repack4 = getelementptr inbounds [5 x i8], ptr %3, i64 0, i64 4
  store i8 101, ptr %.repack4, align 1
  %4 = tail call dereferenceable_or_null(5) ptr @malloc(i64 5)
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %4, ptr noundef nonnull align 1 dereferenceable(5) %3, i64 5, i1 false)
  br label %merge

merge:                                            ; preds = %else, %then
  %.unpack.i.pn = phi ptr [ %1, %then ], [ %4, %else ]
  %.unpack7.i.pn = phi i64 [ 4, %then ], [ 5, %else ]
  %.pn2 = insertvalue %"#dynarray" undef, ptr %.unpack.i.pn, 0
  %.pn = insertvalue %"#dynarray" %.pn2, i64 %.unpack7.i.pn, 1
  %.pn1 = insertvalue %"#dynarray" %.pn, i64 %.unpack7.i.pn, 2
  %if_result = insertvalue %string zeroinitializer, %"#dynarray" %.pn1, 0
  ret %string %if_result
}

define i1 @"float::isnan"(double %self) local_unnamed_addr {
body:
  %0 = tail call i1 @isnan(double %self)
  ret i1 %0
}

declare i1 @isnan(double) local_unnamed_addr

define i1 @"float::isinf"(double %self) local_unnamed_addr {
body:
  %0 = tail call i1 @isinf(double %self)
  ret i1 %0
}

declare i1 @isinf(double) local_unnamed_addr

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::sign"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @llvm.copysign.f64(double 1.000000e+00, double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.copysign.f64(double, double) #8

define double @"float::nexttoward"(double %self, double %twd) local_unnamed_addr {
body:
  %0 = tail call double @nexttoward(double %self, double %twd)
  ret double %0
}

declare double @nexttoward(double, double) local_unnamed_addr

define i64 @"float::iround"(double %self) local_unnamed_addr {
body:
  %0 = tail call i64 @lround(double %self)
  ret i64 %0
}

declare i64 @lround(double) local_unnamed_addr

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::round"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @llvm.round.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.round.f64(double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::trunc"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @llvm.trunc.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.trunc.f64(double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::floor"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @llvm.floor.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.floor.f64(double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::ceil"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @llvm.ceil.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.ceil.f64(double) #8

define double @"float::tgamma"(double %self) local_unnamed_addr {
body:
  %0 = tail call double @tgamma(double %self)
  ret double %0
}

declare double @tgamma(double) local_unnamed_addr

define double @"float::lgamma"(double %self) local_unnamed_addr {
body:
  %0 = tail call double @lgamma(double %self)
  ret double %0
}

declare double @lgamma(double) local_unnamed_addr

define double @"float::erfc"(double %self) local_unnamed_addr {
body:
  %0 = tail call double @erfc(double %self)
  ret double %0
}

declare double @erfc(double) local_unnamed_addr

define double @"float::erf"(double %self) local_unnamed_addr {
body:
  %0 = tail call double @erf(double %self)
  ret double %0
}

declare double @erf(double) local_unnamed_addr

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::atanh"(double %self) local_unnamed_addr #11 {
body:
  %0 = tail call double @atanh(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @atanh(double) local_unnamed_addr #11

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::acosh"(double %self) local_unnamed_addr #11 {
body:
  %0 = tail call double @acosh(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @acosh(double) local_unnamed_addr #11

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::asinh"(double %self) local_unnamed_addr #11 {
body:
  %0 = tail call double @asinh(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @asinh(double) local_unnamed_addr #11

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::tanh"(double %self) local_unnamed_addr #11 {
body:
  %0 = tail call double @tanh(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @tanh(double) local_unnamed_addr #11

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::cosh"(double %self) local_unnamed_addr #11 {
body:
  %0 = tail call double @cosh(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @cosh(double) local_unnamed_addr #11

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::sinh"(double %self) local_unnamed_addr #11 {
body:
  %0 = tail call double @sinh(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @sinh(double) local_unnamed_addr #11

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::atan2"(double %self, double %x) local_unnamed_addr #11 {
body:
  %0 = tail call double @atan2(double %self, double %x)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @atan2(double, double) local_unnamed_addr #11

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::atan"(double %self) local_unnamed_addr #11 {
body:
  %0 = tail call double @atan(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @atan(double) local_unnamed_addr #11

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::acos"(double %self) local_unnamed_addr #11 {
body:
  %0 = tail call double @acos(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @acos(double) local_unnamed_addr #11

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::asin"(double %self) local_unnamed_addr #11 {
body:
  %0 = tail call double @asin(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @asin(double) local_unnamed_addr #11

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::tan"(double %self) local_unnamed_addr #11 {
body:
  %0 = tail call double @tan(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @tan(double) local_unnamed_addr #11

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::cos"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @llvm.cos.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.cos.f64(double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::sin"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @llvm.sin.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.sin.f64(double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::powi"(double %self, i64 %exp) local_unnamed_addr #9 {
body:
  %cast = sitofp i64 %exp to double
  %0 = tail call double @llvm.pow.f64(double %self, double %cast)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.pow.f64(double, double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::pow"(double %self, double %exp) local_unnamed_addr #9 {
body:
  %0 = tail call double @llvm.pow.f64(double %self, double %exp)
  ret double %0
}

define double @"float::hypot"(double %self, double %y) local_unnamed_addr {
body:
  %0 = tail call double @hypot(double %self, double %y)
  ret double %0
}

declare double @hypot(double, double) local_unnamed_addr

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::cbrt"(double %self) local_unnamed_addr #11 {
body:
  %0 = tail call double @cbrt(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @cbrt(double) local_unnamed_addr #11

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::sqrt"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @llvm.sqrt.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.sqrt.f64(double) #8

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::log1p"(double %self) local_unnamed_addr #11 {
body:
  %0 = tail call double @log1p(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @log1p(double) local_unnamed_addr #11

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::log10"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @llvm.log10.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.log10.f64(double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::log2"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @llvm.log2.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.log2.f64(double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::log"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @llvm.log.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.log.f64(double) #8

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::expm1"(double %self) local_unnamed_addr #11 {
body:
  %0 = tail call double @expm1(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @expm1(double) local_unnamed_addr #11

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::exp2"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @llvm.exp2.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.exp2.f64(double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::exp"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @llvm.exp.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.exp.f64(double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::min"(double %self, double %o) local_unnamed_addr #9 {
body:
  %0 = tail call double @llvm.minnum.f64(double %self, double %o)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.minnum.f64(double, double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::max"(double %self, double %o) local_unnamed_addr #9 {
body:
  %0 = tail call double @llvm.maxnum.f64(double %self, double %o)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.maxnum.f64(double, double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::fma"(double %self, double %multiplicand, double %addend) local_unnamed_addr #9 {
body:
  %0 = tail call double @llvm.fma.f64(double %self, double %multiplicand, double %addend)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.fma.f64(double, double, double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::abs"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @llvm.fabs.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.fabs.f64(double) #8

define %string @"float::to_string"(double %self) local_unnamed_addr {
body:
  %0 = alloca ptr, align 8
  %1 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %0, ptr nonnull @_tmpl_float_to_string, double %self)
  %deref = load ptr, ptr %0, align 8
  %i_add = add i64 %1, 1
  %2 = insertvalue %"#dynarray" zeroinitializer, ptr %deref, 0
  %3 = insertvalue %"#dynarray" %2, i64 %1, 1
  %4 = insertvalue %"#dynarray" %3, i64 %i_add, 2
  %5 = insertvalue %string zeroinitializer, %"#dynarray" %4, 0
  ret %string %5
}

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::trailing_zeroes"(i64 %self) local_unnamed_addr #9 {
body:
  %0 = tail call i64 @llvm.cttz.i64(i64 %self, i1 false), !range !0
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.cttz.i64(i64, i1 immarg) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::leading_zeroes"(i64 %self) local_unnamed_addr #9 {
body:
  %0 = tail call i64 @llvm.ctlz.i64(i64 %self, i1 false), !range !0
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.ctlz.i64(i64, i1 immarg) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::reverse_bytes"(i64 %self) local_unnamed_addr #9 {
body:
  %0 = tail call i64 @llvm.bswap.i64(i64 %self)
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.bswap.i64(i64) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::reverse_bits"(i64 %self) local_unnamed_addr #9 {
body:
  %0 = tail call i64 @llvm.bitreverse.i64(i64 %self)
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.bitreverse.i64(i64) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::count_ones"(i64 %self) local_unnamed_addr #9 {
body:
  %0 = tail call i64 @llvm.ctpop.i64(i64 %self), !range !0
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.ctpop.i64(i64) #8

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define i64 @"int::sign"(i64 %self) local_unnamed_addr #1 {
body:
  %i_lt.not = icmp ne i64 %self, 0
  %spec.select = sext i1 %i_lt.not to i64
  %i_gt.inv = icmp slt i64 %self, 1
  %if_result = select i1 %i_gt.inv, i64 %spec.select, i64 1
  ret i64 %if_result
}

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::max"(i64 %self, i64 %o) local_unnamed_addr #9 {
body:
  %0 = tail call i64 @llvm.smax.i64(i64 %self, i64 %o)
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.smax.i64(i64, i64) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::abs"(i64 %self) local_unnamed_addr #9 {
body:
  %0 = tail call i64 @llvm.abs.i64(i64 %self, i1 false)
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.abs.i64(i64, i1 immarg) #8

define i64 @"int::idiv"(i64 %self, i64 %d) local_unnamed_addr {
body:
  %i_eq = icmp eq i64 %d, 0
  br i1 %i_eq, label %then, label %else

then:                                             ; preds = %body
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.3, i64 22, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

else:                                             ; preds = %body
  %2 = sdiv i64 %self, %d
  ret i64 %2
}

define %string @"int::to_string"(i64 %self) local_unnamed_addr {
body:
  %0 = alloca ptr, align 8
  %1 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %0, ptr nonnull @_tmpl_int_to_string, i64 %self)
  %deref = load ptr, ptr %0, align 8
  %i_add = add i64 %1, 1
  %2 = insertvalue %"#dynarray" zeroinitializer, ptr %deref, 0
  %3 = insertvalue %"#dynarray" %2, i64 %1, 1
  %4 = insertvalue %"#dynarray" %3, i64 %i_add, 2
  %5 = insertvalue %string zeroinitializer, %"#dynarray" %4, 0
  ret %string %5
}

declare ptr @setlocale(i64, ptr) local_unnamed_addr

; Function Attrs: mustprogress nofree nounwind willreturn
define %string @"list<int>::new"() local_unnamed_addr #2 {
body:
  %0 = tail call ptr @malloc(i64 0)
  %1 = insertvalue %"#dynarray" zeroinitializer, ptr %0, 0
  %2 = insertvalue %"#dynarray" %1, i64 0, 1
  %3 = insertvalue %"#dynarray" %2, i64 0, 2
  %4 = insertvalue %string zeroinitializer, %"#dynarray" %3, 0
  ret %string %4
}

define i64 @"list<int>::sum"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %.unpack.unpack.i.i = load ptr, ptr %self, align 8
  %.unpack.elt1.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %.unpack.unpack2.i.i = load i64, ptr %.unpack.elt1.i.i, align 8
  %.unpack.elt3.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %.unpack.unpack4.i.i = load i64, ptr %.unpack.elt3.i.i, align 8
  %0 = alloca %"listiterator<int>", align 8
  store ptr %.unpack.unpack.i.i, ptr %0, align 8
  %.repack4 = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 1
  store i64 %.unpack.unpack2.i.i, ptr %.repack4, align 8
  %.repack6 = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 2
  store i64 %.unpack.unpack4.i.i, ptr %.repack6, align 8
  %.repack1 = getelementptr inbounds %"listiterator<int>", ptr %0, i64 0, i32 1
  store i64 0, ptr %.repack1, align 8
  %self.1.i = getelementptr inbounds %"listiterator<int>", ptr %0, i64 0, i32 1
  %self.1.load.i = load i64, ptr %self.1.i, align 4
  %self.0.1.i.i = getelementptr inbounds %string, ptr %0, i64 0, i32 0, i32 1
  %self.0.1.load.i.i = load i64, ptr %self.0.1.i.i, align 4
  %1 = sdiv i64 %self.0.1.load.i.i, 8
  %i_ge.not.i = icmp slt i64 %self.1.load.i, %1
  br i1 %i_ge.not.i, label %else.i, label %"listiterator<int>::next.exit"

else.i:                                           ; preds = %body
  %i_le.i.i = icmp sgt i64 %self.1.load.i, -1
  br i1 %i_le.i.i, label %"list<int>::get.exit.i", label %else.i.i

else.i.i:                                         ; preds = %else.i
  %stderr.i.i = call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %2 = call i64 @fwrite(ptr nonnull @throw_msg.2.5, i64 25, i64 1, ptr %stderr.i.i)
  %3 = call i64 @fputwc(i32 10, ptr %stderr.i.i)
  call void @exit(i64 1)
  unreachable

"list<int>::get.exit.i":                          ; preds = %else.i
  %self.0.0.load.i.i = load ptr, ptr %0, align 8
  %gep.i.i = getelementptr i64, ptr %self.0.0.load.i.i, i64 %self.1.load.i
  %deref.i.i = load i64, ptr %gep.i.i, align 4
  %4 = call dereferenceable_or_null(8) ptr @malloc(i64 8)
  store i64 %deref.i.i, ptr %4, align 4
  %5 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %4, 1
  %i_add.i = add nuw nsw i64 %self.1.load.i, 1
  store i64 %i_add.i, ptr %self.1.i, align 4
  br label %"listiterator<int>::next.exit"

"listiterator<int>::next.exit":                   ; preds = %body, %"list<int>::get.exit.i"
  %if_result.i = phi %"option<char>" [ %5, %"list<int>::get.exit.i" ], [ zeroinitializer, %body ]
  %present8 = extractvalue %"option<char>" %if_result.i, 0
  br i1 %present8, label %post_block, label %post_for

post_for:                                         ; preds = %"listiterator<int>::next.exit25", %"listiterator<int>::next.exit"
  %result.0.lcssa = phi i64 [ 0, %"listiterator<int>::next.exit" ], [ %i_add, %"listiterator<int>::next.exit25" ]
  ret i64 %result.0.lcssa

post_block:                                       ; preds = %"listiterator<int>::next.exit", %"listiterator<int>::next.exit25"
  %6 = phi %"option<char>" [ %if_result.i24, %"listiterator<int>::next.exit25" ], [ %if_result.i, %"listiterator<int>::next.exit" ]
  %result.09 = phi i64 [ %i_add, %"listiterator<int>::next.exit25" ], [ 0, %"listiterator<int>::next.exit" ]
  %7 = extractvalue %"option<char>" %6, 1
  %8 = load i64, ptr %7, align 4
  %i_add = add i64 %8, %result.09
  %self.1.i10 = getelementptr inbounds %"listiterator<int>", ptr %0, i64 0, i32 1
  %self.1.load.i11 = load i64, ptr %self.1.i10, align 4
  %self.0.1.i.i12 = getelementptr inbounds %string, ptr %0, i64 0, i32 0, i32 1
  %self.0.1.load.i.i13 = load i64, ptr %self.0.1.i.i12, align 4
  %9 = sdiv i64 %self.0.1.load.i.i13, 8
  %i_ge.not.i14 = icmp slt i64 %self.1.load.i11, %9
  br i1 %i_ge.not.i14, label %else.i16, label %"listiterator<int>::next.exit25"

else.i16:                                         ; preds = %post_block
  %i_le.i.i15 = icmp sgt i64 %self.1.load.i11, -1
  br i1 %i_le.i.i15, label %"list<int>::get.exit.i23", label %else.i.i18

else.i.i18:                                       ; preds = %else.i16
  %stderr.i.i17 = call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %10 = call i64 @fwrite(ptr nonnull @throw_msg.2.5, i64 25, i64 1, ptr %stderr.i.i17)
  %11 = call i64 @fputwc(i32 10, ptr %stderr.i.i17)
  call void @exit(i64 1)
  unreachable

"list<int>::get.exit.i23":                        ; preds = %else.i16
  %self.0.0.load.i.i19 = load ptr, ptr %0, align 8
  %gep.i.i20 = getelementptr i64, ptr %self.0.0.load.i.i19, i64 %self.1.load.i11
  %deref.i.i21 = load i64, ptr %gep.i.i20, align 4
  %12 = call dereferenceable_or_null(8) ptr @malloc(i64 8)
  store i64 %deref.i.i21, ptr %12, align 4
  %13 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %12, 1
  %i_add.i22 = add nuw nsw i64 %self.1.load.i11, 1
  store i64 %i_add.i22, ptr %self.1.i10, align 4
  br label %"listiterator<int>::next.exit25"

"listiterator<int>::next.exit25":                 ; preds = %post_block, %"list<int>::get.exit.i23"
  %if_result.i24 = phi %"option<char>" [ %13, %"list<int>::get.exit.i23" ], [ zeroinitializer, %post_block ]
  %present = extractvalue %"option<char>" %if_result.i24, 0
  br i1 %present, label %post_block, label %post_for
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define %"listiterator<int>" @"list<int>::iterator"(ptr nocapture readonly %self) local_unnamed_addr #4 {
body:
  %.unpack.unpack.i = load ptr, ptr %self, align 8
  %0 = insertvalue %"#dynarray" undef, ptr %.unpack.unpack.i, 0
  %.unpack.elt1.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %.unpack.unpack2.i = load i64, ptr %.unpack.elt1.i, align 8
  %1 = insertvalue %"#dynarray" %0, i64 %.unpack.unpack2.i, 1
  %.unpack.elt3.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %.unpack.unpack4.i = load i64, ptr %.unpack.elt3.i, align 8
  %.unpack5.i = insertvalue %"#dynarray" %1, i64 %.unpack.unpack4.i, 2
  %2 = insertvalue %string undef, %"#dynarray" %.unpack5.i, 0
  %3 = insertvalue %"listiterator<int>" zeroinitializer, %string %2, 0
  %4 = insertvalue %"listiterator<int>" %3, i64 0, 1
  ret %"listiterator<int>" %4
}

define %"option<char>" @"listiterator<int>::next"(ptr nocapture %self) local_unnamed_addr {
body:
  %self.1 = getelementptr inbounds %"listiterator<int>", ptr %self, i64 0, i32 1
  %self.1.load = load i64, ptr %self.1, align 4
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %0 = sdiv i64 %self.0.1.load.i, 8
  %i_ge.not = icmp slt i64 %self.1.load, %0
  br i1 %i_ge.not, label %else, label %merge

else:                                             ; preds = %body
  %i_le.i = icmp sgt i64 %self.1.load, -1
  br i1 %i_le.i, label %"list<int>::get.exit", label %else.i

else.i:                                           ; preds = %else
  %stderr.i = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %1 = tail call i64 @fwrite(ptr nonnull @throw_msg.2.5, i64 25, i64 1, ptr %stderr.i)
  %2 = tail call i64 @fputwc(i32 10, ptr %stderr.i)
  tail call void @exit(i64 1)
  unreachable

"list<int>::get.exit":                            ; preds = %else
  %self.0.0.load.i = load ptr, ptr %self, align 8
  %gep.i = getelementptr i64, ptr %self.0.0.load.i, i64 %self.1.load
  %deref.i = load i64, ptr %gep.i, align 4
  %3 = tail call dereferenceable_or_null(8) ptr @malloc(i64 8)
  store i64 %deref.i, ptr %3, align 4
  %4 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %3, 1
  %i_add = add nuw nsw i64 %self.1.load, 1
  store i64 %i_add, ptr %self.1, align 4
  br label %merge

merge:                                            ; preds = %body, %"list<int>::get.exit"
  %if_result = phi %"option<char>" [ %4, %"list<int>::get.exit" ], [ zeroinitializer, %body ]
  ret %"option<char>" %if_result
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define i64 @"list<int>::len"(ptr nocapture readonly %self) local_unnamed_addr #4 {
body:
  %self.0.1 = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load = load i64, ptr %self.0.1, align 4
  %0 = sdiv i64 %self.0.1.load, 8
  ret i64 %0
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define %"option<char>" @"option<int>::none"() local_unnamed_addr #1 {
body:
  ret %"option<char>" zeroinitializer
}

define i64 @"list<int>::get"(ptr nocapture readonly %self, i64 %i) local_unnamed_addr {
body:
  %i_le = icmp sgt i64 %i, -1
  br i1 %i_le, label %and_true, label %else

else:                                             ; preds = %body, %and_true
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.2.5, i64 25, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

merge:                                            ; preds = %and_true
  %self.0.0.load = load ptr, ptr %self, align 8
  %gep = getelementptr i64, ptr %self.0.0.load, i64 %i
  %deref = load i64, ptr %gep, align 4
  ret i64 %deref

and_true:                                         ; preds = %body
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %2 = sdiv i64 %self.0.1.load.i, 8
  %i_lt = icmp sgt i64 %2, %i
  br i1 %i_lt, label %merge, label %else
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %"option<char>" @"option<int>::some"(i64 %t) local_unnamed_addr #2 {
body:
  %0 = tail call dereferenceable_or_null(8) ptr @malloc(i64 8)
  store i64 %t, ptr %0, align 4
  %1 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %0, 1
  ret %"option<char>" %1
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define %"listiterator<int>" @"listiterator<int>::new"(ptr nocapture readonly %inner) local_unnamed_addr #4 {
body:
  %.unpack.unpack = load ptr, ptr %inner, align 8
  %0 = insertvalue %"#dynarray" undef, ptr %.unpack.unpack, 0
  %.unpack.elt1 = getelementptr inbounds %"#dynarray", ptr %inner, i64 0, i32 1
  %.unpack.unpack2 = load i64, ptr %.unpack.elt1, align 8
  %1 = insertvalue %"#dynarray" %0, i64 %.unpack.unpack2, 1
  %.unpack.elt3 = getelementptr inbounds %"#dynarray", ptr %inner, i64 0, i32 2
  %.unpack.unpack4 = load i64, ptr %.unpack.elt3, align 8
  %.unpack5 = insertvalue %"#dynarray" %1, i64 %.unpack.unpack4, 2
  %2 = insertvalue %string undef, %"#dynarray" %.unpack5, 0
  %3 = insertvalue %"listiterator<int>" zeroinitializer, %string %2, 0
  %4 = insertvalue %"listiterator<int>" %3, i64 0, 1
  ret %"listiterator<int>" %4
}

define %string @"list<int>::to_string"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %0 = alloca ptr, align 8
  %1 = alloca ptr, align 8
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %self.0.1.load.i.off = add i64 %self.0.1.load.i, 7
  %2 = icmp ult i64 %self.0.1.load.i.off, 15
  br i1 %2, label %then, label %else

then:                                             ; preds = %body
  %3 = alloca [2 x i8], align 2
  store i8 91, ptr %3, align 2
  %.repack59 = getelementptr inbounds [2 x i8], ptr %3, i64 0, i64 1
  store i8 93, ptr %.repack59, align 1
  %4 = tail call dereferenceable_or_null(2) ptr @malloc(i64 2)
  %5 = load i16, ptr %3, align 2
  store i16 %5, ptr %4, align 1
  br label %merge

else:                                             ; preds = %body
  %6 = tail call dereferenceable_or_null(1) ptr @malloc(i64 1)
  store i8 91, ptr %6, align 1
  %it = alloca %"listiterator<int>", align 8
  %.unpack.unpack.i.i = load ptr, ptr %self, align 8
  %.unpack.elt3.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %.unpack.unpack4.i.i = load i64, ptr %.unpack.elt3.i.i, align 8
  store ptr %.unpack.unpack.i.i, ptr %it, align 8
  %it.repack9 = getelementptr inbounds %"#dynarray", ptr %it, i64 0, i32 1
  store i64 %self.0.1.load.i, ptr %it.repack9, align 8
  %it.repack11 = getelementptr inbounds %"#dynarray", ptr %it, i64 0, i32 2
  store i64 %.unpack.unpack4.i.i, ptr %it.repack11, align 8
  %it.repack6 = getelementptr inbounds %"listiterator<int>", ptr %it, i64 0, i32 1
  store i64 0, ptr %it.repack6, align 8
  %self.1.i125 = getelementptr inbounds %"listiterator<int>", ptr %it, i64 0, i32 1
  %self.1.load.i126 = load i64, ptr %self.1.i125, align 4
  %self.0.1.i.i = getelementptr inbounds %string, ptr %it, i64 0, i32 0, i32 1
  %self.0.1.load.i.i127 = load i64, ptr %self.0.1.i.i, align 4
  %7 = sdiv i64 %self.0.1.load.i.i127, 8
  %i_ge.not.i = icmp slt i64 %self.1.load.i126, %7
  br i1 %i_ge.not.i, label %else.i128, label %"listiterator<int>::next.exit"

else.i128:                                        ; preds = %else
  %i_le.i.i = icmp sgt i64 %self.1.load.i126, -1
  br i1 %i_le.i.i, label %"list<int>::get.exit.i", label %else.i.i

else.i.i:                                         ; preds = %else.i128
  %stderr.i.i = call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %8 = call i64 @fwrite(ptr nonnull @throw_msg.2.5, i64 25, i64 1, ptr %stderr.i.i)
  %9 = call i64 @fputwc(i32 10, ptr %stderr.i.i)
  call void @exit(i64 1)
  unreachable

"list<int>::get.exit.i":                          ; preds = %else.i128
  %self.0.0.load.i.i = load ptr, ptr %it, align 8
  %gep.i.i129 = getelementptr i64, ptr %self.0.0.load.i.i, i64 %self.1.load.i126
  %deref.i.i = load i64, ptr %gep.i.i129, align 4
  %10 = call dereferenceable_or_null(8) ptr @malloc(i64 8)
  store i64 %deref.i.i, ptr %10, align 4
  %11 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %10, 1
  %i_add.i130 = add nuw nsw i64 %self.1.load.i126, 1
  store i64 %i_add.i130, ptr %self.1.i125, align 4
  br label %"listiterator<int>::next.exit"

"listiterator<int>::next.exit":                   ; preds = %else, %"list<int>::get.exit.i"
  %if_result.i = phi %"option<char>" [ %11, %"list<int>::get.exit.i" ], [ zeroinitializer, %else ]
  %12 = alloca %"option<char>", align 8
  store %"option<char>" %if_result.i, ptr %12, align 8
  %self.0.load.i = load i1, ptr %12, align 8
  br i1 %self.0.load.i, label %"option<int>::get.exit", label %else.i

else.i:                                           ; preds = %"listiterator<int>::next.exit"
  %stderr.i = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %13 = tail call i64 @fwrite(ptr nonnull @throw_msg.4, i64 16, i64 1, ptr %stderr.i)
  %14 = tail call i64 @fputwc(i32 10, ptr %stderr.i)
  tail call void @exit(i64 1)
  unreachable

"option<int>::get.exit":                          ; preds = %"listiterator<int>::next.exit"
  %self.1.i = getelementptr inbounds %"option<char>", ptr %12, i64 0, i32 1
  %self.1.load.i = load ptr, ptr %self.1.i, align 8
  %deref.i = load i64, ptr %self.1.load.i, align 4
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %1)
  %15 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %1, ptr nonnull @_tmpl_int_to_string, i64 %deref.i)
  %deref.i70 = load ptr, ptr %1, align 8
  %i_add.i71 = add i64 %15, 1
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %1)
  %16 = call ptr @malloc(i64 %i_add.i71)
  %i_lt.i.i21.i78 = icmp ugt i64 %15, 9223372036854775806
  br i1 %i_lt.i.i21.i78, label %"#dynarray::extend.exit31.i79", label %"string::add_string.exit84"

"#dynarray::extend.exit31.i79":                   ; preds = %"option<int>::get.exit"
  %17 = call dereferenceable_or_null(1) ptr @malloc(i64 1)
  call void @free(ptr %16)
  br label %"string::add_string.exit84"

"string::add_string.exit84":                      ; preds = %"option<int>::get.exit", %"#dynarray::extend.exit31.i79"
  %.sink.i80 = phi ptr [ %17, %"#dynarray::extend.exit31.i79" ], [ %16, %"option<int>::get.exit" ]
  %18 = load i8, ptr %6, align 1
  store i8 %18, ptr %.sink.i80, align 1
  %gep.i.i83 = getelementptr i8, ptr %.sink.i80, i64 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %gep.i.i83, ptr align 1 %deref.i70, i64 %15, i1 false)
  %self.1.i131 = getelementptr inbounds %"listiterator<int>", ptr %it, i64 0, i32 1
  %self.1.load.i132 = load i64, ptr %self.1.i131, align 4
  %self.0.1.i.i133 = getelementptr inbounds %string, ptr %it, i64 0, i32 0, i32 1
  %self.0.1.load.i.i134 = load i64, ptr %self.0.1.i.i133, align 4
  %19 = sdiv i64 %self.0.1.load.i.i134, 8
  %i_ge.not.i135 = icmp slt i64 %self.1.load.i132, %19
  br i1 %i_ge.not.i135, label %else.i137, label %"listiterator<int>::next.exit146"

else.i137:                                        ; preds = %"string::add_string.exit84"
  %i_le.i.i136 = icmp sgt i64 %self.1.load.i132, -1
  br i1 %i_le.i.i136, label %"list<int>::get.exit.i144", label %else.i.i139

else.i.i139:                                      ; preds = %else.i137
  %stderr.i.i138 = call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %20 = call i64 @fwrite(ptr nonnull @throw_msg.2.5, i64 25, i64 1, ptr %stderr.i.i138)
  %21 = call i64 @fputwc(i32 10, ptr %stderr.i.i138)
  call void @exit(i64 1)
  unreachable

"list<int>::get.exit.i144":                       ; preds = %else.i137
  %self.0.0.load.i.i140 = load ptr, ptr %it, align 8
  %gep.i.i141 = getelementptr i64, ptr %self.0.0.load.i.i140, i64 %self.1.load.i132
  %deref.i.i142 = load i64, ptr %gep.i.i141, align 4
  %22 = call dereferenceable_or_null(8) ptr @malloc(i64 8)
  store i64 %deref.i.i142, ptr %22, align 4
  %23 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %22, 1
  %i_add.i143 = add nuw nsw i64 %self.1.load.i132, 1
  store i64 %i_add.i143, ptr %self.1.i131, align 4
  br label %"listiterator<int>::next.exit146"

"listiterator<int>::next.exit146":                ; preds = %"string::add_string.exit84", %"list<int>::get.exit.i144"
  %if_result.i145 = phi %"option<char>" [ %23, %"list<int>::get.exit.i144" ], [ zeroinitializer, %"string::add_string.exit84" ]
  %present69 = extractvalue %"option<char>" %if_result.i145, 0
  br i1 %present69, label %for, label %block5

merge:                                            ; preds = %"string::add_string.exit", %then
  %.pn120 = phi ptr [ %4, %then ], [ %.sink.i, %"string::add_string.exit" ]
  %.pn118 = phi i64 [ 2, %then ], [ %i_add.i, %"string::add_string.exit" ]
  %.pn116 = phi i64 [ 2, %then ], [ %result_inner.sroa.15.1.i, %"string::add_string.exit" ]
  %.pn117 = insertvalue %"#dynarray" undef, ptr %.pn120, 0
  %.pn115 = insertvalue %"#dynarray" %.pn117, i64 %.pn118, 1
  %.pn114 = insertvalue %"#dynarray" %.pn115, i64 %.pn116, 2
  %if_result = insertvalue %string zeroinitializer, %"#dynarray" %.pn114, 0
  ret %string %if_result

for:                                              ; preds = %"listiterator<int>::next.exit146", %"listiterator<int>::next.exit162"
  %self.0.0.load.i90 = phi ptr [ %.sink.i108, %"listiterator<int>::next.exit162" ], [ %.sink.i80, %"listiterator<int>::next.exit146" ]
  %self.0.1.load.i.i86 = phi i64 [ %i_add.i104, %"listiterator<int>::next.exit162" ], [ %i_add.i71, %"listiterator<int>::next.exit146" ]
  %24 = phi %"option<char>" [ %if_result.i161, %"listiterator<int>::next.exit162" ], [ %if_result.i145, %"listiterator<int>::next.exit146" ]
  %25 = extractvalue %"option<char>" %24, 1
  %26 = load i64, ptr %25, align 4
  %27 = alloca [2 x i8], align 2
  store i8 44, ptr %27, align 2
  %.repack38 = getelementptr inbounds [2 x i8], ptr %27, i64 0, i64 1
  store i8 32, ptr %.repack38, align 1
  %28 = call dereferenceable_or_null(2) ptr @malloc(i64 2)
  %29 = load i16, ptr %27, align 2
  store i16 %29, ptr %28, align 1
  %i_add.i89 = add i64 %self.0.1.load.i.i86, 2
  %30 = call ptr @malloc(i64 %i_add.i89)
  %i_lt.i.i21.i91 = icmp sgt i64 %self.0.1.load.i.i86, 9223372036854775805
  br i1 %i_lt.i.i21.i91, label %"#dynarray::extend.exit31.i92", label %"string::add_string.exit97"

"#dynarray::extend.exit31.i92":                   ; preds = %for
  %31 = call ptr @malloc(i64 %self.0.1.load.i.i86)
  call void @free(ptr %30)
  br label %"string::add_string.exit97"

"string::add_string.exit97":                      ; preds = %for, %"#dynarray::extend.exit31.i92"
  %.sink.i93 = phi ptr [ %31, %"#dynarray::extend.exit31.i92" ], [ %30, %for ]
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %.sink.i93, ptr align 1 %self.0.0.load.i90, i64 %self.0.1.load.i.i86, i1 false)
  %gep.i.i96 = getelementptr i8, ptr %.sink.i93, i64 %self.0.1.load.i.i86
  %32 = load i16, ptr %28, align 1
  store i16 %32, ptr %gep.i.i96, align 1
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %0)
  %33 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %0, ptr nonnull @_tmpl_int_to_string, i64 %26)
  %deref.i98 = load ptr, ptr %0, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %0)
  %i_add.i104 = add i64 %33, %i_add.i89
  %34 = call ptr @malloc(i64 %i_add.i104)
  %i_lt.i.i21.i106 = icmp slt i64 %i_add.i104, %i_add.i89
  br i1 %i_lt.i.i21.i106, label %"#dynarray::extend.exit31.i107", label %"string::add_string.exit112"

"#dynarray::extend.exit31.i107":                  ; preds = %"string::add_string.exit97"
  %35 = call ptr @malloc(i64 %i_add.i89)
  call void @free(ptr %34)
  br label %"string::add_string.exit112"

"string::add_string.exit112":                     ; preds = %"string::add_string.exit97", %"#dynarray::extend.exit31.i107"
  %.sink.i108 = phi ptr [ %35, %"#dynarray::extend.exit31.i107" ], [ %34, %"string::add_string.exit97" ]
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %.sink.i108, ptr nonnull align 1 %.sink.i93, i64 %i_add.i89, i1 false)
  %gep.i.i111 = getelementptr i8, ptr %.sink.i108, i64 %i_add.i89
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %gep.i.i111, ptr align 1 %deref.i98, i64 %33, i1 false)
  %self.1.i147 = getelementptr inbounds %"listiterator<int>", ptr %it, i64 0, i32 1
  %self.1.load.i148 = load i64, ptr %self.1.i147, align 4
  %self.0.1.i.i149 = getelementptr inbounds %string, ptr %it, i64 0, i32 0, i32 1
  %self.0.1.load.i.i150 = load i64, ptr %self.0.1.i.i149, align 4
  %36 = sdiv i64 %self.0.1.load.i.i150, 8
  %i_ge.not.i151 = icmp slt i64 %self.1.load.i148, %36
  br i1 %i_ge.not.i151, label %else.i153, label %"listiterator<int>::next.exit162"

else.i153:                                        ; preds = %"string::add_string.exit112"
  %i_le.i.i152 = icmp sgt i64 %self.1.load.i148, -1
  br i1 %i_le.i.i152, label %"list<int>::get.exit.i160", label %else.i.i155

else.i.i155:                                      ; preds = %else.i153
  %stderr.i.i154 = call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %37 = call i64 @fwrite(ptr nonnull @throw_msg.2.5, i64 25, i64 1, ptr %stderr.i.i154)
  %38 = call i64 @fputwc(i32 10, ptr %stderr.i.i154)
  call void @exit(i64 1)
  unreachable

"list<int>::get.exit.i160":                       ; preds = %else.i153
  %self.0.0.load.i.i156 = load ptr, ptr %it, align 8
  %gep.i.i157 = getelementptr i64, ptr %self.0.0.load.i.i156, i64 %self.1.load.i148
  %deref.i.i158 = load i64, ptr %gep.i.i157, align 4
  %39 = call dereferenceable_or_null(8) ptr @malloc(i64 8)
  store i64 %deref.i.i158, ptr %39, align 4
  %40 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %39, 1
  %i_add.i159 = add nuw nsw i64 %self.1.load.i148, 1
  store i64 %i_add.i159, ptr %self.1.i147, align 4
  br label %"listiterator<int>::next.exit162"

"listiterator<int>::next.exit162":                ; preds = %"string::add_string.exit112", %"list<int>::get.exit.i160"
  %if_result.i161 = phi %"option<char>" [ %40, %"list<int>::get.exit.i160" ], [ zeroinitializer, %"string::add_string.exit112" ]
  %present = extractvalue %"option<char>" %if_result.i161, 0
  br i1 %present, label %for, label %block5

block5:                                           ; preds = %"listiterator<int>::next.exit162", %"listiterator<int>::next.exit146"
  %self.0.0.load.i = phi ptr [ %.sink.i80, %"listiterator<int>::next.exit146" ], [ %.sink.i108, %"listiterator<int>::next.exit162" ]
  %self.0.1.load.i.i = phi i64 [ %i_add.i71, %"listiterator<int>::next.exit146" ], [ %i_add.i104, %"listiterator<int>::next.exit162" ]
  %41 = call dereferenceable_or_null(1) ptr @malloc(i64 1)
  store i8 93, ptr %41, align 1
  %i_add.i = add i64 %self.0.1.load.i.i, 1
  %42 = call ptr @malloc(i64 %i_add.i)
  %i_lt.i.i21.i = icmp eq i64 %self.0.1.load.i.i, 9223372036854775807
  br i1 %i_lt.i.i21.i, label %"#dynarray::extend.exit31.i", label %"string::add_string.exit"

"#dynarray::extend.exit31.i":                     ; preds = %block5
  %43 = call ptr @malloc(i64 %self.0.1.load.i.i)
  call void @free(ptr %42)
  br label %"string::add_string.exit"

"string::add_string.exit":                        ; preds = %block5, %"#dynarray::extend.exit31.i"
  %.sink.i = phi ptr [ %43, %"#dynarray::extend.exit31.i" ], [ %42, %block5 ]
  %result_inner.sroa.15.1.i = phi i64 [ 9223372036854775807, %"#dynarray::extend.exit31.i" ], [ %i_add.i, %block5 ]
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %.sink.i, ptr align 1 %self.0.0.load.i, i64 %self.0.1.load.i.i, i1 false)
  %gep.i.i = getelementptr i8, ptr %.sink.i, i64 %self.0.1.load.i.i
  %44 = load i8, ptr %41, align 1
  store i8 %44, ptr %gep.i.i, align 1
  br label %merge
}

define i64 @"option<int>::get"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %self.0.load = load i1, ptr %self, align 1
  br i1 %self.0.load, label %merge, label %else

else:                                             ; preds = %body
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.4, i64 16, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

merge:                                            ; preds = %body
  %self.1 = getelementptr inbounds %"option<char>", ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  %deref = load i64, ptr %self.1.load, align 4
  ret i64 %deref
}

define void @"list<int>::set"(ptr nocapture readonly %self, i64 %i, i64 %t) local_unnamed_addr {
body:
  %i_le = icmp sgt i64 %i, -1
  br i1 %i_le, label %and_true, label %else

else:                                             ; preds = %body, %and_true
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.2.5, i64 25, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

and_true:                                         ; preds = %body
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %2 = sdiv i64 %self.0.1.load.i, 8
  %i_lt = icmp sgt i64 %2, %i
  br i1 %i_lt, label %block, label %else

block:                                            ; preds = %and_true
  %self.0.0.load = load ptr, ptr %self, align 8
  %gep = getelementptr i64, ptr %self.0.0.load, i64 %i
  store i64 %t, ptr %gep, align 4
  ret void
}

define void @"list<int>::extend"(ptr nocapture %self, ptr nocapture readonly %lst) local_unnamed_addr {
body:
  %.unpack.unpack.i.i = load ptr, ptr %lst, align 8
  %.unpack.elt1.i.i = getelementptr inbounds %"#dynarray", ptr %lst, i64 0, i32 1
  %.unpack.unpack2.i.i = load i64, ptr %.unpack.elt1.i.i, align 8
  %.unpack.elt3.i.i = getelementptr inbounds %"#dynarray", ptr %lst, i64 0, i32 2
  %.unpack.unpack4.i.i = load i64, ptr %.unpack.elt3.i.i, align 8
  %0 = alloca %"listiterator<int>", align 8
  store ptr %.unpack.unpack.i.i, ptr %0, align 8
  %.repack4 = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 1
  store i64 %.unpack.unpack2.i.i, ptr %.repack4, align 8
  %.repack6 = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 2
  store i64 %.unpack.unpack4.i.i, ptr %.repack6, align 8
  %.repack1 = getelementptr inbounds %"listiterator<int>", ptr %0, i64 0, i32 1
  store i64 0, ptr %.repack1, align 8
  %self.1.i = getelementptr inbounds %"listiterator<int>", ptr %0, i64 0, i32 1
  %self.1.load.i = load i64, ptr %self.1.i, align 4
  %self.0.1.i.i = getelementptr inbounds %string, ptr %0, i64 0, i32 0, i32 1
  %self.0.1.load.i.i = load i64, ptr %self.0.1.i.i, align 4
  %1 = sdiv i64 %self.0.1.load.i.i, 8
  %i_ge.not.i = icmp slt i64 %self.1.load.i, %1
  br i1 %i_ge.not.i, label %else.i, label %"listiterator<int>::next.exit"

else.i:                                           ; preds = %body
  %i_le.i.i = icmp sgt i64 %self.1.load.i, -1
  br i1 %i_le.i.i, label %"list<int>::get.exit.i", label %else.i.i

else.i.i:                                         ; preds = %else.i
  %stderr.i.i = call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %2 = call i64 @fwrite(ptr nonnull @throw_msg.2.5, i64 25, i64 1, ptr %stderr.i.i)
  %3 = call i64 @fputwc(i32 10, ptr %stderr.i.i)
  call void @exit(i64 1)
  unreachable

"list<int>::get.exit.i":                          ; preds = %else.i
  %self.0.0.load.i.i = load ptr, ptr %0, align 8
  %gep.i.i9 = getelementptr i64, ptr %self.0.0.load.i.i, i64 %self.1.load.i
  %deref.i.i = load i64, ptr %gep.i.i9, align 4
  %4 = call dereferenceable_or_null(8) ptr @malloc(i64 8)
  store i64 %deref.i.i, ptr %4, align 4
  %5 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %4, 1
  %i_add.i = add nuw nsw i64 %self.1.load.i, 1
  store i64 %i_add.i, ptr %self.1.i, align 4
  br label %"listiterator<int>::next.exit"

"listiterator<int>::next.exit":                   ; preds = %body, %"list<int>::get.exit.i"
  %if_result.i = phi %"option<char>" [ %5, %"list<int>::get.exit.i" ], [ zeroinitializer, %body ]
  %present8 = extractvalue %"option<char>" %if_result.i, 0
  br i1 %present8, label %for.preheader, label %post_for

for.preheader:                                    ; preds = %"listiterator<int>::next.exit"
  %self.1.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %self.2.i.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  br label %for

for:                                              ; preds = %for.preheader, %"listiterator<int>::next.exit25"
  %6 = phi %"option<char>" [ %if_result.i24, %"listiterator<int>::next.exit25" ], [ %if_result.i, %for.preheader ]
  %7 = extractvalue %"option<char>" %6, 1
  %8 = load i64, ptr %7, align 4
  %self.1.load.i.i = load i64, ptr %self.1.i.i, align 4
  %i_add.i.i = add i64 %self.1.load.i.i, 8
  %self.2.load.i.i.i = load i64, ptr %self.2.i.i.i, align 4
  %i_lt.i.i.i = icmp slt i64 %self.2.load.i.i.i, %i_add.i.i
  br i1 %i_lt.i.i.i, label %then.i.i.i, label %"list<int>::push.exit"

then.i.i.i:                                       ; preds = %for
  %self.0.load.i.i.i = load ptr, ptr %self, align 8
  %9 = tail call ptr @malloc(i64 %i_add.i.i)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %9, ptr align 1 %self.0.load.i.i.i, i64 %self.1.load.i.i, i1 false)
  store ptr %9, ptr %self, align 8
  store i64 %i_add.i.i, ptr %self.2.i.i.i, align 4
  tail call void @free(ptr %self.0.load.i.i.i)
  %self.13.load.pre.i.i = load i64, ptr %self.1.i.i, align 4
  br label %"list<int>::push.exit"

"list<int>::push.exit":                           ; preds = %for, %then.i.i.i
  %self.13.load.i.i = phi i64 [ %self.13.load.pre.i.i, %then.i.i.i ], [ %self.1.load.i.i, %for ]
  %self.0.load.i.i = load ptr, ptr %self, align 8
  %gep.i.i = getelementptr i8, ptr %self.0.load.i.i, i64 %self.13.load.i.i
  store i64 %8, ptr %gep.i.i, align 1
  %self.14.load.i.i = load i64, ptr %self.1.i.i, align 4
  %i_add5.i.i = add i64 %self.14.load.i.i, 8
  store i64 %i_add5.i.i, ptr %self.1.i.i, align 4
  %self.1.i10 = getelementptr inbounds %"listiterator<int>", ptr %0, i64 0, i32 1
  %self.1.load.i11 = load i64, ptr %self.1.i10, align 4
  %self.0.1.i.i12 = getelementptr inbounds %string, ptr %0, i64 0, i32 0, i32 1
  %self.0.1.load.i.i13 = load i64, ptr %self.0.1.i.i12, align 4
  %10 = sdiv i64 %self.0.1.load.i.i13, 8
  %i_ge.not.i14 = icmp slt i64 %self.1.load.i11, %10
  br i1 %i_ge.not.i14, label %else.i16, label %"listiterator<int>::next.exit25"

else.i16:                                         ; preds = %"list<int>::push.exit"
  %i_le.i.i15 = icmp sgt i64 %self.1.load.i11, -1
  br i1 %i_le.i.i15, label %"list<int>::get.exit.i23", label %else.i.i18

else.i.i18:                                       ; preds = %else.i16
  %stderr.i.i17 = call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %11 = call i64 @fwrite(ptr nonnull @throw_msg.2.5, i64 25, i64 1, ptr %stderr.i.i17)
  %12 = call i64 @fputwc(i32 10, ptr %stderr.i.i17)
  call void @exit(i64 1)
  unreachable

"list<int>::get.exit.i23":                        ; preds = %else.i16
  %self.0.0.load.i.i19 = load ptr, ptr %0, align 8
  %gep.i.i20 = getelementptr i64, ptr %self.0.0.load.i.i19, i64 %self.1.load.i11
  %deref.i.i21 = load i64, ptr %gep.i.i20, align 4
  %13 = call dereferenceable_or_null(8) ptr @malloc(i64 8)
  store i64 %deref.i.i21, ptr %13, align 4
  %14 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %13, 1
  %i_add.i22 = add nuw nsw i64 %self.1.load.i11, 1
  store i64 %i_add.i22, ptr %self.1.i10, align 4
  br label %"listiterator<int>::next.exit25"

"listiterator<int>::next.exit25":                 ; preds = %"list<int>::push.exit", %"list<int>::get.exit.i23"
  %if_result.i24 = phi %"option<char>" [ %14, %"list<int>::get.exit.i23" ], [ zeroinitializer, %"list<int>::push.exit" ]
  %present = extractvalue %"option<char>" %if_result.i24, 0
  br i1 %present, label %for, label %post_for

post_for:                                         ; preds = %"listiterator<int>::next.exit25", %"listiterator<int>::next.exit"
  ret void
}

; Function Attrs: mustprogress nounwind willreturn
define void @"list<int>::push"(ptr nocapture %self, i64 %e) local_unnamed_addr #5 {
body:
  %self.1.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %self.1.load.i = load i64, ptr %self.1.i, align 4
  %i_add.i = add i64 %self.1.load.i, 8
  %self.2.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %self.2.load.i.i = load i64, ptr %self.2.i.i, align 4
  %i_lt.i.i = icmp slt i64 %self.2.load.i.i, %i_add.i
  br i1 %i_lt.i.i, label %then.i.i, label %"#dynarray::extend.exit"

then.i.i:                                         ; preds = %body
  %self.0.load.i.i = load ptr, ptr %self, align 8
  %0 = tail call ptr @malloc(i64 %i_add.i)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %0, ptr align 1 %self.0.load.i.i, i64 %self.1.load.i, i1 false)
  store ptr %0, ptr %self, align 8
  store i64 %i_add.i, ptr %self.2.i.i, align 4
  tail call void @free(ptr %self.0.load.i.i)
  %self.13.load.pre.i = load i64, ptr %self.1.i, align 4
  br label %"#dynarray::extend.exit"

"#dynarray::extend.exit":                         ; preds = %body, %then.i.i
  %self.13.load.i = phi i64 [ %self.13.load.pre.i, %then.i.i ], [ %self.1.load.i, %body ]
  %self.0.load.i = load ptr, ptr %self, align 8
  %gep.i = getelementptr i8, ptr %self.0.load.i, i64 %self.13.load.i
  store i64 %e, ptr %gep.i, align 1
  %self.14.load.i = load i64, ptr %self.1.i, align 4
  %i_add5.i = add i64 %self.14.load.i, 8
  store i64 %i_add5.i, ptr %self.1.i, align 4
  ret void
}

define i64 @"list<int>::pop"(ptr nocapture %self) local_unnamed_addr {
body:
  %self.1.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %self.1.load.i = load i64, ptr %self.1.i, align 4
  %i_ge.not.i = icmp slt i64 %self.1.load.i, 8
  br i1 %i_ge.not.i, label %else.i, label %"#dynarray::take.exit"

else.i:                                           ; preds = %body
  %stderr.i = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.1, i64 30, i64 1, ptr %stderr.i)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr.i)
  tail call void @exit(i64 1)
  unreachable

"#dynarray::take.exit":                           ; preds = %body
  %i_sub.i = add nsw i64 %self.1.load.i, -8
  store i64 %i_sub.i, ptr %self.1.i, align 4
  %self.0.load.i = load ptr, ptr %self, align 8
  %gep.i = getelementptr i8, ptr %self.0.load.i, i64 %i_sub.i
  %deref = load i64, ptr %gep.i, align 4
  ret i64 %deref
}

define %string @"option<int>::to_string"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %0 = alloca ptr, align 8
  %self.0.load = load i1, ptr %self, align 1
  br i1 %self.0.load, label %then, label %else

then:                                             ; preds = %body
  %self.1 = getelementptr inbounds %"option<char>", ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  %deref = load i64, ptr %self.1.load, align 4
  %1 = alloca [5 x i8], align 1
  store i8 115, ptr %1, align 1
  %.repack4 = getelementptr inbounds [5 x i8], ptr %1, i64 0, i64 1
  store i8 111, ptr %.repack4, align 1
  %.repack5 = getelementptr inbounds [5 x i8], ptr %1, i64 0, i64 2
  store i8 109, ptr %.repack5, align 1
  %.repack6 = getelementptr inbounds [5 x i8], ptr %1, i64 0, i64 3
  store i8 101, ptr %.repack6, align 1
  %.repack7 = getelementptr inbounds [5 x i8], ptr %1, i64 0, i64 4
  store i8 40, ptr %.repack7, align 1
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %0)
  %2 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %0, ptr nonnull @_tmpl_int_to_string, i64 %deref)
  %deref.i = load ptr, ptr %0, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %0)
  %i_add.i27 = add i64 %2, 5
  %3 = call ptr @malloc(i64 %i_add.i27)
  %i_lt.i.i21.i = icmp ugt i64 %2, 9223372036854775802
  br i1 %i_lt.i.i21.i, label %"#dynarray::extend.exit31.i", label %"string::add_string.exit"

"#dynarray::extend.exit31.i":                     ; preds = %then
  %4 = call dereferenceable_or_null(5) ptr @malloc(i64 5)
  call void @free(ptr %3)
  br label %"string::add_string.exit"

"string::add_string.exit":                        ; preds = %then, %"#dynarray::extend.exit31.i"
  %.sink.i = phi ptr [ %4, %"#dynarray::extend.exit31.i" ], [ %3, %then ]
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %.sink.i, ptr noundef nonnull align 1 dereferenceable(5) %1, i64 5, i1 false)
  %gep.i.i = getelementptr i8, ptr %.sink.i, i64 5
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %gep.i.i, ptr align 1 %deref.i, i64 %2, i1 false)
  %5 = call dereferenceable_or_null(1) ptr @malloc(i64 1)
  store i8 41, ptr %5, align 1
  %i_add.i32 = add i64 %2, 6
  %6 = call ptr @malloc(i64 %i_add.i32)
  %i_lt.i.i21.i34 = icmp slt i64 %i_add.i32, %i_add.i27
  br i1 %i_lt.i.i21.i34, label %"#dynarray::extend.exit31.i35", label %"string::add_string.exit40"

"#dynarray::extend.exit31.i35":                   ; preds = %"string::add_string.exit"
  %7 = call ptr @malloc(i64 %i_add.i27)
  call void @free(ptr %6)
  br label %"string::add_string.exit40"

"string::add_string.exit40":                      ; preds = %"string::add_string.exit", %"#dynarray::extend.exit31.i35"
  %.sink.i36 = phi ptr [ %7, %"#dynarray::extend.exit31.i35" ], [ %6, %"string::add_string.exit" ]
  %result_inner.sroa.15.1.i37 = phi i64 [ %i_add.i27, %"#dynarray::extend.exit31.i35" ], [ %i_add.i32, %"string::add_string.exit" ]
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %.sink.i36, ptr nonnull align 1 %.sink.i, i64 %i_add.i27, i1 false)
  %gep.i.i39 = getelementptr i8, ptr %.sink.i36, i64 %i_add.i27
  %8 = load i8, ptr %5, align 1
  store i8 %8, ptr %gep.i.i39, align 1
  br label %merge

else:                                             ; preds = %body
  %9 = alloca [4 x i8], align 4
  store i8 110, ptr %9, align 4
  %.repack1 = getelementptr inbounds [4 x i8], ptr %9, i64 0, i64 1
  store i8 111, ptr %.repack1, align 1
  %.repack2 = getelementptr inbounds [4 x i8], ptr %9, i64 0, i64 2
  store i8 110, ptr %.repack2, align 2
  %.repack3 = getelementptr inbounds [4 x i8], ptr %9, i64 0, i64 3
  store i8 101, ptr %.repack3, align 1
  %10 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  %11 = load i32, ptr %9, align 4
  store i32 %11, ptr %10, align 1
  br label %merge

merge:                                            ; preds = %else, %"string::add_string.exit40"
  %.sink.i36.pn = phi ptr [ %.sink.i36, %"string::add_string.exit40" ], [ %10, %else ]
  %i_add.i32.pn = phi i64 [ %i_add.i32, %"string::add_string.exit40" ], [ 4, %else ]
  %result_inner.sroa.15.1.i37.pn = phi i64 [ %result_inner.sroa.15.1.i37, %"string::add_string.exit40" ], [ 4, %else ]
  %.pn42 = insertvalue %"#dynarray" undef, ptr %.sink.i36.pn, 0
  %.pn = insertvalue %"#dynarray" %.pn42, i64 %i_add.i32.pn, 1
  %.pn41 = insertvalue %"#dynarray" %.pn, i64 %result_inner.sroa.15.1.i37.pn, 2
  %if_result = insertvalue %string zeroinitializer, %"#dynarray" %.pn41, 0
  ret %string %if_result
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %string @"list<int>::from_raw"(ptr nocapture readonly %contents, i64 %len) local_unnamed_addr #2 {
body:
  %i_mul = shl i64 %len, 3
  %0 = tail call ptr @malloc(i64 %i_mul)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %0, ptr align 1 %contents, i64 %i_mul, i1 false)
  %1 = insertvalue %"#dynarray" undef, ptr %0, 0
  %2 = insertvalue %"#dynarray" %1, i64 %i_mul, 1
  %3 = insertvalue %"#dynarray" %2, i64 %i_mul, 2
  %4 = insertvalue %string zeroinitializer, %"#dynarray" %3, 0
  ret %string %4
}

define i8 @main() local_unnamed_addr {
body:
  %a.i = alloca %string, align 8
  %0 = tail call ptr @setlocale(i64 0, ptr nonnull @locale.6)
  call void @llvm.lifetime.start.p0(i64 24, ptr %a.i)
  %1 = tail call dereferenceable_or_null(40) ptr @malloc(i64 40)
  store i64 1, ptr %1, align 1
  %.sroa.215.0.self.0.load.i.i.sroa_idx.i = getelementptr inbounds i8, ptr %1, i64 8
  store i64 2, ptr %.sroa.215.0.self.0.load.i.i.sroa_idx.i, align 1
  %.sroa.3.0.self.0.load.i.i.sroa_idx.i = getelementptr inbounds i8, ptr %1, i64 16
  store i64 3, ptr %.sroa.3.0.self.0.load.i.i.sroa_idx.i, align 1
  %.sroa.416.0.self.0.load.i.i.sroa_idx.i = getelementptr inbounds i8, ptr %1, i64 24
  store i64 4, ptr %.sroa.416.0.self.0.load.i.i.sroa_idx.i, align 1
  %.sroa.5.0.self.0.load.i.i.sroa_idx.i = getelementptr inbounds i8, ptr %1, i64 32
  store i64 5, ptr %.sroa.5.0.self.0.load.i.i.sroa_idx.i, align 1
  store ptr %1, ptr %a.i, align 8
  %a.repack5.i = getelementptr inbounds %"#dynarray", ptr %a.i, i64 0, i32 1
  store i64 40, ptr %a.repack5.i, align 8
  %a.repack7.i = getelementptr inbounds %"#dynarray", ptr %a.i, i64 0, i32 2
  store i64 40, ptr %a.repack7.i, align 8
  %2 = call %string @"list<int>::to_string"(ptr nonnull %a.i)
  %3 = extractvalue %string %2, 0
  %.elt9.i = extractvalue %"#dynarray" %3, 0
  %.elt11.i = extractvalue %"#dynarray" %3, 1
  %4 = tail call i64 (ptr, ...) @printf(ptr nonnull @_tmpl_print, i64 %.elt11.i, ptr %.elt9.i)
  call void @llvm.lifetime.end.p0(i64 24, ptr %a.i)
  ret i8 0
}

; Function Attrs: argmemonly mustprogress nocallback nofree nounwind willreturn writeonly
declare void @llvm.memset.p0.i64(ptr nocapture writeonly, i8, i64, i1 immarg) #12

attributes #0 = { nofree nounwind }
attributes #1 = { mustprogress nofree norecurse nosync nounwind readnone willreturn }
attributes #2 = { mustprogress nofree nounwind willreturn }
attributes #3 = { inaccessiblememonly mustprogress nofree nounwind willreturn allockind("alloc,uninitialized") allocsize(0) "alloc-family"="malloc" }
attributes #4 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn }
attributes #5 = { mustprogress nounwind willreturn }
attributes #6 = { argmemonly mustprogress nocallback nofree nounwind willreturn }
attributes #7 = { inaccessiblemem_or_argmemonly mustprogress nounwind willreturn allockind("free") "alloc-family"="malloc" }
attributes #8 = { mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #9 = { mustprogress nofree nosync nounwind readnone willreturn }
attributes #10 = { argmemonly mustprogress nocallback nofree nosync nounwind willreturn }
attributes #11 = { mustprogress nofree nounwind willreturn writeonly }
attributes #12 = { argmemonly mustprogress nocallback nofree nounwind willreturn writeonly }

!0 = !{i64 0, i64 65}
