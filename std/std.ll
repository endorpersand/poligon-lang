; ModuleID = 'std.gon'
source_filename = "std.gon"

%string = type { %"#dynarray" }
%"#dynarray" = type { ptr, i64, i64 }
%string_chars = type { %string, ptr }
%"option<char>" = type { i1, ptr }

@_tmpl_print = private unnamed_addr constant [6 x i8] c"%.*s\0A\00", align 1
@_tmpl_int_to_string = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@_tmpl_float_to_string = private unnamed_addr constant [4 x i8] c"%#f\00", align 1
@_tmpl_char_to_string = private unnamed_addr constant [4 x i8] c"%lc\00", align 1
@_tmpl_byte_to_string = private unnamed_addr constant [6 x i8] c"%#hhx\00", align 1
@_tmpl_ptr_to_string = private unnamed_addr constant [9 x i8] c"ptr %#lx\00", align 1
@throw_msg = private unnamed_addr constant [14 x i8] c"invalid slice\00", align 1
@_write = private unnamed_addr constant [2 x i8] c"w\00", align 1
@throw_msg.1 = private unnamed_addr constant [31 x i8] c"cannot take element from array\00", align 1
@throw_msg.2 = private unnamed_addr constant [17 x i8] c"no value present\00", align 1
@throw_msg.3 = private unnamed_addr constant [23 x i8] c"division by zero error\00", align 1
@locale = private unnamed_addr constant [12 x i8] c"en_US.UTF-8\00", align 1

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

define %string @"#ptr::to_string"(ptr %self) local_unnamed_addr {
body:
  %0 = alloca ptr, align 8
  %1 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %0, ptr nonnull @_tmpl_ptr_to_string, ptr %self)
  %deref = load ptr, ptr %0, align 8
  %i_add = add i64 %1, 1
  %2 = insertvalue %"#dynarray" zeroinitializer, ptr %deref, 0
  %3 = insertvalue %"#dynarray" %2, i64 %1, 1
  %4 = insertvalue %"#dynarray" %3, i64 %i_add, 2
  %5 = insertvalue %string zeroinitializer, %"#dynarray" %4, 0
  ret %string %5
}

declare i64 @asprintf(ptr, ptr, ...) local_unnamed_addr

; Function Attrs: mustprogress nofree nounwind willreturn
define %string_chars @"string::chars"(ptr nocapture readonly %self) local_unnamed_addr #1 {
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

; Function Attrs: mustprogress nofree nounwind willreturn
define %string_chars @"string_chars::new"(ptr nocapture readonly %str) local_unnamed_addr #1 {
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

; Function Attrs: inaccessiblememonly mustprogress nofree nounwind willreturn allockind("alloc,uninitialized") allocsize(0)
declare noalias noundef ptr @malloc(i64 noundef) local_unnamed_addr #2

define %string @"string::slice_bytes"(ptr nocapture readonly %self, i64 %start, i64 %end) local_unnamed_addr {
body:
  %i_le = icmp sgt i64 %start, -1
  %i_le4 = icmp sge i64 %end, %start
  %and_result = select i1 %i_le, i1 %i_le4, i1 false
  br i1 %and_result, label %and_true6, label %else

else:                                             ; preds = %body, %and_true6
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write)
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

declare i64 @fputwc(i32, ptr) local_unnamed_addr

declare void @exit(i64) local_unnamed_addr

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define i64 @"string::len"(ptr nocapture readonly %self) local_unnamed_addr #3 {
body:
  %self.0.1 = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load = load i64, ptr %self.0.1, align 4
  ret i64 %self.0.1.load
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define %string @"string::to_string"(ptr nocapture readonly %self) local_unnamed_addr #3 {
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
define %string @"string::add_string"(ptr nocapture readonly %self, ptr nocapture readonly %other) local_unnamed_addr #4 {
body:
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %self.0.1.i10 = getelementptr inbounds %string, ptr %other, i64 0, i32 0, i32 1
  %self.0.1.load.i11 = load i64, ptr %self.0.1.i10, align 4
  %i_add = add i64 %self.0.1.load.i11, %self.0.1.load.i
  %0 = tail call ptr @malloc(i64 %i_add)
  %self.0.0.load = load ptr, ptr %self, align 8
  %i_lt.i.i21 = icmp slt i64 %i_add, %self.0.1.load.i
  br i1 %i_lt.i.i21, label %"#dynarray::extend.exit32", label %"#dynarray::extend.exit32.thread"

"#dynarray::extend.exit32.thread":                ; preds = %body
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %0, ptr align 1 %self.0.0.load, i64 %self.0.1.load.i, i1 false)
  %other.0.0.load41 = load ptr, ptr %other, align 8
  br label %"#dynarray::extend.exit"

"#dynarray::extend.exit32":                       ; preds = %body
  %i_mul.i.i22 = shl i64 %i_add, 1
  %1 = tail call i64 @llvm.smax.i64(i64 %self.0.1.load.i, i64 %i_mul.i.i22)
  %2 = tail call ptr @malloc(i64 %1)
  tail call void @free(ptr %0)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %2, ptr align 1 %self.0.0.load, i64 %self.0.1.load.i, i1 false)
  %other.0.0.load = load ptr, ptr %other, align 8
  %i_lt.i.i = icmp slt i64 %1, %i_add
  br i1 %i_lt.i.i, label %then.i.i, label %"#dynarray::extend.exit"

then.i.i:                                         ; preds = %"#dynarray::extend.exit32"
  %i_mul.i.i = shl i64 %1, 1
  %3 = tail call i64 @llvm.smax.i64(i64 %i_add, i64 %i_mul.i.i)
  %4 = tail call ptr @malloc(i64 %3)
  call void @llvm.memmove.p0.p0.i64(ptr align 1 %4, ptr align 1 %self.0.0.load, i64 %self.0.1.load.i, i1 false)
  tail call void @free(ptr %2)
  br label %"#dynarray::extend.exit"

"#dynarray::extend.exit":                         ; preds = %"#dynarray::extend.exit32.thread", %"#dynarray::extend.exit32", %then.i.i
  %other.0.0.load43 = phi ptr [ %other.0.0.load, %then.i.i ], [ %other.0.0.load, %"#dynarray::extend.exit32" ], [ %other.0.0.load41, %"#dynarray::extend.exit32.thread" ]
  %result_inner.sroa.15.1 = phi i64 [ %3, %then.i.i ], [ %1, %"#dynarray::extend.exit32" ], [ %i_add, %"#dynarray::extend.exit32.thread" ]
  %result_inner.sroa.0.1 = phi ptr [ %4, %then.i.i ], [ %2, %"#dynarray::extend.exit32" ], [ %0, %"#dynarray::extend.exit32.thread" ]
  %gep.i = getelementptr i8, ptr %result_inner.sroa.0.1, i64 %self.0.1.load.i
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %gep.i, ptr align 1 %other.0.0.load43, i64 %self.0.1.load.i11, i1 false)
  %5 = insertvalue %"#dynarray" undef, ptr %result_inner.sroa.0.1, 0
  %6 = insertvalue %"#dynarray" %5, i64 %i_add, 1
  %7 = insertvalue %"#dynarray" %6, i64 %result_inner.sroa.15.1, 2
  %8 = insertvalue %string zeroinitializer, %"#dynarray" %7, 0
  ret %string %8
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %"#dynarray" @"#dynarray::new"(i64 %cap) local_unnamed_addr #1 {
body:
  %0 = tail call ptr @malloc(i64 %cap)
  %1 = insertvalue %"#dynarray" zeroinitializer, ptr %0, 0
  %2 = insertvalue %"#dynarray" %1, i64 0, 1
  %3 = insertvalue %"#dynarray" %2, i64 %cap, 2
  ret %"#dynarray" %3
}

; Function Attrs: mustprogress nounwind willreturn
define void @"#dynarray::extend"(ptr nocapture %self, ptr nocapture readonly %add_buf, i64 %add_len) local_unnamed_addr #4 {
body:
  %self.1 = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %self.1.load = load i64, ptr %self.1, align 4
  %i_add = add i64 %self.1.load, %add_len
  %self.2.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %self.2.load.i = load i64, ptr %self.2.i, align 4
  %i_lt.i = icmp slt i64 %self.2.load.i, %i_add
  br i1 %i_lt.i, label %then.i, label %"#dynarray::resize.exit"

then.i:                                           ; preds = %body
  %i_mul.i = shl i64 %self.2.load.i, 1
  %0 = tail call i64 @llvm.smax.i64(i64 %i_add, i64 %i_mul.i)
  %self.0.load.i = load ptr, ptr %self, align 8
  %1 = tail call ptr @malloc(i64 %0)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %1, ptr align 1 %self.0.load.i, i64 %self.1.load, i1 false)
  store ptr %1, ptr %self, align 8
  store i64 %0, ptr %self.2.i, align 4
  tail call void @free(ptr %self.0.load.i)
  %self.13.load.pre = load i64, ptr %self.1, align 4
  br label %"#dynarray::resize.exit"

"#dynarray::resize.exit":                         ; preds = %body, %then.i
  %self.13.load = phi i64 [ %self.1.load, %body ], [ %self.13.load.pre, %then.i ]
  %self.0.load = load ptr, ptr %self, align 8
  %gep = getelementptr i8, ptr %self.0.load, i64 %self.13.load
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %gep, ptr align 1 %add_buf, i64 %add_len, i1 false)
  %self.14.load = load i64, ptr %self.1, align 4
  %i_add5 = add i64 %self.14.load, %add_len
  store i64 %i_add5, ptr %self.1, align 4
  ret void
}

; Function Attrs: mustprogress nounwind willreturn
define void @"#dynarray::resize"(ptr nocapture %self, i64 %new_cap) local_unnamed_addr #4 {
body:
  %self.2 = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %self.2.load = load i64, ptr %self.2, align 4
  %i_lt = icmp slt i64 %self.2.load, %new_cap
  br i1 %i_lt, label %then, label %merge

then:                                             ; preds = %body
  %i_mul = shl i64 %self.2.load, 1
  %0 = tail call i64 @llvm.smax.i64(i64 %new_cap, i64 %i_mul)
  %self.0.load = load ptr, ptr %self, align 8
  %1 = tail call ptr @malloc(i64 %0)
  %self.1 = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %self.1.load = load i64, ptr %self.1, align 4
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %1, ptr align 1 %self.0.load, i64 %self.1.load, i1 false)
  store ptr %1, ptr %self, align 8
  store i64 %0, ptr %self.2, align 4
  tail call void @free(ptr %self.0.load)
  br label %merge

merge:                                            ; preds = %body, %then
  ret void
}

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::max"(i64 %self, i64 %o) local_unnamed_addr #5 {
body:
  %0 = tail call i64 @llvm.smax.i64(i64 %self, i64 %o)
  ret i64 %0
}

; Function Attrs: inaccessiblemem_or_argmemonly mustprogress nounwind willreturn allockind("free")
declare void @free(ptr allocptr nocapture noundef) local_unnamed_addr #6

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.smax.i64(i64, i64) #7

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

merge:                                            ; preds = %body, %block, %"string::slice_bytes.exit"
  %if_result14 = phi %"option<char>" [ zeroinitializer, %block ], [ %6, %"string::slice_bytes.exit" ], [ zeroinitializer, %body ]
  ret %"option<char>" %if_result14

block:                                            ; preds = %else
  %2 = tail call ptr @malloc(i64 0)
  store ptr %2, ptr %self, align 8
  call void @llvm.memset.p0.i64(ptr noundef nonnull align 8 dereferenceable(16) %self.0.1.i, i8 0, i64 16, i1 false)
  br label %merge

block8:                                           ; preds = %else
  %self.0.1.load.i13 = load i64, ptr %self.0.1.i, align 4
  %i_le4.i.not = icmp slt i64 %self.0.1.load.i13, %1
  br i1 %i_le4.i.not, label %else.i, label %"string::slice_bytes.exit"

else.i:                                           ; preds = %block8
  %stderr.i = tail call ptr @fdopen(i64 2, ptr nonnull @_write)
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

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define %"option<char>" @"option<char>::none"() local_unnamed_addr #8 {
body:
  ret %"option<char>" zeroinitializer
}

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::min"(i64 %self, i64 %o) local_unnamed_addr #5 {
body:
  %0 = tail call i64 @llvm.smin.i64(i64 %self, i64 %o)
  ret i64 %0
}

declare i64 @mbtowc(ptr, ptr, i64) local_unnamed_addr

; Function Attrs: mustprogress nofree nounwind willreturn
define %string @"string::new"() local_unnamed_addr #1 {
body:
  %0 = tail call ptr @malloc(i64 0)
  %1 = insertvalue %"#dynarray" zeroinitializer, ptr %0, 0
  %2 = insertvalue %"#dynarray" %1, i64 0, 1
  %3 = insertvalue %"#dynarray" %2, i64 0, 2
  %4 = insertvalue %string zeroinitializer, %"#dynarray" %3, 0
  ret %string %4
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %"option<char>" @"option<char>::some"(i32 %t) local_unnamed_addr #1 {
body:
  %0 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  store i32 %t, ptr %0, align 4
  %1 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %0, 1
  ret %"option<char>" %1
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.smin.i64(i64, i64) #7

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define noalias ptr @"#ptr::null"() local_unnamed_addr #8 {
body:
  ret ptr null
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
  %2 = call ptr @malloc(i64 5)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %2, ptr align 1 %1, i64 5, i1 false)
  %3 = insertvalue %"#dynarray" undef, ptr %2, 0
  %4 = insertvalue %"#dynarray" %3, i64 5, 1
  %5 = insertvalue %"#dynarray" %4, i64 5, 2
  %6 = insertvalue %string zeroinitializer, %"#dynarray" %5, 0
  %7 = alloca %string, align 8
  %8 = extractvalue %string %6, 0
  %.elt = extractvalue %"#dynarray" %8, 0
  store ptr %.elt, ptr %7, align 8
  %.repack8 = getelementptr inbounds %"#dynarray", ptr %7, i64 0, i32 1
  %.elt9 = extractvalue %"#dynarray" %8, 1
  store i64 %.elt9, ptr %.repack8, align 8
  %.repack10 = getelementptr inbounds %"#dynarray", ptr %7, i64 0, i32 2
  %.elt11 = extractvalue %"#dynarray" %8, 2
  store i64 %.elt11, ptr %.repack10, align 8
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %0)
  %9 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %0, ptr nonnull @_tmpl_char_to_string, i32 %deref)
  %deref.i = load ptr, ptr %0, align 8
  %i_add.i = add i64 %9, 1
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %0)
  %10 = alloca %string, align 8
  store ptr %deref.i, ptr %10, align 8
  %.repack13 = getelementptr inbounds %"#dynarray", ptr %10, i64 0, i32 1
  store i64 %9, ptr %.repack13, align 8
  %.repack15 = getelementptr inbounds %"#dynarray", ptr %10, i64 0, i32 2
  store i64 %i_add.i, ptr %.repack15, align 8
  %self.0.1.i.i = getelementptr inbounds %string, ptr %7, i64 0, i32 0, i32 1
  %self.0.1.load.i.i = load i64, ptr %self.0.1.i.i, align 4
  %self.0.1.i10.i = getelementptr inbounds %string, ptr %10, i64 0, i32 0, i32 1
  %self.0.1.load.i11.i = load i64, ptr %self.0.1.i10.i, align 4
  %i_add.i27 = add i64 %self.0.1.load.i11.i, %self.0.1.load.i.i
  %11 = call ptr @malloc(i64 %i_add.i27)
  %self.0.0.load.i = load ptr, ptr %7, align 8
  %i_lt.i.i21.i = icmp slt i64 %i_add.i27, %self.0.1.load.i.i
  br i1 %i_lt.i.i21.i, label %"#dynarray::extend.exit32.i", label %"#dynarray::extend.exit32.thread.i"

"#dynarray::extend.exit32.thread.i":              ; preds = %then
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %11, ptr align 1 %self.0.0.load.i, i64 %self.0.1.load.i.i, i1 false)
  %other.0.0.load41.i = load ptr, ptr %10, align 8
  br label %"string::add_string.exit"

"#dynarray::extend.exit32.i":                     ; preds = %then
  %i_mul.i.i22.i = shl i64 %i_add.i27, 1
  %12 = call i64 @llvm.smax.i64(i64 %self.0.1.load.i.i, i64 %i_mul.i.i22.i)
  %13 = call ptr @malloc(i64 %12)
  call void @free(ptr %11)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %13, ptr align 1 %self.0.0.load.i, i64 %self.0.1.load.i.i, i1 false)
  %other.0.0.load.i = load ptr, ptr %10, align 8
  %i_lt.i.i.i = icmp slt i64 %12, %i_add.i27
  br i1 %i_lt.i.i.i, label %then.i.i.i, label %"string::add_string.exit"

then.i.i.i:                                       ; preds = %"#dynarray::extend.exit32.i"
  %i_mul.i.i.i = shl i64 %12, 1
  %14 = call i64 @llvm.smax.i64(i64 %i_add.i27, i64 %i_mul.i.i.i)
  %15 = call ptr @malloc(i64 %14)
  call void @llvm.memmove.p0.p0.i64(ptr align 1 %15, ptr align 1 %self.0.0.load.i, i64 %self.0.1.load.i.i, i1 false)
  call void @free(ptr %13)
  br label %"string::add_string.exit"

"string::add_string.exit":                        ; preds = %"#dynarray::extend.exit32.thread.i", %"#dynarray::extend.exit32.i", %then.i.i.i
  %other.0.0.load43.i = phi ptr [ %other.0.0.load.i, %then.i.i.i ], [ %other.0.0.load.i, %"#dynarray::extend.exit32.i" ], [ %other.0.0.load41.i, %"#dynarray::extend.exit32.thread.i" ]
  %result_inner.sroa.15.1.i = phi i64 [ %14, %then.i.i.i ], [ %12, %"#dynarray::extend.exit32.i" ], [ %i_add.i27, %"#dynarray::extend.exit32.thread.i" ]
  %result_inner.sroa.0.1.i = phi ptr [ %15, %then.i.i.i ], [ %13, %"#dynarray::extend.exit32.i" ], [ %11, %"#dynarray::extend.exit32.thread.i" ]
  %gep.i.i = getelementptr i8, ptr %result_inner.sroa.0.1.i, i64 %self.0.1.load.i.i
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %gep.i.i, ptr align 1 %other.0.0.load43.i, i64 %self.0.1.load.i11.i, i1 false)
  %16 = insertvalue %"#dynarray" undef, ptr %result_inner.sroa.0.1.i, 0
  %17 = insertvalue %"#dynarray" %16, i64 %i_add.i27, 1
  %18 = insertvalue %"#dynarray" %17, i64 %result_inner.sroa.15.1.i, 2
  %19 = insertvalue %string zeroinitializer, %"#dynarray" %18, 0
  %20 = alloca %string, align 8
  %21 = extractvalue %string %19, 0
  %.elt17 = extractvalue %"#dynarray" %21, 0
  store ptr %.elt17, ptr %20, align 8
  %.repack18 = getelementptr inbounds %"#dynarray", ptr %20, i64 0, i32 1
  %.elt19 = extractvalue %"#dynarray" %21, 1
  store i64 %.elt19, ptr %.repack18, align 8
  %.repack20 = getelementptr inbounds %"#dynarray", ptr %20, i64 0, i32 2
  %.elt21 = extractvalue %"#dynarray" %21, 2
  store i64 %.elt21, ptr %.repack20, align 8
  %22 = alloca [1 x i8], align 1
  store i8 41, ptr %22, align 1
  %23 = call ptr @malloc(i64 1)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %23, ptr align 1 %22, i64 1, i1 false)
  %24 = insertvalue %"#dynarray" undef, ptr %23, 0
  %25 = insertvalue %"#dynarray" %24, i64 1, 1
  %26 = insertvalue %"#dynarray" %25, i64 1, 2
  %27 = insertvalue %string zeroinitializer, %"#dynarray" %26, 0
  %28 = alloca %string, align 8
  %29 = extractvalue %string %27, 0
  %.elt22 = extractvalue %"#dynarray" %29, 0
  store ptr %.elt22, ptr %28, align 8
  %.repack23 = getelementptr inbounds %"#dynarray", ptr %28, i64 0, i32 1
  %.elt24 = extractvalue %"#dynarray" %29, 1
  store i64 %.elt24, ptr %.repack23, align 8
  %.repack25 = getelementptr inbounds %"#dynarray", ptr %28, i64 0, i32 2
  %.elt26 = extractvalue %"#dynarray" %29, 2
  store i64 %.elt26, ptr %.repack25, align 8
  %self.0.1.i.i28 = getelementptr inbounds %string, ptr %20, i64 0, i32 0, i32 1
  %self.0.1.load.i.i29 = load i64, ptr %self.0.1.i.i28, align 4
  %self.0.1.i10.i30 = getelementptr inbounds %string, ptr %28, i64 0, i32 0, i32 1
  %self.0.1.load.i11.i31 = load i64, ptr %self.0.1.i10.i30, align 4
  %i_add.i32 = add i64 %self.0.1.load.i11.i31, %self.0.1.load.i.i29
  %30 = call ptr @malloc(i64 %i_add.i32)
  %self.0.0.load.i33 = load ptr, ptr %20, align 8
  %i_lt.i.i21.i34 = icmp slt i64 %i_add.i32, %self.0.1.load.i.i29
  br i1 %i_lt.i.i21.i34, label %"#dynarray::extend.exit32.i40", label %"#dynarray::extend.exit32.thread.i36"

"#dynarray::extend.exit32.thread.i36":            ; preds = %"string::add_string.exit"
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %30, ptr align 1 %self.0.0.load.i33, i64 %self.0.1.load.i.i29, i1 false)
  %other.0.0.load41.i35 = load ptr, ptr %28, align 8
  br label %"string::add_string.exit47"

"#dynarray::extend.exit32.i40":                   ; preds = %"string::add_string.exit"
  %i_mul.i.i22.i37 = shl i64 %i_add.i32, 1
  %31 = call i64 @llvm.smax.i64(i64 %self.0.1.load.i.i29, i64 %i_mul.i.i22.i37)
  %32 = call ptr @malloc(i64 %31)
  call void @free(ptr %30)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %32, ptr align 1 %self.0.0.load.i33, i64 %self.0.1.load.i.i29, i1 false)
  %other.0.0.load.i38 = load ptr, ptr %28, align 8
  %i_lt.i.i.i39 = icmp slt i64 %31, %i_add.i32
  br i1 %i_lt.i.i.i39, label %then.i.i.i42, label %"string::add_string.exit47"

then.i.i.i42:                                     ; preds = %"#dynarray::extend.exit32.i40"
  %i_mul.i.i.i41 = shl i64 %31, 1
  %33 = call i64 @llvm.smax.i64(i64 %i_add.i32, i64 %i_mul.i.i.i41)
  %34 = call ptr @malloc(i64 %33)
  call void @llvm.memmove.p0.p0.i64(ptr align 1 %34, ptr align 1 %self.0.0.load.i33, i64 %self.0.1.load.i.i29, i1 false)
  call void @free(ptr %32)
  br label %"string::add_string.exit47"

"string::add_string.exit47":                      ; preds = %"#dynarray::extend.exit32.thread.i36", %"#dynarray::extend.exit32.i40", %then.i.i.i42
  %other.0.0.load43.i43 = phi ptr [ %other.0.0.load.i38, %then.i.i.i42 ], [ %other.0.0.load.i38, %"#dynarray::extend.exit32.i40" ], [ %other.0.0.load41.i35, %"#dynarray::extend.exit32.thread.i36" ]
  %result_inner.sroa.15.1.i44 = phi i64 [ %33, %then.i.i.i42 ], [ %31, %"#dynarray::extend.exit32.i40" ], [ %i_add.i32, %"#dynarray::extend.exit32.thread.i36" ]
  %result_inner.sroa.0.1.i45 = phi ptr [ %34, %then.i.i.i42 ], [ %32, %"#dynarray::extend.exit32.i40" ], [ %30, %"#dynarray::extend.exit32.thread.i36" ]
  %gep.i.i46 = getelementptr i8, ptr %result_inner.sroa.0.1.i45, i64 %self.0.1.load.i.i29
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %gep.i.i46, ptr align 1 %other.0.0.load43.i43, i64 %self.0.1.load.i11.i31, i1 false)
  %35 = insertvalue %"#dynarray" undef, ptr %result_inner.sroa.0.1.i45, 0
  %36 = insertvalue %"#dynarray" %35, i64 %i_add.i32, 1
  %37 = insertvalue %"#dynarray" %36, i64 %result_inner.sroa.15.1.i44, 2
  %38 = insertvalue %string zeroinitializer, %"#dynarray" %37, 0
  br label %merge

else:                                             ; preds = %body
  %39 = alloca [4 x i8], align 1
  store i8 110, ptr %39, align 1
  %.repack1 = getelementptr inbounds [4 x i8], ptr %39, i64 0, i64 1
  store i8 111, ptr %.repack1, align 1
  %.repack2 = getelementptr inbounds [4 x i8], ptr %39, i64 0, i64 2
  store i8 110, ptr %.repack2, align 1
  %.repack3 = getelementptr inbounds [4 x i8], ptr %39, i64 0, i64 3
  store i8 101, ptr %.repack3, align 1
  %40 = call ptr @malloc(i64 4)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %40, ptr align 1 %39, i64 4, i1 false)
  %41 = insertvalue %"#dynarray" undef, ptr %40, 0
  %42 = insertvalue %"#dynarray" %41, i64 4, 1
  %43 = insertvalue %"#dynarray" %42, i64 4, 2
  %44 = insertvalue %string zeroinitializer, %"#dynarray" %43, 0
  br label %merge

merge:                                            ; preds = %else, %"string::add_string.exit47"
  %if_result = phi %string [ %38, %"string::add_string.exit47" ], [ %44, %else ]
  ret %string %if_result
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %string @"string::from_raw"(ptr nocapture readonly %contents, i64 %len) local_unnamed_addr #1 {
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
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write)
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
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.2, i64 16, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

merge:                                            ; preds = %body
  %self.1 = getelementptr inbounds %"option<char>", ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  %deref = load i32, ptr %self.1.load, align 4
  ret i32 %deref
}

define %string @"#byte::to_string"(i8 %self) local_unnamed_addr {
body:
  %0 = alloca ptr, align 8
  %1 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %0, ptr nonnull @_tmpl_byte_to_string, i8 %self)
  %deref = load ptr, ptr %0, align 8
  %i_add = add i64 %1, 1
  %2 = insertvalue %"#dynarray" zeroinitializer, ptr %deref, 0
  %3 = insertvalue %"#dynarray" %2, i64 %1, 1
  %4 = insertvalue %"#dynarray" %3, i64 %i_add, 2
  %5 = insertvalue %string zeroinitializer, %"#dynarray" %4, 0
  ret %string %5
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %string @"bool::to_string"(i1 %self) local_unnamed_addr #1 {
body:
  br i1 %self, label %then, label %else

then:                                             ; preds = %body
  %0 = alloca [4 x i8], align 1
  store i8 116, ptr %0, align 1
  %.repack5 = getelementptr inbounds [4 x i8], ptr %0, i64 0, i64 1
  store i8 114, ptr %.repack5, align 1
  %.repack6 = getelementptr inbounds [4 x i8], ptr %0, i64 0, i64 2
  store i8 117, ptr %.repack6, align 1
  %.repack7 = getelementptr inbounds [4 x i8], ptr %0, i64 0, i64 3
  store i8 101, ptr %.repack7, align 1
  %1 = call ptr @malloc(i64 4)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %1, ptr align 1 %0, i64 4, i1 false)
  %2 = insertvalue %"#dynarray" undef, ptr %1, 0
  %3 = insertvalue %"#dynarray" %2, i64 4, 1
  %4 = insertvalue %"#dynarray" %3, i64 4, 2
  %5 = insertvalue %string zeroinitializer, %"#dynarray" %4, 0
  br label %merge

else:                                             ; preds = %body
  %6 = alloca [5 x i8], align 1
  store i8 102, ptr %6, align 1
  %.repack1 = getelementptr inbounds [5 x i8], ptr %6, i64 0, i64 1
  store i8 97, ptr %.repack1, align 1
  %.repack2 = getelementptr inbounds [5 x i8], ptr %6, i64 0, i64 2
  store i8 108, ptr %.repack2, align 1
  %.repack3 = getelementptr inbounds [5 x i8], ptr %6, i64 0, i64 3
  store i8 115, ptr %.repack3, align 1
  %.repack4 = getelementptr inbounds [5 x i8], ptr %6, i64 0, i64 4
  store i8 101, ptr %.repack4, align 1
  %7 = call ptr @malloc(i64 5)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %7, ptr align 1 %6, i64 5, i1 false)
  %8 = insertvalue %"#dynarray" undef, ptr %7, 0
  %9 = insertvalue %"#dynarray" %8, i64 5, 1
  %10 = insertvalue %"#dynarray" %9, i64 5, 2
  %11 = insertvalue %string zeroinitializer, %"#dynarray" %10, 0
  br label %merge

merge:                                            ; preds = %else, %then
  %if_result = phi %string [ %5, %then ], [ %11, %else ]
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
define double @"float::sign"(double %self) local_unnamed_addr #5 {
body:
  %0 = tail call double @llvm.copysign.f64(double 1.000000e+00, double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.copysign.f64(double, double) #7

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
define double @"float::round"(double %self) local_unnamed_addr #5 {
body:
  %0 = tail call double @llvm.round.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::trunc"(double %self) local_unnamed_addr #5 {
body:
  %0 = tail call double @llvm.trunc.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.trunc.f64(double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::floor"(double %self) local_unnamed_addr #5 {
body:
  %0 = tail call double @llvm.floor.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.floor.f64(double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::ceil"(double %self) local_unnamed_addr #5 {
body:
  %0 = tail call double @llvm.ceil.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.ceil.f64(double) #7

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
define double @"float::atanh"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @atanh(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @atanh(double) local_unnamed_addr #9

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::acosh"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @acosh(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @acosh(double) local_unnamed_addr #9

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::asinh"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @asinh(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @asinh(double) local_unnamed_addr #9

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::tanh"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @tanh(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @tanh(double) local_unnamed_addr #9

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::cosh"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @cosh(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @cosh(double) local_unnamed_addr #9

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::sinh"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @sinh(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @sinh(double) local_unnamed_addr #9

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::atan2"(double %self, double %x) local_unnamed_addr #9 {
body:
  %0 = tail call double @atan2(double %self, double %x)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @atan2(double, double) local_unnamed_addr #9

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::atan"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @atan(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @atan(double) local_unnamed_addr #9

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::acos"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @acos(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @acos(double) local_unnamed_addr #9

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::asin"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @asin(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @asin(double) local_unnamed_addr #9

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::tan"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @tan(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @tan(double) local_unnamed_addr #9

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::cos"(double %self) local_unnamed_addr #5 {
body:
  %0 = tail call double @llvm.cos.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.cos.f64(double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::sin"(double %self) local_unnamed_addr #5 {
body:
  %0 = tail call double @llvm.sin.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.sin.f64(double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::powi"(double %self, i64 %exp) local_unnamed_addr #5 {
body:
  %cast = sitofp i64 %exp to double
  %0 = tail call double @llvm.pow.f64(double %self, double %cast)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.pow.f64(double, double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::pow"(double %self, double %exp) local_unnamed_addr #5 {
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
define double @"float::cbrt"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @cbrt(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @cbrt(double) local_unnamed_addr #9

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::sqrt"(double %self) local_unnamed_addr #5 {
body:
  %0 = tail call double @llvm.sqrt.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.sqrt.f64(double) #7

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::log1p"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @log1p(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @log1p(double) local_unnamed_addr #9

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::log10"(double %self) local_unnamed_addr #5 {
body:
  %0 = tail call double @llvm.log10.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.log10.f64(double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::log2"(double %self) local_unnamed_addr #5 {
body:
  %0 = tail call double @llvm.log2.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.log2.f64(double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::log"(double %self) local_unnamed_addr #5 {
body:
  %0 = tail call double @llvm.log.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.log.f64(double) #7

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::expm1"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @expm1(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @expm1(double) local_unnamed_addr #9

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::exp2"(double %self) local_unnamed_addr #5 {
body:
  %0 = tail call double @llvm.exp2.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.exp2.f64(double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::exp"(double %self) local_unnamed_addr #5 {
body:
  %0 = tail call double @llvm.exp.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.exp.f64(double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::min"(double %self, double %o) local_unnamed_addr #5 {
body:
  %0 = tail call double @llvm.minnum.f64(double %self, double %o)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.minnum.f64(double, double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::max"(double %self, double %o) local_unnamed_addr #5 {
body:
  %0 = tail call double @llvm.maxnum.f64(double %self, double %o)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.maxnum.f64(double, double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::fma"(double %self, double %multiplicand, double %addend) local_unnamed_addr #5 {
body:
  %0 = tail call double @llvm.fma.f64(double %self, double %multiplicand, double %addend)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.fma.f64(double, double, double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::abs"(double %self) local_unnamed_addr #5 {
body:
  %0 = tail call double @llvm.fabs.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.fabs.f64(double) #7

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
define i64 @"int::trailing_zeroes"(i64 %self) local_unnamed_addr #5 {
body:
  %0 = tail call i64 @llvm.cttz.i64(i64 %self, i1 false), !range !0
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.cttz.i64(i64, i1 immarg) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::leading_zeroes"(i64 %self) local_unnamed_addr #5 {
body:
  %0 = tail call i64 @llvm.ctlz.i64(i64 %self, i1 false), !range !0
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.ctlz.i64(i64, i1 immarg) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::reverse_bytes"(i64 %self) local_unnamed_addr #5 {
body:
  %0 = tail call i64 @llvm.bswap.i64(i64 %self)
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.bswap.i64(i64) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::reverse_bits"(i64 %self) local_unnamed_addr #5 {
body:
  %0 = tail call i64 @llvm.bitreverse.i64(i64 %self)
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.bitreverse.i64(i64) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::count_ones"(i64 %self) local_unnamed_addr #5 {
body:
  %0 = tail call i64 @llvm.ctpop.i64(i64 %self), !range !0
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.ctpop.i64(i64) #7

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define i64 @"int::sign"(i64 %self) local_unnamed_addr #8 {
body:
  %i_lt.not = icmp ne i64 %self, 0
  %spec.select = sext i1 %i_lt.not to i64
  %i_gt.inv = icmp slt i64 %self, 1
  %if_result = select i1 %i_gt.inv, i64 %spec.select, i64 1
  ret i64 %if_result
}

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::abs"(i64 %self) local_unnamed_addr #5 {
body:
  %0 = tail call i64 @llvm.abs.i64(i64 %self, i1 false)
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.abs.i64(i64, i1 immarg) #7

define i64 @"int::idiv"(i64 %self, i64 %d) local_unnamed_addr {
body:
  %i_eq = icmp eq i64 %d, 0
  br i1 %i_eq, label %then, label %else

then:                                             ; preds = %body
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write)
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

define i8 @main() local_unnamed_addr {
body:
  %0 = tail call ptr @setlocale(i64 0, ptr nonnull @locale)
  ret i8 0
}

declare ptr @setlocale(i64, ptr) local_unnamed_addr

; Function Attrs: nofree nounwind
declare noundef i64 @fwrite(ptr nocapture noundef, i64 noundef, i64 noundef, ptr nocapture noundef) local_unnamed_addr #0

; Function Attrs: argmemonly mustprogress nocallback nofree nounwind willreturn
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #10

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.round.f64(double) #7

; Function Attrs: argmemonly mustprogress nocallback nofree nosync nounwind willreturn
declare void @llvm.lifetime.start.p0(i64 immarg, ptr nocapture) #11

; Function Attrs: argmemonly mustprogress nocallback nofree nosync nounwind willreturn
declare void @llvm.lifetime.end.p0(i64 immarg, ptr nocapture) #11

; Function Attrs: argmemonly nocallback nofree nounwind willreturn
declare void @llvm.memmove.p0.p0.i64(ptr nocapture writeonly, ptr nocapture readonly, i64, i1 immarg) #12

; Function Attrs: argmemonly nocallback nofree nounwind willreturn writeonly
declare void @llvm.memset.p0.i64(ptr nocapture writeonly, i8, i64, i1 immarg) #13

attributes #0 = { nofree nounwind }
attributes #1 = { mustprogress nofree nounwind willreturn }
attributes #2 = { inaccessiblememonly mustprogress nofree nounwind willreturn allockind("alloc,uninitialized") allocsize(0) "alloc-family"="malloc" }
attributes #3 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn }
attributes #4 = { mustprogress nounwind willreturn }
attributes #5 = { mustprogress nofree nosync nounwind readnone willreturn }
attributes #6 = { inaccessiblemem_or_argmemonly mustprogress nounwind willreturn allockind("free") "alloc-family"="malloc" }
attributes #7 = { mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #8 = { mustprogress nofree norecurse nosync nounwind readnone willreturn }
attributes #9 = { mustprogress nofree nounwind willreturn writeonly }
attributes #10 = { argmemonly mustprogress nocallback nofree nounwind willreturn }
attributes #11 = { argmemonly mustprogress nocallback nofree nosync nounwind willreturn }
attributes #12 = { argmemonly nocallback nofree nounwind willreturn }
attributes #13 = { argmemonly nocallback nofree nounwind willreturn writeonly }

!0 = !{i64 0, i64 65}
