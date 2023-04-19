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
@_tmpl_byte_to_string = private unnamed_addr constant [6 x i8] c"%#hhx\00", align 1
@_tmpl_ptr_to_string = private unnamed_addr constant [9 x i8] c"ptr %#lx\00", align 1
@throw_msg.2 = private unnamed_addr constant [31 x i8] c"cannot take element from array\00", align 1
@throw_msg.3 = private unnamed_addr constant [14 x i8] c"invalid slice\00", align 1
@throw_msg.4 = private unnamed_addr constant [23 x i8] c"division by zero error\00", align 1
@_write.1 = private unnamed_addr constant [2 x i8] c"w\00", align 1
@throw_msg.4.9 = private unnamed_addr constant [26 x i8] c"array index out of bounds\00", align 1
@throw_msg.5.10 = private unnamed_addr constant [17 x i8] c"no value present\00", align 1
@locale.11 = private unnamed_addr constant [12 x i8] c"en_US.UTF-8\00", align 1

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
  %alloca.ptr = alloca ptr, align 8
  %0 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %alloca.ptr, ptr nonnull @_tmpl_ptr_to_string, ptr %self)
  %buf_ptr4.deref = load ptr, ptr %alloca.ptr, align 8
  %i_add = add i64 %0, 1
  %1 = insertvalue %"#dynarray" zeroinitializer, ptr %buf_ptr4.deref, 0
  %2 = insertvalue %"#dynarray" %1, i64 %0, 1
  %3 = insertvalue %"#dynarray" %2, i64 %i_add, 2
  %4 = insertvalue %string zeroinitializer, %"#dynarray" %3, 0
  ret %string %4
}

declare i64 @asprintf(ptr, ptr, ...) local_unnamed_addr

define i32 @"string::get"(ptr nocapture readonly %self, i64 %i) local_unnamed_addr {
body:
  %i_le = icmp sgt i64 %i, -1
  br i1 %i_le, label %and_true, label %else

then:                                             ; preds = %and_true
  %chars = alloca %string_chars, align 8
  %str1.unpack.unpack.i.i = load ptr, ptr %self, align 8
  %str1.unpack.elt4.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %str1.unpack.unpack5.i.i = load i64, ptr %str1.unpack.elt4.i.i, align 8
  %0 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  store ptr %str1.unpack.unpack.i.i, ptr %chars, align 8
  %chars.repack4 = getelementptr inbounds %"#dynarray", ptr %chars, i64 0, i32 1
  store i64 %self.0.1.load.i, ptr %chars.repack4, align 8
  %chars.repack6 = getelementptr inbounds %"#dynarray", ptr %chars, i64 0, i32 2
  store i64 %str1.unpack.unpack5.i.i, ptr %chars.repack6, align 8
  %chars.repack1 = getelementptr inbounds %string_chars, ptr %chars, i64 0, i32 1
  store ptr %0, ptr %chars.repack1, align 8
  %i_lt99.not = icmp eq i64 %i, 0
  br i1 %i_lt99.not, label %post_while, label %while

else:                                             ; preds = %and_true, %body
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %1 = tail call i64 @fwrite(ptr nonnull @throw_msg.4.9, i64 25, i64 1, ptr %stderr)
  %2 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

and_true:                                         ; preds = %body
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %i_lt = icmp sgt i64 %self.0.1.load.i, %i
  br i1 %i_lt, label %then, label %else

while:                                            ; preds = %then, %while
  %i_add810 = phi i64 [ %i_add, %while ], [ 0, %then ]
  %3 = call %"option<char>" @"string_chars::next"(ptr nonnull %chars)
  %i_add = add nuw nsw i64 %i_add810, 1
  %i_lt9 = icmp slt i64 %i_add, %i
  br i1 %i_lt9, label %while, label %post_while

post_while:                                       ; preds = %while, %then
  %4 = call %"option<char>" @"string_chars::next"(ptr nonnull %chars)
  %5 = alloca %"option<char>", align 8
  store %"option<char>" %4, ptr %5, align 8
  %self.0.load.i = load i1, ptr %5, align 8
  br i1 %self.0.load.i, label %"option<char>::get.exit", label %else.i

else.i:                                           ; preds = %post_while
  %stderr.i = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %6 = tail call i64 @fwrite(ptr nonnull @throw_msg.5.10, i64 16, i64 1, ptr %stderr.i)
  %7 = tail call i64 @fputwc(i32 10, ptr %stderr.i)
  tail call void @exit(i64 1)
  unreachable

"option<char>::get.exit":                         ; preds = %post_while
  %self.1.i = getelementptr inbounds %"option<char>", ptr %5, i64 0, i32 1
  %self.1.load.i = load ptr, ptr %self.1.i, align 8
  %self.1.load.deref.i = load i32, ptr %self.1.load.i, align 4
  ret i32 %self.1.load.deref.i
}

; Function Attrs: inaccessiblememonly mustprogress nofree nounwind willreturn allockind("alloc,uninitialized") allocsize(0)
declare noalias noundef ptr @malloc(i64 noundef) local_unnamed_addr #1

; Function Attrs: nofree nounwind
declare noalias noundef ptr @fdopen(i64 noundef, ptr nocapture noundef readonly) local_unnamed_addr #0

; Function Attrs: nofree nounwind
declare noundef i64 @fwrite(ptr nocapture noundef, i64 noundef, i64 noundef, ptr nocapture noundef) local_unnamed_addr #0

declare i64 @fputwc(i32, ptr) local_unnamed_addr

declare void @exit(i64) local_unnamed_addr

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
  br i1 %i_lt, label %block, label %block9

merge:                                            ; preds = %"string::slice_bytes.exit", %block, %body
  %if_result17 = phi %"option<char>" [ zeroinitializer, %block ], [ %6, %"string::slice_bytes.exit" ], [ zeroinitializer, %body ]
  ret %"option<char>" %if_result17

block:                                            ; preds = %else
  %2 = tail call ptr @malloc(i64 0)
  store ptr %2, ptr %self, align 8
  tail call void @llvm.memset.p0.i64(ptr noundef nonnull align 8 dereferenceable(16) %self.0.1.i, i8 0, i64 16, i1 false)
  br label %merge

block9:                                           ; preds = %else
  %self.0.1.load.i13 = load i64, ptr %self.0.1.i, align 4
  %i_le7.i.not = icmp slt i64 %self.0.1.load.i13, %1
  br i1 %i_le7.i.not, label %else.i, label %"string::slice_bytes.exit"

else.i:                                           ; preds = %block9
  %stderr.i = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %3 = tail call i64 @fwrite(ptr nonnull @throw_msg.3, i64 13, i64 1, ptr %stderr.i)
  %4 = tail call i64 @fputwc(i32 10, ptr %stderr.i)
  tail call void @exit(i64 1)
  unreachable

"string::slice_bytes.exit":                       ; preds = %block9
  %self.0.0.load.i = load ptr, ptr %self, align 8
  %self.0.0.load.gep.i = getelementptr i8, ptr %self.0.0.load.i, i64 %1
  %i_sub.i = sub i64 %self.0.1.load.i13, %1
  %self.0.2.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 2
  %self.0.2.load.i = load i64, ptr %self.0.2.i, align 4
  %i_sub20.i = sub i64 %self.0.2.load.i, %1
  store ptr %self.0.0.load.gep.i, ptr %self, align 8
  store i64 %i_sub.i, ptr %self.0.1.i, align 8
  store i64 %i_sub20.i, ptr %self.0.2.i, align 8
  %self.115.load = load ptr, ptr %self.1, align 8
  %self.115.load.deref = load i32, ptr %self.115.load, align 4
  %5 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  store i32 %self.115.load.deref, ptr %5, align 4
  %6 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %5, 1
  br label %merge
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.smin.i64(i64, i64) #2

declare i64 @mbtowc(ptr, ptr, i64) local_unnamed_addr

; Function Attrs: mustprogress nofree nounwind willreturn
define %string_chars @"string::chars"(ptr nocapture readonly %self) local_unnamed_addr #3 {
body:
  %str1.unpack.unpack.i = load ptr, ptr %self, align 8
  %0 = insertvalue %"#dynarray" undef, ptr %str1.unpack.unpack.i, 0
  %str1.unpack.elt2.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %str1.unpack.unpack3.i = load i64, ptr %str1.unpack.elt2.i, align 8
  %1 = insertvalue %"#dynarray" %0, i64 %str1.unpack.unpack3.i, 1
  %str1.unpack.elt4.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %str1.unpack.unpack5.i = load i64, ptr %str1.unpack.elt4.i, align 8
  %str1.unpack6.i = insertvalue %"#dynarray" %1, i64 %str1.unpack.unpack5.i, 2
  %str11.i = insertvalue %string undef, %"#dynarray" %str1.unpack6.i, 0
  %2 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  %3 = insertvalue %string_chars zeroinitializer, %string %str11.i, 0
  %4 = insertvalue %string_chars %3, ptr %2, 1
  ret %string_chars %4
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define i64 @"string::len"(ptr nocapture readonly %self) local_unnamed_addr #4 {
body:
  %self.0.1 = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load = load i64, ptr %self.0.1, align 4
  ret i64 %self.0.1.load
}

define i32 @"option<char>::get"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %self.0.load = load i1, ptr %self, align 1
  br i1 %self.0.load, label %merge, label %else

else:                                             ; preds = %body
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.5.10, i64 16, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

merge:                                            ; preds = %body
  %self.1 = getelementptr inbounds %"option<char>", ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  %self.1.load.deref = load i32, ptr %self.1.load, align 4
  ret i32 %self.1.load.deref
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define %"option<char>" @"option<char>::none"() local_unnamed_addr #5 {
body:
  ret %"option<char>" zeroinitializer
}

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::min"(i64 %self, i64 %o) local_unnamed_addr #6 {
body:
  %0 = tail call i64 @llvm.smin.i64(i64 %self, i64 %o)
  ret i64 %0
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %string @"string::new"() local_unnamed_addr #3 {
body:
  %0 = tail call ptr @malloc(i64 0)
  %1 = insertvalue %"#dynarray" zeroinitializer, ptr %0, 0
  %2 = insertvalue %"#dynarray" %1, i64 0, 1
  %3 = insertvalue %"#dynarray" %2, i64 0, 2
  %4 = insertvalue %string zeroinitializer, %"#dynarray" %3, 0
  ret %string %4
}

define %string @"string::slice_bytes"(ptr nocapture readonly %self, i64 %start, i64 %end) local_unnamed_addr {
body:
  %i_le = icmp sgt i64 %start, -1
  %i_le7 = icmp sge i64 %end, %start
  %and_result = select i1 %i_le, i1 %i_le7, i1 false
  br i1 %and_result, label %and_true9, label %else

else:                                             ; preds = %and_true9, %body
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.3, i64 13, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

merge:                                            ; preds = %and_true9
  %self.0.0.load = load ptr, ptr %self, align 8
  %self.0.0.load.gep = getelementptr i8, ptr %self.0.0.load, i64 %start
  %2 = insertvalue %"#dynarray" zeroinitializer, ptr %self.0.0.load.gep, 0
  %i_sub = sub i64 %end, %start
  %3 = insertvalue %"#dynarray" %2, i64 %i_sub, 1
  %self.0.2 = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 2
  %self.0.2.load = load i64, ptr %self.0.2, align 4
  %i_sub20 = sub i64 %self.0.2.load, %start
  %4 = insertvalue %"#dynarray" %3, i64 %i_sub20, 2
  %5 = insertvalue %string zeroinitializer, %"#dynarray" %4, 0
  ret %string %5

and_true9:                                        ; preds = %body
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %i_le13.not = icmp slt i64 %self.0.1.load.i, %end
  br i1 %i_le13.not, label %else, label %merge
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %"option<char>" @"option<char>::some"(i32 %t) local_unnamed_addr #3 {
body:
  %0 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  store i32 %t, ptr %0, align 4
  %1 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %0, 1
  ret %"option<char>" %1
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %"#dynarray" @"#dynarray::new"(i64 %cap) local_unnamed_addr #3 {
body:
  %0 = tail call ptr @malloc(i64 %cap)
  %1 = insertvalue %"#dynarray" zeroinitializer, ptr %0, 0
  %2 = insertvalue %"#dynarray" %1, i64 0, 1
  %3 = insertvalue %"#dynarray" %2, i64 %cap, 2
  ret %"#dynarray" %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define noalias ptr @"#ptr::null"() local_unnamed_addr #5 {
body:
  ret ptr null
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %string_chars @"string_chars::new"(ptr nocapture readonly %str) local_unnamed_addr #3 {
body:
  %str1.unpack.unpack = load ptr, ptr %str, align 8
  %0 = insertvalue %"#dynarray" undef, ptr %str1.unpack.unpack, 0
  %str1.unpack.elt2 = getelementptr inbounds %"#dynarray", ptr %str, i64 0, i32 1
  %str1.unpack.unpack3 = load i64, ptr %str1.unpack.elt2, align 8
  %1 = insertvalue %"#dynarray" %0, i64 %str1.unpack.unpack3, 1
  %str1.unpack.elt4 = getelementptr inbounds %"#dynarray", ptr %str, i64 0, i32 2
  %str1.unpack.unpack5 = load i64, ptr %str1.unpack.elt4, align 8
  %str1.unpack6 = insertvalue %"#dynarray" %1, i64 %str1.unpack.unpack5, 2
  %str11 = insertvalue %string undef, %"#dynarray" %str1.unpack6, 0
  %2 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  %3 = insertvalue %string_chars zeroinitializer, %string %str11, 0
  %4 = insertvalue %string_chars %3, ptr %2, 1
  ret %string_chars %4
}

define i1 @"string::contains_char"(ptr nocapture readonly %self, i32 %m) local_unnamed_addr {
body:
  %str1.unpack.unpack.i.i = load ptr, ptr %self, align 8
  %str1.unpack.elt2.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %str1.unpack.unpack3.i.i = load i64, ptr %str1.unpack.elt2.i.i, align 8
  %str1.unpack.elt4.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %str1.unpack.unpack5.i.i = load i64, ptr %str1.unpack.elt4.i.i, align 8
  %0 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  %1 = alloca %string_chars, align 8
  store ptr %str1.unpack.unpack.i.i, ptr %1, align 8
  %.repack4 = getelementptr inbounds %"#dynarray", ptr %1, i64 0, i32 1
  store i64 %str1.unpack.unpack3.i.i, ptr %.repack4, align 8
  %.repack6 = getelementptr inbounds %"#dynarray", ptr %1, i64 0, i32 2
  store i64 %str1.unpack.unpack5.i.i, ptr %.repack6, align 8
  %.repack1 = getelementptr inbounds %string_chars, ptr %1, i64 0, i32 1
  store ptr %0, ptr %.repack1, align 8
  br label %for_cond

for_cond:                                         ; preds = %post_cmp, %body
  %2 = call %"option<char>" @"string_chars::next"(ptr nonnull %1)
  %present = extractvalue %"option<char>" %2, 0
  br i1 %present, label %post_cmp, label %common.ret

common.ret:                                       ; preds = %post_cmp, %for_cond
  %present.le = extractvalue %"option<char>" %2, 0
  ret i1 %present.le

post_cmp:                                         ; preds = %for_cond
  %3 = extractvalue %"option<char>" %2, 1
  %4 = load i32, ptr %3, align 4
  %i_eq = icmp eq i32 %4, %m
  br i1 %i_eq, label %common.ret, label %for_cond
}

define %string @"option<char>::to_string"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %alloca.ptr.i = alloca ptr, align 8
  %self.0.load = load i1, ptr %self, align 1
  br i1 %self.0.load, label %then, label %else

then:                                             ; preds = %body
  %self.1 = getelementptr inbounds %"option<char>", ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  %self.1.load.deref = load i32, ptr %self.1.load, align 4
  %0 = alloca [5 x i8], align 1
  store i8 115, ptr %0, align 1
  %.repack4 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 1
  store i8 111, ptr %.repack4, align 1
  %.repack5 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 2
  store i8 109, ptr %.repack5, align 1
  %.repack6 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 3
  store i8 101, ptr %.repack6, align 1
  %.repack7 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 4
  store i8 40, ptr %.repack7, align 1
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %alloca.ptr.i)
  %1 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %alloca.ptr.i, ptr nonnull @_tmpl_char_to_string, i32 %self.1.load.deref)
  %buf_ptr4.deref.i = load ptr, ptr %alloca.ptr.i, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %alloca.ptr.i)
  %i_add.i1 = add i64 %1, 5
  %2 = call ptr @malloc(i64 %i_add.i1)
  %i_lt.i.i21.i = icmp ugt i64 %1, 9223372036854775802
  br i1 %i_lt.i.i21.i, label %"#dynarray::extend.exit32.i", label %"#dynarray::extend.exit32.thread.i"

"#dynarray::extend.exit32.thread.i":              ; preds = %then
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %2, ptr noundef nonnull align 1 dereferenceable(5) %0, i64 5, i1 false)
  br label %"string::add_string.exit"

"#dynarray::extend.exit32.i":                     ; preds = %then
  %i_mul.i.i22.i = shl i64 %i_add.i1, 1
  %3 = call i64 @llvm.smax.i64(i64 %i_mul.i.i22.i, i64 5)
  %4 = call ptr @malloc(i64 %3)
  call void @free(ptr %2)
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %4, ptr noundef nonnull align 1 dereferenceable(5) %0, i64 5, i1 false)
  %i_lt.i.i.i = icmp slt i64 %3, %i_add.i1
  br i1 %i_lt.i.i.i, label %then.i.i.i, label %"string::add_string.exit"

then.i.i.i:                                       ; preds = %"#dynarray::extend.exit32.i"
  %i_mul.i.i.i = shl nuw i64 %3, 1
  %5 = call i64 @llvm.smax.i64(i64 %i_add.i1, i64 %i_mul.i.i.i)
  %6 = call ptr @malloc(i64 %5)
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %6, ptr noundef nonnull align 1 dereferenceable(5) %0, i64 5, i1 false)
  call void @free(ptr nonnull %4)
  br label %"string::add_string.exit"

"string::add_string.exit":                        ; preds = %"#dynarray::extend.exit32.thread.i", %"#dynarray::extend.exit32.i", %then.i.i.i
  %result_inner.sroa.0.1.i = phi ptr [ %6, %then.i.i.i ], [ %4, %"#dynarray::extend.exit32.i" ], [ %2, %"#dynarray::extend.exit32.thread.i" ]
  %self.0.load.gep.i.i = getelementptr i8, ptr %result_inner.sroa.0.1.i, i64 5
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self.0.load.gep.i.i, ptr align 1 %buf_ptr4.deref.i, i64 %1, i1 false)
  %7 = call dereferenceable_or_null(1) ptr @malloc(i64 1)
  store i8 41, ptr %7, align 1
  %i_add.i7 = add i64 %1, 6
  %8 = call ptr @malloc(i64 %i_add.i7)
  %i_lt.i.i21.i9 = icmp slt i64 %i_add.i7, %i_add.i1
  br i1 %i_lt.i.i21.i9, label %"#dynarray::extend.exit32.i15", label %"#dynarray::extend.exit32.thread.i11"

"#dynarray::extend.exit32.thread.i11":            ; preds = %"string::add_string.exit"
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %8, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i1, i1 false)
  br label %"string::add_string.exit23"

"#dynarray::extend.exit32.i15":                   ; preds = %"string::add_string.exit"
  %i_mul.i.i22.i12 = shl i64 %i_add.i7, 1
  %9 = call i64 @llvm.smax.i64(i64 %i_add.i1, i64 %i_mul.i.i22.i12)
  %10 = call ptr @malloc(i64 %9)
  call void @free(ptr %8)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %10, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i1, i1 false)
  %i_lt.i.i.i14 = icmp slt i64 %9, %i_add.i7
  br i1 %i_lt.i.i.i14, label %then.i.i.i17, label %"string::add_string.exit23"

then.i.i.i17:                                     ; preds = %"#dynarray::extend.exit32.i15"
  %i_mul.i.i.i16 = shl i64 %9, 1
  %11 = call i64 @llvm.smax.i64(i64 %i_add.i7, i64 %i_mul.i.i.i16)
  %12 = call ptr @malloc(i64 %11)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %12, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i1, i1 false)
  call void @free(ptr %10)
  br label %"string::add_string.exit23"

"string::add_string.exit23":                      ; preds = %"#dynarray::extend.exit32.thread.i11", %"#dynarray::extend.exit32.i15", %then.i.i.i17
  %result_inner.sroa.15.1.i19 = phi i64 [ %11, %then.i.i.i17 ], [ %9, %"#dynarray::extend.exit32.i15" ], [ %i_add.i7, %"#dynarray::extend.exit32.thread.i11" ]
  %result_inner.sroa.0.1.i20 = phi ptr [ %12, %then.i.i.i17 ], [ %10, %"#dynarray::extend.exit32.i15" ], [ %8, %"#dynarray::extend.exit32.thread.i11" ]
  %self.0.load.gep.i.i21 = getelementptr i8, ptr %result_inner.sroa.0.1.i20, i64 %i_add.i1
  %13 = load i8, ptr %7, align 1
  store i8 %13, ptr %self.0.load.gep.i.i21, align 1
  br label %merge

else:                                             ; preds = %body
  %14 = alloca [4 x i8], align 4
  store i8 110, ptr %14, align 4
  %.repack1 = getelementptr inbounds [4 x i8], ptr %14, i64 0, i64 1
  store i8 111, ptr %.repack1, align 1
  %.repack2 = getelementptr inbounds [4 x i8], ptr %14, i64 0, i64 2
  store i8 110, ptr %.repack2, align 2
  %.repack3 = getelementptr inbounds [4 x i8], ptr %14, i64 0, i64 3
  store i8 101, ptr %.repack3, align 1
  %15 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  %16 = load i32, ptr %14, align 4
  store i32 %16, ptr %15, align 1
  br label %merge

merge:                                            ; preds = %else, %"string::add_string.exit23"
  %result_inner.sroa.0.1.i20.pn = phi ptr [ %result_inner.sroa.0.1.i20, %"string::add_string.exit23" ], [ %15, %else ]
  %i_add.i7.pn = phi i64 [ %i_add.i7, %"string::add_string.exit23" ], [ 4, %else ]
  %result_inner.sroa.15.1.i19.pn = phi i64 [ %result_inner.sroa.15.1.i19, %"string::add_string.exit23" ], [ 4, %else ]
  %.pn27 = insertvalue %"#dynarray" undef, ptr %result_inner.sroa.0.1.i20.pn, 0
  %.pn = insertvalue %"#dynarray" %.pn27, i64 %i_add.i7.pn, 1
  %result_inner29.i22.pn = insertvalue %"#dynarray" %.pn, i64 %result_inner.sroa.15.1.i19.pn, 2
  %if_result = insertvalue %string zeroinitializer, %"#dynarray" %result_inner29.i22.pn, 0
  ret %string %if_result
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %string @"string::from_raw"(ptr nocapture readonly %contents, i64 %len) local_unnamed_addr #3 {
body:
  %0 = tail call ptr @malloc(i64 %len)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %0, ptr align 1 %contents, i64 %len, i1 false)
  %1 = insertvalue %"#dynarray" undef, ptr %0, 0
  %2 = insertvalue %"#dynarray" %1, i64 %len, 1
  %inner69 = insertvalue %"#dynarray" %2, i64 %len, 2
  %3 = insertvalue %string zeroinitializer, %"#dynarray" %inner69, 0
  ret %string %3
}

; Function Attrs: argmemonly mustprogress nocallback nofree nosync nounwind willreturn
declare void @llvm.lifetime.start.p0(i64 immarg, ptr nocapture) #7

; Function Attrs: argmemonly mustprogress nocallback nofree nosync nounwind willreturn
declare void @llvm.lifetime.end.p0(i64 immarg, ptr nocapture) #7

; Function Attrs: mustprogress nounwind willreturn
define %string @"string::add_string"(ptr nocapture readonly %self, ptr nocapture readonly %other) local_unnamed_addr #8 {
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
  %other.0.0.load9 = load ptr, ptr %other, align 8
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
  tail call void @llvm.memmove.p0.p0.i64(ptr align 1 %4, ptr align 1 %self.0.0.load, i64 %self.0.1.load.i, i1 false)
  tail call void @free(ptr %2)
  br label %"#dynarray::extend.exit"

"#dynarray::extend.exit":                         ; preds = %"#dynarray::extend.exit32.thread", %then.i.i, %"#dynarray::extend.exit32"
  %other.0.0.load11 = phi ptr [ %other.0.0.load, %then.i.i ], [ %other.0.0.load, %"#dynarray::extend.exit32" ], [ %other.0.0.load9, %"#dynarray::extend.exit32.thread" ]
  %result_inner.sroa.15.1 = phi i64 [ %3, %then.i.i ], [ %1, %"#dynarray::extend.exit32" ], [ %i_add, %"#dynarray::extend.exit32.thread" ]
  %result_inner.sroa.0.1 = phi ptr [ %4, %then.i.i ], [ %2, %"#dynarray::extend.exit32" ], [ %0, %"#dynarray::extend.exit32.thread" ]
  %self.0.load.gep.i = getelementptr i8, ptr %result_inner.sroa.0.1, i64 %self.0.1.load.i
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self.0.load.gep.i, ptr align 1 %other.0.0.load11, i64 %self.0.1.load.i11, i1 false)
  %5 = insertvalue %"#dynarray" undef, ptr %result_inner.sroa.0.1, 0
  %6 = insertvalue %"#dynarray" %5, i64 %i_add, 1
  %result_inner29 = insertvalue %"#dynarray" %6, i64 %result_inner.sroa.15.1, 2
  %7 = insertvalue %string zeroinitializer, %"#dynarray" %result_inner29, 0
  ret %string %7
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.smax.i64(i64, i64) #2

; Function Attrs: argmemonly mustprogress nocallback nofree nounwind willreturn
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #9

; Function Attrs: inaccessiblemem_or_argmemonly mustprogress nounwind willreturn allockind("free")
declare void @free(ptr allocptr nocapture noundef) local_unnamed_addr #10

define %string @"char::to_string"(i32 %self) local_unnamed_addr {
body:
  %alloca.ptr = alloca ptr, align 8
  %0 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %alloca.ptr, ptr nonnull @_tmpl_char_to_string, i32 %self)
  %buf_ptr4.deref = load ptr, ptr %alloca.ptr, align 8
  %i_add = add i64 %0, 1
  %1 = insertvalue %"#dynarray" zeroinitializer, ptr %buf_ptr4.deref, 0
  %2 = insertvalue %"#dynarray" %1, i64 %0, 1
  %3 = insertvalue %"#dynarray" %2, i64 %i_add, 2
  %4 = insertvalue %string zeroinitializer, %"#dynarray" %3, 0
  ret %string %4
}

; Function Attrs: mustprogress nounwind willreturn
define void @"#dynarray::extend"(ptr nocapture %self, ptr nocapture readonly %add_buf, i64 %add_len) local_unnamed_addr #8 {
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
  %self.14.load.pre = load i64, ptr %self.1, align 4
  br label %"#dynarray::resize.exit"

"#dynarray::resize.exit":                         ; preds = %then.i, %body
  %self.14.load = phi i64 [ %self.14.load.pre, %then.i ], [ %self.1.load, %body ]
  %self.0.load = load ptr, ptr %self, align 8
  %self.0.load.gep = getelementptr i8, ptr %self.0.load, i64 %self.14.load
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self.0.load.gep, ptr align 1 %add_buf, i64 %add_len, i1 false)
  %self.18.load = load i64, ptr %self.1, align 4
  %i_add10 = add i64 %self.18.load, %add_len
  store i64 %i_add10, ptr %self.1, align 4
  ret void
}

; Function Attrs: mustprogress nounwind willreturn
define void @"#dynarray::resize"(ptr nocapture %self, i64 %new_cap) local_unnamed_addr #8 {
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

merge:                                            ; preds = %then, %body
  ret void
}

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::max"(i64 %self, i64 %o) local_unnamed_addr #6 {
body:
  %0 = tail call i64 @llvm.smax.i64(i64 %self, i64 %o)
  ret i64 %0
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define %string @"string::to_string"(ptr nocapture readonly %self) local_unnamed_addr #4 {
body:
  %self1.unpack.unpack = load ptr, ptr %self, align 8
  %0 = insertvalue %"#dynarray" undef, ptr %self1.unpack.unpack, 0
  %self1.unpack.elt2 = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %self1.unpack.unpack3 = load i64, ptr %self1.unpack.elt2, align 8
  %1 = insertvalue %"#dynarray" %0, i64 %self1.unpack.unpack3, 1
  %self1.unpack.elt4 = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %self1.unpack.unpack5 = load i64, ptr %self1.unpack.elt4, align 8
  %self1.unpack6 = insertvalue %"#dynarray" %1, i64 %self1.unpack.unpack5, 2
  %self11 = insertvalue %string undef, %"#dynarray" %self1.unpack6, 0
  ret %string %self11
}

define ptr @"#dynarray::take"(ptr nocapture %self, i64 %sub_len) local_unnamed_addr {
body:
  %self.1 = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %self.1.load = load i64, ptr %self.1, align 4
  %i_ge.not = icmp slt i64 %self.1.load, %sub_len
  br i1 %i_ge.not, label %else, label %block

else:                                             ; preds = %body
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.2, i64 30, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

block:                                            ; preds = %body
  %i_sub = sub i64 %self.1.load, %sub_len
  store i64 %i_sub, ptr %self.1, align 4
  %self.0.load = load ptr, ptr %self, align 8
  %self.0.load.gep = getelementptr i8, ptr %self.0.load, i64 %i_sub
  ret ptr %self.0.load.gep
}

define %string @"#byte::to_string"(i8 %self) local_unnamed_addr {
body:
  %alloca.ptr = alloca ptr, align 8
  %0 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %alloca.ptr, ptr nonnull @_tmpl_byte_to_string, i8 %self)
  %buf_ptr4.deref = load ptr, ptr %alloca.ptr, align 8
  %i_add = add i64 %0, 1
  %1 = insertvalue %"#dynarray" zeroinitializer, ptr %buf_ptr4.deref, 0
  %2 = insertvalue %"#dynarray" %1, i64 %0, 1
  %3 = insertvalue %"#dynarray" %2, i64 %i_add, 2
  %4 = insertvalue %string zeroinitializer, %"#dynarray" %3, 0
  ret %string %4
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %string @"bool::to_string"(i1 %self) local_unnamed_addr #3 {
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
  %.pn6 = phi ptr [ %1, %then ], [ %4, %else ]
  %.pn2 = phi i64 [ 4, %then ], [ 5, %else ]
  %.pn3 = insertvalue %"#dynarray" undef, ptr %.pn6, 0
  %.pn = insertvalue %"#dynarray" %.pn3, i64 %.pn2, 1
  %inner69.i.pn = insertvalue %"#dynarray" %.pn, i64 %.pn2, 2
  %if_result = insertvalue %string zeroinitializer, %"#dynarray" %inner69.i.pn, 0
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
define double @"float::sign"(double %self) local_unnamed_addr #6 {
body:
  %0 = tail call double @llvm.copysign.f64(double 1.000000e+00, double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.copysign.f64(double, double) #2

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
define double @"float::round"(double %self) local_unnamed_addr #6 {
body:
  %0 = tail call double @llvm.round.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.round.f64(double) #2

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::trunc"(double %self) local_unnamed_addr #6 {
body:
  %0 = tail call double @llvm.trunc.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.trunc.f64(double) #2

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::floor"(double %self) local_unnamed_addr #6 {
body:
  %0 = tail call double @llvm.floor.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.floor.f64(double) #2

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::ceil"(double %self) local_unnamed_addr #6 {
body:
  %0 = tail call double @llvm.ceil.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.ceil.f64(double) #2

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
define double @"float::cos"(double %self) local_unnamed_addr #6 {
body:
  %0 = tail call double @llvm.cos.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.cos.f64(double) #2

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::sin"(double %self) local_unnamed_addr #6 {
body:
  %0 = tail call double @llvm.sin.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.sin.f64(double) #2

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::powi"(double %self, i64 %exp) local_unnamed_addr #6 {
body:
  %cast = sitofp i64 %exp to double
  %0 = tail call double @llvm.pow.f64(double %self, double %cast)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.pow.f64(double, double) #2

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::pow"(double %self, double %exp) local_unnamed_addr #6 {
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
define double @"float::sqrt"(double %self) local_unnamed_addr #6 {
body:
  %0 = tail call double @llvm.sqrt.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.sqrt.f64(double) #2

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::log1p"(double %self) local_unnamed_addr #11 {
body:
  %0 = tail call double @log1p(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @log1p(double) local_unnamed_addr #11

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::log10"(double %self) local_unnamed_addr #6 {
body:
  %0 = tail call double @llvm.log10.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.log10.f64(double) #2

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::log2"(double %self) local_unnamed_addr #6 {
body:
  %0 = tail call double @llvm.log2.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.log2.f64(double) #2

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::log"(double %self) local_unnamed_addr #6 {
body:
  %0 = tail call double @llvm.log.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.log.f64(double) #2

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::expm1"(double %self) local_unnamed_addr #11 {
body:
  %0 = tail call double @expm1(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @expm1(double) local_unnamed_addr #11

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::exp2"(double %self) local_unnamed_addr #6 {
body:
  %0 = tail call double @llvm.exp2.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.exp2.f64(double) #2

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::exp"(double %self) local_unnamed_addr #6 {
body:
  %0 = tail call double @llvm.exp.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.exp.f64(double) #2

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::min"(double %self, double %o) local_unnamed_addr #6 {
body:
  %0 = tail call double @llvm.minnum.f64(double %self, double %o)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.minnum.f64(double, double) #2

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::max"(double %self, double %o) local_unnamed_addr #6 {
body:
  %0 = tail call double @llvm.maxnum.f64(double %self, double %o)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.maxnum.f64(double, double) #2

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::fma"(double %self, double %multiplicand, double %addend) local_unnamed_addr #6 {
body:
  %0 = tail call double @llvm.fma.f64(double %self, double %multiplicand, double %addend)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.fma.f64(double, double, double) #2

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::abs"(double %self) local_unnamed_addr #6 {
body:
  %0 = tail call double @llvm.fabs.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.fabs.f64(double) #2

define %string @"float::to_string"(double %self) local_unnamed_addr {
body:
  %alloca.ptr = alloca ptr, align 8
  %0 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %alloca.ptr, ptr nonnull @_tmpl_float_to_string, double %self)
  %buf_ptr4.deref = load ptr, ptr %alloca.ptr, align 8
  %i_add = add i64 %0, 1
  %1 = insertvalue %"#dynarray" zeroinitializer, ptr %buf_ptr4.deref, 0
  %2 = insertvalue %"#dynarray" %1, i64 %0, 1
  %3 = insertvalue %"#dynarray" %2, i64 %i_add, 2
  %4 = insertvalue %string zeroinitializer, %"#dynarray" %3, 0
  ret %string %4
}

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::trailing_zeroes"(i64 %self) local_unnamed_addr #6 {
body:
  %0 = tail call i64 @llvm.cttz.i64(i64 %self, i1 false), !range !0
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.cttz.i64(i64, i1 immarg) #2

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::leading_zeroes"(i64 %self) local_unnamed_addr #6 {
body:
  %0 = tail call i64 @llvm.ctlz.i64(i64 %self, i1 false), !range !0
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.ctlz.i64(i64, i1 immarg) #2

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::reverse_bytes"(i64 %self) local_unnamed_addr #6 {
body:
  %0 = tail call i64 @llvm.bswap.i64(i64 %self)
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.bswap.i64(i64) #2

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::reverse_bits"(i64 %self) local_unnamed_addr #6 {
body:
  %0 = tail call i64 @llvm.bitreverse.i64(i64 %self)
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.bitreverse.i64(i64) #2

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::count_ones"(i64 %self) local_unnamed_addr #6 {
body:
  %0 = tail call i64 @llvm.ctpop.i64(i64 %self), !range !0
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.ctpop.i64(i64) #2

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define i64 @"int::sign"(i64 %self) local_unnamed_addr #5 {
body:
  %i_lt.not = icmp ne i64 %self, 0
  %spec.select = sext i1 %i_lt.not to i64
  %i_gt.inv = icmp slt i64 %self, 1
  %if_result = select i1 %i_gt.inv, i64 %spec.select, i64 1
  ret i64 %if_result
}

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::abs"(i64 %self) local_unnamed_addr #6 {
body:
  %0 = tail call i64 @llvm.abs.i64(i64 %self, i1 false)
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.abs.i64(i64, i1 immarg) #2

define i64 @"int::idiv"(i64 %self, i64 %d) local_unnamed_addr {
body:
  %i_eq = icmp eq i64 %d, 0
  br i1 %i_eq, label %then, label %else

then:                                             ; preds = %body
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.4, i64 22, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

else:                                             ; preds = %body
  %2 = sdiv i64 %self, %d
  ret i64 %2
}

define %string @"int::to_string"(i64 %self) local_unnamed_addr {
body:
  %alloca.ptr = alloca ptr, align 8
  %0 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %alloca.ptr, ptr nonnull @_tmpl_int_to_string, i64 %self)
  %buf_ptr4.deref = load ptr, ptr %alloca.ptr, align 8
  %i_add = add i64 %0, 1
  %1 = insertvalue %"#dynarray" zeroinitializer, ptr %buf_ptr4.deref, 0
  %2 = insertvalue %"#dynarray" %1, i64 %0, 1
  %3 = insertvalue %"#dynarray" %2, i64 %i_add, 2
  %4 = insertvalue %string zeroinitializer, %"#dynarray" %3, 0
  ret %string %4
}

declare ptr @setlocale(i64, ptr) local_unnamed_addr

; Function Attrs: mustprogress nofree nounwind willreturn
define %string @"list<int>::new"() local_unnamed_addr #3 {
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
  %inner1.unpack.unpack.i.i = load ptr, ptr %self, align 8
  %inner1.unpack.elt2.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %inner1.unpack.unpack3.i.i = load i64, ptr %inner1.unpack.elt2.i.i, align 8
  %inner1.unpack.elt4.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %inner1.unpack.unpack5.i.i = load i64, ptr %inner1.unpack.elt4.i.i, align 8
  %0 = alloca %"listiterator<int>", align 8
  store ptr %inner1.unpack.unpack.i.i, ptr %0, align 8
  %.repack4 = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 1
  store i64 %inner1.unpack.unpack3.i.i, ptr %.repack4, align 8
  %.repack6 = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 2
  store i64 %inner1.unpack.unpack5.i.i, ptr %.repack6, align 8
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
  %2 = call i64 @fwrite(ptr nonnull @throw_msg.4.9, i64 25, i64 1, ptr %stderr.i.i)
  %3 = call i64 @fputwc(i32 10, ptr %stderr.i.i)
  call void @exit(i64 1)
  unreachable

"list<int>::get.exit.i":                          ; preds = %else.i
  %self.0.0.load.i.i = load ptr, ptr %0, align 8
  %self.0.0.load.gep.i.i = getelementptr i64, ptr %self.0.0.load.i.i, i64 %self.1.load.i
  %self.0.0.load.gep.deref.i.i = load i64, ptr %self.0.0.load.gep.i.i, align 4
  %4 = call dereferenceable_or_null(8) ptr @malloc(i64 8)
  store i64 %self.0.0.load.gep.deref.i.i, ptr %4, align 4
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
  %10 = call i64 @fwrite(ptr nonnull @throw_msg.4.9, i64 25, i64 1, ptr %stderr.i.i17)
  %11 = call i64 @fputwc(i32 10, ptr %stderr.i.i17)
  call void @exit(i64 1)
  unreachable

"list<int>::get.exit.i23":                        ; preds = %else.i16
  %self.0.0.load.i.i19 = load ptr, ptr %0, align 8
  %self.0.0.load.gep.i.i20 = getelementptr i64, ptr %self.0.0.load.i.i19, i64 %self.1.load.i11
  %self.0.0.load.gep.deref.i.i21 = load i64, ptr %self.0.0.load.gep.i.i20, align 4
  %12 = call dereferenceable_or_null(8) ptr @malloc(i64 8)
  store i64 %self.0.0.load.gep.deref.i.i21, ptr %12, align 4
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
  %inner1.unpack.unpack.i = load ptr, ptr %self, align 8
  %0 = insertvalue %"#dynarray" undef, ptr %inner1.unpack.unpack.i, 0
  %inner1.unpack.elt2.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %inner1.unpack.unpack3.i = load i64, ptr %inner1.unpack.elt2.i, align 8
  %1 = insertvalue %"#dynarray" %0, i64 %inner1.unpack.unpack3.i, 1
  %inner1.unpack.elt4.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %inner1.unpack.unpack5.i = load i64, ptr %inner1.unpack.elt4.i, align 8
  %inner1.unpack6.i = insertvalue %"#dynarray" %1, i64 %inner1.unpack.unpack5.i, 2
  %inner11.i = insertvalue %string undef, %"#dynarray" %inner1.unpack6.i, 0
  %2 = insertvalue %"listiterator<int>" zeroinitializer, %string %inner11.i, 0
  %3 = insertvalue %"listiterator<int>" %2, i64 0, 1
  ret %"listiterator<int>" %3
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
  %1 = tail call i64 @fwrite(ptr nonnull @throw_msg.4.9, i64 25, i64 1, ptr %stderr.i)
  %2 = tail call i64 @fputwc(i32 10, ptr %stderr.i)
  tail call void @exit(i64 1)
  unreachable

"list<int>::get.exit":                            ; preds = %else
  %self.0.0.load.i = load ptr, ptr %self, align 8
  %self.0.0.load.gep.i = getelementptr i64, ptr %self.0.0.load.i, i64 %self.1.load
  %self.0.0.load.gep.deref.i = load i64, ptr %self.0.0.load.gep.i, align 4
  %3 = tail call dereferenceable_or_null(8) ptr @malloc(i64 8)
  store i64 %self.0.0.load.gep.deref.i, ptr %3, align 4
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
define %"option<char>" @"option<int>::none"() local_unnamed_addr #5 {
body:
  ret %"option<char>" zeroinitializer
}

define i64 @"list<int>::get"(ptr nocapture readonly %self, i64 %i) local_unnamed_addr {
body:
  %i_le = icmp sgt i64 %i, -1
  br i1 %i_le, label %and_true, label %else

else:                                             ; preds = %body, %and_true
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.4.9, i64 25, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

merge:                                            ; preds = %and_true
  %self.0.0.load = load ptr, ptr %self, align 8
  %self.0.0.load.gep = getelementptr i64, ptr %self.0.0.load, i64 %i
  %self.0.0.load.gep.deref = load i64, ptr %self.0.0.load.gep, align 4
  ret i64 %self.0.0.load.gep.deref

and_true:                                         ; preds = %body
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %2 = sdiv i64 %self.0.1.load.i, 8
  %i_lt = icmp sgt i64 %2, %i
  br i1 %i_lt, label %merge, label %else
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %"option<char>" @"option<int>::some"(i64 %t) local_unnamed_addr #3 {
body:
  %0 = tail call dereferenceable_or_null(8) ptr @malloc(i64 8)
  store i64 %t, ptr %0, align 4
  %1 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %0, 1
  ret %"option<char>" %1
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define %"listiterator<int>" @"listiterator<int>::new"(ptr nocapture readonly %inner) local_unnamed_addr #4 {
body:
  %inner1.unpack.unpack = load ptr, ptr %inner, align 8
  %0 = insertvalue %"#dynarray" undef, ptr %inner1.unpack.unpack, 0
  %inner1.unpack.elt2 = getelementptr inbounds %"#dynarray", ptr %inner, i64 0, i32 1
  %inner1.unpack.unpack3 = load i64, ptr %inner1.unpack.elt2, align 8
  %1 = insertvalue %"#dynarray" %0, i64 %inner1.unpack.unpack3, 1
  %inner1.unpack.elt4 = getelementptr inbounds %"#dynarray", ptr %inner, i64 0, i32 2
  %inner1.unpack.unpack5 = load i64, ptr %inner1.unpack.elt4, align 8
  %inner1.unpack6 = insertvalue %"#dynarray" %1, i64 %inner1.unpack.unpack5, 2
  %inner11 = insertvalue %string undef, %"#dynarray" %inner1.unpack6, 0
  %2 = insertvalue %"listiterator<int>" zeroinitializer, %string %inner11, 0
  %3 = insertvalue %"listiterator<int>" %2, i64 0, 1
  ret %"listiterator<int>" %3
}

define %string @"option<option<option<option<int>>>>::to_string"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %self.0.load = load i1, ptr %self, align 1
  br i1 %self.0.load, label %then, label %else

then:                                             ; preds = %body
  %value = alloca %"option<char>", align 8
  %self.1 = getelementptr inbounds %"option<char>", ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 8 dereferenceable(16) %value, ptr noundef nonnull align 8 dereferenceable(16) %self.1.load, i64 16, i1 false)
  %0 = alloca [5 x i8], align 1
  store i8 115, ptr %0, align 1
  %.repack4 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 1
  store i8 111, ptr %.repack4, align 1
  %.repack5 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 2
  store i8 109, ptr %.repack5, align 1
  %.repack6 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 3
  store i8 101, ptr %.repack6, align 1
  %.repack7 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 4
  store i8 40, ptr %.repack7, align 1
  %1 = call %string @"option<option<option<int>>>::to_string"(ptr nonnull %value)
  %2 = extractvalue %string %1, 0
  %.elt12 = extractvalue %"#dynarray" %2, 0
  %.elt14 = extractvalue %"#dynarray" %2, 1
  %i_add.i = add i64 %.elt14, 5
  %3 = tail call ptr @malloc(i64 %i_add.i)
  %i_lt.i.i21.i = icmp ugt i64 %.elt14, 9223372036854775802
  br i1 %i_lt.i.i21.i, label %"#dynarray::extend.exit32.i", label %"#dynarray::extend.exit32.thread.i"

"#dynarray::extend.exit32.thread.i":              ; preds = %then
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %3, ptr noundef nonnull align 1 dereferenceable(5) %0, i64 5, i1 false)
  br label %"string::add_string.exit"

"#dynarray::extend.exit32.i":                     ; preds = %then
  %i_mul.i.i22.i = shl i64 %i_add.i, 1
  %4 = tail call i64 @llvm.smax.i64(i64 %i_mul.i.i22.i, i64 5)
  %5 = tail call ptr @malloc(i64 %4)
  tail call void @free(ptr %3)
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %5, ptr noundef nonnull align 1 dereferenceable(5) %0, i64 5, i1 false)
  %i_lt.i.i.i = icmp slt i64 %4, %i_add.i
  br i1 %i_lt.i.i.i, label %then.i.i.i, label %"string::add_string.exit"

then.i.i.i:                                       ; preds = %"#dynarray::extend.exit32.i"
  %i_mul.i.i.i = shl nuw i64 %4, 1
  %6 = tail call i64 @llvm.smax.i64(i64 %i_add.i, i64 %i_mul.i.i.i)
  %7 = tail call ptr @malloc(i64 %6)
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %7, ptr noundef nonnull align 1 dereferenceable(5) %0, i64 5, i1 false)
  tail call void @free(ptr nonnull %5)
  br label %"string::add_string.exit"

"string::add_string.exit":                        ; preds = %"#dynarray::extend.exit32.thread.i", %"#dynarray::extend.exit32.i", %then.i.i.i
  %result_inner.sroa.0.1.i = phi ptr [ %7, %then.i.i.i ], [ %5, %"#dynarray::extend.exit32.i" ], [ %3, %"#dynarray::extend.exit32.thread.i" ]
  %self.0.load.gep.i.i = getelementptr i8, ptr %result_inner.sroa.0.1.i, i64 5
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self.0.load.gep.i.i, ptr align 1 %.elt12, i64 %.elt14, i1 false)
  %8 = tail call dereferenceable_or_null(1) ptr @malloc(i64 1)
  store i8 41, ptr %8, align 1
  %i_add.i32 = add i64 %.elt14, 6
  %9 = tail call ptr @malloc(i64 %i_add.i32)
  %i_lt.i.i21.i34 = icmp slt i64 %i_add.i32, %i_add.i
  br i1 %i_lt.i.i21.i34, label %"#dynarray::extend.exit32.i40", label %"#dynarray::extend.exit32.thread.i36"

"#dynarray::extend.exit32.thread.i36":            ; preds = %"string::add_string.exit"
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %9, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i, i1 false)
  br label %"string::add_string.exit48"

"#dynarray::extend.exit32.i40":                   ; preds = %"string::add_string.exit"
  %i_mul.i.i22.i37 = shl i64 %i_add.i32, 1
  %10 = tail call i64 @llvm.smax.i64(i64 %i_add.i, i64 %i_mul.i.i22.i37)
  %11 = tail call ptr @malloc(i64 %10)
  tail call void @free(ptr %9)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %11, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i, i1 false)
  %i_lt.i.i.i39 = icmp slt i64 %10, %i_add.i32
  br i1 %i_lt.i.i.i39, label %then.i.i.i42, label %"string::add_string.exit48"

then.i.i.i42:                                     ; preds = %"#dynarray::extend.exit32.i40"
  %i_mul.i.i.i41 = shl i64 %10, 1
  %12 = tail call i64 @llvm.smax.i64(i64 %i_add.i32, i64 %i_mul.i.i.i41)
  %13 = tail call ptr @malloc(i64 %12)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %13, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i, i1 false)
  tail call void @free(ptr %11)
  br label %"string::add_string.exit48"

"string::add_string.exit48":                      ; preds = %"#dynarray::extend.exit32.thread.i36", %"#dynarray::extend.exit32.i40", %then.i.i.i42
  %result_inner.sroa.15.1.i44 = phi i64 [ %12, %then.i.i.i42 ], [ %10, %"#dynarray::extend.exit32.i40" ], [ %i_add.i32, %"#dynarray::extend.exit32.thread.i36" ]
  %result_inner.sroa.0.1.i45 = phi ptr [ %13, %then.i.i.i42 ], [ %11, %"#dynarray::extend.exit32.i40" ], [ %9, %"#dynarray::extend.exit32.thread.i36" ]
  %self.0.load.gep.i.i46 = getelementptr i8, ptr %result_inner.sroa.0.1.i45, i64 %i_add.i
  %14 = load i8, ptr %8, align 1
  store i8 %14, ptr %self.0.load.gep.i.i46, align 1
  br label %merge

else:                                             ; preds = %body
  %15 = alloca [4 x i8], align 4
  store i8 110, ptr %15, align 4
  %.repack1 = getelementptr inbounds [4 x i8], ptr %15, i64 0, i64 1
  store i8 111, ptr %.repack1, align 1
  %.repack2 = getelementptr inbounds [4 x i8], ptr %15, i64 0, i64 2
  store i8 110, ptr %.repack2, align 2
  %.repack3 = getelementptr inbounds [4 x i8], ptr %15, i64 0, i64 3
  store i8 101, ptr %.repack3, align 1
  %16 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  %17 = load i32, ptr %15, align 4
  store i32 %17, ptr %16, align 1
  br label %merge

merge:                                            ; preds = %else, %"string::add_string.exit48"
  %result_inner.sroa.0.1.i45.pn = phi ptr [ %result_inner.sroa.0.1.i45, %"string::add_string.exit48" ], [ %16, %else ]
  %i_add.i32.pn = phi i64 [ %i_add.i32, %"string::add_string.exit48" ], [ 4, %else ]
  %result_inner.sroa.15.1.i44.pn = phi i64 [ %result_inner.sroa.15.1.i44, %"string::add_string.exit48" ], [ 4, %else ]
  %.pn52 = insertvalue %"#dynarray" undef, ptr %result_inner.sroa.0.1.i45.pn, 0
  %.pn = insertvalue %"#dynarray" %.pn52, i64 %i_add.i32.pn, 1
  %result_inner29.i47.pn = insertvalue %"#dynarray" %.pn, i64 %result_inner.sroa.15.1.i44.pn, 2
  %if_result = insertvalue %string zeroinitializer, %"#dynarray" %result_inner29.i47.pn, 0
  ret %string %if_result
}

define %string @"option<option<option<int>>>::to_string"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %self.0.load = load i1, ptr %self, align 1
  br i1 %self.0.load, label %then, label %else

then:                                             ; preds = %body
  %value = alloca %"option<char>", align 8
  %self.1 = getelementptr inbounds %"option<char>", ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 8 dereferenceable(16) %value, ptr noundef nonnull align 8 dereferenceable(16) %self.1.load, i64 16, i1 false)
  %0 = alloca [5 x i8], align 1
  store i8 115, ptr %0, align 1
  %.repack4 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 1
  store i8 111, ptr %.repack4, align 1
  %.repack5 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 2
  store i8 109, ptr %.repack5, align 1
  %.repack6 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 3
  store i8 101, ptr %.repack6, align 1
  %.repack7 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 4
  store i8 40, ptr %.repack7, align 1
  %1 = call %string @"option<option<int>>::to_string"(ptr nonnull %value)
  %2 = extractvalue %string %1, 0
  %.elt12 = extractvalue %"#dynarray" %2, 0
  %.elt14 = extractvalue %"#dynarray" %2, 1
  %i_add.i = add i64 %.elt14, 5
  %3 = tail call ptr @malloc(i64 %i_add.i)
  %i_lt.i.i21.i = icmp ugt i64 %.elt14, 9223372036854775802
  br i1 %i_lt.i.i21.i, label %"#dynarray::extend.exit32.i", label %"#dynarray::extend.exit32.thread.i"

"#dynarray::extend.exit32.thread.i":              ; preds = %then
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %3, ptr noundef nonnull align 1 dereferenceable(5) %0, i64 5, i1 false)
  br label %"string::add_string.exit"

"#dynarray::extend.exit32.i":                     ; preds = %then
  %i_mul.i.i22.i = shl i64 %i_add.i, 1
  %4 = tail call i64 @llvm.smax.i64(i64 %i_mul.i.i22.i, i64 5)
  %5 = tail call ptr @malloc(i64 %4)
  tail call void @free(ptr %3)
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %5, ptr noundef nonnull align 1 dereferenceable(5) %0, i64 5, i1 false)
  %i_lt.i.i.i = icmp slt i64 %4, %i_add.i
  br i1 %i_lt.i.i.i, label %then.i.i.i, label %"string::add_string.exit"

then.i.i.i:                                       ; preds = %"#dynarray::extend.exit32.i"
  %i_mul.i.i.i = shl nuw i64 %4, 1
  %6 = tail call i64 @llvm.smax.i64(i64 %i_add.i, i64 %i_mul.i.i.i)
  %7 = tail call ptr @malloc(i64 %6)
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %7, ptr noundef nonnull align 1 dereferenceable(5) %0, i64 5, i1 false)
  tail call void @free(ptr nonnull %5)
  br label %"string::add_string.exit"

"string::add_string.exit":                        ; preds = %"#dynarray::extend.exit32.thread.i", %"#dynarray::extend.exit32.i", %then.i.i.i
  %result_inner.sroa.0.1.i = phi ptr [ %7, %then.i.i.i ], [ %5, %"#dynarray::extend.exit32.i" ], [ %3, %"#dynarray::extend.exit32.thread.i" ]
  %self.0.load.gep.i.i = getelementptr i8, ptr %result_inner.sroa.0.1.i, i64 5
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self.0.load.gep.i.i, ptr align 1 %.elt12, i64 %.elt14, i1 false)
  %8 = tail call dereferenceable_or_null(1) ptr @malloc(i64 1)
  store i8 41, ptr %8, align 1
  %i_add.i32 = add i64 %.elt14, 6
  %9 = tail call ptr @malloc(i64 %i_add.i32)
  %i_lt.i.i21.i34 = icmp slt i64 %i_add.i32, %i_add.i
  br i1 %i_lt.i.i21.i34, label %"#dynarray::extend.exit32.i40", label %"#dynarray::extend.exit32.thread.i36"

"#dynarray::extend.exit32.thread.i36":            ; preds = %"string::add_string.exit"
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %9, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i, i1 false)
  br label %"string::add_string.exit48"

"#dynarray::extend.exit32.i40":                   ; preds = %"string::add_string.exit"
  %i_mul.i.i22.i37 = shl i64 %i_add.i32, 1
  %10 = tail call i64 @llvm.smax.i64(i64 %i_add.i, i64 %i_mul.i.i22.i37)
  %11 = tail call ptr @malloc(i64 %10)
  tail call void @free(ptr %9)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %11, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i, i1 false)
  %i_lt.i.i.i39 = icmp slt i64 %10, %i_add.i32
  br i1 %i_lt.i.i.i39, label %then.i.i.i42, label %"string::add_string.exit48"

then.i.i.i42:                                     ; preds = %"#dynarray::extend.exit32.i40"
  %i_mul.i.i.i41 = shl i64 %10, 1
  %12 = tail call i64 @llvm.smax.i64(i64 %i_add.i32, i64 %i_mul.i.i.i41)
  %13 = tail call ptr @malloc(i64 %12)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %13, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i, i1 false)
  tail call void @free(ptr %11)
  br label %"string::add_string.exit48"

"string::add_string.exit48":                      ; preds = %"#dynarray::extend.exit32.thread.i36", %"#dynarray::extend.exit32.i40", %then.i.i.i42
  %result_inner.sroa.15.1.i44 = phi i64 [ %12, %then.i.i.i42 ], [ %10, %"#dynarray::extend.exit32.i40" ], [ %i_add.i32, %"#dynarray::extend.exit32.thread.i36" ]
  %result_inner.sroa.0.1.i45 = phi ptr [ %13, %then.i.i.i42 ], [ %11, %"#dynarray::extend.exit32.i40" ], [ %9, %"#dynarray::extend.exit32.thread.i36" ]
  %self.0.load.gep.i.i46 = getelementptr i8, ptr %result_inner.sroa.0.1.i45, i64 %i_add.i
  %14 = load i8, ptr %8, align 1
  store i8 %14, ptr %self.0.load.gep.i.i46, align 1
  br label %merge

else:                                             ; preds = %body
  %15 = alloca [4 x i8], align 4
  store i8 110, ptr %15, align 4
  %.repack1 = getelementptr inbounds [4 x i8], ptr %15, i64 0, i64 1
  store i8 111, ptr %.repack1, align 1
  %.repack2 = getelementptr inbounds [4 x i8], ptr %15, i64 0, i64 2
  store i8 110, ptr %.repack2, align 2
  %.repack3 = getelementptr inbounds [4 x i8], ptr %15, i64 0, i64 3
  store i8 101, ptr %.repack3, align 1
  %16 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  %17 = load i32, ptr %15, align 4
  store i32 %17, ptr %16, align 1
  br label %merge

merge:                                            ; preds = %else, %"string::add_string.exit48"
  %result_inner.sroa.0.1.i45.pn = phi ptr [ %result_inner.sroa.0.1.i45, %"string::add_string.exit48" ], [ %16, %else ]
  %i_add.i32.pn = phi i64 [ %i_add.i32, %"string::add_string.exit48" ], [ 4, %else ]
  %result_inner.sroa.15.1.i44.pn = phi i64 [ %result_inner.sroa.15.1.i44, %"string::add_string.exit48" ], [ 4, %else ]
  %.pn52 = insertvalue %"#dynarray" undef, ptr %result_inner.sroa.0.1.i45.pn, 0
  %.pn = insertvalue %"#dynarray" %.pn52, i64 %i_add.i32.pn, 1
  %result_inner29.i47.pn = insertvalue %"#dynarray" %.pn, i64 %result_inner.sroa.15.1.i44.pn, 2
  %if_result = insertvalue %string zeroinitializer, %"#dynarray" %result_inner29.i47.pn, 0
  ret %string %if_result
}

define %string @"option<option<int>>::to_string"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %self.0.load = load i1, ptr %self, align 1
  br i1 %self.0.load, label %then, label %else

then:                                             ; preds = %body
  %value = alloca %"option<char>", align 8
  %self.1 = getelementptr inbounds %"option<char>", ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 8 dereferenceable(16) %value, ptr noundef nonnull align 8 dereferenceable(16) %self.1.load, i64 16, i1 false)
  %0 = alloca [5 x i8], align 1
  store i8 115, ptr %0, align 1
  %.repack4 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 1
  store i8 111, ptr %.repack4, align 1
  %.repack5 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 2
  store i8 109, ptr %.repack5, align 1
  %.repack6 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 3
  store i8 101, ptr %.repack6, align 1
  %.repack7 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 4
  store i8 40, ptr %.repack7, align 1
  %1 = call %string @"option<int>::to_string"(ptr nonnull %value)
  %2 = extractvalue %string %1, 0
  %.elt12 = extractvalue %"#dynarray" %2, 0
  %.elt14 = extractvalue %"#dynarray" %2, 1
  %i_add.i = add i64 %.elt14, 5
  %3 = tail call ptr @malloc(i64 %i_add.i)
  %i_lt.i.i21.i = icmp ugt i64 %.elt14, 9223372036854775802
  br i1 %i_lt.i.i21.i, label %"#dynarray::extend.exit32.i", label %"#dynarray::extend.exit32.thread.i"

"#dynarray::extend.exit32.thread.i":              ; preds = %then
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %3, ptr noundef nonnull align 1 dereferenceable(5) %0, i64 5, i1 false)
  br label %"string::add_string.exit"

"#dynarray::extend.exit32.i":                     ; preds = %then
  %i_mul.i.i22.i = shl i64 %i_add.i, 1
  %4 = tail call i64 @llvm.smax.i64(i64 %i_mul.i.i22.i, i64 5)
  %5 = tail call ptr @malloc(i64 %4)
  tail call void @free(ptr %3)
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %5, ptr noundef nonnull align 1 dereferenceable(5) %0, i64 5, i1 false)
  %i_lt.i.i.i = icmp slt i64 %4, %i_add.i
  br i1 %i_lt.i.i.i, label %then.i.i.i, label %"string::add_string.exit"

then.i.i.i:                                       ; preds = %"#dynarray::extend.exit32.i"
  %i_mul.i.i.i = shl nuw i64 %4, 1
  %6 = tail call i64 @llvm.smax.i64(i64 %i_add.i, i64 %i_mul.i.i.i)
  %7 = tail call ptr @malloc(i64 %6)
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %7, ptr noundef nonnull align 1 dereferenceable(5) %0, i64 5, i1 false)
  tail call void @free(ptr nonnull %5)
  br label %"string::add_string.exit"

"string::add_string.exit":                        ; preds = %"#dynarray::extend.exit32.thread.i", %"#dynarray::extend.exit32.i", %then.i.i.i
  %result_inner.sroa.0.1.i = phi ptr [ %7, %then.i.i.i ], [ %5, %"#dynarray::extend.exit32.i" ], [ %3, %"#dynarray::extend.exit32.thread.i" ]
  %self.0.load.gep.i.i = getelementptr i8, ptr %result_inner.sroa.0.1.i, i64 5
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self.0.load.gep.i.i, ptr align 1 %.elt12, i64 %.elt14, i1 false)
  %8 = tail call dereferenceable_or_null(1) ptr @malloc(i64 1)
  store i8 41, ptr %8, align 1
  %i_add.i32 = add i64 %.elt14, 6
  %9 = tail call ptr @malloc(i64 %i_add.i32)
  %i_lt.i.i21.i34 = icmp slt i64 %i_add.i32, %i_add.i
  br i1 %i_lt.i.i21.i34, label %"#dynarray::extend.exit32.i40", label %"#dynarray::extend.exit32.thread.i36"

"#dynarray::extend.exit32.thread.i36":            ; preds = %"string::add_string.exit"
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %9, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i, i1 false)
  br label %"string::add_string.exit48"

"#dynarray::extend.exit32.i40":                   ; preds = %"string::add_string.exit"
  %i_mul.i.i22.i37 = shl i64 %i_add.i32, 1
  %10 = tail call i64 @llvm.smax.i64(i64 %i_add.i, i64 %i_mul.i.i22.i37)
  %11 = tail call ptr @malloc(i64 %10)
  tail call void @free(ptr %9)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %11, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i, i1 false)
  %i_lt.i.i.i39 = icmp slt i64 %10, %i_add.i32
  br i1 %i_lt.i.i.i39, label %then.i.i.i42, label %"string::add_string.exit48"

then.i.i.i42:                                     ; preds = %"#dynarray::extend.exit32.i40"
  %i_mul.i.i.i41 = shl i64 %10, 1
  %12 = tail call i64 @llvm.smax.i64(i64 %i_add.i32, i64 %i_mul.i.i.i41)
  %13 = tail call ptr @malloc(i64 %12)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %13, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i, i1 false)
  tail call void @free(ptr %11)
  br label %"string::add_string.exit48"

"string::add_string.exit48":                      ; preds = %"#dynarray::extend.exit32.thread.i36", %"#dynarray::extend.exit32.i40", %then.i.i.i42
  %result_inner.sroa.15.1.i44 = phi i64 [ %12, %then.i.i.i42 ], [ %10, %"#dynarray::extend.exit32.i40" ], [ %i_add.i32, %"#dynarray::extend.exit32.thread.i36" ]
  %result_inner.sroa.0.1.i45 = phi ptr [ %13, %then.i.i.i42 ], [ %11, %"#dynarray::extend.exit32.i40" ], [ %9, %"#dynarray::extend.exit32.thread.i36" ]
  %self.0.load.gep.i.i46 = getelementptr i8, ptr %result_inner.sroa.0.1.i45, i64 %i_add.i
  %14 = load i8, ptr %8, align 1
  store i8 %14, ptr %self.0.load.gep.i.i46, align 1
  br label %merge

else:                                             ; preds = %body
  %15 = alloca [4 x i8], align 4
  store i8 110, ptr %15, align 4
  %.repack1 = getelementptr inbounds [4 x i8], ptr %15, i64 0, i64 1
  store i8 111, ptr %.repack1, align 1
  %.repack2 = getelementptr inbounds [4 x i8], ptr %15, i64 0, i64 2
  store i8 110, ptr %.repack2, align 2
  %.repack3 = getelementptr inbounds [4 x i8], ptr %15, i64 0, i64 3
  store i8 101, ptr %.repack3, align 1
  %16 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  %17 = load i32, ptr %15, align 4
  store i32 %17, ptr %16, align 1
  br label %merge

merge:                                            ; preds = %else, %"string::add_string.exit48"
  %result_inner.sroa.0.1.i45.pn = phi ptr [ %result_inner.sroa.0.1.i45, %"string::add_string.exit48" ], [ %16, %else ]
  %i_add.i32.pn = phi i64 [ %i_add.i32, %"string::add_string.exit48" ], [ 4, %else ]
  %result_inner.sroa.15.1.i44.pn = phi i64 [ %result_inner.sroa.15.1.i44, %"string::add_string.exit48" ], [ 4, %else ]
  %.pn52 = insertvalue %"#dynarray" undef, ptr %result_inner.sroa.0.1.i45.pn, 0
  %.pn = insertvalue %"#dynarray" %.pn52, i64 %i_add.i32.pn, 1
  %result_inner29.i47.pn = insertvalue %"#dynarray" %.pn, i64 %result_inner.sroa.15.1.i44.pn, 2
  %if_result = insertvalue %string zeroinitializer, %"#dynarray" %result_inner29.i47.pn, 0
  ret %string %if_result
}

define %string @"option<int>::to_string"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %alloca.ptr.i = alloca ptr, align 8
  %self.0.load = load i1, ptr %self, align 1
  br i1 %self.0.load, label %then, label %else

then:                                             ; preds = %body
  %self.1 = getelementptr inbounds %"option<char>", ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  %self.1.load.deref = load i64, ptr %self.1.load, align 4
  %0 = alloca [5 x i8], align 1
  store i8 115, ptr %0, align 1
  %.repack4 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 1
  store i8 111, ptr %.repack4, align 1
  %.repack5 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 2
  store i8 109, ptr %.repack5, align 1
  %.repack6 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 3
  store i8 101, ptr %.repack6, align 1
  %.repack7 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 4
  store i8 40, ptr %.repack7, align 1
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %alloca.ptr.i)
  %1 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %alloca.ptr.i, ptr nonnull @_tmpl_int_to_string, i64 %self.1.load.deref)
  %buf_ptr4.deref.i = load ptr, ptr %alloca.ptr.i, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %alloca.ptr.i)
  %i_add.i27 = add i64 %1, 5
  %2 = call ptr @malloc(i64 %i_add.i27)
  %i_lt.i.i21.i = icmp ugt i64 %1, 9223372036854775802
  br i1 %i_lt.i.i21.i, label %"#dynarray::extend.exit32.i", label %"#dynarray::extend.exit32.thread.i"

"#dynarray::extend.exit32.thread.i":              ; preds = %then
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %2, ptr noundef nonnull align 1 dereferenceable(5) %0, i64 5, i1 false)
  br label %"string::add_string.exit"

"#dynarray::extend.exit32.i":                     ; preds = %then
  %i_mul.i.i22.i = shl i64 %i_add.i27, 1
  %3 = call i64 @llvm.smax.i64(i64 %i_mul.i.i22.i, i64 5)
  %4 = call ptr @malloc(i64 %3)
  call void @free(ptr %2)
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %4, ptr noundef nonnull align 1 dereferenceable(5) %0, i64 5, i1 false)
  %i_lt.i.i.i = icmp slt i64 %3, %i_add.i27
  br i1 %i_lt.i.i.i, label %then.i.i.i, label %"string::add_string.exit"

then.i.i.i:                                       ; preds = %"#dynarray::extend.exit32.i"
  %i_mul.i.i.i = shl nuw i64 %3, 1
  %5 = call i64 @llvm.smax.i64(i64 %i_add.i27, i64 %i_mul.i.i.i)
  %6 = call ptr @malloc(i64 %5)
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %6, ptr noundef nonnull align 1 dereferenceable(5) %0, i64 5, i1 false)
  call void @free(ptr nonnull %4)
  br label %"string::add_string.exit"

"string::add_string.exit":                        ; preds = %"#dynarray::extend.exit32.thread.i", %"#dynarray::extend.exit32.i", %then.i.i.i
  %result_inner.sroa.0.1.i = phi ptr [ %6, %then.i.i.i ], [ %4, %"#dynarray::extend.exit32.i" ], [ %2, %"#dynarray::extend.exit32.thread.i" ]
  %self.0.load.gep.i.i = getelementptr i8, ptr %result_inner.sroa.0.1.i, i64 5
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self.0.load.gep.i.i, ptr align 1 %buf_ptr4.deref.i, i64 %1, i1 false)
  %7 = call dereferenceable_or_null(1) ptr @malloc(i64 1)
  store i8 41, ptr %7, align 1
  %i_add.i33 = add i64 %1, 6
  %8 = call ptr @malloc(i64 %i_add.i33)
  %i_lt.i.i21.i35 = icmp slt i64 %i_add.i33, %i_add.i27
  br i1 %i_lt.i.i21.i35, label %"#dynarray::extend.exit32.i41", label %"#dynarray::extend.exit32.thread.i37"

"#dynarray::extend.exit32.thread.i37":            ; preds = %"string::add_string.exit"
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %8, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i27, i1 false)
  br label %"string::add_string.exit49"

"#dynarray::extend.exit32.i41":                   ; preds = %"string::add_string.exit"
  %i_mul.i.i22.i38 = shl i64 %i_add.i33, 1
  %9 = call i64 @llvm.smax.i64(i64 %i_add.i27, i64 %i_mul.i.i22.i38)
  %10 = call ptr @malloc(i64 %9)
  call void @free(ptr %8)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %10, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i27, i1 false)
  %i_lt.i.i.i40 = icmp slt i64 %9, %i_add.i33
  br i1 %i_lt.i.i.i40, label %then.i.i.i43, label %"string::add_string.exit49"

then.i.i.i43:                                     ; preds = %"#dynarray::extend.exit32.i41"
  %i_mul.i.i.i42 = shl i64 %9, 1
  %11 = call i64 @llvm.smax.i64(i64 %i_add.i33, i64 %i_mul.i.i.i42)
  %12 = call ptr @malloc(i64 %11)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %12, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i27, i1 false)
  call void @free(ptr %10)
  br label %"string::add_string.exit49"

"string::add_string.exit49":                      ; preds = %"#dynarray::extend.exit32.thread.i37", %"#dynarray::extend.exit32.i41", %then.i.i.i43
  %result_inner.sroa.15.1.i45 = phi i64 [ %11, %then.i.i.i43 ], [ %9, %"#dynarray::extend.exit32.i41" ], [ %i_add.i33, %"#dynarray::extend.exit32.thread.i37" ]
  %result_inner.sroa.0.1.i46 = phi ptr [ %12, %then.i.i.i43 ], [ %10, %"#dynarray::extend.exit32.i41" ], [ %8, %"#dynarray::extend.exit32.thread.i37" ]
  %self.0.load.gep.i.i47 = getelementptr i8, ptr %result_inner.sroa.0.1.i46, i64 %i_add.i27
  %13 = load i8, ptr %7, align 1
  store i8 %13, ptr %self.0.load.gep.i.i47, align 1
  br label %merge

else:                                             ; preds = %body
  %14 = alloca [4 x i8], align 4
  store i8 110, ptr %14, align 4
  %.repack1 = getelementptr inbounds [4 x i8], ptr %14, i64 0, i64 1
  store i8 111, ptr %.repack1, align 1
  %.repack2 = getelementptr inbounds [4 x i8], ptr %14, i64 0, i64 2
  store i8 110, ptr %.repack2, align 2
  %.repack3 = getelementptr inbounds [4 x i8], ptr %14, i64 0, i64 3
  store i8 101, ptr %.repack3, align 1
  %15 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  %16 = load i32, ptr %14, align 4
  store i32 %16, ptr %15, align 1
  br label %merge

merge:                                            ; preds = %else, %"string::add_string.exit49"
  %result_inner.sroa.0.1.i46.pn = phi ptr [ %result_inner.sroa.0.1.i46, %"string::add_string.exit49" ], [ %15, %else ]
  %i_add.i33.pn = phi i64 [ %i_add.i33, %"string::add_string.exit49" ], [ 4, %else ]
  %result_inner.sroa.15.1.i45.pn = phi i64 [ %result_inner.sroa.15.1.i45, %"string::add_string.exit49" ], [ 4, %else ]
  %.pn53 = insertvalue %"#dynarray" undef, ptr %result_inner.sroa.0.1.i46.pn, 0
  %.pn = insertvalue %"#dynarray" %.pn53, i64 %i_add.i33.pn, 1
  %result_inner29.i48.pn = insertvalue %"#dynarray" %.pn, i64 %result_inner.sroa.15.1.i45.pn, 2
  %if_result = insertvalue %string zeroinitializer, %"#dynarray" %result_inner29.i48.pn, 0
  ret %string %if_result
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %"option<char>" @"option<option<option<option<int>>>>::some"(ptr nocapture readonly %t) local_unnamed_addr #3 {
body:
  %0 = tail call dereferenceable_or_null(16) ptr @malloc(i64 16)
  tail call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 8 dereferenceable(16) %0, ptr noundef nonnull align 8 dereferenceable(16) %t, i64 16, i1 false)
  %1 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %0, 1
  ret %"option<char>" %1
}

define %"option<char>" @"option<option<option<int>>>::get"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %self.0.load = load i1, ptr %self, align 1
  br i1 %self.0.load, label %merge, label %else

else:                                             ; preds = %body
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.5.10, i64 16, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

merge:                                            ; preds = %body
  %self.1 = getelementptr inbounds %"option<char>", ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  %self.1.load.deref = load %"option<char>", ptr %self.1.load, align 8
  ret %"option<char>" %self.1.load.deref
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define %"option<char>" @"option<option<option<int>>>::none"() local_unnamed_addr #5 {
body:
  ret %"option<char>" zeroinitializer
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %"option<char>" @"option<option<int>>::some"(ptr nocapture readonly %t) local_unnamed_addr #3 {
body:
  %0 = tail call dereferenceable_or_null(16) ptr @malloc(i64 16)
  tail call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 8 dereferenceable(16) %0, ptr noundef nonnull align 8 dereferenceable(16) %t, i64 16, i1 false)
  %1 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %0, 1
  ret %"option<char>" %1
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define %"option<char>" @"option<option<int>>::none"() local_unnamed_addr #5 {
body:
  ret %"option<char>" zeroinitializer
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define %"option<char>" @"option<option<option<option<int>>>>::none"() local_unnamed_addr #5 {
body:
  ret %"option<char>" zeroinitializer
}

define i64 @"option<int>::get"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %self.0.load = load i1, ptr %self, align 1
  br i1 %self.0.load, label %merge, label %else

else:                                             ; preds = %body
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.5.10, i64 16, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

merge:                                            ; preds = %body
  %self.1 = getelementptr inbounds %"option<char>", ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  %self.1.load.deref = load i64, ptr %self.1.load, align 4
  ret i64 %self.1.load.deref
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %"option<char>" @"option<option<option<int>>>::some"(ptr nocapture readonly %t) local_unnamed_addr #3 {
body:
  %0 = tail call dereferenceable_or_null(16) ptr @malloc(i64 16)
  tail call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 8 dereferenceable(16) %0, ptr noundef nonnull align 8 dereferenceable(16) %t, i64 16, i1 false)
  %1 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %0, 1
  ret %"option<char>" %1
}

define %"option<char>" @"option<option<option<option<int>>>>::get"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %self.0.load = load i1, ptr %self, align 1
  br i1 %self.0.load, label %merge, label %else

else:                                             ; preds = %body
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.5.10, i64 16, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

merge:                                            ; preds = %body
  %self.1 = getelementptr inbounds %"option<char>", ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  %self.1.load.deref = load %"option<char>", ptr %self.1.load, align 8
  ret %"option<char>" %self.1.load.deref
}

define %string @"list<int>::to_string"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %alloca.ptr.i116 = alloca ptr, align 8
  %alloca.ptr.i = alloca ptr, align 8
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %self.0.1.load.i.off = add i64 %self.0.1.load.i, 7
  %0 = icmp ult i64 %self.0.1.load.i.off, 15
  br i1 %0, label %then, label %else

then:                                             ; preds = %body
  %1 = alloca [2 x i8], align 2
  store i8 91, ptr %1, align 2
  %.repack60 = getelementptr inbounds [2 x i8], ptr %1, i64 0, i64 1
  store i8 93, ptr %.repack60, align 1
  %2 = tail call dereferenceable_or_null(2) ptr @malloc(i64 2)
  %3 = load i16, ptr %1, align 2
  store i16 %3, ptr %2, align 1
  br label %merge

else:                                             ; preds = %body
  %4 = tail call dereferenceable_or_null(1) ptr @malloc(i64 1)
  store i8 91, ptr %4, align 1
  %it = alloca %"listiterator<int>", align 8
  %inner1.unpack.unpack.i.i = load ptr, ptr %self, align 8
  %inner1.unpack.elt4.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %inner1.unpack.unpack5.i.i = load i64, ptr %inner1.unpack.elt4.i.i, align 8
  store ptr %inner1.unpack.unpack.i.i, ptr %it, align 8
  %it.repack9 = getelementptr inbounds %"#dynarray", ptr %it, i64 0, i32 1
  store i64 %self.0.1.load.i, ptr %it.repack9, align 8
  %it.repack11 = getelementptr inbounds %"#dynarray", ptr %it, i64 0, i32 2
  store i64 %inner1.unpack.unpack5.i.i, ptr %it.repack11, align 8
  %it.repack6 = getelementptr inbounds %"listiterator<int>", ptr %it, i64 0, i32 1
  store i64 0, ptr %it.repack6, align 8
  %self.1.i158 = getelementptr inbounds %"listiterator<int>", ptr %it, i64 0, i32 1
  %self.1.load.i159 = load i64, ptr %self.1.i158, align 4
  %self.0.1.i.i = getelementptr inbounds %string, ptr %it, i64 0, i32 0, i32 1
  %self.0.1.load.i.i160 = load i64, ptr %self.0.1.i.i, align 4
  %5 = sdiv i64 %self.0.1.load.i.i160, 8
  %i_ge.not.i = icmp slt i64 %self.1.load.i159, %5
  br i1 %i_ge.not.i, label %else.i161, label %"listiterator<int>::next.exit"

else.i161:                                        ; preds = %else
  %i_le.i.i = icmp sgt i64 %self.1.load.i159, -1
  br i1 %i_le.i.i, label %"list<int>::get.exit.i", label %else.i.i

else.i.i:                                         ; preds = %else.i161
  %stderr.i.i = call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %6 = call i64 @fwrite(ptr nonnull @throw_msg.4.9, i64 25, i64 1, ptr %stderr.i.i)
  %7 = call i64 @fputwc(i32 10, ptr %stderr.i.i)
  call void @exit(i64 1)
  unreachable

"list<int>::get.exit.i":                          ; preds = %else.i161
  %self.0.0.load.i.i = load ptr, ptr %it, align 8
  %self.0.0.load.gep.i.i = getelementptr i64, ptr %self.0.0.load.i.i, i64 %self.1.load.i159
  %self.0.0.load.gep.deref.i.i = load i64, ptr %self.0.0.load.gep.i.i, align 4
  %8 = call dereferenceable_or_null(8) ptr @malloc(i64 8)
  store i64 %self.0.0.load.gep.deref.i.i, ptr %8, align 4
  %9 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %8, 1
  %i_add.i162 = add nuw nsw i64 %self.1.load.i159, 1
  store i64 %i_add.i162, ptr %self.1.i158, align 4
  br label %"listiterator<int>::next.exit"

"listiterator<int>::next.exit":                   ; preds = %else, %"list<int>::get.exit.i"
  %if_result.i = phi %"option<char>" [ %9, %"list<int>::get.exit.i" ], [ zeroinitializer, %else ]
  %10 = alloca %"option<char>", align 8
  store %"option<char>" %if_result.i, ptr %10, align 8
  %self.0.load.i = load i1, ptr %10, align 8
  br i1 %self.0.load.i, label %"option<int>::get.exit", label %else.i

else.i:                                           ; preds = %"listiterator<int>::next.exit"
  %stderr.i = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %11 = tail call i64 @fwrite(ptr nonnull @throw_msg.5.10, i64 16, i64 1, ptr %stderr.i)
  %12 = tail call i64 @fputwc(i32 10, ptr %stderr.i)
  tail call void @exit(i64 1)
  unreachable

"option<int>::get.exit":                          ; preds = %"listiterator<int>::next.exit"
  %self.1.i = getelementptr inbounds %"option<char>", ptr %10, i64 0, i32 1
  %self.1.load.i = load ptr, ptr %self.1.i, align 8
  %self.1.load.deref.i = load i64, ptr %self.1.load.i, align 4
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %alloca.ptr.i)
  %13 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %alloca.ptr.i, ptr nonnull @_tmpl_int_to_string, i64 %self.1.load.deref.i)
  %buf_ptr4.deref.i = load ptr, ptr %alloca.ptr.i, align 8
  %i_add.i72 = add i64 %13, 1
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %alloca.ptr.i)
  %14 = call ptr @malloc(i64 %i_add.i72)
  %i_lt.i.i21.i79 = icmp ugt i64 %13, 9223372036854775806
  br i1 %i_lt.i.i21.i79, label %"#dynarray::extend.exit32.i85", label %"#dynarray::extend.exit32.thread.i81"

"#dynarray::extend.exit32.thread.i81":            ; preds = %"option<int>::get.exit"
  %15 = load i8, ptr %4, align 1
  store i8 %15, ptr %14, align 1
  br label %"string::add_string.exit93"

"#dynarray::extend.exit32.i85":                   ; preds = %"option<int>::get.exit"
  %i_mul.i.i22.i82 = shl i64 %i_add.i72, 1
  %16 = call i64 @llvm.smax.i64(i64 %i_mul.i.i22.i82, i64 1)
  %17 = call ptr @malloc(i64 %16)
  call void @free(ptr %14)
  %18 = load i8, ptr %4, align 1
  store i8 %18, ptr %17, align 1
  %i_lt.i.i.i84 = icmp slt i64 %16, %i_add.i72
  br i1 %i_lt.i.i.i84, label %then.i.i.i87, label %"string::add_string.exit93"

then.i.i.i87:                                     ; preds = %"#dynarray::extend.exit32.i85"
  %i_mul.i.i.i86 = shl nuw i64 %16, 1
  %19 = call i64 @llvm.smax.i64(i64 %i_add.i72, i64 %i_mul.i.i.i86)
  %20 = call ptr @malloc(i64 %19)
  %21 = load i8, ptr %4, align 1
  store i8 %21, ptr %20, align 1
  call void @free(ptr nonnull %17)
  br label %"string::add_string.exit93"

"string::add_string.exit93":                      ; preds = %"#dynarray::extend.exit32.thread.i81", %"#dynarray::extend.exit32.i85", %then.i.i.i87
  %result_inner.sroa.0.1.i90 = phi ptr [ %20, %then.i.i.i87 ], [ %17, %"#dynarray::extend.exit32.i85" ], [ %14, %"#dynarray::extend.exit32.thread.i81" ]
  %self.0.load.gep.i.i91 = getelementptr i8, ptr %result_inner.sroa.0.1.i90, i64 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self.0.load.gep.i.i91, ptr align 1 %buf_ptr4.deref.i, i64 %13, i1 false)
  %self.1.i163 = getelementptr inbounds %"listiterator<int>", ptr %it, i64 0, i32 1
  %self.1.load.i164 = load i64, ptr %self.1.i163, align 4
  %self.0.1.i.i165 = getelementptr inbounds %string, ptr %it, i64 0, i32 0, i32 1
  %self.0.1.load.i.i166 = load i64, ptr %self.0.1.i.i165, align 4
  %22 = sdiv i64 %self.0.1.load.i.i166, 8
  %i_ge.not.i167 = icmp slt i64 %self.1.load.i164, %22
  br i1 %i_ge.not.i167, label %else.i169, label %"listiterator<int>::next.exit178"

else.i169:                                        ; preds = %"string::add_string.exit93"
  %i_le.i.i168 = icmp sgt i64 %self.1.load.i164, -1
  br i1 %i_le.i.i168, label %"list<int>::get.exit.i176", label %else.i.i171

else.i.i171:                                      ; preds = %else.i169
  %stderr.i.i170 = call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %23 = call i64 @fwrite(ptr nonnull @throw_msg.4.9, i64 25, i64 1, ptr %stderr.i.i170)
  %24 = call i64 @fputwc(i32 10, ptr %stderr.i.i170)
  call void @exit(i64 1)
  unreachable

"list<int>::get.exit.i176":                       ; preds = %else.i169
  %self.0.0.load.i.i172 = load ptr, ptr %it, align 8
  %self.0.0.load.gep.i.i173 = getelementptr i64, ptr %self.0.0.load.i.i172, i64 %self.1.load.i164
  %self.0.0.load.gep.deref.i.i174 = load i64, ptr %self.0.0.load.gep.i.i173, align 4
  %25 = call dereferenceable_or_null(8) ptr @malloc(i64 8)
  store i64 %self.0.0.load.gep.deref.i.i174, ptr %25, align 4
  %26 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %25, 1
  %i_add.i175 = add nuw nsw i64 %self.1.load.i164, 1
  store i64 %i_add.i175, ptr %self.1.i163, align 4
  br label %"listiterator<int>::next.exit178"

"listiterator<int>::next.exit178":                ; preds = %"string::add_string.exit93", %"list<int>::get.exit.i176"
  %if_result.i177 = phi %"option<char>" [ %26, %"list<int>::get.exit.i176" ], [ zeroinitializer, %"string::add_string.exit93" ]
  %present70 = extractvalue %"option<char>" %if_result.i177, 0
  br i1 %present70, label %for, label %block6

merge:                                            ; preds = %"string::add_string.exit", %then
  %.pn153 = phi ptr [ %2, %then ], [ %result_inner.sroa.0.1.i, %"string::add_string.exit" ]
  %.pn151 = phi i64 [ 2, %then ], [ %i_add.i, %"string::add_string.exit" ]
  %.pn149 = phi i64 [ 2, %then ], [ %result_inner.sroa.15.1.i, %"string::add_string.exit" ]
  %.pn150 = insertvalue %"#dynarray" undef, ptr %.pn153, 0
  %.pn148 = insertvalue %"#dynarray" %.pn150, i64 %.pn151, 1
  %inner69.i.pn = insertvalue %"#dynarray" %.pn148, i64 %.pn149, 2
  %if_result = insertvalue %string zeroinitializer, %"#dynarray" %inner69.i.pn, 0
  ret %string %if_result

for:                                              ; preds = %"listiterator<int>::next.exit178", %"listiterator<int>::next.exit194"
  %self.0.0.load.i100 = phi ptr [ %result_inner.sroa.0.1.i136, %"listiterator<int>::next.exit194" ], [ %result_inner.sroa.0.1.i90, %"listiterator<int>::next.exit178" ]
  %self.0.1.load.i.i96 = phi i64 [ %i_add.i123, %"listiterator<int>::next.exit194" ], [ %i_add.i72, %"listiterator<int>::next.exit178" ]
  %27 = phi %"option<char>" [ %if_result.i193, %"listiterator<int>::next.exit194" ], [ %if_result.i177, %"listiterator<int>::next.exit178" ]
  %28 = extractvalue %"option<char>" %27, 1
  %29 = load i64, ptr %28, align 4
  %30 = alloca [2 x i8], align 2
  store i8 44, ptr %30, align 2
  %.repack39 = getelementptr inbounds [2 x i8], ptr %30, i64 0, i64 1
  store i8 32, ptr %.repack39, align 1
  %31 = call dereferenceable_or_null(2) ptr @malloc(i64 2)
  %32 = load i16, ptr %30, align 2
  store i16 %32, ptr %31, align 1
  %i_add.i99 = add i64 %self.0.1.load.i.i96, 2
  %33 = call ptr @malloc(i64 %i_add.i99)
  %i_lt.i.i21.i101 = icmp sgt i64 %self.0.1.load.i.i96, 9223372036854775805
  br i1 %i_lt.i.i21.i101, label %"#dynarray::extend.exit32.i107", label %"#dynarray::extend.exit32.thread.i103"

"#dynarray::extend.exit32.thread.i103":           ; preds = %for
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %33, ptr align 1 %self.0.0.load.i100, i64 %self.0.1.load.i.i96, i1 false)
  br label %"string::add_string.exit115"

"#dynarray::extend.exit32.i107":                  ; preds = %for
  %i_mul.i.i22.i104 = shl i64 %i_add.i99, 1
  %34 = call i64 @llvm.smax.i64(i64 %self.0.1.load.i.i96, i64 %i_mul.i.i22.i104)
  %35 = call ptr @malloc(i64 %34)
  call void @free(ptr %33)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %35, ptr align 1 %self.0.0.load.i100, i64 %self.0.1.load.i.i96, i1 false)
  %i_lt.i.i.i106.not = icmp ult i64 %34, %i_add.i99
  br i1 %i_lt.i.i.i106.not, label %"string::add_string.exit115", label %then.i.i.i109

then.i.i.i109:                                    ; preds = %"#dynarray::extend.exit32.i107"
  %i_mul.i.i.i108 = shl nuw i64 %34, 1
  %36 = call i64 @llvm.smax.i64(i64 %i_add.i99, i64 %i_mul.i.i.i108)
  %37 = call ptr @malloc(i64 %36)
  call void @llvm.memmove.p0.p0.i64(ptr align 1 %37, ptr align 1 %self.0.0.load.i100, i64 %self.0.1.load.i.i96, i1 false)
  call void @free(ptr %35)
  br label %"string::add_string.exit115"

"string::add_string.exit115":                     ; preds = %"#dynarray::extend.exit32.thread.i103", %"#dynarray::extend.exit32.i107", %then.i.i.i109
  %result_inner.sroa.0.1.i112 = phi ptr [ %37, %then.i.i.i109 ], [ %35, %"#dynarray::extend.exit32.i107" ], [ %33, %"#dynarray::extend.exit32.thread.i103" ]
  %self.0.load.gep.i.i113 = getelementptr i8, ptr %result_inner.sroa.0.1.i112, i64 %self.0.1.load.i.i96
  %38 = load i16, ptr %31, align 1
  store i16 %38, ptr %self.0.load.gep.i.i113, align 1
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %alloca.ptr.i116)
  %39 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %alloca.ptr.i116, ptr nonnull @_tmpl_int_to_string, i64 %29)
  %buf_ptr4.deref.i117 = load ptr, ptr %alloca.ptr.i116, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %alloca.ptr.i116)
  %i_add.i123 = add i64 %39, %i_add.i99
  %40 = call ptr @malloc(i64 %i_add.i123)
  %i_lt.i.i21.i125 = icmp slt i64 %i_add.i123, %i_add.i99
  br i1 %i_lt.i.i21.i125, label %"#dynarray::extend.exit32.i131", label %"#dynarray::extend.exit32.thread.i127"

"#dynarray::extend.exit32.thread.i127":           ; preds = %"string::add_string.exit115"
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %40, ptr nonnull align 1 %result_inner.sroa.0.1.i112, i64 %i_add.i99, i1 false)
  br label %"string::add_string.exit139"

"#dynarray::extend.exit32.i131":                  ; preds = %"string::add_string.exit115"
  %i_mul.i.i22.i128 = shl i64 %i_add.i123, 1
  %41 = call i64 @llvm.smax.i64(i64 %i_add.i99, i64 %i_mul.i.i22.i128)
  %42 = call ptr @malloc(i64 %41)
  call void @free(ptr %40)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %42, ptr nonnull align 1 %result_inner.sroa.0.1.i112, i64 %i_add.i99, i1 false)
  %i_lt.i.i.i130 = icmp slt i64 %41, %i_add.i123
  br i1 %i_lt.i.i.i130, label %then.i.i.i133, label %"string::add_string.exit139"

then.i.i.i133:                                    ; preds = %"#dynarray::extend.exit32.i131"
  %i_mul.i.i.i132 = shl i64 %41, 1
  %43 = call i64 @llvm.smax.i64(i64 %i_add.i123, i64 %i_mul.i.i.i132)
  %44 = call ptr @malloc(i64 %43)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %44, ptr nonnull align 1 %result_inner.sroa.0.1.i112, i64 %i_add.i99, i1 false)
  call void @free(ptr %42)
  br label %"string::add_string.exit139"

"string::add_string.exit139":                     ; preds = %"#dynarray::extend.exit32.thread.i127", %"#dynarray::extend.exit32.i131", %then.i.i.i133
  %result_inner.sroa.0.1.i136 = phi ptr [ %44, %then.i.i.i133 ], [ %42, %"#dynarray::extend.exit32.i131" ], [ %40, %"#dynarray::extend.exit32.thread.i127" ]
  %self.0.load.gep.i.i137 = getelementptr i8, ptr %result_inner.sroa.0.1.i136, i64 %i_add.i99
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self.0.load.gep.i.i137, ptr align 1 %buf_ptr4.deref.i117, i64 %39, i1 false)
  %self.1.i179 = getelementptr inbounds %"listiterator<int>", ptr %it, i64 0, i32 1
  %self.1.load.i180 = load i64, ptr %self.1.i179, align 4
  %self.0.1.i.i181 = getelementptr inbounds %string, ptr %it, i64 0, i32 0, i32 1
  %self.0.1.load.i.i182 = load i64, ptr %self.0.1.i.i181, align 4
  %45 = sdiv i64 %self.0.1.load.i.i182, 8
  %i_ge.not.i183 = icmp slt i64 %self.1.load.i180, %45
  br i1 %i_ge.not.i183, label %else.i185, label %"listiterator<int>::next.exit194"

else.i185:                                        ; preds = %"string::add_string.exit139"
  %i_le.i.i184 = icmp sgt i64 %self.1.load.i180, -1
  br i1 %i_le.i.i184, label %"list<int>::get.exit.i192", label %else.i.i187

else.i.i187:                                      ; preds = %else.i185
  %stderr.i.i186 = call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %46 = call i64 @fwrite(ptr nonnull @throw_msg.4.9, i64 25, i64 1, ptr %stderr.i.i186)
  %47 = call i64 @fputwc(i32 10, ptr %stderr.i.i186)
  call void @exit(i64 1)
  unreachable

"list<int>::get.exit.i192":                       ; preds = %else.i185
  %self.0.0.load.i.i188 = load ptr, ptr %it, align 8
  %self.0.0.load.gep.i.i189 = getelementptr i64, ptr %self.0.0.load.i.i188, i64 %self.1.load.i180
  %self.0.0.load.gep.deref.i.i190 = load i64, ptr %self.0.0.load.gep.i.i189, align 4
  %48 = call dereferenceable_or_null(8) ptr @malloc(i64 8)
  store i64 %self.0.0.load.gep.deref.i.i190, ptr %48, align 4
  %49 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %48, 1
  %i_add.i191 = add nuw nsw i64 %self.1.load.i180, 1
  store i64 %i_add.i191, ptr %self.1.i179, align 4
  br label %"listiterator<int>::next.exit194"

"listiterator<int>::next.exit194":                ; preds = %"string::add_string.exit139", %"list<int>::get.exit.i192"
  %if_result.i193 = phi %"option<char>" [ %49, %"list<int>::get.exit.i192" ], [ zeroinitializer, %"string::add_string.exit139" ]
  %present = extractvalue %"option<char>" %if_result.i193, 0
  br i1 %present, label %for, label %block6

block6:                                           ; preds = %"listiterator<int>::next.exit194", %"listiterator<int>::next.exit178"
  %self.0.0.load.i = phi ptr [ %result_inner.sroa.0.1.i90, %"listiterator<int>::next.exit178" ], [ %result_inner.sroa.0.1.i136, %"listiterator<int>::next.exit194" ]
  %self.0.1.load.i.i = phi i64 [ %i_add.i72, %"listiterator<int>::next.exit178" ], [ %i_add.i123, %"listiterator<int>::next.exit194" ]
  %50 = call dereferenceable_or_null(1) ptr @malloc(i64 1)
  store i8 93, ptr %50, align 1
  %i_add.i = add i64 %self.0.1.load.i.i, 1
  %51 = call ptr @malloc(i64 %i_add.i)
  %i_lt.i.i21.i = icmp eq i64 %self.0.1.load.i.i, 9223372036854775807
  br i1 %i_lt.i.i21.i, label %"#dynarray::extend.exit32.i", label %"#dynarray::extend.exit32.thread.i"

"#dynarray::extend.exit32.thread.i":              ; preds = %block6
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %51, ptr align 1 %self.0.0.load.i, i64 %self.0.1.load.i.i, i1 false)
  br label %"string::add_string.exit"

"#dynarray::extend.exit32.i":                     ; preds = %block6
  %i_mul.i.i22.i = shl i64 %i_add.i, 1
  %52 = call i64 @llvm.smax.i64(i64 %self.0.1.load.i.i, i64 %i_mul.i.i22.i)
  %53 = call ptr @malloc(i64 %52)
  call void @free(ptr %51)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %53, ptr align 1 %self.0.0.load.i, i64 %self.0.1.load.i.i, i1 false)
  %i_lt.i.i.i.not = icmp ult i64 %52, %i_add.i
  br i1 %i_lt.i.i.i.not, label %"string::add_string.exit", label %then.i.i.i

then.i.i.i:                                       ; preds = %"#dynarray::extend.exit32.i"
  %i_mul.i.i.i = shl nuw i64 %52, 1
  %54 = call i64 @llvm.smax.i64(i64 %i_add.i, i64 %i_mul.i.i.i)
  %55 = call ptr @malloc(i64 %54)
  call void @llvm.memmove.p0.p0.i64(ptr align 1 %55, ptr align 1 %self.0.0.load.i, i64 %self.0.1.load.i.i, i1 false)
  call void @free(ptr %53)
  br label %"string::add_string.exit"

"string::add_string.exit":                        ; preds = %"#dynarray::extend.exit32.thread.i", %"#dynarray::extend.exit32.i", %then.i.i.i
  %result_inner.sroa.15.1.i = phi i64 [ -2, %then.i.i.i ], [ 9223372036854775807, %"#dynarray::extend.exit32.i" ], [ %i_add.i, %"#dynarray::extend.exit32.thread.i" ]
  %result_inner.sroa.0.1.i = phi ptr [ %55, %then.i.i.i ], [ %53, %"#dynarray::extend.exit32.i" ], [ %51, %"#dynarray::extend.exit32.thread.i" ]
  %self.0.load.gep.i.i = getelementptr i8, ptr %result_inner.sroa.0.1.i, i64 %self.0.1.load.i.i
  %56 = load i8, ptr %50, align 1
  store i8 %56, ptr %self.0.load.gep.i.i, align 1
  br label %merge
}

define void @"list<int>::set"(ptr nocapture readonly %self, i64 %i, i64 %t) local_unnamed_addr {
body:
  %i_le = icmp sgt i64 %i, -1
  br i1 %i_le, label %and_true, label %else

else:                                             ; preds = %body, %and_true
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.4.9, i64 25, i64 1, ptr %stderr)
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
  %self.0.0.load.gep = getelementptr i64, ptr %self.0.0.load, i64 %i
  store i64 %t, ptr %self.0.0.load.gep, align 4
  ret void
}

define void @"list<int>::extend"(ptr nocapture %self, ptr nocapture readonly %lst) local_unnamed_addr {
body:
  %inner1.unpack.unpack.i.i = load ptr, ptr %lst, align 8
  %inner1.unpack.elt2.i.i = getelementptr inbounds %"#dynarray", ptr %lst, i64 0, i32 1
  %inner1.unpack.unpack3.i.i = load i64, ptr %inner1.unpack.elt2.i.i, align 8
  %inner1.unpack.elt4.i.i = getelementptr inbounds %"#dynarray", ptr %lst, i64 0, i32 2
  %inner1.unpack.unpack5.i.i = load i64, ptr %inner1.unpack.elt4.i.i, align 8
  %0 = alloca %"listiterator<int>", align 8
  store ptr %inner1.unpack.unpack.i.i, ptr %0, align 8
  %.repack4 = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 1
  store i64 %inner1.unpack.unpack3.i.i, ptr %.repack4, align 8
  %.repack6 = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 2
  store i64 %inner1.unpack.unpack5.i.i, ptr %.repack6, align 8
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
  %2 = call i64 @fwrite(ptr nonnull @throw_msg.4.9, i64 25, i64 1, ptr %stderr.i.i)
  %3 = call i64 @fputwc(i32 10, ptr %stderr.i.i)
  call void @exit(i64 1)
  unreachable

"list<int>::get.exit.i":                          ; preds = %else.i
  %self.0.0.load.i.i = load ptr, ptr %0, align 8
  %self.0.0.load.gep.i.i = getelementptr i64, ptr %self.0.0.load.i.i, i64 %self.1.load.i
  %self.0.0.load.gep.deref.i.i = load i64, ptr %self.0.0.load.gep.i.i, align 4
  %4 = call dereferenceable_or_null(8) ptr @malloc(i64 8)
  store i64 %self.0.0.load.gep.deref.i.i, ptr %4, align 4
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

for:                                              ; preds = %for.preheader, %"listiterator<int>::next.exit24"
  %6 = phi %"option<char>" [ %if_result.i23, %"listiterator<int>::next.exit24" ], [ %if_result.i, %for.preheader ]
  %7 = extractvalue %"option<char>" %6, 1
  %8 = load i64, ptr %7, align 4
  %self.1.load.i.i = load i64, ptr %self.1.i.i, align 4
  %i_add.i.i = add i64 %self.1.load.i.i, 8
  %self.2.load.i.i.i = load i64, ptr %self.2.i.i.i, align 4
  %i_lt.i.i.i = icmp slt i64 %self.2.load.i.i.i, %i_add.i.i
  br i1 %i_lt.i.i.i, label %then.i.i.i, label %"list<int>::push.exit"

then.i.i.i:                                       ; preds = %for
  %i_mul.i.i.i = shl i64 %self.2.load.i.i.i, 1
  %9 = tail call i64 @llvm.smax.i64(i64 %i_add.i.i, i64 %i_mul.i.i.i)
  %self.0.load.i.i.i = load ptr, ptr %self, align 8
  %10 = tail call ptr @malloc(i64 %9)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %10, ptr align 1 %self.0.load.i.i.i, i64 %self.1.load.i.i, i1 false)
  store ptr %10, ptr %self, align 8
  store i64 %9, ptr %self.2.i.i.i, align 4
  tail call void @free(ptr %self.0.load.i.i.i)
  %self.14.load.pre.i.i = load i64, ptr %self.1.i.i, align 4
  br label %"list<int>::push.exit"

"list<int>::push.exit":                           ; preds = %for, %then.i.i.i
  %self.14.load.i.i = phi i64 [ %self.14.load.pre.i.i, %then.i.i.i ], [ %self.1.load.i.i, %for ]
  %self.0.load.i.i = load ptr, ptr %self, align 8
  %self.0.load.gep.i.i = getelementptr i8, ptr %self.0.load.i.i, i64 %self.14.load.i.i
  store i64 %8, ptr %self.0.load.gep.i.i, align 1
  %self.18.load.i.i = load i64, ptr %self.1.i.i, align 4
  %i_add10.i.i = add i64 %self.18.load.i.i, 8
  store i64 %i_add10.i.i, ptr %self.1.i.i, align 4
  %self.1.i9 = getelementptr inbounds %"listiterator<int>", ptr %0, i64 0, i32 1
  %self.1.load.i10 = load i64, ptr %self.1.i9, align 4
  %self.0.1.i.i11 = getelementptr inbounds %string, ptr %0, i64 0, i32 0, i32 1
  %self.0.1.load.i.i12 = load i64, ptr %self.0.1.i.i11, align 4
  %11 = sdiv i64 %self.0.1.load.i.i12, 8
  %i_ge.not.i13 = icmp slt i64 %self.1.load.i10, %11
  br i1 %i_ge.not.i13, label %else.i15, label %"listiterator<int>::next.exit24"

else.i15:                                         ; preds = %"list<int>::push.exit"
  %i_le.i.i14 = icmp sgt i64 %self.1.load.i10, -1
  br i1 %i_le.i.i14, label %"list<int>::get.exit.i22", label %else.i.i17

else.i.i17:                                       ; preds = %else.i15
  %stderr.i.i16 = call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %12 = call i64 @fwrite(ptr nonnull @throw_msg.4.9, i64 25, i64 1, ptr %stderr.i.i16)
  %13 = call i64 @fputwc(i32 10, ptr %stderr.i.i16)
  call void @exit(i64 1)
  unreachable

"list<int>::get.exit.i22":                        ; preds = %else.i15
  %self.0.0.load.i.i18 = load ptr, ptr %0, align 8
  %self.0.0.load.gep.i.i19 = getelementptr i64, ptr %self.0.0.load.i.i18, i64 %self.1.load.i10
  %self.0.0.load.gep.deref.i.i20 = load i64, ptr %self.0.0.load.gep.i.i19, align 4
  %14 = call dereferenceable_or_null(8) ptr @malloc(i64 8)
  store i64 %self.0.0.load.gep.deref.i.i20, ptr %14, align 4
  %15 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %14, 1
  %i_add.i21 = add nuw nsw i64 %self.1.load.i10, 1
  store i64 %i_add.i21, ptr %self.1.i9, align 4
  br label %"listiterator<int>::next.exit24"

"listiterator<int>::next.exit24":                 ; preds = %"list<int>::push.exit", %"list<int>::get.exit.i22"
  %if_result.i23 = phi %"option<char>" [ %15, %"list<int>::get.exit.i22" ], [ zeroinitializer, %"list<int>::push.exit" ]
  %present = extractvalue %"option<char>" %if_result.i23, 0
  br i1 %present, label %for, label %post_for

post_for:                                         ; preds = %"listiterator<int>::next.exit24", %"listiterator<int>::next.exit"
  ret void
}

; Function Attrs: mustprogress nounwind willreturn
define void @"list<int>::push"(ptr nocapture %self, i64 %e) local_unnamed_addr #8 {
body:
  %self.1.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %self.1.load.i = load i64, ptr %self.1.i, align 4
  %i_add.i = add i64 %self.1.load.i, 8
  %self.2.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %self.2.load.i.i = load i64, ptr %self.2.i.i, align 4
  %i_lt.i.i = icmp slt i64 %self.2.load.i.i, %i_add.i
  br i1 %i_lt.i.i, label %then.i.i, label %"#dynarray::extend.exit"

then.i.i:                                         ; preds = %body
  %i_mul.i.i = shl i64 %self.2.load.i.i, 1
  %0 = tail call i64 @llvm.smax.i64(i64 %i_add.i, i64 %i_mul.i.i)
  %self.0.load.i.i = load ptr, ptr %self, align 8
  %1 = tail call ptr @malloc(i64 %0)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %1, ptr align 1 %self.0.load.i.i, i64 %self.1.load.i, i1 false)
  store ptr %1, ptr %self, align 8
  store i64 %0, ptr %self.2.i.i, align 4
  tail call void @free(ptr %self.0.load.i.i)
  %self.14.load.pre.i = load i64, ptr %self.1.i, align 4
  br label %"#dynarray::extend.exit"

"#dynarray::extend.exit":                         ; preds = %body, %then.i.i
  %self.14.load.i = phi i64 [ %self.14.load.pre.i, %then.i.i ], [ %self.1.load.i, %body ]
  %self.0.load.i = load ptr, ptr %self, align 8
  %self.0.load.gep.i = getelementptr i8, ptr %self.0.load.i, i64 %self.14.load.i
  store i64 %e, ptr %self.0.load.gep.i, align 1
  %self.18.load.i = load i64, ptr %self.1.i, align 4
  %i_add10.i = add i64 %self.18.load.i, 8
  store i64 %i_add10.i, ptr %self.1.i, align 4
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
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.2, i64 30, i64 1, ptr %stderr.i)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr.i)
  tail call void @exit(i64 1)
  unreachable

"#dynarray::take.exit":                           ; preds = %body
  %i_sub.i = add nsw i64 %self.1.load.i, -8
  store i64 %i_sub.i, ptr %self.1.i, align 4
  %self.0.load.i = load ptr, ptr %self, align 8
  %self.0.load.gep.i = getelementptr i8, ptr %self.0.load.i, i64 %i_sub.i
  %.deref = load i64, ptr %self.0.load.gep.i, align 4
  ret i64 %.deref
}

define %"option<char>" @"option<option<int>>::get"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %self.0.load = load i1, ptr %self, align 1
  br i1 %self.0.load, label %merge, label %else

else:                                             ; preds = %body
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.5.10, i64 16, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

merge:                                            ; preds = %body
  %self.1 = getelementptr inbounds %"option<char>", ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  %self.1.load.deref = load %"option<char>", ptr %self.1.load, align 8
  ret %"option<char>" %self.1.load.deref
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %string @"list<int>::from_raw"(ptr nocapture readonly %contents, i64 %len) local_unnamed_addr #3 {
body:
  %i_mul = shl i64 %len, 3
  %0 = tail call ptr @malloc(i64 %i_mul)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %0, ptr align 1 %contents, i64 %i_mul, i1 false)
  %1 = insertvalue %"#dynarray" undef, ptr %0, 0
  %2 = insertvalue %"#dynarray" %1, i64 %i_mul, 1
  %inner79 = insertvalue %"#dynarray" %2, i64 %i_mul, 2
  %3 = insertvalue %string zeroinitializer, %"#dynarray" %inner79, 0
  ret %string %3
}

define i8 @main() local_unnamed_addr {
body:
  %z.i = alloca %"option<char>", align 8
  %0 = tail call ptr @setlocale(i64 0, ptr nonnull @locale.11)
  call void @llvm.lifetime.start.p0(i64 16, ptr nonnull %z.i)
  %1 = tail call dereferenceable_or_null(8) ptr @malloc(i64 8)
  store i64 2, ptr %1, align 4
  %2 = tail call dereferenceable_or_null(16) ptr @malloc(i64 16)
  store i1 true, ptr %2, align 8
  %.sroa.49.0..sroa_idx = getelementptr inbounds i8, ptr %2, i64 8
  store ptr %1, ptr %.sroa.49.0..sroa_idx, align 8
  %3 = tail call dereferenceable_or_null(16) ptr @malloc(i64 16)
  store i1 true, ptr %3, align 8
  %.sroa.46.0..sroa_idx = getelementptr inbounds i8, ptr %3, i64 8
  store ptr %2, ptr %.sroa.46.0..sroa_idx, align 8
  %4 = tail call dereferenceable_or_null(16) ptr @malloc(i64 16)
  store i1 true, ptr %4, align 8
  %.sroa.43.0..sroa_idx = getelementptr inbounds i8, ptr %4, i64 8
  store ptr %3, ptr %.sroa.43.0..sroa_idx, align 8
  store i1 true, ptr %z.i, align 8
  %.fca.1.gep16.i = getelementptr inbounds %"option<char>", ptr %z.i, i64 0, i32 1
  store ptr %4, ptr %.fca.1.gep16.i, align 8
  %5 = call %string @"option<option<option<option<int>>>>::to_string"(ptr nonnull %z.i)
  %6 = extractvalue %string %5, 0
  %.elt.i = extractvalue %"#dynarray" %6, 0
  %.elt2.i = extractvalue %"#dynarray" %6, 1
  %7 = tail call i64 (ptr, ...) @printf(ptr nonnull @_tmpl_print, i64 %.elt2.i, ptr %.elt.i)
  call void @llvm.lifetime.end.p0(i64 16, ptr nonnull %z.i)
  ret i8 0
}

; Function Attrs: argmemonly mustprogress nocallback nofree nounwind willreturn writeonly
declare void @llvm.memset.p0.i64(ptr nocapture writeonly, i8, i64, i1 immarg) #12

; Function Attrs: argmemonly mustprogress nocallback nofree nounwind willreturn
declare void @llvm.memmove.p0.p0.i64(ptr nocapture writeonly, ptr nocapture readonly, i64, i1 immarg) #9

attributes #0 = { nofree nounwind }
attributes #1 = { inaccessiblememonly mustprogress nofree nounwind willreturn allockind("alloc,uninitialized") allocsize(0) "alloc-family"="malloc" }
attributes #2 = { mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #3 = { mustprogress nofree nounwind willreturn }
attributes #4 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn }
attributes #5 = { mustprogress nofree norecurse nosync nounwind readnone willreturn }
attributes #6 = { mustprogress nofree nosync nounwind readnone willreturn }
attributes #7 = { argmemonly mustprogress nocallback nofree nosync nounwind willreturn }
attributes #8 = { mustprogress nounwind willreturn }
attributes #9 = { argmemonly mustprogress nocallback nofree nounwind willreturn }
attributes #10 = { inaccessiblemem_or_argmemonly mustprogress nounwind willreturn allockind("free") "alloc-family"="malloc" }
attributes #11 = { mustprogress nofree nounwind willreturn writeonly }
attributes #12 = { argmemonly mustprogress nocallback nofree nounwind willreturn writeonly }

!0 = !{i64 0, i64 65}
