; ModuleID = 'frq3.gon'
source_filename = "frq3.gon"

%string = type { %"#dynarray" }
%"#dynarray" = type { ptr, i64, i64 }
%string_chars = type { %string, ptr }
%"option<char>" = type { i1, ptr }
%Review = type { i64, %string }
%"listiterator<string>" = type { %string, i64 }
%ReviewAnalysis = type { %string }

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
@throw_msg.4.10 = private unnamed_addr constant [26 x i8] c"array index out of bounds\00", align 1
@throw_msg.5.9 = private unnamed_addr constant [17 x i8] c"no value present\00", align 1
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
  %1 = tail call i64 @fwrite(ptr nonnull @throw_msg.4.10, i64 25, i64 1, ptr %stderr)
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
  %6 = tail call i64 @fwrite(ptr nonnull @throw_msg.5.9, i64 16, i64 1, ptr %stderr.i)
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
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.5.9, i64 16, i64 1, ptr %stderr)
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

define %string @"Review::to_string"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %alloca.ptr.i = alloca ptr, align 8
  %self.1 = getelementptr inbounds %Review, ptr %self, i64 0, i32 1
  %0 = tail call dereferenceable_or_null(2) ptr @malloc(i64 2)
  store i16 10272, ptr %0, align 1
  %1 = alloca %string, align 8
  store ptr %0, ptr %1, align 8
  %.repack2 = getelementptr inbounds %"#dynarray", ptr %1, i64 0, i32 1
  store i64 2, ptr %.repack2, align 8
  %.repack4 = getelementptr inbounds %"#dynarray", ptr %1, i64 0, i32 2
  store i64 2, ptr %.repack4, align 8
  %2 = call %string @"string::add_string"(ptr nonnull %self.1, ptr nonnull %1)
  %3 = extractvalue %string %2, 0
  %.elt6 = extractvalue %"#dynarray" %3, 0
  %.elt8 = extractvalue %"#dynarray" %3, 1
  %self.0.load = load i64, ptr %self, align 4
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %alloca.ptr.i)
  %4 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %alloca.ptr.i, ptr nonnull @_tmpl_int_to_string, i64 %self.0.load)
  %buf_ptr4.deref.i = load ptr, ptr %alloca.ptr.i, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %alloca.ptr.i)
  %i_add.i26 = add i64 %4, %.elt8
  %5 = call ptr @malloc(i64 %i_add.i26)
  %i_lt.i.i21.i = icmp slt i64 %i_add.i26, %.elt8
  br i1 %i_lt.i.i21.i, label %"#dynarray::extend.exit32.i", label %"#dynarray::extend.exit32.thread.i"

"#dynarray::extend.exit32.thread.i":              ; preds = %body
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %5, ptr align 1 %.elt6, i64 %.elt8, i1 false)
  br label %"string::add_string.exit"

"#dynarray::extend.exit32.i":                     ; preds = %body
  %i_mul.i.i22.i = shl i64 %i_add.i26, 1
  %6 = call i64 @llvm.smax.i64(i64 %.elt8, i64 %i_mul.i.i22.i)
  %7 = call ptr @malloc(i64 %6)
  call void @free(ptr %5)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %7, ptr align 1 %.elt6, i64 %.elt8, i1 false)
  %i_lt.i.i.i = icmp slt i64 %6, %i_add.i26
  br i1 %i_lt.i.i.i, label %then.i.i.i, label %"string::add_string.exit"

then.i.i.i:                                       ; preds = %"#dynarray::extend.exit32.i"
  %i_mul.i.i.i = shl i64 %6, 1
  %8 = call i64 @llvm.smax.i64(i64 %i_add.i26, i64 %i_mul.i.i.i)
  %9 = call ptr @malloc(i64 %8)
  call void @llvm.memmove.p0.p0.i64(ptr align 1 %9, ptr align 1 %.elt6, i64 %.elt8, i1 false)
  call void @free(ptr %7)
  br label %"string::add_string.exit"

"string::add_string.exit":                        ; preds = %"#dynarray::extend.exit32.thread.i", %"#dynarray::extend.exit32.i", %then.i.i.i
  %result_inner.sroa.0.1.i = phi ptr [ %9, %then.i.i.i ], [ %7, %"#dynarray::extend.exit32.i" ], [ %5, %"#dynarray::extend.exit32.thread.i" ]
  %self.0.load.gep.i.i = getelementptr i8, ptr %result_inner.sroa.0.1.i, i64 %.elt8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self.0.load.gep.i.i, ptr align 1 %buf_ptr4.deref.i, i64 %4, i1 false)
  %10 = call dereferenceable_or_null(1) ptr @malloc(i64 1)
  store i8 41, ptr %10, align 1
  %i_add.i32 = add i64 %i_add.i26, 1
  %11 = call ptr @malloc(i64 %i_add.i32)
  %i_lt.i.i21.i34 = icmp eq i64 %i_add.i26, 9223372036854775807
  br i1 %i_lt.i.i21.i34, label %"#dynarray::extend.exit32.i40", label %"#dynarray::extend.exit32.thread.i36"

"#dynarray::extend.exit32.thread.i36":            ; preds = %"string::add_string.exit"
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %11, ptr align 1 %result_inner.sroa.0.1.i, i64 %i_add.i26, i1 false)
  br label %"string::add_string.exit48"

"#dynarray::extend.exit32.i40":                   ; preds = %"string::add_string.exit"
  %i_mul.i.i22.i37 = shl i64 %i_add.i32, 1
  %12 = call i64 @llvm.smax.i64(i64 %i_add.i26, i64 %i_mul.i.i22.i37)
  %13 = call ptr @malloc(i64 %12)
  call void @free(ptr %11)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %13, ptr align 1 %result_inner.sroa.0.1.i, i64 %i_add.i26, i1 false)
  %i_lt.i.i.i39.not = icmp ult i64 %12, %i_add.i32
  br i1 %i_lt.i.i.i39.not, label %"string::add_string.exit48", label %then.i.i.i42

then.i.i.i42:                                     ; preds = %"#dynarray::extend.exit32.i40"
  %i_mul.i.i.i41 = shl nuw i64 %12, 1
  %14 = call i64 @llvm.smax.i64(i64 %i_add.i32, i64 %i_mul.i.i.i41)
  %15 = call ptr @malloc(i64 %14)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %15, ptr align 1 %result_inner.sroa.0.1.i, i64 %i_add.i26, i1 false)
  call void @free(ptr %13)
  br label %"string::add_string.exit48"

"string::add_string.exit48":                      ; preds = %"#dynarray::extend.exit32.thread.i36", %"#dynarray::extend.exit32.i40", %then.i.i.i42
  %result_inner.sroa.15.1.i44 = phi i64 [ -2, %then.i.i.i42 ], [ 9223372036854775807, %"#dynarray::extend.exit32.i40" ], [ %i_add.i32, %"#dynarray::extend.exit32.thread.i36" ]
  %result_inner.sroa.0.1.i45 = phi ptr [ %15, %then.i.i.i42 ], [ %13, %"#dynarray::extend.exit32.i40" ], [ %11, %"#dynarray::extend.exit32.thread.i36" ]
  %self.0.load.gep.i.i46 = getelementptr i8, ptr %result_inner.sroa.0.1.i45, i64 %i_add.i26
  %16 = load i8, ptr %10, align 1
  store i8 %16, ptr %self.0.load.gep.i.i46, align 1
  %17 = insertvalue %"#dynarray" undef, ptr %result_inner.sroa.0.1.i45, 0
  %18 = insertvalue %"#dynarray" %17, i64 %i_add.i32, 1
  %result_inner29.i47 = insertvalue %"#dynarray" %18, i64 %result_inner.sroa.15.1.i44, 2
  %19 = insertvalue %string zeroinitializer, %"#dynarray" %result_inner29.i47, 0
  ret %string %19
}

define %string @"list<string>::to_string"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %self.0.1.load.i.off = add i64 %self.0.1.load.i, 23
  %0 = icmp ult i64 %self.0.1.load.i.off, 47
  br i1 %0, label %then, label %else

then:                                             ; preds = %body
  %1 = alloca [2 x i8], align 2
  store i8 91, ptr %1, align 2
  %.repack64 = getelementptr inbounds [2 x i8], ptr %1, i64 0, i64 1
  store i8 93, ptr %.repack64, align 1
  %2 = tail call dereferenceable_or_null(2) ptr @malloc(i64 2)
  %3 = load i16, ptr %1, align 2
  store i16 %3, ptr %2, align 1
  br label %merge

else:                                             ; preds = %body
  %4 = tail call dereferenceable_or_null(1) ptr @malloc(i64 1)
  store i8 91, ptr %4, align 1
  %it = alloca %"listiterator<string>", align 8
  %inner1.unpack.unpack.i.i = load ptr, ptr %self, align 8
  %inner1.unpack.elt4.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %inner1.unpack.unpack5.i.i = load i64, ptr %inner1.unpack.elt4.i.i, align 8
  store ptr %inner1.unpack.unpack.i.i, ptr %it, align 8
  %it.repack9 = getelementptr inbounds %"#dynarray", ptr %it, i64 0, i32 1
  store i64 %self.0.1.load.i, ptr %it.repack9, align 8
  %it.repack11 = getelementptr inbounds %"#dynarray", ptr %it, i64 0, i32 2
  store i64 %inner1.unpack.unpack5.i.i, ptr %it.repack11, align 8
  %it.repack6 = getelementptr inbounds %"listiterator<string>", ptr %it, i64 0, i32 1
  store i64 0, ptr %it.repack6, align 8
  %5 = call %"option<char>" @"listiterator<string>::next"(ptr nonnull %it)
  %6 = alloca %"option<char>", align 8
  store %"option<char>" %5, ptr %6, align 8
  %self.0.load.i = load i1, ptr %6, align 8
  br i1 %self.0.load.i, label %"option<string>::get.exit", label %else.i

else.i:                                           ; preds = %else
  %stderr.i = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %7 = tail call i64 @fwrite(ptr nonnull @throw_msg.5.9, i64 16, i64 1, ptr %stderr.i)
  %8 = tail call i64 @fputwc(i32 10, ptr %stderr.i)
  tail call void @exit(i64 1)
  unreachable

"option<string>::get.exit":                       ; preds = %else
  %self.1.i = getelementptr inbounds %"option<char>", ptr %6, i64 0, i32 1
  %self.1.load.i = load ptr, ptr %self.1.i, align 8
  %self.1.load.deref.unpack.unpack.i = load ptr, ptr %self.1.load.i, align 8
  %self.1.load.deref.unpack.elt2.i = getelementptr inbounds %"#dynarray", ptr %self.1.load.i, i64 0, i32 1
  %self.1.load.deref.unpack.unpack3.i = load i64, ptr %self.1.load.deref.unpack.elt2.i, align 8
  %i_add.i71 = add i64 %self.1.load.deref.unpack.unpack3.i, 1
  %9 = tail call ptr @malloc(i64 %i_add.i71)
  %i_lt.i.i21.i73 = icmp ugt i64 %self.1.load.deref.unpack.unpack3.i, 9223372036854775806
  br i1 %i_lt.i.i21.i73, label %"#dynarray::extend.exit32.i79", label %"#dynarray::extend.exit32.thread.i75"

"#dynarray::extend.exit32.thread.i75":            ; preds = %"option<string>::get.exit"
  %10 = load i8, ptr %4, align 1
  store i8 %10, ptr %9, align 1
  br label %"string::add_string.exit87"

"#dynarray::extend.exit32.i79":                   ; preds = %"option<string>::get.exit"
  %i_mul.i.i22.i76 = shl i64 %i_add.i71, 1
  %11 = tail call i64 @llvm.smax.i64(i64 %i_mul.i.i22.i76, i64 1)
  %12 = tail call ptr @malloc(i64 %11)
  tail call void @free(ptr %9)
  %13 = load i8, ptr %4, align 1
  store i8 %13, ptr %12, align 1
  %i_lt.i.i.i78 = icmp slt i64 %11, %i_add.i71
  br i1 %i_lt.i.i.i78, label %then.i.i.i81, label %"string::add_string.exit87"

then.i.i.i81:                                     ; preds = %"#dynarray::extend.exit32.i79"
  %i_mul.i.i.i80 = shl nuw i64 %11, 1
  %14 = tail call i64 @llvm.smax.i64(i64 %i_add.i71, i64 %i_mul.i.i.i80)
  %15 = tail call ptr @malloc(i64 %14)
  %16 = load i8, ptr %4, align 1
  store i8 %16, ptr %15, align 1
  tail call void @free(ptr nonnull %12)
  br label %"string::add_string.exit87"

"string::add_string.exit87":                      ; preds = %"#dynarray::extend.exit32.thread.i75", %"#dynarray::extend.exit32.i79", %then.i.i.i81
  %result_inner.sroa.0.1.i84 = phi ptr [ %15, %then.i.i.i81 ], [ %12, %"#dynarray::extend.exit32.i79" ], [ %9, %"#dynarray::extend.exit32.thread.i75" ]
  %self.0.load.gep.i.i85 = getelementptr i8, ptr %result_inner.sroa.0.1.i84, i64 1
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self.0.load.gep.i.i85, ptr align 1 %self.1.load.deref.unpack.unpack.i, i64 %self.1.load.deref.unpack.unpack3.i, i1 false)
  %17 = call %"option<char>" @"listiterator<string>::next"(ptr nonnull %it)
  %present65 = extractvalue %"option<char>" %17, 0
  br i1 %present65, label %for, label %block5

merge:                                            ; preds = %"string::add_string.exit", %then
  %.pn142 = phi ptr [ %2, %then ], [ %result_inner.sroa.0.1.i, %"string::add_string.exit" ]
  %.pn140 = phi i64 [ 2, %then ], [ %i_add.i, %"string::add_string.exit" ]
  %.pn138 = phi i64 [ 2, %then ], [ %result_inner.sroa.15.1.i, %"string::add_string.exit" ]
  %.pn139 = insertvalue %"#dynarray" undef, ptr %.pn142, 0
  %.pn = insertvalue %"#dynarray" %.pn139, i64 %.pn140, 1
  %inner69.i.pn = insertvalue %"#dynarray" %.pn, i64 %.pn138, 2
  %if_result = insertvalue %string zeroinitializer, %"#dynarray" %inner69.i.pn, 0
  ret %string %if_result

for:                                              ; preds = %"string::add_string.exit87", %"string::add_string.exit130"
  %self.0.0.load.i94 = phi ptr [ %result_inner.sroa.0.1.i127, %"string::add_string.exit130" ], [ %result_inner.sroa.0.1.i84, %"string::add_string.exit87" ]
  %self.0.1.load.i.i90 = phi i64 [ %i_add.i114, %"string::add_string.exit130" ], [ %i_add.i71, %"string::add_string.exit87" ]
  %18 = phi %"option<char>" [ %34, %"string::add_string.exit130" ], [ %17, %"string::add_string.exit87" ]
  %19 = extractvalue %"option<char>" %18, 1
  %.unpack.unpack = load ptr, ptr %19, align 8
  %.unpack.elt39 = getelementptr inbounds %"#dynarray", ptr %19, i64 0, i32 1
  %.unpack.unpack40 = load i64, ptr %.unpack.elt39, align 8
  %20 = alloca [2 x i8], align 2
  store i8 44, ptr %20, align 2
  %.repack48 = getelementptr inbounds [2 x i8], ptr %20, i64 0, i64 1
  store i8 32, ptr %.repack48, align 1
  %21 = tail call dereferenceable_or_null(2) ptr @malloc(i64 2)
  %22 = load i16, ptr %20, align 2
  store i16 %22, ptr %21, align 1
  %i_add.i93 = add i64 %self.0.1.load.i.i90, 2
  %23 = tail call ptr @malloc(i64 %i_add.i93)
  %i_lt.i.i21.i95 = icmp sgt i64 %self.0.1.load.i.i90, 9223372036854775805
  br i1 %i_lt.i.i21.i95, label %"#dynarray::extend.exit32.i101", label %"#dynarray::extend.exit32.thread.i97"

"#dynarray::extend.exit32.thread.i97":            ; preds = %for
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %23, ptr align 1 %self.0.0.load.i94, i64 %self.0.1.load.i.i90, i1 false)
  br label %"string::add_string.exit109"

"#dynarray::extend.exit32.i101":                  ; preds = %for
  %i_mul.i.i22.i98 = shl i64 %i_add.i93, 1
  %24 = tail call i64 @llvm.smax.i64(i64 %self.0.1.load.i.i90, i64 %i_mul.i.i22.i98)
  %25 = tail call ptr @malloc(i64 %24)
  tail call void @free(ptr %23)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %25, ptr align 1 %self.0.0.load.i94, i64 %self.0.1.load.i.i90, i1 false)
  %i_lt.i.i.i100.not = icmp ult i64 %24, %i_add.i93
  br i1 %i_lt.i.i.i100.not, label %"string::add_string.exit109", label %then.i.i.i103

then.i.i.i103:                                    ; preds = %"#dynarray::extend.exit32.i101"
  %i_mul.i.i.i102 = shl nuw i64 %24, 1
  %26 = tail call i64 @llvm.smax.i64(i64 %i_add.i93, i64 %i_mul.i.i.i102)
  %27 = tail call ptr @malloc(i64 %26)
  tail call void @llvm.memmove.p0.p0.i64(ptr align 1 %27, ptr align 1 %self.0.0.load.i94, i64 %self.0.1.load.i.i90, i1 false)
  tail call void @free(ptr %25)
  br label %"string::add_string.exit109"

"string::add_string.exit109":                     ; preds = %"#dynarray::extend.exit32.thread.i97", %"#dynarray::extend.exit32.i101", %then.i.i.i103
  %result_inner.sroa.0.1.i106 = phi ptr [ %27, %then.i.i.i103 ], [ %25, %"#dynarray::extend.exit32.i101" ], [ %23, %"#dynarray::extend.exit32.thread.i97" ]
  %self.0.load.gep.i.i107 = getelementptr i8, ptr %result_inner.sroa.0.1.i106, i64 %self.0.1.load.i.i90
  %28 = load i16, ptr %21, align 1
  store i16 %28, ptr %self.0.load.gep.i.i107, align 1
  %i_add.i114 = add i64 %.unpack.unpack40, %i_add.i93
  %29 = tail call ptr @malloc(i64 %i_add.i114)
  %i_lt.i.i21.i116 = icmp slt i64 %i_add.i114, %i_add.i93
  br i1 %i_lt.i.i21.i116, label %"#dynarray::extend.exit32.i122", label %"#dynarray::extend.exit32.thread.i118"

"#dynarray::extend.exit32.thread.i118":           ; preds = %"string::add_string.exit109"
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %29, ptr nonnull align 1 %result_inner.sroa.0.1.i106, i64 %i_add.i93, i1 false)
  br label %"string::add_string.exit130"

"#dynarray::extend.exit32.i122":                  ; preds = %"string::add_string.exit109"
  %i_mul.i.i22.i119 = shl i64 %i_add.i114, 1
  %30 = tail call i64 @llvm.smax.i64(i64 %i_add.i93, i64 %i_mul.i.i22.i119)
  %31 = tail call ptr @malloc(i64 %30)
  tail call void @free(ptr %29)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %31, ptr nonnull align 1 %result_inner.sroa.0.1.i106, i64 %i_add.i93, i1 false)
  %i_lt.i.i.i121 = icmp slt i64 %30, %i_add.i114
  br i1 %i_lt.i.i.i121, label %then.i.i.i124, label %"string::add_string.exit130"

then.i.i.i124:                                    ; preds = %"#dynarray::extend.exit32.i122"
  %i_mul.i.i.i123 = shl i64 %30, 1
  %32 = tail call i64 @llvm.smax.i64(i64 %i_add.i114, i64 %i_mul.i.i.i123)
  %33 = tail call ptr @malloc(i64 %32)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %33, ptr nonnull align 1 %result_inner.sroa.0.1.i106, i64 %i_add.i93, i1 false)
  tail call void @free(ptr %31)
  br label %"string::add_string.exit130"

"string::add_string.exit130":                     ; preds = %"#dynarray::extend.exit32.thread.i118", %"#dynarray::extend.exit32.i122", %then.i.i.i124
  %result_inner.sroa.0.1.i127 = phi ptr [ %33, %then.i.i.i124 ], [ %31, %"#dynarray::extend.exit32.i122" ], [ %29, %"#dynarray::extend.exit32.thread.i118" ]
  %self.0.load.gep.i.i128 = getelementptr i8, ptr %result_inner.sroa.0.1.i127, i64 %i_add.i93
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self.0.load.gep.i.i128, ptr align 1 %.unpack.unpack, i64 %.unpack.unpack40, i1 false)
  %34 = call %"option<char>" @"listiterator<string>::next"(ptr nonnull %it)
  %present = extractvalue %"option<char>" %34, 0
  br i1 %present, label %for, label %block5

block5:                                           ; preds = %"string::add_string.exit130", %"string::add_string.exit87"
  %self.0.0.load.i = phi ptr [ %result_inner.sroa.0.1.i84, %"string::add_string.exit87" ], [ %result_inner.sroa.0.1.i127, %"string::add_string.exit130" ]
  %self.0.1.load.i.i = phi i64 [ %i_add.i71, %"string::add_string.exit87" ], [ %i_add.i114, %"string::add_string.exit130" ]
  %35 = tail call dereferenceable_or_null(1) ptr @malloc(i64 1)
  store i8 93, ptr %35, align 1
  %i_add.i = add i64 %self.0.1.load.i.i, 1
  %36 = tail call ptr @malloc(i64 %i_add.i)
  %i_lt.i.i21.i = icmp eq i64 %self.0.1.load.i.i, 9223372036854775807
  br i1 %i_lt.i.i21.i, label %"#dynarray::extend.exit32.i", label %"#dynarray::extend.exit32.thread.i"

"#dynarray::extend.exit32.thread.i":              ; preds = %block5
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %36, ptr align 1 %self.0.0.load.i, i64 %self.0.1.load.i.i, i1 false)
  br label %"string::add_string.exit"

"#dynarray::extend.exit32.i":                     ; preds = %block5
  %i_mul.i.i22.i = shl i64 %i_add.i, 1
  %37 = tail call i64 @llvm.smax.i64(i64 %self.0.1.load.i.i, i64 %i_mul.i.i22.i)
  %38 = tail call ptr @malloc(i64 %37)
  tail call void @free(ptr %36)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %38, ptr align 1 %self.0.0.load.i, i64 %self.0.1.load.i.i, i1 false)
  %i_lt.i.i.i.not = icmp ult i64 %37, %i_add.i
  br i1 %i_lt.i.i.i.not, label %"string::add_string.exit", label %then.i.i.i

then.i.i.i:                                       ; preds = %"#dynarray::extend.exit32.i"
  %i_mul.i.i.i = shl nuw i64 %37, 1
  %39 = tail call i64 @llvm.smax.i64(i64 %i_add.i, i64 %i_mul.i.i.i)
  %40 = tail call ptr @malloc(i64 %39)
  tail call void @llvm.memmove.p0.p0.i64(ptr align 1 %40, ptr align 1 %self.0.0.load.i, i64 %self.0.1.load.i.i, i1 false)
  tail call void @free(ptr %38)
  br label %"string::add_string.exit"

"string::add_string.exit":                        ; preds = %"#dynarray::extend.exit32.thread.i", %"#dynarray::extend.exit32.i", %then.i.i.i
  %result_inner.sroa.15.1.i = phi i64 [ -2, %then.i.i.i ], [ 9223372036854775807, %"#dynarray::extend.exit32.i" ], [ %i_add.i, %"#dynarray::extend.exit32.thread.i" ]
  %result_inner.sroa.0.1.i = phi ptr [ %40, %then.i.i.i ], [ %38, %"#dynarray::extend.exit32.i" ], [ %36, %"#dynarray::extend.exit32.thread.i" ]
  %self.0.load.gep.i.i = getelementptr i8, ptr %result_inner.sroa.0.1.i, i64 %self.0.1.load.i.i
  %41 = load i8, ptr %35, align 1
  store i8 %41, ptr %self.0.load.gep.i.i, align 1
  br label %merge
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define i64 @"list<string>::len"(ptr nocapture readonly %self) local_unnamed_addr #4 {
body:
  %self.0.1 = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load = load i64, ptr %self.0.1, align 4
  %0 = sdiv i64 %self.0.1.load, 24
  ret i64 %0
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define %"listiterator<string>" @"list<string>::iterator"(ptr nocapture readonly %self) local_unnamed_addr #4 {
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
  %2 = insertvalue %"listiterator<string>" zeroinitializer, %string %inner11.i, 0
  %3 = insertvalue %"listiterator<string>" %2, i64 0, 1
  ret %"listiterator<string>" %3
}

define %"option<char>" @"listiterator<string>::next"(ptr nocapture %self) local_unnamed_addr {
body:
  %self.1 = getelementptr inbounds %"listiterator<string>", ptr %self, i64 0, i32 1
  %self.1.load = load i64, ptr %self.1, align 4
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %0 = sdiv i64 %self.0.1.load.i, 24
  %i_ge.not = icmp slt i64 %self.1.load, %0
  br i1 %i_ge.not, label %else, label %merge

else:                                             ; preds = %body
  %i_le.i = icmp sgt i64 %self.1.load, -1
  br i1 %i_le.i, label %"list<string>::get.exit", label %else.i

else.i:                                           ; preds = %else
  %stderr.i = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %1 = tail call i64 @fwrite(ptr nonnull @throw_msg.4.10, i64 25, i64 1, ptr %stderr.i)
  %2 = tail call i64 @fputwc(i32 10, ptr %stderr.i)
  tail call void @exit(i64 1)
  unreachable

"list<string>::get.exit":                         ; preds = %else
  %self.0.0.load.i = load ptr, ptr %self, align 8
  %self.0.0.load.gep.i = getelementptr %string, ptr %self.0.0.load.i, i64 %self.1.load
  %self.0.0.load.gep.deref.unpack.unpack.i = load ptr, ptr %self.0.0.load.gep.i, align 8
  %self.0.0.load.gep.deref.unpack.elt2.i = getelementptr inbounds %"#dynarray", ptr %self.0.0.load.gep.i, i64 0, i32 1
  %self.0.0.load.gep.deref.unpack.unpack3.i = load i64, ptr %self.0.0.load.gep.deref.unpack.elt2.i, align 8
  %self.0.0.load.gep.deref.unpack.elt4.i = getelementptr inbounds %"#dynarray", ptr %self.0.0.load.gep.i, i64 0, i32 2
  %self.0.0.load.gep.deref.unpack.unpack5.i = load i64, ptr %self.0.0.load.gep.deref.unpack.elt4.i, align 8
  %3 = tail call dereferenceable_or_null(24) ptr @malloc(i64 24)
  store ptr %self.0.0.load.gep.deref.unpack.unpack.i, ptr %3, align 8
  %.repack7.i = getelementptr inbounds %"#dynarray", ptr %3, i64 0, i32 1
  store i64 %self.0.0.load.gep.deref.unpack.unpack3.i, ptr %.repack7.i, align 8
  %.repack9.i = getelementptr inbounds %"#dynarray", ptr %3, i64 0, i32 2
  store i64 %self.0.0.load.gep.deref.unpack.unpack5.i, ptr %.repack9.i, align 8
  %4 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %3, 1
  %i_add = add nuw nsw i64 %self.1.load, 1
  store i64 %i_add, ptr %self.1, align 4
  br label %merge

merge:                                            ; preds = %body, %"list<string>::get.exit"
  %if_result = phi %"option<char>" [ %4, %"list<string>::get.exit" ], [ zeroinitializer, %body ]
  ret %"option<char>" %if_result
}

define %string @"option<string>::get"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %self.0.load = load i1, ptr %self, align 1
  br i1 %self.0.load, label %merge, label %else

else:                                             ; preds = %body
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.5.9, i64 16, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

merge:                                            ; preds = %body
  %self.1 = getelementptr inbounds %"option<char>", ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  %self.1.load.deref.unpack.unpack = load ptr, ptr %self.1.load, align 8
  %2 = insertvalue %"#dynarray" undef, ptr %self.1.load.deref.unpack.unpack, 0
  %self.1.load.deref.unpack.elt2 = getelementptr inbounds %"#dynarray", ptr %self.1.load, i64 0, i32 1
  %self.1.load.deref.unpack.unpack3 = load i64, ptr %self.1.load.deref.unpack.elt2, align 8
  %3 = insertvalue %"#dynarray" %2, i64 %self.1.load.deref.unpack.unpack3, 1
  %self.1.load.deref.unpack.elt4 = getelementptr inbounds %"#dynarray", ptr %self.1.load, i64 0, i32 2
  %self.1.load.deref.unpack.unpack5 = load i64, ptr %self.1.load.deref.unpack.elt4, align 8
  %self.1.load.deref.unpack6 = insertvalue %"#dynarray" %3, i64 %self.1.load.deref.unpack.unpack5, 2
  %self.1.load.deref1 = insertvalue %string undef, %"#dynarray" %self.1.load.deref.unpack6, 0
  ret %string %self.1.load.deref1
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define %"option<char>" @"option<string>::none"() local_unnamed_addr #5 {
body:
  ret %"option<char>" zeroinitializer
}

define %string @"list<string>::get"(ptr nocapture readonly %self, i64 %i) local_unnamed_addr {
body:
  %i_le = icmp sgt i64 %i, -1
  br i1 %i_le, label %and_true, label %else

else:                                             ; preds = %body, %and_true
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.4.10, i64 25, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

merge:                                            ; preds = %and_true
  %self.0.0.load = load ptr, ptr %self, align 8
  %self.0.0.load.gep = getelementptr %string, ptr %self.0.0.load, i64 %i
  %self.0.0.load.gep.deref.unpack.unpack = load ptr, ptr %self.0.0.load.gep, align 8
  %2 = insertvalue %"#dynarray" undef, ptr %self.0.0.load.gep.deref.unpack.unpack, 0
  %self.0.0.load.gep.deref.unpack.elt2 = getelementptr inbounds %"#dynarray", ptr %self.0.0.load.gep, i64 0, i32 1
  %self.0.0.load.gep.deref.unpack.unpack3 = load i64, ptr %self.0.0.load.gep.deref.unpack.elt2, align 8
  %3 = insertvalue %"#dynarray" %2, i64 %self.0.0.load.gep.deref.unpack.unpack3, 1
  %self.0.0.load.gep.deref.unpack.elt4 = getelementptr inbounds %"#dynarray", ptr %self.0.0.load.gep, i64 0, i32 2
  %self.0.0.load.gep.deref.unpack.unpack5 = load i64, ptr %self.0.0.load.gep.deref.unpack.elt4, align 8
  %self.0.0.load.gep.deref.unpack6 = insertvalue %"#dynarray" %3, i64 %self.0.0.load.gep.deref.unpack.unpack5, 2
  %self.0.0.load.gep.deref1 = insertvalue %string undef, %"#dynarray" %self.0.0.load.gep.deref.unpack6, 0
  ret %string %self.0.0.load.gep.deref1

and_true:                                         ; preds = %body
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %4 = sdiv i64 %self.0.1.load.i, 24
  %i_lt = icmp sgt i64 %4, %i
  br i1 %i_lt, label %merge, label %else
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %"option<char>" @"option<string>::some"(ptr nocapture readonly %t) local_unnamed_addr #3 {
body:
  %0 = tail call dereferenceable_or_null(24) ptr @malloc(i64 24)
  %t1.unpack.unpack = load ptr, ptr %t, align 8
  %t1.unpack.elt2 = getelementptr inbounds %"#dynarray", ptr %t, i64 0, i32 1
  %t1.unpack.unpack3 = load i64, ptr %t1.unpack.elt2, align 8
  %t1.unpack.elt4 = getelementptr inbounds %"#dynarray", ptr %t, i64 0, i32 2
  %t1.unpack.unpack5 = load i64, ptr %t1.unpack.elt4, align 8
  store ptr %t1.unpack.unpack, ptr %0, align 8
  %.repack7 = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 1
  store i64 %t1.unpack.unpack3, ptr %.repack7, align 8
  %.repack9 = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 2
  store i64 %t1.unpack.unpack5, ptr %.repack9, align 8
  %1 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %0, 1
  ret %"option<char>" %1
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define %"listiterator<string>" @"listiterator<string>::new"(ptr nocapture readonly %inner) local_unnamed_addr #4 {
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
  %2 = insertvalue %"listiterator<string>" zeroinitializer, %string %inner11, 0
  %3 = insertvalue %"listiterator<string>" %2, i64 0, 1
  ret %"listiterator<string>" %3
}

; Function Attrs: mustprogress nounwind willreturn
define %string @"option<string>::to_string"(ptr nocapture readonly %self) local_unnamed_addr #8 {
body:
  %self.0.load = load i1, ptr %self, align 1
  br i1 %self.0.load, label %then, label %else

then:                                             ; preds = %body
  %self.1 = getelementptr inbounds %"option<char>", ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  %self.1.load.deref.unpack.unpack = load ptr, ptr %self.1.load, align 8
  %self.1.load.deref.unpack.elt5 = getelementptr inbounds %"#dynarray", ptr %self.1.load, i64 0, i32 1
  %self.1.load.deref.unpack.unpack6 = load i64, ptr %self.1.load.deref.unpack.elt5, align 8
  %0 = alloca [5 x i8], align 1
  store i8 115, ptr %0, align 1
  %.repack14 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 1
  store i8 111, ptr %.repack14, align 1
  %.repack15 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 2
  store i8 109, ptr %.repack15, align 1
  %.repack16 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 3
  store i8 101, ptr %.repack16, align 1
  %.repack17 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 4
  store i8 40, ptr %.repack17, align 1
  %i_add.i = add i64 %self.1.load.deref.unpack.unpack6, 5
  %1 = tail call ptr @malloc(i64 %i_add.i)
  %i_lt.i.i21.i = icmp ugt i64 %self.1.load.deref.unpack.unpack6, 9223372036854775802
  br i1 %i_lt.i.i21.i, label %"#dynarray::extend.exit32.i", label %"#dynarray::extend.exit32.thread.i"

"#dynarray::extend.exit32.thread.i":              ; preds = %then
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %1, ptr noundef nonnull align 1 dereferenceable(5) %0, i64 5, i1 false)
  br label %"string::add_string.exit"

"#dynarray::extend.exit32.i":                     ; preds = %then
  %i_mul.i.i22.i = shl i64 %i_add.i, 1
  %2 = tail call i64 @llvm.smax.i64(i64 %i_mul.i.i22.i, i64 5)
  %3 = tail call ptr @malloc(i64 %2)
  tail call void @free(ptr %1)
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %3, ptr noundef nonnull align 1 dereferenceable(5) %0, i64 5, i1 false)
  %i_lt.i.i.i = icmp slt i64 %2, %i_add.i
  br i1 %i_lt.i.i.i, label %then.i.i.i, label %"string::add_string.exit"

then.i.i.i:                                       ; preds = %"#dynarray::extend.exit32.i"
  %i_mul.i.i.i = shl nuw i64 %2, 1
  %4 = tail call i64 @llvm.smax.i64(i64 %i_add.i, i64 %i_mul.i.i.i)
  %5 = tail call ptr @malloc(i64 %4)
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 1 dereferenceable(5) %5, ptr noundef nonnull align 1 dereferenceable(5) %0, i64 5, i1 false)
  tail call void @free(ptr nonnull %3)
  br label %"string::add_string.exit"

"string::add_string.exit":                        ; preds = %"#dynarray::extend.exit32.thread.i", %"#dynarray::extend.exit32.i", %then.i.i.i
  %result_inner.sroa.0.1.i = phi ptr [ %5, %then.i.i.i ], [ %3, %"#dynarray::extend.exit32.i" ], [ %1, %"#dynarray::extend.exit32.thread.i" ]
  %self.0.load.gep.i.i = getelementptr i8, ptr %result_inner.sroa.0.1.i, i64 5
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self.0.load.gep.i.i, ptr align 1 %self.1.load.deref.unpack.unpack, i64 %self.1.load.deref.unpack.unpack6, i1 false)
  %6 = tail call dereferenceable_or_null(1) ptr @malloc(i64 1)
  store i8 41, ptr %6, align 1
  %i_add.i37 = add i64 %self.1.load.deref.unpack.unpack6, 6
  %7 = tail call ptr @malloc(i64 %i_add.i37)
  %i_lt.i.i21.i39 = icmp slt i64 %i_add.i37, %i_add.i
  br i1 %i_lt.i.i21.i39, label %"#dynarray::extend.exit32.i45", label %"#dynarray::extend.exit32.thread.i41"

"#dynarray::extend.exit32.thread.i41":            ; preds = %"string::add_string.exit"
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %7, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i, i1 false)
  br label %"string::add_string.exit53"

"#dynarray::extend.exit32.i45":                   ; preds = %"string::add_string.exit"
  %i_mul.i.i22.i42 = shl i64 %i_add.i37, 1
  %8 = tail call i64 @llvm.smax.i64(i64 %i_add.i, i64 %i_mul.i.i22.i42)
  %9 = tail call ptr @malloc(i64 %8)
  tail call void @free(ptr %7)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %9, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i, i1 false)
  %i_lt.i.i.i44 = icmp slt i64 %8, %i_add.i37
  br i1 %i_lt.i.i.i44, label %then.i.i.i47, label %"string::add_string.exit53"

then.i.i.i47:                                     ; preds = %"#dynarray::extend.exit32.i45"
  %i_mul.i.i.i46 = shl i64 %8, 1
  %10 = tail call i64 @llvm.smax.i64(i64 %i_add.i37, i64 %i_mul.i.i.i46)
  %11 = tail call ptr @malloc(i64 %10)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %11, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i, i1 false)
  tail call void @free(ptr %9)
  br label %"string::add_string.exit53"

"string::add_string.exit53":                      ; preds = %"#dynarray::extend.exit32.thread.i41", %"#dynarray::extend.exit32.i45", %then.i.i.i47
  %result_inner.sroa.15.1.i49 = phi i64 [ %10, %then.i.i.i47 ], [ %8, %"#dynarray::extend.exit32.i45" ], [ %i_add.i37, %"#dynarray::extend.exit32.thread.i41" ]
  %result_inner.sroa.0.1.i50 = phi ptr [ %11, %then.i.i.i47 ], [ %9, %"#dynarray::extend.exit32.i45" ], [ %7, %"#dynarray::extend.exit32.thread.i41" ]
  %self.0.load.gep.i.i51 = getelementptr i8, ptr %result_inner.sroa.0.1.i50, i64 %i_add.i
  %12 = load i8, ptr %6, align 1
  store i8 %12, ptr %self.0.load.gep.i.i51, align 1
  br label %merge

else:                                             ; preds = %body
  %13 = alloca [4 x i8], align 4
  store i8 110, ptr %13, align 4
  %.repack1 = getelementptr inbounds [4 x i8], ptr %13, i64 0, i64 1
  store i8 111, ptr %.repack1, align 1
  %.repack2 = getelementptr inbounds [4 x i8], ptr %13, i64 0, i64 2
  store i8 110, ptr %.repack2, align 2
  %.repack3 = getelementptr inbounds [4 x i8], ptr %13, i64 0, i64 3
  store i8 101, ptr %.repack3, align 1
  %14 = tail call dereferenceable_or_null(4) ptr @malloc(i64 4)
  %15 = load i32, ptr %13, align 4
  store i32 %15, ptr %14, align 1
  br label %merge

merge:                                            ; preds = %else, %"string::add_string.exit53"
  %result_inner.sroa.0.1.i50.pn = phi ptr [ %result_inner.sroa.0.1.i50, %"string::add_string.exit53" ], [ %14, %else ]
  %i_add.i37.pn = phi i64 [ %i_add.i37, %"string::add_string.exit53" ], [ 4, %else ]
  %result_inner.sroa.15.1.i49.pn = phi i64 [ %result_inner.sroa.15.1.i49, %"string::add_string.exit53" ], [ 4, %else ]
  %.pn57 = insertvalue %"#dynarray" undef, ptr %result_inner.sroa.0.1.i50.pn, 0
  %.pn = insertvalue %"#dynarray" %.pn57, i64 %i_add.i37.pn, 1
  %result_inner29.i52.pn = insertvalue %"#dynarray" %.pn, i64 %result_inner.sroa.15.1.i49.pn, 2
  %if_result = insertvalue %string zeroinitializer, %"#dynarray" %result_inner29.i52.pn, 0
  ret %string %if_result
}

define void @"list<string>::set"(ptr nocapture readonly %self, i64 %i, ptr nocapture readonly %t) local_unnamed_addr {
body:
  %i_le = icmp sgt i64 %i, -1
  br i1 %i_le, label %and_true, label %else

else:                                             ; preds = %body, %and_true
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.4.10, i64 25, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

and_true:                                         ; preds = %body
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %2 = sdiv i64 %self.0.1.load.i, 24
  %i_lt = icmp sgt i64 %2, %i
  br i1 %i_lt, label %block, label %else

block:                                            ; preds = %and_true
  %t6.unpack.unpack = load ptr, ptr %t, align 8
  %t6.unpack.elt2 = getelementptr inbounds %"#dynarray", ptr %t, i64 0, i32 1
  %t6.unpack.unpack3 = load i64, ptr %t6.unpack.elt2, align 8
  %t6.unpack.elt4 = getelementptr inbounds %"#dynarray", ptr %t, i64 0, i32 2
  %t6.unpack.unpack5 = load i64, ptr %t6.unpack.elt4, align 8
  %self.0.0.load = load ptr, ptr %self, align 8
  %self.0.0.load.gep = getelementptr %string, ptr %self.0.0.load, i64 %i
  store ptr %t6.unpack.unpack, ptr %self.0.0.load.gep, align 8
  %self.0.0.load.gep.repack7 = getelementptr inbounds %"#dynarray", ptr %self.0.0.load.gep, i64 0, i32 1
  store i64 %t6.unpack.unpack3, ptr %self.0.0.load.gep.repack7, align 8
  %self.0.0.load.gep.repack9 = getelementptr inbounds %"#dynarray", ptr %self.0.0.load.gep, i64 0, i32 2
  store i64 %t6.unpack.unpack5, ptr %self.0.0.load.gep.repack9, align 8
  ret void
}

define void @"list<string>::extend"(ptr nocapture %self, ptr nocapture readonly %lst) local_unnamed_addr {
body:
  %inner1.unpack.unpack.i.i = load ptr, ptr %lst, align 8
  %inner1.unpack.elt2.i.i = getelementptr inbounds %"#dynarray", ptr %lst, i64 0, i32 1
  %inner1.unpack.unpack3.i.i = load i64, ptr %inner1.unpack.elt2.i.i, align 8
  %inner1.unpack.elt4.i.i = getelementptr inbounds %"#dynarray", ptr %lst, i64 0, i32 2
  %inner1.unpack.unpack5.i.i = load i64, ptr %inner1.unpack.elt4.i.i, align 8
  %0 = alloca %"listiterator<string>", align 8
  store ptr %inner1.unpack.unpack.i.i, ptr %0, align 8
  %.repack4 = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 1
  store i64 %inner1.unpack.unpack3.i.i, ptr %.repack4, align 8
  %.repack6 = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 2
  store i64 %inner1.unpack.unpack5.i.i, ptr %.repack6, align 8
  %.repack1 = getelementptr inbounds %"listiterator<string>", ptr %0, i64 0, i32 1
  store i64 0, ptr %.repack1, align 8
  %1 = call %"option<char>" @"listiterator<string>::next"(ptr nonnull %0)
  %present17 = extractvalue %"option<char>" %1, 0
  br i1 %present17, label %for.preheader, label %post_for

for.preheader:                                    ; preds = %body
  %self.1.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %self.2.i.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  br label %for

for:                                              ; preds = %for.preheader, %"list<string>::push.exit"
  %2 = phi %"option<char>" [ %6, %"list<string>::push.exit" ], [ %1, %for.preheader ]
  %3 = extractvalue %"option<char>" %2, 1
  %.unpack.unpack = load ptr, ptr %3, align 8
  %.unpack.elt8 = getelementptr inbounds %"#dynarray", ptr %3, i64 0, i32 1
  %.unpack.unpack9 = load i64, ptr %.unpack.elt8, align 8
  %.unpack.elt10 = getelementptr inbounds %"#dynarray", ptr %3, i64 0, i32 2
  %.unpack.unpack11 = load i64, ptr %.unpack.elt10, align 8
  %self.1.load.i.i = load i64, ptr %self.1.i.i, align 4
  %i_add.i.i = add i64 %self.1.load.i.i, 24
  %self.2.load.i.i.i = load i64, ptr %self.2.i.i.i, align 4
  %i_lt.i.i.i = icmp slt i64 %self.2.load.i.i.i, %i_add.i.i
  br i1 %i_lt.i.i.i, label %then.i.i.i, label %"list<string>::push.exit"

then.i.i.i:                                       ; preds = %for
  %i_mul.i.i.i = shl i64 %self.2.load.i.i.i, 1
  %4 = tail call i64 @llvm.smax.i64(i64 %i_add.i.i, i64 %i_mul.i.i.i)
  %self.0.load.i.i.i = load ptr, ptr %self, align 8
  %5 = tail call ptr @malloc(i64 %4)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %5, ptr align 1 %self.0.load.i.i.i, i64 %self.1.load.i.i, i1 false)
  store ptr %5, ptr %self, align 8
  store i64 %4, ptr %self.2.i.i.i, align 4
  tail call void @free(ptr %self.0.load.i.i.i)
  %self.14.load.pre.i.i = load i64, ptr %self.1.i.i, align 4
  br label %"list<string>::push.exit"

"list<string>::push.exit":                        ; preds = %for, %then.i.i.i
  %self.14.load.i.i = phi i64 [ %self.14.load.pre.i.i, %then.i.i.i ], [ %self.1.load.i.i, %for ]
  %self.0.load.i.i = load ptr, ptr %self, align 8
  %self.0.load.gep.i.i = getelementptr i8, ptr %self.0.load.i.i, i64 %self.14.load.i.i
  store ptr %.unpack.unpack, ptr %self.0.load.gep.i.i, align 1
  %alloca.string.i.sroa.4.0.self.0.load.gep.i.i.sroa_idx = getelementptr inbounds i8, ptr %self.0.load.gep.i.i, i64 8
  store i64 %.unpack.unpack9, ptr %alloca.string.i.sroa.4.0.self.0.load.gep.i.i.sroa_idx, align 1
  %alloca.string.i.sroa.5.0.self.0.load.gep.i.i.sroa_idx = getelementptr inbounds i8, ptr %self.0.load.gep.i.i, i64 16
  store i64 %.unpack.unpack11, ptr %alloca.string.i.sroa.5.0.self.0.load.gep.i.i.sroa_idx, align 1
  %self.18.load.i.i = load i64, ptr %self.1.i.i, align 4
  %i_add10.i.i = add i64 %self.18.load.i.i, 24
  store i64 %i_add10.i.i, ptr %self.1.i.i, align 4
  %6 = call %"option<char>" @"listiterator<string>::next"(ptr nonnull %0)
  %present = extractvalue %"option<char>" %6, 0
  br i1 %present, label %for, label %post_for

post_for:                                         ; preds = %"list<string>::push.exit", %body
  ret void
}

; Function Attrs: mustprogress nounwind willreturn
define void @"list<string>::push"(ptr nocapture %self, ptr nocapture readonly %e) local_unnamed_addr #8 {
body:
  %e1.unpack.unpack = load ptr, ptr %e, align 8
  %e1.unpack.elt2 = getelementptr inbounds %"#dynarray", ptr %e, i64 0, i32 1
  %e1.unpack.unpack3 = load i64, ptr %e1.unpack.elt2, align 8
  %e1.unpack.elt4 = getelementptr inbounds %"#dynarray", ptr %e, i64 0, i32 2
  %e1.unpack.unpack5 = load i64, ptr %e1.unpack.elt4, align 8
  %self.1.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %self.1.load.i = load i64, ptr %self.1.i, align 4
  %i_add.i = add i64 %self.1.load.i, 24
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
  store ptr %e1.unpack.unpack, ptr %self.0.load.gep.i, align 1
  %alloca.string.sroa.2.0.self.0.load.gep.i.sroa_idx = getelementptr inbounds i8, ptr %self.0.load.gep.i, i64 8
  store i64 %e1.unpack.unpack3, ptr %alloca.string.sroa.2.0.self.0.load.gep.i.sroa_idx, align 1
  %alloca.string.sroa.3.0.self.0.load.gep.i.sroa_idx = getelementptr inbounds i8, ptr %self.0.load.gep.i, i64 16
  store i64 %e1.unpack.unpack5, ptr %alloca.string.sroa.3.0.self.0.load.gep.i.sroa_idx, align 1
  %self.18.load.i = load i64, ptr %self.1.i, align 4
  %i_add10.i = add i64 %self.18.load.i, 24
  store i64 %i_add10.i, ptr %self.1.i, align 4
  ret void
}

define %string @"list<string>::pop"(ptr nocapture %self) local_unnamed_addr {
body:
  %self.1.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %self.1.load.i = load i64, ptr %self.1.i, align 4
  %i_ge.not.i = icmp slt i64 %self.1.load.i, 24
  br i1 %i_ge.not.i, label %else.i, label %"#dynarray::take.exit"

else.i:                                           ; preds = %body
  %stderr.i = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.2, i64 30, i64 1, ptr %stderr.i)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr.i)
  tail call void @exit(i64 1)
  unreachable

"#dynarray::take.exit":                           ; preds = %body
  %i_sub.i = add nsw i64 %self.1.load.i, -24
  store i64 %i_sub.i, ptr %self.1.i, align 4
  %self.0.load.i = load ptr, ptr %self, align 8
  %self.0.load.gep.i = getelementptr i8, ptr %self.0.load.i, i64 %i_sub.i
  %.deref.unpack.unpack = load ptr, ptr %self.0.load.gep.i, align 8
  %2 = insertvalue %"#dynarray" undef, ptr %.deref.unpack.unpack, 0
  %.deref.unpack.elt2 = getelementptr inbounds %"#dynarray", ptr %self.0.load.gep.i, i64 0, i32 1
  %.deref.unpack.unpack3 = load i64, ptr %.deref.unpack.elt2, align 8
  %3 = insertvalue %"#dynarray" %2, i64 %.deref.unpack.unpack3, 1
  %.deref.unpack.elt4 = getelementptr inbounds %"#dynarray", ptr %self.0.load.gep.i, i64 0, i32 2
  %.deref.unpack.unpack5 = load i64, ptr %.deref.unpack.elt4, align 8
  %.deref.unpack6 = insertvalue %"#dynarray" %3, i64 %.deref.unpack.unpack5, 2
  %.deref1 = insertvalue %string undef, %"#dynarray" %.deref.unpack6, 0
  ret %string %.deref1
}

define %string @"ReviewAnalysis::collect_comments"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %alloca.ptr.i109 = alloca ptr, align 8
  %alloca.ptr.i = alloca ptr, align 8
  %0 = alloca %string_chars, align 8
  %1 = tail call ptr @malloc(i64 0)
  %inner1.unpack.unpack.i.i = load ptr, ptr %self, align 8
  %inner1.unpack.elt2.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %inner1.unpack.unpack3.i.i = load i64, ptr %inner1.unpack.elt2.i.i, align 8
  %inner1.unpack.elt4.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %inner1.unpack.unpack5.i.i = load i64, ptr %inner1.unpack.elt4.i.i, align 8
  %2 = alloca %"listiterator<string>", align 8
  store ptr %inner1.unpack.unpack.i.i, ptr %2, align 8
  %.repack9 = getelementptr inbounds %"#dynarray", ptr %2, i64 0, i32 1
  store i64 %inner1.unpack.unpack3.i.i, ptr %.repack9, align 8
  %.repack11 = getelementptr inbounds %"#dynarray", ptr %2, i64 0, i32 2
  store i64 %inner1.unpack.unpack5.i.i, ptr %.repack11, align 8
  %.repack6 = getelementptr inbounds %"listiterator<string>", ptr %2, i64 0, i32 1
  store i64 0, ptr %.repack6, align 8
  %3 = call %"option<char>" @"listiterator<Review>::next"(ptr nonnull %2)
  %present65 = extractvalue %"option<char>" %3, 0
  br i1 %present65, label %for.preheader, label %post_for

for.preheader:                                    ; preds = %body
  %.repack4.i = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 1
  %.repack6.i = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 2
  %.repack1.i = getelementptr inbounds %string_chars, ptr %0, i64 0, i32 1
  br label %for

for:                                              ; preds = %for.preheader, %post_block13
  %comments.sroa.0.0 = phi ptr [ %comments.sroa.0.2, %post_block13 ], [ %1, %for.preheader ]
  %comments.sroa.5.0 = phi i64 [ %comments.sroa.5.1, %post_block13 ], [ 0, %for.preheader ]
  %comments.sroa.11.0 = phi i64 [ %comments.sroa.11.2, %post_block13 ], [ 0, %for.preheader ]
  %4 = phi %"option<char>" [ %35, %post_block13 ], [ %3, %for.preheader ]
  %i.066 = phi i64 [ %i_add, %post_block13 ], [ 0, %for.preheader ]
  %5 = extractvalue %"option<char>" %4, 1
  %.elt20 = getelementptr inbounds %Review, ptr %5, i64 0, i32 1
  %.unpack21.unpack.unpack = load ptr, ptr %.elt20, align 8
  %.unpack21.unpack.elt23 = getelementptr inbounds %Review, ptr %5, i64 0, i32 1, i32 0, i32 1
  %.unpack21.unpack.unpack24 = load i64, ptr %.unpack21.unpack.elt23, align 8
  %.unpack21.unpack.elt25 = getelementptr inbounds %Review, ptr %5, i64 0, i32 1, i32 0, i32 2
  %.unpack21.unpack.unpack26 = load i64, ptr %.unpack21.unpack.elt25, align 8
  call void @llvm.lifetime.start.p0(i64 32, ptr nonnull %0)
  %6 = call dereferenceable_or_null(4) ptr @malloc(i64 4)
  store ptr %.unpack21.unpack.unpack, ptr %0, align 8
  store i64 %.unpack21.unpack.unpack24, ptr %.repack4.i, align 8
  store i64 %.unpack21.unpack.unpack26, ptr %.repack6.i, align 8
  store ptr %6, ptr %.repack1.i, align 8
  br label %for_cond.i

for_cond.i:                                       ; preds = %post_cmp.i, %for
  %7 = call %"option<char>" @"string_chars::next"(ptr nonnull %0)
  %present.i = extractvalue %"option<char>" %7, 0
  br i1 %present.i, label %post_cmp.i, label %"string::contains_char.exit"

post_cmp.i:                                       ; preds = %for_cond.i
  %8 = extractvalue %"option<char>" %7, 1
  %9 = load i32, ptr %8, align 4
  %i_eq.i = icmp eq i32 %9, 33
  br i1 %i_eq.i, label %then, label %for_cond.i

"string::contains_char.exit":                     ; preds = %for_cond.i
  call void @llvm.lifetime.end.p0(i64 32, ptr nonnull %0)
  br label %post_block13

post_for:                                         ; preds = %post_block13, %body
  %comments15.unpack.unpack17 = phi i64 [ 0, %body ], [ %comments.sroa.11.2, %post_block13 ]
  %comments15.unpack.unpack15 = phi i64 [ 0, %body ], [ %comments.sroa.5.1, %post_block13 ]
  %comments15.unpack.unpack = phi ptr [ %1, %body ], [ %comments.sroa.0.2, %post_block13 ]
  %10 = insertvalue %"#dynarray" undef, ptr %comments15.unpack.unpack, 0
  %11 = insertvalue %"#dynarray" %10, i64 %comments15.unpack.unpack15, 1
  %comments15.unpack18 = insertvalue %"#dynarray" %11, i64 %comments15.unpack.unpack17, 2
  %comments1513 = insertvalue %string undef, %"#dynarray" %comments15.unpack18, 0
  ret %string %comments1513

then:                                             ; preds = %post_cmp.i
  call void @llvm.lifetime.end.p0(i64 32, ptr nonnull %0)
  %comment = alloca %string, align 8
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %alloca.ptr.i)
  %12 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %alloca.ptr.i, ptr nonnull @_tmpl_int_to_string, i64 %i.066)
  %buf_ptr4.deref.i = load ptr, ptr %alloca.ptr.i, align 8
  %i_add.i = add i64 %12, 1
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %alloca.ptr.i)
  %13 = call dereferenceable_or_null(1) ptr @malloc(i64 1)
  store i8 45, ptr %13, align 1
  %14 = call ptr @malloc(i64 %i_add.i)
  %i_lt.i.i21.i = icmp eq i64 %12, 9223372036854775807
  br i1 %i_lt.i.i21.i, label %"#dynarray::extend.exit32.i", label %"#dynarray::extend.exit32.thread.i"

"#dynarray::extend.exit32.thread.i":              ; preds = %then
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %14, ptr align 1 %buf_ptr4.deref.i, i64 %12, i1 false)
  br label %"string::add_string.exit"

"#dynarray::extend.exit32.i":                     ; preds = %then
  %i_mul.i.i22.i = shl i64 %i_add.i, 1
  %15 = call i64 @llvm.smax.i64(i64 %12, i64 %i_mul.i.i22.i)
  %16 = call ptr @malloc(i64 %15)
  call void @free(ptr %14)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %16, ptr align 1 %buf_ptr4.deref.i, i64 %12, i1 false)
  %i_lt.i.i.i71.not = icmp ult i64 %15, %i_add.i
  br i1 %i_lt.i.i.i71.not, label %"string::add_string.exit", label %then.i.i.i73

then.i.i.i73:                                     ; preds = %"#dynarray::extend.exit32.i"
  %i_mul.i.i.i72 = shl nuw i64 %15, 1
  %17 = call i64 @llvm.smax.i64(i64 %i_add.i, i64 %i_mul.i.i.i72)
  %18 = call ptr @malloc(i64 %17)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %18, ptr align 1 %buf_ptr4.deref.i, i64 %12, i1 false)
  call void @free(ptr %16)
  br label %"string::add_string.exit"

"string::add_string.exit":                        ; preds = %"#dynarray::extend.exit32.thread.i", %"#dynarray::extend.exit32.i", %then.i.i.i73
  %result_inner.sroa.0.1.i = phi ptr [ %18, %then.i.i.i73 ], [ %16, %"#dynarray::extend.exit32.i" ], [ %14, %"#dynarray::extend.exit32.thread.i" ]
  %self.0.load.gep.i.i74 = getelementptr i8, ptr %result_inner.sroa.0.1.i, i64 %12
  %19 = load i8, ptr %13, align 1
  store i8 %19, ptr %self.0.load.gep.i.i74, align 1
  %i_add.i79 = add i64 %i_add.i, %.unpack21.unpack.unpack24
  %20 = call ptr @malloc(i64 %i_add.i79)
  %i_lt.i.i21.i81 = icmp slt i64 %i_add.i79, %i_add.i
  br i1 %i_lt.i.i21.i81, label %"#dynarray::extend.exit32.i87", label %"#dynarray::extend.exit32.thread.i83"

"#dynarray::extend.exit32.thread.i83":            ; preds = %"string::add_string.exit"
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %20, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i, i1 false)
  br label %"string::add_string.exit95"

"#dynarray::extend.exit32.i87":                   ; preds = %"string::add_string.exit"
  %i_mul.i.i22.i84 = shl i64 %i_add.i79, 1
  %21 = call i64 @llvm.smax.i64(i64 %i_add.i, i64 %i_mul.i.i22.i84)
  %22 = call ptr @malloc(i64 %21)
  call void @free(ptr %20)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %22, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i, i1 false)
  %i_lt.i.i.i86 = icmp slt i64 %21, %i_add.i79
  br i1 %i_lt.i.i.i86, label %then.i.i.i89, label %"string::add_string.exit95"

then.i.i.i89:                                     ; preds = %"#dynarray::extend.exit32.i87"
  %i_mul.i.i.i88 = shl i64 %21, 1
  %23 = call i64 @llvm.smax.i64(i64 %i_add.i79, i64 %i_mul.i.i.i88)
  %24 = call ptr @malloc(i64 %23)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %24, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i, i1 false)
  call void @free(ptr %22)
  br label %"string::add_string.exit95"

"string::add_string.exit95":                      ; preds = %"#dynarray::extend.exit32.thread.i83", %"#dynarray::extend.exit32.i87", %then.i.i.i89
  %result_inner.sroa.15.1.i91 = phi i64 [ %23, %then.i.i.i89 ], [ %21, %"#dynarray::extend.exit32.i87" ], [ %i_add.i79, %"#dynarray::extend.exit32.thread.i83" ]
  %result_inner.sroa.0.1.i92 = phi ptr [ %24, %then.i.i.i89 ], [ %22, %"#dynarray::extend.exit32.i87" ], [ %20, %"#dynarray::extend.exit32.thread.i83" ]
  %self.0.load.gep.i.i93 = getelementptr i8, ptr %result_inner.sroa.0.1.i92, i64 %i_add.i
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self.0.load.gep.i.i93, ptr align 1 %.unpack21.unpack.unpack, i64 %.unpack21.unpack.unpack24, i1 false)
  store ptr %result_inner.sroa.0.1.i92, ptr %comment, align 8
  %comment.repack51 = getelementptr inbounds %"#dynarray", ptr %comment, i64 0, i32 1
  store i64 %i_add.i79, ptr %comment.repack51, align 8
  %comment.repack53 = getelementptr inbounds %"#dynarray", ptr %comment, i64 0, i32 2
  store i64 %result_inner.sroa.15.1.i91, ptr %comment.repack53, align 8
  %i_sub = add i64 %12, %.unpack21.unpack.unpack24
  %25 = call i32 @"string::get"(ptr nonnull %comment, i64 %i_sub)
  %i_ne.not = icmp eq i32 %25, 33
  br i1 %i_ne.not, label %merge3, label %and_true

merge3:                                           ; preds = %"string::add_string.exit95", %and_true, %"string::add_string.exit132"
  %e1.unpack.unpack5.i = phi i64 [ %result_inner.sroa.15.1.i91, %"string::add_string.exit95" ], [ %result_inner.sroa.15.1.i91, %and_true ], [ %result_inner.sroa.15.1.i128, %"string::add_string.exit132" ]
  %e1.unpack.unpack3.i = phi i64 [ %i_add.i79, %"string::add_string.exit95" ], [ %i_add.i79, %and_true ], [ %i_add.i116, %"string::add_string.exit132" ]
  %e1.unpack.unpack.i = phi ptr [ %result_inner.sroa.0.1.i92, %"string::add_string.exit95" ], [ %result_inner.sroa.0.1.i92, %and_true ], [ %result_inner.sroa.0.1.i129, %"string::add_string.exit132" ]
  %i_add.i.i = add i64 %comments.sroa.5.0, 24
  %i_lt.i.i.i100 = icmp slt i64 %comments.sroa.11.0, %i_add.i.i
  br i1 %i_lt.i.i.i100, label %then.i.i.i104, label %"list<string>::push.exit"

then.i.i.i104:                                    ; preds = %merge3
  %i_mul.i.i.i101 = shl i64 %comments.sroa.11.0, 1
  %26 = call i64 @llvm.smax.i64(i64 %i_add.i.i, i64 %i_mul.i.i.i101)
  %27 = call ptr @malloc(i64 %26)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %27, ptr align 1 %comments.sroa.0.0, i64 %comments.sroa.5.0, i1 false)
  call void @free(ptr %comments.sroa.0.0)
  br label %"list<string>::push.exit"

"list<string>::push.exit":                        ; preds = %merge3, %then.i.i.i104
  %comments.sroa.0.1 = phi ptr [ %27, %then.i.i.i104 ], [ %comments.sroa.0.0, %merge3 ]
  %comments.sroa.11.1 = phi i64 [ %26, %then.i.i.i104 ], [ %comments.sroa.11.0, %merge3 ]
  %self.0.load.gep.i.i107 = getelementptr i8, ptr %comments.sroa.0.1, i64 %comments.sroa.5.0
  store ptr %e1.unpack.unpack.i, ptr %self.0.load.gep.i.i107, align 1
  %alloca.string.i.sroa.4.0.self.0.load.gep.i.i107.sroa_idx = getelementptr inbounds i8, ptr %self.0.load.gep.i.i107, i64 8
  store i64 %e1.unpack.unpack3.i, ptr %alloca.string.i.sroa.4.0.self.0.load.gep.i.i107.sroa_idx, align 1
  %alloca.string.i.sroa.5.0.self.0.load.gep.i.i107.sroa_idx = getelementptr inbounds i8, ptr %self.0.load.gep.i.i107, i64 16
  store i64 %e1.unpack.unpack5.i, ptr %alloca.string.i.sroa.5.0.self.0.load.gep.i.i107.sroa_idx, align 1
  br label %post_block13

and_true:                                         ; preds = %"string::add_string.exit95"
  %28 = call i32 @"string::get"(ptr nonnull %comment, i64 %i_sub)
  %i_ne6.not = icmp eq i32 %28, 46
  br i1 %i_ne6.not, label %merge3, label %block

block:                                            ; preds = %and_true
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %alloca.ptr.i109)
  %29 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %alloca.ptr.i109, ptr nonnull @_tmpl_char_to_string, i32 46)
  %buf_ptr4.deref.i110 = load ptr, ptr %alloca.ptr.i109, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %alloca.ptr.i109)
  %i_add.i116 = add i64 %29, %i_add.i79
  %30 = call ptr @malloc(i64 %i_add.i116)
  %i_lt.i.i21.i118 = icmp slt i64 %i_add.i116, %i_add.i79
  br i1 %i_lt.i.i21.i118, label %"#dynarray::extend.exit32.i124", label %"#dynarray::extend.exit32.thread.i120"

"#dynarray::extend.exit32.thread.i120":           ; preds = %block
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %30, ptr align 1 %result_inner.sroa.0.1.i92, i64 %i_add.i79, i1 false)
  br label %"string::add_string.exit132"

"#dynarray::extend.exit32.i124":                  ; preds = %block
  %i_mul.i.i22.i121 = shl i64 %i_add.i116, 1
  %31 = call i64 @llvm.smax.i64(i64 %i_add.i79, i64 %i_mul.i.i22.i121)
  %32 = call ptr @malloc(i64 %31)
  call void @free(ptr %30)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %32, ptr align 1 %result_inner.sroa.0.1.i92, i64 %i_add.i79, i1 false)
  %i_lt.i.i.i123 = icmp slt i64 %31, %i_add.i116
  br i1 %i_lt.i.i.i123, label %then.i.i.i126, label %"string::add_string.exit132"

then.i.i.i126:                                    ; preds = %"#dynarray::extend.exit32.i124"
  %i_mul.i.i.i125 = shl i64 %31, 1
  %33 = call i64 @llvm.smax.i64(i64 %i_add.i116, i64 %i_mul.i.i.i125)
  %34 = call ptr @malloc(i64 %33)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %34, ptr align 1 %result_inner.sroa.0.1.i92, i64 %i_add.i79, i1 false)
  call void @free(ptr %32)
  br label %"string::add_string.exit132"

"string::add_string.exit132":                     ; preds = %"#dynarray::extend.exit32.thread.i120", %"#dynarray::extend.exit32.i124", %then.i.i.i126
  %result_inner.sroa.15.1.i128 = phi i64 [ %33, %then.i.i.i126 ], [ %31, %"#dynarray::extend.exit32.i124" ], [ %i_add.i116, %"#dynarray::extend.exit32.thread.i120" ]
  %result_inner.sroa.0.1.i129 = phi ptr [ %34, %then.i.i.i126 ], [ %32, %"#dynarray::extend.exit32.i124" ], [ %30, %"#dynarray::extend.exit32.thread.i120" ]
  %self.0.load.gep.i.i130 = getelementptr i8, ptr %result_inner.sroa.0.1.i129, i64 %i_add.i79
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self.0.load.gep.i.i130, ptr align 1 %buf_ptr4.deref.i110, i64 %29, i1 false)
  store ptr %result_inner.sroa.0.1.i129, ptr %comment, align 8
  store i64 %i_add.i116, ptr %comment.repack51, align 8
  store i64 %result_inner.sroa.15.1.i128, ptr %comment.repack53, align 8
  br label %merge3

post_block13:                                     ; preds = %"string::contains_char.exit", %"list<string>::push.exit"
  %comments.sroa.0.2 = phi ptr [ %comments.sroa.0.1, %"list<string>::push.exit" ], [ %comments.sroa.0.0, %"string::contains_char.exit" ]
  %comments.sroa.5.1 = phi i64 [ %i_add.i.i, %"list<string>::push.exit" ], [ %comments.sroa.5.0, %"string::contains_char.exit" ]
  %comments.sroa.11.2 = phi i64 [ %comments.sroa.11.1, %"list<string>::push.exit" ], [ %comments.sroa.11.0, %"string::contains_char.exit" ]
  %i_add = add i64 %i.066, 1
  %35 = call %"option<char>" @"listiterator<Review>::next"(ptr nonnull %2)
  %present = extractvalue %"option<char>" %35, 0
  br i1 %present, label %for, label %post_for
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %string @"list<string>::from_raw"(ptr nocapture readonly %contents, i64 %len) local_unnamed_addr #3 {
body:
  %i_mul = mul i64 %len, 24
  %0 = tail call ptr @malloc(i64 %i_mul)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %0, ptr align 1 %contents, i64 %i_mul, i1 false)
  %1 = insertvalue %"#dynarray" undef, ptr %0, 0
  %2 = insertvalue %"#dynarray" %1, i64 %i_mul, 1
  %inner79 = insertvalue %"#dynarray" %2, i64 %i_mul, 2
  %3 = insertvalue %string zeroinitializer, %"#dynarray" %inner79, 0
  ret %string %3
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define %"listiterator<string>" @"list<Review>::iterator"(ptr nocapture readonly %self) local_unnamed_addr #4 {
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
  %2 = insertvalue %"listiterator<string>" zeroinitializer, %string %inner11.i, 0
  %3 = insertvalue %"listiterator<string>" %2, i64 0, 1
  ret %"listiterator<string>" %3
}

define %"option<char>" @"listiterator<Review>::next"(ptr nocapture %self) local_unnamed_addr {
body:
  %self.1 = getelementptr inbounds %"listiterator<string>", ptr %self, i64 0, i32 1
  %self.1.load = load i64, ptr %self.1, align 4
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %0 = sdiv i64 %self.0.1.load.i, 32
  %i_ge.not = icmp slt i64 %self.1.load, %0
  br i1 %i_ge.not, label %else, label %merge

else:                                             ; preds = %body
  %i_le.i = icmp sgt i64 %self.1.load, -1
  br i1 %i_le.i, label %"list<Review>::get.exit", label %else.i

else.i:                                           ; preds = %else
  %stderr.i = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %1 = tail call i64 @fwrite(ptr nonnull @throw_msg.4.10, i64 25, i64 1, ptr %stderr.i)
  %2 = tail call i64 @fputwc(i32 10, ptr %stderr.i)
  tail call void @exit(i64 1)
  unreachable

"list<Review>::get.exit":                         ; preds = %else
  %self.0.0.load.i = load ptr, ptr %self, align 8
  %self.0.0.load.gep.i = getelementptr %Review, ptr %self.0.0.load.i, i64 %self.1.load
  %self.0.0.load.gep.deref.unpack.i = load i64, ptr %self.0.0.load.gep.i, align 8
  %self.0.0.load.gep.deref.elt1.i = getelementptr %Review, ptr %self.0.0.load.i, i64 %self.1.load, i32 1
  %self.0.0.load.gep.deref.unpack2.unpack.unpack.i = load ptr, ptr %self.0.0.load.gep.deref.elt1.i, align 8
  %self.0.0.load.gep.deref.unpack2.unpack.elt5.i = getelementptr inbounds %"#dynarray", ptr %self.0.0.load.gep.deref.elt1.i, i64 0, i32 1
  %self.0.0.load.gep.deref.unpack2.unpack.unpack6.i = load i64, ptr %self.0.0.load.gep.deref.unpack2.unpack.elt5.i, align 8
  %self.0.0.load.gep.deref.unpack2.unpack.elt7.i = getelementptr inbounds %"#dynarray", ptr %self.0.0.load.gep.deref.elt1.i, i64 0, i32 2
  %self.0.0.load.gep.deref.unpack2.unpack.unpack8.i = load i64, ptr %self.0.0.load.gep.deref.unpack2.unpack.elt7.i, align 8
  %3 = tail call dereferenceable_or_null(32) ptr @malloc(i64 32)
  store i64 %self.0.0.load.gep.deref.unpack.i, ptr %3, align 8
  %.repack10.i = getelementptr inbounds %Review, ptr %3, i64 0, i32 1
  store ptr %self.0.0.load.gep.deref.unpack2.unpack.unpack.i, ptr %.repack10.i, align 8
  %.repack10.repack12.i = getelementptr inbounds %Review, ptr %3, i64 0, i32 1, i32 0, i32 1
  store i64 %self.0.0.load.gep.deref.unpack2.unpack.unpack6.i, ptr %.repack10.repack12.i, align 8
  %.repack10.repack14.i = getelementptr inbounds %Review, ptr %3, i64 0, i32 1, i32 0, i32 2
  store i64 %self.0.0.load.gep.deref.unpack2.unpack.unpack8.i, ptr %.repack10.repack14.i, align 8
  %4 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %3, 1
  %i_add = add nuw nsw i64 %self.1.load, 1
  store i64 %i_add, ptr %self.1, align 4
  br label %merge

merge:                                            ; preds = %body, %"list<Review>::get.exit"
  %if_result = phi %"option<char>" [ %4, %"list<Review>::get.exit" ], [ zeroinitializer, %body ]
  ret %"option<char>" %if_result
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define i64 @"list<Review>::len"(ptr nocapture readonly %self) local_unnamed_addr #4 {
body:
  %self.0.1 = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load = load i64, ptr %self.0.1, align 4
  %0 = sdiv i64 %self.0.1.load, 32
  ret i64 %0
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define %"option<char>" @"option<Review>::none"() local_unnamed_addr #5 {
body:
  ret %"option<char>" zeroinitializer
}

define %Review @"list<Review>::get"(ptr nocapture readonly %self, i64 %i) local_unnamed_addr {
body:
  %i_le = icmp sgt i64 %i, -1
  br i1 %i_le, label %and_true, label %else

else:                                             ; preds = %body, %and_true
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.4.10, i64 25, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

merge:                                            ; preds = %and_true
  %self.0.0.load = load ptr, ptr %self, align 8
  %self.0.0.load.gep = getelementptr %Review, ptr %self.0.0.load, i64 %i
  %self.0.0.load.gep.deref.unpack = load i64, ptr %self.0.0.load.gep, align 8
  %2 = insertvalue %Review undef, i64 %self.0.0.load.gep.deref.unpack, 0
  %self.0.0.load.gep.deref.elt1 = getelementptr %Review, ptr %self.0.0.load, i64 %i, i32 1
  %self.0.0.load.gep.deref.unpack2.unpack.unpack = load ptr, ptr %self.0.0.load.gep.deref.elt1, align 8
  %3 = insertvalue %"#dynarray" undef, ptr %self.0.0.load.gep.deref.unpack2.unpack.unpack, 0
  %self.0.0.load.gep.deref.unpack2.unpack.elt5 = getelementptr inbounds %"#dynarray", ptr %self.0.0.load.gep.deref.elt1, i64 0, i32 1
  %self.0.0.load.gep.deref.unpack2.unpack.unpack6 = load i64, ptr %self.0.0.load.gep.deref.unpack2.unpack.elt5, align 8
  %4 = insertvalue %"#dynarray" %3, i64 %self.0.0.load.gep.deref.unpack2.unpack.unpack6, 1
  %self.0.0.load.gep.deref.unpack2.unpack.elt7 = getelementptr inbounds %"#dynarray", ptr %self.0.0.load.gep.deref.elt1, i64 0, i32 2
  %self.0.0.load.gep.deref.unpack2.unpack.unpack8 = load i64, ptr %self.0.0.load.gep.deref.unpack2.unpack.elt7, align 8
  %self.0.0.load.gep.deref.unpack2.unpack9 = insertvalue %"#dynarray" %4, i64 %self.0.0.load.gep.deref.unpack2.unpack.unpack8, 2
  %self.0.0.load.gep.deref.unpack24 = insertvalue %string undef, %"#dynarray" %self.0.0.load.gep.deref.unpack2.unpack9, 0
  %self.0.0.load.gep.deref3 = insertvalue %Review %2, %string %self.0.0.load.gep.deref.unpack24, 1
  ret %Review %self.0.0.load.gep.deref3

and_true:                                         ; preds = %body
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %5 = sdiv i64 %self.0.1.load.i, 32
  %i_lt = icmp sgt i64 %5, %i
  br i1 %i_lt, label %merge, label %else
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %"option<char>" @"option<Review>::some"(ptr nocapture readonly %t) local_unnamed_addr #3 {
body:
  %0 = tail call dereferenceable_or_null(32) ptr @malloc(i64 32)
  %t1.unpack = load i64, ptr %t, align 8
  %t1.elt1 = getelementptr inbounds %Review, ptr %t, i64 0, i32 1
  %t1.unpack2.unpack.unpack = load ptr, ptr %t1.elt1, align 8
  %t1.unpack2.unpack.elt5 = getelementptr inbounds %Review, ptr %t, i64 0, i32 1, i32 0, i32 1
  %t1.unpack2.unpack.unpack6 = load i64, ptr %t1.unpack2.unpack.elt5, align 8
  %t1.unpack2.unpack.elt7 = getelementptr inbounds %Review, ptr %t, i64 0, i32 1, i32 0, i32 2
  %t1.unpack2.unpack.unpack8 = load i64, ptr %t1.unpack2.unpack.elt7, align 8
  store i64 %t1.unpack, ptr %0, align 8
  %.repack10 = getelementptr inbounds %Review, ptr %0, i64 0, i32 1
  store ptr %t1.unpack2.unpack.unpack, ptr %.repack10, align 8
  %.repack10.repack12 = getelementptr inbounds %Review, ptr %0, i64 0, i32 1, i32 0, i32 1
  store i64 %t1.unpack2.unpack.unpack6, ptr %.repack10.repack12, align 8
  %.repack10.repack14 = getelementptr inbounds %Review, ptr %0, i64 0, i32 1, i32 0, i32 2
  store i64 %t1.unpack2.unpack.unpack8, ptr %.repack10.repack14, align 8
  %1 = insertvalue %"option<char>" { i1 true, ptr null }, ptr %0, 1
  ret %"option<char>" %1
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define %"listiterator<string>" @"listiterator<Review>::new"(ptr nocapture readonly %inner) local_unnamed_addr #4 {
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
  %2 = insertvalue %"listiterator<string>" zeroinitializer, %string %inner11, 0
  %3 = insertvalue %"listiterator<string>" %2, i64 0, 1
  ret %"listiterator<string>" %3
}

define %string @"option<Review>::to_string"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %self.0.load = load i1, ptr %self, align 1
  br i1 %self.0.load, label %then, label %else

then:                                             ; preds = %body
  %value = alloca %Review, align 8
  %self.1 = getelementptr inbounds %"option<char>", ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  %self.1.load.deref.unpack = load i64, ptr %self.1.load, align 8
  %self.1.load.deref.elt4 = getelementptr inbounds %Review, ptr %self.1.load, i64 0, i32 1
  %self.1.load.deref.unpack5.unpack.unpack = load ptr, ptr %self.1.load.deref.elt4, align 8
  %self.1.load.deref.unpack5.unpack.elt8 = getelementptr inbounds %Review, ptr %self.1.load, i64 0, i32 1, i32 0, i32 1
  %self.1.load.deref.unpack5.unpack.unpack9 = load i64, ptr %self.1.load.deref.unpack5.unpack.elt8, align 8
  %self.1.load.deref.unpack5.unpack.elt10 = getelementptr inbounds %Review, ptr %self.1.load, i64 0, i32 1, i32 0, i32 2
  %self.1.load.deref.unpack5.unpack.unpack11 = load i64, ptr %self.1.load.deref.unpack5.unpack.elt10, align 8
  store i64 %self.1.load.deref.unpack, ptr %value, align 8
  %value.repack13 = getelementptr inbounds %Review, ptr %value, i64 0, i32 1
  store ptr %self.1.load.deref.unpack5.unpack.unpack, ptr %value.repack13, align 8
  %value.repack13.repack15 = getelementptr inbounds %Review, ptr %value, i64 0, i32 1, i32 0, i32 1
  store i64 %self.1.load.deref.unpack5.unpack.unpack9, ptr %value.repack13.repack15, align 8
  %value.repack13.repack17 = getelementptr inbounds %Review, ptr %value, i64 0, i32 1, i32 0, i32 2
  store i64 %self.1.load.deref.unpack5.unpack.unpack11, ptr %value.repack13.repack17, align 8
  %0 = alloca [5 x i8], align 1
  store i8 115, ptr %0, align 1
  %.repack19 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 1
  store i8 111, ptr %.repack19, align 1
  %.repack20 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 2
  store i8 109, ptr %.repack20, align 1
  %.repack21 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 3
  store i8 101, ptr %.repack21, align 1
  %.repack22 = getelementptr inbounds [5 x i8], ptr %0, i64 0, i64 4
  store i8 40, ptr %.repack22, align 1
  %1 = call %string @"Review::to_string"(ptr nonnull %value)
  %2 = extractvalue %string %1, 0
  %.elt27 = extractvalue %"#dynarray" %2, 0
  %.elt29 = extractvalue %"#dynarray" %2, 1
  %i_add.i = add i64 %.elt29, 5
  %3 = tail call ptr @malloc(i64 %i_add.i)
  %i_lt.i.i21.i = icmp ugt i64 %.elt29, 9223372036854775802
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
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self.0.load.gep.i.i, ptr align 1 %.elt27, i64 %.elt29, i1 false)
  %8 = tail call dereferenceable_or_null(1) ptr @malloc(i64 1)
  store i8 41, ptr %8, align 1
  %i_add.i47 = add i64 %.elt29, 6
  %9 = tail call ptr @malloc(i64 %i_add.i47)
  %i_lt.i.i21.i49 = icmp slt i64 %i_add.i47, %i_add.i
  br i1 %i_lt.i.i21.i49, label %"#dynarray::extend.exit32.i55", label %"#dynarray::extend.exit32.thread.i51"

"#dynarray::extend.exit32.thread.i51":            ; preds = %"string::add_string.exit"
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %9, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i, i1 false)
  br label %"string::add_string.exit63"

"#dynarray::extend.exit32.i55":                   ; preds = %"string::add_string.exit"
  %i_mul.i.i22.i52 = shl i64 %i_add.i47, 1
  %10 = tail call i64 @llvm.smax.i64(i64 %i_add.i, i64 %i_mul.i.i22.i52)
  %11 = tail call ptr @malloc(i64 %10)
  tail call void @free(ptr %9)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %11, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i, i1 false)
  %i_lt.i.i.i54 = icmp slt i64 %10, %i_add.i47
  br i1 %i_lt.i.i.i54, label %then.i.i.i57, label %"string::add_string.exit63"

then.i.i.i57:                                     ; preds = %"#dynarray::extend.exit32.i55"
  %i_mul.i.i.i56 = shl i64 %10, 1
  %12 = tail call i64 @llvm.smax.i64(i64 %i_add.i47, i64 %i_mul.i.i.i56)
  %13 = tail call ptr @malloc(i64 %12)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %13, ptr nonnull align 1 %result_inner.sroa.0.1.i, i64 %i_add.i, i1 false)
  tail call void @free(ptr %11)
  br label %"string::add_string.exit63"

"string::add_string.exit63":                      ; preds = %"#dynarray::extend.exit32.thread.i51", %"#dynarray::extend.exit32.i55", %then.i.i.i57
  %result_inner.sroa.15.1.i59 = phi i64 [ %12, %then.i.i.i57 ], [ %10, %"#dynarray::extend.exit32.i55" ], [ %i_add.i47, %"#dynarray::extend.exit32.thread.i51" ]
  %result_inner.sroa.0.1.i60 = phi ptr [ %13, %then.i.i.i57 ], [ %11, %"#dynarray::extend.exit32.i55" ], [ %9, %"#dynarray::extend.exit32.thread.i51" ]
  %self.0.load.gep.i.i61 = getelementptr i8, ptr %result_inner.sroa.0.1.i60, i64 %i_add.i
  %14 = load i8, ptr %8, align 1
  store i8 %14, ptr %self.0.load.gep.i.i61, align 1
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

merge:                                            ; preds = %else, %"string::add_string.exit63"
  %result_inner.sroa.0.1.i60.pn = phi ptr [ %result_inner.sroa.0.1.i60, %"string::add_string.exit63" ], [ %16, %else ]
  %i_add.i47.pn = phi i64 [ %i_add.i47, %"string::add_string.exit63" ], [ 4, %else ]
  %result_inner.sroa.15.1.i59.pn = phi i64 [ %result_inner.sroa.15.1.i59, %"string::add_string.exit63" ], [ 4, %else ]
  %.pn67 = insertvalue %"#dynarray" undef, ptr %result_inner.sroa.0.1.i60.pn, 0
  %.pn = insertvalue %"#dynarray" %.pn67, i64 %i_add.i47.pn, 1
  %result_inner29.i62.pn = insertvalue %"#dynarray" %.pn, i64 %result_inner.sroa.15.1.i59.pn, 2
  %if_result = insertvalue %string zeroinitializer, %"#dynarray" %result_inner29.i62.pn, 0
  ret %string %if_result
}

define double @"ReviewAnalysis::get_average_rating"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %inner1.unpack.unpack.i.i = load ptr, ptr %self, align 8
  %inner1.unpack.elt2.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %inner1.unpack.unpack3.i.i = load i64, ptr %inner1.unpack.elt2.i.i, align 8
  %inner1.unpack.elt4.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %inner1.unpack.unpack5.i.i = load i64, ptr %inner1.unpack.elt4.i.i, align 8
  %0 = alloca %"listiterator<string>", align 8
  store ptr %inner1.unpack.unpack.i.i, ptr %0, align 8
  %.repack4 = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 1
  store i64 %inner1.unpack.unpack3.i.i, ptr %.repack4, align 8
  %.repack6 = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 2
  store i64 %inner1.unpack.unpack5.i.i, ptr %.repack6, align 8
  %.repack1 = getelementptr inbounds %"listiterator<string>", ptr %0, i64 0, i32 1
  store i64 0, ptr %.repack1, align 8
  %1 = call %"option<char>" @"listiterator<Review>::next"(ptr nonnull %0)
  %present24 = extractvalue %"option<char>" %1, 0
  br i1 %present24, label %for, label %post_for

for:                                              ; preds = %body, %for
  %2 = phi %"option<char>" [ %4, %for ], [ %1, %body ]
  %sum.025 = phi i64 [ %i_add, %for ], [ 0, %body ]
  %3 = extractvalue %"option<char>" %2, 1
  %.unpack = load i64, ptr %3, align 8
  %i_add = add i64 %.unpack, %sum.025
  %4 = call %"option<char>" @"listiterator<Review>::next"(ptr nonnull %0)
  %present = extractvalue %"option<char>" %4, 0
  br i1 %present, label %for, label %post_for.loopexit

post_for.loopexit:                                ; preds = %for
  %phi.cast = sitofp i64 %i_add to double
  br label %post_for

post_for:                                         ; preds = %post_for.loopexit, %body
  %sum.0.lcssa = phi double [ 0.000000e+00, %body ], [ %phi.cast, %post_for.loopexit ]
  %self.0.1.load.i = load i64, ptr %inner1.unpack.elt2.i.i, align 4
  %5 = sdiv i64 %self.0.1.load.i, 32
  %cast4 = sitofp i64 %5 to double
  %f_div = fdiv double %sum.0.lcssa, %cast4
  ret double %f_div
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %string @"list<string>::new"() local_unnamed_addr #3 {
body:
  %0 = tail call ptr @malloc(i64 0)
  %1 = insertvalue %"#dynarray" zeroinitializer, ptr %0, 0
  %2 = insertvalue %"#dynarray" %1, i64 0, 1
  %3 = insertvalue %"#dynarray" %2, i64 0, 2
  %4 = insertvalue %string zeroinitializer, %"#dynarray" %3, 0
  ret %string %4
}

define %string @"list<Review>::to_string"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %self.0.1.load.i.off = add i64 %self.0.1.load.i, 31
  %0 = icmp ult i64 %self.0.1.load.i.off, 63
  br i1 %0, label %then, label %else

then:                                             ; preds = %body
  %1 = alloca [2 x i8], align 2
  store i8 91, ptr %1, align 2
  %.repack84 = getelementptr inbounds [2 x i8], ptr %1, i64 0, i64 1
  store i8 93, ptr %.repack84, align 1
  %2 = tail call dereferenceable_or_null(2) ptr @malloc(i64 2)
  %3 = load i16, ptr %1, align 2
  store i16 %3, ptr %2, align 1
  br label %merge

else:                                             ; preds = %body
  %4 = tail call dereferenceable_or_null(1) ptr @malloc(i64 1)
  store i8 91, ptr %4, align 1
  %it = alloca %"listiterator<string>", align 8
  %inner1.unpack.unpack.i.i = load ptr, ptr %self, align 8
  %inner1.unpack.elt4.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %inner1.unpack.unpack5.i.i = load i64, ptr %inner1.unpack.elt4.i.i, align 8
  store ptr %inner1.unpack.unpack.i.i, ptr %it, align 8
  %it.repack9 = getelementptr inbounds %"#dynarray", ptr %it, i64 0, i32 1
  store i64 %self.0.1.load.i, ptr %it.repack9, align 8
  %it.repack11 = getelementptr inbounds %"#dynarray", ptr %it, i64 0, i32 2
  store i64 %inner1.unpack.unpack5.i.i, ptr %it.repack11, align 8
  %it.repack6 = getelementptr inbounds %"listiterator<string>", ptr %it, i64 0, i32 1
  store i64 0, ptr %it.repack6, align 8
  %5 = call %"option<char>" @"listiterator<Review>::next"(ptr nonnull %it)
  %6 = alloca %"option<char>", align 8
  store %"option<char>" %5, ptr %6, align 8
  %self.0.load.i = load i1, ptr %6, align 8
  br i1 %self.0.load.i, label %"option<Review>::get.exit", label %else.i

else.i:                                           ; preds = %else
  %stderr.i = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %7 = tail call i64 @fwrite(ptr nonnull @throw_msg.5.9, i64 16, i64 1, ptr %stderr.i)
  %8 = tail call i64 @fputwc(i32 10, ptr %stderr.i)
  tail call void @exit(i64 1)
  unreachable

"option<Review>::get.exit":                       ; preds = %else
  %self.1.i = getelementptr inbounds %"option<char>", ptr %6, i64 0, i32 1
  %self.1.load.i = load ptr, ptr %self.1.i, align 8
  %self.1.load.deref.unpack.i = load i64, ptr %self.1.load.i, align 8
  %self.1.load.deref.elt1.i = getelementptr inbounds %Review, ptr %self.1.load.i, i64 0, i32 1
  %self.1.load.deref.unpack2.unpack.unpack.i = load ptr, ptr %self.1.load.deref.elt1.i, align 8
  %self.1.load.deref.unpack2.unpack.elt5.i = getelementptr inbounds %Review, ptr %self.1.load.i, i64 0, i32 1, i32 0, i32 1
  %self.1.load.deref.unpack2.unpack.unpack6.i = load i64, ptr %self.1.load.deref.unpack2.unpack.elt5.i, align 8
  %self.1.load.deref.unpack2.unpack.elt7.i = getelementptr inbounds %Review, ptr %self.1.load.i, i64 0, i32 1, i32 0, i32 2
  %self.1.load.deref.unpack2.unpack.unpack8.i = load i64, ptr %self.1.load.deref.unpack2.unpack.elt7.i, align 8
  %9 = alloca %Review, align 8
  store i64 %self.1.load.deref.unpack.i, ptr %9, align 8
  %.repack14 = getelementptr inbounds %Review, ptr %9, i64 0, i32 1
  store ptr %self.1.load.deref.unpack2.unpack.unpack.i, ptr %.repack14, align 8
  %.repack14.repack17 = getelementptr inbounds %Review, ptr %9, i64 0, i32 1, i32 0, i32 1
  store i64 %self.1.load.deref.unpack2.unpack.unpack6.i, ptr %.repack14.repack17, align 8
  %.repack14.repack19 = getelementptr inbounds %Review, ptr %9, i64 0, i32 1, i32 0, i32 2
  store i64 %self.1.load.deref.unpack2.unpack.unpack8.i, ptr %.repack14.repack19, align 8
  %10 = call %string @"Review::to_string"(ptr nonnull %9)
  %11 = extractvalue %string %10, 0
  %.elt21 = extractvalue %"#dynarray" %11, 0
  %.elt23 = extractvalue %"#dynarray" %11, 1
  %i_add.i91 = add i64 %.elt23, 1
  %12 = tail call ptr @malloc(i64 %i_add.i91)
  %i_lt.i.i21.i93 = icmp ugt i64 %.elt23, 9223372036854775806
  br i1 %i_lt.i.i21.i93, label %"#dynarray::extend.exit32.i99", label %"#dynarray::extend.exit32.thread.i95"

"#dynarray::extend.exit32.thread.i95":            ; preds = %"option<Review>::get.exit"
  %13 = load i8, ptr %4, align 1
  store i8 %13, ptr %12, align 1
  br label %"string::add_string.exit107"

"#dynarray::extend.exit32.i99":                   ; preds = %"option<Review>::get.exit"
  %i_mul.i.i22.i96 = shl i64 %i_add.i91, 1
  %14 = tail call i64 @llvm.smax.i64(i64 %i_mul.i.i22.i96, i64 1)
  %15 = tail call ptr @malloc(i64 %14)
  tail call void @free(ptr %12)
  %16 = load i8, ptr %4, align 1
  store i8 %16, ptr %15, align 1
  %i_lt.i.i.i98 = icmp slt i64 %14, %i_add.i91
  br i1 %i_lt.i.i.i98, label %then.i.i.i101, label %"string::add_string.exit107"

then.i.i.i101:                                    ; preds = %"#dynarray::extend.exit32.i99"
  %i_mul.i.i.i100 = shl nuw i64 %14, 1
  %17 = tail call i64 @llvm.smax.i64(i64 %i_add.i91, i64 %i_mul.i.i.i100)
  %18 = tail call ptr @malloc(i64 %17)
  %19 = load i8, ptr %4, align 1
  store i8 %19, ptr %18, align 1
  tail call void @free(ptr nonnull %15)
  br label %"string::add_string.exit107"

"string::add_string.exit107":                     ; preds = %"#dynarray::extend.exit32.thread.i95", %"#dynarray::extend.exit32.i99", %then.i.i.i101
  %result_inner.sroa.0.1.i104 = phi ptr [ %18, %then.i.i.i101 ], [ %15, %"#dynarray::extend.exit32.i99" ], [ %12, %"#dynarray::extend.exit32.thread.i95" ]
  %self.0.load.gep.i.i105 = getelementptr i8, ptr %result_inner.sroa.0.1.i104, i64 1
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self.0.load.gep.i.i105, ptr align 1 %.elt21, i64 %.elt23, i1 false)
  %e = alloca %Review, align 8
  %20 = call %"option<char>" @"listiterator<Review>::next"(ptr nonnull %it)
  %present85 = extractvalue %"option<char>" %20, 0
  br i1 %present85, label %for.lr.ph, label %block5

for.lr.ph:                                        ; preds = %"string::add_string.exit107"
  %e.repack57 = getelementptr inbounds %Review, ptr %e, i64 0, i32 1
  %e.repack57.repack59 = getelementptr inbounds %Review, ptr %e, i64 0, i32 1, i32 0, i32 1
  %e.repack57.repack61 = getelementptr inbounds %Review, ptr %e, i64 0, i32 1, i32 0, i32 2
  br label %for

merge:                                            ; preds = %"string::add_string.exit", %then
  %.pn162 = phi ptr [ %2, %then ], [ %result_inner.sroa.0.1.i, %"string::add_string.exit" ]
  %.pn160 = phi i64 [ 2, %then ], [ %i_add.i, %"string::add_string.exit" ]
  %.pn158 = phi i64 [ 2, %then ], [ %result_inner.sroa.15.1.i, %"string::add_string.exit" ]
  %.pn159 = insertvalue %"#dynarray" undef, ptr %.pn162, 0
  %.pn = insertvalue %"#dynarray" %.pn159, i64 %.pn160, 1
  %inner69.i.pn = insertvalue %"#dynarray" %.pn, i64 %.pn158, 2
  %if_result = insertvalue %string zeroinitializer, %"#dynarray" %inner69.i.pn, 0
  ret %string %if_result

for:                                              ; preds = %for.lr.ph, %"string::add_string.exit150"
  %self.0.0.load.i114 = phi ptr [ %result_inner.sroa.0.1.i104, %for.lr.ph ], [ %result_inner.sroa.0.1.i147, %"string::add_string.exit150" ]
  %self.0.1.load.i.i110 = phi i64 [ %i_add.i91, %for.lr.ph ], [ %i_add.i134, %"string::add_string.exit150" ]
  %21 = phi %"option<char>" [ %20, %for.lr.ph ], [ %39, %"string::add_string.exit150" ]
  %22 = extractvalue %"option<char>" %21, 1
  %.unpack = load i64, ptr %22, align 8
  %.elt48 = getelementptr inbounds %Review, ptr %22, i64 0, i32 1
  %.unpack49.unpack.unpack = load ptr, ptr %.elt48, align 8
  %.unpack49.unpack.elt51 = getelementptr inbounds %Review, ptr %22, i64 0, i32 1, i32 0, i32 1
  %.unpack49.unpack.unpack52 = load i64, ptr %.unpack49.unpack.elt51, align 8
  %.unpack49.unpack.elt53 = getelementptr inbounds %Review, ptr %22, i64 0, i32 1, i32 0, i32 2
  %.unpack49.unpack.unpack54 = load i64, ptr %.unpack49.unpack.elt53, align 8
  store i64 %.unpack, ptr %e, align 8
  store ptr %.unpack49.unpack.unpack, ptr %e.repack57, align 8
  store i64 %.unpack49.unpack.unpack52, ptr %e.repack57.repack59, align 8
  store i64 %.unpack49.unpack.unpack54, ptr %e.repack57.repack61, align 8
  %23 = alloca [2 x i8], align 2
  store i8 44, ptr %23, align 2
  %.repack63 = getelementptr inbounds [2 x i8], ptr %23, i64 0, i64 1
  store i8 32, ptr %.repack63, align 1
  %24 = tail call dereferenceable_or_null(2) ptr @malloc(i64 2)
  %25 = load i16, ptr %23, align 2
  store i16 %25, ptr %24, align 1
  %i_add.i113 = add i64 %self.0.1.load.i.i110, 2
  %26 = tail call ptr @malloc(i64 %i_add.i113)
  %i_lt.i.i21.i115 = icmp sgt i64 %self.0.1.load.i.i110, 9223372036854775805
  br i1 %i_lt.i.i21.i115, label %"#dynarray::extend.exit32.i121", label %"#dynarray::extend.exit32.thread.i117"

"#dynarray::extend.exit32.thread.i117":           ; preds = %for
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %26, ptr align 1 %self.0.0.load.i114, i64 %self.0.1.load.i.i110, i1 false)
  br label %"string::add_string.exit129"

"#dynarray::extend.exit32.i121":                  ; preds = %for
  %i_mul.i.i22.i118 = shl i64 %i_add.i113, 1
  %27 = tail call i64 @llvm.smax.i64(i64 %self.0.1.load.i.i110, i64 %i_mul.i.i22.i118)
  %28 = tail call ptr @malloc(i64 %27)
  tail call void @free(ptr %26)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %28, ptr align 1 %self.0.0.load.i114, i64 %self.0.1.load.i.i110, i1 false)
  %i_lt.i.i.i120.not = icmp ult i64 %27, %i_add.i113
  br i1 %i_lt.i.i.i120.not, label %"string::add_string.exit129", label %then.i.i.i123

then.i.i.i123:                                    ; preds = %"#dynarray::extend.exit32.i121"
  %i_mul.i.i.i122 = shl nuw i64 %27, 1
  %29 = tail call i64 @llvm.smax.i64(i64 %i_add.i113, i64 %i_mul.i.i.i122)
  %30 = tail call ptr @malloc(i64 %29)
  tail call void @llvm.memmove.p0.p0.i64(ptr align 1 %30, ptr align 1 %self.0.0.load.i114, i64 %self.0.1.load.i.i110, i1 false)
  tail call void @free(ptr %28)
  br label %"string::add_string.exit129"

"string::add_string.exit129":                     ; preds = %"#dynarray::extend.exit32.thread.i117", %"#dynarray::extend.exit32.i121", %then.i.i.i123
  %result_inner.sroa.0.1.i126 = phi ptr [ %30, %then.i.i.i123 ], [ %28, %"#dynarray::extend.exit32.i121" ], [ %26, %"#dynarray::extend.exit32.thread.i117" ]
  %self.0.load.gep.i.i127 = getelementptr i8, ptr %result_inner.sroa.0.1.i126, i64 %self.0.1.load.i.i110
  %31 = load i16, ptr %24, align 1
  store i16 %31, ptr %self.0.load.gep.i.i127, align 1
  %32 = call %string @"Review::to_string"(ptr nonnull %e)
  %33 = extractvalue %string %32, 0
  %.elt74 = extractvalue %"#dynarray" %33, 0
  %.elt76 = extractvalue %"#dynarray" %33, 1
  %i_add.i134 = add i64 %.elt76, %i_add.i113
  %34 = tail call ptr @malloc(i64 %i_add.i134)
  %i_lt.i.i21.i136 = icmp slt i64 %i_add.i134, %i_add.i113
  br i1 %i_lt.i.i21.i136, label %"#dynarray::extend.exit32.i142", label %"#dynarray::extend.exit32.thread.i138"

"#dynarray::extend.exit32.thread.i138":           ; preds = %"string::add_string.exit129"
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %34, ptr nonnull align 1 %result_inner.sroa.0.1.i126, i64 %i_add.i113, i1 false)
  br label %"string::add_string.exit150"

"#dynarray::extend.exit32.i142":                  ; preds = %"string::add_string.exit129"
  %i_mul.i.i22.i139 = shl i64 %i_add.i134, 1
  %35 = tail call i64 @llvm.smax.i64(i64 %i_add.i113, i64 %i_mul.i.i22.i139)
  %36 = tail call ptr @malloc(i64 %35)
  tail call void @free(ptr %34)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %36, ptr nonnull align 1 %result_inner.sroa.0.1.i126, i64 %i_add.i113, i1 false)
  %i_lt.i.i.i141 = icmp slt i64 %35, %i_add.i134
  br i1 %i_lt.i.i.i141, label %then.i.i.i144, label %"string::add_string.exit150"

then.i.i.i144:                                    ; preds = %"#dynarray::extend.exit32.i142"
  %i_mul.i.i.i143 = shl i64 %35, 1
  %37 = tail call i64 @llvm.smax.i64(i64 %i_add.i134, i64 %i_mul.i.i.i143)
  %38 = tail call ptr @malloc(i64 %37)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %38, ptr nonnull align 1 %result_inner.sroa.0.1.i126, i64 %i_add.i113, i1 false)
  tail call void @free(ptr %36)
  br label %"string::add_string.exit150"

"string::add_string.exit150":                     ; preds = %"#dynarray::extend.exit32.thread.i138", %"#dynarray::extend.exit32.i142", %then.i.i.i144
  %result_inner.sroa.0.1.i147 = phi ptr [ %38, %then.i.i.i144 ], [ %36, %"#dynarray::extend.exit32.i142" ], [ %34, %"#dynarray::extend.exit32.thread.i138" ]
  %self.0.load.gep.i.i148 = getelementptr i8, ptr %result_inner.sroa.0.1.i147, i64 %i_add.i113
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self.0.load.gep.i.i148, ptr align 1 %.elt74, i64 %.elt76, i1 false)
  %39 = call %"option<char>" @"listiterator<Review>::next"(ptr nonnull %it)
  %present = extractvalue %"option<char>" %39, 0
  br i1 %present, label %for, label %block5

block5:                                           ; preds = %"string::add_string.exit150", %"string::add_string.exit107"
  %self.0.0.load.i = phi ptr [ %result_inner.sroa.0.1.i104, %"string::add_string.exit107" ], [ %result_inner.sroa.0.1.i147, %"string::add_string.exit150" ]
  %self.0.1.load.i.i = phi i64 [ %i_add.i91, %"string::add_string.exit107" ], [ %i_add.i134, %"string::add_string.exit150" ]
  %40 = tail call dereferenceable_or_null(1) ptr @malloc(i64 1)
  store i8 93, ptr %40, align 1
  %i_add.i = add i64 %self.0.1.load.i.i, 1
  %41 = tail call ptr @malloc(i64 %i_add.i)
  %i_lt.i.i21.i = icmp eq i64 %self.0.1.load.i.i, 9223372036854775807
  br i1 %i_lt.i.i21.i, label %"#dynarray::extend.exit32.i", label %"#dynarray::extend.exit32.thread.i"

"#dynarray::extend.exit32.thread.i":              ; preds = %block5
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %41, ptr align 1 %self.0.0.load.i, i64 %self.0.1.load.i.i, i1 false)
  br label %"string::add_string.exit"

"#dynarray::extend.exit32.i":                     ; preds = %block5
  %i_mul.i.i22.i = shl i64 %i_add.i, 1
  %42 = tail call i64 @llvm.smax.i64(i64 %self.0.1.load.i.i, i64 %i_mul.i.i22.i)
  %43 = tail call ptr @malloc(i64 %42)
  tail call void @free(ptr %41)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %43, ptr align 1 %self.0.0.load.i, i64 %self.0.1.load.i.i, i1 false)
  %i_lt.i.i.i.not = icmp ult i64 %42, %i_add.i
  br i1 %i_lt.i.i.i.not, label %"string::add_string.exit", label %then.i.i.i

then.i.i.i:                                       ; preds = %"#dynarray::extend.exit32.i"
  %i_mul.i.i.i = shl nuw i64 %42, 1
  %44 = tail call i64 @llvm.smax.i64(i64 %i_add.i, i64 %i_mul.i.i.i)
  %45 = tail call ptr @malloc(i64 %44)
  tail call void @llvm.memmove.p0.p0.i64(ptr align 1 %45, ptr align 1 %self.0.0.load.i, i64 %self.0.1.load.i.i, i1 false)
  tail call void @free(ptr %43)
  br label %"string::add_string.exit"

"string::add_string.exit":                        ; preds = %"#dynarray::extend.exit32.thread.i", %"#dynarray::extend.exit32.i", %then.i.i.i
  %result_inner.sroa.15.1.i = phi i64 [ -2, %then.i.i.i ], [ 9223372036854775807, %"#dynarray::extend.exit32.i" ], [ %i_add.i, %"#dynarray::extend.exit32.thread.i" ]
  %result_inner.sroa.0.1.i = phi ptr [ %45, %then.i.i.i ], [ %43, %"#dynarray::extend.exit32.i" ], [ %41, %"#dynarray::extend.exit32.thread.i" ]
  %self.0.load.gep.i.i = getelementptr i8, ptr %result_inner.sroa.0.1.i, i64 %self.0.1.load.i.i
  %46 = load i8, ptr %40, align 1
  store i8 %46, ptr %self.0.load.gep.i.i, align 1
  br label %merge
}

define %Review @"option<Review>::get"(ptr nocapture readonly %self) local_unnamed_addr {
body:
  %self.0.load = load i1, ptr %self, align 1
  br i1 %self.0.load, label %merge, label %else

else:                                             ; preds = %body
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.5.9, i64 16, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

merge:                                            ; preds = %body
  %self.1 = getelementptr inbounds %"option<char>", ptr %self, i64 0, i32 1
  %self.1.load = load ptr, ptr %self.1, align 8
  %self.1.load.deref.unpack = load i64, ptr %self.1.load, align 8
  %2 = insertvalue %Review undef, i64 %self.1.load.deref.unpack, 0
  %self.1.load.deref.elt1 = getelementptr inbounds %Review, ptr %self.1.load, i64 0, i32 1
  %self.1.load.deref.unpack2.unpack.unpack = load ptr, ptr %self.1.load.deref.elt1, align 8
  %3 = insertvalue %"#dynarray" undef, ptr %self.1.load.deref.unpack2.unpack.unpack, 0
  %self.1.load.deref.unpack2.unpack.elt5 = getelementptr inbounds %Review, ptr %self.1.load, i64 0, i32 1, i32 0, i32 1
  %self.1.load.deref.unpack2.unpack.unpack6 = load i64, ptr %self.1.load.deref.unpack2.unpack.elt5, align 8
  %4 = insertvalue %"#dynarray" %3, i64 %self.1.load.deref.unpack2.unpack.unpack6, 1
  %self.1.load.deref.unpack2.unpack.elt7 = getelementptr inbounds %Review, ptr %self.1.load, i64 0, i32 1, i32 0, i32 2
  %self.1.load.deref.unpack2.unpack.unpack8 = load i64, ptr %self.1.load.deref.unpack2.unpack.elt7, align 8
  %self.1.load.deref.unpack2.unpack9 = insertvalue %"#dynarray" %4, i64 %self.1.load.deref.unpack2.unpack.unpack8, 2
  %self.1.load.deref.unpack24 = insertvalue %string undef, %"#dynarray" %self.1.load.deref.unpack2.unpack9, 0
  %self.1.load.deref3 = insertvalue %Review %2, %string %self.1.load.deref.unpack24, 1
  ret %Review %self.1.load.deref3
}

define void @"list<Review>::set"(ptr nocapture readonly %self, i64 %i, ptr nocapture readonly %t) local_unnamed_addr {
body:
  %i_le = icmp sgt i64 %i, -1
  br i1 %i_le, label %and_true, label %else

else:                                             ; preds = %body, %and_true
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.4.10, i64 25, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

and_true:                                         ; preds = %body
  %self.0.1.i = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load.i = load i64, ptr %self.0.1.i, align 4
  %2 = sdiv i64 %self.0.1.load.i, 32
  %i_lt = icmp sgt i64 %2, %i
  br i1 %i_lt, label %block, label %else

block:                                            ; preds = %and_true
  %t6.unpack = load i64, ptr %t, align 8
  %t6.elt1 = getelementptr inbounds %Review, ptr %t, i64 0, i32 1
  %t6.unpack2.unpack.unpack = load ptr, ptr %t6.elt1, align 8
  %t6.unpack2.unpack.elt5 = getelementptr inbounds %Review, ptr %t, i64 0, i32 1, i32 0, i32 1
  %t6.unpack2.unpack.unpack6 = load i64, ptr %t6.unpack2.unpack.elt5, align 8
  %t6.unpack2.unpack.elt7 = getelementptr inbounds %Review, ptr %t, i64 0, i32 1, i32 0, i32 2
  %t6.unpack2.unpack.unpack8 = load i64, ptr %t6.unpack2.unpack.elt7, align 8
  %self.0.0.load = load ptr, ptr %self, align 8
  %self.0.0.load.gep = getelementptr %Review, ptr %self.0.0.load, i64 %i
  store i64 %t6.unpack, ptr %self.0.0.load.gep, align 8
  %self.0.0.load.gep.repack10 = getelementptr %Review, ptr %self.0.0.load, i64 %i, i32 1
  store ptr %t6.unpack2.unpack.unpack, ptr %self.0.0.load.gep.repack10, align 8
  %self.0.0.load.gep.repack10.repack12 = getelementptr inbounds %"#dynarray", ptr %self.0.0.load.gep.repack10, i64 0, i32 1
  store i64 %t6.unpack2.unpack.unpack6, ptr %self.0.0.load.gep.repack10.repack12, align 8
  %self.0.0.load.gep.repack10.repack14 = getelementptr inbounds %"#dynarray", ptr %self.0.0.load.gep.repack10, i64 0, i32 2
  store i64 %t6.unpack2.unpack.unpack8, ptr %self.0.0.load.gep.repack10.repack14, align 8
  ret void
}

define void @"list<Review>::extend"(ptr nocapture %self, ptr nocapture readonly %lst) local_unnamed_addr {
body:
  %inner1.unpack.unpack.i.i = load ptr, ptr %lst, align 8
  %inner1.unpack.elt2.i.i = getelementptr inbounds %"#dynarray", ptr %lst, i64 0, i32 1
  %inner1.unpack.unpack3.i.i = load i64, ptr %inner1.unpack.elt2.i.i, align 8
  %inner1.unpack.elt4.i.i = getelementptr inbounds %"#dynarray", ptr %lst, i64 0, i32 2
  %inner1.unpack.unpack5.i.i = load i64, ptr %inner1.unpack.elt4.i.i, align 8
  %0 = alloca %"listiterator<string>", align 8
  store ptr %inner1.unpack.unpack.i.i, ptr %0, align 8
  %.repack4 = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 1
  store i64 %inner1.unpack.unpack3.i.i, ptr %.repack4, align 8
  %.repack6 = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 2
  store i64 %inner1.unpack.unpack5.i.i, ptr %.repack6, align 8
  %.repack1 = getelementptr inbounds %"listiterator<string>", ptr %0, i64 0, i32 1
  store i64 0, ptr %.repack1, align 8
  %1 = call %"option<char>" @"listiterator<Review>::next"(ptr nonnull %0)
  %present24 = extractvalue %"option<char>" %1, 0
  br i1 %present24, label %for.preheader, label %post_for

for.preheader:                                    ; preds = %body
  %self.1.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %self.2.i.i.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  br label %for

for:                                              ; preds = %for.preheader, %"list<Review>::push.exit"
  %2 = phi %"option<char>" [ %6, %"list<Review>::push.exit" ], [ %1, %for.preheader ]
  %3 = extractvalue %"option<char>" %2, 1
  %.unpack = load i64, ptr %3, align 8
  %.elt9 = getelementptr inbounds %Review, ptr %3, i64 0, i32 1
  %.unpack10.unpack.unpack = load ptr, ptr %.elt9, align 8
  %.unpack10.unpack.elt12 = getelementptr inbounds %Review, ptr %3, i64 0, i32 1, i32 0, i32 1
  %.unpack10.unpack.unpack13 = load i64, ptr %.unpack10.unpack.elt12, align 8
  %.unpack10.unpack.elt14 = getelementptr inbounds %Review, ptr %3, i64 0, i32 1, i32 0, i32 2
  %.unpack10.unpack.unpack15 = load i64, ptr %.unpack10.unpack.elt14, align 8
  %self.1.load.i.i = load i64, ptr %self.1.i.i, align 4
  %i_add.i.i = add i64 %self.1.load.i.i, 32
  %self.2.load.i.i.i = load i64, ptr %self.2.i.i.i, align 4
  %i_lt.i.i.i = icmp slt i64 %self.2.load.i.i.i, %i_add.i.i
  br i1 %i_lt.i.i.i, label %then.i.i.i, label %"list<Review>::push.exit"

then.i.i.i:                                       ; preds = %for
  %i_mul.i.i.i = shl i64 %self.2.load.i.i.i, 1
  %4 = tail call i64 @llvm.smax.i64(i64 %i_add.i.i, i64 %i_mul.i.i.i)
  %self.0.load.i.i.i = load ptr, ptr %self, align 8
  %5 = tail call ptr @malloc(i64 %4)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %5, ptr align 1 %self.0.load.i.i.i, i64 %self.1.load.i.i, i1 false)
  store ptr %5, ptr %self, align 8
  store i64 %4, ptr %self.2.i.i.i, align 4
  tail call void @free(ptr %self.0.load.i.i.i)
  %self.14.load.pre.i.i = load i64, ptr %self.1.i.i, align 4
  br label %"list<Review>::push.exit"

"list<Review>::push.exit":                        ; preds = %for, %then.i.i.i
  %self.14.load.i.i = phi i64 [ %self.14.load.pre.i.i, %then.i.i.i ], [ %self.1.load.i.i, %for ]
  %self.0.load.i.i = load ptr, ptr %self, align 8
  %self.0.load.gep.i.i = getelementptr i8, ptr %self.0.load.i.i, i64 %self.14.load.i.i
  store i64 %.unpack, ptr %self.0.load.gep.i.i, align 1
  %alloca.Review.i.sroa.4.0.self.0.load.gep.i.i.sroa_idx = getelementptr inbounds i8, ptr %self.0.load.gep.i.i, i64 8
  store ptr %.unpack10.unpack.unpack, ptr %alloca.Review.i.sroa.4.0.self.0.load.gep.i.i.sroa_idx, align 1
  %alloca.Review.i.sroa.5.0.self.0.load.gep.i.i.sroa_idx = getelementptr inbounds i8, ptr %self.0.load.gep.i.i, i64 16
  store i64 %.unpack10.unpack.unpack13, ptr %alloca.Review.i.sroa.5.0.self.0.load.gep.i.i.sroa_idx, align 1
  %alloca.Review.i.sroa.6.0.self.0.load.gep.i.i.sroa_idx = getelementptr inbounds i8, ptr %self.0.load.gep.i.i, i64 24
  store i64 %.unpack10.unpack.unpack15, ptr %alloca.Review.i.sroa.6.0.self.0.load.gep.i.i.sroa_idx, align 1
  %self.18.load.i.i = load i64, ptr %self.1.i.i, align 4
  %i_add10.i.i = add i64 %self.18.load.i.i, 32
  store i64 %i_add10.i.i, ptr %self.1.i.i, align 4
  %6 = call %"option<char>" @"listiterator<Review>::next"(ptr nonnull %0)
  %present = extractvalue %"option<char>" %6, 0
  br i1 %present, label %for, label %post_for

post_for:                                         ; preds = %"list<Review>::push.exit", %body
  ret void
}

; Function Attrs: mustprogress nounwind willreturn
define void @"list<Review>::push"(ptr nocapture %self, ptr nocapture readonly %e) local_unnamed_addr #8 {
body:
  %e1.unpack = load i64, ptr %e, align 8
  %e1.elt1 = getelementptr inbounds %Review, ptr %e, i64 0, i32 1
  %e1.unpack2.unpack.unpack = load ptr, ptr %e1.elt1, align 8
  %e1.unpack2.unpack.elt5 = getelementptr inbounds %Review, ptr %e, i64 0, i32 1, i32 0, i32 1
  %e1.unpack2.unpack.unpack6 = load i64, ptr %e1.unpack2.unpack.elt5, align 8
  %e1.unpack2.unpack.elt7 = getelementptr inbounds %Review, ptr %e, i64 0, i32 1, i32 0, i32 2
  %e1.unpack2.unpack.unpack8 = load i64, ptr %e1.unpack2.unpack.elt7, align 8
  %self.1.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %self.1.load.i = load i64, ptr %self.1.i, align 4
  %i_add.i = add i64 %self.1.load.i, 32
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
  store i64 %e1.unpack, ptr %self.0.load.gep.i, align 1
  %alloca.Review.sroa.2.0.self.0.load.gep.i.sroa_idx = getelementptr inbounds i8, ptr %self.0.load.gep.i, i64 8
  store ptr %e1.unpack2.unpack.unpack, ptr %alloca.Review.sroa.2.0.self.0.load.gep.i.sroa_idx, align 1
  %alloca.Review.sroa.3.0.self.0.load.gep.i.sroa_idx = getelementptr inbounds i8, ptr %self.0.load.gep.i, i64 16
  store i64 %e1.unpack2.unpack.unpack6, ptr %alloca.Review.sroa.3.0.self.0.load.gep.i.sroa_idx, align 1
  %alloca.Review.sroa.4.0.self.0.load.gep.i.sroa_idx = getelementptr inbounds i8, ptr %self.0.load.gep.i, i64 24
  store i64 %e1.unpack2.unpack.unpack8, ptr %alloca.Review.sroa.4.0.self.0.load.gep.i.sroa_idx, align 1
  %self.18.load.i = load i64, ptr %self.1.i, align 4
  %i_add10.i = add i64 %self.18.load.i, 32
  store i64 %i_add10.i, ptr %self.1.i, align 4
  ret void
}

define %Review @"list<Review>::pop"(ptr nocapture %self) local_unnamed_addr {
body:
  %self.1.i = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %self.1.load.i = load i64, ptr %self.1.i, align 4
  %i_ge.not.i = icmp slt i64 %self.1.load.i, 32
  br i1 %i_ge.not.i, label %else.i, label %"#dynarray::take.exit"

else.i:                                           ; preds = %body
  %stderr.i = tail call ptr @fdopen(i64 2, ptr nonnull @_write.1)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.2, i64 30, i64 1, ptr %stderr.i)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr.i)
  tail call void @exit(i64 1)
  unreachable

"#dynarray::take.exit":                           ; preds = %body
  %i_sub.i = add nsw i64 %self.1.load.i, -32
  store i64 %i_sub.i, ptr %self.1.i, align 4
  %self.0.load.i = load ptr, ptr %self, align 8
  %self.0.load.gep.i = getelementptr i8, ptr %self.0.load.i, i64 %i_sub.i
  %.deref.unpack = load i64, ptr %self.0.load.gep.i, align 8
  %2 = insertvalue %Review undef, i64 %.deref.unpack, 0
  %.deref.elt1 = getelementptr inbounds %Review, ptr %self.0.load.gep.i, i64 0, i32 1
  %.deref.unpack2.unpack.unpack = load ptr, ptr %.deref.elt1, align 8
  %3 = insertvalue %"#dynarray" undef, ptr %.deref.unpack2.unpack.unpack, 0
  %.deref.unpack2.unpack.elt5 = getelementptr inbounds %Review, ptr %self.0.load.gep.i, i64 0, i32 1, i32 0, i32 1
  %.deref.unpack2.unpack.unpack6 = load i64, ptr %.deref.unpack2.unpack.elt5, align 8
  %4 = insertvalue %"#dynarray" %3, i64 %.deref.unpack2.unpack.unpack6, 1
  %.deref.unpack2.unpack.elt7 = getelementptr inbounds %Review, ptr %self.0.load.gep.i, i64 0, i32 1, i32 0, i32 2
  %.deref.unpack2.unpack.unpack8 = load i64, ptr %.deref.unpack2.unpack.elt7, align 8
  %.deref.unpack2.unpack9 = insertvalue %"#dynarray" %4, i64 %.deref.unpack2.unpack.unpack8, 2
  %.deref.unpack24 = insertvalue %string undef, %"#dynarray" %.deref.unpack2.unpack9, 0
  %.deref3 = insertvalue %Review %2, %string %.deref.unpack24, 1
  ret %Review %.deref3
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %string @"list<Review>::from_raw"(ptr nocapture readonly %contents, i64 %len) local_unnamed_addr #3 {
body:
  %i_mul = shl i64 %len, 5
  %0 = tail call ptr @malloc(i64 %i_mul)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %0, ptr align 1 %contents, i64 %i_mul, i1 false)
  %1 = insertvalue %"#dynarray" undef, ptr %0, 0
  %2 = insertvalue %"#dynarray" %1, i64 %i_mul, 1
  %inner79 = insertvalue %"#dynarray" %2, i64 %i_mul, 2
  %3 = insertvalue %string zeroinitializer, %"#dynarray" %inner79, 0
  ret %string %3
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %string @"list<Review>::new"() local_unnamed_addr #3 {
body:
  %0 = tail call ptr @malloc(i64 0)
  %1 = insertvalue %"#dynarray" zeroinitializer, ptr %0, 0
  %2 = insertvalue %"#dynarray" %1, i64 0, 1
  %3 = insertvalue %"#dynarray" %2, i64 0, 2
  %4 = insertvalue %string zeroinitializer, %"#dynarray" %3, 0
  ret %string %4
}

define i8 @main() local_unnamed_addr {
body:
  %0 = tail call ptr @setlocale(i64 0, ptr nonnull @locale.11)
  tail call fastcc void @main.inner()
  ret i8 0
}

define private fastcc void @main.inner() unnamed_addr {
body:
  %alloca.ptr.i = alloca ptr, align 8
  %0 = alloca %"listiterator<string>", align 8
  %analysis = alloca %ReviewAnalysis, align 8
  %1 = tail call dereferenceable_or_null(9) ptr @malloc(i64 9)
  store i8 71, ptr %1, align 1
  %.sroa.2127.0..sroa_idx = getelementptr inbounds i8, ptr %1, i64 1
  store i8 111, ptr %.sroa.2127.0..sroa_idx, align 1
  %.sroa.3128.0..sroa_idx = getelementptr inbounds i8, ptr %1, i64 2
  store i8 111, ptr %.sroa.3128.0..sroa_idx, align 1
  %.sroa.4129.0..sroa_idx = getelementptr inbounds i8, ptr %1, i64 3
  store i8 100, ptr %.sroa.4129.0..sroa_idx, align 1
  %.sroa.5130.0..sroa_idx = getelementptr inbounds i8, ptr %1, i64 4
  store i8 33, ptr %.sroa.5130.0..sroa_idx, align 1
  %.sroa.6131.0..sroa_idx = getelementptr inbounds i8, ptr %1, i64 5
  store i8 32, ptr %.sroa.6131.0..sroa_idx, align 1
  %.sroa.7132.0..sroa_idx = getelementptr inbounds i8, ptr %1, i64 6
  store i8 84, ptr %.sroa.7132.0..sroa_idx, align 1
  %.sroa.8133.0..sroa_idx = getelementptr inbounds i8, ptr %1, i64 7
  store i8 104, ptr %.sroa.8133.0..sroa_idx, align 1
  %.sroa.9134.0..sroa_idx = getelementptr inbounds i8, ptr %1, i64 8
  store i8 120, ptr %.sroa.9134.0..sroa_idx, align 1
  %2 = tail call dereferenceable_or_null(7) ptr @malloc(i64 7)
  store i8 79, ptr %2, align 1
  %.sroa.2120.0..sroa_idx = getelementptr inbounds i8, ptr %2, i64 1
  store i8 75, ptr %.sroa.2120.0..sroa_idx, align 1
  %.sroa.3121.0..sroa_idx = getelementptr inbounds i8, ptr %2, i64 2
  store i8 32, ptr %.sroa.3121.0..sroa_idx, align 1
  %.sroa.4122.0..sroa_idx = getelementptr inbounds i8, ptr %2, i64 3
  store i8 115, ptr %.sroa.4122.0..sroa_idx, align 1
  %.sroa.5123.0..sroa_idx = getelementptr inbounds i8, ptr %2, i64 4
  store i8 105, ptr %.sroa.5123.0..sroa_idx, align 1
  %.sroa.6124.0..sroa_idx = getelementptr inbounds i8, ptr %2, i64 5
  store i8 116, ptr %.sroa.6124.0..sroa_idx, align 1
  %.sroa.7125.0..sroa_idx = getelementptr inbounds i8, ptr %2, i64 6
  store i8 101, ptr %.sroa.7125.0..sroa_idx, align 1
  %3 = tail call dereferenceable_or_null(6) ptr @malloc(i64 6)
  store i8 71, ptr %3, align 1
  %.sroa.2114.0..sroa_idx = getelementptr inbounds i8, ptr %3, i64 1
  store i8 114, ptr %.sroa.2114.0..sroa_idx, align 1
  %.sroa.3115.0..sroa_idx = getelementptr inbounds i8, ptr %3, i64 2
  store i8 101, ptr %.sroa.3115.0..sroa_idx, align 1
  %.sroa.4116.0..sroa_idx = getelementptr inbounds i8, ptr %3, i64 3
  store i8 97, ptr %.sroa.4116.0..sroa_idx, align 1
  %.sroa.5117.0..sroa_idx = getelementptr inbounds i8, ptr %3, i64 4
  store i8 116, ptr %.sroa.5117.0..sroa_idx, align 1
  %.sroa.6118.0..sroa_idx = getelementptr inbounds i8, ptr %3, i64 5
  store i8 33, ptr %.sroa.6118.0..sroa_idx, align 1
  %4 = tail call dereferenceable_or_null(10) ptr @malloc(i64 10)
  store i8 80, ptr %4, align 1
  %.sroa.2104.0..sroa_idx = getelementptr inbounds i8, ptr %4, i64 1
  store i8 111, ptr %.sroa.2104.0..sroa_idx, align 1
  %.sroa.3105.0..sroa_idx = getelementptr inbounds i8, ptr %4, i64 2
  store i8 111, ptr %.sroa.3105.0..sroa_idx, align 1
  %.sroa.4106.0..sroa_idx = getelementptr inbounds i8, ptr %4, i64 3
  store i8 114, ptr %.sroa.4106.0..sroa_idx, align 1
  %.sroa.5107.0..sroa_idx = getelementptr inbounds i8, ptr %4, i64 4
  store i8 33, ptr %.sroa.5107.0..sroa_idx, align 1
  %.sroa.6108.0..sroa_idx = getelementptr inbounds i8, ptr %4, i64 5
  store i8 32, ptr %.sroa.6108.0..sroa_idx, align 1
  %.sroa.7109.0..sroa_idx = getelementptr inbounds i8, ptr %4, i64 6
  store i8 66, ptr %.sroa.7109.0..sroa_idx, align 1
  %.sroa.8110.0..sroa_idx = getelementptr inbounds i8, ptr %4, i64 7
  store i8 97, ptr %.sroa.8110.0..sroa_idx, align 1
  %.sroa.9111.0..sroa_idx = getelementptr inbounds i8, ptr %4, i64 8
  store i8 100, ptr %.sroa.9111.0..sroa_idx, align 1
  %.sroa.10112.0..sroa_idx = getelementptr inbounds i8, ptr %4, i64 9
  store i8 46, ptr %.sroa.10112.0..sroa_idx, align 1
  %5 = tail call ptr @malloc(i64 0)
  %6 = tail call dereferenceable_or_null(160) ptr @malloc(i64 160)
  store i64 4, ptr %6, align 1
  %.sroa.2.0.self.0.load.i.i.sroa_idx = getelementptr inbounds i8, ptr %6, i64 8
  store ptr %1, ptr %.sroa.2.0.self.0.load.i.i.sroa_idx, align 1
  %.sroa.3.0.self.0.load.i.i.sroa_idx = getelementptr inbounds i8, ptr %6, i64 16
  store i64 9, ptr %.sroa.3.0.self.0.load.i.i.sroa_idx, align 1
  %.sroa.4.0.self.0.load.i.i.sroa_idx = getelementptr inbounds i8, ptr %6, i64 24
  store i64 9, ptr %.sroa.4.0.self.0.load.i.i.sroa_idx, align 1
  %.sroa.5.0.self.0.load.i.i.sroa_idx = getelementptr inbounds i8, ptr %6, i64 32
  store i64 3, ptr %.sroa.5.0.self.0.load.i.i.sroa_idx, align 1
  %.sroa.6.0.self.0.load.i.i.sroa_idx = getelementptr inbounds i8, ptr %6, i64 40
  store ptr %2, ptr %.sroa.6.0.self.0.load.i.i.sroa_idx, align 1
  %.sroa.7.0.self.0.load.i.i.sroa_idx = getelementptr inbounds i8, ptr %6, i64 48
  store i64 7, ptr %.sroa.7.0.self.0.load.i.i.sroa_idx, align 1
  %.sroa.8.0.self.0.load.i.i.sroa_idx = getelementptr inbounds i8, ptr %6, i64 56
  store i64 7, ptr %.sroa.8.0.self.0.load.i.i.sroa_idx, align 1
  %.sroa.9.0.self.0.load.i.i.sroa_idx = getelementptr inbounds i8, ptr %6, i64 64
  store i64 5, ptr %.sroa.9.0.self.0.load.i.i.sroa_idx, align 1
  %.sroa.10.0.self.0.load.i.i.sroa_idx = getelementptr inbounds i8, ptr %6, i64 72
  store ptr %3, ptr %.sroa.10.0.self.0.load.i.i.sroa_idx, align 1
  %.sroa.11.0.self.0.load.i.i.sroa_idx = getelementptr inbounds i8, ptr %6, i64 80
  store i64 6, ptr %.sroa.11.0.self.0.load.i.i.sroa_idx, align 1
  %.sroa.12.0.self.0.load.i.i.sroa_idx = getelementptr inbounds i8, ptr %6, i64 88
  store i64 6, ptr %.sroa.12.0.self.0.load.i.i.sroa_idx, align 1
  %.sroa.13.0.self.0.load.i.i.sroa_idx = getelementptr inbounds i8, ptr %6, i64 96
  store i64 2, ptr %.sroa.13.0.self.0.load.i.i.sroa_idx, align 1
  %.sroa.14.0.self.0.load.i.i.sroa_idx = getelementptr inbounds i8, ptr %6, i64 104
  store ptr %4, ptr %.sroa.14.0.self.0.load.i.i.sroa_idx, align 1
  %.sroa.15.0.self.0.load.i.i.sroa_idx = getelementptr inbounds i8, ptr %6, i64 112
  store i64 10, ptr %.sroa.15.0.self.0.load.i.i.sroa_idx, align 1
  %.sroa.16.0.self.0.load.i.i.sroa_idx = getelementptr inbounds i8, ptr %6, i64 120
  store i64 10, ptr %.sroa.16.0.self.0.load.i.i.sroa_idx, align 1
  %.sroa.17.0.self.0.load.i.i.sroa_idx = getelementptr inbounds i8, ptr %6, i64 128
  store i64 3, ptr %.sroa.17.0.self.0.load.i.i.sroa_idx, align 1
  %.sroa.18.0.self.0.load.i.i.sroa_idx = getelementptr inbounds i8, ptr %6, i64 136
  store ptr %5, ptr %.sroa.18.0.self.0.load.i.i.sroa_idx, align 1
  %.sroa.19.0.self.0.load.i.i.sroa_idx = getelementptr inbounds i8, ptr %6, i64 144
  call void @llvm.memset.p0.i64(ptr noundef nonnull align 1 dereferenceable(16) %.sroa.19.0.self.0.load.i.i.sroa_idx, i8 0, i64 16, i1 false)
  store ptr %6, ptr %analysis, align 8
  %analysis.repack76 = getelementptr inbounds %"#dynarray", ptr %analysis, i64 0, i32 1
  store i64 160, ptr %analysis.repack76, align 8
  %analysis.repack78 = getelementptr inbounds %"#dynarray", ptr %analysis, i64 0, i32 2
  store i64 160, ptr %analysis.repack78, align 8
  call void @llvm.lifetime.start.p0(i64 32, ptr nonnull %0)
  store ptr %6, ptr %0, align 8
  %.repack4.i = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 1
  store i64 160, ptr %.repack4.i, align 8
  %.repack6.i = getelementptr inbounds %"#dynarray", ptr %0, i64 0, i32 2
  store i64 160, ptr %.repack6.i, align 8
  %.repack1.i = getelementptr inbounds %"listiterator<string>", ptr %0, i64 0, i32 1
  store i64 0, ptr %.repack1.i, align 8
  %7 = call %"option<char>" @"listiterator<Review>::next"(ptr nonnull %0)
  %present24.i = extractvalue %"option<char>" %7, 0
  br i1 %present24.i, label %for.i, label %"ReviewAnalysis::get_average_rating.exit"

for.i:                                            ; preds = %body, %for.i
  %8 = phi %"option<char>" [ %10, %for.i ], [ %7, %body ]
  %sum.025.i = phi i64 [ %i_add.i, %for.i ], [ 0, %body ]
  %9 = extractvalue %"option<char>" %8, 1
  %.unpack.i = load i64, ptr %9, align 8
  %i_add.i = add i64 %.unpack.i, %sum.025.i
  %10 = call %"option<char>" @"listiterator<Review>::next"(ptr nonnull %0)
  %present.i = extractvalue %"option<char>" %10, 0
  br i1 %present.i, label %for.i, label %post_for.loopexit.i

post_for.loopexit.i:                              ; preds = %for.i
  %phi.cast.i = sitofp i64 %i_add.i to double
  br label %"ReviewAnalysis::get_average_rating.exit"

"ReviewAnalysis::get_average_rating.exit":        ; preds = %body, %post_for.loopexit.i
  %sum.0.lcssa.i = phi double [ 0.000000e+00, %body ], [ %phi.cast.i, %post_for.loopexit.i ]
  %f_div.i = fdiv double %sum.0.lcssa.i, 5.000000e+00
  call void @llvm.lifetime.end.p0(i64 32, ptr nonnull %0)
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %alloca.ptr.i)
  %11 = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %alloca.ptr.i, ptr nonnull @_tmpl_float_to_string, double %f_div.i)
  %buf_ptr4.deref.i = load ptr, ptr %alloca.ptr.i, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %alloca.ptr.i)
  %12 = call i64 (ptr, ...) @printf(ptr nonnull @_tmpl_print, i64 %11, ptr %buf_ptr4.deref.i)
  %13 = call %string @"ReviewAnalysis::collect_comments"(ptr nonnull %analysis)
  %14 = alloca %string, align 8
  %15 = extractvalue %string %13, 0
  %.elt85 = extractvalue %"#dynarray" %15, 0
  store ptr %.elt85, ptr %14, align 8
  %.repack86 = getelementptr inbounds %"#dynarray", ptr %14, i64 0, i32 1
  %.elt87 = extractvalue %"#dynarray" %15, 1
  store i64 %.elt87, ptr %.repack86, align 8
  %.repack88 = getelementptr inbounds %"#dynarray", ptr %14, i64 0, i32 2
  %.elt89 = extractvalue %"#dynarray" %15, 2
  store i64 %.elt89, ptr %.repack88, align 8
  %16 = call %string @"list<string>::to_string"(ptr nonnull %14)
  %17 = extractvalue %string %16, 0
  %.elt90 = extractvalue %"#dynarray" %17, 0
  %.elt92 = extractvalue %"#dynarray" %17, 1
  %18 = call i64 (ptr, ...) @printf(ptr nonnull @_tmpl_print, i64 %.elt92, ptr %.elt90)
  ret void
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
