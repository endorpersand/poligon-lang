; ModuleID = 'std.gon'
source_filename = "std.gon"

%string = type { %"#dynarray" }
%"#dynarray" = type { ptr, i64, i64 }

@_tmpl_print = private unnamed_addr constant [6 x i8] c"%.*s\0A\00", align 1
@_tmpl_int_to_string = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@_tmpl_float_to_string = private unnamed_addr constant [4 x i8] c"%#f\00", align 1
@_tmpl_char_to_string = private unnamed_addr constant [4 x i8] c"%lc\00", align 1
@throw_msg = private unnamed_addr constant [31 x i8] c"cannot take element from array\00", align 1
@_write = private unnamed_addr constant [2 x i8] c"w\00", align 1
@throw_msg.1 = private unnamed_addr constant [23 x i8] c"division by zero error\00", align 1
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

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define noalias ptr @"#ptr::null"() local_unnamed_addr #1 {
body:
  ret ptr null
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define %string @"string::to_string"(ptr nocapture readonly %self) local_unnamed_addr #2 {
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
define %string @"string::add_string"(ptr nocapture readonly %self, ptr nocapture readonly %other) local_unnamed_addr #3 {
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
  %other.0.0.load42 = load ptr, ptr %other, align 8
  %gep.i = getelementptr i8, ptr %.sink, i64 %self.0.1.load.i
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %gep.i, ptr align 1 %other.0.0.load42, i64 %self.0.1.load.i11, i1 false)
  %2 = insertvalue %"#dynarray" undef, ptr %.sink, 0
  %3 = insertvalue %"#dynarray" %2, i64 %i_add, 1
  %4 = insertvalue %"#dynarray" %3, i64 %result_inner.sroa.15.1, 2
  %5 = insertvalue %string zeroinitializer, %"#dynarray" %4, 0
  ret %string %5
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define i64 @"string::len"(ptr nocapture readonly %self) local_unnamed_addr #2 {
body:
  %self.0.1 = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %self.0.1.load = load i64, ptr %self.0.1, align 4
  ret i64 %self.0.1.load
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %"#dynarray" @"#dynarray::new"(i64 %cap) local_unnamed_addr #4 {
body:
  %0 = tail call ptr @malloc(i64 %cap)
  %1 = insertvalue %"#dynarray" zeroinitializer, ptr %0, 0
  %2 = insertvalue %"#dynarray" %1, i64 0, 1
  %3 = insertvalue %"#dynarray" %2, i64 %cap, 2
  ret %"#dynarray" %3
}

; Function Attrs: mustprogress nounwind willreturn
define void @"#dynarray::extend"(ptr nocapture %self, ptr nocapture readonly %add_buf, i64 %add_len) local_unnamed_addr #3 {
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
define void @"#dynarray::resize"(ptr nocapture %self, i64 %new_cap) local_unnamed_addr #3 {
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

merge:                                            ; preds = %body, %then
  ret void
}

; Function Attrs: inaccessiblememonly mustprogress nofree nounwind willreturn allockind("alloc,uninitialized") allocsize(0)
declare noalias noundef ptr @malloc(i64 noundef) local_unnamed_addr #5

; Function Attrs: inaccessiblemem_or_argmemonly mustprogress nounwind willreturn allockind("free")
declare void @free(ptr allocptr nocapture noundef) local_unnamed_addr #6

; Function Attrs: mustprogress nofree nounwind willreturn
define %string @"string::from_raw"(ptr nocapture readonly %contents, i64 %len) local_unnamed_addr #4 {
body:
  %0 = tail call ptr @malloc(i64 %len)
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %0, ptr align 1 %contents, i64 %len, i1 false)
  %1 = insertvalue %"#dynarray" undef, ptr %0, 0
  %2 = insertvalue %"#dynarray" %1, i64 %len, 1
  %3 = insertvalue %"#dynarray" %2, i64 %len, 2
  %4 = insertvalue %string zeroinitializer, %"#dynarray" %3, 0
  ret %string %4
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %string @"string::new"() local_unnamed_addr #4 {
body:
  %0 = tail call ptr @malloc(i64 0)
  %1 = insertvalue %"#dynarray" zeroinitializer, ptr %0, 0
  %2 = insertvalue %"#dynarray" %1, i64 0, 1
  %3 = insertvalue %"#dynarray" %2, i64 0, 2
  %4 = insertvalue %string zeroinitializer, %"#dynarray" %3, 0
  ret %string %4
}

define ptr @"#dynarray::take"(ptr nocapture %self, i64 %sub_len) local_unnamed_addr {
body:
  %self.1 = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %self.1.load = load i64, ptr %self.1, align 4
  %i_ge.not = icmp slt i64 %self.1.load, %sub_len
  br i1 %i_ge.not, label %else, label %block

else:                                             ; preds = %body
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg, i64 30, i64 1, ptr %stderr)
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

; Function Attrs: nofree nounwind
declare noalias noundef ptr @fdopen(i64 noundef, ptr nocapture noundef readonly) local_unnamed_addr #0

declare i64 @fputwc(i32, ptr) local_unnamed_addr

declare void @exit(i64) local_unnamed_addr

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

declare i64 @asprintf(ptr, ptr, ...) local_unnamed_addr

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
define double @"float::sign"(double %self) local_unnamed_addr #7 {
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
define double @"float::round"(double %self) local_unnamed_addr #7 {
body:
  %0 = tail call double @llvm.round.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::trunc"(double %self) local_unnamed_addr #7 {
body:
  %0 = tail call double @llvm.trunc.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.trunc.f64(double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::floor"(double %self) local_unnamed_addr #7 {
body:
  %0 = tail call double @llvm.floor.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.floor.f64(double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::ceil"(double %self) local_unnamed_addr #7 {
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
define double @"float::cos"(double %self) local_unnamed_addr #7 {
body:
  %0 = tail call double @llvm.cos.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.cos.f64(double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::sin"(double %self) local_unnamed_addr #7 {
body:
  %0 = tail call double @llvm.sin.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.sin.f64(double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::powi"(double %self, i64 %exp) local_unnamed_addr #7 {
body:
  %cast = sitofp i64 %exp to double
  %0 = tail call double @llvm.pow.f64(double %self, double %cast)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.pow.f64(double, double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::pow"(double %self, double %exp) local_unnamed_addr #7 {
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
define double @"float::sqrt"(double %self) local_unnamed_addr #7 {
body:
  %0 = tail call double @llvm.sqrt.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.sqrt.f64(double) #8

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::log1p"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @log1p(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @log1p(double) local_unnamed_addr #9

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::log10"(double %self) local_unnamed_addr #7 {
body:
  %0 = tail call double @llvm.log10.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.log10.f64(double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::log2"(double %self) local_unnamed_addr #7 {
body:
  %0 = tail call double @llvm.log2.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.log2.f64(double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::log"(double %self) local_unnamed_addr #7 {
body:
  %0 = tail call double @llvm.log.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.log.f64(double) #8

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::expm1"(double %self) local_unnamed_addr #9 {
body:
  %0 = tail call double @expm1(double %self)
  ret double %0
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @expm1(double) local_unnamed_addr #9

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::exp2"(double %self) local_unnamed_addr #7 {
body:
  %0 = tail call double @llvm.exp2.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.exp2.f64(double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::exp"(double %self) local_unnamed_addr #7 {
body:
  %0 = tail call double @llvm.exp.f64(double %self)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.exp.f64(double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::min"(double %self, double %o) local_unnamed_addr #7 {
body:
  %0 = tail call double @llvm.minnum.f64(double %self, double %o)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.minnum.f64(double, double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::max"(double %self, double %o) local_unnamed_addr #7 {
body:
  %0 = tail call double @llvm.maxnum.f64(double %self, double %o)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.maxnum.f64(double, double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::fma"(double %self, double %multiplicand, double %addend) local_unnamed_addr #7 {
body:
  %0 = tail call double @llvm.fma.f64(double %self, double %multiplicand, double %addend)
  ret double %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.fma.f64(double, double, double) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::abs"(double %self) local_unnamed_addr #7 {
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
define i64 @"int::trailing_zeroes"(i64 %self) local_unnamed_addr #7 {
body:
  %0 = tail call i64 @llvm.cttz.i64(i64 %self, i1 false), !range !0
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.cttz.i64(i64, i1 immarg) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::leading_zeroes"(i64 %self) local_unnamed_addr #7 {
body:
  %0 = tail call i64 @llvm.ctlz.i64(i64 %self, i1 false), !range !0
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.ctlz.i64(i64, i1 immarg) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::reverse_bytes"(i64 %self) local_unnamed_addr #7 {
body:
  %0 = tail call i64 @llvm.bswap.i64(i64 %self)
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.bswap.i64(i64) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::reverse_bits"(i64 %self) local_unnamed_addr #7 {
body:
  %0 = tail call i64 @llvm.bitreverse.i64(i64 %self)
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.bitreverse.i64(i64) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::count_ones"(i64 %self) local_unnamed_addr #7 {
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
define i64 @"int::min"(i64 %self, i64 %o) local_unnamed_addr #7 {
body:
  %0 = tail call i64 @llvm.smin.i64(i64 %self, i64 %o)
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.smin.i64(i64, i64) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::max"(i64 %self, i64 %o) local_unnamed_addr #7 {
body:
  %0 = tail call i64 @llvm.smax.i64(i64 %self, i64 %o)
  ret i64 %0
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.smax.i64(i64, i64) #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::abs"(i64 %self) local_unnamed_addr #7 {
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
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg.1, i64 22, i64 1, ptr %stderr)
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
init:
  %0 = tail call ptr @setlocale(i64 0, ptr nonnull @locale)
  ret i8 0
}

declare ptr @setlocale(i64, ptr) local_unnamed_addr

; Function Attrs: argmemonly mustprogress nocallback nofree nounwind willreturn
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #10

; Function Attrs: nofree nounwind
declare noundef i64 @fwrite(ptr nocapture noundef, i64 noundef, i64 noundef, ptr nocapture noundef) local_unnamed_addr #0

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.round.f64(double) #8

attributes #0 = { nofree nounwind }
attributes #1 = { mustprogress nofree norecurse nosync nounwind readnone willreturn }
attributes #2 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn }
attributes #3 = { mustprogress nounwind willreturn }
attributes #4 = { mustprogress nofree nounwind willreturn }
attributes #5 = { inaccessiblememonly mustprogress nofree nounwind willreturn allockind("alloc,uninitialized") allocsize(0) "alloc-family"="malloc" }
attributes #6 = { inaccessiblemem_or_argmemonly mustprogress nounwind willreturn allockind("free") "alloc-family"="malloc" }
attributes #7 = { mustprogress nofree nosync nounwind readnone willreturn }
attributes #8 = { mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #9 = { mustprogress nofree nounwind willreturn writeonly }
attributes #10 = { argmemonly mustprogress nocallback nofree nounwind willreturn }

!0 = !{i64 0, i64 65}
