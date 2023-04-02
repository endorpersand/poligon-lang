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
@locale = private unnamed_addr constant [12 x i8] c"en_US.UTF-8\00", align 1

; Function Attrs: nofree nounwind
define void @print(ptr nocapture readonly %s) local_unnamed_addr #0 {
body:
  %path_access = getelementptr inbounds %string, ptr %s, i64 0, i32 0, i32 1
  %path_load = load i64, ptr %path_access, align 4
  %path_load2 = load ptr, ptr %s, align 8
  %call = tail call i64 (ptr, ...) @printf(ptr nonnull @_tmpl_print, i64 %path_load, ptr %path_load2)
  ret void
}

; Function Attrs: nofree nounwind
declare noundef i64 @printf(ptr nocapture noundef readonly, ...) local_unnamed_addr #0

define %string @"char::to_string"(i32 %self) local_unnamed_addr {
body:
  %0 = alloca ptr, align 8
  %call = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %0, ptr nonnull @_tmpl_char_to_string, i32 %self)
  %deref = load ptr, ptr %0, align 8
  %i_add = add i64 %call, 1
  %1 = insertvalue %"#dynarray" zeroinitializer, ptr %deref, 0
  %2 = insertvalue %"#dynarray" %1, i64 %call, 1
  %3 = insertvalue %"#dynarray" %2, i64 %i_add, 2
  %4 = insertvalue %string zeroinitializer, %"#dynarray" %3, 0
  ret %string %4
}

declare i64 @asprintf(ptr, ptr, ...) local_unnamed_addr

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define %string @"string::to_string"(ptr nocapture readonly %self) local_unnamed_addr #1 {
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
define %string @"string::add_string"(ptr nocapture readonly %self, ptr nocapture readonly %other) local_unnamed_addr #2 {
body:
  %call = tail call i64 @"string::len"(ptr %self)
  %call1 = tail call i64 @"string::len"(ptr %other)
  %i_add = add i64 %call1, %call
  %result_inner = alloca %"#dynarray", align 8
  %call2 = tail call %"#dynarray" @"#dynarray::new"(i64 %i_add)
  %call2.elt = extractvalue %"#dynarray" %call2, 0
  store ptr %call2.elt, ptr %result_inner, align 8
  %result_inner.repack1 = getelementptr inbounds %"#dynarray", ptr %result_inner, i64 0, i32 1
  store i64 0, ptr %result_inner.repack1, align 8
  %result_inner.repack3 = getelementptr inbounds %"#dynarray", ptr %result_inner, i64 0, i32 2
  %call2.elt4 = extractvalue %"#dynarray" %call2, 2
  store i64 %call2.elt4, ptr %result_inner.repack3, align 8
  %path_load = load ptr, ptr %self, align 8
  %call3 = tail call i64 @"string::len"(ptr nonnull %self)
  call void @"#dynarray::extend"(ptr nonnull %result_inner, ptr %path_load, i64 %call3)
  %path_load5 = load ptr, ptr %other, align 8
  %call6 = tail call i64 @"string::len"(ptr nonnull %other)
  call void @"#dynarray::extend"(ptr nonnull %result_inner, ptr %path_load5, i64 %call6)
  %.unpack = load ptr, ptr %result_inner, align 8
  %0 = insertvalue %"#dynarray" undef, ptr %.unpack, 0
  %.unpack6 = load i64, ptr %result_inner.repack1, align 8
  %1 = insertvalue %"#dynarray" %0, i64 %.unpack6, 1
  %.unpack8 = load i64, ptr %result_inner.repack3, align 8
  %2 = insertvalue %"#dynarray" %1, i64 %.unpack8, 2
  %3 = insertvalue %string zeroinitializer, %"#dynarray" %2, 0
  ret %string %3
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define i64 @"string::len"(ptr nocapture readonly %self) local_unnamed_addr #1 {
body:
  %path_access = getelementptr inbounds %string, ptr %self, i64 0, i32 0, i32 1
  %path_load = load i64, ptr %path_access, align 4
  ret i64 %path_load
}

; Function Attrs: mustprogress nofree nounwind willreturn
define %"#dynarray" @"#dynarray::new"(i64 %cap) local_unnamed_addr #3 {
body:
  %call = tail call ptr @malloc(i64 %cap)
  %0 = insertvalue %"#dynarray" zeroinitializer, ptr %call, 0
  %1 = insertvalue %"#dynarray" %0, i64 0, 1
  %2 = insertvalue %"#dynarray" %1, i64 %cap, 2
  ret %"#dynarray" %2
}

; Function Attrs: mustprogress nounwind willreturn
define void @"#dynarray::extend"(ptr nocapture %self, ptr nocapture readonly %add_buf, i64 %add_len) local_unnamed_addr #2 {
body:
  %path_access = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %path_load = load i64, ptr %path_access, align 4
  %i_add = add i64 %path_load, %add_len
  tail call void @"#dynarray::resize"(ptr %self, i64 %i_add)
  %path_load4 = load i64, ptr %path_access, align 4
  %path_load6 = load ptr, ptr %self, align 8
  %gep = getelementptr i8, ptr %path_load6, i64 %path_load4
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %gep, ptr align 1 %add_buf, i64 %add_len, i1 false)
  %path_load8 = load i64, ptr %path_access, align 4
  %i_add9 = add i64 %path_load8, %add_len
  store i64 %i_add9, ptr %path_access, align 4
  ret void
}

; Function Attrs: mustprogress nounwind willreturn
define void @"#dynarray::resize"(ptr nocapture %self, i64 %new_cap) local_unnamed_addr #2 {
body:
  %path_access = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 2
  %path_load = load i64, ptr %path_access, align 4
  %i_lt = icmp slt i64 %path_load, %new_cap
  br i1 %i_lt, label %then, label %merge

then:                                             ; preds = %body
  %path_load3 = load ptr, ptr %self, align 8
  %call = tail call ptr @malloc(i64 %new_cap)
  %path_access4 = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %path_load5 = load i64, ptr %path_access4, align 4
  tail call void @llvm.memcpy.p0.p0.i64(ptr align 1 %call, ptr align 1 %path_load3, i64 %path_load5, i1 false)
  tail call void @free(ptr %path_load3)
  store ptr %call, ptr %self, align 8
  store i64 %new_cap, ptr %path_access, align 4
  br label %merge

merge:                                            ; preds = %body, %then
  ret void
}

; Function Attrs: inaccessiblememonly mustprogress nofree nounwind willreturn allockind("alloc,uninitialized") allocsize(0)
declare noalias noundef ptr @malloc(i64 noundef) local_unnamed_addr #4

; Function Attrs: inaccessiblemem_or_argmemonly mustprogress nounwind willreturn allockind("free")
declare void @free(ptr allocptr nocapture noundef) local_unnamed_addr #5

; Function Attrs: mustprogress nounwind willreturn
define %string @"string::new"(ptr nocapture readonly %contents, i64 %len) local_unnamed_addr #2 {
body:
  %inner = alloca %"#dynarray", align 8
  %call = tail call %"#dynarray" @"#dynarray::new"(i64 %len)
  %call.elt = extractvalue %"#dynarray" %call, 0
  store ptr %call.elt, ptr %inner, align 8
  %inner.repack1 = getelementptr inbounds %"#dynarray", ptr %inner, i64 0, i32 1
  store i64 0, ptr %inner.repack1, align 8
  %inner.repack3 = getelementptr inbounds %"#dynarray", ptr %inner, i64 0, i32 2
  %call.elt4 = extractvalue %"#dynarray" %call, 2
  store i64 %call.elt4, ptr %inner.repack3, align 8
  call void @"#dynarray::extend"(ptr nonnull %inner, ptr %contents, i64 %len)
  %.unpack = load ptr, ptr %inner, align 8
  %0 = insertvalue %"#dynarray" undef, ptr %.unpack, 0
  %.unpack6 = load i64, ptr %inner.repack1, align 8
  %1 = insertvalue %"#dynarray" %0, i64 %.unpack6, 1
  %.unpack8 = load i64, ptr %inner.repack3, align 8
  %2 = insertvalue %"#dynarray" %1, i64 %.unpack8, 2
  %3 = insertvalue %string zeroinitializer, %"#dynarray" %2, 0
  ret %string %3
}

define ptr @"#dynarray::take"(ptr nocapture %self, i64 %sub_len) local_unnamed_addr {
body:
  %path_access = getelementptr inbounds %"#dynarray", ptr %self, i64 0, i32 1
  %path_load = load i64, ptr %path_access, align 4
  %i_ge.not = icmp slt i64 %path_load, %sub_len
  br i1 %i_ge.not, label %else, label %block

else:                                             ; preds = %body
  %stderr = tail call ptr @fdopen(i64 2, ptr nonnull @_write)
  %0 = tail call i64 @fwrite(ptr nonnull @throw_msg, i64 30, i64 1, ptr %stderr)
  %1 = tail call i64 @fputwc(i32 10, ptr %stderr)
  tail call void @exit(i64 1)
  unreachable

block:                                            ; preds = %body
  %i_sub = sub i64 %path_load, %sub_len
  store i64 %i_sub, ptr %path_access, align 4
  %path_load8 = load ptr, ptr %self, align 8
  %gep = getelementptr i8, ptr %path_load8, i64 %i_sub
  ret ptr %gep
}

; Function Attrs: nofree nounwind
declare noalias noundef ptr @fdopen(i64 noundef, ptr nocapture noundef readonly) local_unnamed_addr #0

declare i64 @fputwc(i32, ptr) local_unnamed_addr

declare void @exit(i64) local_unnamed_addr

define i1 @"float::isnan"(double %self) local_unnamed_addr {
body:
  %call = tail call i1 @isnan(double %self)
  ret i1 %call
}

declare i1 @isnan(double) local_unnamed_addr

define i1 @"float::isinf"(double %self) local_unnamed_addr {
body:
  %call = tail call i1 @isinf(double %self)
  ret i1 %call
}

declare i1 @isinf(double) local_unnamed_addr

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::sign"(double %self) local_unnamed_addr #6 {
body:
  %call = tail call double @llvm.copysign.f64(double 1.000000e+00, double %self)
  ret double %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.copysign.f64(double, double) #7

define double @"float::nexttoward"(double %self, double %twd) local_unnamed_addr {
body:
  %call = tail call double @nexttoward(double %self, double %twd)
  ret double %call
}

declare double @nexttoward(double, double) local_unnamed_addr

define i64 @"float::iround"(double %self) local_unnamed_addr {
body:
  %call = tail call i64 @lround(double %self)
  ret i64 %call
}

declare i64 @lround(double) local_unnamed_addr

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::round"(double %self) local_unnamed_addr #6 {
body:
  %call = tail call double @llvm.round.f64(double %self)
  ret double %call
}

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::trunc"(double %self) local_unnamed_addr #6 {
body:
  %call = tail call double @llvm.trunc.f64(double %self)
  ret double %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.trunc.f64(double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::floor"(double %self) local_unnamed_addr #6 {
body:
  %call = tail call double @llvm.floor.f64(double %self)
  ret double %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.floor.f64(double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::ceil"(double %self) local_unnamed_addr #6 {
body:
  %call = tail call double @llvm.ceil.f64(double %self)
  ret double %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.ceil.f64(double) #7

define double @"float::tgamma"(double %self) local_unnamed_addr {
body:
  %call = tail call double @tgamma(double %self)
  ret double %call
}

declare double @tgamma(double) local_unnamed_addr

define double @"float::lgamma"(double %self) local_unnamed_addr {
body:
  %call = tail call double @lgamma(double %self)
  ret double %call
}

declare double @lgamma(double) local_unnamed_addr

define double @"float::erfc"(double %self) local_unnamed_addr {
body:
  %call = tail call double @erfc(double %self)
  ret double %call
}

declare double @erfc(double) local_unnamed_addr

define double @"float::erf"(double %self) local_unnamed_addr {
body:
  %call = tail call double @erf(double %self)
  ret double %call
}

declare double @erf(double) local_unnamed_addr

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::atanh"(double %self) local_unnamed_addr #8 {
body:
  %call = tail call double @atanh(double %self)
  ret double %call
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @atanh(double) local_unnamed_addr #8

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::acosh"(double %self) local_unnamed_addr #8 {
body:
  %call = tail call double @acosh(double %self)
  ret double %call
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @acosh(double) local_unnamed_addr #8

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::asinh"(double %self) local_unnamed_addr #8 {
body:
  %call = tail call double @asinh(double %self)
  ret double %call
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @asinh(double) local_unnamed_addr #8

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::tanh"(double %self) local_unnamed_addr #8 {
body:
  %call = tail call double @tanh(double %self)
  ret double %call
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @tanh(double) local_unnamed_addr #8

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::cosh"(double %self) local_unnamed_addr #8 {
body:
  %call = tail call double @cosh(double %self)
  ret double %call
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @cosh(double) local_unnamed_addr #8

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::sinh"(double %self) local_unnamed_addr #8 {
body:
  %call = tail call double @sinh(double %self)
  ret double %call
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @sinh(double) local_unnamed_addr #8

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::atan2"(double %self, double %x) local_unnamed_addr #8 {
body:
  %call = tail call double @atan2(double %self, double %x)
  ret double %call
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @atan2(double, double) local_unnamed_addr #8

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::atan"(double %self) local_unnamed_addr #8 {
body:
  %call = tail call double @atan(double %self)
  ret double %call
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @atan(double) local_unnamed_addr #8

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::acos"(double %self) local_unnamed_addr #8 {
body:
  %call = tail call double @acos(double %self)
  ret double %call
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @acos(double) local_unnamed_addr #8

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::asin"(double %self) local_unnamed_addr #8 {
body:
  %call = tail call double @asin(double %self)
  ret double %call
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @asin(double) local_unnamed_addr #8

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::tan"(double %self) local_unnamed_addr #8 {
body:
  %call = tail call double @tan(double %self)
  ret double %call
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @tan(double) local_unnamed_addr #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::cos"(double %self) local_unnamed_addr #6 {
body:
  %call = tail call double @llvm.cos.f64(double %self)
  ret double %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.cos.f64(double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::sin"(double %self) local_unnamed_addr #6 {
body:
  %call = tail call double @llvm.sin.f64(double %self)
  ret double %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.sin.f64(double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::powi"(double %self, i64 %exp) local_unnamed_addr #6 {
body:
  %cast = sitofp i64 %exp to double
  %call = tail call double @llvm.pow.f64(double %self, double %cast)
  ret double %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.pow.f64(double, double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::pow"(double %self, double %exp) local_unnamed_addr #6 {
body:
  %call = tail call double @llvm.pow.f64(double %self, double %exp)
  ret double %call
}

define double @"float::hypot"(double %self, double %y) local_unnamed_addr {
body:
  %call = tail call double @hypot(double %self, double %y)
  ret double %call
}

declare double @hypot(double, double) local_unnamed_addr

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::cbrt"(double %self) local_unnamed_addr #8 {
body:
  %call = tail call double @cbrt(double %self)
  ret double %call
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @cbrt(double) local_unnamed_addr #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::sqrt"(double %self) local_unnamed_addr #6 {
body:
  %call = tail call double @llvm.sqrt.f64(double %self)
  ret double %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.sqrt.f64(double) #7

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::log1p"(double %self) local_unnamed_addr #8 {
body:
  %call = tail call double @log1p(double %self)
  ret double %call
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @log1p(double) local_unnamed_addr #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::log10"(double %self) local_unnamed_addr #6 {
body:
  %call = tail call double @llvm.log10.f64(double %self)
  ret double %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.log10.f64(double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::log2"(double %self) local_unnamed_addr #6 {
body:
  %call = tail call double @llvm.log2.f64(double %self)
  ret double %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.log2.f64(double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::log"(double %self) local_unnamed_addr #6 {
body:
  %call = tail call double @llvm.log.f64(double %self)
  ret double %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.log.f64(double) #7

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
define double @"float::expm1"(double %self) local_unnamed_addr #8 {
body:
  %call = tail call double @expm1(double %self)
  ret double %call
}

; Function Attrs: mustprogress nofree nounwind willreturn writeonly
declare double @expm1(double) local_unnamed_addr #8

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::exp2"(double %self) local_unnamed_addr #6 {
body:
  %call = tail call double @llvm.exp2.f64(double %self)
  ret double %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.exp2.f64(double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::exp"(double %self) local_unnamed_addr #6 {
body:
  %call = tail call double @llvm.exp.f64(double %self)
  ret double %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.exp.f64(double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::min"(double %self, double %o) local_unnamed_addr #6 {
body:
  %call = tail call double @llvm.minnum.f64(double %self, double %o)
  ret double %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.minnum.f64(double, double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::max"(double %self, double %o) local_unnamed_addr #6 {
body:
  %call = tail call double @llvm.maxnum.f64(double %self, double %o)
  ret double %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.maxnum.f64(double, double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::fma"(double %self, double %multiplicand, double %addend) local_unnamed_addr #6 {
body:
  %call = tail call double @llvm.fma.f64(double %self, double %multiplicand, double %addend)
  ret double %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.fma.f64(double, double, double) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define double @"float::abs"(double %self) local_unnamed_addr #6 {
body:
  %call = tail call double @llvm.fabs.f64(double %self)
  ret double %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.fabs.f64(double) #7

define %string @"float::to_string"(double %self) local_unnamed_addr {
body:
  %0 = alloca ptr, align 8
  %call = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %0, ptr nonnull @_tmpl_float_to_string, double %self)
  %deref = load ptr, ptr %0, align 8
  %i_add = add i64 %call, 1
  %1 = insertvalue %"#dynarray" zeroinitializer, ptr %deref, 0
  %2 = insertvalue %"#dynarray" %1, i64 %call, 1
  %3 = insertvalue %"#dynarray" %2, i64 %i_add, 2
  %4 = insertvalue %string zeroinitializer, %"#dynarray" %3, 0
  ret %string %4
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define void @"int::trailing_zeroes"(i64 %self) local_unnamed_addr #9 {
body:
  ret void
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define void @"int::leading_zeroes"(i64 %self) local_unnamed_addr #9 {
body:
  ret void
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define void @"int::reverse_bytes"(i64 %self) local_unnamed_addr #9 {
body:
  ret void
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define void @"int::reverse_bits"(i64 %self) local_unnamed_addr #9 {
body:
  ret void
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define void @"int::count_ones"(i64 %self) local_unnamed_addr #9 {
body:
  ret void
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define i64 @"int::sign"(i64 %self) local_unnamed_addr #9 {
body:
  %i_lt.not = icmp ne i64 %self, 0
  %spec.select = sext i1 %i_lt.not to i64
  %i_gt.inv = icmp slt i64 %self, 1
  %if_result = select i1 %i_gt.inv, i64 %spec.select, i64 1
  ret i64 %if_result
}

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::min"(i64 %self, i64 %o) local_unnamed_addr #6 {
body:
  %call = tail call i64 @llvm.smin.i64(i64 %self, i64 %o)
  ret i64 %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.smin.i64(i64, i64) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::max"(i64 %self, i64 %o) local_unnamed_addr #6 {
body:
  %call = tail call i64 @llvm.smax.i64(i64 %self, i64 %o)
  ret i64 %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.smax.i64(i64, i64) #7

; Function Attrs: mustprogress nofree nosync nounwind readnone willreturn
define i64 @"int::abs"(i64 %self) local_unnamed_addr #6 {
body:
  %call = tail call i64 @llvm.abs.i64(i64 %self, i1 false)
  ret i64 %call
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.abs.i64(i64, i1 immarg) #7

define %string @"int::to_string"(i64 %self) local_unnamed_addr {
body:
  %0 = alloca ptr, align 8
  %call = call i64 (ptr, ptr, ...) @asprintf(ptr nonnull %0, ptr nonnull @_tmpl_int_to_string, i64 %self)
  %deref = load ptr, ptr %0, align 8
  %i_add = add i64 %call, 1
  %1 = insertvalue %"#dynarray" zeroinitializer, ptr %deref, 0
  %2 = insertvalue %"#dynarray" %1, i64 %call, 1
  %3 = insertvalue %"#dynarray" %2, i64 %i_add, 2
  %4 = insertvalue %string zeroinitializer, %"#dynarray" %3, 0
  ret %string %4
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
declare double @llvm.round.f64(double) #7

attributes #0 = { nofree nounwind }
attributes #1 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn }
attributes #2 = { mustprogress nounwind willreturn }
attributes #3 = { mustprogress nofree nounwind willreturn }
attributes #4 = { inaccessiblememonly mustprogress nofree nounwind willreturn allockind("alloc,uninitialized") allocsize(0) "alloc-family"="malloc" }
attributes #5 = { inaccessiblemem_or_argmemonly mustprogress nounwind willreturn allockind("free") "alloc-family"="malloc" }
attributes #6 = { mustprogress nofree nosync nounwind readnone willreturn }
attributes #7 = { mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #8 = { mustprogress nofree nounwind willreturn writeonly }
attributes #9 = { mustprogress nofree norecurse nosync nounwind readnone willreturn }
attributes #10 = { argmemonly mustprogress nocallback nofree nounwind willreturn }
