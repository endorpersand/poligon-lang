; ModuleID = 'std.gon'
source_filename = "std.gon"

%string = type { %"#dynarray" }
%"#dynarray" = type { ptr, i64, i64 }

@_tmpl_print = private unnamed_addr constant [6 x i8] c"%.*s\0A\00", align 1
@_tmpl_int_to_string = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@_tmpl_float_to_string = private unnamed_addr constant [4 x i8] c"%#f\00", align 1
@_tmpl_char_to_string = private unnamed_addr constant [4 x i8] c"%lc\00", align 1
@locale = private unnamed_addr constant [12 x i8] c"en_US.UTF-8\00", align 1

define %string @"char::to_string"(i32 %self) {
body:
  %self1 = alloca i32, align 4
  store i32 %self, ptr %self1, align 4
  %0 = alloca ptr, align 8
  %buf_ptr = alloca ptr, align 8
  store ptr %0, ptr %buf_ptr, align 8
  %1 = load ptr, ptr %buf_ptr, align 8
  %2 = load i32, ptr %self1, align 4
  %call = call i64 (ptr, ptr, ...) @asprintf(ptr %1, ptr @_tmpl_char_to_string, i32 %2)
  %len = alloca i64, align 8
  store i64 %call, ptr %len, align 4
  %3 = load ptr, ptr %buf_ptr, align 8
  %deref = load ptr, ptr %3, align 8
  %buf = alloca ptr, align 8
  store ptr %deref, ptr %buf, align 8
  %4 = load i64, ptr %len, align 4
  %i_add = add i64 %4, 1
  %cap = alloca i64, align 8
  store i64 %i_add, ptr %cap, align 4
  %5 = load ptr, ptr %buf, align 8
  %6 = load i64, ptr %len, align 4
  %7 = load i64, ptr %cap, align 4
  %8 = insertvalue %"#dynarray" zeroinitializer, ptr %5, 0
  %9 = insertvalue %"#dynarray" %8, i64 %6, 1
  %10 = insertvalue %"#dynarray" %9, i64 %7, 2
  %11 = insertvalue %string zeroinitializer, %"#dynarray" %10, 0
  ret %string %11
}

declare i64 @asprintf(ptr, ptr, ...)

define double @"float::round"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @round(double %0)
  ret double %call
}

declare double @round(double)

define %string @"string::to_string"(ptr %self) {
body:
  %0 = load %string, ptr %self, align 8
  ret %string %0
}

define double @"float::cbrt"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @cbrt(double %0)
  ret double %call
}

declare double @cbrt(double)

define double @"float::max"(double %self, double %o) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %o2 = alloca double, align 8
  store double %o, ptr %o2, align 8
  %0 = load double, ptr %self1, align 8
  %1 = load double, ptr %o2, align 8
  %call = call double @llvm.maxnum.f64(double %0, double %1)
  ret double %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.maxnum.f64(double, double) #0

define void @"int::count_ones"(i64 %self) {
body:
  %self1 = alloca i64, align 8
  store i64 %self, ptr %self1, align 4
  %0 = load i64, ptr %self1, align 4
  %call = call i64 @llvm.ctpop.i64(i64 %0)
  ret void
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.ctpop.i64(i64) #0

define i64 @"float::iround"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call i64 @lround(double %0)
  ret i64 %call
}

declare i64 @lround(double)

define double @"float::log2"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @llvm.log2.f64(double %0)
  ret double %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.log2.f64(double) #0

define double @"float::sign"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @llvm.copysign.f64(double 1.000000e+00, double %0)
  ret double %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.copysign.f64(double, double) #0

define double @"float::log1p"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @log1p(double %0)
  ret double %call
}

declare double @log1p(double)

define %"#dynarray" @"#dynarray::new"(i64 %cap) {
body:
  %cap1 = alloca i64, align 8
  store i64 %cap, ptr %cap1, align 4
  %0 = load i64, ptr %cap1, align 4
  %call = call ptr @malloc(i64 %0)
  %buf = alloca ptr, align 8
  store ptr %call, ptr %buf, align 8
  %1 = load ptr, ptr %buf, align 8
  %2 = load i64, ptr %cap1, align 4
  %3 = insertvalue %"#dynarray" zeroinitializer, ptr %1, 0
  %4 = insertvalue %"#dynarray" %3, i64 0, 1
  %5 = insertvalue %"#dynarray" %4, i64 %2, 2
  ret %"#dynarray" %5
}

declare ptr @malloc(i64)

define double @"float::nexttoward"(double %self, double %twd) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %twd2 = alloca double, align 8
  store double %twd, ptr %twd2, align 8
  %0 = load double, ptr %self1, align 8
  %1 = load double, ptr %twd2, align 8
  %call = call double @nexttoward(double %0, double %1)
  ret double %call
}

declare double @nexttoward(double, double)

define i64 @"int::max"(i64 %self, i64 %o) {
body:
  %self1 = alloca i64, align 8
  store i64 %self, ptr %self1, align 4
  %o2 = alloca i64, align 8
  store i64 %o, ptr %o2, align 4
  %0 = load i64, ptr %self1, align 4
  %1 = load i64, ptr %o2, align 4
  %call = call i64 @llvm.smax.i64(i64 %0, i64 %1)
  ret i64 %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.smax.i64(i64, i64) #0

define i64 @"int::abs"(i64 %self) {
body:
  %self1 = alloca i64, align 8
  store i64 %self, ptr %self1, align 4
  %0 = load i64, ptr %self1, align 4
  %call = call i64 @llvm.abs.i64(i64 %0, i1 false)
  ret i64 %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.abs.i64(i64, i1 immarg) #0

define void @print(ptr %s) {
body:
  %path_access = getelementptr inbounds %string, ptr %s, i32 0, i32 0, i32 1
  %path_load = load i64, ptr %path_access, align 4
  %path_access1 = getelementptr inbounds %string, ptr %s, i32 0, i32 0, i32 0
  %path_load2 = load ptr, ptr %path_access1, align 8
  %call = call i64 (ptr, ...) @printf(ptr @_tmpl_print, i64 %path_load, ptr %path_load2)
  ret void
}

declare i64 @printf(ptr, ...)

define void @"int::reverse_bits"(i64 %self) {
body:
  %self1 = alloca i64, align 8
  store i64 %self, ptr %self1, align 4
  %0 = load i64, ptr %self1, align 4
  %call = call i64 @llvm.bitreverse.i64(i64 %0)
  ret void
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.bitreverse.i64(i64) #0

define %string @"string::add_string"(ptr %self, ptr %other) {
body:
  %call = call i64 @"string::len"(ptr %self)
  %call1 = call i64 @"string::len"(ptr %other)
  %i_add = add i64 %call, %call1
  %new_len = alloca i64, align 8
  store i64 %i_add, ptr %new_len, align 4
  %0 = load i64, ptr %new_len, align 4
  %call2 = call %"#dynarray" @"#dynarray::new"(i64 %0)
  %result_inner = alloca %"#dynarray", align 8
  store %"#dynarray" %call2, ptr %result_inner, align 8
  %path_access = getelementptr inbounds %string, ptr %self, i32 0, i32 0, i32 0
  %path_load = load ptr, ptr %path_access, align 8
  %call3 = call i64 @"string::len"(ptr %self)
  call void @"#dynarray::extend"(ptr %result_inner, ptr %path_load, i64 %call3)
  %path_access4 = getelementptr inbounds %string, ptr %other, i32 0, i32 0, i32 0
  %path_load5 = load ptr, ptr %path_access4, align 8
  %call6 = call i64 @"string::len"(ptr %other)
  call void @"#dynarray::extend"(ptr %result_inner, ptr %path_load5, i64 %call6)
  %1 = load %"#dynarray", ptr %result_inner, align 8
  %2 = insertvalue %string zeroinitializer, %"#dynarray" %1, 0
  ret %string %2
}

define i64 @"string::len"(ptr %self) {
body:
  %path_access = getelementptr inbounds %string, ptr %self, i32 0, i32 0, i32 1
  %path_load = load i64, ptr %path_access, align 4
  ret i64 %path_load
}

define void @"#dynarray::extend"(ptr %self, ptr %add_buf, i64 %add_len) {
body:
  %add_buf1 = alloca ptr, align 8
  store ptr %add_buf, ptr %add_buf1, align 8
  %add_len2 = alloca i64, align 8
  store i64 %add_len, ptr %add_len2, align 4
  %path_access = getelementptr inbounds %"#dynarray", ptr %self, i32 0, i32 1
  %path_load = load i64, ptr %path_access, align 4
  %0 = load i64, ptr %add_len2, align 4
  %i_add = add i64 %path_load, %0
  call void @"#dynarray::resize"(ptr %self, i64 %i_add)
  %path_access3 = getelementptr inbounds %"#dynarray", ptr %self, i32 0, i32 1
  %path_load4 = load i64, ptr %path_access3, align 4
  %path_access5 = getelementptr inbounds %"#dynarray", ptr %self, i32 0, i32 0
  %path_load6 = load ptr, ptr %path_access5, align 8
  %gep = getelementptr [0 x i8], ptr %path_load6, i64 0, i64 %path_load4
  %shift_buf = alloca ptr, align 8
  store ptr %gep, ptr %shift_buf, align 8
  %1 = load ptr, ptr %shift_buf, align 8
  %2 = load ptr, ptr %add_buf1, align 8
  %3 = load i64, ptr %add_len2, align 4
  %call = call ptr @memcpy(ptr %1, ptr %2, i64 %3)
  br label %block

block:                                            ; preds = %body
  %path_access7 = getelementptr inbounds %"#dynarray", ptr %self, i32 0, i32 1
  %path_load8 = load i64, ptr %path_access7, align 4
  %4 = load i64, ptr %add_len2, align 4
  %i_add9 = add i64 %path_load8, %4
  %path_access10 = getelementptr inbounds %"#dynarray", ptr %self, i32 0, i32 1
  store i64 %i_add9, ptr %path_access10, align 4
  br label %post_block

post_block:                                       ; preds = %block
  ret void
}

define void @"#dynarray::resize"(ptr %self, i64 %new_cap) {
body:
  %new_cap1 = alloca i64, align 8
  store i64 %new_cap, ptr %new_cap1, align 4
  %path_access = getelementptr inbounds %"#dynarray", ptr %self, i32 0, i32 2
  %path_load = load i64, ptr %path_access, align 4
  %0 = load i64, ptr %new_cap1, align 4
  %i_lt = icmp slt i64 %path_load, %0
  br label %post_cmp

then:                                             ; preds = %post_cmp
  %path_access2 = getelementptr inbounds %"#dynarray", ptr %self, i32 0, i32 0
  %path_load3 = load ptr, ptr %path_access2, align 8
  %old_buf = alloca ptr, align 8
  store ptr %path_load3, ptr %old_buf, align 8
  %1 = load i64, ptr %new_cap1, align 4
  %call = call ptr @malloc(i64 %1)
  %new_buf = alloca ptr, align 8
  store ptr %call, ptr %new_buf, align 8
  %2 = load ptr, ptr %new_buf, align 8
  %3 = load ptr, ptr %old_buf, align 8
  %path_access4 = getelementptr inbounds %"#dynarray", ptr %self, i32 0, i32 1
  %path_load5 = load i64, ptr %path_access4, align 4
  %call6 = call ptr @memcpy(ptr %2, ptr %3, i64 %path_load5)
  %4 = load ptr, ptr %old_buf, align 8
  call void @free(ptr %4)
  br label %block

else:                                             ; preds = %post_cmp
  br label %merge

merge:                                            ; preds = %else, %post_block
  %if_result = phi {} [ zeroinitializer, %post_block ], [ zeroinitializer, %else ]
  ret void

post_cmp:                                         ; preds = %body
  %cmp_result = phi i1 [ %i_lt, %body ]
  br i1 %cmp_result, label %then, label %else

block:                                            ; preds = %then
  %5 = load i64, ptr %new_cap1, align 4
  %path_access7 = getelementptr inbounds %"#dynarray", ptr %self, i32 0, i32 2
  store i64 %5, ptr %path_access7, align 4
  br label %post_block

post_block:                                       ; preds = %block
  br label %merge
}

declare ptr @memcpy(ptr, ptr, i64)

declare void @free(ptr)

define void @"int::leading_zeroes"(i64 %self) {
body:
  %self1 = alloca i64, align 8
  store i64 %self, ptr %self1, align 4
  %0 = load i64, ptr %self1, align 4
  %call = call i64 @llvm.ctlz.i64(i64 %0, i1 false)
  ret void
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.ctlz.i64(i64, i1 immarg) #0

define double @"float::sin"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @llvm.sin.f64(double %0)
  ret double %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.sin.f64(double) #0

define double @"float::lgamma"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @lgamma(double %0)
  ret double %call
}

declare double @lgamma(double)

define double @"float::expm1"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @expm1(double %0)
  ret double %call
}

declare double @expm1(double)

define i64 @"int::min"(i64 %self, i64 %o) {
body:
  %self1 = alloca i64, align 8
  store i64 %self, ptr %self1, align 4
  %o2 = alloca i64, align 8
  store i64 %o, ptr %o2, align 4
  %0 = load i64, ptr %self1, align 4
  %1 = load i64, ptr %o2, align 4
  %call = call i64 @llvm.smin.i64(i64 %0, i64 %1)
  ret i64 %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.smin.i64(i64, i64) #0

define double @"float::cosh"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @cosh(double %0)
  ret double %call
}

declare double @cosh(double)

define double @"float::erf"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @erf(double %0)
  ret double %call
}

declare double @erf(double)

define double @"float::asin"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @asin(double %0)
  ret double %call
}

declare double @asin(double)

define double @"float::acosh"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @acosh(double %0)
  ret double %call
}

declare double @acosh(double)

define double @"float::abs"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @llvm.fabs.f64(double %0)
  ret double %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.fabs.f64(double) #0

define double @"float::min"(double %self, double %o) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %o2 = alloca double, align 8
  store double %o, ptr %o2, align 8
  %0 = load double, ptr %self1, align 8
  %1 = load double, ptr %o2, align 8
  %call = call double @llvm.minnum.f64(double %0, double %1)
  ret double %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.minnum.f64(double, double) #0

define %string @"float::to_string"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = alloca ptr, align 8
  %buf_ptr = alloca ptr, align 8
  store ptr %0, ptr %buf_ptr, align 8
  %1 = load ptr, ptr %buf_ptr, align 8
  %2 = load double, ptr %self1, align 8
  %call = call i64 (ptr, ptr, ...) @asprintf(ptr %1, ptr @_tmpl_float_to_string, double %2)
  %len = alloca i64, align 8
  store i64 %call, ptr %len, align 4
  %3 = load ptr, ptr %buf_ptr, align 8
  %deref = load ptr, ptr %3, align 8
  %buf = alloca ptr, align 8
  store ptr %deref, ptr %buf, align 8
  %4 = load i64, ptr %len, align 4
  %i_add = add i64 %4, 1
  %cap = alloca i64, align 8
  store i64 %i_add, ptr %cap, align 4
  %5 = load ptr, ptr %buf, align 8
  %6 = load i64, ptr %len, align 4
  %7 = load i64, ptr %cap, align 4
  %8 = insertvalue %"#dynarray" zeroinitializer, ptr %5, 0
  %9 = insertvalue %"#dynarray" %8, i64 %6, 1
  %10 = insertvalue %"#dynarray" %9, i64 %7, 2
  %11 = insertvalue %string zeroinitializer, %"#dynarray" %10, 0
  ret %string %11
}

define i1 @"float::isinf"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call i1 @isinf(double %0)
  ret i1 %call
}

declare i1 @isinf(double)

define double @"float::trunc"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @llvm.trunc.f64(double %0)
  ret double %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.trunc.f64(double) #0

define double @"float::exp2"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @llvm.exp2.f64(double %0)
  ret double %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.exp2.f64(double) #0

define double @"float::sqrt"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @llvm.sqrt.f64(double %0)
  ret double %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.sqrt.f64(double) #0

define double @"float::atan2"(double %self, double %x) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %x2 = alloca double, align 8
  store double %x, ptr %x2, align 8
  %0 = load double, ptr %self1, align 8
  %1 = load double, ptr %x2, align 8
  %call = call double @atan2(double %0, double %1)
  ret double %call
}

declare double @atan2(double, double)

define double @"float::tgamma"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @tgamma(double %0)
  ret double %call
}

declare double @tgamma(double)

define void @"#dynarray::pop"(ptr %self) {
body:
  %path_access = getelementptr inbounds %"#dynarray", ptr %self, i32 0, i32 1
  %path_load = load i64, ptr %path_access, align 4
  %i_gt = icmp sgt i64 %path_load, 0
  br label %post_cmp

then:                                             ; preds = %post_cmp
  br label %block

else:                                             ; preds = %post_cmp
  br label %block4

merge:                                            ; preds = %post_block5, %post_block
  %if_result = phi {} [ zeroinitializer, %post_block ], [ zeroinitializer, %post_block5 ]
  ret void

post_cmp:                                         ; preds = %body
  %cmp_result = phi i1 [ %i_gt, %body ]
  br i1 %cmp_result, label %then, label %else

block:                                            ; preds = %then
  %path_access1 = getelementptr inbounds %"#dynarray", ptr %self, i32 0, i32 1
  %path_load2 = load i64, ptr %path_access1, align 4
  %i_sub = sub i64 %path_load2, 1
  %path_access3 = getelementptr inbounds %"#dynarray", ptr %self, i32 0, i32 1
  store i64 %i_sub, ptr %path_access3, align 4
  br label %post_block

post_block:                                       ; preds = %block
  br label %merge

block4:                                           ; preds = %else
  %path_access6 = getelementptr inbounds %"#dynarray", ptr %self, i32 0, i32 1
  store i64 0, ptr %path_access6, align 4
  br label %post_block5

post_block5:                                      ; preds = %block4
  br label %merge
}

define double @"float::atan"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @atan(double %0)
  ret double %call
}

declare double @atan(double)

define double @"float::asinh"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @asinh(double %0)
  ret double %call
}

declare double @asinh(double)

define double @"float::pow"(double %self, double %exp) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %exp2 = alloca double, align 8
  store double %exp, ptr %exp2, align 8
  %0 = load double, ptr %self1, align 8
  %1 = load double, ptr %exp2, align 8
  %call = call double @llvm.pow.f64(double %0, double %1)
  ret double %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.pow.f64(double, double) #0

define %string @"int::to_string"(i64 %self) {
body:
  %self1 = alloca i64, align 8
  store i64 %self, ptr %self1, align 4
  %0 = alloca ptr, align 8
  %buf_ptr = alloca ptr, align 8
  store ptr %0, ptr %buf_ptr, align 8
  %1 = load ptr, ptr %buf_ptr, align 8
  %2 = load i64, ptr %self1, align 4
  %call = call i64 (ptr, ptr, ...) @asprintf(ptr %1, ptr @_tmpl_int_to_string, i64 %2)
  %len = alloca i64, align 8
  store i64 %call, ptr %len, align 4
  %3 = load ptr, ptr %buf_ptr, align 8
  %deref = load ptr, ptr %3, align 8
  %buf = alloca ptr, align 8
  store ptr %deref, ptr %buf, align 8
  %4 = load i64, ptr %len, align 4
  %i_add = add i64 %4, 1
  %cap = alloca i64, align 8
  store i64 %i_add, ptr %cap, align 4
  %5 = load ptr, ptr %buf, align 8
  %6 = load i64, ptr %len, align 4
  %7 = load i64, ptr %cap, align 4
  %8 = insertvalue %"#dynarray" zeroinitializer, ptr %5, 0
  %9 = insertvalue %"#dynarray" %8, i64 %6, 1
  %10 = insertvalue %"#dynarray" %9, i64 %7, 2
  %11 = insertvalue %string zeroinitializer, %"#dynarray" %10, 0
  ret %string %11
}

define double @"float::ceil"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @llvm.ceil.f64(double %0)
  ret double %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.ceil.f64(double) #0

define double @"float::acos"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @acos(double %0)
  ret double %call
}

declare double @acos(double)

define double @"float::cos"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @llvm.cos.f64(double %0)
  ret double %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.cos.f64(double) #0

define double @"float::tan"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @tan(double %0)
  ret double %call
}

declare double @tan(double)

define double @"float::log"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @llvm.log.f64(double %0)
  ret double %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.log.f64(double) #0

define void @"int::reverse_bytes"(i64 %self) {
body:
  %self1 = alloca i64, align 8
  store i64 %self, ptr %self1, align 4
  %0 = load i64, ptr %self1, align 4
  %call = call i64 @llvm.bswap.i64(i64 %0)
  ret void
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.bswap.i64(i64) #0

define double @"float::log10"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @llvm.log10.f64(double %0)
  ret double %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.log10.f64(double) #0

define i1 @"float::isnan"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call i1 @isnan(double %0)
  ret i1 %call
}

declare i1 @isnan(double)

define double @"float::floor"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @llvm.floor.f64(double %0)
  ret double %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.floor.f64(double) #0

define double @"float::fma"(double %self, double %multiplicand, double %addend) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %multiplicand2 = alloca double, align 8
  store double %multiplicand, ptr %multiplicand2, align 8
  %addend3 = alloca double, align 8
  store double %addend, ptr %addend3, align 8
  %0 = load double, ptr %self1, align 8
  %1 = load double, ptr %multiplicand2, align 8
  %2 = load double, ptr %addend3, align 8
  %call = call double @llvm.fma.f64(double %0, double %1, double %2)
  ret double %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.fma.f64(double, double, double) #0

define void @"int::trailing_zeroes"(i64 %self) {
body:
  %self1 = alloca i64, align 8
  store i64 %self, ptr %self1, align 4
  %0 = load i64, ptr %self1, align 4
  %call = call i64 @llvm.cttz.i64(i64 %0, i1 false)
  ret void
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.cttz.i64(i64, i1 immarg) #0

define i64 @"int::sign"(i64 %self) {
body:
  %self1 = alloca i64, align 8
  store i64 %self, ptr %self1, align 4
  %0 = load i64, ptr %self1, align 4
  %i_gt = icmp sgt i64 %0, 0
  br label %post_cmp

then:                                             ; preds = %post_cmp
  br label %merge

else:                                             ; preds = %post_cmp
  %1 = load i64, ptr %self1, align 4
  %i_lt = icmp slt i64 %1, 0
  br label %post_cmp2

then4:                                            ; preds = %post_cmp2
  br label %merge

else5:                                            ; preds = %post_cmp2
  br label %merge

merge:                                            ; preds = %else5, %then4, %then
  %if_result = phi i64 [ 1, %then ], [ -1, %then4 ], [ 0, %else5 ]
  ret i64 %if_result

post_cmp:                                         ; preds = %body
  %cmp_result = phi i1 [ %i_gt, %body ]
  br i1 %cmp_result, label %then, label %else

post_cmp2:                                        ; preds = %else
  %cmp_result3 = phi i1 [ %i_lt, %else ]
  br i1 %cmp_result3, label %then4, label %else5
}

define void @"#dynarray::push"(ptr %self, i8 %byte) {
body:
  %byte1 = alloca i8, align 1
  store i8 %byte, ptr %byte1, align 1
  %path_access = getelementptr inbounds %"#dynarray", ptr %self, i32 0, i32 1
  %path_load = load i64, ptr %path_access, align 4
  %i_add = add i64 %path_load, 1
  call void @"#dynarray::resize"(ptr %self, i64 %i_add)
  %path_access2 = getelementptr inbounds %"#dynarray", ptr %self, i32 0, i32 1
  %path_load3 = load i64, ptr %path_access2, align 4
  %path_access4 = getelementptr inbounds %"#dynarray", ptr %self, i32 0, i32 0
  %path_load5 = load ptr, ptr %path_access4, align 8
  %gep = getelementptr [0 x i8], ptr %path_load5, i64 0, i64 %path_load3
  %push_ptr = alloca ptr, align 8
  store ptr %gep, ptr %push_ptr, align 8
  br label %block

block:                                            ; preds = %body
  %path_access6 = getelementptr inbounds %"#dynarray", ptr %self, i32 0, i32 1
  %path_load7 = load i64, ptr %path_access6, align 4
  %i_add8 = add i64 %path_load7, 1
  %path_access9 = getelementptr inbounds %"#dynarray", ptr %self, i32 0, i32 1
  store i64 %i_add8, ptr %path_access9, align 4
  br label %post_block

post_block:                                       ; preds = %block
  br label %block10

block10:                                          ; preds = %post_block
  %0 = load i8, ptr %byte1, align 1
  %1 = load ptr, ptr %push_ptr, align 8
  store i8 %0, ptr %1, align 1
  br label %post_block11

post_block11:                                     ; preds = %block10
  ret void
}

define double @"float::atanh"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @atanh(double %0)
  ret double %call
}

declare double @atanh(double)

define double @"float::hypot"(double %self, double %y) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %y2 = alloca double, align 8
  store double %y, ptr %y2, align 8
  %0 = load double, ptr %self1, align 8
  %1 = load double, ptr %y2, align 8
  %call = call double @hypot(double %0, double %1)
  ret double %call
}

declare double @hypot(double, double)

define double @"float::erfc"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @erfc(double %0)
  ret double %call
}

declare double @erfc(double)

define double @"float::powi"(double %self, i64 %exp) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %exp2 = alloca i64, align 8
  store i64 %exp, ptr %exp2, align 4
  %0 = load double, ptr %self1, align 8
  %1 = load i64, ptr %exp2, align 4
  %cast = sitofp i64 %1 to double
  %call = call double @llvm.pow.f64(double %0, double %cast)
  ret double %call
}

define double @"float::sinh"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @sinh(double %0)
  ret double %call
}

declare double @sinh(double)

define double @"float::exp"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @llvm.exp.f64(double %0)
  ret double %call
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.exp.f64(double) #0

define double @"float::tanh"(double %self) {
body:
  %self1 = alloca double, align 8
  store double %self, ptr %self1, align 8
  %0 = load double, ptr %self1, align 8
  %call = call double @tanh(double %0)
  ret double %call
}

declare double @tanh(double)

define %string @"string::new"(ptr %contents, i64 %len) {
body:
  %contents1 = alloca ptr, align 8
  store ptr %contents, ptr %contents1, align 8
  %len2 = alloca i64, align 8
  store i64 %len, ptr %len2, align 4
  %0 = load i64, ptr %len2, align 4
  %call = call %"#dynarray" @"#dynarray::new"(i64 %0)
  %inner = alloca %"#dynarray", align 8
  store %"#dynarray" %call, ptr %inner, align 8
  %1 = load ptr, ptr %contents1, align 8
  %2 = load i64, ptr %len2, align 4
  call void @"#dynarray::extend"(ptr %inner, ptr %1, i64 %2)
  %3 = load %"#dynarray", ptr %inner, align 8
  %4 = insertvalue %string zeroinitializer, %"#dynarray" %3, 0
  ret %string %4
}

define void @main() {
init:
  %0 = call ptr @setlocale(i64 0, ptr @locale)
  br label %main_body

main_body:                                        ; preds = %init
  ret void
}

declare ptr @setlocale(i64, ptr)

attributes #0 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
