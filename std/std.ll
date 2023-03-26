; ModuleID = 'std.gon'
source_filename = "std.gon"

%string = type { %"#dynarray" }
%"#dynarray" = type { ptr, i64, i64 }

@_tmpl_print = private unnamed_addr constant [6 x i8] c"%.*s\0A\00", align 1
@locale = private unnamed_addr constant [12 x i8] c"en_US.UTF-8\00", align 1

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

declare ptr @malloc(i64)

declare void @free(ptr)

define i64 @"string::len"(ptr %self) {
body:
  %path_access = getelementptr inbounds %string, ptr %self, i32 0, i32 0, i32 1
  %path_load = load i64, ptr %path_access, align 4
  ret i64 %path_load
}

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

define void @main() {
init:
  %0 = call ptr @setlocale(i64 0, ptr @locale)
  br label %main_body

main_body:                                        ; preds = %init
  ret void
}

declare ptr @setlocale(i64, ptr)
