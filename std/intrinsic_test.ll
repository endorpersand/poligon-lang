; ModuleID = 'eval'
source_filename = "eval"

%string = type { %"#dynarray" }
%"#dynarray" = type { ptr, i64, i64 }

@_tmpl_print = private unnamed_addr constant [6 x i8] c"%.*s\0A\00", align 1
@locale = private unnamed_addr constant [12 x i8] c"en_US.UTF-8\00", align 1

declare ptr @"#malloc"(i64)

declare ptr @malloc(i64)

declare void @"#free"(ptr)

declare void @free(ptr)

define void @main() {
init:
  %0 = call ptr @setlocale(i64 0, ptr @locale)
  br label %body

body:                                             ; preds = %init
  %call = call ptr @"#malloc"(i64 10)
  %ptr = alloca ptr, align 8
  store ptr %call, ptr %ptr, align 8
  %1 = load ptr, ptr %ptr, align 8
  call void @"#free"(ptr %1)
  ret void
}

define void @print(ptr %s) {
body:
  %0 = getelementptr inbounds %string, ptr %s, i32 0, i32 0
  %1 = getelementptr inbounds %"#dynarray", ptr %0, i32 0, i32 0
  %2 = getelementptr inbounds %"#dynarray", ptr %0, i32 0, i32 1
  %buf = load ptr, ptr %1, align 8
  %len = load i64, ptr %2, align 4
  %3 = call i64 (ptr, ...) @printf(ptr @_tmpl_print, i64 %len, ptr %buf)
  ret void
}

declare i64 @printf(ptr, ...)

declare ptr @setlocale(i64, ptr)
