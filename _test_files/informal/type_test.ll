; ModuleID = 'type_test.gon'
source_filename = "type_test.gon"

%"generic<#tyvar<generic, T>>" = type {}

@locale = private unnamed_addr constant [12 x i8] c"en_US.UTF-8\00", align 1

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define %"generic<#tyvar<generic, T>>" @"generic<?<generic>::T>::new"() local_unnamed_addr #0 {
body:
  ret %"generic<#tyvar<generic, T>>" zeroinitializer
}

define i8 @main() local_unnamed_addr {
body:
  %0 = tail call ptr @setlocale(i64 0, ptr nonnull @locale)
  ret i8 0
}

declare ptr @setlocale(i64, ptr) local_unnamed_addr

attributes #0 = { mustprogress nofree norecurse nosync nounwind readnone willreturn }
