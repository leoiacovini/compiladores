
@.str0.9856993623680765 = private unnamed_addr constant [2 x i8] c"a\00"
@.str0.05082418382669729 = private unnamed_addr constant [2 x i8] c"a\00"

declare i32 @puts(i8* nocapture) nounwind

define i32 @main() {
    %str0.9856993623680765_cast = getelementptr [2 x i8], [2 x i8]* @.str0.9856993623680765, i64 0, i64 0
    call i32 @puts(i8* %str0.9856993623680765_cast)
    %str0.05082418382669729_cast = getelementptr [2 x i8], [2 x i8]* @.str0.05082418382669729, i64 0, i64 0
    call i32 @puts(i8* %str0.05082418382669729_cast)
    ret i32 0
}
    