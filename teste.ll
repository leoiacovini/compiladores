
@.str0.975877941787027 = private unnamed_addr constant [4 x i8] c"foo\00"

declare i32 @puts(i8* nocapture) nounwind

define i32 @main() {
    %str0.975877941787027_cast = getelementptr [4 x i8], [4 x i8]* @.str0.975877941787027, i64 0, i64 0
    call i32 @puts(i8* %str0.975877941787027_cast)
    ret i32 0
}
    