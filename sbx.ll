


declare i32 @puts(i8* nocapture) nounwind
declare i8* @calloc(i32, i32)
declare void @free(i8*)
declare i32 @printf(i8*, ...)
@.integerFormatString = internal constant [4 x i8] c"%d\0A\00"
@.floatFormatString = internal constant [4 x i8] c"%f\0A\00"

define i32 @main() {
    %variables = alloca double, i32 100


    %temp.0 = getelementptr double, double* %variables, i32 0
    store double 30.0, double* %temp.0
    %temp.1 = getelementptr double, double* %variables, i32 1
    store double 42.0, double* %temp.1
    %temp.2 = fadd double 0.0, 10.0
    %temp.3 = fadd double 0.0, 20.0
    %temp.4_ptr = getelementptr double, double* %variables, i32 0
    %temp.4 = load double, double* %temp.4_ptr
    %temp.5 = fadd double %temp.3, %temp.4
    %temp.6 = fadd double %temp.2, %temp.5
    %temp.7 = fadd double 0.0, 5.0
    %temp.8 = fmul double %temp.6, %temp.7

    tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.floatFormatString, i32 0, i32 0), double %temp.8)
    tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.floatFormatString, i32 0, i32 0), double %temp.8)

    ret i32 0
}
