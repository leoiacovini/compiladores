


declare i32 @puts(i8* nocapture) nounwind
declare i8* @calloc(i32, i32)
declare void @free(i8*)

define i32 @main() {
    %variables = alloca i32, i32 10
    %cells = call i8* @calloc(i32 30000, i32 1)
    %cell_index_ptr = alloca i32
    %variables_index_ptr = alloca i32

    store i32 0, i32* %cell_index_ptr
    store i32 0, i32* %variables_index_ptr
    %variable_ptr = getelementptr i32, i32* %variables, i32 0
    store i32 1, i32* %variable_ptr
    %variable = load i32, i32* %variable_ptr

    ret i32 %variable
}
