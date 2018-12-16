


declare i32 @puts(i8* nocapture) nounwind
declare i8* @calloc(i32, i32)
declare void @free(i8*)

define i32 @main() {
    %variables = alloca i32, i32 100
    %cells = call i8* @calloc(i32 30000, i32 1)
    %cell_index_ptr = alloca i32
    %variable_index_ptr = alloca i32

    store i32 0, i32* %cell_index_ptr
    store i32 0, i32* %variable_index_ptr
    %variable_ptr.0 = getelementptr i32, i32* %variables, i32 0
    store i32 34, i32* %variable_ptr.0
    %variable_ptr.1 = getelementptr i32, i32* %variables, i32 1
    store i32 42, i32* %variable_ptr.1
    call void @free(i8* %cells)

    %variable_ptr2 = getelementptr i32, i32* %variables, i32 1
    %variable2 = load i32, i32* %variable_ptr2
    ret i32 %variable2
}
    