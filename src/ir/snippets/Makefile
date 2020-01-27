all: push_iloc.out array_index_access.out call.out

array_index_access.out : array_index_access.cpp
	clang++ array_index_access.cpp `llvm-config --cxxflags --ldflags --system-libs --libs all` -fcxx-exceptions -o array_index_access.out

push_iloc.out : push_iloc.cpp
	clang++ push_iloc.cpp `llvm-config --cxxflags --ldflags --system-libs --libs all` -fcxx-exceptions -o push_iloc.out

call.out : call.cpp
	clang++ call.cpp `llvm-config --cxxflags --ldflags --system-libs --libs all` -fcxx-exceptions -o call.out

# notation: https://llvm.org/doxygen/EfficiencySanitizer_8cpp_source.html
# construct: https://buildmedia.readthedocs.org/media/pdf/mapping-high-level-constructs-to-llvm-ir/latest/mapping-high-level-constructs-to-llvm-ir.pdf
# constant list: https://stackoverflow.com/questions/42255453/how-do-i-initialize-an-integer-array-in-llvm-using-a-list-of-integers
# llc --march=x86-64 --x86-asm-syntax=intel