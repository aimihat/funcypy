# Sergei Chirkunov (sc7617) README for Group Phase
* Worked on Combinator Z recursion implementation. Succeed treating every function as a recursive function without any conflicts at run-time. Failed to make working recursion due to the nested Combinator Y call (i.e. Call(Call(Combinator Y, Combinator Y, ...))). This problem could be resolved by differentiating recursive calls from non-cursive within the recursive functions. However, this needs extensions to AST and a change of run-time. The hacky way was used at the end.
* Property end-to-end tests with Arithmetics
* Worked on identifying initial blockages between Parser and Run-time
* Made project build and run files for Windows environment
* Tested simple demo cases