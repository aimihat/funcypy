# Sample Individual Statement for Individual Code Submission

This sample project contains a single F# sample module with uninspiring name `Module1.fs`.

It uses `Dotnet Core 3.1`, with dependency on `Expecto.Fscheck`.

It comes with a VS solution file which however is optional (it can be recreated as needed from project file).

My login is `tomcl` and it is used:

* In `tomcl` directory under which this project is put
* As project (and solution) `tomcl.fsproj`, `tomcl.sol`
* As second part of `README-tomcl` name

To create correct individual code skeleton:

* copy this directory to one with correct name
* change names of other files with login dependent names to use your login.
* load `login.fsproj` in VS etc
* check code will build
* delete `tomcl.sol` if it causes problems, or overwrite it with VS changed version
* change `Module1` to appropriate name for your module - keeping file name and module name the same
   * you need also to change the module name on first line of the file, and the `open Module1` line in `program.fs`. 
   * In a multi-module FS program use `open ModName` to access function `myFunc` in module `ModName` as `myFunc`.
   * Without `open Modname` you can access `myFunc` as `modName.myFunc`.
* replace sample code by your own.
* change `program.fs code as you wish
* delete the contents of this readme, the renamed version will be your individual statement for individual code submission.
* check that you understand all the files (`login.fsproj`, `Program.fs`, `YourModule.fs`) that make up this F# project and how it runs.
