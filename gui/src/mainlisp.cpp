/*
 "Example of a C++ program embedding ECL with two-way calls."
  Copyright (C) 2014 Chris Kohlhepp
*/
/*

#include <unistd.h>
#include <iostream>
#include <cstdlib>
#include <ecl/ecl.h>

// A macro to create a DEFUN abstraction in C++
// Credit: https://gist.github.com/vwood/662109
#define DEFUN(name,fun,args) \
 cl_def_c_function(c_string_to_object(name), \
 (cl_objectfn_fixed)fun, \
 args)

/*
// Define some accessors.
cl_object runtime() {
 return ecl_make_integer(elapsed);
}

cl_object set_runtime(cl_object i) {
 auto seconds = fix(i);
 elapsed = seconds;
 return ecl_make_integer(elapsed);
}
+/
// Define a function to run arbitrary Lisp expressions
cl_object lisp(const std::string & call) {
 return cl_safe_eval(c_string_to_object(call.c_str()), Cnil, Cnil);
}

// Initialisation does the following
// 1) "Bootstrap" the lisp runtime
// 2) Load an initrc to provide initial
//    configuration for our Lisp runtime
// 3) Make our accessors available to Lisp
// 4) Any In-line Lisp functions for later reference
void initialize(int argc, char **argv) {

 // Bootstrap
 cl_boot(argc, argv);
 atexit(cl_shutdown);

 // Run initrc script
 lisp("(load \"initrc.lisp\")");

 // Make C++ functions available to Lisp
 //DEFUN("runtime", runtime, 0);
 //DEFUN("set_runtime", set_runtime, 1);

 // Define some Lisp functions to call from C++
 lisp("(defun header () (format t \"Starting program...~%\"))");
 lisp("(defun makeanumber () 3.2)");
}

void mainLisp() {
    char * argvFake = "Hello";
    char ** argvFakePtr = &argvFake;
    // Bootstrap Lisp
    initialize(1,argvFakePtr);

    // Run some Lisp functions...
    // Demonstrates calling Lisp from C++
    lisp("(header)");

    // Demonstrate calling Lisp from C++ and
    // return its value to C++ using C++11 style syntax.
    auto x = ecl_to_float(lisp("(makeanumber)"));

    /*
 // Main loop
 // Do something "not so useful."
 for (; elapsed < maxtime; elapsed++){
    sleep(1);
    std::cout << "Time elapsed " << elapsed << std::endl;
 }
 +/
}
*/
