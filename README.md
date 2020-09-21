
# Ps-lib-tool

Utilities for building parenscript libraries, including a project generator for parenscript-based node libraries.

# Rationale

The dependency managment tools for Common Lisp and Javascript/Node are incompatible practically and philosopically. This set of tools is meant to help those who wish to develop libraries that straddle both worlds.

Included are a rudimentary interface to NPM - the Node Package Manager, a project generator for Node libraries written in Parenscript, and a dependency manager for node/js requirements in Common Lisp libraries.


# Def-ps-package

There are a few cases where you may want to distribute your parenscript code as a standard ASDF lisp library rather than as a Node library:
 - When it contains macro definitions. Macros can only be accessed from Lisp/PS code. Once the code is distributed as JS in a Node library the macros will all be gone.
 - When it will only be used from other Parenscript projects.
 - When you don't want the hassle of formally publishing a Node library.

 Def-ps-package allows you to specify Node requirements for your Common Lisp library. This is useful when you write a library of macro wrappers for an existing Node library. You need to require the library where ever your macros are used, but you don't really want a require statement in each macro expansion. Def-ps-package allows you to supply init code to be run by any parenscript code that depends on your macros.

## import-into, @l and chainl

A problem shows up when you want to write macros that call a JS library. It's similar to the need for gensyms in unhygenic macros. Your macro needs to find the library. For example:

    > (paren6:import (-react) "react")

This causes the reactjs library to be loaded as the `React` variable. Your macros can now call react.

    > `(chain -react (create-class ...

But due to the nature of macros, the `React` variable has been set in your client's namespace. Who knows what you have just trampled!

Ps-lib-tool supplies a solution to this with the @l and chainl macros. Your :init-code section can look like this:

    > (def-ps-package my-package
    >   ...
    >   :init-code
    >   (ps
    >     (unless (@l :my-package)
    >       (setf (@l :my-package) (new -object)) ;; Init. the :my-package location
    >       (paren6:import-into (@l :my-package) (-react) "react"))))

The import-into macro from paren6 acts like import, but binds the specified names in the supplied object rather than in the current namespace. You can now access your import with `(@l :my-package -react)` as with the parenscript @ macro. The chainl macro is likewise the equivalent of the chain macro:

    > (chainl :my-package -react (create-element ...))

This is plainly not as safe as a gensym. You can obviously do `(@l :other-package ...)`. Don't do that! Why, you ask, are we not using a gensym? We don't want to run the import code for each expansion of every macro in the package. One import for the package will do just fine.

## :ps-requirements

The :ps-requirements field of def-ps-package is a list of symbols specifying other ps-packages that are needed and whose dependencies should be included. The ASDF system in which your def-ps-package resides should have already loaded the systems which contain these other ps-packages. Make sure to add the appropriate items to your .asd file.

## :js-requirements

This section specifies Node/js requirements. It is a list. Each member of the list can be a string containing the name of the library or a list containing the namestring and a version specifier. Example:

    > :js-requirements
    > '("library-a"
    >   ("library-b" "1.2.3"))

## :export

[Not Implemented]

# Make-node-library

Make-node-library is a tool for building Node libraries with parenscript. Features:
 - Creates a basic build script that uses [Sigil](https://www.npmjs.com/package/sigil-cli) to compile your parenscript to javascript.
 - Generates a package.json file.
 - Handles dependencies for any ps-packages that you include.

Most of the keyword parameters are fields that go into the package.json file. The package.json file format is documented [here](https://docs.npmjs.com/creating-a-package-json-file). The exception is the :ps-dependencies parameter, which is processed by make-node-library.

Once generated, projects can be built from the project directory with the following commands:

    > npm install
    > npm run build

Make-node-library does not install npm. You will need to install it in order to build your project.

# Author

Ben McGunigle (bnmcgn at gmail.com)

# License

MIT

