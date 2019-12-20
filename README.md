# Ypsilon::Digamma

Note: How to rebuild heap on changing identifiers

* go src/core.h and set 'UNBOUND_GLOC_RETURN_UNSPEC' to '1'
* 'make'
* 'cd heap'
* (edit library.scm)
* 'make'
* 'cd ..'
* 'make'
* 'cd heap'
* 'make'
* 'cd ..'
* (changes should be available in binary at this point)
* revert 'UNBOUND_GLOC_RETURN_UNSPEC' back to '0'
* make
