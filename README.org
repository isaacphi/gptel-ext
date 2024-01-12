* gptel-ext

Extra functions for [[https://github.com/karthink/gptel][gptel]], a package that allows you to use various LLM providers for text generation and completion directly from Emacs.

** Functions

*** =gptel-ext-send-whole-buffer=

Jump to the bottom of the buffer and then send the entire buffer.

*** =gptel-ext-ask-document=

Load the current buffer into a session so you can ask questions about it.

*** =gptel-ext-quick=

Accept a prompt in the minibuffer and pop up a window with the response. Extracted from the [[https://github.com/karthink/gptel/wiki][Wiki]].

*** =gptel-ext-rewrite-and-replace=

Rewrite the region or sentence at point and replace it with the response. Extracted from the [[https://github.com/karthink/gptel/wiki][Wiki]].

*** =gptel-ext-refactor=

Refactor the region or sentence at point.

** Hooks
The hook =gptel-ext-clean-up-gptel-refactored-code= was taken from the wiki and added. It automatically cleans up code snippets returned from the LLM.

** Variables

*** =gptel-ext-ask-document-prefix=

Prefix to use when asking questions about a document. Defaults to:

#+BEGIN_EXAMPLE
Your task is to answer questions about the following document. If you don't know the answer, reply with "I don't know"

###### DOCUMENT START ######

#+END_EXAMPLE

*** =gptel-ext-ask-document-suffix=

Suffix to use when asking questions about a document. Defaults to:

#+BEGIN_EXAMPLE

###### DOCUMENT END ######

### Question: 
#+END_EXAMPLE

*** =gptel-ext-refactor-directive=

Directive to use when refactoring code. Defaults to:

#+BEGIN_EXAMPLE
You are a programmer. Refactor my code to improve readability. Reply only with the code.
#+END_EXAMPLE

*** =gptel-ext-rewrite-and-replace-directive=

Directive to use when calling =gptel-ext-rewrite-and-replace=. Defaults to:

#+BEGIN_EXAMPLE
You are a programmer. Re-write this code.
#+END_EXAMPLE


** Credits

This merely extends the amazing [[https://github.com/karthink/gptel/][gptel]] by Karthik Chikmagalur, who performed all the hard work.

This package is a fork of [[https://github.com/kamushadenes/gptel-extensions.el][gptel-extensions]] by [[https://github.com/kamushadenes][kamushadenes]].
