License: this entire project is licensed using the LGPL version 3. This
license is compatible with the Apache 2 license so you can use this code
in projects where the rest of the code is under the Apache 2 license.

Tuesday, July 14, 2009:

I am cloning this project from KBtextmaster_lisp, creating: KBnlp. I did
this by removing:

  web app stuff
  wordnet support
  xml-rpc server support
  some anaphora resolution support
  some linking support
  code using CIA World Factbook data

I have added new code from my natively compiled NLP command line
toolkit (written in Scheme, built with Gambit-C Scheme).

The purpose of creating the KBnlp sub-project is to create a useful
NLP library in Common Lisp that other people will have an easier time
working with: the KBtextmaster_lisp code base has been developed between
1998 and 2008 and the code base is more than a little crufty. KBnlp
contains a lot of refactoring and general code cleanup.

The LGPL license also applies to the data files provided in the
subdirectory "data". I have attempted to reduce the size of the
data for human names, places, and products by eliminating terms
and words that are seldom used.

I work as a consultant: please consider hiring me to customize
my NLP libraries (both KBnlp and KBtextmaster_lisp) for your
projects.

-Mark Watson  http://markwatson.com
