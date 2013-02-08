<!-- -*- coding: utf-8 -*-
****
****   This file is in Markdown format. It is primarily used to
****   display a top-level project page in the Github repository.
****
****   For a more detailed README, see the "README.txt" file.
****
-->
httpheader
==========

Httpheader is a Python module for dealing with HTTP headers and
content negotiation.  It provides a set of utility functions and
classes which properly implement all the details and edge cases of the
HTTP 1.1 protocol headers.  Httpheader is intended to be used as part
of a larger web framework or any application that must deal with HTTP.

In particular, httpheader can handle:

  * Byte range requests (multipart/byteranges)
  * Content negotiation (content type, language, all the Accept-* style headers; including full support for priority/qvalue handling.
  * Content/media type parameters
  * Conversion to and from HTTP date and time formats

More information
================
Complete documentation and additional information is available on the
[httpheader project homepage](http://deron.meranda.us/python/httpheader/).
