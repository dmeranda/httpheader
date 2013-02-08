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

Summary of functions
====================

There are a few classes defined by this module:

  * class content_type   -- media types such as 'text/plain'
  * class language_tag   -- language tags such as 'en-US'
  * class range_set      -- a collection of (byte) range specifiers
  * class range_spec     -- a single (byte) range specifier

The primary functions in this module may be categorized as follows:

  * Content negotiation functions...
     * acceptable_content_type()
     * acceptable_language()
     * acceptable_charset()
     * acceptable_encoding()

  * Mid-level header parsing functions...
     * parse_accept_header()
     * parse_accept_language_header()
     * parse_range_header()
 
  * Date and time...
     * http_datetime()
     * parse_http_datetime()

  * Utility functions...
     * quote_string()
     * remove_comments()
     * canonical_charset()

  * Low level string parsing functions...
     * parse_comma_list()
     * parse_comment()
     * parse_qvalue_accept_list()
     * parse_media_type()
     * parse_number()
     * parse_parameter_list()
     * parse_quoted_string()
     * parse_range_set()
     * parse_range_spec()
     * parse_token()
     * parse_token_or_quoted_string()

And there are some specialized exception classes:

  * RangeUnsatisfiableError
  * RangeUnmergableError
  * ParseError


See also
========

  * RFC 2616, "Hypertext Transfer Protocol -- HTTP/1.1", June 1999.
    <http://www.ietf.org/rfc/rfc2616.txt>
    Errata at <http://purl.org/NET/http-errata>
  * RFC 2046, "(MIME) Part Two: Media Types", November 1996.
    <http://www.ietf.org/rfc/rfc2046.txt>
  * RFC 3066, "Tags for the Identification of Languages", January 2001.
    <http://www.ietf.org/rfc/rfc3066.txt>


More information
================

Complete documentation and additional information is available on the
[httpheader project homepage](http://deron.meranda.us/python/httpheader/).
