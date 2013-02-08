About "httpheader" by Deron Meranda

Version 1.1 (February 2013)
Homepage: http://deron.meranda.us/python/httpheader/
Author: Deron Meranda <http://deron.meranda.us/>

This is free software. See the included LICENSE.txt file for details.


====================
About httpheader
====================

Httpheader is a Python module for dealing with HTTP headers and
content negotiation.  It provides a set of utility functions and
classes which properly implement all the details and edge cases of the
HTTP 1.1 protocol headers.  Httpheader is intended to be used as part
of a larger web framework or any application that must deal with HTTP.

In particular, httpheader can handle:

  * Byte range requests (multipart/byteranges)
  * Content negotiation (content type, language, all the Accept-*
    style headers; including full support for priority/qvalue
    handling.
  * Content/media type parameters
  * Conversion to and from HTTP date and time formats


====================
Requirements
====================

This module is pure Python and does not have any additional
dependencies.  It requires Python 2.2 or any later 2.x version.  It is
not directly supported under Python 3.x.


====================
Using HTTP range requests
====================

This is a simple example of how to use this module to correctly handle
HTTP range requests (on a GET or HEAD operation); i.e., those that
specify the Ranges: header.

This example is presented in a way that is not bound to any particular
web framework, whether that be mod_python, WSGI, etc.  So some of the
actions are intentionally generic.  This example is also intentionally
simplified by assuming that you know ahead of time how big the whole
"file" is.  If you don't know this, such as if the file is generated
on-the-fly, then your actual use will be a bit more complex (mainly
because range requests may reference file offsets relative to the end
of the file rather than the beginning).

This is what you must do first, by whatever method is available to you
in your framework.


----------
# Preliminary handling of the HTTP request
ranges_hdr = http_request_headers.get( 'Range' )

# This example only supports GET and HEAD requests
if http_method not in ('GET', 'HEAD'):
    http_response_headers[ 'Allow' ] = 'GET, HEAD'
    set_http_return_code( 405, 'Method not allowed' )
    return # stop processing

# Determine the "file" being requested
file_size = size of the whole file being retrieved.
file_contents = the actual contents of the file
file_content_type = 'application/octet-stream' # or whatever it is
----------


Now try to parse the Range header and figure out what portion(s) of
the file has been requested.


----------
# Process the ranges header
import httpheader
ranges = None  # None means return the whole file
if ranges_hdr:
    try:
        ranges = httpheader.parse_range_header( ranges_hdr )
    except httpheader.ParseError:
        # Ranges header is malformed; you should ignore this error
        # per the RFC and just serve the whole file instead.
        ranges = None
    if ranges:
        try:
            # Simplify/optimize the range(s) if possible
            ranges.fix_to_size( file_size )
            ranges.coalesce()  # You may wish to skip this
        except httpheader.RangeUnsatisfiableError:
            # Requested range can not be satisifed, return error
            http_repsonse_header['Content-Range'] = '*/%d' % file_size
            set_http_return_code( 416, 'Range not satisfiable' )
            return # stop processing

        if ranges.is_single_range() and \
           ranges.range_specs[0].first == 0 and \
           ranges.range_specs[0].last == file_size - 1:
            # Effectively getting whole file in pieces
            ranges = None
----------


At this point, if ranges is None, then you're returning the whole file
just like you would in a normal request.  Be sure to return the
Accept-Ranges header to announce that you can process range requests
on future requests.


----------
if not ranges:
    # Returning the whole file
    http_response_header['Accept-Ranges'] = 'bytes'
    http_response_header['Content-Length'] = file_size
    http_response_header['Content-Type'] = file_content_type
    set_http_return_code( 200, 'OK' )
    if http_method != 'HEAD':
        http_response_write( file_contents )
----------


Otherwise you are returning one or more portions of the file.  First
we set up the various HTTP response headers.


----------
else: # ranges is not None
    if ranges.is_single_range():
        # Just one part of the file, no need for a multipart
        http_response_header['Content-Type'] = file_content_type
        http_response_header['Content-Range'] = \
            '%s %d-%d/%d' % ( 'bytes',
                               ranges.range_specs[0].first,
                               ranges.range_specs[0].last,
                               file_size - 1)
    else:
        # Multiple parts.  Pick a random MIME boundary string
        import randon, string
        boundary = '--------' + ''.join([ random.choice(string.letters) for i in range(32) ])
        http_response_header['Content-Type'] = \
            'multipart/byteranges; boundary=%s' % boundary

    http_response_header['Accept-Ranges'] = ranges.units # 'bytes'
    set_http_return_code( 206, 'Partial Content' )
----------


Now we output the actual content.


----------
    if http_method == 'GET':
        if ranges.is_single_range():
            # Single range output
            first = ranges.range_specs[0].first )
            last = ranges.range_specs[0].last + 1  # Notice the +1
            http_response_write( file_contents[first:last] )
        else: # not ranges.is_single_range()
            # Multipart!
            for R in ranges.range_specs:
                http_response_write( '%s\r\n' % boundary )
                http_response_write('Content-Type: %s\r\n' % file_content_type )
                http_response_write('Content-Range: %s %d-%d/%d\r\n' \
                        % (ranges.units, R.first, R.last, file_size) )
                http_response_write( '\r\n' )
                http_respose_write( file_contents[ R.first : R.last+1 ] )
                http_response_write( '\r\n' )
            http_response_write( '%s--\r\n' % boundary )
    elif http_method == 'HEAD':
        # do similar to above to output correct Content-Length,
        # but don't output body.
----------


Don't forget: In a real-world use, be sure to return the ETag and
other cache-friendly headers such as Last-Modified or Digest.  Just be
sure that those headers are with respect to the entire file, not just
the portions returned in the range request.

== End ==

