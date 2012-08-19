#!/usr/bin/env python
# -*- coding: utf-8 -*-
""" Utility functions to work with Accept-* headers as defined by HTTP 1.1.

This module provides some utility functions useful for writing
websites which want to deal with some of the HTTP protocol headers;
especially the correct interpretation of the various Accept-* style
headers, content negotiation, and so forth.

There are a few classes defined by this module:

 * class content_type  -- media types such as 'text/plain'
 * class language_tag  -- language tags such as 'en-US'

The primary functions this modules may be categorized as follows:

 * Content negotiation functions...
     * acceptable_content_type()
     * acceptable_language()
     * acceptable_charset()
     * acceptable_encoding()

 * Low-level string parsing functions...
     * parse_accept_header()
     * parse_comment()
     * parse_token_or_quoted_string()

See also:

 * RFC 2046, "(MIME) Part Two: Media Types", November 1996.
             <http://www.ietf.org/rfc/rfc2046.txt>
 * RFC 2616, "Hypertext Transfer Protocol -- HTTP/1.1", June 1999.
             <http://www.ietf.org/rfc/rfc2616.txt>
 * RFC 3066, "Tags for the Identification of Languages", January 2001.
             <http://www.ietf.org/rfc/rfc3066.txt>
"""

__author__ = """Deron Meranda <http://deron.meranda.us/>"""
__date__ = "2005-12-19"
__version__ = "1.0"
__credits__ = """Copyright (c) 2005 Deron E. Meranda <http://deron.meranda.us/>
Licensed under GNU LGPL 2.1 or later.  See <http://www.fsf.org/>.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
"""

# Character classes from RFC 2616 section 2.2
separators = '()<>@,;:\\"/[]?={} \t'
LWS = ' \t\n\r'  # linear white space
CRLF = '\r\n'
DIGIT = '0123456789'
HEX = '0123456789ABCDEFabcdef'

try:
    # Turn into set types (for Python 2.4 or greater)
    separators = frozenset([c for c in separators])
    LWS = frozenset([c for c in LWS])
    CRLF = frozenset([c for c in CRLF])
    DIGIT = frozenset([c for c in DIGIT])
    HEX = frozenset([c for c in HEX])
    del c
except NameError:
    # Python 2.3 or earlier, leave as simple strings
    pass


def _is_string( obj ):
    """Returns True if the object is a string or unicode type."""
    return isinstance(obj,str) or isinstance(obj,unicode)


def is_token(s):
    """Determines if the string is a legal token."""
    for c in s:
        if ord(c) < 32 or ord(c) > 128 or c in separators:
            return False
    return True

def parse_token_or_quoted_string(s, start=0, allow_quoted=True, allow_token=True):
    """Parses a token or a quoted-string.

    's' is the string to parse, while start is the position within the
    string where parsing should begin.  It will returns a tuple
    (token, chars_consumed), with all \-escapes and quotation already
    processed.

    Syntax is according to BNF rules in RFC 2161 section 2.2,
    specifically the 'token' and 'quoted-string' declarations.
    Syntax errors in the input string will result in ValueError
    being raised.

    If allow_quoted is False, then only tokens will be parsed instead
    of either a token or quoted-string.

    If allow_token is False, then only quoted-strings will be parsed
    instead of either a token or quoted-string.
    """
    if not allow_quoted and not allow_token:
        raise ValueError('Parsing can not continue with options provided')
    if start >= len(s):
        return IndexError('Starting position is beyond the end of the string')
    has_quote = (s[start] == '"')
    if has_quote and not allow_quoted:
        return ValueError('A quoted string was not expected')
    if not has_quote and not allow_token:
        return ValueError('Expected a quotation mark')

    s2 = ''
    pos = start
    if has_quote:
        pos += 1
    while pos < len(s):
        c = s[pos]
        if c == '\\' and has_quote:
            # Note this is not C-style escaping; the character after the \ is
            # taken literally.
            pos += 1
            if pos == len(s):
                raise ValueError("End of string while expecting a character after '\\'")
            s2 += s[pos]
            pos += 1
        elif c == '"' and has_quote:
            break
        elif not has_quote and (c in self.separators or ord(c)<32 or ord(c)>127):
            break
        else:
            s2 += c
            pos += 1
    if has_quote:
        # Make sure we have a closing quote mark
        if pos >= len(s) or s[pos] != '"':
            raise ValueError('Quoted string is missing closing quote mark')
        else:
            pos += 1
    return s2, (pos - start)


def parse_comment(s, start=0):
    """Parses a ()-style comment from a header value.

    Returns tuple (comment, chars_consumed), where the comment will
    have had the outer-most parentheses and white space stripped.  Any
    nested comments will still have their parentheses and whitespace
    left intact.

    All \-escaped quoted pairs will have been replaced with the actual
    characters they represent, even within the inner nested comments.

    You should note that only a few HTTP headers, such as User-Agent
    or Via, allow ()-style comments within the header value.

    """
    if start >= len(s):
        return IndexError('Starting position is beyond the end of the string')
    if s[start] != '(':
        raise ValueError('Comment must begin with opening parenthesis')

    s2 = ''
    nestlevel = 1
    pos = start + 1
    while pos < len(s) and s[pos] in LWS:
        pos += 1

    while pos < len(s):
        c = s[pos]
        if c == '\\':
            # Note this is not C-style escaping; the character after the \ is
            # taken literally.
            pos += 1
            if pos == len(s):
                raise ValueError("End of string while expecting a character after '\\'")
            s2 += s[pos]
            pos += 1
        elif c == '(':
            nestlevel += 1
            s2 += c
            pos += 1
        elif c == ')':
            nestlevel -= 1
            if nestlevel >= 1:
                s2 += c
                pos += 1
            else:
                break
        else:
            s2 += c
            pos += 1
    if nestlevel > 0:
        raise ValueError('End of string reached before comment was closed')
    # Now rstrip s2 of all LWS chars.
    while len(s2) and s2[-1] in LWS:
        s2 = s2[:-1]
    return s2, (pos - start)
    

def _split_at_qfactor( s ):
    """Splits a string at the quality factor (;q=) parameter.

    Returns the left and right substrings as a two-member tuple.

    """
    # It may be faster, but incorrect, to use s.split(';q=',1), since
    # HTTP allows any amount of linear white space (LWS) to appear
    # between the parts, so it could also be "; q = ".

    # We do this parsing 'manually' for speed rather than using a
    # regex, which would be r';[ \t\r\n]*q[ \t\r\n]*=[ \t\r\n]*'

    pos = 0
    while 0 <= pos < len(s):
        pos = s.find(';', pos)
        if pos < 0:
            break # no more parameters
        startpos = pos
        pos = pos + 1
        while pos < len(s) and s[pos] in LWS:
            pos = pos + 1
        if pos < len(s) and s[pos] == 'q':
            pos = pos + 1
            while pos < len(s) and s[pos] in LWS:
                pos = pos + 1
            if pos < len(s) and s[pos] == '=':
                pos = pos + 1
                while pos < len(s) and s[pos] in LWS:
                    pos = pos + 1
                return ( s[:startpos], s[pos:] )
    return (s, '')


def parse_accept_header( header_value ):
    """Parses the value of an HTTP Accept-*: style header with quality factors.

    The value of the header as a string should be passed in; without
    the header name itself.
    
    This will parse the value of any of the HTTP headers "Accept",
    "Accept-Charset", "Accept-Encoding", or "Accept-Language".  These
    headers are similarly formatted, in that they are a list of items
    with associated quality factors.  The quality factor, or qvalue,
    is a number in the range [0.0..1.0] which indicates the relative
    preference of each item.

    This function returns a list of those items, sorted by preference
    (from most-prefered to least-prefered).  Each item in the returned
    list is actually a tuple consisting of:

       ( item_name, item_parms, qvalue, accept_parms )

    As an example, the following string,
        text/plain; charset="utf-8"; q=.5; columns=80
    would be parsed into this resulting tuple,
        ( 'text/plain', [('charset','utf-8')], 0.5, [('columns','80')] )

    The value of the returned item_name depends upon which header is
    being parsed, but for example it may be a MIME content or media
    type (without parameters), a language tag, or so on.  Any optional
    parameters (delimited by semicolons) occuring before the "q="
    attribute will be in the item_parms list as (attribute,value)
    tuples in the same order as they appear in the header.  Any quoted
    values will have been unquoted and unescaped.

    The qvalue is a floating point number in the inclusive range 0.0
    to 1.0, and roughly indicates the preference for this item.
    Values outside this range will be capped to the closest extreme.

         (!) Note that a qvalue of 0 indicates that the item is
         explicitly NOT acceptable to the user agent, and should be
         handled differently by the caller.

    The accept_parms, like the item_parms, is a list of any attributes
    occuring after the "q=" attribute, and will be in the list as
    (attribute,value) tuples in the same order as they occur.
    Usually accept_parms will be an empty list, as the HTTP spec
    allows these extra parameters in the syntax but does not
    currently define any possible values.

    All empty items will be removed from the list.  However, duplicate
    or conflicting values are not detected or handled in any way by
    this function.

    """

    accept_list = []
    # itemnum is used to insure a stable-sort later.  Could use enumerate(),
    # but we want to preserve Python 2.2 compatibility.
    itemnum = 0
    for item, qvalue in [ _split_at_qfactor(v.strip()) for v in header_value.split(',') ]:
        if not item[0]:
            continue # an empty list item
        if not qvalue:
            qvalue, accept_ext = 1, ''
        else:
            if ';' in qvalue:
                qvalue, accept_ext = qvalue.split(';', 1)
            else:
                accept_ext = ''
            try:
                qvalue = float(qvalue.strip())
            except ValueError:
                qvalue = 0.1 # Mangled q-value, assume low quality
            if qvalue < 0: # Insure in range 0 to 1.
                qvalue = 0
            elif qvalue > 1:
                qvalue = 1
        accept_list.append( (qvalue, itemnum, item, accept_ext) )
        itemnum = itemnum + 1
    accept_list.sort()
    accept_list.reverse()
    # Reformat the tuples in the list, so the name comes first.  We also
    # discard the itemnum ordinal, since it was only there to enforce
    # a stable sort.
    accept_list = [(x[2], x[0], x[3]) for x in accept_list]
    return accept_list


class content_type(object):
    """This class represents a media type (aka a MIME content type).

    You initialize these by passing in a content-type declaration string,
    such as "text/plain", to the constructor or to the set() method.

    Normally you will get the value by using str(), or optionally you
    can access the components via the 'major', 'minor', and 'parmdict'
    members.

    """
    def __init__(self, content_type_string=None):
        if content_type_string:
            self.set( content_type_string )
        else:
            self.set( '*/*' )

    def set(self, content_type_string):
        """Parses the content type string and sets this object to it's value."""
        major, minor, pdict = self._parse_media_type( content_type_string )
        self._set_major( major )
        self._set_minor( minor )
        self.parmdict = pdict
        
    def _get_major(self):
        return self._major
    def _set_major(self, s):
        s = s.lower()  # case-insentive
        if not is_token(s):
            raise ValueError('Major media type contains an illegal character')
        self._major = s

    def _get_minor(self):
        return self._minor
    def _set_minor(self, s):
        s = s.lower()  # case-insentive
        if not is_token(s):
            raise ValueError('Minor media type contains an illegal character')
        self._minor = s

    major = property(_get_major,_set_major,doc="Major media classification")
    minor = property(_get_minor,_set_minor,doc="Minor media sub-classification")

    def __str__(self):
        """String value."""
        s = '%s/%s' % (self.major, self.minor)
        if self.parmdict:
            extra = '; '.join([ '%s=%s' % (a[0],self._quote(a[1])) \
                                for a in self.parmdict.items()])
            s += '; ' + extra
        return s

    def __unicode__(self):
        """Unicode string value."""
        return unicode(self.__str__())

    def __repr__(self):
        """Python representation of this object."""
        s = '%s(%s)' % (self.__class__.__name__, repr(self.__str__()))
        return s


    def __hash__(self):
        """Hash this object; the hash is dependent only upon the value."""
        return hash(str(self))

    def __getstate__(self):
        """Pickler"""
        return str(self)

    def __setstate__(self, state):
        """Unpickler"""
        self.set(state)

    def __len__(self):
        """Logical length of this media type.
        For example:
           len('*/*')  -> 0
           len('image/*') -> 1
           len('image/png') -> 2
           len('text/plain; charset=utf-8')  -> 3
           len('text/plain; charset=utf-8; filename=xyz.txt') -> 4

        """
        if self.major == '*':
            return 0
        elif self.minor == '*':
            return 1
        else:
            return 2 + len(self.parmdict)

    def __eq__(self, other):
        """Equality test."""
        return self.major == other.major and \
                   self.minor == other.minor and \
                   self.parmdict == other.parmdict

    def __ne__(self, other):
        """Inequality test."""
        return not self.__eq__(other)
            
    def _quote(self, val):
        """Produces a token, or a quoted string if necessary, from the input string value.
        """
        need_quotes = False
        s = ''
        for c in val:
            if c in self.separators or ord(c)<32 or ord(c)>127:
                need_quotes = True
                s += "\\%s" % c
            else:
                s += c
        if need_quotes:
            s = '"%s"' % s
        return s

    def _parse_media_type(self, media_type):
        """Parses a media type (MIME type) designator into it's parts.

        Given a media type string, returns a tuple of it's parts.

            (major,minor,parmlist).

        Examples:
            image/png -> ('image','png',[])
            text/plain; charset="utf-16be" -> ('text','plain',[('charset,'utf-16be')])

        """

        ctmaj, ctmin = media_type.split('/', 1)
        parmlist = []
        if ';' in ctmin:
            ctmin, ctparms = ctmin.split(';', 1)
            i = 0
            while i < len(ctparms):
                while i < len(ctparms) and ctparms[i] in LWS:
                    i += 1
                pname, i = parse_token_or_quoted_string( ctparms, start=i, allow_quoted=False )
                while i < len(ctparms) and ctparms[i] in LWS:
                    i += 1
                #print 'pname=[%s]' % pname, 'at', i
                if i < len(ctparms) and ctparms[i] == '=':
                    i += 1
                    while i < len(ctparms) and ctparms[i] in LWS:
                        i += 1
                    #print 'found = at', i
                    pval, i = parse_token_or_quoted_string( ctparms, start=i, allow_quoted=True )
                else:
                    pval = ''
                #print 'pval=[%s]' % pval, 'at', i
                while i < len(ctparms) and ctparms[i] in LWS:
                    i += 1
                if i < len(ctparms):
                    if ctparms[i] == ';':
                        i += 1
                    else:
                        raise ValueError('Content type parmeters not separated with semicolons at "%s"' % ctparms[i:])
                parmlist.append( (pname, pval) )

            if i < len(ctparms):
                raise ValueError('Syntax error in content type parmeters')
        return (ctmaj, ctmin, parmlist)

    def media_type(self):
        """Returns the media 'type/subtype' string, without parameters."""
        return '%s/%s' % (self.major, self.minor)

    def is_wildcard(self):
        """Returns True if this is a 'something/*' media type.
        """
        return self.minor == '*'

    def is_universal_wildcard(self):
        """Returns True if this is the unspecified '*/*' media type.
        """
        return self.major == '*' and self.minor == '*'

    def is_composite(self):
        """Is this media type composed of multiple parts.
        """
        return self.major == 'multipart' or self.major == 'message'

    def is_xml(self):
        """Returns True if this media type is XML-based.

        Note this does not consider text/html to be XML, but
        application/xhtml+xml is.
        """
        return self.minor == 'xml' or self.minor.endswith('+xml')

# Some common media types
enctype_formdata = content_type('multipart/form-data')
enctype_urlencoded = content_type('application/x-www-form-urlencoded')
octet_stream = content_type('application/octet-stream')
html = content_type('text/html')
xhtml = content_type('application/xhtml+xml')


def acceptable_content_type( accept_header, content_types, ignore_wildcard=True ):
    """Determines if the given content type is acceptable to the user agent.

    The accept_header should be the value present in the HTTP
    "Accept:" header.  In mod_python this is typically obtained from
    the req.http_headers_in table; in WSGI it is environ["Accept"];
    other web frameworks may provide other methods of obtaining it.

    Optionally the accept_header parameter can instead be the list
    returned from the parse_accept_header() function in this module.

    The content_types argument should either be a single MIME media type
    string, or a sequence of them.  It represents the set of content
    types that the caller (server) is willing to send.

    This function determines the content type which is the most prefered
    and is acceptable to both the user agent and the caller.  If one
    is negotiated, it will return a tuple of:

        (content_type, accept_parms)

    In most cases accept_parms will be an empty string (see
    description of parse_accept_header() for more details).  If no
    content type could be negotiated, then this function will return
    None (and the caller should typically cause an HTTP 406 Not
    Acceptable as a response).

    Note that the wildcarded content type "*/*" will be ignored, since
    it is often incorrectly sent by web browsers that don't really
    mean it.  To override this, call with ignore_wildcard=False.
    Partial wildcards such as "image/*" will always be processed,
    but be at a lower priority than a complete matching type.

    See also: RFC 2616 section 14.1, and
    <http://www.iana.org/assignments/media-types/>
    """
    if _is_string(accept_header):
        accept_list = parse_accept_header(accept_header)
    else:
        accept_list = accept_header

    if _is_string(content_types):
        content_types = [content_types]

    #server_ctlist = [content_type(ct) for ct in content_types]

    best = None
    for ct, qvalue, aargs in accept_list:
        try:
            # The content type is like "major/minor;parms...", parse it apart.
            ctmaj, ctmin, ctparms = parse_media_type(ct)
        except:
            continue # content type is malformed, skip it

        if ignore_wildcard and ctmaj=='*' and ctmin=='*':
            continue  # */* being ignored

        for server_ct in content_types:
            server_ct = content_type(server_ct)

            test_ctmaj, test_ctmin, test_ctparms = server_ct
            # The best match is determined first by the quality factor,
            # and then by the most specific match.

            print "comparing", server_ct
            matchlen = 0 # how specifically this one matches (0 is a non-match)
            if ctmaj == '*' and ctmin == '*':
                matchlen = 1   # */* is a 1
            elif ctmaj == test_ctmaj:
                if ctmin == '*':  # something/* is a 2
                    matchlen = 2
                elif ctmin == test_ctmin: # something/something is a 3
                    matchlen = 3
                    if ctparms: # must make sure all the parms match too
                        for pname, pval in ctparms.items():
                            if test_ctparms.get(pname) == pval:
                                matchlen = matchlen + 1
                            else:
                                matchlen = 0
                                break
                else:
                    matchlen = 0

            if matchlen:
                if not best \
                       or matchlen > best[3] \
                       or (matchlen == best[3] and qvalue > best[1]):
                    # This match is better
                    best = (ct, qvalue, aargs, matchlen)
    if not best or best[1] <= 0:
        return None
    return (best[0], best[2])


def _canonical_charset( charset ):
    return charset.upper()

def acceptable_charset( accept_charset_header, charsets, ignore_wildcard=True, default='ISO-8859-1' ):
    """
    Determines if the given charset is acceptable to the user agent.

    The accept_charset_header should be the value present in the HTTP
    "Accept-Charset:" header.  In mod_python this is typically
    obtained from the req.http_headers table; in WSGI it is
    environ["Accept-Charset"]; other web frameworks may provide other
    methods of obtaining it.

    Optionally the accept_charset_header parameter can instead be the
    list returned from the parse_accept_header() function in this
    module.

    The charsets argument should either be a charset identifier string,
    or a sequence of them.

    This function returns the charset identifier string which is the
    most prefered and is acceptable to both the user agent and the
    caller.  It will return the default value if no charset is negotiable.
    
    Note that the wildcarded charset "*" will be ignored.  To override
    this, call with ignore_wildcard=False.

    See also: RFC 2616 section 14.2, and
    <http://www.iana.org/assignments/character-sets>

    """
    if default:
        default = _canonical_charset(default)

    if _is_string(accept_charset_header):
        accept_list = parse_accept_header(accept_charset_header)
    else:
        accept_list = accept_charset_header

    if _is_string(charsets):
        charsets = [_canonical_charset(charsets)]
    else:
        charsets = [_canonical_charset(c) for c in charsets]

    # Note per RFC that 'ISO-8859-1' is special, and is implictly in the
    # accept list with q=1; unless it is already in the list, or '*' is in the list.

    best = None
    for c, qvalue, junk in accept_list:
        if c == '*':
            default = None
            if ignore_wildcard:
                continue
            if not best or qvalue > best[1]:
                best = (c, qvalue)
        else:
            c = _canonical_charset(c)
            for test_c in charsets:
                if c == default:
                    default = None
                if c == test_c and (not best or best[0]=='*' or qvalue > best[1]):
                    best = (c, qvalue)
    if default and default in [test_c.upper() for test_c in charsets]:
        best = (default, 1)
    if best[0] == '*':
        best = (charsets[0], best[1])
    return best



class language_tag(object):
    """This class represents an RFC 3066 language tag.

    Initialize objects of this class with a single string representing
    the language tag, such as "en-US".
        
    Case is insensitive. Wildcarded subtags are ignored or stripped as
    they have no significance, so that "en-*" is the same as "en".
    However the universal wildcard "*" language tag is kept as-is.

    Note that although relational operators such as < are defined,
    they only form a partial order based upon specialization.

    Thus for example,
         "en" <= "en-US"
    but,
         not "en" <= "de", and
         not "de" <= "en".

    """

    def __init__(self, tagname):
        """Initialize objects of this class with a single string representing
        the language tag, such as "en-US".  Case is insensitive.

        """

        self.parts = tagname.lower().split('-')
        while len(self.parts) > 1 and self.parts[-1] == '*':
            del self.parts[-1]

    def __len__(self):
        """Number of subtags in this tag."""
        if len(self.parts) == 1 and self.parts[0] == '*':
            return 0
        return len(self.parts)

    def __str__(self):
        """The standard string form of this language tag."""
        a = []
        if len(self.parts) >= 1:
            a.append(self.parts[0])
        if len(self.parts) >= 2:
            if len(self.parts[1]) == 2:
                a.append( self.parts[1].upper() )
            else:
                a.append( self.parts[1] )
        a.extend( self.parts[2:] )
        return '-'.join(a)

    def __unicode__(self):
        """The unicode string form of this language tag."""
        return unicode(self.__str__())

    def __repr__(self):
        """The python representation of this language tag."""
        s = '%s("%s")' % (self.__class__.__name__, self.__str__())
        return s

    def superior(self):
        """Returns another instance of language_tag which is the superior.

        Thus en-US gives en, and en gives *.

        """
        if len(self) <= 1:
            return self.__class__('*')
        return self.__class__( '-'.join(self.parts[:-1]) )

    def all_superiors(self, include_wildcard=False):
        """Returns a list of this language and all it's superiors.

        If include_wildcard is False, then "*" will not be among the
        output list, unless this language is itself "*".

        """
        langlist = [ self ]
        l = self
        while not l.is_universal_wildcard():
            l = l.superior()
            if l.is_universal_wildcard() and not include_wildcard:
                continue
            langlist.append(l)
        return langlist
                
    def is_universal_wildcard(self):
        """Returns True if this language tag represents all possible
        languages, by using the reserved tag of "*".

        """
        return len(self.parts) == 1 and self.parts[0] == '*'

    def dialect_of(self, other, ignore_wildcard=True):
        """Is this language a dialect (or subset/specialization) of another.

        This method returns True if this language is the same as or a
        specialization (dialect) of the other language_tag.

        If ignore_wildcard is False, then all languages will be
        considered to be a dialect of the special language tag of "*".

        """
        if not ignore_wildcard and self.is_universal_wildcard():
            return True
        for i in range( min(len(self), len(other)) ):
            if self.parts[i] != other.parts[i]:
                return False
        if len(self) >= len(other):
            return True
        return False

    def __eq__(self, other):
        """== operator. Are the two languages the same?"""

        return self.parts == other.parts

    def __neq__(self, other):
        """!= operator. Are the two languages different?"""

        return not self.__eq__(other)

    def __lt__(self, other):
        """< operator. Returns True if the other language is a more
        specialized dialect of this one."""

        return other.dialect_of(self) and self != other

    def __le__(self, other):
        """<= operator. Returns True if the other language is the same
        as or a more specialized dialect of this one."""
        return other.dialect_of(self)

    def __gt__(self, other):
        """> operator.  Returns True if this language is a more
        specialized dialect of the other one."""

        return self.dialect_of(other) and self != other

    def __ge__(self, other):
        """>= operator.  Returns True if this language is the same as
        or a more specialized dialect of the other one."""

        return self.dialect_of(other)



def acceptable_language( accept_language, languages, ignore_wildcard=True, assume_superiors=True ):
    """Determines if the given language is acceptable to the user agent.

    The accept_language should be the value present in the HTTP
    "Accept-Language:" header.  In mod_python this is typically
    obtained from the req.http_headers_in table; in WSGI it is
    environ["Accept-Language"]; other web frameworks may provide other
    methods of obtaining it.

    Optionally the accept_language parameter can instead be the list
    resulting from the parse_accept_header() function defined in this
    module.

    The languages argument should either be a single language string,
    a language_tag object, or a sequence of them.  It represents the
    set of languages that the caller is willing to send.

    Note that the wildcarded language tag "*" will be ignored.  To
    override this, call with ignore_wildcard=False, and even then
    it will be the lowest-priority choice regardless of it's
    quality factor (as per HTTP spec).

    If the assume_superiors is True then it the languages that the
    browser accepts will automatically include all superior languages.
    Any superior languages which must be added are done so with one
    half the qvalue of the language which is present.  For example, if
    the accept string is "en-US", then it will be treated as if it
    were "en-US, en;q=0.5".  Note that although the HTTP 1.1 spec says
    that browsers are supposed to encourage users to configure all
    acceptable languages, sometimes they don't, thus the ability
    for this function to assume this.  But setting assume_superiors
    to False will insure strict adherence to the HTTP 1.1 spec; which
    means that if the browser accepts "en-US", then it will not
    be acceptable to send just "en" to it.

    This function returns the language which is the most prefered and
    is acceptable to both the user agent and the caller.  It will
    return None if no language is negotiable, otherwise the return
    value is always an instance of language_tag.

    See also: RFC 3066 <http://www.ietf.org/rfc/rfc3066.txt>, and
    ISO 639, links at <http://en.wikipedia.org/wiki/ISO_639>, and
    <http://www.iana.org/assignments/language-tags>.
    
    """
    # Note special instructions from RFC 2616 sect. 14.1:
    #   "The language quality factor assigned to a language-tag by the
    #   Accept-Language field is the quality value of the longest
    #   language- range in the field that matches the language-tag."

    if _is_string(accept_language):
        accept_list = parse_accept_header(accept_language)
    else:
        accept_list = accept_header

    # Possibly add in any "missing" languages that the browser may
    # have forgotten to include in the list. Insure list is sorted so
    # more general languages come before more specific ones.

    accept_list.sort()
    all_tags = [a[0] for a in accept_list]
    if assume_superiors:
        to_add = []
        for lang, qvalue, aargs in accept_list:
            try:
                langtag = language_tag(lang)
            except:
                continue
            if len(langtag) >= 2:
                for sup in langtag.all_superiors( include_wildcard=False ):
                    suptag = str(sup)
                    if suptag not in all_tags:
                        to_add.append( (suptag, qvalue / 2, '') )
                        all_tags.append( suptag )
        accept_list.extend( to_add )

    if _is_string(languages):
        server_languages = [language_tag(languages)]
    else:
        server_languages = [language_tag(lang) for lang in languages]

    #print 'accept_list', repr(accept_list)
    #print 'server_languages', repr(server_languages)

    best = None  # tuple (langtag, qvalue, matchlen)
    
    for lang, qvalue, aargs in accept_list:
        # aargs is ignored for Accept-Language
        if qvalue <= 0:
            continue # UA doesn't accept this language
        try:
            # The content type is like "major/minor;parms...", parse it apart.
            langtag = language_tag(lang)
        except:
            continue # language tag is malformed, skip it

        if ignore_wildcard and langtag.is_universal_wildcard():
            continue  # "*" being ignored

        for svrlang in server_languages:
            # The best match is determined first by the quality factor,
            # and then by the most specific match.

            #print 'comparing UA language', repr(langtag), 'with server language', repr(svrlang)

            matchlen = -1 # how specifically this one matches (0 is a non-match)
            #if langtag.dialect_of( svrlang ):
            if svrlang.dialect_of( langtag, ignore_wildcard=ignore_wildcard ):
                matchlen = len(langtag)
                #print '   matches', repr(langtag), ', len', matchlen, ', qvalue', qvalue
                if not best \
                       or matchlen > best[2] \
                       or (matchlen == best[2] and qvalue > best[1]):
                    # This match is better
                    best = (langtag, qvalue, matchlen)
    if not best:
        return None
    return best[0]

