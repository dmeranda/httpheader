# -*- coding: utf-8 -*-
from distutils.core import setup
name = "httpheader"
version = "1.1"
setup( name=name,
       version=version,
       py_modules=[name],
       author='Deron Meranda',
       author_email='deron.meranda@gmail.com',
       url='http://deron.meranda.us/python/%s/' % name,
       download_url='http://deron.meranda.us/python/%(name)s/dist/%(name)s-%(version)s.tar.gz'\
           % {'name':name, 'version':version},
       description='Utility functions to parse HTTP 1.1 protocol headers.',
       long_description="""

This module provides several utility functions for parsing various
HTTP headers; such as those dealing with byte range requests and
content negotiation (Accept-xxx headers).  It tries to be an exact
implementation of the HTTP 1.1 RFC.

""".strip(),
       license='GNU LGPL',
       keywords=['http'],
       classifiers=["License :: OSI Approved :: GNU Lesser General Public License v2 or later (LGPLv2+)",
                    "Development Status :: 6 - Mature",
                    "Intended Audience :: Developers",
                    "Programming Language :: Python :: 2 :: Only",
                    "Topic :: Internet :: WWW/HTTP",
                    "Topic :: Software Development :: Libraries :: Python Modules"
                    ]
       )

