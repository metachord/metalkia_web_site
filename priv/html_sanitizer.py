#!/usr/bin/env python
import sys
import struct
from BeautifulSoup import BeautifulSoup

acceptable_elements = ['a', 'abbr', 'acronym', 'address', 'area', 'b', 'big',
                       'blockquote', 'br', 'button', 'caption', 'center', 'cite', 'code', 'col',
                       'colgroup', 'dd', 'del', 'dfn', 'dir', 'div', 'dl', 'dt', 'em',
                       'font', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'hr', 'i', 'img',
                       'ins', 'kbd', 'label', 'legend', 'li', 'map', 'menu', 'ol',
                       'p', 'pre', 'q', 's', 'samp', 'small', 'span', 'strike',
                       'strong', 'sub', 'sup', 'table', 'tbody', 'td', 'tfoot', 'th',
                       'thead', 'tr', 'tt', 'u', 'ul', 'var']

acceptable_attributes = ['abbr', 'accept', 'accept-charset', 'accesskey',
                         'action', 'align', 'alt', 'axis', 'border', 'cellpadding', 'cellspacing',
                         'char', 'charoff', 'charset', 'checked', 'cite', 'clear', 'cols',
                         'colspan', 'color', 'compact', 'coords', 'datetime', 'dir',
                         'enctype', 'for', 'headers', 'height', 'href', 'hreflang', 'hspace',
                         'id', 'ismap', 'label', 'lang', 'longdesc', 'maxlength', 'method',
                         'multiple', 'name', 'nohref', 'noshade', 'nowrap', 'prompt',
                         'rel', 'rev', 'rows', 'rowspan', 'rules', 'scope', 'shape', 'size',
                         'span', 'src', 'start', 'summary', 'tabindex', 'target', 'title', 'type',
                         'usemap', 'valign', 'value', 'vspace', 'width']

def clean_html( fragment ):
    while True:
        soup = BeautifulSoup( fragment )
        removed = False
        for tag in soup.findAll(True): # find all tags
            if tag.name not in acceptable_elements:
                tag.extract() # remove the bad ones
                removed = True
            else: # it might have bad attributes
                # a better way to get all attributes?
                for attr in tag._getAttrMap().keys():
                    if attr not in acceptable_attributes:
                        del tag[attr]

        # turn it back to html
        fragment = unicode(soup)

        if removed:
            # we removed tags and tricky can could exploit that!
            # we need to reparse the html until it stops changing
            continue # next round

        return fragment

inlen = struct.unpack('>I', sys.stdin.read(4))
intext = sys.stdin.read(inlen[0])

outtext = clean_html(intext)
outlen = len(outtext)

sys.stdout.write(struct.pack('>I', outlen))
sys.stdout.write(outtext)
