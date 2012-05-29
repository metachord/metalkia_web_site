#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sys
import struct
import re
from BeautifulSoup import BeautifulSoup, NavigableString
import argparse


parser = argparse.ArgumentParser(description='Sanitize HTML.')
parser.add_argument('--oformat', nargs='?', default="html",
                    dest="oformat",
                    help='output format: [html | text]')

args = parser.parse_args()

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
                         'span', 'src', 'start', 'style', 'summary', 'tabindex', 'target', 'title', 'type',
                         'usemap', 'valign', 'value', 'vspace', 'width']

if args.oformat == "text":
    acceptable_elements = []
    acceptable_attributes = []
elif args.oformat == "html":
    pass
elif args.oformat == "markdown":
    import markdown

BeautifulSoup.QUOTE_TAGS = ['pre', 'code']

def clean_html( fragment, quirks=True ):
    while True:
        soup = BeautifulSoup( fragment )
        removed = False
        for tag in soup.findAll(True): # find all tags
            if tag.name == 'script':
                if re.match("https?:\/\/(?:www\.)?gist\.github\.com\/\d+\.js", dict(tag.attrs)[u'src']):
                    removed = False
                if re.match("(https?:)?\/\/speakerdeck.com\/assets\/embed\.js", dict(tag.attrs)[u'src']):
                    removed = False
                else:
                    tag.extract()
                    removed = True
            elif tag.name == 'iframe':
                if re.match("https?:\/\/(?:www\.)?youtube(?:-nocookie)?\.com\/", dict(tag.attrs)[u'src']):
                    removed = False
                elif re.match("https?:\/\/(?:www\.)?slideshare\.net\/slideshow\/embed_code\/\d+", dict(tag.attrs)[u'src']):
                    removed = False
                else:
                    tag.extract()
                    removed = True
            elif tag.name == 'mt-cut':
                removed = False
            elif tag.name == 'pre':
                pass
            elif tag.name == 'div' and dict(tag.attrs)[u'class'] == 'codehilite':
                removed = False
            elif tag.name not in acceptable_elements:
                tag.extract() # remove the bad ones
                removed = True
            else: # it might have bad attributes
                if not len(tag.contents):
                    pass
                elif quirks:
                    stopNode = tag.contents[-1]
                    strings = []
                    current = tag.contents[0]
                    while current:
                        if not isinstance(current, NavigableString):
                            text = str(current)
                            pass
                        else:
                            text = current
                            text = re.sub(re.compile('\n[\n]+', re.UNICODE), '</p><p>', text)
                            text = re.sub(re.compile('\n', re.UNICODE), '<br />', text)
                            current.replaceWith(text)
                        current = current.next

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

def clean_text(text):
    text = re.sub(re.compile('</?p>', re.UNICODE), '\n', text)
    text = re.sub(re.compile('<br(\s*/?\s*)?>', re.UNICODE), '\n', text)
    text = re.sub(re.compile('<.*?>', re.UNICODE), '', text)
    return text

inlen = struct.unpack('>I', sys.stdin.read(4))
intext = unicode(sys.stdin.read(inlen[0]), "utf-8")

if args.oformat == "html":
    outtext = clean_html(intext).encode('utf-8')
elif args.oformat == "markdown":
    outtext = clean_html(markdown.markdown(intext, ['codehilite']), quirks=False).encode('utf-8')
elif args.oformat == "text":
    outtext = clean_text(intext).encode('utf-8')

outlen = len(outtext)

sys.stdout.write(struct.pack('>I', outlen))
sys.stdout.write(outtext)
