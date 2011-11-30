#!/usr/bin/ruby
# Encoding: utf-8
require 'rubygems'
require 'sanitize'

def errlog(str)
  File.open('/tmp/san.log', 'a') do |f|
    f.puts str
  end
end

class HTMLSanitize
  module Config
    RELAXED = {
      :elements => %w[
        a abbr b bdo blockquote br caption cite code col colgroup dd del dfn dl
        dt em figcaption figure h1 h2 h3 h4 h5 h6 hgroup i img ins kbd li mark
        ol p pre q rp rt ruby s samp small strike strong sub sup table tbody td
        tfoot th thead time tr u ul var wbr
      ],

      :attributes => {
        :all         => ['dir', 'lang', 'title'],
        'a'          => ['href'],
        'blockquote' => ['cite'],
        'col'        => ['span', 'width'],
        'colgroup'   => ['span', 'width'],
        'del'        => ['cite', 'datetime'],
        'img'        => ['align', 'alt', 'height', 'src', 'width'],
        'ins'        => ['cite', 'datetime'],
        'ol'         => ['start', 'reversed', 'type'],
        'q'          => ['cite'],
        'table'      => ['summary', 'width'],
        'td'         => ['abbr', 'axis', 'colspan', 'rowspan', 'width'],
        'th'         => ['abbr', 'axis', 'colspan', 'rowspan', 'scope', 'width'],
        'time'       => ['datetime', 'pubdate'],
        'ul'         => ['type']
      },

      :protocols => {
        'a'          => {'href' => ['ftp', 'http', 'https', 'mailto', :relative]},
        'blockquote' => {'cite' => ['http', 'https', :relative]},
        'del'        => {'cite' => ['http', 'https', :relative]},
        'img'        => {'src'  => ['http', 'https', :relative]},
        'ins'        => {'cite' => ['http', 'https', :relative]},
        'q'          => {'cite' => ['http', 'https', :relative]}
      },
      :transformers =>
      [
       lambda do |env|
         node      = env[:node]
         node_name = env[:node_name]
         return if env[:is_whitelisted] || !node.element?
         return unless node_name == 'iframe'
         return unless node['src'] =~ /\Ahttps?:\/\/(?:www\.)?youtube(?:-nocookie)?\.com\//
         Sanitize.clean_node!(node, {
                                :elements => %w[iframe],
                                :attributes => {
                                  'iframe'  => %w[allowfullscreen frameborder height src width]
                                }
                              })
         {:node_whitelist => [node]}
       end,

       lambda do |env|
         node      = env[:node]
         node_name = env[:node_name]
         return if env[:is_whitelisted] || !node.element?
         return unless node_name == 'script'
         return unless node['src'] =~ /\Ahttps?:\/\/(?:www\.)?gist\.github\.com\/\d+\.js/
         Sanitize.clean_node!(node, {
                                :elements => %w[script],
                                :attributes => {
                                  'script'  => %w[src]
                                }
                              })
         {:node_whitelist => [node]}
       end
      ]
    }
  end
end

errlog("Start")

inlen = STDIN.read(4).unpack('N')
intext = STDIN.read(inlen[0])

outtext = Sanitize.clean(intext, HTMLSanitize::Config::RELAXED)
outlen = outtext.size()

STDOUT.write([outlen].pack('N'))
STDOUT.write(outtext)
