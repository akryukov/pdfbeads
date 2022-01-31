######################################################################
#
# PDFBeads -- convert scanned images to a single PDF file
# Version 1.0
#
# Unlike other PDF creation tools, this utility attempts to implement
# the approach typically used for DjVu books. Its key feature is
# separating scanned text (typically black, but indexed images with
# a small number of colors are also accepted) from halftone images
# placed into a background layer.
#
# Copyright (C) 2010 Alexey Kryukov (amkryukov@gmail.com).
# All rights reserved.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#######################################################################

spec = Gem::Specification.new do |s|
  s.name = 'pdfbeads'
  s.version = '1.1.3'
  s.licenses = ['GPL-3.0+']
  s.summary = "PDFBeads -- convert scanned images to a single PDF file."
  s.description = <<-EOF
    PDFBeads is a small utility written in Ruby which takes scanned
    page images and converts them into a single PDF file. Unlike other
    PDF creation tools, PDFBeads attempts to implement the approach
    typically used for DjVu books. Its key feature is separating scanned
    text (typically black, but indexed images with a small number of
    colors are also accepted) from halftone pictures. Each type of
    graphical data is encoded into its own layer with a specific
    compression method and resolution.
  EOF
  s.add_runtime_dependency('rmagick', '>= 3.2.0')
  s.add_runtime_dependency('nokogiri', '>= 1.5.10')
  s.add_runtime_dependency('pdf-reader', '>= 1.0.0')
  s.requirements << 'RMagick, v3.2.0 or greater'
  s.requirements << 'nokogiri, v1.5.10 or greater'
  s.requirements << 'PDF::Reader, v1.0.0 or greater'
  s.executables = [ 'pdfbeads' ]
  s.files = Dir['lib/**/*.rb'] + Dir['bin/*'] + Dir['doc/*.html'] +
            ['README', 'COPYING', 'ChangeLog']
  s.extra_rdoc_files = ['README', 'COPYING', 'ChangeLog']
  s.author = "Alexey Kryukov"
  s.email = "amkryukov@gmail.com"
  s.homepage = "http://pdfbeads.rubyforge.org"
end
