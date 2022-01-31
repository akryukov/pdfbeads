#!/usr/bin/env ruby
# encoding: UTF-8

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

require 'time'
require 'stringio'

# The key class where the actual generation of a PDF file is performed.
class PDFBeads::PDFBuilder
  require 'pdfbeads/pdfdoc'
  require 'pdfbeads/pdffont'
  require 'pdfbeads/pdflabels'
  require 'pdfbeads/pdftoc'

  @@cmodes = Hash[
    'BilevelType'              => '/DeviceGray',
    'GrayscaleType'            => '/DeviceGray',
    'PaletteType'              => '/Indexed',
    'PaletteMatteType'         => '/Indexed',
    'TrueColorType'            => '/DeviceRGB',
    'TrueColorMatteType'       => '/DeviceRGB',
    'ColorSeparationType'      => '/DeviceCMYK',
    'ColorSeparationMatteType' => '/DeviceCMYK',
    'PaletteBilevelMatteType'  => '/DeviceGray'
  ]
  @@cmodes.default = '/DeviceRGB'

  def initialize( pdfargs )
    @pdfargs = pdfargs
    @now = Time.now()
    @doc = Doc.new()
    @fdata = FontDataProvider.new()

    @dictpath = ''
    @dictobj  = nil
  end

  def process( pagefiles,st_format )
    labels = toc = nil
    labels = PDFLabels.new( @pdfargs[:labels] ) unless @pdfargs[:labels].nil?
    toc    = PDFTOC.new( @pdfargs[:toc] ) unless @pdfargs[:toc].nil?
    meta   = parseMeta( @pdfargs[:meta] )
    reader = getPDFReader( @pdfargs[:textpdf] )

    cat = XObj.new(Hash[
      'Type'       => '/Catalog',
      'PageLayout' => "/#{@pdfargs[:pagelayout]}"
      ])
    @doc.addObject(cat)

    offsign = 'Z'
    if @now.gmt_offset > 0
      offsign = "+"
    else
      offsign = "-"
    end
    creationDate = sprintf( "D:%04d%02d%02d%02d%02d%02d%s",
      @now.year, @now.month, @now.day, @now.hour, @now.min, @now.sec, offsign )
    unless offsign.eql? 'Z'
      gmt_mins = @now.gmt_offset/60
      creationDate << sprintf( "%02d'%02d", gmt_mins/60, gmt_mins%60 )
    end
    info = XObj.new(Hash[
      'Creator'      => "(PDFBeads)",
      'Producer'     => "(PDFBeads)",
      'CreationDate' => "(#{creationDate})"
    ])
    @doc.addObject(info)
    meta.each_key do |key|
      info.addToDict(key, "(\xFE\xFF#{meta[key].to_text})")
    end

    if ( toc != nil and toc.length > 0 ) or @pdfargs[:rtl]
      vpref = XObj.new(Hash.new())
      vpref.addToDict('Direction', "/R2L") if @pdfargs[:rtl]
      @doc.addObject(vpref)
      cat.addToDict('ViewerPreferences', ref(vpref.getID))
    end

    pages = XObj.new(Hash[
      'Type' => '/Pages'
    ])
    @doc.addObject(pages)
    cat.addToDict('Pages', ref(pages.getID))

    creator = XObj.new(Hash[
      'Subtype' => '/Artwork',
      'Creator' => "(PDFBeads)",
      'Feature' => '(Layers)'
    ])
    @doc.addObject(creator)

    ocFore = XObj.new(Hash[
      'Type'   => '/OCG',
      'Name'   => '(Foreground)',
      'Usage'  => "<</CreatorInfo #{ref(creator.getID)}>>",
      'Intent' => '[/View/Design]'
    ])
    @doc.addObject(ocFore)
    ocBack = XObj.new({
      'Type'   => '/OCG',
      'Name'   => '(Background)',
      'Usage'  => "<</CreatorInfo #{ref(creator.getID)}>>",
      'Intent' => '[/View/Design]'
    })
    @doc.addObject(ocBack)
    cat.addToDict('OCProperties',
      sprintf("<< /OCGs[%s %s] /D<< /Intent /View /BaseState /ON /Order[%s %s] >>>>",
        ref(ocFore.getID), ref(ocBack.getID), ref(ocFore.getID), ref(ocBack.getID)))

    page_objs = Array.new()
    pages_by_num = Hash.new()
    symd = nil
    font = nil
    pidx = 0

    if labels != nil and labels.length > 0
      nTree = "<</Nums[\n"
      labels.each do |rng|
        nTree << "#{rng[:first]} << "
        if rng.has_key? :prefix
          begin
            # If possible, use iso8859-1 (aka PDFDocEncoding) for page labels:
            # it is at least guaranteed to be safe
            if rng[:prefix].respond_to? :encode
              ltitl = rng[:prefix].encode( "iso8859-1", "utf-8" )
            else
              ltitl = Iconv.iconv( "iso8859-1", "utf-8", rng[:prefix] ).first
            end
            nTree << "/P (#{ltitl.to_text}) "
          # Iconv::InvalidCharacter, Iconv::IllegalSequence, Encoding::UndefinedConversionError, Encoding::InvalidByteSequenceError
          rescue
            if rng[:prefix].respond_to? :encode
              ltitl = rng[:prefix].encode( "utf-16be", "utf-8" )
            else
              ltitl = Iconv.iconv( "utf-16be", "utf-8", rng[:prefix] ).first
            end
            # If there is no number (just prefix) then put a zero character after the prefix:
            # this makes acroread happy, but prevents displaying the number in evince
            unless rng.has_key? :style
              nTree << "/P (\xFE\xFF#{ltitl.to_text}\x00\x00) "
            # Otherwise put a formally correct Unicode string, which, however, may stumble acroread
            else
              nTree << "/P (\xFE\xFF#{ltitl.to_text}) "
            end
          end
        end
        nTree << "/S /#{rng[:style]} " if rng.has_key? :style
        nTree << "/St #{rng[:start]}" if rng.has_key? :start
        nTree << ">>\n"
      end

      nTree << "]\n>>"
      cat.addToDict('PageLabels', nTree)
      cur_range_id = 0
    end

    needs_font = false
    fonts = encodings = nil
    unless reader.nil?
      fdict = importPDFFonts( reader,@pdfargs[:textpdf] )
    else
      pagefiles.each do |p|
        unless p.hocr_path.nil?
          needs_font = true
          break
        end
      end

      if needs_font
        fonts = Array.new()
        encodings = [ [' '] ]
        fdict = XObj.new( Hash[] )
        @doc.addObject( fdict )

        descr = XObj.new( Hash[
          'Type'     => '/FontDescriptor',
          'BaseFont' => '/Times-Roman',
          ] )
        @fdata.header.each_key do |key|
          descr.addToDict( key,@fdata.header[key] )
        end
        @doc.addObject( descr )
      end
    end

    pagefiles.each do |p|
      procSet = ['/PDF', '/ImageB']
      c_str = ''
      doc_objs = Array.new()
      lastimg = 0

      width = p.width; height = p.height
      xres  = p.x_res; yres   = p.y_res
      pwidth  = width.to_f  / xres * 72
      pheight = height.to_f / yres * 72

      p.stencils.each do |s|
        if st_format.eql? 'JBIG2'
          xobj,width,height,xres,yres = loadJBIG2Page( s[:jbig2path],s[:jbig2dict],ref(ocFore.getID) )
        else
          xobj,width,height,xres,yres = loadCCITTPage( s[:path],ref(ocFore.getID) )
        end
        break if xobj.nil?

        color = s[:rgb].join(' ') << ' rg'
        doc_objs << xobj

        c_str << "#{color} /Im#{lastimg} Do "
        lastimg += 1
      end

      fg_image = bg_image = nil
      fg_image = loadImage( p.fg_layer,ocFore.getID,procSet ) unless p.fg_layer.nil?
      bg_image = loadImage( p.bg_layer,ocBack.getID,procSet ) unless p.bg_layer.nil?

      contents = XObj.new(Hash[
        'Filter' => '/FlateDecode'
      ])
      resobj = XObj.new(Hash.new())
      resources = XObj.new(Hash[
        'XObject' => ref(resobj.getID)
      ])

      unless fg_image.nil?
        xobj = doc_objs[0]
        fg_image.addToDict('SMask', ref(xobj.getID))
        xobj.removeFromDict('ImageMask')
        xobj.addToDict('Decode', '[1 0]')
        resobj.addToDict('Im0', ref(fg_image.getID))
        doc_objs << fg_image
        c_str = '/Im0 Do '
      else
        doc_objs.each_index do |i|
          resobj.addToDict( "Im#{i}", ref(doc_objs[i].getID) )
        end
      end

      unless bg_image.nil?
        c_str = "/Im#{resobj.dictLength} Do " << c_str
        resobj.addToDict( "Im#{resobj.dictLength}", ref(bg_image.getID) )
        doc_objs << bg_image
      end
      c_str = sprintf( "q %.2f 0 0 %.2f 0 0 cm %sQ",pwidth,pheight,c_str )

      doc_objs.concat( [contents, resobj, resources] )

      hocr = nil
      if not reader.nil?
        procSet << '/Text'
        c_str   << getPDFText( reader,pidx,@pdfargs[:debug] )
      elsif not p.hocr_path.nil?
        hocr = open( p.hocr_path ) { |f| Nokogiri::HTML( f ) }
        procSet << '/Text'
        c_str   << getHOCRText( hocr,pheight,72.0/xres,72.0/yres,encodings )
      end

      unless @pdfargs[:debug]
        contents.reinit( Hash[
          'Filter' => '/FlateDecode'
        ], Zlib::Deflate.deflate( c_str,9 ) )
      else
        contents.reinit( Hash[], c_str )
      end
      resources.addToDict( 'ProcSet', "[ #{procSet.join(' ')} ]" )
      resources.addToDict( 'Font', ref( fdict.getID ) ) unless hocr.nil? and reader.nil?

      page = XObj.new(Hash[
        'Type'      => '/Page',
        'Parent'    => "#{pages.getID} 0 R",
        'MediaBox'  => sprintf( "[ 0 0 %.02f %.02f ]",pwidth,pheight ),
        'Contents'  => ref( contents.getID ),
        'Resources' => ref( resources.getID )
      ])
      # By default acroread uses /DeviceCMYK as a transparency blending space,
      # so adding an SMask image to a page would result to colors being shifted,
      # uless we take a special care of this. For more details see
      # http://comments.gmane.org/gmane.comp.tex.pdftex/3747
      unless fg_image.nil?
        cspace = '/DeviceRGB'
        cspace = fg_image.getFromDict( 'ColorSpace' ) if fg_image.hasInDict( 'ColorSpace' )
        page.addToDict( 'Group', "<< /S /Transparency /CS #{cspace} >>" )
      end
      doc_objs  << page
      doc_objs.each{ |x| @doc.addObject(x) }
      page_objs << page

      pages.addToDict( 'Count', page_objs.length )
      pages.addToDict( 'Kids', '[' << page_objs.map{|x| ref(x.getID).to_s}.join(' ') << ']' )

      pkey = (pidx + 1).to_s
      pkey = labels.getPageLabel( cur_range_id,pidx ) if labels != nil and labels.length > 0
      pages_by_num[pkey] = page.getID
      pidx += 1
      if labels != nil and labels.length > 0
        if cur_range_id < labels.length - 1 and labels[cur_range_id + 1][:first] == pidx
          cur_range_id += 1
        end
      end

      $stderr.puts("Processed #{p.name}\n")
      $stderr.puts("  Added background image from #{p.bg_layer}\n") unless bg_image.nil?
      $stderr.puts("  Added foreground image from #{p.fg_layer}\n") unless fg_image.nil?
    end

    if needs_font
      fidx = 1
      encodings.each do |enc|
        font = addFont( descr,enc,"Fnt#{fidx}" )
        fdict.addToDict( "Fnt#{fidx}",ref(font.getID) )
        fonts << font
        fidx += 1
      end
    end

    if toc != nil and toc.length > 0
      getOutlineObjs( toc,pages_by_num,page_objs[0].getID )
      cat.addToDict('Outlines', ref(toc[0][:pdfobj].getID))
      cat.addToDict('PageMode', "/UseOutlines")
      vpref.addToDict('NonFullScreenPageMode', "/UseOutlines")
    end

    if @pdfargs[:delfiles]
      pagefiles.each do |p|
        $stderr.puts( "Cleaning up temporary files for #{p.name}" )
        safe_delete( p.fg_layer ) if p.fg_created
        safe_delete( p.bg_layer ) if p.bg_created
        p.stencils.each do |s|
          safe_delete( s[:path] ) if s[:created]
        end
      end
    end
  end

  # Output the created PDF file to the disk.
  def output( outpath )
    begin
      if outpath.eql? 'STDOUT'
        out = $stdout
      else
        out = File.open( outpath,'w' )
      end

      out.binmode if /(win|w)32$/.match( RUBY_PLATFORM )
      out.write( @doc.to_s )
      out.close unless outpath.eql? 'STDOUT'
    rescue
      $stderr.puts( "Error: could not write to #{outpath}" )
    end
  end

  private

  def safe_delete( path )
    begin
      File.delete( path )
      $stderr.puts( " Deleted #{path}" )
    rescue Exception => e
        $stderr.puts( "Could not delete #{path}: #{e.message}" )
    end
  end

  def parseMeta( path )
    ret = Hash.new()
    return ret if path.nil? or path.eql? ''

    keys = [ 'Title', 'Author', 'Subject', 'Keywords' ]
    File.open( path,'r' ) do |fin|
      fin.set_encoding 'UTF-8' if fin.respond_to? :set_encoding
      fin.each do |fl|
        next if /^\#/.match( fl )

        if /^\/?([A-Za-z]+)[ \t]*:[ \t]+\"(.*)\"/.match( fl )
          key = $1
          if keys.include? key
            begin
              tmp_str = ''
              if $2.respond_to? :encode
                tmp_str = $2.encode( "utf-16be", "utf-8" )
              else
                tmp_str = Iconv.iconv( "utf-16be", "utf-8", $2 ).first
              end
              # a parenthesis code in a formally correct utf-16 should nevertheless be escaped
              ret[key] = tmp_str.to_binary
              ret[key].gsub!( /\x5C/,"\x5C\x5C" )
              ret[key].gsub!( /\x28/,"\x5C\x28" )
              ret[key].gsub!( /\x29/,"\x5C\x29" )
            rescue
              $stderr.puts("Error: metadata should be specified in utf-8")
            end
          end
        end
      end
    end
    ret
  end

  def getPDFReader( path )
    return nil if path.nil? or path.eql? ''
    return nil unless File.file? path

    PDF::Reader.new( path )
  end

  def encodePDFArray( in_a )
    out_a = Array.new()
    out_a << '['
    in_a.each do |item|
      if item.is_a? String
        out_a << ( '(' << item.to_s << ')' )
      elsif item.is_a? Symbol
        out_a << ( '/' << item.to_s )
      elsif item.is_a? Array
        out_a << encodePDFArray( item )
      else
        out_a << item.to_s
      end
    end
    out_a << ']'
    out_a.join( ' ' )
  end

  def encodePDFObjEntry( inhash,outobj,label )
    if inhash[label].is_a? String
      outobj.addToDict( label,"(#{inhash[label]})" )

    elsif inhash[label].is_a? Symbol
      outobj.addToDict( label,"/#{inhash[label]}" )

    elsif inhash[label].is_a? Integer
      outobj.addToDict( label,"#{inhash[label]}" )

    elsif inhash[label].is_a? Array
      outobj.addToDict( label,encodePDFArray( inhash[label] ) )

    elsif inhash[label].is_a? Hash
      newobj = XObj.new( Hash.new() )
      @doc.addObject( newobj )
      outobj.addToDict( label,ref(newobj.getID) )
      inhash[label].keys.each do |newlabel|
        encodePDFObjEntry( inhash[label],newobj,newlabel )
      end

    elsif inhash[label].is_a? PDF::Reader::Stream
      newobj = XObj.new( Hash.new(),inhash[label].data )
      @doc.addObject( newobj )
      outobj.addToDict( label,ref(newobj.getID) )
      inhash[label].hash.keys.each do |newlabel|
        encodePDFObjEntry( inhash[label].hash,newobj,newlabel ) unless newlabel.eql? :Length
      end
    end
  end

  def importPDFFont( label,font )
    fontobj = XObj.new( Hash.new() )
    fontobj.addToDict( 'Name',"/#{label}" ) unless label.nil?
    @doc.addObject( fontobj )

    if font.has_key? :DescendantFonts
      dfonts = Array.new()
      font[:DescendantFonts].each {|dfont| dfonts << importPDFFont( nil,dfont ) }
      fontobj.addToDict( "DescendantFonts",'[ ' << dfonts.map{|dfont| ref(dfont.getID)}.join(' ') << ' ]' )
    end

    [ :BaseFont, :Type, :Subtype, :FirstChar, :LastChar, :Widths, :FontDescriptor,
      :Encoding, :ToUnicode, :DW, :W, :CIDSystemInfo, :CIDToGIDMap ].each do |fontkey|
      encodePDFObjEntry( font,fontobj,fontkey ) if font.has_key? fontkey
    end
    fontobj
  end

  def importPDFFonts( reader,path )
    fonts = Hash.new()
    reader.pages.each_index do |i|
      $stderr.puts("Reading font data from #{path}: page #{i}\n")
      page = reader.pages[i]
      page.fonts.each do |label,font|
        fonts[label] = page.objects.deref( font ) unless fonts.has_key? label
      end
    end

    fdict = XObj.new( Hash[] )
    @doc.addObject( fdict )
    fonts.keys.sort_by {|sym| sym.to_s}.each do |label|
      fontobj = importPDFFont( label,fonts[label] )
      fdict.addToDict( label,ref(fontobj.getID) )
    end
    fdict
  end

  def getPDFText( reader,pidx,debug )
    return "" unless reader.pages.length > pidx

    page = reader.pages[pidx]
    pcont = page.raw_content.to_binary()
    cidx = 0
    in_t = false
    pstack = 0
    prevc = "\0"
    ch_start = -1
    ret = ""
    tr_val = debug ? 0 : 3

    pcont.each_byte do |char|
      if char.chr.eql? '('
        ctx = pcont[0,cidx].match( /\\+$/ )
        pstack += 1 if ( ctx.nil? or ctx[0].length % 2 == 0 )
      elsif char.chr.eql? ')'
        ctx = pcont[0,cidx].match( /\\+$/ )
        pstack -= 1 if ( ctx.nil? or ctx[0].length % 2 == 0 )
      end

      unless pstack > 0
        # Text state operators may occur outside text objects. We have to take care of this
        if not in_t and prevc.eql? 'T'
          case char.chr
            when 'c'
              if pcont[0,cidx-1] =~ /([-+]?\d*\.?\d+)\s+$/
                ret << " #{$1} Tc"
              end
            when 'w'
              if pcont[0,cidx-1] =~ /([-+]?\d*\.?\d+)\s+$/
                ret << " #{$1} Tw"
              end
            when 'z'
              if pcont[0,cidx-1] =~ /([-+]?\d*\.?\d+)\s+$/
                ret << " #{$1} Tz"
              end
            when 'L'
              if pcont[0,cidx-1] =~ /([-+]?\d*\.?\d+)\s+$/
                ret << " #{$1} TL"
              end
            when 'f'
              if pcont[0,cidx-1] =~ /\/([A-Za-z0-9]+)\s+([-+]?\d*\.?\d+)\s+$/
                ret << " /#{$1} #{$2} Tf"
              end
            # Tr operators are ignored, since we always need either a hidden text (3 Tr)
            # or (for debugging purposes) a visible text without special effects (0 Tr)
            when 's'
              if pcont[0,cidx-1] =~ /([-+]?\d*\.?\d+)\s+$/
                chunks << " #{$1} Ts"
              end
          end
        elsif not in_t and ( prevc + char.chr ).eql? 'BT'
          ch_start = cidx -1
          in_t = true
        elsif in_t and ( prevc + char.chr ).eql? 'ET'
          chunk = pcont.slice( ch_start,cidx - ch_start + 1 )
          chunk.gsub!( /\d{1}\s+Tr/,"#{tr_val} Tr" )
          ret << "\n" << chunk
          ch_start = -1
          in_t = false
        end
      end

      prevc = char.chr
      cidx += 1
    end
    return "\nq #{tr_val} Tr" << ret << " Q" if ret.length > 0
    return ""
  end

  def getOutlineObjs( toc,page_ids,fp_id )
    root = toc[0]
    root[:pdfobj] = XObj.new( Hash[
      'Type'  => '/Outlines',
      'Count' => root.getChildrenCount
    ])
    @doc.addObject(root[:pdfobj])

    toc[1..-1].each do |item|
      dest = fp_id
      if page_ids.has_key? item[:ref]
        dest = page_ids[item[:ref]]
      else
        dest = nil
        $stderr.puts("Malformed TOC: there is no page #{item[:ref]} in this document.")
      end

      item_text = item[:title].to_binary
      item_text.gsub!( /\x5C/,"\x5C\x5C" )
      item_text.gsub!( /\x28/,"\x5C\x28" )
      item_text.gsub!( /\x29/,"\x5C\x29" )
      item[:pdfobj] = XObj.new(Hash[
        'Title'  => "(\xFE\xFF#{item_text.to_text})",
        'Parent' => ref(item[:parent][:pdfobj].getID),
      ])
      if dest != nil
        item[:pdfobj].addToDict('Dest', "[ #{dest} 0 R /XYZ null null null ]")
      else
        item[:pdfobj].addToDict('C', "[0.75 0.75 0.75]")
      end


      if item[:children].length > 0
        cnt = item.getChildrenCount
        if item[:open]
          item[:pdfobj].addToDict('Count', cnt)
        else
          item[:pdfobj].addToDict('Count', -cnt)
        end
      end

      unless item.has_key? :prev
        item[:parent][:pdfobj].addToDict('First', ref(item[:pdfobj].getID))
      else
        item[:prev][:pdfobj].addToDict('Next', ref(item[:pdfobj].getID))
        item[:pdfobj].addToDict('Prev', ref(item[:prev][:pdfobj].getID))
      end

      unless item.has_key? :next
        item[:parent][:pdfobj].addToDict('Last', ref(item[:pdfobj].getID))
      end

      @doc.addObject(item[:pdfobj])
    end
  end

  # Returns an array containing the coordinates of the bounding box around
  # an element
  def elementCoordinates( element,xscale,yscale )
    out = [0,0,0,0]

    if element.attributes.has_key? 'title'
      if /bbox((\s+\d+){4})/.match(element.attributes['title'].content)
        coords = $1.strip.split(/\s+/)
        out = [ (coords[0].to_i*xscale).to_f,(coords[1].to_i*xscale).to_f,
                (coords[2].to_i*yscale).to_f,(coords[3].to_i*yscale).to_f ]
      end
    end
    return out
  end

  def elementText( elem )
    # used to put some Iconv stuff here, but nokogiri performs this conversion itself
    return elem.inner_text.strip
  end

  def getOCRUnits( ocr_line,lbbox,fsize,xscale,yscale )
    units = Array.new()
    ocr_words = ocr_line.xpath(".//span[@class='ocrx_word']")
    ocr_chars = nil
    ocr_chars = ocr_line.at_xpath(".//span[@class='ocr_cinfo']") if ocr_words.length == 0

    # If 'ocrx_word' elements are available (as in Tesseract owtput), split the line
    # into individual words
    if ocr_words.length > 0
      ocr_words.each do |word|
        bbox = elementCoordinates( word,xscale,yscale )
        next if bbox == [0,0,0,0]
        txt = elementText( word )
        units << [txt,bbox]
      end

    # If 'ocrx_cinfo' data is available (as in Cuneiform) owtput, then split it
    # into individual characters and then combine them into words
    elsif not ocr_chars.nil? and ocr_chars.attributes.has_key? 'title'
      if /x_bboxes([-\s\d]+)/.match( ocr_chars.attributes['title'].content )
        coords = $1.strip.split(/\s+/)
        ltxt = elementText( ocr_line )
        charcnt = 0
        ltxt.each_char { |uc| charcnt += 1 }

        if charcnt <= coords.length/4
          i = 0
          wtxt = ''
          bbox = [-1,-1,-1,-1]
          ltxt.each_char do |uc|
            cbbox = [ (coords[i*4].to_i*xscale).to_f,(coords[i*4+1].to_i*xscale).to_f,
                      (coords[i*4+2].to_i*yscale).to_f,(coords[i*4+3].to_i*yscale).to_f ]

            unless cbbox[0] < 0
              bbox[0] = cbbox[0] if cbbox[0] < bbox[0] or bbox[0] < 0
              bbox[1] = cbbox[1] if cbbox[1] < bbox[1] or bbox[1] < 0
              bbox[2] = cbbox[2] if cbbox[2] > bbox[2] or bbox[2] < 0
              bbox[3] = cbbox[3] if cbbox[3] > bbox[3] or bbox[3] < 0
              wtxt << uc

            else
              units << [wtxt,bbox]
              bbox = [-1,-1,-1,-1]
              if /^\s+$/.match( uc )
                wtxt = ''

              # A workaround for probable hpricot bug (TODO: is Nokogiri affected?),
              # which sometimes causes whitespace characters from inside a string
              # to be stripped. So if we find a bounding box with negative values
              # we assume there was a whitespace character here, even if not
              # preserved in the string itself
              else
                wtxt = uc
                i += 1
                bbox =  [ (coords[i*4].to_i*xscale).to_f,(coords[i*4+1].to_i*xscale).to_f,
                          (coords[i*4+2].to_i*yscale).to_f,(coords[i*4+3].to_i*yscale).to_f ]
              end
            end
            i += 1
          end
          units << [wtxt,bbox] unless wtxt.eql? ''
        end
      end
    end

    # If neither word nor character bounding boxes are available, then store the line as a whole
    if units.length == 0
      ltxt = elementText( ocr_line )
      units << [ltxt,lbbox] unless ltxt.eql? ''
    end

    units[units.length-1][0].sub!( /-\Z/, "\xC2\xAD" ) unless units.length == 0
    return units
  end

  def getHOCRText( hocr,pheight,xscale,yscale,encodings )
    fsize = 10
    cur_enc = nil
    ret = " BT 3 Tr "

    hocr.xpath("//span[@class='ocr_line']").each do |line|
      lbbox = elementCoordinates( line,xscale,yscale )
      next if lbbox[2] - lbbox[0] <= 0 or lbbox[3] - lbbox[1] <= 0
      units = getOCRUnits( line,lbbox,fsize,xscale,yscale )
      next if units.length == 0

      wwidth = 0
      ltxt = ''
      units.each do |unit|
        ltxt << unit[0]
        wwidth += ( unit[1][2] - unit[1][0] )
      end
      lw = @fdata.getLineWidth( ltxt,fsize )
      ratio = 1
      ratio = wwidth / lw unless lw == 0
      pos = lbbox[0]
      posdiff = 0

      ret << sprintf( "%f %f %f %f %f %f Tm ",
        ratio, 0, 0, ratio, lbbox[0], pheight - lbbox[3] - @fdata.header['Descent'] * fsize / 1000.0 * ratio)
      in_txt = false

      units.each_index do |i|
        unit = units[i]
        wtxt = unit[0]
        bbox = unit[1]

        posdiff = ( (pos - bbox[0]) * 1000 / fsize / ratio ).to_i if i > 0
        pos = bbox[0] + ( @fdata.getLineWidth( wtxt,fsize ) * ratio )

        txt8 = ''
        wtxt.each_char do |char|
          begin
            if char.respond_to? :encode
              char.encode!( "utf-16be", "utf-8" )
            else
              Iconv.iconv( "utf-16be","utf-8",char )
            end
          rescue
            rawbytes = char.unpack( 'C*' )
            bs = ''
            rawbytes.each{ |b| bs << sprintf( "%02x",b ) }
            $stderr.puts( "Warning: an invalid UTF-8 sequence (#{bs}) in the hOCR data." )
            char = '?' * rawbytes.length
          end

          encoded = false
          if cur_enc.nil? or not cur_enc.include? char
            encodings.each_index do |i|
              enc = encodings[i]
              next if enc == cur_enc

              if enc.include? char
                if in_txt
                  ret << "#{posdiff} " if posdiff != 0
                  ret << "<#{txt8}> " unless txt8.eql? ''
                  ret << "] TJ "
                end
                cur_enc = enc
                ret << "/Fnt#{i + 1} #{fsize} Tf "
                txt8 = ''
                posdiff = 0
                encoded = true
                in_txt = false
                break
              end
            end

            unless encoded
              last = encodings[-1]
              if last.length < 256
                last << char
              else
                last = [ ' ',char ]
                encodings << last
              end

              if cur_enc != last
                if in_txt
                  ret << "#{posdiff} " if posdiff != 0
                  ret << "<#{txt8}> " unless txt8.eql? ''
                  ret << "] TJ "
                end
                cur_enc = last
                ret << "/Fnt#{encodings.length} #{fsize} Tf "
                txt8 = ''
                posdiff = 0
                in_txt = false
              end
            end
          end

          unless in_txt
            ret << "[ "
            in_txt = true
          end
          txt8 << sprintf( "%02X",cur_enc.index(char) )
        end

        unless txt8.eql? ''
          ret << "#{posdiff} " if posdiff != 0
          ret << "<#{txt8}> "
        end
      end
      if in_txt
        ret << "] TJ "
        in_txt = false
      end
    end

    ret << "ET "
    return ret
  end

  def addFont( descr,fenc,fname )
    enc_str = @fdata.getEncoding( fenc ).join( ' ' )
    enc  = XObj.new( Hash[
      'Type'        => "/Encoding",
      'Differences' => "[ 0 #{enc_str} ]"
    ])
    @doc.addObject( enc )

    toUni = @fdata.getCMAP( fenc )
    @doc.addObject( toUni )

    font = XObj.new( Hash[
      'BaseFont'       => '/Times-Roman',
      'Name'           => "/#{fname}",
      'Subtype'        => '/Type1',
      'Type'           => '/Font',
      'FirstChar'      => 0,
      'LastChar'       => fenc.length - 1,
      'Widths'         => '[ ' << @fdata.getWidths(fenc).map{|w| w.to_s}.join(' ') << ' ]',
      'FontDescriptor' => ref(descr.getID),
      'ToUnicode'      => ref(toUni.getID),
    ] )
    if enc.nil?
      font.addToDict( 'Encoding','/WinAnsiEncoding' )
    else
      font.addToDict( 'Encoding',ref(enc.getID) )
    end
    @doc.addObject( font )
    return font
  end

  def loadCCITTPage( path,ocref )
    stencil = ImageInspector.new( path )
    return nil if stencil.width.nil?

    width = stencil.width
    height = rows_per_strip = stencil.height
    xres = stencil.x_dpi
    yres = stencil.y_dpi
    rows_per_strip = stencil.tags[0x116][0] if
      stencil.format.eql? :TIFF and stencil.tags.has_key? 0x116

    unless stencil.compression.eql? :CCITTFaxDecode and rows_per_strip >= height
      img = ImageList.new( path )
      imgdata = img.to_blob { |imd|
        imd.format = 'TIFF'
        imd.define( 'TIFF','rows-per-strip',height )
        imd.compression = Group4Compression
      }
      stencil = ImageInspector.new( StringIO.new(imgdata) )
      img.destroy!
    end
    body = stencil.getRawData
    photometric = 0
    photometric = stencil.tags[0x106][0] if
      stencil.format.eql? :TIFF and stencil.tags.has_key? 0x106

    xobj = XObj.new(Hash[
      'Type'             => '/XObject',
      'Subtype'          => '/Image',
      'OC'               => ocref,
      'Width'            => width.to_s,
      'Height'           => height.to_s,
      'ImageMask'        => 'true',
      'ColorSpace'       => '/DeviceGray',
      'BitsPerComponent' => '1',
      'Filter'           => '/CCITTFaxDecode',
      'DecodeParms'      => "<< /Columns #{width} /K -1 >>",
    ], body)
    if photometric == 1 then
      # As ImageMask is always on, BlackIs1 actually doesn't work, while
      # the Decode array does.
      xobj.addToDict( 'BlackIs1', 'true' )
      xobj.addToDict( 'Decode', '[1 0]' )
    end

    return [ xobj,width,height,xres,yres ]
  end

  def loadJBIG2Page( path,dictpath,ocref )
    begin
      jbig2  = File.open( path,'rb' ).read
      width, height, xres, yres = jbig2[11...27].unpack( 'NNNN' )
      unless @dictpath.eql? dictpath
        symd_f = File.open( dictpath,'rb' ).read
        symd_o = @doc.addObject( XObj.new(Hash.new(),symd_f) )
        @dictpath = dictpath
        @dictobj  = symd_o
      end
    rescue
      $stderr.puts( "Page not completed: could not access #{path}" )
      return nil
    end

    xobj = XObj.new(Hash[
      'Type'             => '/XObject',
      'Subtype'          => '/Image',
      'OC'               => ocref,
      'Width'            => width.to_s,
      'Height'           => height.to_s,
      'ImageMask'        => 'true',
      'ColorSpace'       => '/DeviceGray',
      'BitsPerComponent' => '1',
      'Filter'           => '/JBIG2Decode',
      'DecodeParms'      => "<< /JBIG2Globals #{@dictobj.getID} 0 R >>"
    ], jbig2)

    return [ xobj,width,height,xres,yres ]
  end

  def loadImage( impath,ocID,procSet )
    insp = ImageInspector.new( impath )
    return nil if insp.width.nil?

    # JPEG, JPEG2000 and PNG images can be handled directly. We also can
    # handle uncompressed TIFF files, although it is very unlikely someone
    # would use them for page background. Unfortunately things are more
    # difficult for compressed TIFF images, as they normally contain several
    # compressed chunks, so that we can't just concatenate them. So for all
    # other image types we just call ImageMagick to convert them into a
    # zip-compressed PNG, and then retrieve the raw data from that PNG image.
    unless [ :JPEG, :JPEG2000, :PNG ].include? insp.format or
      ( insp.format.eql? :TIFF and ( insp.compression.eql? :NoCompression or
      ( [ :FlateDecode,:LZWDecode,:CCITTFaxDecode ].include? insp.compression and insp.tags[0x0116][0] >= insp.height )))

      img = ImageList.new( impath )
      imgdata = img.to_blob { |imd|
        imd.format = 'PNG'
        imd.quality = 95
        imd.compression = ZipCompression
      }
      insp = ImageInspector.new( StringIO.new(imgdata) )
      img.destroy!
    end
    rawdata  = insp.getRawData
    cspace   = "/#{insp.cspace}"
    fmt      = insp.format
    imgcompr = insp.compression
    per_comp = 1

    if cspace.eql? '/Indexed' and not insp.palette.nil?
      cspace = '/DeviceGray'; cpal = insp.palette
      rgb = false
      cpal.each do |c|
        if c[0] != c[1] or c[0] != c[2]
          cspace = '/DeviceRGB'
          rgb = true
          break
        end
      end

      cspace = "[/Indexed #{cspace} #{cpal.length - 1} < "
      cpal.each do |c|
        cspace << sprintf( "%02x ",c[0] )
        cspace << sprintf( "%02x %02x ",c[1],c[2] ) if rgb
      end
      cspace << '>]'

      procSet << '/ImageI' unless procSet.include? '/ImageI'

    elsif not cspace.eql? '/DeviceGray' and not procSet.include? '/ImageC'
      procSet << '/ImageC'
    end

    if cspace.eql? '/DeviceRGB'
      per_comp = 3
    elsif cspace.eql? '/DeviceCMYK'
      per_comp = 4
    end
    image = XObj.new( Hash[
      'Type'              => '/XObject',
      'Subtype'           => '/Image',
      'OC'                => ref( ocID ),
      'Width'             => insp.width,
      'Height'            => insp.height,
      'Interpolate'       => 'true'
    ], rawdata )

    unless fmt.eql? :JPEG2000
      image.addToDict( 'BitsPerComponent',insp.depth )
      image.addToDict( 'ColorSpace',"#{cspace}" )
    end
    image.addToDict( 'Filter',"/#{imgcompr}" ) unless insp.compression.eql? :NoCompression
    if [:PNG, :TIFF].include? fmt
      predictor = (fmt.eql? :PNG) ? 15 : 2
      image.addToDict( 'DecodeParms',
        "<< /Predictor #{predictor} /Colors #{per_comp} /BitsPerComponent #{insp.depth} /Columns #{insp.width} >>" )
    end
    return image
  end

  def ref(x)
    return "#{x} 0 R"
  end
end

