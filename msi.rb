require 'rubygems'
require 'gtk2'

class MSI < Gtk::Window

  FILE_IMAGE_TRAIN = 'train-images.idx3-ubyte'
  FILE_LABEL_TRAIN = 'train-labels.idx1-ubyte'
  FILE_IMAGE_TEST = 't10k-images.idx3-ubyte'
  FILE_LABEL_TEST = 't10k-labels.idx1-ubyte'
  
  def initialize
    super

    #@image_train = loadIDX(FILE_IMAGE_TRAIN)
    #@label_train = loadIDX(FILE_LABEL_TRAIN)
    @image_test = loadIDX(FILE_IMAGE_TEST)
    @label_test = loadIDX(FILE_LABEL_TEST)
    @current = 0

    @pixbuf = Gdk::Pixbuf.new(Gdk::Pixbuf::COLORSPACE_RGB, false, 8, 28, 28)
    @image = Gtk::Image.new @pixbuf.scale(280,280,Gdk::Pixbuf::INTERP_NEAREST)
    add @image
    setImage 
    
    signal_connect "destroy" do
      Gtk.main_quit
    end
    signal_connect("key-press-event") do |w, e|
      #p "#{e.keyval}, Gdk::Keyval::GDK_#{Gdk::Keyval.to_name(e.keyval)}"
      case Gdk::Keyval.to_name(e.keyval)
      when 'Escape' then Gtk.main_quit
      when 'Right' then @current += 1; @current = 0 if @current == @image_test.size; setImage;
      when 'Left' then @current -= 1; @current = @image_test.size-1 if @current < 0; setImage;
      end
    end
          
    set_window_position Gtk::Window::POS_CENTER
    show_all
  end
  
  def setImage
    str = @pixbuf.pixels
    str.length.times do |i| str[i] = @image_test[@current][i/3] end
    @pixbuf.set_pixels(str)
    @image.pixbuf = @pixbuf.scale(280,280,Gdk::Pixbuf::INTERP_NEAREST)
    set_title("MSI, img ##{@current} => #{@label_test[@current]}")
  end
  
  def loadIDX(filename)
    File.open(filename, "rb") do |file|
      buf = file.read(3)
      # check for magic number (and data type)
      raise 'ERROR: Invalid file (not IDX Vector File)' unless buf == "\000\000\010"
      # get number of dimensions
      dim = file.getbyte
      # check for dimension count
      raise 'ERROR: Unexpected number of dimensions' unless dim > 0
      ary = Array.new(msb2int(file.read(4)))
      if dim == 1
        # load label data
        ary.size.times do |i|
          ary[i] = file.getbyte
        end
      elsif dim == 3
        w = msb2int(file.read(4))
        h = msb2int(file.read(4))
        # load image data
        ary.size.times do |i|
          ary[i] = file.read(w*h)
        end
      else
        raise 'ERROR: Unexpected number of dimensions'
      end
      # check for expected end of file
      raise 'ERROR: Excessive data in file' unless file.getbyte.nil?
      return ary
    end
  end
  
  def msb2int(arg)
    ret = 0
    arg.length.times do |i|
      ret = ret * 256 + arg[i]
    end
    return ret
  end
  
end

Gtk.init
  window = MSI.new
Gtk.main
