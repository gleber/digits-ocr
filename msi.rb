require 'rubygems'
require 'gtk2'

require 'opencv'
include OpenCV

class MSI < Gtk::Window

  FILE_IMAGE_TRAIN = 'train-images.idx3-ubyte'
  FILE_LABEL_TRAIN = 'train-labels.idx1-ubyte'
  FILE_IMAGE_TEST = 't10k-images-idx3-ubyte'
  FILE_LABEL_TEST = 't10k-labels-idx1-ubyte'

  def initialize
    super

    #@image_train = loadIDX(FILE_IMAGE_TRAIN)
    #@label_train = loadIDX(FILE_LABEL_TRAIN)
    @image_test = loadIDX(FILE_IMAGE_TEST)
    @label_test = loadIDX(FILE_LABEL_TEST)
    @current = 0
    @edge = false

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
      when 'F1' then @edge = !@edge; setImage;
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
    @pixbuf.pixels = str
    @image.pixbuf = @pixbuf.scale(280,280,Gdk::Pixbuf::INTERP_NEAREST)
    set_title("MSI, img ##{@current} => #{@label_test[@current]}")
    if @edge
      edgifyImage
    end
  end

  def loadIDX(filename)
    x = 0
    File.open(filename, "rb") do |file|
      buf = file.read(3)
      # check for magic number (and data type)
      raise 'ERROR: Invalid file (not IDX Vector File)' unless buf == "\000\000\010"
      # get number of dimensions
      dim = file.getbyte
      # check for dimension count
      raise 'ERROR: Unexpected number of dimensions' unless dim > 0
      count = file.read(4).unpack('N')[0]
      ary = Array.new(count)
      if dim == 1
        # load label data
        ary.size.times do |i|
          ary[i] = file.getbyte
        end
      elsif dim == 3
        w = file.read(4).unpack('N')[0]
        h = file.read(4).unpack('N')[0]
        ary.size.times do |i|
          ary[i] = file.read(w*h)
        end
      else
        raise 'ERROR: Unexpected number of dimensions'
      end
      # check for expected end of file
      raise 'ERROR: Excessive data in file' unless file.getbyte().nil?
      return ary
    end
  end

  def edgifyImage
    mat = pixbufToCv(@pixbuf)
    mat2 = mat.canny(50, 150)
    @image.pixbuf = cvToPixbuf(mat2).scale(280, 280, Gdk::Pixbuf::INTERP_NEAREST)
  end

  def pixbufToCv(pixbuf)
    stride = pixbuf.rowstride
    w = pixbuf.height
    h = pixbuf.width
    image = CvMat.new(w, h, :cv8u, 1).clear!
    for y in 0..(h-1)
      for x in 0..(w-1)
        i = x * 3 + y * stride
        # puts i, pixbuf.pixels[i].getbyte(0)
        # image[x, y] = @image_test[@current][i].getbyte(0)
        v = pixbuf.pixels.getbyte(i)
        printf('%3d ', v)
        image[x, y] = v
      end
      puts
    end
    return image
  end

  def cvToPixbuf(image)
    pixbuf = Gdk::Pixbuf.new(Gdk::Pixbuf::COLORSPACE_RGB, false, 8, 28, 28)
    stride = pixbuf.rowstride
    w = pixbuf.height
    h = pixbuf.width
    puts w, h, stride
    puts pixbuf.pixels.length
    str = pixbuf.pixels
    for y in 0..(h-1)
      for x in 0..(w-1)
        v = image[x, y][0].truncate
        printf('%3d ', v)

        # printf("%d %d : %d\n", x, y, image[x, y][0].truncate)
        i = x * 3 + y * stride
        str.setbyte(i+0, v)
        str.setbyte(i+1, v)
        str.setbyte(i+2, v)
      end
      puts
    end
    pixbuf.pixels = str
    return pixbuf
  end

end

Gtk.init
  window = MSI.new
Gtk.main
