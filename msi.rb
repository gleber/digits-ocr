require 'rubygems'
require 'gtk2'

require 'opencv'
require 'pp'

include OpenCV
include Math

$LOAD_PATH << './'

require 'hungarian'
require 'kmeans'

def max(a, b)
  a > b ? a : b
end

def min(a, b)
  a < b ? a : b
end

class ImageSample
  def initialize(pixels, label)
    stride = 28
    w = 28
    h = 28
    @mat = CvMat.new(w, h, :cv8u, 1).clear!
    for y in 0...h
      for x in 0...w
        i = x + y * stride
        v = pixels[i].getbyte(0)
        @mat[x, y] = v
      end
    end
    @edge = @mat.canny(50, 150)
    @medoid = nil
  end

  def medoid
    @medoid
  end

  def medoid=(val)
    @medoid = val
  end

  def estimator
    @estimator
  end

  def create_estimator
    @estimator = OCR.shapeEstimator(@edge)
  end

  def compare(other)
    r = OCR.matchEstimators(@estimator, other.estimator)
    # pp r
    r.map{|x| x[2][0]}.reduce(:+)
  end
end

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
    @current = 3
    @edge = true

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
      when 'l' then learn;
      when 'Right' then @current += 1; @current = 0 if @current == @image_test.size; setImage;
      when 'Left' then @current -= 1; @current = @image_test.size-1 if @current < 0; setImage;
      end
    end

    set_window_position Gtk::Window::POS_CENTER
    show_all
  end


  #=========================================================================
  #
  # UI functions
  #
  #=========================================================================

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


  #=========================================================================
  #
  # GDK drawing functions
  #
  #=========================================================================

  def drawPoint(pixbuf, x, y, r, g=0, b=0)
    stride = pixbuf.rowstride
    i = x * 3 + y * stride
    str = pixbuf.pixels
    str.setbyte(i+0, r)
    str.setbyte(i+1, g)
    str.setbyte(i+2, b)
    pixbuf.pixels = str
  end

  def drawRect(pixbuf, x, y, w, h, r, g=0, b=0)
    stride = pixbuf.rowstride
    str = pixbuf.pixels
    for xx in 0...w
      for yy in 0...h
        i = (x + xx) * 3 + (y + yy) * stride
        str.setbyte(i+0, r)
        str.setbyte(i+1, g)
        str.setbyte(i+2, b)
      end
    end
    pixbuf.pixels = str
  end

  def edgifyImage
    mat = OCR.pixbufToCv(@pixbuf)
    mat2 = mat.canny(50, 150)
    @image.pixbuf = OCR.cvToPixbuf(mat2).scale(280, 280, Gdk::Pixbuf::INTERP_NEAREST)
    # imageSimilarity(mat, mat)
  end

  #=========================================================================
  #
  # Glossary
  #
  #=========================================================================
  #
  # Image - grayscale matrich
  # Edges - binary matrix with pixels on edges set to 1
  # Contour - set of points on the edges
  # Shape context - histogram of relative positions of other points in
  #                 contour
  # Shape estimator - hash of { point => shape context }
  #

  #=========================================================================
  #
  # Learning
  #
  #=========================================================================

  def learn
    max = 100 # @label_test.length
    images = {}
    for i in 0..9
      images[i] = []
    end
    puts max
    for i in 0...max
      label = @label_test[i]
      if label != 1
        next
      end
      printf("%d / %d - %d\n", i, max, label)
      images[label] << createImageSample(i)
    end
    for i in images.keys.sort
      printf("%d => %d\n", i, images[i].length)
    end
    @prototypes = selectPrototypes(images)
    #pp @prototypes
  end

  def selectPrototypes(images)
    proto = {}
    for i in [1]
      printf("Clustering %d - %d elements...", i, images[i].length)
      km = KMeans.new(images[i], 14)
      protos = km.cluster.map {|p| images[i][p]}
      proto[i] = protos
      printf(" done\n")
    end
    proto
  end

  def createImageSample(i)
    is = ImageSample.new(@image_test[i], @label_test[i])
    is.create_estimator
    is
  end

end

module OCR
  #=========================================================================
  #
  # Image/shape similarity functions
  #
  #=========================================================================

  def self.matchEstimators(ca, cb)
    if ca.length != cb.length
      raise "Estimators of different length"
    end
    ml = min(ca.length, cb.length)
    caa = ca.to_a
    cba = cb.to_a
    distances = CvMat.new(ml, ml, :cv32f, 1).clear! # angle x log(distance)
    for i in 0...ml
      for j in i...ml
        d = chisquare(caa[i][1], cba[j][1])
        distances[i, j] = CvScalar.new(d)
        distances[j, i] = CvScalar.new(d)
      end
    end
    # pp cvmat_to_matrix(distances)
    # pp cvmat_to_matrix(distances.diag)

    h = Hungarian.new
    solution = h.solve(cvmat_to_matrix(distances))
    return solution.map{|p|
      a = caa[p[0]]
      b = cba[p[1]]
      [a[0], b[0], distances[p[0], p[1]]]
    }
  end

  def self.imageSimilarity(ia, ib)
    sea = imageShapeEstimator(ia)
    seb = imageShapeEstimator(ib)
    shapeSimilarity(sea, seb)
  end

  def self.shapeSimilarity(ca, cb)
    matched = matchEstimators(ca, cb)
    return matched.map{|match|
      match[2][1]
    }.reduce(:+)
  end

  def self.imageShapeEstimator(image)
    mat2 = image.canny(50, 150)
    shapeEstimator(mat2)
  end

  def self.shapeEstimator(mat)
    contour = pruneContour(getContour(mat), 30)
    estimator = {}

    # for p in contour
    #   drawRect(@image.pixbuf, p[0] * 10, p[1] * 10, 10, 10, 255)
    # end

    for p in contour
      estimator[p] = getShapeContext(p, contour)
    end
    return estimator
  end

  #=========================================================================
  #
  # Shape context functions
  #
  #=========================================================================

  def self.chisquare(a, b)
    w = a.width
    h = a.height
    s = 0
    for row in 0...h
      for column in 0...w
        aa = a[row,column][0]
        bb = b[row,column][0]
        s += aa+bb > 0 ? ((aa-bb)**2)/(aa+bb) : 0
      end
    end
    return s
  end

  def self.getShapeContext(point, contour)
    sc = CvMat.new(12, 5, :cv32f, 1) # angle x log(distance)
    sc.clear!
    max_dist = 0
    for p in contour
      d = distance(point, p)
      max_dist = max(d, max_dist)
    end
    log_max_dist = log(max_dist)
    # printf "max distance %f, log %f\n", max_dist, log_max_dist

    for p in contour
      if p == point
        next
      end
      d = distance(point, p)
      a = angle(point, p)
      # printf "distance %f, log %f, angle %f\n", d, log(d), a
      dbin = min(((Math.log(d) / (log_max_dist)) * 5).truncate, 4)
      abin = min((((a + (Math::PI/2)) / Math::PI) * 12).truncate, 11)
      # printf "abin %d dbin %d\n", abin, dbin
      sc[abin, dbin] -= (-1)
    end
    sc /= sc.sum
    return sc
  end

  #=========================================================================
  #
  # geometry functions
  #
  #=========================================================================

  def self.distance(p1, p2)
    return Math.hypot((p1[0] - p2[0]).abs, (p1[1] - p2[1]).abs)
  end

  def self.angle(p1, p2)
    dx = p1[0] - p2[0]
    dy = p1[1] - p2[1]
    if dx == 0 and dy == 0
      return 0
    end
    return atan(dy.to_f / dx.to_f)
  end

  #=========================================================================
  #
  # contour functions
  #
  #=========================================================================

  def self.getContour(mat)
    contour = []
    w = mat.width
    h = mat.height
    for y in 0...h
      for x in 0...w
        if mat[x, y][0] > 0
          contour.push([x,y])
        end
      end
    end
    # printf("%d edge points\n", contour.length)
    return contour
  end


  def self.pruneContour(contour, n)
    # distances = {}
    # for i in contour
    #   ds = 0
    #   for j in contour
    #     d = distance(i, j)
    #     ds += d < 5 ? d : 0
    #   end
    #   distances[i] = ds
    # end

    while contour.length > n
      min = contour[0]
      min_dist = 1000

      for i in contour
        for j in contour
          break if i == j
          d = distance(i, j)
          if distance(i, j) < min_dist
            min = i
            min_dist = d
          end
        end
      end
      contour.delete(min)
    end
    return contour
  end

  def self.advancedContours(mat)
    contour = mat.find_contours({:mode => :ccomp})
    # puts contour.class
    #puts contour[0].class
    # puts contour.length
    c = 255
    while true
      for p in contour
        for x in 0...10
          for y in 0...10
            # drawPoint(@image.pixbuf, (p.y * 10) + x, (p.x * 10) + y, c)
          end
        end
      end
      contour = contour.v_next
      # puts contour.class
      break if contour.nil?
      c = (c / 1.5).truncate
    end
    return
  end

  def self.pruneContourPreferOutliers(contour, n)
    distances = {}
    for i in contour
      ds = 0
      for j in contour
        d = distance(i, j)
        ds += sqrt(d)
      end
      distances[i] = ds
    end

    distances = distances.to_a.sort {|a,b| a[1] <=> b[1]}

    while contour.length > n
      contour.delete(distances.shift[0])
    end
    return contour
  end

  #=========================================================================
  #
  # cvmat utility functions
  #
  #=========================================================================


  def self.cvmat_to_matrix(cvmat)
    r = []
    cvmat.each_row {|row|
      w = row.width
      rr = Array.new(w)
      for i in 0...w
        rr[i] = row[i][0]
      end
      r.push(rr)
    }
    return r
  end

  def self.matrix_to_cvmat(matrix, type=:cv8u, channels=1)
    h = matrix.length
    w = h ? matrix[0].length : 0
    cvmat = CvMat.new(w, h, type, channels)
    for y in 0...h
      r = matrix[y]
      for x in 0...w
        cvmat[x, y] = r[x]
      end
    end
    return cvmat
  end


  #=========================================================================
  #
  # CV / GTK/GDK utility functions
  #
  #=========================================================================

  def self.pixbufToCv(pixbuf)
    stride = pixbuf.rowstride
    w = pixbuf.height
    h = pixbuf.width
    image = CvMat.new(w, h, :cv8u, 1).clear!
    for y in 0...h
      for x in 0...w
        i = x * 3 + y * stride
        # puts i, pixbuf.pixels[i].getbyte(0)
        # image[x, y] = @image_test[@current][i].getbyte(0)
        v = pixbuf.pixels.getbyte(i)
        # printf('%3d ', v)
        image[x, y] = v
      end
      # puts
    end
    return image
  end

  def self.cvToPixbuf(image)
    pixbuf = Gdk::Pixbuf.new(Gdk::Pixbuf::COLORSPACE_RGB, false, 8, image.width, image.height)
    stride = pixbuf.rowstride
    w = pixbuf.width
    h = pixbuf.height
    # puts w, h, stride
    # puts pixbuf.pixels.length
    str = pixbuf.pixels
    for y in 0...h
      for x in 0...w
        v = image[x, y][0].truncate
        # printf('%3d ', v)
        i = x * 3 + y * stride
        str.setbyte(i+0, v)
        str.setbyte(i+1, v)
        str.setbyte(i+2, v)
      end
      # puts
    end
    pixbuf.pixels = str
    return pixbuf
  end


end

Gtk.init
  window = MSI.new
Gtk.main
