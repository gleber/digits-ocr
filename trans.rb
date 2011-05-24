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

digits = loadIDX("train-images-idx3-ubyte")
labels = loadIDX("train-labels-idx1-ubyte")

File.open("train_fann.txt", "wb") do |file|
  file.puts("%d %d %d" % [digits.size, 28*28, 10])
  digits.size.times do |i|
    ds = digits[i].bytes.map{|x| x}
    file.puts(ds.join(" "))
    a = Array.new(10, 0)
    a[labels[i]] = 1
    file.puts(a.join(" "))
  end
end
