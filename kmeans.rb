class KMeans

  def initialize(nodes, k)
    @nodes = nodes
    @centroids = (0...k).map{ |x|
      rand(@nodes.length)
    }
  end

  def assignClosest
    for n in @nodes
      bc = @centroids[0]
      dc = 1.0 / 0.0
      for c in @centroids
        d = @nodes[c].compare(n)
        if d < dc
          bc = c
          dc = d
        end
      end
      n.medoid = bc
    end
  end
  
  def cluster
    i = 0
    while i < 100
      assignClosest
      i += 1
    end
  end
  
end
