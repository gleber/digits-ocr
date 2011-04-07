class KMeans

  def initialize(nodes, k)
    @nodes = nodes
    @centroids = (0...k).map{ |x|
      rand(@nodes.length)
    }
    @cluster_nodes = Array.new(k) {|i|
      []
    }
    @distances = Array.new(@nodes.length) { |i|
      Array.new(@nodes.length) { |j|
        nil
      }
    }
  end

  def cachedDistance(a, b)
    d = @distances[a][b]
    if d.nil?
      d = @nodes[a].compare(@nodes[b])
      @distances[a][b] = d
      @distances[b][a] = d
    end
    d
  end

  def assignClosest
    @cluster_nodes.map! {|x| []}
    for n in @nodes.each_index
      bc = @centroids[0]
      dc = 1.0 / 0.0
      for i in @centroids.each_index
        c = @centroids[i]
        d = cachedDistance(c, n)
        # pp d
        # pp dc
        if d < dc
          bc = c
          dc = d
        end
      end
      @nodes[n].medoid = bc
      @cluster_nodes[i] << n
    end
  end

  def cluster
    i = 0
    generation_cost = sum_old_cost = 1.0 / 0.0
    while i < 100
      assignClosest
      generation_cost = 0
      for i in @centroids.each_index
        c = @centroids[i]
        nds = @cluster_nodes[i]
        bc = c
        cc = cost(c, nds)
        for n in nds
          c = cost(n, nds)
          if c < cc
            cc = c
            bc = n
          end
        end
        generation_cost += cc
        for n in nds
          @nodes[n].medoid = bc
        end
        @centroids[i] = bc
      end
      if generation_cost > sum_old_cost
        raise "Next generation cost should not rise!"
      end
      if sum_old_cost == generation_cost
        break
      end
      sum_old_cost = generation_cost
      i += 1
    end
    printf("final cost %f\n", generation_cost)
    return @centroids.dup
  end

  def cost(centroid, nds)
    c = 0
    for n in nds
      c += cachedDistance(centroid, n)
    end
    c
  end

end
