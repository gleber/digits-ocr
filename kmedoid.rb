def min(a, b)
  a < b ? a : b
end

class KMedoid

  def initialize(nodes, k, cached_dist={})
    @nodes = nodes
    # contains a list of ImageSample objects
    k = @nodes.length if @nodes.length < k
    rands = (0...@nodes.length).to_a.sort_by! { rand }
    @medoids = (0...k).map{ |x|
      rands.pop
    }
    # medoids contains a list of indexes from @nodes which are
    # currently chosen as medoids
    pp @medoids
    @distances = cached_dist
  end

  def cachedDistance(a, b)
    an = @nodes[a]
    bn = @nodes[b]
    a = an.id
    b = bn.id
    if a < b
      id = [a,b]
    else
      id = [b,a]
    end
    d = @distances[id]
    if d.nil?
      d = an.compare(bn)
      printf("Distance %d %d : %f\n", an.id, bn.id, d)
      @distances[id] = d
    end
    d
  end

  def cluster
    i = 0
    best_conf = @medoids.dup
    last_cost = best_cost = cost(@nodes, best_conf)
    while true
      last_cost = best_cost
      for m in @medoids.each_index
        for n in @nodes.each_index
          meds = @medoids.dup          
          old_m = meds[m]
          ind = meds.index(n)
          meds[m] = n
          if ind
            meds[ind] = old_m
          end          
          new_cost = cost(@nodes, meds)
          #pp [new_cost, meds]
          if new_cost < best_cost
            best_conf = meds
            best_cost = new_cost            
          end
        end
      end
      puts "New best cost", best_cost
      puts "Last known best cost", last_cost
      if best_cost > last_cost
        raise "Next generation cost should not rise!"
      end        
      if last_cost == best_cost
        break
      end
      last_cost = best_cost
      @medoids = best_conf
    end
    printf("final cost %f\n", best_cost)
    return @medoids.dup
  end

  def cost(nds, medoids)
    c = 0
    for n in nds.each_index
      cc = 1.0 / 0.0
      for m in medoids
        d = cachedDistance(m, n)
        cc = min(cc, d)
      end
      c += cc
    end
    c
  end

end
