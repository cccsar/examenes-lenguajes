require './pila_cola.rb'

# Se utilizan las implementaciones de pila y cola del inciso 1 para implementar
# los recorridos.

# Implementacion de lista de adyacencia con metodos para creacion de nodos
# e inicializacion por IO.
class Graph
  attr_reader :ady_list, :vtd

  def initialize(n,m) 
    @ady_list = [] 
    for i in (0..n-1).to_a
      self.add_node
    end
    
    puts "Se agregaran los pares (u,v)"

    for i in (0..m-1).to_a
      print "Agregar u para el nodo #{i}: "
      u = gets.chomp.to_i
      print "" 
  
      print "Agregar u para el nodo #{i}: "
      v = gets.chomp.to_i
      print "" 

      self.add_edge(u,v)
    end
  end

  def add_node 
    @ady_list << [] 
  end

  def add_edge (u, v)
    if u < 0 || v < 0 || u > @ady_list.length || v > @ady_list.length
      raise "Fuera de los limites de la lista de adyacencia"
    else
      @ady_list[u].push v
      @ady_list[v].push u
    end
  end

  def sons i 
    @ady_list[i]
  end

end

# Clase abstracta busqueda definida como una clase con un unico metodo sin inicializar
class Busqueda
  def buscar
    raise "definir"
  end
end


class DFS  < Busqueda
  attr_reader :graph , :vtd

  def initialize (n,m) 
    @graph = Graph.new(n,m)
    @vtd = [] 

    for i in (0..@graph.ady_list.length-1).to_a
      @vtd<<false
    end
  end

  #Reseteo del arreglo de visitados
  def set_vtd
    for i in (0..@graph.ady_list.length-1).to_a
      @vtd[i] = false 
    end
  end

  def busqueda (d, h)
    my_stack = Pila.new()
    cnt = 0
    found = false
    dum = 0

    my_stack.agregar d


    while ( !my_stack.vacio ) 
      dum = my_stack.remover
      vtd[dum] = true; 
      cnt += 1

      if ( dum == h ) 
        found = true
        break ;
      end

      for i in @graph.sons dum
        if ( !vtd[i] ) 
          my_stack.agregar i
        end
      end

    end

    self.set_vtd

    if found 
      cnt
    else
      -1
    end
  end


end

class BFS
  attr_reader :graph, :vtd

  def initialize (n,m) 
    @graph = Graph.new(n,m)
    @vtd = []

    for i in (0..@graph.ady_list.length-1).to_a
      @vtd<<false
    end
  end

  # Reseteo del arreglo de visitados
  def set_vtd
    for i in (0..@graph.ady_list.length-1).to_a
      @vtd[i] = false 
    end
  end

  def busqueda (d,h)
    my_q = Cola.new()
    cnt = 0
    dum = 0
    found = false

    my_q.agregar d

    while ( !my_q.vacio ) 
      dum = my_q.remover
      vtd[dum] = true
      cnt +=1

      if (dum == h) 
        found = true
        break
      end

      for i in @graph.sons dum
        if (!vtd[i])
          my_q.agregar i
        end
      end

    end

    self.set_vtd

    if found
      cnt
    else
      -1
    end
  end

end

###########
# Pruebas # DESCOMENTAR PRUEBAS
###########

#dfs = DFS.new(5,3) # Se define el grafo por consola
#dfs.busqueda(0,2)
#dfs.busqueda(0,4)

#bfs = BFS.new(5,4) # Se define el grafo por consola
#bfs.busqueda(0,3)
#bfs.busqueda(4,0)
