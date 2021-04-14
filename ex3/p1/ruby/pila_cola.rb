# Módulo sólo con firmas de funciones utilizado como interfaz
module Secuencia
  def agregar a
  end
  def remover
  end
  def vacio
  end
end

####################
# Implementaciones #
####################

class Pila
  include Secuencia 
  attr_reader :list

  def initialize
    @list = []
  end

  def agregar el 
    @list << el  
    return 
  end

  def remover 
    if self.vacio
      raise "La pila esta vacia"
    else
      @list.pop
    end
  end

  def vacio 
    @list.empty?
  end

end

class Cola
  attr_reader :list
  def initialize
    @list = [] 
  end

  def agregar el 
    @list = [el] + @list 
    return 
  end

  def remover 
    if self.vacio 
      raise "La Cola esta vacia" 
    else
      @list.pop
    end
  end

  def vacio 
    @list.empty?
  end
end


###########
# Pruebas # DESCOMENTAR PRUEBAS
###########

a = Pila.new() 
b = Cola.new()

=begin
puts "Agregando a pila:"
for i in (1..10).to_a
  puts "Agregado #{i}" 
  a.agregar i 
end

puts "Removiendo de pila:" 
for i in (1..10).to_a
  print "#{a.remover} "
end
print "" 

#a.remove # causaria error

=end

=begin

puts "Agregando a cola:"
for i in (1..10).to_a
  puts "Agregado #{i}" 
  b.agregar i 
end

puts "Removiendo de cola:" 
for i in (1..10).to_a
  print "#{b.remover} "
end
print "" 

#b.remove # causaria error

=end
