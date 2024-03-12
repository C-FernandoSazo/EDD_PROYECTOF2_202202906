module Arbol_Capas
    use matriz_dispersa
    implicit none
    ! Arbol Binario de Busqueda ABB

    type :: Capa
        integer :: key
        type(matrizDispersa) :: matriz
    end type Capa

    type :: NodoCapa
        type(Capa) :: capa
        type(NodoCapa), pointer :: left => null()
        type(NodoCapa), pointer :: right => null()
    end type NodoCapa

    type :: ArbolCapas 
        type(NodoCapa), pointer :: raiz => null()
        contains
            procedure :: insertarNodo
            procedure :: insertarNodoConMatriz
            procedure :: imprimirEnOrden
            procedure :: graficarABB
            procedure :: vaciarArbol
            procedure :: buscarNodo
            procedure :: ingresarMatriz
    end type ArbolCapas

contains 

    subroutine insertarNodo(arbol, key)
        class(ArbolCapas), intent(inout) :: arbol
        integer, intent(in) :: key
        type(NodoCapa), pointer :: nuevoNodo

        ! Crear un nuevo nodo
        allocate(nuevoNodo)
        nuevoNodo%capa%key = key
        nuevoNodo%left => null()
        nuevoNodo%right => null()

        if (.not. associated(arbol%raiz)) then
            ! Si el árbol está vacío, el nuevo nodo es la raíz
            arbol%raiz => nuevoNodo
        else
            ! De lo contrario, llamar a la subrutina recursiva para insertar el nodo
            call insertarRecursivo(arbol%raiz, nuevoNodo)
        end if
    end subroutine insertarNodo

    subroutine insertarNodoConMatriz(arbol, key, matriz)
        class(ArbolCapas), intent(inout) :: arbol
        integer, intent(in) :: key
        type(matrizDispersa), intent(in) :: matriz
        type(NodoCapa), pointer :: nuevoNodo
    
        ! Crear un nuevo nodo
        allocate(nuevoNodo)
        nuevoNodo%capa%key = key
        nuevoNodo%capa%matriz = matriz
        nuevoNodo%left => null()
        nuevoNodo%right => null()
    
        if (.not. associated(arbol%raiz)) then
            ! Si el árbol está vacío, el nuevo nodo es la raíz
            arbol%raiz => nuevoNodo
        else
            ! De lo contrario, llamar a la subrutina recursiva para insertar el nodo
            call insertarRecursivo(arbol%raiz, nuevoNodo)
        end if
    end subroutine insertarNodoConMatriz

    ! Subrutina recursiva para insertar el nodo en la posición correcta
    recursive subroutine insertarRecursivo(nodo, nuevoNodo)
        type(NodoCapa), pointer, intent(inout) :: nodo
        type(NodoCapa), pointer, intent(in) :: nuevoNodo

        if (nuevoNodo%capa%key < nodo%capa%key) then
            if (.not. associated(nodo%left)) then
                nodo%left => nuevoNodo
            else
                call insertarRecursivo(nodo%left, nuevoNodo)
            end if
        else if (nuevoNodo%capa%key > nodo%capa%key) then
            if (.not. associated(nodo%right)) then
                nodo%right => nuevoNodo
            else
                call insertarRecursivo(nodo%right, nuevoNodo)
            end if
        else
            print *, "La clave ya existe en el árbol."
        end if
    end subroutine insertarRecursivo

    subroutine imprimirEnOrden(arbol)
        class(ArbolCapas), intent(in) :: arbol
        ! Si el árbol no está vacío, imprimir recursivamente
        if (associated(arbol%raiz)) then
            call imprimirRecursivo(arbol%raiz)
        end if
    end subroutine imprimirEnOrden

    ! Subrutina recursiva para imprimir los nodos en orden
    recursive subroutine imprimirRecursivo(nodo)
        type(NodoCapa), pointer, intent(in) :: nodo
        
        if (associated(nodo)) then
            ! Imprimir subárbol izquierdo
            call imprimirRecursivo(nodo%left)
            ! Imprimir clave actual del nodo
            print *, "Clave:", nodo%capa%key
            call nodo%capa%matriz%mostrarMatriz()
            ! Imprimir subárbol derecho
            call imprimirRecursivo(nodo%right)
        end if
    end subroutine imprimirRecursivo

    subroutine graficarABB(arbol)
        class(ArbolCapas), intent(in) :: arbol
        character(len=12) :: filename = "arbolABB.dot"
        integer :: fileUnit, iostat

        open(newunit=fileUnit, file=filename, status='replace', iostat=iostat)
        if (iostat /= 0) then
            print *, "Error al abrir el archivo."
            return
        end if

        write(fileUnit, *) "digraph impresiones {"
        if (associated(arbol%raiz)) then
            call escribirNodoRecursivo(arbol%raiz, fileUnit)
        end if
        write(fileunit,*) '}'
        close(fileUnit)
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')   
    end subroutine graficarABB

    recursive subroutine escribirNodoRecursivo(nodo, unitNum)
        type(NodoCapa), pointer, intent(in) :: nodo
        integer, intent(in) :: unitNum

        if (.not. associated(nodo)) return

        ! Escribe el nodo actual
        write(unitNum,'(I0,A,I0,A)') nodo%capa%key, '[label="Capa ', nodo%capa%key, '"]'
        ! Escribe la arista al hijo izquierdo si existe
        if (associated(nodo%left)) then
            write(unitNum,*) nodo%capa%key, ' -> ', nodo%left%capa%key
            call escribirNodoRecursivo(nodo%left, unitNum)
        end if
        ! Escribe la arista al hijo derecho si existe
        if (associated(nodo%right)) then
            write(unitNum,*) nodo%capa%key, ' -> ', nodo%right%capa%key
            call escribirNodoRecursivo(nodo%right, unitNum)
        end if
    end subroutine escribirNodoRecursivo

        
    subroutine vaciarArbol(arbol)
        class(ArbolCapas), intent(inout) :: arbol
        if (associated(arbol%raiz)) then
            call vaciarNodosRecursivo(arbol%raiz)
            arbol%raiz => null()  ! Asegurar que la raíz del árbol apunte a null después de vaciar el árbol
        end if
        print *, "VACIAMOS EL ARBOL"
    end subroutine vaciarArbol
    
    recursive subroutine vaciarNodosRecursivo(nodo)
        type(NodoCapa), pointer, intent(inout) :: nodo
        if (.not. associated(nodo)) return
        ! Recursivamente vaciar el subárbol izquierdo
        call vaciarNodosRecursivo(nodo%left)
        ! Recursivamente vaciar el subárbol derecho
        call vaciarNodosRecursivo(nodo%right)
        ! Liberar el nodo actual
        deallocate(nodo)
    end subroutine vaciarNodosRecursivo

    ! Función para buscar un nodo por su clave
    function buscarNodo(arbol, key) result(capaEncontrada)
        class(ArbolCapas), intent(in) :: arbol
        integer, intent(in) :: key
        type(Capa) :: capaEncontrada
        type(NodoCapa), pointer :: nodoEncontrado

        nodoEncontrado => buscarRecursivo(arbol%raiz, key)
        if (associated(nodoEncontrado)) then
            capaEncontrada = nodoEncontrado%capa
        else 
            capaEncontrada%key = -1
        end if

    end function buscarNodo

    ! Subrutina recursiva para buscar un nodo
    recursive function buscarRecursivo(nodo, key) result(nodoEncontrado)
        type(NodoCapa), pointer, intent(in) :: nodo
        integer, intent(in) :: key
        type(NodoCapa), pointer :: nodoEncontrado

        ! Si el nodo actual es nulo, no se encontró el nodo buscado
        if (.not. associated(nodo)) then
            nodoEncontrado => null()
            return
        end if

        ! Si la clave buscada es menor que la clave del nodo actual, buscar en el subárbol izquierdo
        if (key < nodo%capa%key) then
            nodoEncontrado => buscarRecursivo(nodo%left, key)
        ! Si la clave buscada es mayor que la clave del nodo actual, buscar en el subárbol derecho
        else if (key > nodo%capa%key) then
            nodoEncontrado => buscarRecursivo(nodo%right, key)
        ! Si la clave buscada es igual a la clave del nodo actual, hemos encontrado el nodo
        else
            nodoEncontrado => nodo
        end if
    end function buscarRecursivo

    subroutine ingresarMatriz(arbol, key, fila, columna, color) 
        class(ArbolCapas), intent(in) :: arbol
        integer, intent(in) :: key, fila, columna
        character(len=7), intent(in) :: color

        call matrizRecusrivo(arbol%raiz, key, fila, columna, color)

    end subroutine ingresarMatriz

    ! Subrutina recursiva para buscar un nodo
    recursive subroutine matrizRecusrivo(nodo, key, fila, columna, color)
        type(NodoCapa), pointer, intent(in) :: nodo
        integer, intent(in) :: key, fila, columna
        character(len=7), intent(in) :: color

        ! Si la clave buscada es menor que la clave del nodo actual, buscar en el subárbol izquierdo
        if (key < nodo%capa%key) then
            call matrizRecusrivo(nodo%left, key,fila,columna,color)
        ! Si la clave buscada es mayor que la clave del nodo actual, buscar en el subárbol derecho
        else if (key > nodo%capa%key) then
            call matrizRecusrivo(nodo%right, key, fila, columna, color)
        ! Si la clave buscada es igual a la clave del nodo actual, hemos encontrado el nodo
        else
            call nodo%capa%matriz%agregarMatriz(fila,columna,color)
        end if
    end subroutine  matrizRecusrivo

end module Arbol_Capas