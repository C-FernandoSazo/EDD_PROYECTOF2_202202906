module Arbol_CapaBB
    use matriz_DispersaI
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
            procedure :: escribirABB
            procedure :: buscarNodo
            procedure :: ingresarMatriz
            procedure :: preorder
            procedure :: inorder
            procedure :: postorder
            procedure :: calcularProfundidad
            procedure :: imprimirHojas
            procedure :: preorderLimit
            procedure :: inorderLimit
            procedure :: postorderLimit
            procedure :: recorridoAmplitud
            procedure :: apilarMatriz
            procedure :: getRaiz
            procedure :: contarCapas
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
            write(*,'(A,I0)') "Clave: ", nodo%capa%key
            ! Imprimir subárbol derecho
            call imprimirRecursivo(nodo%right)
        end if
    end subroutine imprimirRecursivo

    subroutine graficarABB(arbol)
        class(ArbolCapas), intent(in) :: arbol
        character(len=12) :: filename = "arbolCapas"
        integer :: fileUnit, iostat
        character(len=256) :: dotPath, pngPath

        dotPath = 'dot/' // trim(filename) // '.dot'
        pngPath = 'img/' // trim(adjustl(filename))

        open(newunit=fileUnit, file=dotPath, status='replace', iostat=iostat)
        if (iostat /= 0) then
            print *, "Error al abrir el archivo."
            return
        end if

        write(fileUnit, *) "digraph capas {"
        if (associated(arbol%raiz)) then
            call escribirNodoRecursivo(arbol%raiz, fileUnit)
        end if
        write(fileunit,*) '}'
        close(fileUnit)
        call system('dot -Tpng ' // trim(dotPath) // ' -o ' // trim(adjustl(pngPath)) // '.png')   
    end subroutine graficarABB

    subroutine escribirABB(arbol,fileUnit)
        class(ArbolCapas), intent(in) :: arbol
        integer, intent(in) :: fileUnit

        write(fileUnit, *) "subgraph capas {"
        write(fileUnit, *) "node [shape=circle];"
        if (associated(arbol%raiz)) then
            call escribirNodoRecursivo(arbol%raiz, fileUnit)
        end if
        write(fileunit,*) '}'
    end subroutine escribirABB

    recursive subroutine escribirNodoRecursivo(nodo, unitNum)
        type(NodoCapa), pointer, intent(in) :: nodo
        integer, intent(in) :: unitNum

        if (.not. associated(nodo)) return

        ! Escribe el nodo actual
        write(unitNum,'(A,I0,A,I0,A)') '"Capa',nodo%capa%key, '" [label="Capa ', nodo%capa%key, '"]'
        ! Escribe la arista al hijo izquierdo si existe
        if (associated(nodo%left)) then
            write(unitNum,'(A,I0,A,I0,A)') '"Capa', nodo%capa%key, '" -> "Capa', nodo%left%capa%key,'"'
            call escribirNodoRecursivo(nodo%left, unitNum)
        end if
        ! Escribe la arista al hijo derecho si existe
        if (associated(nodo%right)) then
            write(unitNum,'(A,I0,A,I0,A)') '"Capa', nodo%capa%key, '" -> "Capa', nodo%right%capa%key,'"'
            call escribirNodoRecursivo(nodo%right, unitNum)
        end if
    end subroutine escribirNodoRecursivo

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

    subroutine preorder(this, tmp)
        class(ArbolCapas), intent(in) :: this
        type(NodoCapa), intent(in), pointer :: tmp
        if( .not. associated(tmp)) then
            return
        end if
        write (*, '(A,I0)', advance='no') ' Capa: ',tmp%capa%key
        call this%preorder(tmp%left)
        call this%preorder(tmp%right)
    end subroutine preorder

    subroutine inorder(this, tmp)
        class(ArbolCapas), intent(in) :: this
        type(NodoCapa), intent(in), pointer :: tmp
        if( .not. associated(tmp)) then
            return
        end if
        call this%inorder(tmp%left)
        write (*, '(A,I0)', advance='no') ' Capa: ',tmp%capa%key
        call this%inorder(tmp%right)
    end subroutine inorder

    subroutine postorder(this, tmp)
        class(ArbolCapas), intent(in) :: this
        type(NodoCapa), intent(in), pointer :: tmp
        if( .not. associated(tmp)) then
            return
        end if
        call this%postorder(tmp%left)
        call this%postorder(tmp%right)
        write (*, '(A,I0)', advance='no') ' Capa: ',tmp%capa%key
    end subroutine postorder

    function calcularProfundidad(arbol) result(profundidad)
        class(ArbolCapas), intent(in) :: arbol
        integer :: profundidad
    
        profundidad = profundidadRecursiva(arbol%raiz)
        
    end function calcularProfundidad
    
    recursive function profundidadRecursiva(nodo) result(profundidadNodo)
        type(NodoCapa), pointer, intent(in) :: nodo
        integer :: profundidadIzquierda, profundidadDerecha, profundidadNodo
    
        if (.not. associated(nodo)) then
            profundidadNodo = 0
            return
        end if

        profundidadIzquierda = profundidadRecursiva(nodo%left)
        profundidadDerecha = profundidadRecursiva(nodo%right)
        profundidadNodo = 1 + max(profundidadIzquierda, profundidadDerecha)
    end function profundidadRecursiva

    subroutine imprimirHojas(arbol)
        class(ArbolCapas), intent(in) :: arbol
        if (associated(arbol%raiz)) then
            call imprimirHojasRecursivo(arbol%raiz)
        end if
    end subroutine imprimirHojas
    
    recursive subroutine imprimirHojasRecursivo(nodo)
        type(NodoCapa), pointer, intent(in) :: nodo
        if (.not. associated(nodo)) return
        ! Si ambos hijos son nulos, el nodo es una hoja
        if (.not. associated(nodo%left) .and. .not. associated(nodo%right)) then
            write(*,'(A,I0)'), "Nodo hoja con clave: Capa ", nodo%capa%key
        else
            ! Si no, sigue buscando en los hijos
            if (associated(nodo%left)) call imprimirHojasRecursivo(nodo%left)
            if (associated(nodo%right)) call imprimirHojasRecursivo(nodo%right)
        end if
    end subroutine imprimirHojasRecursivo

    subroutine preorderLimit(this, tmp, limit, contador, matriz, capasTemp) 
        class(ArbolCapas), intent(in) :: this
        type(NodoCapa), intent(in), pointer :: tmp
        type(nodo_matriz), pointer :: nodoActual, filaActual
        integer, intent(in) :: limit
        integer, intent(inout) :: contador 
        type(matrizDispersa), intent(inout) :: matriz
        type(ArbolCapas), intent(inout) :: capasTemp
        if( .not. associated(tmp)) then
            return
        end if
        if (contador /= limit) then
            contador = contador + 1
            call capasTemp%insertarNodoConMatriz(tmp%capa%key, tmp%capa%matriz)
            filaActual => tmp%capa%matriz%head%down
            do while (associated(filaActual))
                nodoActual => filaActual%right
                
                do while(associated(nodoActual))
                    call matriz%agregarMatriz(nodoActual%fila, nodoActual%columna, nodoActual%color)
                    nodoActual => nodoActual%right
                end do
                filaActual => filaActual%down
            end do
            call this%preorderLimit(tmp%left,limit,contador,matriz, capasTemp)
            call this%preorderLimit(tmp%right,limit,contador,matriz, capasTemp)
        end if
    end subroutine preorderLimit

    subroutine inorderLimit(this, tmp, limit, contador, matriz, capasTemp)
        class(ArbolCapas), intent(in) :: this
        type(NodoCapa), intent(in), pointer :: tmp
        type(nodo_matriz), pointer :: filaActual, nodoActual
        integer, intent(in) :: limit
        integer, intent(inout) :: contador 
        type(matrizDispersa), intent(inout) :: matriz
        type(ArbolCapas), intent(inout) :: capasTemp
        
        if( .not. associated(tmp)) then
            return
        end if
        call this%inorderLimit(tmp%left,limit,contador,matriz, capasTemp)
        if (contador /= limit) then
            contador = contador + 1
            call capasTemp%insertarNodoConMatriz(tmp%capa%key, tmp%capa%matriz)
            filaActual => tmp%capa%matriz%head%down
            do while (associated(filaActual))
                nodoActual => filaActual%right
                
                do while(associated(nodoActual))
                    call matriz%agregarMatriz(nodoActual%fila, nodoActual%columna, nodoActual%color)
                    nodoActual => nodoActual%right
                end do
                filaActual => filaActual%down
            end do
        end if
        call this%inorderLimit(tmp%right,limit,contador,matriz, capasTemp)
    end subroutine inorderLimit

    subroutine postorderLimit(this, tmp, limit, contador, matriz, capasTemp)
        class(ArbolCapas), intent(in) :: this
        type(NodoCapa), intent(in), pointer :: tmp
        type(nodo_matriz), pointer :: filaActual, nodoActual
        integer, intent(in) :: limit
        integer, intent(inout) :: contador 
        type(matrizDispersa), intent(inout) :: matriz
        type(ArbolCapas), intent(inout) :: capasTemp
        
        if( .not. associated(tmp)) then
            return
        end if
        call this%postorderLimit(tmp%left,limit,contador,matriz,capasTemp)
        call this%postorderLimit(tmp%right,limit,contador,matriz,capasTemp)
        if (contador /= limit) then
            contador = contador + 1
            call capasTemp%insertarNodoConMatriz(tmp%capa%key, tmp%capa%matriz)
            filaActual => tmp%capa%matriz%head%down
            do while (associated(filaActual))
                nodoActual => filaActual%right
                
                do while(associated(nodoActual))
                    call matriz%agregarMatriz(nodoActual%fila, nodoActual%columna, nodoActual%color)
                    nodoActual => nodoActual%right
                end do
                filaActual => filaActual%down
            end do
        end if
    end subroutine postorderLimit

    subroutine recorridoAmplitud(arbol,matriz)
        class(ArbolCapas), intent(in) :: arbol
        type(matrizDispersa), intent(inout) :: matriz
        integer :: alturaArbol, nivel
    
        alturaArbol = arbol%calcularProfundidad()
        ! Recorrer cada nivel del árbol
        do nivel = 0, alturaArbol
            call recorrerNivel(arbol%raiz, nivel, matriz)
        end do
    end subroutine recorridoAmplitud

    recursive subroutine recorrerNivel(nodo, nivel, matriz)
        type(NodoCapa), pointer, intent(in) :: nodo
        integer, intent(in) :: nivel
        type(matrizDispersa), intent(inout) :: matriz
        type(nodo_matriz), pointer :: filaActual, nodoActual

        if (.not. associated(nodo)) then
            return
        end if
        if (nivel == 0) then
            print *, "Clave:", nodo%capa%key
            filaActual => nodo%capa%matriz%head%down
            do while (associated(filaActual))
                nodoActual => filaActual%right
                
                do while(associated(nodoActual))
                    call matriz%agregarMatriz(nodoActual%fila, nodoActual%columna, nodoActual%color)
                    nodoActual => nodoActual%right
                end do
                filaActual => filaActual%down
            end do
        else
            call recorrerNivel(nodo%left, nivel-1, matriz)
            call recorrerNivel(nodo%right, nivel-1, matriz)
        end if
    end subroutine recorrerNivel

    subroutine apilarMatriz(arbol, key, matriz)
        class(ArbolCapas), intent(in) :: arbol
        integer, intent(in) :: key
        type(matrizDispersa), intent(inout) :: matriz

        call apilarRecursivo(arbol%raiz,key, matriz)

    end subroutine apilarMatriz

    ! Subrutina recursiva para buscar un nodo
    recursive subroutine apilarRecursivo(nodo, key, matriz) 
        type(NodoCapa), pointer, intent(in) :: nodo
        integer, intent(in) :: key
        type(matrizDispersa), intent(inout) :: matriz
        type(nodo_matriz), pointer :: filaActual, nodoActual

        if (.not. associated(nodo)) then
            return
        end if

        if (key < nodo%capa%key) then
            call apilarRecursivo(nodo%left, key, matriz)
        else if (key > nodo%capa%key) then
            call apilarRecursivo(nodo%right, key, matriz)
        else
            filaActual => nodo%capa%matriz%head%down
            do while (associated(filaActual))
                nodoActual => filaActual%right
                
                do while(associated(nodoActual))
                    call matriz%agregarMatriz(nodoActual%fila, nodoActual%columna, nodoActual%color)
                    nodoActual => nodoActual%right
                end do
                filaActual => filaActual%down
            end do
        end if
    end subroutine apilarRecursivo

    function getRaiz(arbol) result(raiz)
        class(ArbolCapas), intent(in) :: arbol
        integer :: raiz
        raiz = arbol%raiz%capa%key
    end function getRaiz

    function contarCapas(this) result(numNodos)
        class(ArbolCapas), intent(in) :: this
        integer :: numNodos

        numNodos = contarRecursivoCapa(this%raiz)
    end function contarCapas

    ! Función auxiliar recursiva para contar nodos
    recursive function contarRecursivoCapa(nodo) result(contador)
        type(NodoCapa), pointer, intent(in) :: nodo
        integer :: contador

        if (.not. associated(nodo)) then
            contador = 0
        else
            contador = 1 + contarRecursivoCapa(nodo%left) + contarRecursivoCapa(nodo%right)
        end if
    end function contarRecursivoCapa
        
end module Arbol_CapaBB